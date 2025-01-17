package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.frame.*;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LEXICAL_BINDING;

/// Basically our implementation of GNU Emacs' `internal-interpreter-environment`
///
///   - Dynamic bindings are simply utility functions that swap values in and out.
///   - Lexical bindings are more complex, whose implementation entails a few functions
///     in [BuiltInEval] (`src/eval.c`).
///
/// ## Lexical Scoping
///
/// Whether a variable is lexical or dynamic is determined by several things in GNU Emacs:
///
///   - If the `special` field of a symbol is set, `let/let*` statements should
///     always bind it dynamically.
///   - The arguments of a function defined in a lexical environment is always lexically bound.
///   - If a symbol is somehow marked "special" in `internal-interpreter-environment`,
///     then it should be bound dynamically by `let/let*`.
///   - If a symbol is found "normal" in `internal-interpreter-environment`, it is
///     lexically bound and looked up in `internal-interpreter-environment`. Otherwise,
///     it is still dynamically (i.e. globally) bound.
///
/// ### Reusing Frame Slots
///
/// So, if a framed is materialized, some other functions might be referencing slots in this frame,
/// and we must not reuse *those* frame slots. However, this can lead to excessive memory usage
/// in cases like {@code let} in loops, so we do want to reuse slots as much as possible.
///
/// ### Primitives On The Stack
///
/// In order to avoid excessive GC pressure, it is a good idea to avoid the cost of boxing and
/// unboxing primitives when storing things into the frame. GraalJS does this by **rewriting**
/// the frame descriptor to use a different frame slot kind for primitives when a primitive is
/// written to the slot. However, this will invalidate the whole function compilation, causing
/// significant slow down if this happens too often. (In JavaScript, this is mitigated by the
/// fact that `long`, `double` or `SafeInt` are interchangeable internal types, but this is not
/// the case in ELisp.)
///
/// Instead, we use a different strategy: we use a container object to store primitives -
/// similar to [Long]/[Double] boxing, but mutable to avoid the allocation cost.
/// The logic is implemented in [party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNode].
/// [ELispLexical] only implements the logic for object accessing.
///
public final class ELispLexical {
    /// Placeholder to mark a symbol "dynamic"
    public final static LexicalReference DYNAMIC = new LexicalReference(null, 0);

    public final static int NON_VAR_SLOT0 = 0;
    private final static int MATERIALIZED_TOP_SLOT = 0;
    private final static int LEXICAL_FRAME_SLOT = 1;
    private final static int SPILL_LIST_SLOT = 2;
    private final static int START_SLOT = 3;
    public final static int MAX_SLOTS = 32;

    private final static FrameDescriptor LEXICAL_DESCRIPTOR;
    private final static FrameDescriptor DYNAMIC_DESCRIPTOR;

    static {
        FrameDescriptor.Builder lexical = FrameDescriptor.newBuilder(MAX_SLOTS);
        lexical.addSlots(1, FrameSlotKind.Int);
        lexical.addSlots(MAX_SLOTS - 1, FrameSlotKind.Object);
        LEXICAL_DESCRIPTOR = lexical.build();
        FrameDescriptor.Builder dynamic = FrameDescriptor.newBuilder(1);
        dynamic.addSlots(1, FrameSlotKind.Object);
        DYNAMIC_DESCRIPTOR = dynamic.build();
    }

    public static FrameDescriptor frameDescriptor(boolean lexical) {
        return lexical ? LEXICAL_DESCRIPTOR : DYNAMIC_DESCRIPTOR;
    }

    public static void initFrame(VirtualFrame frame) {
        frame.setInt(MATERIALIZED_TOP_SLOT, -1);
    }

    private final List<ELispSymbol> args;
    private final List<ELispSymbol> variables;
    private final IntArrayList variableIndices;
    private boolean materialized;
    private int topIndex;
    @Nullable
    private final MaterializedFrame materializedParent;
    @Nullable
    private final ELispLexical parent;
    private MaterializedAssumption materializedTopUnchanged;

    private ELispLexical(
            VirtualFrame frame,
            @Nullable MaterializedFrame parentFrame,
            @Nullable ELispLexical parent,
            List<ELispSymbol> requiredArgs,
            int startSlot,
            MaterializedAssumption assumption
    ) {
        this.args = requiredArgs;
        this.variables = new ArrayList<>();
        this.variableIndices = new IntArrayList();
        this.parent = parent;
        this.topIndex = startSlot;
        this.materialized = false;
        this.materializedTopUnchanged = assumption;
        this.materializedParent = parentFrame;
        frame.setObject(LEXICAL_FRAME_SLOT, this);
    }

    /// Creates a lexical scope, typically created by `let`
    public static ELispLexical create(
            VirtualFrame frame,
            ELispLexical parent,
            MaterializedAssumption assumption
    ) {
        return new ELispLexical(
                frame,
                null,
                parent,
                List.of(),
                parent.topIndex,
                assumption
        );
    }

    /// Creates a lexical scope for a root node
    public static ELispLexical create(VirtualFrame frame, MaterializedAssumption assumption) {
        ELispLexical lexical = new ELispLexical(frame, null, null, List.of(), START_SLOT, assumption);
        setMaterializedTop(frame, START_SLOT);
        return lexical;
    }

    /// Creates a lexical scope for a root node created inside a parent frame
    public static ELispLexical create(
            VirtualFrame frame,
            ELispLexical parent,
            VirtualFrame parentFrame,
            List<ELispSymbol> requiredArgs,
            MaterializedAssumption assumption
    ) {
        parent.materialize(parentFrame);
        ELispLexical lexical = new ELispLexical(frame, parentFrame.materialize(), parent, requiredArgs, START_SLOT, assumption);
        setMaterializedTop(frame, START_SLOT);
        return lexical;
    }

    private void materialize(VirtualFrame frame) {
        materialized = true;
        setMaterializedTop(frame, topIndex);
    }

    /// Forks the current frame
    ///
    /// ## Usage
    ///
    /// Please remember to call [#restore(VirtualFrame)].
    /// We cannot use [AutoCloseable] for this because a [VirtualFrame]
    /// should not be stored in a field unless it is a [MaterializedFrame]
    /// (which will add some overhead).
    ///
    /// @param frame the current backing frame
    /// @return a new frame
    public ELispLexical fork(VirtualFrame frame, MaterializedAssumption assumption) {
        assumption.checkEntry(this);
        return create(frame, this, assumption);
    }

    /// Restores the current frame
    ///
    /// @param frame the current backing frame
    public void restore(VirtualFrame frame) {
        int newTop = getMaterializedTop(frame);
        if (newTop > topIndex) {
            materialized = true;
            topIndex = newTop;
        }
        frame.setObject(LEXICAL_FRAME_SLOT, this);
    }

    public void addVariable(VirtualFrame frame, ELispSymbol symbol, Object value) {
        int index = topIndex;
        topIndex++;
        if (materialized) {
            setMaterializedTop(frame, topIndex);
        }
        variables.add(symbol);
        variableIndices.add(index);
        value = ELispFrameSlotNode.wrap(value);
        if (index < MAX_SLOTS) {
            frame.setObject(index, value);
        } else {
            List<Object> spills = getSpills(frame);
            if (spills == null) {
                assert index == MAX_SLOTS;
                spills = new ArrayList<>();
                frame.setObject(SPILL_LIST_SLOT, spills);
            }
            if (spills.size() > index - MAX_SLOTS) {
                spills.subList(index - MAX_SLOTS, spills.size()).clear();
            }
            spills.add(value);
            assert spills.size() + MAX_SLOTS == index + 1;
        }
    }

    private int getIndex(ELispSymbol symbol) {
        int i = variables.lastIndexOf(symbol);
        if (i != -1) {
            return variableIndices.get(i);
        }
        i = args.indexOf(symbol);
        if (i != -1) {
            return -i - 1;
        }
        return 0;
    }

    @Nullable
    public LexicalReference getLexicalReference(VirtualFrame frame, ELispSymbol symbol) {
        VirtualFrame virtualFrame = frame;
        ELispLexical currentFrame = this;
        while (true) {
            int i = currentFrame.getIndex(symbol);
            if (i != 0) {
                if (i > 0 && getVariable(frame, i) == DYNAMIC) {
                    return null;
                }
                return new LexicalReference(frame == virtualFrame ? null : frame.materialize(), i);
            }
            if (currentFrame.materializedParent != null) {
                frame = currentFrame.materializedParent;
            }
            currentFrame = currentFrame.parent;
            if (currentFrame == null) {
                return null;
            }
        }
    }

    public Object toAssocList(Frame frame) {
        ELispCons.ListBuilder lb = new ELispCons.ListBuilder();
        for (ELispSymbol symbol : variables) {
            Object variable = getVariable(frame, getIndex(symbol));
            if (variable == DYNAMIC) {
                continue;
            }
            lb.add(new ELispCons(symbol, variable));
        }
        for (ELispSymbol symbol : args) {
            Object variable = getVariable(frame, getIndex(symbol));
            if (variable == DYNAMIC) {
                continue;
            }
            lb.add(new ELispCons(symbol, variable));
        }
        return lb.buildWithCdr(parent == null ? false : parent.toAssocList(
                materializedParent == null ? frame : materializedParent));
    }

    @Nullable
    public static ELispLexical getLexicalFrame(Frame frame) {
        if (getMaterializedTop(frame) == -1)  {
            return null;
        }
        return (ELispLexical) frame.getObject(LEXICAL_FRAME_SLOT);
    }

    @SuppressWarnings("unchecked")
    @Nullable
    private static List<Object> getSpills(VirtualFrame frame) {
        return (List<Object>) frame.getObject(SPILL_LIST_SLOT);
    }

    private static int getMaterializedTop(Frame frame) {
        return frame.getInt(MATERIALIZED_TOP_SLOT);
    }

    public Assumption getMaterializedTopUnchanged() {
        return materializedTopUnchanged.stableMaterializedTop;
    }

    public void setMaterializedTopUnchanged(MaterializedAssumption assumption) {
        assumption.checkEntry(this);
        this.materializedTopUnchanged = assumption;
    }

    private static void setMaterializedTop(VirtualFrame frame, int newTop) {
        int original = getMaterializedTop(frame);
        if (original != newTop) {
            if (newTop != START_SLOT) {
                ELispLexical lexical = getLexicalFrame(frame);
                if (lexical != null) {
                    lexical.materializedTopUnchanged.invalidate(newTop);
                }
            }
            frame.setInt(MATERIALIZED_TOP_SLOT, newTop);
        }
    }

    public static Object getVariable(Frame frame, int i) {
        if (i > 0) {
            if (i >= MAX_SLOTS) {
                return ((List<?>) frame.getObject(SPILL_LIST_SLOT)).get(i - MAX_SLOTS);
            }
            return frame.getObject(i);
        }
        return frame.getArguments()[-i - 1];
    }

    @SuppressWarnings("unchecked")
    public static void setVariable(VirtualFrame frame, int i, Object value) {
        if (i > 0) {
            if (i >= MAX_SLOTS) {
                List<Object> spills = (List<Object>) frame.getObject(SPILL_LIST_SLOT);
                spills.set(i - MAX_SLOTS, value);
            } else {
                frame.setObject(i, value);
            }
        } else {
            frame.getArguments()[-i - 1] = value;
        }
    }

    public static void markDynamic(VirtualFrame frame, ELispSymbol symbol) {
        ELispLexical lexicalFrame = getLexicalFrame(frame);
        if (lexicalFrame != null && lexicalFrame.getIndex(symbol) == 0) {
            lexicalFrame.addVariable(frame, symbol, DYNAMIC);
        }
    }

    public static boolean isDynamic(VirtualFrame frame, ELispSymbol symbol) {
        if (symbol.isSpecial()) {
            return true;
        }
        ELispLexical currentFrame = getLexicalFrame(frame);
        if (currentFrame == null) {
            return false;
        }
        while (true) {
            int i = currentFrame.getIndex(symbol);
            if (i != 0) {
                return getVariable(frame, i) == DYNAMIC;
            }
            if (currentFrame.materializedParent != null) {
                frame = currentFrame.materializedParent;
            }
            currentFrame = currentFrame.parent;
            if (currentFrame == null) {
                return false;
            }
        }
    }

    public static Dynamic withLexicalBinding(boolean value) {
        return pushDynamic(new ELispSymbol[]{LEXICAL_BINDING}, new Object[]{value});
    }

    public static Dynamic pushDynamic(ELispSymbol[] symbols, Object[] newValues) {
        for (int i = 0; i < symbols.length; i++) {
            newValues[i] = symbols[i].swapThreadLocalValue(newValues[i]);
        }
        return new Dynamic(symbols, newValues);
    }

    public ELispInterpretedClosure.LexicalEnvironment getEnv(VirtualFrame frame) {
        materialize(frame);
        return new ELispInterpretedClosure.LexicalEnvironment(frame.materialize(), this);
    }

    public record LexicalReference(@Nullable MaterializedFrame frame, int index) {
    }

    public record Dynamic(ELispSymbol[] symbols, Object[] prevValues) implements AutoCloseable {
        @Override
        public void close() {
            for (int i = 0; i < symbols.length; i++) {
                symbols[i].swapThreadLocalValue(prevValues[i]);
            }
        }
    }

    public static final class MaterializedAssumption {
        private final Assumption stableMaterializedTop;
        private int lastTop;

        public MaterializedAssumption(Assumption stableMaterializedTop, int lastTop) {
            this.stableMaterializedTop = stableMaterializedTop;
            this.lastTop = lastTop;
        }

        public MaterializedAssumption() {
            this(NON_VAR_SLOT0);
        }

        public MaterializedAssumption(int lastTop) {
            this(Assumption.create(), lastTop);
        }

        public void invalidate(int newTop) {
            if (this.lastTop == NON_VAR_SLOT0) {
                this.lastTop = newTop;
            } else if (this.lastTop != newTop) {
                stableMaterializedTop.invalidate();
            }
        }

        public boolean isValid() {
            return stableMaterializedTop.isValid();
        }

        public void checkEntry(ELispLexical parent) {
            invalidate(parent.topIndex);
        }
    }
}
