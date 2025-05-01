package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.*;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;
import java.util.Arrays;
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
///     it is still dynamically (i.e., globally) bound.
///
/// ### Reusing Frame Slots
///
/// So, if a frame is materialized, some other functions might be referencing slots in this frame,
/// and we must not reuse *those* frame slots. However, this can lead to excessive memory usage
/// in cases like {@code let} in loops, so we do want to reuse slots as much as possible.
///
/// To do this, we keep two stack top slot numbers: [#getMaterializedTop(Frame)] and [#topIndex].
/// In the following examples, we use `||` to denote materialized top and `<` for top index.
///
/// - Example 1: `(let ((x 1) (y 2)) ...)`:
///   - Before: `|| <`
///   - Inner: `|| (1) (2) <`
///   - After: `|| <` (reuse slots)
/// - Example 2: `(let* ((x 1) (f (lambda () x)) (y 2)) ...)`
///   - Before: `|| <`
///   - Inner:
///     - #1: `|| (1) <`
///     - #2: `(1) || (f) <`
///     - #3: `(1) || (f) (2) <`
///   - After: `(1) || <` (`x` referenced by `f`)
///
/// ## Slot Number Caching
///
/// If we don't cache the slot number for variables, we will have to look a symbol up every time,
/// which is slow. However, several operations might change the slot number for the same symbol
/// in the AST, and we use [Assumption assumptions] to notify these changes.
///
/// The changes include:
/// - Materializing the frame in a loop, that is, creating lexical lambdas with each having
///   (slightly) different lexical scope, as is shown in the Example 2 above.
///   - See [StableTopAssumption].
/// - Conditional `defvar`.
///   - See [#markDynamic(VirtualFrame, ELispSymbol)]
///
/// ### Primitives On The Stack
///
/// To avoid excessive GC pressure, it should be a good idea to avoid the cost of boxing and
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
/// ## Optimizations
///
/// The original implementation of [ELispLexical] uses an [ELispLexical] object per lexical scope.
/// So you allocate one scope object every time you visit a `let` node. However, when you have
/// code with `let` in a loop, [ELispLexical] soon becomes the most allocated object, leading to
/// great GC pressure.
///
/// The current implementation has one [ELispLexical] per stack frame, merging upper `let` scopes.
public final class ELispLexical {
    /// Placeholder to mark a symbol "dynamic"
    public final static LexicalReference DYNAMIC = new LexicalReference(null, 0);

    /// A stack frame slot that is definite invalid, to be used as an indicator of `null`
    ///
    /// It should always be zero because some code is using constant 0 instead of this constant.
    /// So changes to this API (and slot assignments) will be breaking.
    public final static int NON_VAR_SLOT0 = 0;
    private final static int MATERIALIZED_TOP_SLOT = 0;
    private final static int LEXICAL_FRAME_SLOT = 1;
    private final static int SPILL_LIST_SLOT = 2;
    private final static int START_SLOT = 3;
    public final static int MAX_SLOTS = 32;

    /// Frame descriptor for a [lexical](https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html)
    /// function
    ///
    /// - Slot #0: an integer, marking the materialized `top` of this frame
    /// - Slot #1: reference to [ELispLexical], keeping track of local variables
    /// - Slot #2: spills array list
    /// - Slot #3~#31: Slots for local variables
    /// - Spills: [ELispLexical] has an [ArrayList] for spilled variables
    private final static FrameDescriptor LEXICAL_DESCRIPTOR;
    /// Frame descriptor for a [dynamic](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html)
    /// function
    ///
    /// - Slot #0: an integer, always `-1` (initialized in [#initFrame(VirtualFrame)]) for easy identification of
    ///   lexical/dynamic scopes
    private final static FrameDescriptor DYNAMIC_DESCRIPTOR;

    static {
        FrameDescriptor.Builder lexical = FrameDescriptor.newBuilder(MAX_SLOTS);
        lexical.addSlots(1, FrameSlotKind.Int);
        lexical.addSlots(MAX_SLOTS - 1, FrameSlotKind.Object);
        LEXICAL_DESCRIPTOR = lexical.build();
        FrameDescriptor.Builder dynamic = FrameDescriptor.newBuilder(1);
        dynamic.addSlots(1, FrameSlotKind.Int);
        DYNAMIC_DESCRIPTOR = dynamic.build();
    }

    public static FrameDescriptor frameDescriptor(boolean lexical) {
        return lexical ? LEXICAL_DESCRIPTOR : DYNAMIC_DESCRIPTOR;
    }

    public static void initFrame(VirtualFrame frame) {
        frame.setInt(MATERIALIZED_TOP_SLOT, -1);
    }

    /// Detects on-stack evaluation of the Truffle debugger
    ///
    /// Truffle debugger evaluates the code in the debugger frame, allowing access
    /// to local variables. Currently, we initialize the frame in [party.iroiro.juicemacs.elisp.nodes.ELispRootNode],
    /// but we should not re-initialize the frame for an on-stack evaluation.
    /// This method serves as a workaround to skip the initialization when the frame is already initialized.
    ///
    /// We also have different frame descriptors for bytecode functions. But we don't support
    /// debugging bytecode nodes (for now), so we don't need to worry about that.
    public static boolean isDebuggerEval(VirtualFrame frame) {
        if (CompilerDirectives.inInterpreter()) {
            if (frame.getFrameDescriptor().getNumberOfSlots() == MAX_SLOTS) { // LEXICAL_DESCRIPTOR
                return frame.isInt(MATERIALIZED_TOP_SLOT); // uninitialized int is not an int
            }
        }

        return false;
    }

    private final StackSizeProfile stackSize;
    private final List<ELispSymbol> args;

    private int variableCount;
    private ELispSymbol[] variables;
    private int[] variableIndices;

    private int topIndex;
    private StableTopAssumption topChanged;
    /// The assumption from the root node
    ///
    /// The sole purpose for it is to satisfy `defvar`: it wants to detect
    /// whether a scope is a root scope.
    private final StableTopAssumption rootAssumption;

    @Nullable
    private final MaterializedFrame materializedParent;
    @Nullable
    private final ELispLexical parent;

    private ELispLexical(
            VirtualFrame frame,
            @Nullable MaterializedFrame parentFrame,
            @Nullable ELispLexical parent,
            List<ELispSymbol> requiredArgs,
            StackSizeProfile stackSize,
            StableTopAssumption assumption
    ) {
        this.args = requiredArgs;
        this.stackSize = stackSize;
        this.variableCount = 0;
        this.variables = new ELispSymbol[stackSize.stackSize()];
        this.variableIndices = new int[stackSize.stackSize()];
        this.topIndex = START_SLOT;
        this.topChanged = this.rootAssumption = assumption;

        this.parent = parent;
        this.materializedParent = parentFrame;

        setMaterializedTop(frame, START_SLOT);
        frame.setObject(LEXICAL_FRAME_SLOT, this);
    }

    /// Copy constructor, used to create nested lexical scopes
    ///
    /// @see #getEnv(VirtualFrame)
    private ELispLexical(ELispLexical other) {
        this.args = other.args;
        this.stackSize = other.stackSize;
        this.variableCount = other.variableCount;
        this.variables = Arrays.copyOf(other.variables, variableCount);
        this.variableIndices = Arrays.copyOf(other.variableIndices, variableCount);
        this.topIndex = other.topIndex;
        this.topChanged = other.topChanged;
        this.parent = other.parent;
        this.materializedParent = other.materializedParent;
        this.rootAssumption = other.rootAssumption;
    }

    /// Creates a lexical scope for a root node
    public static ELispLexical create(VirtualFrame frame, StableTopAssumption assumption, StackSizeProfile stackSize) {
        return new ELispLexical(frame, null, null, List.of(), stackSize, assumption);
    }

    /// Creates a lexical scope for a root node created inside a parent frame
    public static ELispLexical create(
            VirtualFrame frame,
            ELispLexical parent,
            MaterializedFrame parentFrame,
            List<ELispSymbol> requiredArgs,
            StackSizeProfile stackSize,
            StableTopAssumption assumption
    ) {
        parent.materialize(parentFrame);
        return new ELispLexical(frame, parentFrame, parent, requiredArgs, stackSize, assumption);
    }

    private void materialize(VirtualFrame frame) {
        int index = topIndex;
        setMaterializedTop(frame, index);
    }

    public boolean isRootScope() {
        return parent == null && materializedParent == null && rootAssumption == topChanged;
    }

    /// Forks the current frame
    ///
    /// ## Usage
    ///
    /// Please remember to use [#saveState()] to get a previous state and
    /// call [#restore(VirtualFrame, int, Object)] afterward to restore it.
    /// We cannot use [AutoCloseable] for this because a [VirtualFrame]
    /// should not be stored in a field unless it is a [MaterializedFrame]
    /// (which will add some overhead).
    public void fork(StableTopAssumption assumption) {
        assumption.checkEntry(this);
        topChanged = assumption;
    }

    /// Used to save states before [#fork(StableTopAssumption)] for [#restore(VirtualFrame, int, StableTopAssumption)]
    ///
    /// It is quite funny having to use two separate methods for it
    /// ([#saveState1()], [#saveState2()]), and the final code is not
    /// pretty. But this does reduce some allocations compared to using
    /// a wrapper object: Java does not seem able to escape-analyze them
    /// away.
    ///
    /// @see #saveState2()
    public int saveState1() {
        return topIndex;
    }
    /// @see #saveState1()
    public StableTopAssumption saveState2() {
        return topChanged;
    }

    /// Restores the current frame
    ///
    /// @param frame the current backing frame
    /// @param top from [#saveState1()]
    /// @param assumption from [#saveState2()]
    public void restore(VirtualFrame frame, int top, StableTopAssumption assumption) {
        int count = variableCount;
        while (count > 0 && variableIndices[count - 1] >= top) {
            count--;
        }
        variableCount = count;

        int materializedTop = getMaterializedTop(frame);
        topIndex = Math.max(materializedTop, top);
        topChanged = assumption;
    }

    public void addVariable(VirtualFrame frame, ELispSymbol symbol, Object value) {
        int index = topIndex;
        topIndex++;
        addVariable(symbol, index);
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

    private void addVariable(ELispSymbol symbol, int index) {
        int count = variableCount++;
        int length = variableIndices.length;
        if (CompilerDirectives.injectBranchProbability(
                CompilerDirectives.SLOWPATH_PROBABILITY,
                count >= length
        )) {
            int newLength = Math.max(length, 4);
            newLength += Math.min(newLength >> 1, 8);
            variables = Arrays.copyOf(variables, newLength);
            variableIndices = Arrays.copyOf(variableIndices, newLength);
            stackSize.updateStackSize(newLength);
        }
        variables[count] = symbol;
        variableIndices[count] = index;
    }

    private int getIndex(ELispSymbol symbol) {
        int i;
        i = variables.length - 1;
        while (i >= 0 && variables[i] != symbol) {
            i--;
        }
        if (i != -1) {
            return variableIndices[i];
        }
        i = args.indexOf(symbol);
        if (i != -1) {
            return -i - 1;
        }
        return 0;
    }

    @Nullable
    public LexicalReference getLexicalReference(Frame frame, ELispSymbol symbol) {
        Frame virtualFrame = frame;
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
            lb.add(new ELispCons(symbol, unwrapContainer(variable)));
        }
        for (ELispSymbol symbol : args) {
            Object variable = getVariable(frame, getIndex(symbol));
            if (variable == DYNAMIC) {
                continue;
            }
            lb.add(new ELispCons(symbol, unwrapContainer(variable)));
        }
        return lb.buildWithCdr(parent == null ? false : parent.toAssocList(
                materializedParent == null ? frame : materializedParent));
    }

    private Object unwrapContainer(Object variable) {
        return variable instanceof ELispFrameSlotNode.SlotPrimitiveContainer container
                ? container.asObject() : variable;
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

    public Assumption getTopUnchanged() {
        return topChanged.stableTop;
    }

    public void setTopChanged(StableTopAssumption assumption) {
        assumption.checkEntry(this);
        this.topChanged = assumption;
    }

    private static void setMaterializedTop(VirtualFrame frame, int newTop) {
        frame.setInt(MATERIALIZED_TOP_SLOT, newTop);
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

    /// Marks a symbol as dynamically bound in the current lexical scope (`defvar`)
    ///
    /// We do not aim to support conditional `defvar` since it will completely break
    /// our slot number caching.
    public static void markDynamic(VirtualFrame frame, ELispSymbol symbol) {
        ELispLexical lexical = getLexicalFrame(frame);
        if (lexical != null && lexical.getIndex(symbol) == 0) {
            lexical.addVariable(frame, symbol, DYNAMIC);
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
        return new ELispInterpretedClosure.LexicalEnvironment(frame.materialize(), new ELispLexical(this));
    }

    public static final class StackSizeProfile {
        @CompilerDirectives.CompilationFinal
        private int stackSize = 0;

        int stackSize() {
            return stackSize;
        }

        void updateStackSize(int stackSize) {
            if (stackSize > this.stackSize) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                this.stackSize = stackSize;
            }
        }
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

    public static final class StableTopAssumption {
        private final Assumption stableTop;
        private int lastTop;

        public StableTopAssumption(Assumption stableTop, int lastTop) {
            this.stableTop = stableTop;
            this.lastTop = lastTop;
        }

        public StableTopAssumption() {
            this(NON_VAR_SLOT0);
        }

        public StableTopAssumption(int lastTop) {
            this(Assumption.create(), lastTop);
        }

        public void invalidate(int newTop) {
            if (!stableTop.isValid()) {
                return;
            }
            if (this.lastTop == NON_VAR_SLOT0) {
                this.lastTop = newTop;
            } else if (this.lastTop != newTop) {
                stableTop.invalidate();
            }
        }

        public boolean isValid() {
            return stableTop.isValid();
        }

        public void checkEntry(ELispLexical parent) {
            invalidate(parent.topIndex);
        }
    }
}
