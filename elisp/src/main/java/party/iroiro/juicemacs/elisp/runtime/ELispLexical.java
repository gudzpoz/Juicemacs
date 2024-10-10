package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.LEXICAL_BINDING;

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
public final class ELispLexical {
    /// Placeholder to mark a symbol "dynamic"
    public final static LexicalReference DYNAMIC = new LexicalReference(null, 0);

    public final static int NON_VAR_SLOT0 = 0;
    public final static int NON_VAR_SLOT1 = 1;
    private final static int LEXICAL_FRAME_SLOT = 0;
    private final static int MATERIALIZED_TOP_SLOT = 1;
    private final static int SPILL_LIST_SLOT = 2;
    private final static int START_SLOT = 3;
    private final static int MAX_SLOTS = 32;

    private final static FrameDescriptor LEXICAL_DESCRIPTOR;
    private final static FrameDescriptor DYNAMIC_DESCRIPTOR;

    static {
        FrameDescriptor.Builder lexical = FrameDescriptor.newBuilder(MAX_SLOTS);
        lexical.addSlots(1, FrameSlotKind.Object);
        lexical.addSlots(1, FrameSlotKind.Int);
        lexical.addSlots(MAX_SLOTS - 2, FrameSlotKind.Object);
        LEXICAL_DESCRIPTOR = lexical.build();
        FrameDescriptor.Builder dynamic = FrameDescriptor.newBuilder(1);
        dynamic.addSlots(1, FrameSlotKind.Object);
        DYNAMIC_DESCRIPTOR = dynamic.build();
    }

    public static FrameDescriptor frameDescriptor(boolean lexical) {
        return lexical ? LEXICAL_DESCRIPTOR : DYNAMIC_DESCRIPTOR;
    }

    private final List<ELispSymbol> args;
    private final List<ELispSymbol> variables;
    private final List<Integer> variableIndices;
    private boolean materialized;
    private int topIndex;
    @Nullable
    private final MaterializedFrame materializedParent;
    @Nullable
    private final ELispLexical parent;

    public ELispLexical(VirtualFrame frame,
                        @Nullable VirtualFrame parentFrame,
                        @Nullable ELispLexical parent,
                        List<ELispSymbol> args) {
        this.args = args;
        this.variables = new ArrayList<>();
        this.variableIndices = new ArrayList<>();
        this.parent = parent;
        this.materialized = false;
        if (parent == null) {
            // A root frame
            assert parentFrame == null;
            this.materializedParent = null;
            this.topIndex = START_SLOT;
            setMaterializedTop(frame, START_SLOT);
        } else {
            if (parentFrame == null) {
                // A lexical frame (called from [#fork])
                this.materializedParent = null;
                this.topIndex = parent.topIndex;
            } else {
                // A root frame (of a called function)
                parent.materialize(parentFrame);
                this.materializedParent = parentFrame.materialize();
                this.topIndex = START_SLOT;
                setMaterializedTop(frame, START_SLOT);
            }
        }
        frame.setObject(LEXICAL_FRAME_SLOT, this);
    }

    private void materialize(VirtualFrame frame) {
        materialized = true;
        setMaterializedTop(frame, topIndex);
    }

    /**
     * Forks the current frame
     *
     * <p>
     * Please remember to call {@link #restore(VirtualFrame)}.
     * We cannot use {@link AutoCloseable} for this because a {@link VirtualFrame}
     * should not be stored in a field unless it is a {@link MaterializedFrame}
     * (which will add some overhead).
     * </p>
     *
     * @param frame the current backing frame
     * @return a new frame
     */
    public ELispLexical fork(VirtualFrame frame) {
        return new ELispLexical(frame, null, this, List.of());
    }

    /**
     * Restores the current frame
     *
     * @param frame the current backing frame
     */
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
        if (index < MAX_SLOTS) {
            frame.setObject(index, value);
        } else {
            List<Object> spills = getSpills(frame);
            if (spills == null) {
                assert index == MAX_SLOTS;
                spills = new ArrayList<>();
                frame.setObject(SPILL_LIST_SLOT, spills);
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
    private Object getVariableValue(VirtualFrame frame, ELispSymbol symbol) {
        ELispLexical currentFrame = this;
        while (true) {
            int i = currentFrame.getIndex(symbol);
            if (i != 0) {
                return getVariable(frame, i);
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
                return new LexicalReference(frame == virtualFrame ? null : frame, i);
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

    @Nullable
    public static ELispLexical getLexicalFrame(VirtualFrame frame) {
        return (ELispLexical) frame.getObject(LEXICAL_FRAME_SLOT);
    }

    @SuppressWarnings("unchecked")
    @Nullable
    private static List<Object> getSpills(VirtualFrame frame) {
        return (List<Object>) frame.getObject(SPILL_LIST_SLOT);
    }

    public static int getMaterializedTop(VirtualFrame frame) {
        if (getLexicalFrame(frame) == null) {
            return -1;
        }
        return frame.getInt(MATERIALIZED_TOP_SLOT);
    }

    private static void setMaterializedTop(VirtualFrame frame, int value) {
        frame.setInt(MATERIALIZED_TOP_SLOT, value);
    }

    public static Object getVariable(VirtualFrame frame, int i) {
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
        ELispLexical lexicalFrame = getLexicalFrame(frame);
        return symbol.isSpecial() || (lexicalFrame != null && lexicalFrame.getVariableValue(frame, symbol) == DYNAMIC);
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

    public record LexicalReference(@Nullable VirtualFrame frame, int index) {
    }

    public record Dynamic(ELispSymbol[] symbols, Object[] prevValues) implements AutoCloseable {
        @Override
        public void close() {
            for (int i = 0; i < symbols.length; i++) {
                symbols[i].swapThreadLocalValue(prevValues[i]);
            }
        }
    }
}
