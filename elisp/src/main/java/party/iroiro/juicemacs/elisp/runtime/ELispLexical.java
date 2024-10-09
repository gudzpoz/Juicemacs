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
import java.util.concurrent.atomic.AtomicInteger;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.LEXICAL_BINDING;

/**
 * Basically our implementation of GNU Emacs' {@code internal-interpreter-environment}
 *
 * <ul>
 * <li>Dynamic bindings are simply utility functions that swap values in and out.</li>
 * <li>Lexical bindings are more complex, whose implementation entails a few functions
 * in {@link BuiltInEval} ({@code src/eval.c}).</li>
 * </ul>
 *
 * <h2>Lexical Scoping</h2>
 * <p>
 * Whether a variable is lexical or dynamic is determined by several things in GNU Emacs:
 * </p>
 * <ul>
 * <li>If the {@code special} field of a symbol is set, {@code let/let*} statements should
 * always bind it dynamically.</li>
 * <li>The arguments of a function defined in a lexical environment is always lexically bound.</li>
 * <li>If a symbol is somehow marked "special" in {@code internal-interpreter-environment},
 * then it should be bound dynamically by {@code let/let*}.</li>
 * <li>If a symbol is found "normal" in {@code internal-interpreter-environment}, it is
 * lexically bound and looked up in {@code internal-interpreter-environment}. Otherwise,
 * it is still dynamically (i.e. globally) bound.</li>
 * </ul>
 */
public final class ELispLexical {
    /**
     * Placeholder to mark a symbol "dynamic"
     */
    public final static LexicalReference DYNAMIC = new LexicalReference(null, 0);

    private final static int LEXICAL_FRAME_SLOT = 0;
    private final static int SPILL_LIST_SLOT = 1;
    private final static int START_SLOT = 2;
    private final static int MAX_SLOTS = 32;

    private final List<ELispSymbol> args;
    private final List<ELispSymbol> variables;
    private final List<Integer> variableIndices;
    private final int baseIndex;
    private final AtomicInteger nextIndex;
    @Nullable
    private final MaterializedFrame materializedParent;
    @Nullable
    private final ELispLexical parent;
    @Nullable
    private List<Object> spills;
    /**
     * If materialized, some other functions might be referencing slots in this frame,
     * and we must not reuse frame slots. Otherwise, we can reuse frame slots to avoid
     * excessive memory usage in cases like {@code let} in loops.
     */
    private boolean materialized;

    public ELispLexical(VirtualFrame frame,
                        @Nullable VirtualFrame parentFrame,
                        @Nullable ELispLexical parent,
                        List<ELispSymbol> args) {
        this(frame, parentFrame, parent, args, new AtomicInteger(START_SLOT));
    }

    private ELispLexical(VirtualFrame frame,
                         @Nullable VirtualFrame parentFrame,
                         @Nullable ELispLexical parent,
                         List<ELispSymbol> args,
                         AtomicInteger nextIndex) {
        this.args = args;
        this.variables = new ArrayList<>();
        this.variableIndices = new ArrayList<>();
        this.baseIndex = nextIndex.get();
        this.nextIndex = nextIndex;
        this.materializedParent = parentFrame == null ? null : parentFrame.materialize();
        this.parent = parent;
        this.spills = null;
        if (parentFrame != null && parent != null) {
            parent.materialized = true;
            this.materialized = false;
        } else {
            this.materialized = parent != null && parent.materialized;
        }
        frame.setObject(LEXICAL_FRAME_SLOT, this);
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
        return new ELispLexical(frame, null, this, List.of(), this.nextIndex);
    }

    /**
     * Restores the current frame
     *
     * @param frame the current backing frame
     */
    public void restore(VirtualFrame frame) {
        if (!materialized) {
            nextIndex.set(baseIndex + variables.size());
        }
        frame.setObject(LEXICAL_FRAME_SLOT, this);
    }

    private void checkSpills(VirtualFrame frame, int index) {
        if (index < MAX_SLOTS) {
            return;
        }
        if (spills == null) {
            spills = new ArrayList<>();
            frame.setObject(SPILL_LIST_SLOT, spills);
        }
        while (spills.size() <= index - MAX_SLOTS) {
            spills.add(null);
        }
    }

    public int addVariable(VirtualFrame frame, ELispSymbol symbol) {
        int index = nextIndex.getAndIncrement();
        variables.add(symbol);
        variableIndices.add(index);
        checkSpills(frame, index);
        return index;
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
                if (i > 0 && frame.getObject(i) == DYNAMIC) {
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

    public static FrameDescriptor frameDescriptor() {
        FrameDescriptor.Builder builder = FrameDescriptor.newBuilder(MAX_SLOTS);
        builder.addSlots(MAX_SLOTS, FrameSlotKind.Object);
        return builder.build();
    }

    public static void markDynamic(VirtualFrame frame, ELispSymbol symbol) {
        ELispLexical lexicalFrame = getLexicalFrame(frame);
        if (lexicalFrame != null) {
            int index = lexicalFrame.addVariable(frame, symbol);
            setVariable(frame, index, DYNAMIC);
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
        this.materialized = true;
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
