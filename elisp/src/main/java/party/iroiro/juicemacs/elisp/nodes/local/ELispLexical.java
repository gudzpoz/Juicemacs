package party.iroiro.juicemacs.elisp.nodes.local;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.Node;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;
import java.util.Arrays;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asSym;

/// Represents a new sub-frame introduced by some AST nodes like `let` or `condition-case`
///
/// A lexical block is introduced at every `let/let*` node,
/// where a new [VirtualFrame] is allocated and requires
/// a new tracker for variables introduced in this scope.
///
/// For `let*` nodes, though, each statement in it may have different variable access scopes,
/// which is why [Scope] exists.
public final class ELispLexical {
    /// Placeholder to mark a symbol "dynamic"
    public static final LexicalReference DYNAMIC = new ELispLexical.LexicalReference(0, -1);
    public static final int UPPER_FRAME_SLOT = 0;
    public static final int FRAME_SLOT_START = 1;
    public static final int DYNAMIC_VARIABLE_SLOT = -1;
    private static final Object[] EMPTY_ARRAY = new Object[0];

    /// Frame descriptor for a [dynamic](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html)
    /// function
    private final static FrameDescriptor DYNAMIC_DESCRIPTOR;

    static {
        DYNAMIC_DESCRIPTOR = FrameDescriptor.newBuilder().build();
    }

    @TruffleBoundary
    public static FrameDescriptor rootFrameDescriptor(int args, boolean lexical) {
        if (!lexical) {
            return DYNAMIC_DESCRIPTOR;
        }
        FrameDescriptor.Builder builder = FrameDescriptor.newBuilder(args);
        builder.addSlots(1, FrameSlotKind.Object);
        builder.addSlots(args, FrameSlotKind.Illegal);
        return builder.build();
    }

    @SuppressWarnings({"unchecked", "TypeParameterUnusedInFormals"})
    private static <T> T fastCast(Object value) {
        return (T) value;
    }

    public static MaterializedFrame getFrameSlot(Frame frame) {
        Object o = frame.getObject(UPPER_FRAME_SLOT);
        return fastCast(o);
    }

    @SuppressWarnings("TypeParameterUnusedInFormals")
    public static <T> T getCallee(Frame frame) {
        Object o = frame.getArguments()[0];
        return fastCast(o);
    }

    @Nullable
    private final Scope upperScope;

    private final ArrayList<ELispSymbol> symbols;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] frameSlots;
    private final FrameDescriptor descriptor;

    /// Allocates a block for the symbols
    ///
    /// By default, all symbols are marked as lexical. And
    /// the caller is responsible for calling [#markAsDynamic(ELispSymbol)]
    /// for any dynamic symbols.
    private ELispLexical(@Nullable Scope upperScope, ELispSymbol[] symbols) {
        this.upperScope = upperScope;
        this.symbols = new ArrayList<>(Arrays.asList(symbols));
        this.frameSlots = new int[symbols.length];
        for (int i = 0; i < symbols.length; i++) {
            frameSlots[i] = i + FRAME_SLOT_START;
        }
        FrameDescriptor.Builder builder = FrameDescriptor.newBuilder(FRAME_SLOT_START + frameSlots.length);
        builder.addSlots(1, FrameSlotKind.Object);
        builder.addSlots(frameSlots.length, FrameSlotKind.Illegal);
        this.descriptor = builder.build();
    }

    @Nullable
    public Scope upperScope() {
        return upperScope;
    }

    public ELispSymbol getSymbol(int i) {
        return symbols.get(i);
    }

    public int[] slots() {
        return frameSlots;
    }

    public FrameDescriptor descriptor() {
        return descriptor;
    }

    public void markAsDynamic(ELispSymbol symbol) {
        int i = findSymbol(symbol, frameSlots.length);
        if (i == -1) {
            symbols.add(symbol);
        } else if (i < frameSlots.length) {
            frameSlots[i] = DYNAMIC_VARIABLE_SLOT;
        } // else: variables appended by defvar - all dynamic
    }

    /// @return index of symbol in [#symbols], `-1` when not found
    private int findSymbol(ELispSymbol symbol, int limit) {
        if (limit == frameSlots.length) {
            limit = symbols.size();
        }
        for (int i = limit - 1; i >= 0; i--) {
            if (symbols.get(i) == symbol) {
                return i;
            }
        }
        return -1;
    }

    public Scope newScope(int limit) {
        return new Scope(this, limit);
    }

    public VirtualFrame newFrame(VirtualFrame upperFrame) {
        VirtualFrame frame = Truffle.getRuntime().createVirtualFrame(EMPTY_ARRAY, descriptor);
        frame.setObject(UPPER_FRAME_SLOT, upperFrame.materialize());
        return frame;
    }

    public static ELispLexical newRoot() {
        return new ELispLexical(null, new ELispSymbol[0]);
    }

    @Nullable
    public static ELispLexical newBlock(Node currentNode, ELispSymbol[] symbols) {
        Scope scope = getScope(currentNode);
        if (scope == null) {
            return null;
        }
        return new ELispLexical(scope, symbols);
    }

    public static ELispLexical newBlock(@Nullable Scope scope, ELispSymbol[] symbols) {
        return new ELispLexical(scope, symbols);
    }

    /// Add variables from a list in reversed direction
    ///
    /// This has to be done reversedly because of Emacs environment semantics:
    /// - When a new variable is declared in Emacs Lisp, a new `cons` is *pushed*
    ///   onto `Vinternal_interpreter_environment` (an association list).
    /// - Therefore, in `(lambda () (let* ((x 1) (y 2)) (debug)))`, when `(debug)`
    ///   is executed, the environment is `((y . 2) (x . 1))`.
    /// - Emacs captures the current environment directly into closures.
    ///
    /// However, we store variables in [#symbols], with new variables added at the tail.
    /// So when we turn closures back into our internal
    /// [party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure],
    /// we need to store variables reversedly.
    ///
    /// Also, Emacs oclosures directly depend on the order of the list items.
    ///
    /// @see Scope#toAssocList(Frame)
    @TruffleBoundary
    public static ELispLexical.Scope newBlockFromAlist(MaterializedFrame frame, ELispCons cons) {
        ArrayList<ELispSymbol> symbols = new ArrayList<>();
        for (int i = 0, count = cons.size(); i < count; i++) {
            symbols.add(NIL);
        }
        ELispLexical block = new ELispLexical(null, symbols.toArray(new ELispSymbol[0]));
        ConsIterator i = cons.listIterator(0);
        int index = block.frameSlots.length - 1;
        while (i.hasNext()) {
            if (i.next() instanceof ELispCons binding) {
                ELispSymbol symbol = asSym(binding.car());
                Object value = binding.cdr();
                block.symbols.set(index, symbol);
                setVariable(frame, block.frameSlots[index], value);
            }
            index--;
        }
        return block.newScope(block.frameSlots.length);
    }

    private static void setVariable(VirtualFrame frame, int slot, Object value) {
        switch (value) {
            case Long l -> {
                frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Long);
                frame.setLong(slot, l);
            }
            case Double d -> {
                frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Double);
                frame.setDouble(slot, d);
            }
            default -> {
                frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Object);
                frame.setObject(slot, value);
            }
        }
    }

    @TruffleBoundary
    @Nullable
    public static Scope getScope(Node currentNode) {
        while (currentNode != null) {
            if (currentNode instanceof ScopeProvider provider) {
                Scope scope = provider.getScope();
                if (scope != null) {
                    return scope;
                }
            }
            currentNode = currentNode.getParent();
        }
        return null;
    }

    @Nullable
    public static LexicalReference getLexicalReference(Node currentNode, ELispSymbol symbol) {
        Scope scope = getScope(currentNode);
        return scope == null ? null : scope.getReference(symbol);
    }

    @TruffleBoundary
    public static boolean isRootScope(Node currentNode) {
        while (currentNode != null) {
            if (currentNode instanceof ScopeProvider provider) {
                ELispLexical.@Nullable Scope scope = provider.getScope();
                if (scope != null) {
                    return false;
                }
            }
            currentNode = currentNode.getParent();
        }
        return true;
    }

    /// Marks a symbol as dynamically bound in the current lexical scope (`defvar`)
    ///
    /// We do not aim to support conditional `defvar` since it will completely break
    /// our slot number caching.
    public static void markAsDynamic(Node currentNode, ELispSymbol symbol) {
        ELispLexical.@Nullable Scope scope = getScope(currentNode);
        if (scope != null) {
            scope.block.markAsDynamic(symbol);
        }
    }

    public interface ScopeProvider {
        @Nullable Scope getScope();
    }

    /// Represents variable-visibility scopes
    ///
    /// For example, the `let*` form below:
    /// ```elisp
    /// (let* ((a 1) (b (+ a 2)))
    ///   (+ a b 3))
    /// ```
    ///
    /// `(+ a 2)` can only access variable `a`, while the body
    /// `(+ a b 3)` can access all variables defined by `let*`.
    /// These forms have different variable-visibility scopes,
    /// but share the same [VirtualFrame] (and [ELispLexical]).
    ///
    /// @param block the block this scope is in
    /// @param limit the upper index of accessible variables
    public record Scope(ELispLexical block, int limit) {
        @Nullable
        public LexicalReference getReference(ELispSymbol symbol) {
            LexicalReference rawReference = getRawReference(symbol);
            return rawReference == DYNAMIC ? null : rawReference;
        }

        @Nullable
        private synchronized LexicalReference getRawReference(ELispSymbol symbol) {
            int level = 0;
            Scope scope = this;
            while (scope != null) {
                int limit = scope.limit;
                ELispLexical block = scope.block;
                int i = block.findSymbol(symbol, limit);
                if (i >= block.frameSlots.length) {
                    return DYNAMIC;
                }
                if (i != -1 && i < limit) {
                    int slot = block.frameSlots[i];
                    return slot == DYNAMIC_VARIABLE_SLOT ? DYNAMIC : new ELispLexical.LexicalReference(level, slot);
                }
                scope = block.upperScope;
                level++;
            }
            return null;
        }

        public boolean isDynamic(ELispSymbol symbol) {
            return getRawReference(symbol) == DYNAMIC;
        }

        public Scope getRootScope() {
            Scope scope = this;
            while (scope.block.upperScope != null) {
                scope = scope.block.upperScope;
            }
            return scope;
        }

        /// Convert the variables into an assoc list compatible with Emacs's closures
        ///
        /// Note that the insertion order of the list matters.
        public synchronized Object toAssocList(Frame frame) {
            ELispCons.ListBuilder lb = new ELispCons.ListBuilder();
            for (int i = limit - 1; i >= 0; i--) {
                ELispSymbol symbol = block.symbols.get(i);
                int slot = block.frameSlots[i];
                if (slot == ELispLexical.DYNAMIC_VARIABLE_SLOT) {
                    continue;
                }
                Object value = frame.getValue(slot);
                lb.add(ELispCons.cons(symbol, value));
            }
            Scope upperScope = block.upperScope;
            return lb.buildWithCdr(upperScope == null
                    ? false
                    : upperScope.toAssocList(getFrameSlot(frame)));
        }
    }

    public record LexicalReference(int level, int index) {
    }

    public record Captured(Scope scope, MaterializedFrame frame) {
    }
}
