package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.*;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LEXICAL_BINDING;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asSym;

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
/// ## Slot Number Assigning
///
/// We take a simple approach against slot number assigning:
///
/// - We do not reuse slot numbers.
/// - An [Allocator] (an atomically incrementing integer) is used to allocate slot numbers.
///
/// This simplifies things for us quite a bit:
///
/// - Each slot/variable can specialize for their `double`/`long` types to avoid boxing.
/// - In most cases, closures can use the parent frame without copying.
///
/// ## Per-Iteration Scope
///
/// However, things get complicated when there are loops involved:
///
/// ```elisp
/// ;; Example 1
/// (while (condition)
///     (let ((i (get-i)))
///         (setq f (lambda () i))))
/// ;; Example 2
/// (while (condition)
///     (let ((i (get-i)))
///         (setq reader (lambda () i)
///               writer (lambda () (setq i (1+ i))))))
/// ```
///
/// In Example 1, all `i` has identical slot numbers, but each closure expects a dedicated `i`.
/// In Example 2, we have different `i`, but each pair of `reader` and `writer` should share
/// the same `i`.
///
/// We handle this with [#updateInnerClosures] and [#captureEnv]:
/// - `captureEnv` is called when a closure is created, "registering" the closure as a [ScopeUpdatable].
/// - `updateInnerClosures` is called by loop nodes. When a loop iteration ends and there are closures registered,
///   it copies the current frame and updates the frame references held by [ScopeHolder].
public record ELispLexical(
        @Nullable ELispLexical parentScope,
        @Nullable MaterializedFrame materializedParent,
        List<ELispSymbol> symbols,
        IntArrayList slots
) {
    /// Placeholder to mark a symbol "dynamic"
    public final static LexicalReference DYNAMIC = new LexicalReference(null, 0);

    /// A stack frame slot that is definite invalid, to be used as an indicator of `null`
    ///
    /// It should always be zero because some code is using constant 0 instead of this constant.
    /// So changes to this API (and slot assignments) will be breaking.
    public final static int NON_VAR_SLOT0 = 0;
    /// An invalid slot stored in [#slots] to represent a local `defvar` (marking var as dynamic)
    public final static int NON_VAR_DYNAMIC = -1;
    public final static int MAX_SLOTS = 32;
    /// Slot hosting a [LinkedList], used for per-iteration scopes
    ///
    /// @see ELispLexical
    private final static int FRAME_COPY_SLOT = 0;
    /// Slot hosting a [SpilledSlots] (or `null`)
    private final static int SPILL_LIST_SLOT = 1;
    private final static int START_SLOT = 2;
    /// Frame descriptor for a [lexical](https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html)
    /// function
    ///
    /// - Slot #0: block-scope [VirtualFrame] or [FrameSlotKind#Illegal]
    /// - Slot #1: spills array
    /// - Slot #2~#31: Slots for local variables
    ///
    /// @see #FRAME_COPY_SLOT
    /// @see #SPILL_LIST_SLOT
    private final static FrameDescriptor LEXICAL_DESCRIPTOR;
    /// Frame descriptor for a [dynamic](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html)
    /// function
    private final static FrameDescriptor DYNAMIC_DESCRIPTOR;

    static {
        FrameDescriptor.Builder lexical = FrameDescriptor.newBuilder(MAX_SLOTS);
        lexical.addSlots(START_SLOT, FrameSlotKind.Object);
        lexical.addSlots(MAX_SLOTS - START_SLOT, FrameSlotKind.Illegal);
        LEXICAL_DESCRIPTOR = lexical.build();
        DYNAMIC_DESCRIPTOR = FrameDescriptor.newBuilder().build();
    }

    public static FrameDescriptor frameDescriptor(boolean lexical) {
        return lexical ? LEXICAL_DESCRIPTOR.copy() : DYNAMIC_DESCRIPTOR;
    }

    @SuppressWarnings("unchecked")
    static LinkedList<ScopeUpdatable> getFrameSlot(VirtualFrame frame) {
        Object o = frame.getObject(FRAME_COPY_SLOT);
        if (o == null) {
            o = new LinkedList<>();
            frame.setObject(FRAME_COPY_SLOT, o);
        }
        return (LinkedList<ScopeUpdatable>) o;
    }

    /// Updates closures containing references to per-iteration scopes
    ///
    /// It should only be called from loop nodes (currently only
    /// [party.iroiro.juicemacs.elisp.forms.BuiltInEval.FWhile.RepeatingBodyNode]).
    ///
    /// @see ELispLexical
    /// @see #captureEnv(VirtualFrame, Node, ScopeHolder)
    public static void updateInnerClosures(VirtualFrame frame, Node loopNode) {
        LinkedList<ScopeUpdatable> updateNeeded = getFrameSlot(frame);
        ListIterator<ScopeUpdatable> i = updateNeeded.listIterator();
        ArrayList<ELispLexical> branch = new ArrayList<>();
        @Nullable MaterializedFrame copy = null;
        while (i.hasNext()) {
            ScopeUpdatable u = i.next();
            if (u.node == loopNode) {
                i.remove();
                ELispLexical boundary = Objects.requireNonNull(getScope(loopNode));
                ELispLexical head = u.holder.getScope();
                do {
                    branch.add(head);
                    head = head.parentScope;
                } while (head != null && head != boundary);
                if (copy == null) {
                    copy = Truffle.getRuntime().createVirtualFrame(new Object[0], frame.getFrameDescriptor())
                            .materialize();
                    frame.copyTo(0, copy, 0, MAX_SLOTS);
                }
                ELispLexical updated = boundary.fork().withParentFrame(frame.materialize());
                boolean atTarget = false;
                for (int j = branch.size() - 1; j >= 0; j--) {
                    ELispLexical scope = branch.get(j);
                    updated = scope.withParentScope(updated);
                    if (scope == u.scope) {
                        atTarget = true;
                    } else if (atTarget) {
                        atTarget = false;
                        updated = updated.withParentFrame(copy);
                    }
                }
                if (atTarget) {
                    updated = updated.fork().withParentFrame(copy);
                }
                u.holder.setScope(updated);
                branch.clear();
            }
        }
    }

    @Nullable
    public static Object getVariable(Frame frame, int i) {
        if (i >= MAX_SLOTS) {
            int slot = i - MAX_SLOTS;
            SpilledSlots spilled = (SpilledSlots) frame.getObject(SPILL_LIST_SLOT);
            if (spilled == null || spilled.slots.length <= slot) {
                return null;
            } else {
                return spilled.slots[slot];
            }
        }
        return frame.getValue(i);
    }

    public static void setVariable(VirtualFrame frame, int i, Object value) {
        if (i >= MAX_SLOTS) {
            SpilledSlots spills = (SpilledSlots) frame.getObject(SPILL_LIST_SLOT);
            int index = i - MAX_SLOTS;
            if (spills == null || spills.slots.length <= index) {
                int length = Math.max(index + (index >> 1), 8);
                if (spills != null) {
                    Object[] newSpills = new Object[length];
                    System.arraycopy(spills.slots, 0, newSpills, 0, spills.slots.length);
                    spills.slots = newSpills;
                } else {
                    spills = new SpilledSlots(length);
                    frame.setObject(SPILL_LIST_SLOT, spills);
                }
            }
            spills.slots[index] = value;
        } else {
            frame.setObject(i, value);
        }
    }

    /// Marks a symbol as dynamically bound in the current lexical scope (`defvar`)
    ///
    /// We do not aim to support conditional `defvar` since it will completely break
    /// our slot number caching.
    public static void markDynamic(Node currentNode, ELispSymbol symbol) {
        @Nullable ELispLexical scope = getScope(currentNode);
        if (scope != null) {
            scope.markDynamic(symbol);
        }
    }

    public static Dynamic withLexicalBinding(boolean value) {
        return pushDynamic(LEXICAL_BINDING, value);
    }

    public static Dynamic pushDynamic(ELispSymbol symbol, Object newValue) {
        return new DynamicSingle(symbol, symbol.swapThreadLocalValue(newValue));
    }

    public static Dynamic pushDynamic(ELispSymbol[] symbols, Object[] newValues) {
        for (int i = 0; i < symbols.length; i++) {
            newValues[i] = symbols[i].swapThreadLocalValue(newValues[i]);
        }
        return new DynamicBatch(symbols, newValues);
    }

    public static Dynamic preparePopDynamic(ELispSymbol[] symbols, Object[] oldValues) {
        return new DynamicBatch(symbols, oldValues);
    }

    @Nullable
    public static LexicalReference getLexicalReference(Node currentNode, ELispSymbol symbol) {
        ELispLexical scope = getScope(currentNode);
        return scope == null ? null : scope.getReference(symbol);
    }

    public synchronized int getSlot(ELispSymbol symbol) {
        for (int i = 0; i < symbols.size(); i++) {
            if (symbol == symbols.get(i)) {
                return slots.get(i);
            }
        }
        return NON_VAR_SLOT0;
    }

    /// Convert the variables into an assoc list compatible with Emacs's closures
    ///
    /// Note that the insertion order of the list matters.
    ///
    /// @see #addVariablesFromAlist(Allocator, VirtualFrame, ELispCons)
    public synchronized Object toAssocList(Frame frame) {
        ELispCons.ListBuilder lb = new ELispCons.ListBuilder();
        for (int i = symbols.size() - 1; i >= 0; i--) {
            ELispSymbol symbol = symbols.get(i);
            int slot = slots.get(i);
            if (slot == NON_VAR_DYNAMIC) {
                continue;
            }
            Object value = Objects.requireNonNull(getVariable(frame, slot));
            lb.add(new ELispCons(symbol, unwrapContainer(value)));
        }
        return lb.buildWithCdr(parentScope == null
                ? false
                : parentScope.toAssocList(Objects.requireNonNullElse(materializedParent, frame)));
    }

    private Object unwrapContainer(Object variable) {
        return variable instanceof ELispFrameSlotNode.SlotPrimitiveContainer container
                ? container.asObject() : variable;
    }

    public synchronized int addVariable(Node currentNode, ELispSymbol symbol) {
        return addVariable(Objects.requireNonNull(findRootScope(currentNode)), symbol);
    }

    public synchronized int addVariable(Allocator counter, ELispSymbol symbol) {
        int slot = counter.nextSlot();
        this.symbols.add(symbol);
        this.slots.add(slot);
        return slot;
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
    /// @see #toAssocList(Frame)
    public synchronized void addVariablesFromAlist(Allocator counter, VirtualFrame frame, ELispCons cons) {
        for (int i = 0, count = cons.size(); i < count; i++) {
            this.symbols.add(NIL);
            this.slots.add(counter.nextSlot());
        }
        ELispCons.BrentTortoiseHareIterator i = cons.listIterator(0);
        int index = this.slots.size() - 1;
        while (i.hasNext()) {
            ELispCons binding = asCons(i.next());
            ELispSymbol symbol = asSym(binding.car());
            Object value = binding.cdr();
            symbols.set(index, symbol);
            setVariable(frame, slots.get(index), value);
            index--;
        }
    }

    public synchronized void markDynamic(ELispSymbol symbol) {
        if (getSlot(symbol) == NON_VAR_SLOT0) {
            this.symbols.add(symbol);
            this.slots.add(NON_VAR_DYNAMIC);
        }
    }

    @Nullable
    public LexicalReference getReference(ELispSymbol symbol) {
        @Nullable LexicalReference rawReference = getRawReference(symbol);
        return rawReference == DYNAMIC ? null : rawReference;
    }

    @Nullable
    private synchronized LexicalReference getRawReference(ELispSymbol symbol) {
        MaterializedFrame frame = null;
        ELispLexical scope = this;
        while (scope != null) {
            int slot = scope.getSlot(symbol);
            if (slot != NON_VAR_SLOT0) {
                return slot == NON_VAR_DYNAMIC ? DYNAMIC : new LexicalReference(frame, slot);
            }
            if (scope.materializedParent() != null) {
                frame = scope.materializedParent();
            }
            scope = scope.parentScope();
        }
        return null;
    }

    public boolean isDynamic(ELispSymbol symbol) {
        return getRawReference(symbol) == DYNAMIC;
    }

    public ELispLexical fork() {
        return newRoot(this, null);
    }

    public ELispLexical withParentFrame(MaterializedFrame frame) {
        return new ELispLexical(parentScope, frame, symbols, slots);
    }

    public ELispLexical withParentScope(ELispLexical scope) {
        return new ELispLexical(scope, materializedParent, symbols, slots);
    }

    /// @see #updateInnerClosures(VirtualFrame, Node)
    public ELispLexical captureEnv(VirtualFrame frame, Node currentNode, ScopeHolder holder) {
        LinkedList<ScopeUpdatable> frameSlot = getFrameSlot(frame);
        @Nullable ELispLexical updateNeeded = null;
        while (currentNode != null) {
            if (currentNode instanceof ScopeProvider provider) {
                @Nullable ELispLexical scope = provider.lexicalScope();
                if (scope == null) {
                    continue;
                }
                if (scope.materializedParent != null) {
                    break;
                }
                if (updateNeeded == null) {
                    updateNeeded = scope;
                }
            } else if (currentNode instanceof BuiltInEval.FWhile.RepeatingBodyNode loop) {
                if (updateNeeded != null) {
                    frameSlot.addFirst(new ScopeUpdatable(loop, updateNeeded, holder));
                    loop.notifyScopeUpdateNeeded();
                    updateNeeded = null;
                }
            }
            currentNode = currentNode.getParent();
        }
        return newRoot(this, frame.materialize());
    }

    public static boolean isRootScope(Node currentNode) {
        while (currentNode != null) {
            if (currentNode instanceof ScopeProvider provider) {
                @Nullable ELispLexical scope = provider.lexicalScope();
                if (scope != null) {
                    return scope.parentScope == null && provider.rootScope() != null;
                }
            }
            currentNode = currentNode.getParent();
        }
        return true;
    }

    @Nullable
    private static Allocator findRootScope(Node currentNode) {
        while (currentNode != null) {
            if (currentNode instanceof ScopeProvider provider) {
                @Nullable Allocator scope = provider.rootScope();
                if (scope != null) {
                    return scope;
                }
            }
            currentNode = currentNode.getParent();
        }
        return null;
    }

    @Nullable
    public static ELispLexical getScope(Node currentNode) {
        while (currentNode != null) {
            if (currentNode instanceof ScopeProvider provider) {
                @Nullable ELispLexical scope = provider.lexicalScope();
                if (scope != null) {
                    return scope;
                }
            }
            currentNode = currentNode.getParent();
        }
        return null;
    }

    public static ELispLexical newRoot() {
        return new ELispLexical(null, null, new ArrayList<>(), new IntArrayList());
    }

    public static ELispLexical newRoot(@Nullable ELispLexical parentScope, @Nullable MaterializedFrame parentFrame) {
        return new ELispLexical(parentScope, parentFrame, new ArrayList<>(), new IntArrayList());
    }

    public interface ScopeProvider {
        @Nullable
        ELispLexical lexicalScope();
        @Nullable
        default Allocator rootScope() {
            return null;
        }
    }

    public interface ScopeHolder {
        ELispLexical getScope();
        void setScope(ELispLexical scope);
    }

    private record ScopeUpdatable(Node node, ELispLexical scope, ScopeHolder holder) {
    }

    public static final class Allocator {
        private static final AtomicIntegerFieldUpdater<Allocator> TOP_INDEX =
                AtomicIntegerFieldUpdater.newUpdater(Allocator.class, "topIndex");

        private volatile int topIndex;

        public Allocator() {
            this.topIndex = START_SLOT;
        }

        public int nextSlot() {
            return TOP_INDEX.getAndIncrement(this);
        }
    }

    public record LexicalReference(@Nullable MaterializedFrame frame, int index) {
    }

    private static final class SpilledSlots {
        private Object[] slots;
        private SpilledSlots(int size) {
            slots = new Object[size];
        }
    }

    public interface Dynamic extends AutoCloseable {
        void close();
    }
    private record DynamicBatch(ELispSymbol[] symbols, Object[] prevValues) implements Dynamic {
        @Override
        public void close() {
            for (int i = 0; i < symbols.length; i++) {
                symbols[i].swapThreadLocalValue(prevValues[i]);
            }
        }
    }
    private record DynamicSingle(ELispSymbol symbol, Object prevValue) implements Dynamic {
        @Override
        public void close() {
            symbol.swapThreadLocalValue(prevValue);
        }
    }
}
