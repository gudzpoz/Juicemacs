package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.HashSet;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.asSym;


/**
 * Port of {@code struct Lisp_Symbol} to Java
 *
 * <h2>Values & Function Values: GNU Emacs Logic</h2>
 * <p>
 * An Emacs Lisp symbol is a container to a value slot and a function value, which are
 * independent of each other. I will start with the relatively simple function value:
 * it can either point to another ELisp symbol, which means that the function is aliased
 * to that symbol's function (recursively), or it can point to literally any object, whose
 * meaning depend on the interpretation of {@code eval_sub}. (It can mean anything from
 * functions to macros, or even keymaps?)
 * </p>
 * <p>
 * The value slot is a lot more complex. It can be one of the following:
 * </p>
 * <ol>
 * <li>When {@code lexical-binding} is enabled, the value of a symbol, when interpreted by
 * the interpreter, comes from a lexical scope, instead of the value slot. (Note that normal
 * functions like {@code symbol-value} ignore lexical-bound values.)</li>
 * <li>Otherwise, if the slot is {@code SYMBOL_PLAINVAL}, the value is read directly from
 * the value slot. GNU Emacs uses an uninterned {@code UNBOUND} symbol as the value of
 * void variables.</li>
 * <li>If the slot is {@code SYMBOL_VARALIAS}, the slot should contain a symbol, which
 * is the alias of the variable. The value of the variable is the value of the alias.</li>
 * <li>If the slot is {@code SYMBOL_LOCALIZED}, the value is a buffer-local value. The
 * value of the variable is either the value of the buffer-local value, or a default
 * value, if the buffer-local value is not set.</li>
 * <li>If the slot is {@code SYMBOL_FORWARDED}, the value is stored in a field of a C struct,
 * which may be a global struct or a buffer (or a keyboard object?).</li>
 * </ol>
 *
 * <h2>Values: Our Implementation</h2>
 * <p>
 * First, we add a thread-local value to the symbol, which further complicates the logic.
 * Basically, the priority is: (1) lexically-bound values; (2) thread-local values; (3)
 * other values stored in the value slot.
 * </p>
 */
public final class ELispSymbol implements ELispValue {
    /// The unbound symbol, used as the value of void variables and not exposed to any caller.
    ///
    /// Qunbound is uninterned, so that it's not confused with any symbol
    /// 'unbound' created by a Lisp program.
    ///
    /// This symbol is not put in [ELispContext] because of recursive dependency.
    public final static ELispSymbol UNBOUND = new ELispSymbol("unbound");

    /**
     * Indicates where the value can be found.
     */
    private Value value;
    /**
     * Indicates whether the value is thread-locally overridden.
     */
    @Nullable
    private ThreadLocalValue threadLocalValue = null;
    /**
     * Indicates whether operations are needed before any writes.
     */
    private TrappedWrite trappedWrite;
    /**
     * Interned state of the symbol.
     */
    private Interned interned;
    /**
     * True means that this variable has been explicitly declared
     * special (with `defvar' etc.), and shouldn't be lexically bound.
     */
    private boolean special;

    // NOTE: Is Emacs symbol names immutable?
    private final String name;

    /**
     * The symbol's property list.
     */
    private final ELispHashtable properties;

    private Object function;

    /**
     * Next symbol in obarray bucket, if the symbol is interned.
     */
    @Nullable
    ELispSymbol next = null;

    public ELispSymbol(String name) {
        this.value = new Value.PlainValue(UNBOUND);
        this.trappedWrite = TrappedWrite.NORMAL_WRITE;
        this.interned = Interned.UNINTERNED;
        this.name = name;
        this.properties = new ELispHashtable();
        // Use false instead of NIL because ELispContext.NIL is null before initialization
        this.function = false;
        boolean keyword = name.startsWith(":");
        this.special = false;
        if (keyword) {
            this.special = true;
            this.value.setValue(this);
            setConstant(true);
        }
    }

    public void setConstant(boolean b) {
        trappedWrite = b ? TrappedWrite.NO_WRITE : TrappedWrite.NORMAL_WRITE;
    }

    public boolean isConstant() {
        return trappedWrite == TrappedWrite.NO_WRITE;
    }

    /**
     * @return the value of the symbol, or {@link #UNBOUND}
     */
    private Object getAnyValue() {
        if (threadLocalValue != null) {
            Object local = threadLocalValue.getValue();
            if (local != UNBOUND) {
                return local;
            }
        }
        return value.getValue();
    }

    public Object getValue() {
        return checkUnbound(getAnyValue());
    }

    /**
     * Sets the thread-local value and returns the previous value
     * <p>
     * It might expose the internally used {@code UNBOUND} value, so the caller should make sure
     * to either dispose the returned value or only swap it back afterwards.
     * </p>
     * @param value the new thread-local value
     * @return the previous thread-local value, which should be treated as non-transparent
     */
    public Object swapThreadLocalValue(Object value) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(this);
        }
        if (threadLocalValue == null) {
            threadLocalValue = new ThreadLocalValue();
        }
        Object prev = threadLocalValue.getValue();
        threadLocalValue.setValue(value);
        return prev;
    }

    public void setValue(Object value) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(this);
        }
        if (threadLocalValue != null && threadLocalValue.isBoundAndSetValue(value)) {
            return;
        }
        this.value.setValue(value);
    }

    public boolean isBound() {
        return this.value.getValue() != UNBOUND;
    }

    public void makeUnbound() {
        setValue(UNBOUND);
    }

    public boolean isDefaultBound() {
        if (value instanceof Value.BufferLocal local) {
            return local.defaultValue != UNBOUND;
        }
        return isBound();
    }

    public Object getDefaultValue() {
        return checkUnbound(value instanceof Value.BufferLocal local ? local.defaultValue : value.getValue());
    }

    @CompilerDirectives.TruffleBoundary
    public void setDefaultValue(Object value) {
        if (isConstant()) {
            if (!BuiltInData.FKeywordp.keywordp(this) || value != this) {
                // "Allow setting keywords to their own value"?
                throw ELispSignals.settingConstant(this);
            }
        }
        if (this.value instanceof Value.BufferLocal local) {
            // TODO: "If this variable is not always local in all buffers"...
            // What is a not-always-local buffer-local variable?
            local.defaultValue = value;
        } else if (this.value instanceof Value.VarAlias alias) {
            // Recursive call: Truffle bails out
            alias.target.setDefaultValue(value);
        } else {
            this.value.setValue(value);
        }
    }

    @CompilerDirectives.TruffleBoundary
    public void setBufferLocal(boolean localIfSet) {
        if (this.value instanceof Value.VarAlias alias) {
            alias.target.setBufferLocal(localIfSet);
            return;
        }
        if (this.value instanceof Value.BufferLocal local) {
            if (localIfSet) {
                // make-variable-buffer-local
                local.localIfSet = true;
            } else {
                // make-local-variable
                local.setBufferLocalValue(getAnyValue());
            }
            return;
        }
        Object prevValue = getAnyValue();
        Value.BufferLocal local = new Value.BufferLocal(this);
        this.value = local;
        local.localIfSet = localIfSet;
        if (localIfSet) {
            // make-variable-buffer-local
            local.defaultValue = prevValue == UNBOUND ? function : prevValue;
        } else {
            // make-local-variable
            local.defaultValue = prevValue;
            local.setBufferLocalValue(prevValue);
        }
    }

    public boolean isBufferLocal(Object buffer) {
        return value instanceof Value.BufferLocal local && local.getBufferLocalValue() != UNBOUND;
    }

    public boolean isBufferLocalIfSet(Object buffer) {
        // TODO: How does buffer-local work?
        return value instanceof Value.BufferLocal local &&
                (local.getBufferLocalValue() != UNBOUND || local.localIfSet);
    }

    public void initForwardTo(Value.AbstractForwarded<?> forwarded) {
        this.special = true;
        this.value = forwarded;
    }

    public void aliasSymbol(ELispSymbol symbol) {
        if (isConstant()) {
            throw ELispSignals.error("Cannot make a constant an alias: " + this);
        }
        switch (this.value) {
            case Value.PlainValue _, Value.VarAlias _ -> this.value = new Value.VarAlias(symbol);
            default -> throw ELispSignals.error("Don’t know how to make a buffer-local variable an alias: " + this);
        }
    }

    public Object getIndirectFunction() {
        Object o = getFunction();
        if (CompilerDirectives.injectBranchProbability(CompilerDirectives.FASTPATH_PROBABILITY, !(o instanceof ELispSymbol))) {
            return o;
        }
        return slowPathGetIndirectFunction((ELispSymbol) o);
    }

    @CompilerDirectives.TruffleBoundary
    private static Object slowPathGetIndirectFunction(ELispSymbol symbol) {
        Object o;
        HashSet<ELispSymbol> visited = new HashSet<>();
        visited.add(symbol);
        while (true) {
            o = symbol.getFunction();
            if (!(o instanceof ELispSymbol nextSymbol)) {
                return o;
            }
            if (visited.contains(nextSymbol)) {
                throw ELispSignals.cyclicVariableIndirection(symbol);
            }
            visited.add(nextSymbol);
            symbol = nextSymbol;
        }
    }

    public Object getFunction() {
        return this.function;
    }

    public void setFunction(Object function) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(this);
        }
        Object original = getFunction();
        if (original instanceof ELispSubroutine(_, _, ELispSubroutine.InlineInfo inline) && inline != null) {
            inline.stable().invalidate();
        }
        this.function = Objects.requireNonNull(function);
        if (function instanceof ELispInterpretedClosure closure) {
            closure.setName(this);
        }
        if (function instanceof ELispCons cons && cons.car() == ELispContext.MACRO
                && cons.cdr() instanceof ELispInterpretedClosure closure) {
            closure.setName(this);
        }
    }

    public String name() {
        return name;
    }

    public boolean isSpecial() {
        return special;
    }

    public void setSpecial(boolean b) {
        this.special = b;
    }

    public void putProperty(Object k, Object v) {
        properties.put(k, v);
    }

    public Object getProperty(Object k) {
        return properties.get(k);
    }

    public Object getProperties() {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        properties.forEach((k, v) -> builder.add(k).add(v));
        return builder.build();
    }

    public void clearProperties() {
        properties.clear();
    }

    public void setInterned(Interned interned) {
        this.interned = interned;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }

    public static boolean isNil(Object nil) {
        return nil == ELispContext.NIL || nil == Boolean.FALSE;
    }

    public static boolean isT(Object nil) {
        return nil == ELispContext.T || nil == Boolean.TRUE;
    }

    public static long notNilOr(Object maybeNil, long defaultValue) {
        if (isNil(maybeNil)) {
            return defaultValue;
        }
        return (long) maybeNil;
    }

    private Object checkUnbound(Object value) {
        if (value == UNBOUND) {
            throw ELispSignals.voidVariable(this);
        }
        return value;
    }

    public enum TrappedWrite {
        NORMAL_WRITE,
        NO_WRITE,
        TRAPPED_WRITE,
    }

    public enum Interned {
        UNINTERNED,
        INTERNED,
        INTERNED_IN_INITIAL_OBARRAY,
    }

    public interface InternalValue {
        Object getValue();
        void setValue(Object value);
    }

    /**
     * Interface with similar semantics to {@code enum symbol_redirect}
     * and the corresponding enum in Emacs
     */
    public sealed interface Value extends InternalValue {

        // TODO: value, alias, blv (buffer-local), fwd (what?)

        final class PlainValue implements Value {
            private Object value;

            public PlainValue(Object value) {
                this.value = value;
            }

            @Override
            public Object getValue() {
                return value;
            }

            @Override
            public void setValue(Object value) {
                this.value = value;
            }
        }

        record VarAlias(ELispSymbol target) implements Value {
            @Override
            public Object getValue() {
                return getAliased().getValue();
            }

            @Override
            public void setValue(Object value) {
                getAliased().setValue(value);
            }

            public ELispSymbol getAliased() {
                ELispSymbol alias = target;
                while (alias.value instanceof VarAlias next) {
                    alias = next.target;
                }
                return alias;
            }
        }

        final class BufferLocal implements Value {
            @Nullable
            private ELispBuffer cachedBuffer = null;
            @Nullable
            private Forwarded value = null;
            private boolean localIfSet = false;
            private Object defaultValue = UNBOUND;
            private final ELispSymbol symbol;

            public BufferLocal(ELispSymbol symbol) {
                this.symbol = symbol;
            }

            private void updateCache() {
                ELispBuffer buffer = (ELispBuffer) ELispContext.CURRENT_BUFFER.getValue();
                if (cachedBuffer != buffer) {
                    cachedBuffer = buffer;
                    value = cachedBuffer.getLocal(symbol);
                }
            }

            public Object getBufferLocalValue() {
                return value == null ? UNBOUND : value.getValue();
            }

            public void setBufferLocalValue(Object value) {
                updateCache();
                if (this.value == null) {
                    this.value = Objects.requireNonNull(cachedBuffer).makeLocal(symbol);
                }
                this.value.setValue(value);
            }

            @Override
            public Object getValue() {
                updateCache();
                return value == null ? defaultValue : value.getValue();
            }

            @Override
            public void setValue(Object value) {
                updateCache();
                if (this.value == null) {
                    if (!localIfSet) {
                        this.defaultValue = value;
                        return;
                    }
                    this.value = Objects.requireNonNull(cachedBuffer).makeLocal(symbol);
                }
                this.value.setValue(value);
            }
        }

        sealed abstract class AbstractForwarded<T> implements Value {
            protected T value;

            public AbstractForwarded() {
                this.value = defaultValue();
            }

            public AbstractForwarded(T o) {
                this.value = o;
            }

            @Override
            public Object getValue() {
                return value;
            }

            @Override
            public void setValue(Object value) {
                this.value = typeCheck(value);
            }

            abstract T defaultValue();
            abstract T typeCheck(Object o);
        }

        final class Forwarded extends AbstractForwarded<Object> {
            public Forwarded() {
                super();
            }
            public Forwarded(Object o) {
                super(o);
            }
            @Override
            Object defaultValue() {
                return ELispContext.NIL;
            }
            @Override
            Object typeCheck(Object o) {
                return o;
            }
        }

        final class ForwardedLong extends AbstractForwarded<Long> {
            public ForwardedLong(Long o) {
                super(o);
            }
            @Override
            Long defaultValue() {
                return 0L;
            }
            @Override
            Long typeCheck(Object o) {
                if (o instanceof Long l) {
                    return l;
                }
                throw ELispSignals.wrongTypeArgument(ELispContext.INTEGERP, o);
            }
        }

        final class ForwardedBool extends AbstractForwarded<Boolean> {
            public ForwardedBool(Boolean o) {
                super(o);
            }
            @Override
            Boolean defaultValue() {
                return Boolean.FALSE;
            }
            @Override
            Boolean typeCheck(Object o) {
                return isNil(o) ? Boolean.FALSE : Boolean.TRUE;
            }
        }

        final class ForwardedPerBuffer extends AbstractForwarded<Object> {
            private final int index;
            public ForwardedPerBuffer(int index, ELispSymbol predicate) {
                super(predicate);
                this.index = index;
            }
            @Override
            public Object getValue() {
                return ((ELispBuffer) ELispContext.CURRENT_BUFFER.getValue()).getSlot(index);
            }
            @Override
            public void setValue(Object value) {
                setBufferValue(value, (ELispBuffer) ELispContext.CURRENT_BUFFER.getValue());
            }
            public void setBufferValue(Object value, ELispBuffer buffer) {
                ELispSymbol predicate = asSym(this.value);
                if (isInvalid(predicate, value)) {
                    throw ELispSignals.wrongTypeArgument(predicate, value);
                }
                buffer.setSlot(index, value);
            }
            private static boolean isInvalid(ELispSymbol predicate, Object value) {
                if (!isNil(value) && predicate != ELispContext.NIL) {
                    Object choices = BuiltInFns.FGet.get(predicate, ELispContext.CHOICE);
                    if (!isNil(choices)) {
                        return isNil(BuiltInFns.FMemq.memq(value, choices));
                    }
                    Object range = BuiltInFns.FGet.get(predicate, ELispContext.RANGE);
                    if (range instanceof ELispCons cons) {
                        return (!BuiltInData.FNumberp.numberp(value)
                                || !(
                                BuiltInData.compareTo(cons.car(), value) <= 0
                                        && BuiltInData.compareTo(value, cons.cdr()) <= 0
                        ));
                    }
                    if (BuiltInEval.FFunctionp.functionp(predicate)) {
                        return isNil(BuiltInEval.FFuncall.funcall(predicate, new Object[]{value}));
                    }
                }
                return false;
            }
            @Override
            Object defaultValue() {
                throw CompilerDirectives.shouldNotReachHere();
            }
            @Override
            Object typeCheck(Object o) {
                return o;
            }
        }
    }

    public static class ThreadLocalValue implements InternalValue {
        private final ThreadLocal<Value.Forwarded> threadLocal = new ThreadLocal<>();

        @Override
        public Object getValue() {
            @Nullable Value forwarded = threadLocal.get();
            //noinspection ConstantValue
            return forwarded == null ? UNBOUND : forwarded.getValue();
        }

        public boolean isBoundAndSetValue(Object value) {
            @Nullable Value forwarded = threadLocal.get();
            //noinspection ConstantValue
            if (forwarded != null) {
                forwarded.setValue(value);
                return true;
            }
            return false;
        }

        @Override
        public void setValue(Object value) {
            if (value == UNBOUND) {
                threadLocal.remove();
            } else {
                Value.@Nullable Forwarded forwarded = threadLocal.get();
                //noinspection ConstantValue
                if (forwarded == null) {
                    forwarded = new Value.Forwarded();
                    threadLocal.set(forwarded);
                }
                forwarded.setValue(value);
            }
        }
    }
}
