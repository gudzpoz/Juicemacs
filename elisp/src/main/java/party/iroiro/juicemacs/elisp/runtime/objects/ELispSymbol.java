package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

import java.util.HashSet;
import java.util.Objects;


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
    /**
     * The unbound symbol, used as the value of void variables and not exposed to any caller.
     */
    // TODO: I guess we can simply use a java.lang.Object for this though.
    private final static ELispSymbol _UNBOUND = new ELispSymbol("unbound");

    /**
     * Indicates where the value can be found.
     */
    private Value value;
    /**
     * Indicates whether the value is thread-locally overridden.
     */
    @Nullable
    private ThreadLocalValue threadLocalValue;
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
        this.value = new Value.PlainValue(_UNBOUND);
        this.threadLocalValue = null;
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
            // TODO: Figure out proper keyword logic
            this.value.setValue(this);
        }
    }

    public void setConstant(boolean b) {
        trappedWrite = b ? TrappedWrite.NO_WRITE : TrappedWrite.NORMAL_WRITE;
    }

    public boolean isConstant() {
        return trappedWrite == TrappedWrite.NO_WRITE;
    }

    /**
     * @return the value of the symbol, or {@link #_UNBOUND}
     */
    private Object getAnyValue() {
        if (threadLocalValue != null) {
            Object local = threadLocalValue.getValue();
            if (local != _UNBOUND) {
                return local;
            }
        }
        return value.getValue();
    }

    public Object getValueOr(Object defaultValue) {
        Object anyValue = getAnyValue();
        return anyValue == _UNBOUND ? defaultValue : anyValue;
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
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        if (threadLocalValue == null) {
            threadLocalValue = new ThreadLocalValue();
        }
        Object prev = threadLocalValue.getValue();
        threadLocalValue.setValue(value);
        return prev;
    }

    public void setValue(Object value) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        if (threadLocalValue != null && threadLocalValue.setIfBound(value)) {
            return;
        }
        this.value.setValue(value);
    }

    public boolean isBound() {
        return this.value.getValue() != _UNBOUND;
    }

    public void makeUnbound() {
        setValue(_UNBOUND);
    }

    public boolean isDefaultBound() {
        if (value instanceof Value.BufferLocal local) {
            return local.defaultValue != _UNBOUND;
        }
        return isBound();
    }

    public Object getDefaultValue() {
        return checkUnbound(value instanceof Value.BufferLocal local ? local.defaultValue : value.getValue());
    }

    public void setDefaultValue(Object value) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            if (!BuiltInData.FKeywordp.keywordp(this)) {
                // "Allow setting keywords to their own value"?
                throw new UnsupportedOperationException();
            }
        }
        if (this.value instanceof Value.BufferLocal local) {
            // TODO: "If this variable is not always local in all buffers"...
            // What is a not-always-local buffer-local variable?
            local.defaultValue = value;
        } else if (this.value instanceof Value.VarAlias alias) {
            alias.target.setDefaultValue(value);
        } else {
            this.value.setValue(value);
        }
    }

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
            local.defaultValue = prevValue == _UNBOUND ? function : prevValue;
        } else {
            // make-local-variable
            local.defaultValue = prevValue;
            local.setBufferLocalValue(prevValue);
        }
    }

    public boolean isBufferLocal(Object buffer) {
        return value instanceof Value.BufferLocal local && local.getBufferLocalValue() != _UNBOUND;
    }

    public boolean isBufferLocalIfSet(Object buffer) {
        // TODO: How does buffer-local work?
        return value instanceof Value.BufferLocal local &&
                (local.getBufferLocalValue() != _UNBOUND || local.localIfSet);
    }

    public void forwardTo(Value.Forwarded forwarded) {
        this.special = true;
        this.value = forwarded;
    }

    public void aliasSymbol(ELispSymbol symbol) {
        switch (this.value) {
            case Value.PlainValue _, Value.VarAlias _ -> this.value = new Value.VarAlias(symbol);
            default -> throw new UnsupportedOperationException();
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
                throw new IllegalArgumentException();
            }
            visited.add(nextSymbol);
            symbol = nextSymbol;
        }
    }

    public Object getFunction() {
        return this.function;
    }

    public void setFunction(Object function) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        this.function = Objects.requireNonNull(function);
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

    public static long or(Object maybeNil, long defaultValue) {
        if (isNil(maybeNil)) {
            return defaultValue;
        }
        return (long) maybeNil;
    }

    @SuppressWarnings("unchecked")
    public static <T extends ELispValue> T or(Object maybeNil, T defaultValue) {
        if (isNil(maybeNil)) {
            return defaultValue;
        }
        Class<T> target = (Class<T>) defaultValue.getClass();
        return target.cast(maybeNil);
    }

    private static Object checkUnbound(Object value) {
        if (value == _UNBOUND) {
            throw new IllegalArgumentException();
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
            private Object defaultValue = _UNBOUND;
            private final ELispSymbol symbol;

            public BufferLocal(ELispSymbol symbol) {
                this.symbol = symbol;
            }

            private void updateCache() {
                if (cachedBuffer != ELispContext.CURRENT_BUFFER) {
                    cachedBuffer = ELispContext.CURRENT_BUFFER;
                    value = cachedBuffer.getLocal(symbol);
                }
            }

            public Object getBufferLocalValue() {
                return value == null ? _UNBOUND : value.getValue();
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

        final class Forwarded implements Value {
            private Object value;

            public Forwarded() {
                this.value = ELispContext.NIL;
            }

            public Forwarded(Object o) {
                this.value = o;
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
    }

    public static class ThreadLocalValue implements InternalValue {
        private final ThreadLocal<Value.Forwarded> threadLocal = new ThreadLocal<>();

        @Override
        public Object getValue() {
            @Nullable Value forwarded = threadLocal.get();
            //noinspection ConstantValue
            return forwarded == null ? _UNBOUND : forwarded.getValue();
        }

        public boolean setIfBound(Object value) {
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
            if (value == _UNBOUND) {
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
