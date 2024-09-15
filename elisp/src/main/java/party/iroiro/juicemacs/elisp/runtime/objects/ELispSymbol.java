package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispBindingScope;

import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.CURRENT_BUFFER;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.T;

/**
 * Port of {@code struct Lisp_Symbol} to Java
 */
public final class ELispSymbol implements ELispValue {

    private final static ELispSymbol _UNBOUND = new ELispSymbol("unbound");

    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
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

    public void setConstant(boolean b) {
        trappedWrite = b ? TrappedWrite.NO_WRITE : TrappedWrite.NORMAL_WRITE;
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
            private Object defaultValue = NIL;
            private ELispSymbol symbol;

            public BufferLocal(ELispSymbol symbol) {
                this.symbol = symbol;
            }

            private void updateCache() {
                if (cachedBuffer != CURRENT_BUFFER) {
                    cachedBuffer = CURRENT_BUFFER;
                    value = cachedBuffer.getLocal(symbol);
                }
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
                    this.value = Objects.requireNonNull(cachedBuffer).makeLocal(symbol);
                }
                this.value.setValue(value);
            }
        }

        final class Forwarded implements Value {
            private Object value;

            public Forwarded() {
                this.value = NIL;
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

    private static class ThreadLocalValue implements InternalValue {
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

    private ELispValue function;

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
        this.function = NIL;
        this.special = name.startsWith(":");
    }

    public boolean isBound() {
        return this.value.getValue() != _UNBOUND;
    }

    public void makeUnbound() {
        setValue(_UNBOUND);
    }

    public Object getProperties() {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        properties.forEach((k, v) -> builder.add(k).add(v));
        return builder.build();
    }

    public boolean isConstant() {
        return false;
    }

    public Object getValue() {
        @Nullable Object lexical = ELispBindingScope.getLexical(this);
        if (lexical != null) {
            return lexical;
        }
        if (threadLocalValue != null) {
            Object local = threadLocalValue.getValue();
            if (local != _UNBOUND) {
                return local;
            }
        }
        return value.getValue();
    }

    public Object swapThreadLocalValue(Object value) {
        if (threadLocalValue == null) {
            threadLocalValue = new ThreadLocalValue();
        }
        Object prev = threadLocalValue.getValue();
        threadLocalValue.setValue(value);
        return prev;
    }

    public void setValue(Object value) {
        if (ELispBindingScope.setLexical(this, value)) {
            return;
        }
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        if (threadLocalValue != null && threadLocalValue.setIfBound(value)) {
            return;
        }
        this.value.setValue(value);
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

    public ELispValue getFunction() {
        if (this.value instanceof Value.VarAlias varAlias) {
            return varAlias.getAliased().getFunction();
        }
        return this.function;
    }

    public void setFunction(ELispValue function) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        if (this.value instanceof Value.VarAlias varAlias) {
            varAlias.getAliased().setFunction(function);
        } else {
            this.function = function;
        }
    }

    public String name() {
        return name;
    }

    @Override
    public String toString() {
        return name;
    }

    public static boolean isNil(Object nil) {
        return nil == NIL || nil == Boolean.FALSE;
    }

    public static boolean isT(Object nil) {
        return nil == T || nil == Boolean.TRUE;
    }
}
