package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;

import java.util.Objects;
import java.util.function.Supplier;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

/**
 * Port of {@code struct Lisp_Symbol} to Java
 */
public final class ELispSymbol implements ELispValue {

    private final static ELispSymbol UNBOUND = new ELispSymbol("unbound");

    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }

    /**
     * Interface with similar semantics to {@code enum symbol_redirect}
     * and the corresponding enum in Emacs
     */
    public sealed interface SymbolValue {
        Object getValue();

        void setValue(Object value);

        default ELispValue getFunction() {
            return NIL;
        }

        default void setFunction(ELispValue function) {
            throw new UnsupportedOperationException();
        }

        // TODO: value, alias, blv (buffer-local), fwd (what?)

        final class PlainValue implements SymbolValue {
            private Object value;
            private ELispValue function;

            public PlainValue(Object value) {
                this.value = value;
                this.function = NIL;
            }

            @Override
            public Object getValue() {
                return value;
            }

            @Override
            public void setValue(Object value) {
                this.value = value;
            }

            @Override
            public ELispValue getFunction() {
                return function;
            }

            @Override
            public void setFunction(ELispValue function) {
                this.function = function;
            }
        }

        record VarAlias(ELispSymbol target) implements SymbolValue {
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

        final class BufferLocal implements SymbolValue {
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

        final class Forwarded implements SymbolValue {
            public Object value = NIL;

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
    private SymbolValue value;
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
    private Object properties;

    /**
     * Next symbol in obarray bucket, if the symbol is interned.
     */
    @Nullable
    ELispSymbol next = null;

    public ELispSymbol(String name) {
        this.value = new SymbolValue.PlainValue(ELispSymbol.UNBOUND);
        this.trappedWrite = TrappedWrite.NORMAL_WRITE;
        this.interned = Interned.UNINTERNED;
        this.name = name;
        this.properties = NIL;
    }

    public boolean isBound() {
        return this.value.getValue() != ELispSymbol.UNBOUND;
    }

    public void makeUnbound() {
        setValue(ELispSymbol.UNBOUND);
    }

    public Object getProperties() {
        return properties;
    }

    public boolean isConstant() {
        return false;
    }

    public Object getValue() {
        return value.getValue();
    }

    public void setValue(Object value) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        this.value.setValue(value);
    }

    public void forwardTo(Supplier<?> forward) {
        this.special = true;
        this.value = new SymbolValue.Forwarded();
    }

    public void aliasSymbol(ELispSymbol symbol) {
        switch (this.value) {
            case SymbolValue.PlainValue _, SymbolValue.VarAlias _ ->
                    this.value = new SymbolValue.VarAlias(symbol);
            default -> throw new UnsupportedOperationException();
        }
    }

    public ELispValue getFunction() {
        return this.value.getFunction();
    }

    public void setFunction(ELispValue function) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        this.value.setFunction(function);
    }

    public String name() {
        return name;
    }

    public static boolean isNil(Object nil) {
        return nil == NIL || nil == Boolean.FALSE;
    }

    public static boolean isT(Object nil) {
        return nil == T || nil == Boolean.TRUE;
    }
}
