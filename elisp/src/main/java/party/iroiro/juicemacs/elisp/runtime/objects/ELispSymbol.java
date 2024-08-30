package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;

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

    public enum Redirection {
        PLAIN_VAL,
        VAR_ALIAS,
        LOCALIZED,
        FORWARDED,
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

    /** Indicates where the value can be found.  */
    private Redirection redirect;
    /** Indicates whether operations are needed before any writes.  */
    private TrappedWrite trappedWrite;
    /** Interned state of the symbol.  */
    private Interned interned;
    /** True means that this variable has been explicitly declared
   special (with `defvar' etc.), and shouldn't be lexically bound.  */
    private boolean special;

    // NOTE: Is Emacs symbol names immutable?
    private final String name;

    /** Value of the symbol or Qunbound if unbound.  Which alternative of the
   union is used depends on the `redirect' field above.  */
    private Object value; // TODO: value, alias, blv (buffer-local), fwd (what?)

    /** Function value of the symbol or Qnil if not fboundp.  */
    private ELispValue function;

    /** The symbol's property list.  */
    private Object properties;

    /** Next symbol in obarray bucket, if the symbol is interned.  */
    @Nullable
    ELispSymbol next = null;

    public ELispSymbol(String name) {
        this.redirect = Redirection.PLAIN_VAL;
        this.trappedWrite = TrappedWrite.NORMAL_WRITE;
        this.interned = Interned.UNINTERNED;
        this.name = name;
        this.value = ELispSymbol.UNBOUND;
        this.function = NIL;
        this.properties = NIL;
    }

    public boolean isBound() {
        return this.value != ELispSymbol.UNBOUND;
    }

    public void makeUnbound() {
        this.value = ELispSymbol.UNBOUND;
    }

    public Object getProperties() {
        return properties;
    }

    public boolean isConstant() {
        return false;
    }

    public Object getValue() {
        return switch (this.redirect) {
            case PLAIN_VAL -> {
                if (value == ELispSymbol.UNBOUND) {
                    throw new IllegalArgumentException();
                }
                yield value;
            }
            case VAR_ALIAS -> getAliased().getValue();
            case FORWARDED -> ((Supplier<?>) value).get();
            case LOCALIZED -> NIL;
        };
    }

    private ELispSymbol getAliased() {
        ELispSymbol alias = this;
        while (alias.redirect == Redirection.VAR_ALIAS) {
            alias = (ELispSymbol) alias.value;
        }
        return alias;
    }

    public void setValue(Object value) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        switch (this.redirect) {
            case PLAIN_VAL -> this.value = value;
            case VAR_ALIAS -> this.getAliased().setValue(value);
            case LOCALIZED, FORWARDED -> throw new UnsupportedOperationException();
        }
    }

    public void forwardTo(Supplier<?> forward) {
        this.special = true;
        this.redirect = Redirection.FORWARDED;
        this.value = forward;
    }

    public void aliasSymbol(ELispSymbol symbol) {
        switch (this.redirect) {
            case PLAIN_VAL, VAR_ALIAS -> {
                this.value = symbol;
                this.redirect = Redirection.VAR_ALIAS;
            }
            default -> throw new UnsupportedOperationException();
        }
    }

    public ELispValue getFunction() {
        return switch (this.redirect) {
            case PLAIN_VAL -> this.function;
            case VAR_ALIAS -> getAliased().getFunction();
            default -> throw new UnsupportedOperationException();
        };
    }

    public void setFunction(ELispValue function) {
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            throw new UnsupportedOperationException();
        }
        switch (this.redirect) {
            case PLAIN_VAL -> this.function = function;
            case VAR_ALIAS -> getAliased().setFunction(function);
            default -> throw new UnsupportedOperationException();
        }
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
