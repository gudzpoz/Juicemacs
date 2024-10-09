package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

public record ELispSubroutine(ELispFunctionObject body, boolean specialForm) implements ELispValue {
    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }

    @Override
    public String toString() {
        return "#<subr " + body.callTarget().toString() +  ">";
    }
}
