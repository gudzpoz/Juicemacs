package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CallTarget;

public record ELispSubroutine(CallTarget body) implements ELispValue {

    @Override
    public String type() {
        return "subr";
    }

    @Override
    public CallTarget getCallTarget() {
        return body;
    }
}
