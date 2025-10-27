package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.Idempotent;

public record ELispFunctionObject(CallTarget callTarget) {
    @Idempotent
    @Override
    public CallTarget callTarget() {
        return callTarget;
    }
}
