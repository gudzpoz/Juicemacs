package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CallTarget;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispValueNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

public interface ELispValue {

    default ELispExpressionNode eval(ELispContext context) {
        return new ELispValueNode(this);
    }

    default CallTarget getCallTarget() {
        return null;
    }

    String type();

}
