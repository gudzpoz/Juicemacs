package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;

public class ELispValueNode extends ELispExpressionNode {

    private final ELispValue value;

    public ELispValueNode(ELispValue value) {
        this.value = value;
    }

    @Override
    public ELispBigNum executeBigNum(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectELispBigNum(value);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return value;
    }
}
