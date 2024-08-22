package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;

import java.math.BigInteger;

public class ELispBigNumLiteralNode extends ELispExpressionNode {

    private final ELispBigNum value;

    public ELispBigNumLiteralNode(BigInteger value) {
        this.value = new ELispBigNum(value);
    }

    @Override
    public ELispBigNum executeBigNum(VirtualFrame frame) {
        return value;
    }

    @Override
    public ELispBigNum executeGeneric(VirtualFrame frame) {
        return value;
    }

}
