package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;

public class ELispFloatLiteralNode extends ELispExpressionNode {

    private final double value;

    public ELispFloatLiteralNode(double value) {
        this.value = value;
    }

    @Override
    public double executeDouble(VirtualFrame frame) {
        return value;
    }

    @Override
    public Double executeGeneric(VirtualFrame frame) {
        return value;
    }

}
