package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;

public class ELispFixNumLiteralNode extends ELispExpressionNode {

    private final long value;

    public ELispFixNumLiteralNode(long value) {
        this.value = value;
    }

    @Override
    public long executeLong(VirtualFrame frame) {
        return value;
    }

    @Override
    public Long executeGeneric(VirtualFrame frame) {
        return value;
    }

}
