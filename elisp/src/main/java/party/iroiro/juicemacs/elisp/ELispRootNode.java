package party.iroiro.juicemacs.elisp;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;

public class ELispRootNode extends RootNode {
    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispExpressionNode expression;

    public ELispRootNode(ELispLanguage language, ELispExpressionNode expression) {
        super(language);

        this.expression = expression;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return this.expression.executeGeneric(frame);
    }
}
