package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import party.iroiro.juicemacs.elisp.ELispLanguage;

public final class FunctionRootNode extends RootNode {

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispExpressionNode functionBody;

    public FunctionRootNode(ELispLanguage language,
                            ELispExpressionNode functionBody) {
        super(language);
        this.functionBody = functionBody;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return this.functionBody.executeGeneric(frame);
    }
}
