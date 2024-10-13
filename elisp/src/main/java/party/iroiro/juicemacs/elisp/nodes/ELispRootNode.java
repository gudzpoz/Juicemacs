package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;

public class ELispRootNode extends RootNode {
    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispExpressionNode expression;

    private final SourceSection sourceSection;

    public ELispRootNode(ELispLanguage language, ELispExpressionNode expression, SourceSection sourceSection) {
        super(language, ELispLexical.frameDescriptor(true));

        this.expression = expression;
        this.sourceSection = sourceSection;

        adoptChildren();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        ELispLexical.initFrame(frame);
        return this.expression.executeGeneric(frame);
    }

    @Override
    protected boolean isCaptureFramesForTrace(boolean compiledFrame) {
        return true;
    }

    @Override
    public String getName() {
        return sourceSection.getSource().getName();
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }
}
