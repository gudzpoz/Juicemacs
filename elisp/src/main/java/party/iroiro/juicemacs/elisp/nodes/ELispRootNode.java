package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;

import java.util.Objects;

public class ELispRootNode extends RootNode {
    @Child
    ELispExpressionNode expression;

    @Nullable
    private final SourceSection sourceSection;

    public ELispRootNode(ELispLanguage language, ELispExpressionNode expression, @Nullable SourceSection sourceSection) {
        super(language, ELispLexical.rootFrameDescriptor(0, true));

        this.expression = expression;
        this.sourceSection = sourceSection;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return this.expression.executeGeneric(frame);
    }

    @Override
    protected boolean isCaptureFramesForTrace(boolean compiledFrame) {
        return true;
    }

    @Override
    public String getName() {
        return sourceSection == null ? Objects.toIdentityString(this) : sourceSection.getSource().getName();
    }

    @Override
    @Nullable
    public SourceSection getSourceSection() {
        return sourceSection;
    }
}
