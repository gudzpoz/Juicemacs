package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;

public final class FunctionRootNode extends RootNode {

    private String name;
    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispExpressionNode functionBody;

    public FunctionRootNode(ELispLanguage language,
                            String name,
                            ELispExpressionNode functionBody,
                            @Nullable FrameDescriptor descriptor) {
        super(language, descriptor);
        this.name = name;
        this.functionBody = functionBody;
        adoptChildren();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return this.functionBody.executeGeneric(frame);
    }

    @Override
    protected boolean isCaptureFramesForTrace(boolean compiledFrame) {
        return true;
    }

    @Override
    public String getName() {
        return name == null ? functionBody.getClass().getSimpleName() : name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public SourceSection getSourceSection() {
        return functionBody.getSourceSection();
    }

    @Override
    public String toString() {
        return getName();
    }
}
