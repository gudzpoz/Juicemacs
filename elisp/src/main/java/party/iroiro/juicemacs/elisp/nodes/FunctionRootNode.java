package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;

public class FunctionRootNode extends RootNode {

    private Object lispFunction;

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ReadFunctionArgNode.ArgCountVerificationNode functionBody;

    public FunctionRootNode(ELispLanguage language,
                            Object lispFunction,
                            ReadFunctionArgNode.ArgCountVerificationNode functionBody,
                            @Nullable FrameDescriptor descriptor) {
        super(language, descriptor);
        this.lispFunction = lispFunction;
        this.functionBody = functionBody;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return this.functionBody.executeGeneric(frame);
    }

    @Override
    protected boolean isCaptureFramesForTrace(boolean compiledFrame) {
        return true;
    }

    public Object getLispFunction() {
        return lispFunction;
    }

    public void setLispFunction(Object lispFunction) {
        this.lispFunction = lispFunction;
    }

    @Override
    public String getName() {
        return lispFunction.toString();
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
