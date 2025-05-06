package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;

@GenerateWrapper
public class ELispRootNode extends RootNode implements InstrumentableNode {
    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispExpressionNode expression;

    private final SourceSection sourceSection;

    public ELispRootNode(ELispLanguage language, ELispExpressionNode expression, SourceSection sourceSection) {
        super(language, ELispLexical.frameDescriptor(true));

        this.expression = expression;
        this.sourceSection = sourceSection;
    }

    ELispRootNode(ELispRootNode other) {
        this(other.getLanguage(ELispLanguage.class), other.expression, other.sourceSection);
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
        return sourceSection.getSource().getName();
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }

    @Override
    public boolean isInstrumentable() {
        return true;
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ELispRootNodeWrapper(this, this, probe);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        return tag == StandardTags.RootTag.class;
    }
}
