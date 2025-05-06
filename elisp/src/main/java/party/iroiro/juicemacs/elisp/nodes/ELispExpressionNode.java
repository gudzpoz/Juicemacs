package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;

@TypeSystemReference(ELispTypeSystem.class)
@GenerateWrapper
public abstract class ELispExpressionNode extends ELispBaseNode implements InstrumentableNode {

    public void executeVoid(VirtualFrame frame) {
        executeGeneric(frame);
    }

    public boolean executeBoolean(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectBoolean(executeGeneric(frame));
    }

    public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectLong(executeGeneric(frame));
    }

    public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectDouble(executeGeneric(frame));
    }

    public abstract Object executeGeneric(VirtualFrame frame);

    @Override
    public boolean isInstrumentable() {
        SourceSection source = getSourceSection();
        return source != null && source.isAvailable();
    }

    @Override
    public SourceSection getSourceSection() {
        if (this instanceof ELispExpressionNodeWrapper wrapper) {
            return wrapper.getDelegateNode().getSourceSection();
        }
        return super.getSourceSection();
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ELispExpressionNodeWrapper(this, probe);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        return tag == StandardTags.ExpressionTag.class;
    }
}
