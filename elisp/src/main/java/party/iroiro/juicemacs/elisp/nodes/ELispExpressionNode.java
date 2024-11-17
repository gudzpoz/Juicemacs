package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;

@TypeSystemReference(ELispTypeSystem.class)
public abstract class ELispExpressionNode extends Node {

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

}
