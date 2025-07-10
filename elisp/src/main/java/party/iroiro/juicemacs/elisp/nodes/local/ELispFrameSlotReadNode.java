package party.iroiro.juicemacs.elisp.nodes.local;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Executed;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.Frame;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;

public abstract class ELispFrameSlotReadNode extends ELispExpressionNode {
    @Child @Executed
    GetFrameNode frameNode;
    final int slot;

    ELispFrameSlotReadNode(GetFrameNode frameNode, int slot) {
        this.slot = slot;
        this.frameNode = frameNode;
    }

    public static ELispExpressionNode createRead(ELispLexical.LexicalReference ref) {
        GetFrameNode framer = GetFrameNode.create(ref.level());
        return ELispFrameSlotReadNodeGen.create(framer, ref.index());
    }

    @Specialization(guards = "frame.isLong(slot)")
    protected final long getLong(Frame frame) {
        return frame.getLong(slot);
    }

    @Specialization(guards = "frame.isDouble(slot)")
    protected final double getDouble(Frame frame) {
        return frame.getDouble(slot);
    }

    @Specialization(replaces = {"getLong", "getDouble"})
    protected final Object getObject(Frame frame) {
        if (frame.isObject(slot)) {
            return frame.getObject(slot);
        }
        CompilerDirectives.transferToInterpreter();
        Object value = frame.getValue(slot);
        frame.setObject(slot, value);
        return value;
    }
}
