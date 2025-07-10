package party.iroiro.juicemacs.elisp.nodes.local;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.FrameSlotKind;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;

/// Write node
///
/// ## Type System
///
/// The [ELispTypeSystem] implements marker-to-integer conversion.
/// However, we do not want it when running `setq`. (Otherwise,
/// after `(setq marker marker)`, you might find `marker` turned
/// into an integer.) We use [ELispTypeSystem.None] here to prevent
/// this happening.
///
/// @see ELispFrameSlotReadNode
@TypeSystemReference(ELispTypeSystem.None.class)
public abstract class ELispFrameSlotWriteNode extends ELispExpressionNode {
    @Child @Executed
    GetFrameNode frameNode;
    @Child @Executed
    ELispExpressionNode value;
    final int slot;

    protected ELispFrameSlotWriteNode(int slot, GetFrameNode frameNode, ELispExpressionNode value) {
        this.slot = slot;
        this.frameNode = frameNode;
        this.value = value;
    }

    public static ELispExpressionNode createWrite(ELispLexical.LexicalReference ref, ELispExpressionNode inner) {
        int level = ref.level();
        int slot = ref.index();
        return createWrite(level, slot, inner);
    }

    public static ELispExpressionNode createWrite(int level, int slot, ELispExpressionNode inner) {
        if (slot < ELispLexical.FRAME_SLOT_START) {
            throw ELispSignals.fatal("invalid frame slot");
        }
        GetFrameNode framer = GetFrameNode.create(level);
        return ELispFrameSlotWriteNodeGen.create(slot, framer, inner);
    }

    protected boolean isLongOrIllegal(Frame frame) {
        FrameSlotKind kind = frame.getFrameDescriptor().getSlotKind(slot);
        return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
    }

    protected boolean isDoubleOrIllegal(Frame frame) {
        FrameSlotKind kind = frame.getFrameDescriptor().getSlotKind(slot);
        return kind == FrameSlotKind.Double || kind == FrameSlotKind.Illegal;
    }

    @Specialization(guards = "isLongOrIllegal(frame)")
    protected final long writeLong(Frame frame, long value) {
        frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Long);
        frame.setLong(slot, value);
        return value;
    }

    @Specialization(guards = "isDoubleOrIllegal(frame)")
    protected final double writeDouble(Frame frame, double value) {
        frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Double);
        frame.setDouble(slot, value);
        return value;
    }

    @Specialization(replaces = {"writeLong", "writeDouble"})
    protected final Object writeObject(Frame frame, Object value) {
        frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Object);
        frame.setObject(slot, value);
        return value;
    }
}
