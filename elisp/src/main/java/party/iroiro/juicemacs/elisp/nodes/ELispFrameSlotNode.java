package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.eclipse.jdt.annotation.Nullable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;

import java.util.Objects;

/// Classes and methods for reading & writing frame slots
///
/// @see ELispLexical.Allocator
public abstract class ELispFrameSlotNode {
    /// Placeholder slot for bypassing frame slot access
    ///
    /// Used by dynamic variable access to evaluate the value child
    /// of [ELispFrameSlotWriteNode].
    public final static int BYPASS = ELispLexical.NON_VAR_SLOT0;

    public static boolean isSpilled(int slot) {
        return slot >= ELispLexical.MAX_SLOTS;
    }

    public static ELispExpressionNode createRead(int slot, @Nullable MaterializedFrame parentFrame) {
        ELispExpressionNode reader = isSpilled(slot)
                ? ELispFrameSlotNodeFactory.ELispFrameSpilledSlotReadNodeGen.create(slot)
                : ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(slot);
        return parentFrame == null ? reader : new WithParentFrameNode(parentFrame, reader);
    }

    public static ELispExpressionNode createWrite(int slot, @Nullable MaterializedFrame parentFrame, ELispExpressionNode inner) {
        if (slot == BYPASS) {
            return inner;
        }
        return isSpilled(slot)
                ? ELispFrameSlotNodeFactory.ELispFrameSpilledSlotWriteNodeGen.create(inner, slot, parentFrame)
                : ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(inner, slot, parentFrame);
    }

    public static Object wrap(Object value) {
        if (value instanceof Double d) {
            return new SlotPrimitiveContainer(true, Double.doubleToRawLongBits(d));
        }
        if (value instanceof Long l) {
            return new SlotPrimitiveContainer(false, l);
        }
        return value;
    }

    public static final class SlotPrimitiveContainer {
        boolean isDouble;
        private long value;

        private SlotPrimitiveContainer(boolean isDouble, long value) {
            this.isDouble = isDouble;
            this.value = value;
        }

        public Object asObject() {
            if (isDouble) {
                return Double.longBitsToDouble(value);
            }
            return value;
        }

        public double getDouble() {
            return Double.longBitsToDouble(value);
        }
        public long getLong() {
            return value;
        }
        public void setDouble(double v) {
            isDouble = true;
            value = Double.doubleToRawLongBits(v);
        }
        public void setLong(long v) {
            isDouble = false;
            value = v;
        }
    }

    public static final class WithParentFrameNode extends ELispExpressionNode  {
        private final MaterializedFrame parentFrame;
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode innerNode;
        public WithParentFrameNode(MaterializedFrame parentFrame, ELispExpressionNode innerNode) {
            this.parentFrame = parentFrame;
            this.innerNode = innerNode;
        }
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return innerNode.executeGeneric(parentFrame);
        }
        @Override
        public void executeVoid(VirtualFrame frame) {
            innerNode.executeVoid(parentFrame);
        }
        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            return innerNode.executeLong(parentFrame);
        }
        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            return innerNode.executeDouble(parentFrame);
        }
    }

    @NodeField(name = "slot", type = int.class)
    public abstract static class ELispFrameSlotReadNode extends ELispExpressionNode {
        protected abstract int getSlot();

        @Specialization(guards = "frame.isLong(slot)")
        protected final long getLong(VirtualFrame frame) {
            return frame.getLong(getSlot());
        }

        @Specialization(guards = "frame.isDouble(slot)")
        protected final double getDouble(VirtualFrame frame) {
            return frame.getDouble(getSlot());
        }

        @Specialization(replaces = {"getLong", "getDouble"})
        protected final Object getObject(VirtualFrame frame) {
            int slot = getSlot();
            if (frame.isObject(slot)) {
                return frame.getObject(slot);
            }
            CompilerDirectives.transferToInterpreter();
            Object value = frame.getValue(slot);
            frame.setObject(slot, value);
            return value;
        }
    }

    @NodeField(name = "slot", type = int.class)
    public abstract static class ELispFrameSpilledSlotReadNode extends ELispExpressionNode {
        protected abstract int getSlot();

        @Nullable
        @NonIdempotent
        protected SlotPrimitiveContainer getContainer(VirtualFrame frame) {
            return ELispLexical.getVariable(frame, getSlot()) instanceof SlotPrimitiveContainer container
                    ? container : null;
        }

        final boolean isLong(SlotPrimitiveContainer container) {
            return container != null && !container.isDouble;
        }

        final boolean isDouble(SlotPrimitiveContainer container) {
            return container != null && container.isDouble;
        }

        @Specialization(guards = "isLong(container)")
        protected final long readLong(
                @SuppressWarnings("unused") VirtualFrame frame,
                @Bind(value = "getContainer(frame)") SlotPrimitiveContainer container
        ) {
            return container.getLong();
        }

        @Specialization(guards = "isDouble(container)")
        protected final double readDouble(
                @SuppressWarnings("unused") VirtualFrame frame,
                @Bind(value = "getContainer(frame)") SlotPrimitiveContainer container
        ) {
            return container.getDouble();
        }

        @Specialization
        protected final Object readObject(VirtualFrame frame) {
            Object variable = ELispLexical.getVariable(frame, getSlot());
            if (variable instanceof SlotPrimitiveContainer container) {
                return container.asObject();
            }
            return Objects.requireNonNull(variable);
        }
    }

    /// Write node
    ///
    /// ## Type System
    ///
    /// The [ELispTypeSystem] implements marker-to-integer conversion.
    /// However, we do not want it when running `setq`. (Otherwise,
    /// after `(setq marker marker)`, you might find `marker` turned
    /// into an integer.) We use [ELispTypeSystem.None] here to prevent
    /// this happening.
    @TypeSystemReference(ELispTypeSystem.None.class)
    @NodeField(name = "slot", type = int.class)
    @NodeField(name = "parentFrame", type = MaterializedFrame.class)
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    public abstract static class ELispFrameSlotWriteNode extends ELispExpressionNode {
        protected abstract int getSlot();
        protected abstract MaterializedFrame getParentFrame();

        private VirtualFrame getFrame(VirtualFrame frame) {
            MaterializedFrame parentFrame = getParentFrame();
            return parentFrame == null ? frame : parentFrame;
        }

        protected boolean isLongOrIllegal(VirtualFrame frame) {
            FrameSlotKind kind = getFrame(frame).getFrameDescriptor().getSlotKind(getSlot());
            return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
        }

        protected boolean isDoubleOrIllegal(VirtualFrame frame) {
            FrameSlotKind kind = getFrame(frame).getFrameDescriptor().getSlotKind(getSlot());
            return kind == FrameSlotKind.Double || kind == FrameSlotKind.Illegal;
        }

        @Specialization(guards = "isLongOrIllegal(frame)")
        protected final long writeLong(VirtualFrame frame, long value) {
            int slot = getSlot();
            frame = getFrame(frame);
            frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Long);
            frame.setLong(slot, value);
            return value;
        }

        @Specialization(guards = "isDoubleOrIllegal(frame)")
        protected final double writeDouble(VirtualFrame frame, double value) {
            int slot = getSlot();
            frame = getFrame(frame);
            frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Double);
            frame.setDouble(slot, value);
            return value;
        }

        @Specialization(replaces = {"writeLong", "writeDouble"})
        protected final Object writeObject(VirtualFrame frame, Object value) {
            int slot = getSlot();
            frame = getFrame(frame);
            frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Object);
            frame.setObject(slot, value);
            return value;
        }
    }

    @TypeSystemReference(ELispTypeSystem.None.class)
    @NodeField(name = "slot", type = int.class)
    @NodeField(name = "parentFrame", type = MaterializedFrame.class)
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    public abstract static class ELispFrameSpilledSlotWriteNode extends ELispExpressionNode {
        protected abstract int getSlot();
        protected abstract MaterializedFrame getParentFrame();

        private VirtualFrame getFrame(VirtualFrame frame) {
            MaterializedFrame parentFrame = getParentFrame();
            return parentFrame == null ? frame : parentFrame;
        }

        @Nullable
        @NonIdempotent
        protected SlotPrimitiveContainer getContainer(VirtualFrame frame) {
            return ELispLexical.getVariable(getFrame(frame), getSlot()) instanceof SlotPrimitiveContainer container
                    ? container : null;
        }

        private void writeContainer(VirtualFrame frame, long value, boolean isDouble) {
            SlotPrimitiveContainer container = getContainer(frame);
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.FASTPATH_PROBABILITY,
                    container != null
            )) {
                container.isDouble = isDouble;
                container.value = value;
                return;
            }
            ELispLexical.setVariable(getFrame(frame), getSlot(), new SlotPrimitiveContainer(isDouble, value));
        }

        @Specialization
        protected final long writeLong(VirtualFrame frame, long value) {
            writeContainer(frame, value, false);
            return value;
        }

        @Specialization
        protected final double writeDouble(VirtualFrame frame, double value) {
            writeContainer(frame, Double.doubleToRawLongBits(value), true);
            return value;
        }

        @Specialization
        protected final Object writeObject(VirtualFrame frame, Object value) {
            ELispLexical.setVariable(getFrame(frame), getSlot(), value);
            return value;
        }
    }
}
