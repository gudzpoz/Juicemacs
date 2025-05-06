package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.FrameSlotKind;
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
public abstract class ELispFrameSlotNode extends ELispExpressionNode {
    /// Placeholder slot for bypassing frame slot access
    ///
    /// Used by dynamic variable access to evaluate the value child
    /// of [ELispFrameSlotWriteNode].
    public final static int BYPASS = ELispLexical.NON_VAR_SLOT0;

    protected final int slot;
    @CompilerDirectives.CompilationFinal
    @Nullable
    protected final MaterializedFrame parentFrame;

    protected final VirtualFrame getFrame(VirtualFrame frame) {
        VirtualFrame f = this.parentFrame;
        return f == null ? frame : f;
    }

    protected ELispFrameSlotNode(int slot, @Nullable MaterializedFrame parentFrame) {
        this.slot = slot;
        this.parentFrame = parentFrame;
    }

    public int getSlot() {
        return slot;
    }

    public static boolean isSpilled(int slot) {
        return slot >= ELispLexical.MAX_SLOTS;
    }

    public static ELispExpressionNode createRead(int slot, @Nullable MaterializedFrame parentFrame) {
        if (isSpilled(slot)) {
            return ELispFrameSlotNodeFactory.ELispFrameSpilledSlotReadNodeGen.create(slot, parentFrame);
        }
        return ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(slot, parentFrame);
    }

    public static ELispExpressionNode createWrite(int slot, @Nullable MaterializedFrame parentFrame, ELispExpressionNode inner) {
        if (slot == BYPASS) {
            return inner;
        }
        if (isSpilled(slot)) {
            return ELispFrameSlotNodeFactory.ELispFrameSpilledSlotWriteNodeGen.create(slot, parentFrame, inner);
        }
        return ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(slot, parentFrame, inner);
    }

    @Nullable
    @NonIdempotent
    protected SlotPrimitiveContainer getContainer(VirtualFrame frame) {
        return ELispLexical.getVariable(getFrame(frame), slot) instanceof SlotPrimitiveContainer container
                ? container : null;
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

    public abstract static class ELispFrameSlotReadNode extends ELispFrameSlotNode {
        protected ELispFrameSlotReadNode(int slot, @Nullable MaterializedFrame frame) {
            super(slot, frame);
        }

        @Specialization(guards = "getFrame(frame).isLong(slot)")
        protected final long getLong(VirtualFrame frame) {
            return getFrame(frame).getLong(slot);
        }

        @Specialization(guards = "getFrame(frame).isDouble(slot)")
        protected final double getDouble(VirtualFrame frame) {
            return getFrame(frame).getDouble(slot);
        }

        @Specialization(replaces = {"getLong", "getDouble"})
        protected final Object getObject(VirtualFrame frame) {
            frame = getFrame(frame);
            if (frame.isObject(slot)) {
                return frame.getObject(slot);
            }
            CompilerDirectives.transferToInterpreter();
            Object value = frame.getValue(slot);
            frame.setObject(slot, value);
            return value;
        }
    }

    public abstract static class ELispFrameSpilledSlotReadNode extends ELispFrameSlotNode {
        protected ELispFrameSpilledSlotReadNode(int slot, @Nullable MaterializedFrame frame) {
            super(slot, frame);
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
            Object variable = ELispLexical.getVariable(getFrame(frame), slot);
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
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    public abstract static class ELispFrameSlotWriteNode extends ELispFrameSlotNode {
        protected ELispFrameSlotWriteNode(int slot, @Nullable MaterializedFrame frame) {
            super(slot, frame);
        }

        protected boolean isLongOrIllegal(VirtualFrame frame) {
            frame = getFrame(frame);
            FrameSlotKind kind = frame.getFrameDescriptor().getSlotKind(slot);
            return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
        }

        protected boolean isDoubleOrIllegal(VirtualFrame frame) {
            frame = getFrame(frame);
            FrameSlotKind kind = frame.getFrameDescriptor().getSlotKind(slot);
            return kind == FrameSlotKind.Double || kind == FrameSlotKind.Illegal;
        }

        @Specialization(guards = "isLongOrIllegal(frame)")
        protected final long setLong(VirtualFrame frame, long value) {
            frame = getFrame(frame);
            frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Long);
            frame.setLong(slot, value);
            return value;
        }

        @Specialization(guards = "isDoubleOrIllegal(frame)")
        protected final double setDouble(VirtualFrame frame, double value) {
            frame = getFrame(frame);
            frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Double);
            frame.setDouble(slot, value);
            return value;
        }

        @Specialization(replaces = {"setLong", "setDouble"})
        protected final Object setObject(VirtualFrame frame, Object value) {
            frame = getFrame(frame);
            frame.getFrameDescriptor().setSlotKind(slot, FrameSlotKind.Object);
            frame.setObject(slot, value);
            return value;
        }
    }

    @TypeSystemReference(ELispTypeSystem.None.class)
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    public abstract static class ELispFrameSpilledSlotWriteNode extends ELispFrameSlotNode {
        protected ELispFrameSpilledSlotWriteNode(int slot, @Nullable MaterializedFrame frame) {
            super(slot, frame);
        }

        private void writeContainer(VirtualFrame frame, long value, boolean isDouble) {
            if (slot == BYPASS) {
                return;
            }
            SlotPrimitiveContainer container = getContainer(frame);
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.FASTPATH_PROBABILITY,
                    container != null
            )) {
                container.isDouble = isDouble;
                container.value = value;
                return;
            }
            VirtualFrame dst = getFrame(frame);
            ELispLexical.setVariable(dst, slot, new SlotPrimitiveContainer(isDouble, value));
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
            if (slot != BYPASS) {
                VirtualFrame dst = getFrame(frame);
                ELispLexical.setVariable(dst, slot, value);
            }
            return value;
        }
    }
}
