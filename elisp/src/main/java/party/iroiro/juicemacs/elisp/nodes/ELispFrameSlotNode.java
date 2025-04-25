package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.*;
import org.eclipse.jdt.annotation.Nullable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;

import static party.iroiro.juicemacs.elisp.runtime.ELispLexical.NON_VAR_SLOT0;
import static party.iroiro.juicemacs.elisp.runtime.ELispLexical.getVariable;
import static party.iroiro.juicemacs.elisp.runtime.ELispLexical.setVariable;

/// Classes and methods for reading & writing frame slots
///
/// @see party.iroiro.juicemacs.elisp.runtime.ELispLexical
public abstract class ELispFrameSlotNode extends ELispExpressionNode {
    /// Placeholder slot for bypassing frame slot access
    ///
    /// Used by dynamic variable access to evaluate the value child
    /// of [ELispFrameSlotWriteNode].
    public final static int BYPASS = NON_VAR_SLOT0;

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

    @Nullable
    @NonIdempotent
    protected SlotPrimitiveContainer getContainer(VirtualFrame frame) {
        return slot != BYPASS && getVariable(getFrame(frame), slot) instanceof SlotPrimitiveContainer container
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

    protected static final class SlotPrimitiveContainer {
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
            value = Double.doubleToRawLongBits(v);
        }
        public void setLong(long v) {
            value = v;
        }
    }

    public abstract static class ELispFrameSlotReadNode extends ELispFrameSlotNode {
        protected ELispFrameSlotReadNode(int slot, @Nullable MaterializedFrame frame) {
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

        @Fallback
        protected final Object readObject(VirtualFrame frame) {
            Object variable = getVariable(getFrame(frame), slot);
            if (variable instanceof SlotPrimitiveContainer container) {
                return container.asObject();
            }
            return variable;
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
            setVariable(dst, slot, new SlotPrimitiveContainer(isDouble, value));
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

        @Fallback
        protected final Object writeObject(VirtualFrame frame, Object value) {
            if (slot != BYPASS) {
                VirtualFrame dst = getFrame(frame);
                setVariable(dst, slot, value);
            }
            return value;
        }
    }
}
