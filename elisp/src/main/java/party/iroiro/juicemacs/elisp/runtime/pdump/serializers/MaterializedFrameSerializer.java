package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.MaterializedFrame;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;

public final class MaterializedFrameSerializer extends Serializer<MaterializedFrame> {
    public MaterializedFrameSerializer(Fory fory) {
        super(fory, MaterializedFrame.class);
    }

    @Override
    @TruffleBoundary
    public void write(MemoryBuffer buffer, MaterializedFrame value) {
        FrameDescriptor descriptor = value.getFrameDescriptor();
        assert descriptor.getAuxiliarySlots().isEmpty();
        int count = descriptor.getNumberOfSlots();
        buffer.writeVarUint32(count);
        for (int i = 0; i < count; i++) {
            FrameSlotKind kind = descriptor.getSlotKind(i);
            switch (kind) {
                case Object -> {
                    buffer.writeByte(0);
                    fory.writeRef(buffer, value.getObject(i));
                }
                case Long -> {
                    buffer.writeByte(1);
                    buffer.writeVarInt64(value.getLong(i));
                }
                case Double -> {
                    buffer.writeByte(2);
                    buffer.writeInt64(Double.doubleToRawLongBits(value.getDouble(i)));
                }
                case Illegal -> buffer.writeByte(-1);
                default -> throw new IllegalStateException("unexpected frame slot: " + kind);
            }
        }
    }

    @Override
    @TruffleBoundary
    public MaterializedFrame read(MemoryBuffer buffer) {
        int count = buffer.readVarUint32();
        MaterializedFrame frame = Truffle.getRuntime()
                .createVirtualFrame(new Object[0], ELispLexical.rootFrameDescriptor(count, true))
                .materialize();
        fory.getRefResolver().reference(frame);
        for (int i = 0; i < count; i++) {
            byte kind = buffer.readByte();
            switch (kind) {
                case 0 -> {
                    frame.getFrameDescriptor().setSlotKind(i, FrameSlotKind.Object);
                    frame.setObject(i, fory.readRef(buffer));
                }
                case 1 -> {
                    frame.getFrameDescriptor().setSlotKind(i, FrameSlotKind.Long);
                    frame.setLong(i, buffer.readVarInt64());
                }
                case 2 -> {
                    frame.getFrameDescriptor().setSlotKind(i, FrameSlotKind.Double);
                    frame.setDouble(i, Double.longBitsToDouble(buffer.readInt64()));
                }
                case -1 -> frame.getFrameDescriptor().setSlotKind(i, FrameSlotKind.Illegal);
                default -> throw new IllegalStateException("unexpected frame slot: " + kind);
            }
        }
        return frame;
    }
}
