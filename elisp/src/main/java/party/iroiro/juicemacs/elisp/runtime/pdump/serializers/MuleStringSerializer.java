package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.CompilerDirectives;
import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.ExternalizableSerializer;
import org.apache.fury.serializer.Serializer;
import party.iroiro.juicemacs.mule.*;

public final class MuleStringSerializer extends Serializer<MuleString> {
    private final ExternalizableSerializer<MuleStringBuffer> muleStringBuffer;

    public MuleStringSerializer(Fury fury) {
        super(fury, MuleString.class);
        this.muleStringBuffer = new ExternalizableSerializer<>(fury, MuleStringBuffer.class);
    }

    @Override
    public void write(MemoryBuffer buffer, MuleString value) {
        long length = value.length();
        buffer.writeVarUint64(length);
        switch (value) {
            case MuleByteArrayString bytes -> {
                int state = bytes.getState();
                assert state <= 0x0F;
                buffer.writeByte(state);
                buffer.writeBytes(bytes.bytes());
            }
            case MuleTruffleString string -> {
                buffer.writeByte(0b10000000);
                fury.writeJavaString(buffer, string.toString());
            }
            case MuleIntArrayString ints -> {
                buffer.writeByte(0b11000000);
                int[] chars = ints.intArray();
                buffer.grow(chars.length * 5 + 8);
                for (int c : chars) {
                    buffer._unsafeWriteVarUint32(c);
                }
            }
            case MuleStringBuffer complex -> {
                buffer.writeByte(0b11100000);
                muleStringBuffer.write(buffer, complex);
            }
        }
    }

    @Override
    public MuleString read(MemoryBuffer buffer) {
        long length = buffer.readVarUint64();
        int flag = Byte.toUnsignedInt(buffer.readByte());
        int byteFlag = flag & 0xF0;
        return switch (byteFlag) {
            case 0 -> {
                int state = flag & 0x0F;
                byte[] bytes = buffer.readBytes(Math.toIntExact(length));
                yield state == MuleByteArrayString.STATE_LATIN_1
                        ? MuleString.fromLatin1(bytes)
                        : MuleString.fromRaw(bytes);
            }
            case 0b10000000 -> MuleString.fromString(fury.readJavaString(buffer));
            case 0b11000000 -> {
                int[] chars = new int[Math.toIntExact(length)];
                for (int i = 0; i < chars.length; i++) {
                    chars[i] = buffer.readVarUint32();
                }
                yield MuleString.fromInts(chars);
            }
            case 0b11100000 -> muleStringBuffer.read(buffer);
            default -> throw CompilerDirectives.shouldNotReachHere();
        };
    }
}
