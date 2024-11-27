package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;

import java.util.Arrays;

import static com.oracle.truffle.api.strings.TruffleString.Encoding.ISO_8859_1;
import static com.oracle.truffle.api.strings.TruffleString.Encoding.UTF_32;

public final class MuleByteArrayString implements MuleString {
    private static final TruffleString.FromByteArrayNode FROM_BYTE_ARRAY = TruffleString.FromByteArrayNode.create();
    private static final TruffleString.SwitchEncodingNode SWITCH_ENCODING = TruffleString.SwitchEncodingNode.create();
    private static final TruffleString.ToJavaStringNode TO_JAVA_STRING = TruffleString.ToJavaStringNode.create();

    public static final int STATE_ASCII = 0;
    public static final int STATE_LATIN_1 = 0b01;
    public static final int STATE_UNI_BYTES = 0b10;

    private final byte[] bytes;
    private final int state;

    MuleByteArrayString(byte[] bytes, int state) {
        this.bytes = bytes;
        this.state = state;
    }

    public int getState() {
        return state;
    }

    @Override
    public int length() {
        return bytes.length;
    }

    @Override
    public int codePointAt(int index) {
        return Byte.toUnsignedInt(bytes[index]);
    }

    @Override
    public MuleString subSequence(int start, int end) {
        byte[] bytes = new byte[end - start];
        System.arraycopy(this.bytes, start, bytes, 0, bytes.length);
        return new MuleByteArrayString(bytes, state);
    }

    @Override
    public String toString() {
        if (state == MuleStringBuffer.BUILDING_UNI_BYTES) {
            return rawByteToString();
        }
        return TO_JAVA_STRING.execute(toTruffleString());
    }

    private String rawByteToString() {
        byte[] bytes = bytes();
        StringBuilder builder = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            int i = Byte.toUnsignedInt(b);
            if (i < 0x80) {
                builder.append((char) i);
            } else {
                builder.append("\\").append(Integer.toOctalString(i));
            }
        }
        return builder.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof MuleString s)) {
            return false;
        }
        if (s instanceof MuleByteArrayString other) {
            return Arrays.equals(other.bytes, bytes);
        }
        return MuleString.equals(this, s);
    }

    @Override
    public int hashCode() {
        return MuleString.hashCode(this);
    }

    public TruffleString toTruffleString() {
        return SWITCH_ENCODING.execute(FROM_BYTE_ARRAY.execute(bytes, ISO_8859_1), UTF_32);
    }

    public byte[] bytes() {
        return bytes;
    }
}
