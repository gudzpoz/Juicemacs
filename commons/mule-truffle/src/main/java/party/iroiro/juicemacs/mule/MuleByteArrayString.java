package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;

import java.util.Arrays;

import static com.oracle.truffle.api.strings.TruffleString.Encoding.ISO_8859_1;
import static com.oracle.truffle.api.strings.TruffleString.Encoding.UTF_32;

public final class MuleByteArrayString implements MuleString {
    private static final TruffleString.FromByteArrayNode FROM_BYTE_ARRAY = TruffleString.FromByteArrayNode.create();
    private static final TruffleString.SwitchEncodingNode SWITCH_ENCODING = TruffleString.SwitchEncodingNode.create();
    private static final TruffleString.ToJavaStringNode TO_JAVA_STRING = TruffleString.ToJavaStringNode.create();

    private final byte[] bytes;

    MuleByteArrayString(byte[] bytes) {
        this.bytes = bytes;
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
        return new MuleByteArrayString(bytes);
    }

    @Override
    public String toString() {
        return TO_JAVA_STRING.execute(toTruffleString());
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
