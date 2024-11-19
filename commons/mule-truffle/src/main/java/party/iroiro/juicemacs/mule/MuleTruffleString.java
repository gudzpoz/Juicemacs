package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;

import java.util.Arrays;

import static com.oracle.truffle.api.strings.TruffleString.Encoding.ISO_8859_1;
import static com.oracle.truffle.api.strings.TruffleString.Encoding.UTF_32;

public final class MuleTruffleString implements MuleString {
    private static final TruffleString.CodePointLengthNode CODE_POINT_LENGTH = TruffleString.CodePointLengthNode.create();
    private static final TruffleString.CodePointAtIndexNode CODE_POINT_AT_INDEX = TruffleString.CodePointAtIndexNode.create();
    private static final TruffleString.SubstringNode SUBSTRING = TruffleString.SubstringNode.create();
    private static final TruffleString.FromJavaStringNode FROM_JAVA_STRING = TruffleString.FromJavaStringNode.create();
    private static final TruffleString.ToJavaStringNode TO_JAVA_STRING = TruffleString.ToJavaStringNode.create();
    private static final TruffleString.ForceEncodingNode FORCE_ENCODING = TruffleString.ForceEncodingNode.create();
    private static final TruffleString.CopyToByteArrayNode COPY_TO_BYTE_ARRAY = TruffleString.CopyToByteArrayNode.create();
    private static final TruffleString.EqualNode EQUAL = TruffleString.EqualNode.create();

    private final TruffleString string;

    MuleTruffleString(TruffleString string) {
        this.string = string;
    }

    @Override
    public int length() {
        return CODE_POINT_LENGTH.execute(string, UTF_32);
    }

    @Override
    public int codePointAt(int index) {
        return CODE_POINT_AT_INDEX.execute(string, index, UTF_32);
    }

    @Override
    public MuleString subSequence(int start, int end) {
        return new MuleTruffleString(SUBSTRING.execute(string, start, end - start, UTF_32, false));
    }

    @Override
    public String toString() {
        return TO_JAVA_STRING.execute(string);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof MuleString s)) {
            return false;
        }
        if (s instanceof MuleTruffleString other) {
            return EQUAL.execute(other.string, string, UTF_32);
        }
        return MuleString.equals(this, s);
    }

    @Override
    public int hashCode() {
        return MuleString.hashCode(this);
    }

    TruffleString truffleString() {
        return string;
    }

    static TruffleString fromJavaString(String string) {
        return FROM_JAVA_STRING.execute(string, UTF_32);
    }

    static byte[] toLatin1(TruffleString string) {
        TruffleString converted = FORCE_ENCODING.execute(string, UTF_32, ISO_8859_1);
        return COPY_TO_BYTE_ARRAY.execute(converted, ISO_8859_1);
    }
}
