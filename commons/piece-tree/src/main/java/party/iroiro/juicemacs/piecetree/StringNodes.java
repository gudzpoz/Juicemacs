package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.strings.*;

public abstract class StringNodes {
    public static final TruffleString.CodePointLengthNode LENGTH = TruffleString.CodePointLengthNode.create();
    public static int length(AbstractTruffleString string) {
        return LENGTH.execute(string, TruffleString.Encoding.UTF_32);
    }

    public static final TruffleString.CodePointAtIndexNode CHAR_AT = TruffleString.CodePointAtIndexNode.create();
    public static int charAt(AbstractTruffleString str, int i) {
        return CHAR_AT.execute(str, i, TruffleString.Encoding.UTF_32);
    }

    public static final TruffleString.SubstringNode SUBSTRING = TruffleString.SubstringNode.create();
    public static TruffleString substring(AbstractTruffleString str, int start, int length) {
        return SUBSTRING.execute(str, start, length, TruffleString.Encoding.UTF_32, true);
    }

    public static final TruffleString.IndexOfCodePointNode INDEX_OF = TruffleString.IndexOfCodePointNode.create();
    public static int indexOf(AbstractTruffleString str, int c, int start, int length) {
        return INDEX_OF.execute(str, c, start, length, TruffleString.Encoding.UTF_32);
    }

    public static final TruffleString.GetInternalByteArrayNode INTERNAL_BYTES = TruffleString.GetInternalByteArrayNode.create();
    public static InternalByteArray bytes(AbstractTruffleString str, TruffleString.Encoding encoding) {
        return INTERNAL_BYTES.execute(str, encoding);
    }

    public static final TruffleStringBuilder.AppendStringNode APPEND = TruffleStringBuilder.AppendStringNode.create();
    public static void append(TruffleStringBuilderUTF32 sb, AbstractTruffleString string) {
        APPEND.execute(sb, string);
    }

    public static final TruffleStringBuilder.ToStringNode STRING_BUILD = TruffleStringBuilder.ToStringNode.create();
    public static TruffleString buildString(TruffleStringBuilderUTF32 sb) {
        return STRING_BUILD.execute(sb);
    }

    private StringNodes() {
    }
}
