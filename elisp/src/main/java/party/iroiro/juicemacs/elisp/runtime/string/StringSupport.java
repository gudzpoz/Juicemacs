package party.iroiro.juicemacs.elisp.runtime.string;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.InternalByteArray;
import com.oracle.truffle.api.strings.TruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.piecetree.StringNodes;

import java.nio.ByteBuffer;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_CHAR;

public abstract class StringSupport {
    public static final int STATE_ASCII = 0;
    public static final int STATE_BYTES = 1;
    public static final int STATE_UTF32 = 2;
    public static final int STATE_EMACS = 3;
    public static final TruffleString.Encoding UTF_32 = TruffleString.Encoding.UTF_32;

    private StringSupport() {
    }

    public static boolean isValidChar(long c) {
        return 0 <= c && c <= MAX_CHAR;
    }

    public static TruffleString toMultibyte(TruffleString s) {
        assert s.getStringCompactionLevelUncached(UTF_32) == TruffleString.CompactionLevel.S1;

        // A hack to get the internal byte array as is.
        // Note that we use ISO-8859-1 (latin-1) here instead of UTF-32
        // to ensure Truffle does not inflate the array.
        InternalByteArray bytes = s.getInternalByteArrayUncached(TruffleString.Encoding.ISO_8859_1);
        assert bytes.getLength() == s.codePointLengthUncached(UTF_32);

        int length = bytes.getLength();
        int[] codePoints = new int[length];
        for (int i = 0; i < length; i++) {
            byte b = bytes.get(i);
            codePoints[i] = b >= 0 ? b : (0x400000 + b);
        }
        return TruffleString.fromIntArrayUTF32Uncached(codePoints);
    }

    private static final TruffleString.FromByteArrayNode FROM_RAW = TruffleString.FromByteArrayNode.create();
    private static final TruffleString.SwitchEncodingNode SWITCH_IDENTICAL = TruffleString.SwitchEncodingNode.create();
    public static TruffleString fromBytes(byte[] bytes) {
        return SWITCH_IDENTICAL.execute(FROM_RAW.execute(bytes, TruffleString.Encoding.ISO_8859_1, false), UTF_32);
    }

    public static TruffleString fromRaw(ByteBuffer buffer) {
        byte[] bytes = new byte[buffer.remaining()];
        buffer.get(bytes);
        return fromBytes(bytes);
    }

    private static final TruffleString.FromJavaStringNode FROM_JAVA = TruffleString.FromJavaStringNode.create();
    public static ELispString fromString(String s) {
        return new ELispString(FROM_JAVA.execute(s, UTF_32));
    }

    private static final TruffleString.IntIndexOfAnyIntUTF32Node INDEX_OF_ANY = TruffleString.IntIndexOfAnyIntUTF32Node.create();
    public static int indexOfAny(AbstractTruffleString haystack, int[] needles, int start, int end) {
        if (start == end) {
            return -1;
        }
        return INDEX_OF_ANY.execute(haystack, start, end, needles);
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class GetInternalBytesNode extends Node {
        public abstract InternalByteArray execute(Node node, ELispString s);

        @Specialization(guards = "s.state() <= 1")
        public static InternalByteArray getBytes(
                ELispString s,
                @Cached(inline = false) TruffleString.GetInternalByteArrayNode get
        ) {
            return get.execute(s.value(), TruffleString.Encoding.ISO_8859_1);
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class StartsWithStringNode extends Node {
        public abstract boolean execute(Node node, ELispString haystack, ELispString needle);

        @Specialization
        public static boolean startsWith(
                ELispString haystack, ELispString needle,
                @Cached(inline = false) TruffleString.RegionEqualNode equalNode
        ) {
            return equalNode.execute(
                    haystack.value(),
                    0,
                    needle.value(),
                    0,
                    StringNodes.length(needle.value()),
                    StringSupport.UTF_32
            );
        }
    }
}
