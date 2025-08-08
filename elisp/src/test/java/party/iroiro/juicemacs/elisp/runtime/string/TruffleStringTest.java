package party.iroiro.juicemacs.elisp.runtime.string;

import com.oracle.truffle.api.strings.InternalByteArray;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringBuilder;
import com.oracle.truffle.api.strings.TruffleStringBuilderUTF32;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class TruffleStringTest {
    public static final int EMACS_MAX_CODEPOINT = 0x3FFFFF;

    /// Test possible operations for UTF-32 strings with out-of-bound chars
    @Test
    public void testEmacsMaxCharOps() {
        TruffleString s = TruffleString.fromIntArrayUTF32Uncached(new int[]{
                EMACS_MAX_CODEPOINT,
        });
        assertEquals(1, s.codePointLengthUncached(TruffleString.Encoding.UTF_32));
        assertEquals(EMACS_MAX_CODEPOINT, s.codePointAtIndexUncached(0, TruffleString.Encoding.UTF_32));
        TruffleString.CodeRange range = s.getCodeRangeImpreciseUncached(TruffleString.Encoding.UTF_32);
        assertEquals(TruffleString.CodeRange.BROKEN, range);
        range = s.getCodeRangeUncached(TruffleString.Encoding.UTF_32);
        assertEquals(TruffleString.CodeRange.BROKEN, range);
        assertEquals(
                TruffleString.CompactionLevel.S4,
                s.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32)
        );

        TruffleStringBuilderUTF32 sb = TruffleStringBuilder.createUTF32();
        sb.appendStringUncached(s);
        TruffleString copy = sb.toStringUncached();
        assertTrue(copy.equalsUncached(s, TruffleString.Encoding.UTF_32));
        assertEquals(1, copy.codePointLengthUncached(TruffleString.Encoding.UTF_32));
        assertEquals(EMACS_MAX_CODEPOINT, copy.codePointAtIndexUncached(0, TruffleString.Encoding.UTF_32));
        assertEquals(TruffleString.CodeRange.BROKEN, copy.getCodeRangeImpreciseUncached(TruffleString.Encoding.UTF_32));
        assertEquals(TruffleString.CodeRange.BROKEN, copy.getCodeRangeUncached(TruffleString.Encoding.UTF_32));
        assertEquals(
                TruffleString.CompactionLevel.S4,
                copy.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32)
        );
    }

    /// Test operations that fail for non-Unicode codepoints
    @Test
    public void testEmacsMaxCharFailures() {
        TruffleStringBuilderUTF32 sb = TruffleStringBuilder.createUTF32();
        assertThrowsExactly(
                IllegalArgumentException.class,
                () -> sb.appendCodePointUncached(EMACS_MAX_CODEPOINT)
        );
    }

    @Test
    public void testUtf32InternalBytes() {
        TruffleStringBuilderUTF32 sb = TruffleStringBuilder.createUTF32();
        sb.appendCodePointUncached('a');
        sb.appendCodePointUncached('b');
        TruffleString s = sb.toStringUncached();
        assertEquals(2, s.codePointLengthUncached(TruffleString.Encoding.UTF_32));
        assertEquals('a', s.codePointAtIndexUncached(0, TruffleString.Encoding.UTF_32));
        assertEquals('b', s.codePointAtIndexUncached(1, TruffleString.Encoding.UTF_32));
        assertEquals(TruffleString.CompactionLevel.S1, s.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32));
        InternalByteArray bytes = s.getInternalByteArrayUncached(TruffleString.Encoding.UTF_32);
        assertEquals(8, bytes.getLength());

        TruffleString conv = s.switchEncodingUncached(TruffleString.Encoding.ISO_8859_1);
        bytes = conv.getInternalByteArrayUncached(TruffleString.Encoding.ISO_8859_1);
        assertEquals(2, bytes.getLength());
        assertEquals('a', bytes.get(0));
        assertEquals('b', bytes.get(1));

        bytes = s.getInternalByteArrayUncached(TruffleString.Encoding.ISO_8859_1);
        assertEquals(2, bytes.getLength());
        assertEquals('a', bytes.get(0));
        assertEquals('b', bytes.get(1));
    }

    private void assertRangePrecise(TruffleString.CodeRange range, TruffleString s) {
        TruffleString.CodeRange actual = s.getCodeRangeUncached(TruffleString.Encoding.UTF_32);
        assertEquals(range, actual);
    }

    @Test
    public void testUtf32Range() {
        TruffleString s = TruffleString.fromJavaStringUncached("test", TruffleString.Encoding.UTF_32);
        assertRangePrecise(TruffleString.CodeRange.ASCII, s);

        s = TruffleString.fromJavaStringUncached("test", TruffleString.Encoding.BYTES);
        assertRangePrecise(TruffleString.CodeRange.ASCII, s);
    }
}
