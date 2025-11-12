package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleString.CodeRange;
import com.oracle.truffle.api.strings.TruffleString.Encoding;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.mule.CodingUtils.*;

@SuppressWarnings("OctalInteger")
public class CodingUtilsTest {
    /// Encoded Emacs character of codepoint `#x3FFF7F`
    private static final byte[] EMACS_BYTES = new byte[]{
            (byte) 0370,
            (byte) 0217,
            (byte) 0277,
            (byte) 0275,
            (byte) 0277,
    };

    private static TruffleString decode(byte[] bytes) {
        return TruffleString.fromByteArrayUncached(
                bytes,
                0, bytes.length,
                Encoding.UTF_8,
                false
        );
    }

    @Test
    void testIsUnibyte() {
        assertTrue(CodingUtils.isUnibyteState(STATE_ASCII));
        assertTrue(CodingUtils.isUnibyteState(STATE_BYTES));
        assertFalse(CodingUtils.isUnibyteState(STATE_UTF_8));
        assertFalse(CodingUtils.isUnibyteState(STATE_EMACS));
    }

    @Test
    void testJavaString() {
        assertEncodeDecoded(fromString("abc"));
        assertEncodeDecoded(fromString("abcdé"));
        assertEncodeDecoded(fromString("é語🟣"));
    }

    @Test
    void testDecodeUtf8Truffle() {
        byte[] abcBytes = "abc".getBytes(StandardCharsets.UTF_8);
        TruffleString abc = decode(abcBytes);
        assertEquals(CodeRange.ASCII, abc.getCodeRangeImpreciseUncached(Encoding.UTF_8));
        assertEquals(3, abc.codePointLengthUncached(Encoding.UTF_8));
        assertEquals(
                new StringAttrs(3, STATE_ASCII, abcBytes),
                StringAttrs.calculate(abcBytes)
        );
        assertReEncoded(decodeEmacsUtf8(abcBytes, 0, abcBytes.length), abcBytes, false);
        assertEncodeDecoded(abcBytes);

        byte[] latinBytes = "abcdé".getBytes(StandardCharsets.UTF_8);
        TruffleString latin1 = decode(latinBytes);
        assertEquals(CodeRange.VALID, latin1.getCodeRangeImpreciseUncached(Encoding.UTF_8));
        assertEquals(5, latin1.codePointLengthUncached(Encoding.UTF_8));
        assertEquals(
                new StringAttrs(5, STATE_UTF_8, latinBytes),
                StringAttrs.calculate(latinBytes)
        );
        assertReEncoded(decodeEmacsUtf8(latinBytes, 0, latinBytes.length), latinBytes, false);
        assertEncodeDecoded(latinBytes);

        TruffleString emacs = decode(EMACS_BYTES);
        assertEquals(CodeRange.BROKEN, emacs.getCodeRangeImpreciseUncached(Encoding.UTF_8));
        assertEquals(5, emacs.codePointLengthUncached(Encoding.UTF_8));
        assertEquals(
                new StringAttrs(1, STATE_EMACS, EMACS_BYTES),
                StringAttrs.calculate(EMACS_BYTES)
        );
        assertReEncoded(decodeEmacsUtf8(EMACS_BYTES, 0, EMACS_BYTES.length), EMACS_BYTES, false);
        assertEncodeDecoded(EMACS_BYTES);

        for (int i = 128; i <= 255; i++) {
            byte[] emacsUtf8 = encodedRawBytes(i);
            TruffleString emacsUnibyte = decode(emacsUtf8);
            assertEquals(CodeRange.BROKEN, emacsUnibyte.getCodeRangeImpreciseUncached(Encoding.UTF_8));
            assertEquals(2, emacsUnibyte.codePointLengthUncached(Encoding.UTF_8));

            StringAttrs decoded = decodeEmacsUtf8(emacsUtf8, 0, emacsUtf8.length);
            assertNotNull(decoded);
            byte[] encoded = decoded.bytes();
            // Raw bytes are only valid in string internal bytes,
            // and external bytes are re-encoded:
            byte[] expected = {
                    (byte) (0b11000000 | ((emacsUtf8[0] >> 6) & 1)),
                    (byte) (0b10000000 | (emacsUtf8[0] & 0b111111)),
                    (byte) (0b11000000 | ((emacsUtf8[1] >> 6) & 1)),
                    (byte) (0b10000000 | (emacsUtf8[1] & 0b111111)),
            };
            assertArrayEquals(expected, encoded);
            assertEncodeDecoded(emacsUtf8);

            StringAttrs attrs = StringAttrs.calculate(emacsUtf8);
            assertEquals(2, attrs.codepointLength());
            assertEquals(STATE_EMACS, attrs.state());
            assertArrayEquals(expected, attrs.bytes());
        }
    }

    private static byte[] encodedRawBytes(int i) {
        return new byte[]{
                (byte) (0b11000000 | ((i >> 6) & 1)),
                (byte) (0b10000000 | (i & 0b111111)),
        };
    }

    private void assertReEncoded(StringAttrs attrs, byte[] original, boolean reEncode) {
        byte[] encoded = attrs.bytes();
        if (reEncode) {
            assertNotSame(original, encoded);
        } else {
            assertSame(original, encoded);
        }
    }

    private void assertDecodeEmacsUtf8(byte[] bytes, int[] expected) {
        byte[] expectedBytes = getBytes(expected);
        StringAttrs attrs = decodeEmacsUtf8(bytes, 0, bytes.length);
        assertReEncoded(attrs, bytes, true);
        assertArrayEquals(expectedBytes, attrs.bytes());
        assertEncodeDecoded(bytes);
    }

    private void assertEncodeDecoded(byte[] bytes) {
        StringAttrs attrs = decodeEmacsUtf8(bytes, 0, bytes.length);
        byte[] decoded = attrs.bytes();
        ByteArrayOutputStream encoded = new ByteArrayOutputStream();
        assertDoesNotThrow(() ->
            encodeEmacsUtf8(decoded, 0, decoded.length, new CodedStringWriter() {
                @Override
                public void writeUtf8(byte[] bytes, int from, int to) {
                    encoded.write(bytes, from, to - from);
                }

                @Override
                public void writeRawByte(byte b) {
                    encoded.write(b);
                }
            })
        );
        assertArrayEquals(bytes, encoded.toByteArray());
    }

    private static byte[] getBytes(Object bytes) {
        byte[] bytesBytes;
        if (bytes instanceof byte[] array) {
            bytesBytes = array;
        } else {
            int[] array = (int[]) bytes;
            bytesBytes = new byte[array.length];
            for (int i = 0; i < array.length; i++) {
                bytesBytes[i] = (byte) array[i];
            }
        }
        return bytesBytes;
    }

    @Test
    void testDecodeEmacsUtf8() {
        for (int i = 128; i < 255; i++) {
            byte[] bytes = encodedRawBytes(i);
            assertDecodeEmacsUtf8(
                    new byte[]{(byte) i},
                    new int[]{bytes[0], bytes[1]}
            );
            for (int c = 'a'; c <= 'z'; c++) {
                assertDecodeEmacsUtf8(
                        new byte[]{(byte) i, (byte) c},
                        new int[]{bytes[0], bytes[1], c}
                );
            }
        }

        for (int c = 0; c <= Character.MAX_CODE_POINT; c++) {
            byte[] bytes = new StringBuilder().appendCodePoint(c).toString().getBytes(StandardCharsets.UTF_8);
            assertReEncoded(decodeEmacsUtf8(bytes, 0, bytes.length), bytes, false);
        }

        for (byte[] bytes : new byte[][]{
                "é".getBytes(StandardCharsets.UTF_8),
                "語".getBytes(StandardCharsets.UTF_8),
                "🟣".getBytes(StandardCharsets.UTF_8),
                EMACS_BYTES,
        }) {
            assertReEncoded(decodeEmacsUtf8(bytes, 0, bytes.length), bytes, false);
            assertTrue(1 < bytes.length && bytes.length <= 5);

            for (int modifiedI = 1; modifiedI < bytes.length; modifiedI++) {
                byte[] copy = bytes.clone();
                copy[modifiedI] = (byte) ('a' + modifiedI);
                int[] expected = new int[copy.length * 2 - 1];
                for (int i = 0; i < copy.length; i++) {
                    int base = i > modifiedI ? i * 2 - 1 : i * 2;
                    if (i == modifiedI) {
                        expected[base] = copy[i];
                    } else {
                        byte[] invalid = encodedRawBytes(copy[i]);
                        expected[base] = invalid[0];
                        expected[base + 1] = invalid[1];
                    }
                }
                assertDecodeEmacsUtf8(copy, expected);
            }
        }
    }

    @Test
    void testOutOfRange() {
        assertThrows(IndexOutOfBoundsException.class, () -> decodeEmacsUtf8(new byte[0], 0, 1));
        assertThrows(IndexOutOfBoundsException.class, () -> decodeEmacsUtf8(new byte[0], 1, 1));
        assertThrows(IndexOutOfBoundsException.class, () -> decodeEmacsUtf8(new byte[1], 1, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> decodeEmacsUtf8(new byte[0], -1, 1));
    }

    @Test
    void testMaxCodepoint() {
        for (boolean good : new boolean[]{true, false}) {
            byte[] bytes = {
                    (byte) 0b1111_1000,
                    (byte) 0b1000_1111,
                    (byte) 0b1011_1111,
                    (byte) (0b1011_1101 | (good ? 0 : 0b0000_0010)),
                    (byte) 0b1011_1111,
            };
            if (good) {
                assertReEncoded(decodeEmacsUtf8(bytes, 0, bytes.length), bytes, false);
            } else {
                StringAttrs attrs = decodeEmacsUtf8(bytes, 0, bytes.length);
                assertNotNull(attrs);
                byte[] decoded = attrs.bytes();
                assertEquals(10, decoded.length);
            }

            bytes = new byte[]{
                    (byte) 0b1111_1000,
                    (byte) (0b1000_1111 | (good ? 0 : 0b0001_0000)),
                    (byte) 0b1011_1111,
                    (byte) 0b1011_1101,
                    (byte) 0b1011_1111,
            };
            if (good) {
                assertReEncoded(decodeEmacsUtf8(bytes, 0, bytes.length), bytes, false);
            } else {
                StringAttrs attrs = decodeEmacsUtf8(bytes, 0, bytes.length);
                assertNotNull(attrs);
                byte[] decoded = attrs.bytes();
                assertEquals(10, decoded.length);
            }
        }
    }

    @Test
    void testAllCodepoints() {
        for (int c = 0; c <= 0x3FFF7F; c++) {
            byte[] bytes = new byte[5];
            int len = writeCodepoint(c, bytes, 0);
            assertEquals(codepointUtf8ByteLength(c), len);
            long res = readCodepointAndByteLength(Arrays.copyOfRange(bytes, 0, len), 0);
            assertEquals(c, res & Integer.MAX_VALUE);
            assertEquals(len, res >> 32);
        }
    }
}
