package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleString.CodeRange;
import com.oracle.truffle.api.strings.TruffleString.Encoding;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/// Utilities for bridging between internal string bytes and externally supplied texts
///
/// ## Encodings
///
/// Actually, there are two encoding system under the `utf-8-emacs` coding: an internal one
/// and an external one. They share the same coding convention (see `character.h` in Emacs),
/// but differ in what are considered invalid byte sequences.
///
/// - Internal encoding: it is used in the internal encoding of Emacs strings. As is documented
///   in `character.h`:
///   - It uses two-byte sequences like `1100000x 10xxxxxx` to represent raw bytes, achieving
///     (more or less) lossless invalid byte storage.
///   - It extends the UTF-8 encoding to 5-byte sequences to represent codepoints outside of
///     Unicode ranges (`0 - 0x10FFFF`).
/// - External encoding: `utf-8-emacs`, this is what gets saved to files, or is accepted as
///   the input to `decode-coding-string`.
///   - Since raw byte sequences in the internal encoding are for invalid bytes only,
///     they should never be exposed to users. So, they are saved, well, as raw bytes.
///   - That is, reading a file containing `1100000x 10xxxxxx`, we treat it as invalid,
///     and convert them to our internal encoding, with two 2-byte raw byte sequences (4 bytes).
public abstract class CodingUtils {
    private CodingUtils() {
    }

    /// ASCII state: the bytes are valid ASCII.
    public static final int STATE_ASCII = 0b00;
    /// UTF-8 state: the bytes are valid UTF-8.
    public static final int STATE_UTF_8 = 0b01;
    /// Bytes state: the bytes are raw bytes.
    ///
    /// This state is incompatible only with ASCII states.
    public static final int STATE_BYTES = 0b10;
    /// Emacs state: the bytes are in utf-8-emacs encoding.
    public static final int STATE_EMACS = 0b11;

    public static final int STATE_MASK = 0b11;

    public static boolean isUnibyteState(int state) {
        return (state & 1) == 0;
    }

    private static final TruffleString.FromByteArrayNode FROM_BYTE_ARRAY =
            TruffleString.FromByteArrayNode.create();
    private static final TruffleString.GetCodeRangeImpreciseNode GET_CODE_RANGE_IMPRECISE =
            TruffleString.GetCodeRangeImpreciseNode.create();
    private static final TruffleString.CodePointLengthNode CODE_POINT_LENGTH =
            TruffleString.CodePointLengthNode.create();

    /// @param codepointLength the length of the string in codepoints
    /// @param state the state (see [#STATE_ASCII], [#STATE_UTF_8], [#STATE_BYTES], [#STATE_EMACS])
    /// @param bytes the bytes of the string, encoded in the `utf-8-emacs` internal encoding
    public record StringAttrs(int codepointLength, int state, byte[] bytes) {
        public static StringAttrs calculate(byte[] bytes) {
            return calculate(bytes, 0, bytes.length);
        }
        /// Calculate the attrs of a string coming from a byte array
        public static StringAttrs calculate(byte[] bytes, int byteOffset, int byteLength) {
            TruffleString checked = FROM_BYTE_ARRAY.execute(
                    bytes, byteOffset, byteLength, Encoding.UTF_8, false
            );
            CodeRange range = GET_CODE_RANGE_IMPRECISE.execute(checked, Encoding.UTF_8);
            if (range != CodeRange.BROKEN) {
                assert range == CodeRange.ASCII || range == CodeRange.VALID;
                int length = CODE_POINT_LENGTH.execute(checked, Encoding.UTF_8);
                int state = range == CodeRange.ASCII ? STATE_ASCII : STATE_UTF_8;
                return new StringAttrs(length, state, bytes);
            }
            return decodeEmacsUtf8(bytes, byteOffset, byteLength);
        }
    }

    /// Encode a basically-UTF-8 string into `utf-8-emacs` coding,
    /// turning invalid bytes into a lossless representation
    ///
    /// @return the re-encoded bytes, or `null` if no encoding is needed
    public static StringAttrs decodeEmacsUtf8(byte[] bytes, int byteOffset, int byteLength) {
        int end = byteOffset + byteLength;
        // removes bound checks
        if (byteOffset < 0 || end < byteOffset || bytes.length < end) {
            throw new IndexOutOfBoundsException();
        }
        int encodedEnd = 0;
        int codepoints = 0;
        byte[] encoded = null;
        int validStart = byteOffset;
        int index = byteOffset;
        loop:
        while (true) {
            int b;
            do {
                if (index >= end) {
                    break loop;
                }
                codepoints++;
            } while ((b = Byte.toUnsignedInt(bytes[index++])) <= 0x7F);

            //noinspection StatementWithEmptyBody
            if (b <= 0xC1) {
                // C0-C1 is for emacs internal eight-bit chars,
                // but since we are handling external bytes,
                // they are invalid.
                // fall through to invalid byte handling
            } else if (b <= 0xDF) {
                // 2-byte UTF-8
                if (index < end && isContinuationByte(bytes[index])) {
                    index += 1;
                    continue;
                }
                // fall through
            } else if (b <= 0xEF) {
                // 3-byte UTF-8
                if (index + 1 < end
                        && isContinuationByte(bytes[index])
                        && isContinuationByte(bytes[index + 1])) {
                    // surrogates are invalid
                    int cUpper = ((b & 0b0000_1111) << 4) | ((bytes[index] & 0b0011_1100) >> 2);
                    if ((cUpper & 0b1111_1000) != 0b11011000) { // 0xD8-0xDF
                        index += 2;
                        continue;
                    }
                }
                // fall through
            } else if (b <= 0xF7) {
                // 4-byte UTF-8
                if (index + 2 < end
                        && isContinuationByte(bytes[index])
                        && isContinuationByte(bytes[index + 1])
                        && isContinuationByte(bytes[index + 2])) {
                    index += 3;
                    continue;
                }
                // fall through
            } else if (b == 0xF8) {
                // F8 marks the beginning of the #x200000-3FFF7F code range,
                // used in the utf-8-emacs coding.
                if (index + 3 < end
                        && isContinuationByte(bytes[index])
                        // upper limit: 0x3FFFFF
                        && (bytes[index] & 0b1111_0000) == 0b1000_0000
                        && isContinuationByte(bytes[index + 1])
                        && isContinuationByte(bytes[index + 2])
                        && isContinuationByte(bytes[index + 3])) {
                    // upper limit: 0x3FFF7F
                    int c = (bytes[index + 3] & 0b0011_1111)
                            | ((bytes[index + 2] & 0b0011_1111) << 6)
                            | ((bytes[index + 1] & 0b0011_1111) << 12)
                            | ((bytes[index] & 0b0011_1111) << 18);
                    // we differ from emacs here: emacs seems to convert 5-byte 0x3FFFFF into
                    // 2-byte raw byte sequence, but we treat it as invalid sequence here.
                    if (c <= 0x3FFF7F) {
                        index += 4;
                        continue;
                    }
                }
                // fall through
            } // else (b <= 0xFF); /* also invalid; fall through */

            // Invalid byte handling
            encoded = commitInvalidByte(bytes, validStart, index - 1, encoded, encodedEnd, b);
            encodedEnd += index - validStart + 1;
            validStart = index;
        }

        if (encoded == null) {
            return new StringAttrs(
                    codepoints, STATE_EMACS,
                    byteOffset == 0 && byteLength == bytes.length
                            ? bytes
                            : Arrays.copyOfRange(bytes, byteOffset, end)
            );
        }
        encoded = Arrays.copyOf(encoded, encodedEnd + index - validStart);
        encoded = commitValidUtf8(bytes, validStart, index, encoded, encodedEnd, 0);
        return new StringAttrs(codepoints, STATE_EMACS, encoded);
    }
    private static boolean isContinuationByte(byte b) {
        return (b & 0b1100_0000) == 0b1000_0000;
    }
    private static byte[] commitInvalidByte(
            byte[] src, int from, int to,
            byte @Nullable[] dst, int dstOffset,
            int invalidByte
    ) {
        dst = commitValidUtf8(src, from, to, dst, dstOffset, 2);
        int base = dstOffset + to - from;
        dst[base] = (byte) (0b1100_0000 | ((invalidByte >> 6) & 1));
        dst[base + 1] = (byte) (0b1000_0000 | (invalidByte & 0b0011_1111));
        return dst;
    }
    private static byte[] commitValidUtf8(
            byte[] src, int from, int to,
            byte @Nullable[] dst, int dstOffset,
            int extraLength
    ) {
        if (dst == null) {
            assert dstOffset == 0;
            return Arrays.copyOfRange(src, from, to + extraLength);
        }
        int len = to - from;
        int required = dstOffset + len + extraLength;
        if (required > dst.length) {
            dst = Arrays.copyOf(dst, required + required >> 1);
        }
        System.arraycopy(src, from, dst, dstOffset, len);
        return dst;
    }

    /// Encodes internal `utf-8-emacs` coding to (possibly invalid) UTF-8.
    ///
    /// It assumes that the input is valid `utf-8-emacs` coding.
    public static void encodeEmacsUtf8(byte[] bytes, int from, int to, CodedStringWriter dst) throws IOException {
        if (from < 0 || to < from || bytes.length < to) {
            throw new IndexOutOfBoundsException();
        }
        int validStart = from;
        int i = from;
        loop:
        while (true) {
            int b;
            do {
                if (i >= to) {
                    break loop;
                }
            } while (((b = Byte.toUnsignedInt(bytes[i++])) & 0b1111_1110) != 0xC0); // 0xC0-0xC1

            dst.writeUtf8(bytes, validStart, i - 1);
            if (i == to) {
                validStart = i;
                break;
            }
            int raw = 0x80 | ((b & 1) << 6) | (bytes[i] & 0b0011_1111);
            dst.writeRawByte((byte) raw);
            validStart = ++i;
        }
        dst.writeUtf8(bytes, validStart, i);
    }

    public static int calculateUnibyteState(byte[] bytes) {
        for (byte b : bytes) {
            if (b < 0) {
                return STATE_BYTES;
            }
        }
        return STATE_ASCII;
    }

    public static byte[] fromString(String s) {
        return s.getBytes(StandardCharsets.UTF_8);
    }

    public static int codepointUtf8ByteLength(int c) {
        return c < 0x80 ? 1
                : c < 0x800 ? 2
                : c < 0x10000 ? 3
                : c < 0x200000 ? 4
                : c < 0x3FFF80 ? 5
                : 2;
    }

    public static void writeRawByte(byte b, ByteArrayBuilder out) {
        if (b >= 0) {
            out.writeByte(b);
        } else {
            out.ensureCapacity(2);
            out.bytes[out.usedBytes++] = (byte) (0b1100_0000 | ((b >> 6) & 1));
            out.bytes[out.usedBytes++] = (byte) (0b1000_0000 | (b & 0b0011_1111));
        }
    }

    public static void writeCodepoint(int c, ByteArrayBuilder out) {
        byte[] buffer = new byte[5];
        int length = writeCodepoint(c, buffer, 0);
        out.writeBytes(buffer, 0, length);
    }

    public static int writeCodepoint(int c, byte[] bytes, int offset) {
        if (c < 0x80) {
            bytes[offset] = (byte) c;
            return 1;
        }
        if (c < 0x800) {
            bytes[offset] = (byte) (0b1100_0000 | (c >> 6));
            bytes[offset + 1] = (byte) (0b1000_0000 | (c & 0b0011_1111));
            return 2;
        }
        if (c < 0x10000) {
            bytes[offset] = (byte) (0b1110_0000 | (c >> 12));
            bytes[offset + 1] = (byte) (0b1000_0000 | ((c >> 6) & 0b0011_1111));
            bytes[offset + 2] = (byte) (0b1000_0000 | (c & 0b0011_1111));
            return 3;
        }
        if (c < 0x200000) {
            bytes[offset] = (byte) (0b1111_0000 | (c >> 18));
            bytes[offset + 1] = (byte) (0b1000_0000 | ((c >> 12) & 0b0011_1111));
            bytes[offset + 2] = (byte) (0b1000_0000 | ((c >> 6) & 0b0011_1111));
            bytes[offset + 3] = (byte) (0b1000_0000 | (c & 0b0011_1111));
            return 4;
        }
        if (c < 0x3FFF80) {
            bytes[offset] = (byte) (0b1111_1000);
            bytes[offset + 1] = (byte) (0b1000_0000 | ((c >> 18) & 0b0011_1111));
            bytes[offset + 2] = (byte) (0b1000_0000 | ((c >> 12) & 0b0011_1111));
            bytes[offset + 3] = (byte) (0b1000_0000 | ((c >> 6) & 0b0011_1111));
            bytes[offset + 4] = (byte) (0b1000_0000 | (c & 0b0011_1111));
            return 5;
        }
        if (c < 0x4000000) {
            bytes[offset] = (byte) (0b1100_0000 | ((c >> 6) & 1));
            bytes[offset + 1] = (byte) (0b1000_0000 | (c & 0b0011_1111));
            return 2;
        }
        throw new IllegalArgumentException();
    }

    /// Reads the utf-8 codepoint at `bytes[i..]`
    ///
    /// The return value is a hack to return multiple value in Java:
    /// - `(int) ret`: the codepoint
    /// - `(int) (ret >> 32)`: the number of bytes read
    ///
    /// It assumes valid utf-8-emacs sequence.
    public static long readCodepointAndByteLength(byte[] bytes, int i) {
        int b = bytes[i] & 0xFF;
        if (b < 0x80) {
            return b | 0x1_0000_0000L;
        }
        if (b < 0xC2) {
            return 0x3FFF80 | ((b & 1) << 6) | (bytes[i + 1] & 0b0011_1111)
                    | 0x2_0000_0000L;
        }
        if (b < 0xE0) {
            return ((b & 0b0001_1111) << 6)
                    | (bytes[i + 1] & 0b0011_1111)
                    | 0x2_0000_0000L;
        }
        if (b < 0xF0) {
            return ((b & 0b0000_1111) << 12)
                    | ((bytes[i + 1] & 0b0011_1111) << 6)
                    | (bytes[i + 2] & 0b0011_1111)
                    | 0x3_0000_0000L;
        }
        if (b < 0xF8) {
            return ((b & 0b0000_0111) << 18)
                    | ((bytes[i + 1] & 0b0011_1111) << 12)
                    | ((bytes[i + 2] & 0b0011_1111) << 6)
                    | (bytes[i + 3] & 0b0011_1111)
                    | 0x4_0000_0000L;
        }
        return ((bytes[i + 1] & 0b0011_1111) << 18)
                | ((bytes[i + 2] & 0b0011_1111) << 12)
                | ((bytes[i + 3] & 0b0011_1111) << 6)
                | (bytes[i + 4] & 0b0011_1111)
                | 0x5_0000_0000L;
    }
}
