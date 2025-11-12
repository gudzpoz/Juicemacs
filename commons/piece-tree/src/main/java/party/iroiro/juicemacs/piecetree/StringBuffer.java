package party.iroiro.juicemacs.piecetree;

import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.mule.ByteArrayBuilder;
import party.iroiro.juicemacs.mule.CodingUtils;
import party.iroiro.juicemacs.mule.Utf8Utils;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

final class StringBuffer extends ByteArrayBuilder {
    private static final int CHAR_SEGMENT_LENGTH = 1 << 8;
    static final byte[] EMPTY_STRING = new byte[0];

    /// Char offsets into each line starts
    private final IndexList lineStarts;
    /// Byte offsets into each substring segment for non-raw buffers
    ///
    /// For raw buffers, this should be null.
    @Nullable
    private final IndexList charStarts;
    private int chars;
    private int cachedCharIndex;
    private int cachedByteIndex;

    private StringBuffer(byte[] storage, boolean raw) {
        super(storage, 0);
        this.lineStarts = new IndexList();
        lineStarts.add(0);
        if (raw) {
            this.charStarts = null;
        } else {
            this.charStarts = new IndexList();
        }
        this.chars = 0;
    }

    /// Creates an immutable string buffer
    public static StringBuffer immutable(byte[] init, boolean raw) {
        StringBuffer buffer = mutable(init, raw);
        buffer.lineStarts.trimToSize();
        if (buffer.charStarts != null) {
            buffer.charStarts.trimToSize();
        }
        return buffer;
    }

    /// Creates a mutable string buffer
    public static StringBuffer mutable(byte[] init, boolean raw) {
        StringBuffer buffer = new StringBuffer(init, raw);
        buffer.usedBytes = init.length;
        buffer.updateAppended(0, init.length);
        return buffer;
    }

    /// Creates a mutable string buffer
    public static StringBuffer mutable(int capacity, boolean raw) {
        return new StringBuffer(new byte[capacity], raw);
    }

    public boolean isRaw() {
        return charStarts == null;
    }

    private void updateAppended(int from, int to) {
        byte[] bytes = this.bytes;
        checkRange(bytes, from, to);
        int chars = this.chars;
        int i = from;
        boolean raw = charStarts == null;
        while (true) {
            while (true) {
                if (i >= to) {
                    this.chars = chars;
                    return;
                }
                if (!raw && (chars & (CHAR_SEGMENT_LENGTH - 1)) == 0) {
                    charStarts.add(i);
                }
                chars++;
                int b;
                if ((b = Byte.toUnsignedInt(bytes[i++])) >= 0x80) {
                    break;
                }
                if (b == '\n') {
                    lineStarts.add(chars);
                }
            }

            if (raw) {
                continue;
            }
            // skip UTF-8 continuation bytes
            while (i < to && (bytes[i] & 0b1100_0000) == 0b1000_0000) {
                i++;
            }
        }
    }

    public boolean isEmpty() {
        return chars == 0;
    }

    public IndexList lineStarts() {
        return lineStarts;
    }

    public int bytes() {
        return usedBytes;
    }

    public int length() {
        return chars;
    }

    public byte[] inner() {
        return bytes;
    }

    public byte[] substring(int startChars, int lengthChars) {
        if (lengthChars == 0) {
            return EMPTY_STRING;
        }
        int start = charIndexToByteIndex(startChars);
        int end = charIndexToByteIndex(startChars + lengthChars);
        return Arrays.copyOfRange(bytes, start, end);
    }

    public void substring(int startChars, int lengthChars, ByteArrayOutputStream output) {
        if (lengthChars == 0) {
            return;
        }
        int start = charIndexToByteIndex(startChars);
        int end = charIndexToByteIndex(startChars + lengthChars);
        output.write(bytes, start, end - start);
    }

    public int charIndexToByteIndex(int index) {
        if (charStarts == null || index <= 0) {
            return index;
        }

        if (index == cachedCharIndex) {
            return cachedByteIndex;
        }

        int segment = (index - 1) >> 8;
        int priorByteIndex = charStarts.get(segment);
        int remainingChars = index - (segment << 8);
        int byteIndex;
        if (remainingChars == 0) {
            byteIndex = priorByteIndex;
        } else {
            byteIndex = Utf8Utils.codepointIndexToByteIndex(
                    bytes, false,
                    remainingChars, priorByteIndex
            );
        }
        cachedCharIndex = index;
        cachedByteIndex = byteIndex;
        return byteIndex;
    }

    public int charAtSlow(int index) {
        if (charStarts == null) {
            return bytes[index] & 0xFF;
        }
        int i = charIndexToByteIndex(index);
        return (int) CodingUtils.readCodepointAndByteLength(bytes, i);
    }

    public StringBuffer append(byte[] bytes) {
        if (bytes.length == 0) {
            return this;
        }
        int from = this.usedBytes;
        writeBytes(bytes, 0, bytes.length);
        updateAppended(from, usedBytes);
        return this;
    }
}
