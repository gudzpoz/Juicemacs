package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.InternalByteArray;
import com.oracle.truffle.api.strings.TruffleString;
import org.jspecify.annotations.Nullable;

/// A hacky string buffer for [TruffleString]
///
/// [TruffleString] does not expect modifications to its internal byte array,
/// while its mutable variant [com.oracle.truffle.api.strings.MutableTruffleString]
/// does not support string compaction. This class tries to hack through these
/// limitations by modifying the internal array in a way that is compatible with
/// Truffle internal compaction.
final class StringBuffer {
    private final LineStartList lineStarts;
    private final TruffleString buffer;
    /// The internal array of [#buffer], not null if the buffer is mutable
    private final byte @Nullable [] bytes;
    private int size;

    /// Creates an immutable string buffer
    public StringBuffer(TruffleString buffer) {
        this(buffer, PieceTreeBase.createLineStartsFast(buffer));
    }

    /// Creates an immutable string buffer
    public StringBuffer(TruffleString buffer, LineStartList lineStarts) {
        this.buffer = buffer;
        this.lineStarts = lineStarts;
        this.bytes = null;
        this.size = StringNodes.length(buffer);
    }

    /// Creates a mutable string buffer
    public StringBuffer(TruffleString.CompactionLevel level) {
        this(level, new LineStartList(), 128);
        lineStarts.add(0);
    }

    /// Creates a mutable string buffer
    private StringBuffer(TruffleString.CompactionLevel level, LineStartList lines, int newLength) {
        int unit = level.getBytes();
        this.bytes = new byte[unit * newLength];
        for (int i = 0; i < unit; i++) {
            bytes[i] = (byte) (unit | 0x80);
        }
        this.buffer = switch (level) {
            case S1 -> TruffleString.fromByteArrayUncached(bytes, TruffleString.Encoding.ISO_8859_1, false)
                    .switchEncodingUncached(TruffleString.Encoding.UTF_32);
            case S2 -> TruffleString.fromByteArrayUncached(bytes, TruffleString.Encoding.UTF_16, false)
                    .switchEncodingUncached(TruffleString.Encoding.UTF_32);
            case S4 -> TruffleString.fromByteArrayUncached(bytes, TruffleString.Encoding.UTF_32, false);
        };
        assert buffer.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32) == level;
        this.lineStarts = lines;
        this.size = 1;
    }

    public boolean isEmpty() {
        return buffer.isEmpty();
    }

    public LineStartList lineStarts() {
        return lineStarts;
    }

    public TruffleString.@Nullable CompactionLevel compactionLevel() {
        if (bytes == null) {
            return null;
        }
        return buffer.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_8);
    }

    public int length() {
        return size;
    }

    public TruffleString substring(int start, int length) {
        return StringNodes.substring(buffer, start, length);
    }

    public int charAt(int index) {
        return StringNodes.charAt(buffer, index);
    }

    private StringBuffer ensureCapacity(int extra) {
        int length = StringNodes.length(buffer);
        if (size + extra > length) {
            do {
                length *= 2;
            } while (size + extra > length);
            StringBuffer expanded = new StringBuffer(
                    buffer.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32),
                    this.lineStarts,
                    length
            );
            assert this.bytes != null;
            assert expanded.bytes != null;
            System.arraycopy(this.bytes, 0, expanded.bytes, 0, this.bytes.length);
            expanded.size = this.size;
            return expanded;
        }
        return this;
    }

    public StringBuffer append(AbstractTruffleString text, int level) {
        int extra = StringNodes.length(text);
        if (extra == 0) {
            return this;
        }
        StringBuffer buffer = ensureCapacity(extra);
        assert buffer.bytes != null;
        assert (buffer.bytes[0] & 0x0F) == (1 << level);
        switch (level) {
            case 0 -> {
                InternalByteArray array = StringNodes.bytes(text, TruffleString.Encoding.ISO_8859_1);
                System.arraycopy(
                        array.getArray(),
                        array.getOffset(),
                        buffer.bytes,
                        buffer.size,
                        array.getLength()
                );
            }
            case 1 -> {
                InternalByteArray array = StringNodes.bytes(text, TruffleString.Encoding.UTF_16);
                System.arraycopy(
                        array.getArray(),
                        array.getOffset(),
                        buffer.bytes,
                        buffer.size * 2,
                        array.getLength()
                );
            }
            case 2 -> {
                InternalByteArray array = StringNodes.bytes(text, TruffleString.Encoding.UTF_32);
                System.arraycopy(
                        array.getArray(),
                        array.getOffset(),
                        buffer.bytes,
                        buffer.size * 4,
                        array.getLength()
                );
            }
            default -> throw CompilerDirectives.shouldNotReachHere();
        }
        buffer.size += extra;
        return buffer;
    }
}
