package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringBuilder;
import com.oracle.truffle.api.strings.TruffleStringBuilderUTF32;
import org.eclipse.collections.impl.list.mutable.primitive.LongArrayList;
import party.iroiro.juicemacs.mule.utils.ByteArrayList;
import party.iroiro.juicemacs.mule.utils.IntArrayList;

import java.util.ArrayList;
import java.util.PrimitiveIterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

public final class MuleStringBuffer implements MuleString {
    private static final TruffleString.FromIntArrayUTF32Node FROM_INT_ARRAY_UTF_32 =
            TruffleString.FromIntArrayUTF32Node.create();
    private static final TruffleStringBuilder.AppendStringNode APPEND_STRING_LATIN1 =
            TruffleStringBuilder.AppendStringNode.create();
    private static final TruffleStringBuilder.AppendStringNode APPEND_STRING_TRUFFLE =
            TruffleStringBuilder.AppendStringNode.create();
    private static final TruffleStringBuilder.ToStringNode TO_STRING = TruffleStringBuilder.ToStringNode.create();

    private static final int MAX_COPY_LIMIT = 4096;
    private static final int MAX_ARRAY_LENGTH = (1 << 16) - 1;

    private final LongArrayList startingCodePointIndices;
    private final ArrayList<MuleString> strings;

    /// State of the building array list
    ///
    /// ## State Changes
    ///
    /// ```
    /// 0 --> 1 --> 3 --> 4
    ///  \               /|\
    ///   \----> 2 --->---|
    /// ```
    ///
    /// @see #BUILDING_ASCII
    /// @see #BUILDING_LATIN_1
    /// @see #BUILDING_UNI_BYTES
    /// @see #BUILDING_UNICODE
    /// @see #BUILDING_MULE
    private int state;
    /// State `0`: byte string, chars (`0 ~ 0x7F`) stored in [#buildingBytes]
    static final int BUILDING_ASCII = MuleByteArrayString.STATE_ASCII;
    /// State `1`: byte string, including non-ASCII Latin-1 chars, stored in [#buildingBytes]
    static final int BUILDING_LATIN_1 = MuleByteArrayString.STATE_LATIN_1;
    /// State `2`: byte string, including non-ASCII raw bytes, stored in [#buildingBytes]
    static final int BUILDING_UNI_BYTES = MuleByteArrayString.STATE_UNI_BYTES;
    /// Nota valid state, but used to check if the buffer is byte-based (if `state < IS_BUILDING_BYTES`)
    private static final int IS_BUILDING_BYTES = 4;
    /// State `10`: Unicode with code points outside Latin1, stored in [#buildingCodePoints]
    private static final int BUILDING_UNICODE = 8;
    /// State `11`: Like `3`, but with non-Unicode code points over [Character#MAX_CODE_POINT]
    private static final int BUILDING_MULE = 16;
    /// Not a valid state but a flag
    ///
    /// Used in [#appendMuleString(MuleString, int, int)] when trying to copy over [#MAX_COPY_LIMIT]
    private static final int IS_BUILDING_COMMIT_NOW = 1 << 16;
    private final ByteArrayList buildingBytes;
    private final IntArrayList buildingCodePoints;

    public MuleStringBuffer() {
        startingCodePointIndices = new LongArrayList();
        startingCodePointIndices.add(0);
        strings = new ArrayList<>();
        buildingBytes = new ByteArrayList();
        buildingCodePoints = new IntArrayList();
        reset();
    }

    private void reset() {
        state = BUILDING_ASCII;
        buildingBytes.clear();
        buildingCodePoints.clear();
    }

    public void clear() {
        reset();
        strings.clear();
        startingCodePointIndices.clear();
        startingCodePointIndices.add(0);
    }

    private int buildingLength() {
        return state < IS_BUILDING_BYTES ? buildingBytes.size() : buildingCodePoints.size();
    }

    @Override
    public long length() {
        return startingCodePointIndices.getLast() + buildingLength();
    }

    private int getBuildingCodePoint(int relIndex) {
        if (state < IS_BUILDING_BYTES) {
            int original = Byte.toUnsignedInt(buildingBytes.get(relIndex));
            return state == BUILDING_UNI_BYTES ? MuleByteArrayString.uniByteCodePoint(original) : original;
        }
        return buildingCodePoints.get(relIndex);
    }

    @CompilerDirectives.TruffleBoundary
    private int offsetToStringIndex(long index) {
        if (index < 0) {
            throw new IndexOutOfBoundsException("Index out of bounds");
        } else {
            long last = startingCodePointIndices.getLast();
            if (index >= last) {
                int buildingIndex = Math.toIntExact(index - last);
                if (buildingIndex >= buildingLength()) {
                    throw new IndexOutOfBoundsException("Index out of bounds");
                }
                return -buildingIndex - 1;
            }
        }
        int stringIndex = startingCodePointIndices.binarySearch(index);
        if (stringIndex < 0) {
            stringIndex = -stringIndex - 2;
        }
        return stringIndex;
    }

    @Override
    public int codePointAt(long index) {
        int stringIndex = offsetToStringIndex(index);
        if (stringIndex < 0) {
            return getBuildingCodePoint(-stringIndex - 1);
        }
        long offset = startingCodePointIndices.get(stringIndex);
        return strings.get(stringIndex).codePointAt(index - offset);
    }

    private void finalizeCurrent() {
        if (buildingLength() == 0) {
            return;
        }
        startingCodePointIndices.add(length());
        strings.add(getBuildingString());
        reset();
    }

    private void ensureCapacity() {
        if (buildingLength() >= MAX_ARRAY_LENGTH) {
            finalizeCurrent();
        }
    }

    private MuleString getBuildingString() {
        if (state < IS_BUILDING_BYTES) {
            return new MuleByteArrayString(buildingBytes.toArray(), state);
        } else if (state == BUILDING_UNICODE) {
            return new MuleTruffleString(FROM_INT_ARRAY_UTF_32.execute(buildingCodePoints.toArray()));
        }
        return new MuleIntArrayString(buildingCodePoints.toArray());
    }

    public MuleStringBuffer appendRawByte(byte rawByte) {
        ensureCapacity();
        if ((state == BUILDING_ASCII && rawByte >= 0) || state == BUILDING_UNI_BYTES) {
            buildingBytes.add(rawByte);
            return this;
        }
        finalizeCurrent();
        state = BUILDING_UNI_BYTES;
        buildingBytes.add(rawByte);
        return this;
    }

    @CompilerDirectives.TruffleBoundary
    public MuleStringBuffer appendCodePoint(int codePoint) {
        ensureCapacity();
        if (state == BUILDING_ASCII) {
            if (codePoint <= 0x7F) {
                buildingBytes.add((byte) codePoint);
                return this;
            }
            state = BUILDING_LATIN_1;
        }
        if (state == BUILDING_UNI_BYTES) {
            if (codePoint <= 0x7F) {
                buildingBytes.add((byte) codePoint);
                return this;
            }
            finalizeCurrent();
            state = BUILDING_LATIN_1;
        }
        if (state == BUILDING_LATIN_1) {
            if (codePoint <= 0xFF) {
                buildingBytes.add((byte) codePoint);
                return this;
            }
            finalizeCurrent();
            state = BUILDING_UNICODE;
        }
        if (state == BUILDING_UNICODE) {
            if (codePoint <= Character.MAX_CODE_POINT) {
                buildingCodePoints.add(codePoint);
                return this;
            }
            finalizeCurrent();
            state = BUILDING_MULE;
        }
        buildingCodePoints.add(codePoint);
        return this;
    }

    public boolean isEmpty() {
        return startingCodePointIndices.getLast() == 0 && buildingLength() == 0;
    }

    public MuleStringBuffer append(MuleString string) {
        return appendMuleString(string, 0, string.length());
    }

    public MuleStringBuffer append(int codePoint) {
        return appendCodePoint(codePoint);
    }

    public MuleStringBuffer append(MuleString string, long start, long end) {
        return appendMuleString(string, start, end);
    }

    @CompilerDirectives.TruffleBoundary
    public MuleStringBuffer appendMuleString(MuleString string, long start, long end) {
        long addLength = end - start;
        if (addLength <= 0) {
            return this;
        }
        ensureCapacity();
        if (addLength > MAX_COPY_LIMIT && !(string instanceof MuleStringBuffer)) {
            state |= IS_BUILDING_COMMIT_NOW;
        }
        switch (string) {
            case MuleByteArrayString bytes when state < IS_BUILDING_BYTES
                    && (state | bytes.getState()) != (BUILDING_LATIN_1 | BUILDING_UNI_BYTES) -> {
                buildingBytes.addAll(bytes.bytes(), Math.toIntExact(start), (int) addLength);
                state |= bytes.getState();
            }
            case MuleTruffleString tString when state == BUILDING_UNICODE -> {
                PrimitiveIterator.OfInt codePoints = tString.codePoints(start).iterator();
                for (int i = 0; i < addLength; i++) {
                    buildingCodePoints.add(codePoints.nextInt());
                }
            }
            case MuleIntArrayString intArray when state == BUILDING_MULE ->
                    buildingCodePoints.addAll(intArray.intArray(), Math.toIntExact(start), (int) addLength);
            case MuleStringBuffer buffer -> appendBuffer(buffer, start, end);
            default -> {
                finalizeCurrent();
                state &= 0xFF;
                startingCodePointIndices.add(startingCodePointIndices.getLast() + addLength);
                strings.add(start == 0  && end == string.length() ? string : string.subSequence(start, end));
            }
        }
        return this;
    }

    private void appendBuffer(MuleStringBuffer buffer, long start, long end) {
        final int startI = buffer.offsetToStringIndex(start);
        final int endI = end == buffer.length() ? -1 : buffer.offsetToStringIndex(end);

        // Supplied buffer can be `this`. So we need to pre-store the states.
        long last = buffer.startingCodePointIndices.getLast();
        int bufferState = buffer.state;
        byte[] trailingBytes = null;
        int[] trailingCodePoints = null;
        if (endI == -1 && end != last) {
            if (bufferState < IS_BUILDING_BYTES) {
                trailingBytes = buffer.buildingBytes.toArray();
            } else {
                trailingCodePoints = buffer.buildingCodePoints.toArray();
            }
        }

        if (startI != -1) {
            int iterEndI = endI == -1 ? buffer.strings.size() - 1 : endI;
            int currentI = startI;
            while (currentI <= iterEndI) {
                MuleString substring = buffer.strings.get(currentI);
                long substringStart;
                if (currentI == startI) {
                    substringStart = start - buffer.startingCodePointIndices.get(currentI);
                } else {
                    substringStart = 0;
                }
                long substringEnd;
                if (currentI == endI) {
                    substringEnd = end - buffer.startingCodePointIndices.get(currentI);
                } else {
                    substringEnd = substring.length();
                }
                appendMuleString(substring, substringStart, substringEnd);
                currentI++;
            }
        }
        if (trailingBytes != null || trailingCodePoints != null) {
            finalizeCurrent();
            int substringStart = last > start ? 0 : (int) (start - last);
            int substringEnd = (int) (end - last);
            if (trailingBytes != null) {
                buildingBytes.addAll(trailingBytes, substringStart, substringEnd);
            }
            if (trailingCodePoints != null) {
                buildingCodePoints.addAll(trailingCodePoints, substringStart, substringEnd);
            }
            state = bufferState;
        }
    }

    public MuleStringBuffer copy() {
        return new MuleStringBuffer().appendMuleString(this, 0, length());
    }

    @CompilerDirectives.TruffleBoundary
    public MuleString build() {
        if (strings.isEmpty()) {
            return getBuildingString();
        } else if (strings.size() == 1 && buildingLength() == 0) {
            return strings.getFirst();
        } else if (length() > Integer.MAX_VALUE) {
            return copy();
        }
        int length = (int) this.length();
        int overallState = state;
        for (MuleString string : strings) {
            overallState |= switch (string) {
                case MuleByteArrayString s -> s.getState();
                case MuleIntArrayString _ -> BUILDING_MULE;
                case MuleTruffleString _ -> BUILDING_UNICODE;
                case MuleStringBuffer _ -> throw CompilerDirectives.shouldNotReachHere();
            };
        }
        if (overallState <= BUILDING_UNI_BYTES) { // ASCII + Latin1 or ASCII + Uni-byte
            byte[] bytes = new byte[length];
            int start = 0;
            for (MuleString string : strings) {
                MuleByteArrayString latin1 = (MuleByteArrayString) string;
                System.arraycopy(latin1.bytes(), 0, bytes, start, (int) latin1.length());
                start += (int) latin1.length();
            }
            System.arraycopy(buildingBytes.inner(), 0, bytes, start, buildingBytes.size());
            return new MuleByteArrayString(bytes, overallState);
        } else if ((overallState & BUILDING_UNI_BYTES) == 0
                && overallState <= (BUILDING_LATIN_1 | BUILDING_UNICODE)) { // Unicode + Latin1 + ASCII but no uni-byte
            TruffleStringBuilderUTF32 builder = TruffleStringBuilderUTF32.createUTF32(length * 2);
            for (MuleString string : strings) {
                if (string instanceof MuleByteArrayString latin1) {
                    APPEND_STRING_LATIN1.execute(builder, latin1.toTruffleString());
                } else {
                    MuleTruffleString truffle = (MuleTruffleString) string;
                    APPEND_STRING_TRUFFLE.execute(builder, truffle.truffleString());
                }
            }
            if (buildingLength() != 0) {
                if (state <= IS_BUILDING_BYTES) {
                    APPEND_STRING_LATIN1.execute(builder, ((MuleByteArrayString) getBuildingString()).toTruffleString());
                } else {
                    APPEND_STRING_TRUFFLE.execute(builder, ((MuleTruffleString) getBuildingString()).truffleString());
                }
            }
            return new MuleTruffleString(TO_STRING.execute(builder));
        }
        return copy();
    }

    @Override
    public MuleString subSequence(long start, long end) {
        return new MuleStringBuffer().appendMuleString(this, start, end).build();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        strings.forEach(builder::append);
        builder.append(getBuildingString());
        return builder.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof MuleString s)) {
            return false;
        }
        return MuleString.equals(this, s);
    }

    @Override
    public int hashCode() {
        return MuleString.hashCode(this);
    }

    @Override
    public IntStream codePoints(long start) {
        return StreamSupport.intStream(
                () -> Spliterators.spliterator(iterator(start), length() - start, Spliterator.ORDERED),
                Spliterator.SUBSIZED | Spliterator.SIZED | Spliterator.ORDERED,
                false
        );
    }

    public PrimitiveIterator.OfInt iterator(long start) {
        int startStringI = start >= startingCodePointIndices.getLast() ? strings.size() : offsetToStringIndex(start);
        long startOffset = start - startingCodePointIndices.get(startStringI);
        return new PrimitiveIterator.OfInt() {
            int stringI = startStringI;
            long offset = startOffset;

            @Override
            public int nextInt() {
                if (stringI >= strings.size()) {
                    return getBuildingCodePoint(Math.toIntExact(offset++));
                }
                MuleString s = strings.get(stringI);
                int c = s.codePointAt(offset++);
                if (offset >= s.length()) {
                    stringI++;
                    offset = 0;
                }
                return c;
            }

            @Override
            public boolean hasNext() {
                return stringI < strings.size() || offset < buildingLength();
            }
        };
    }
}
