package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringBuilder;
import com.oracle.truffle.api.strings.TruffleStringBuilderUTF32;
import party.iroiro.juicemacs.mule.utils.ByteArrayList;
import party.iroiro.juicemacs.mule.utils.IntArrayList;

import java.util.ArrayList;
import java.util.PrimitiveIterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

import static com.oracle.truffle.api.strings.TruffleString.Encoding.UTF_32;

public final class MuleStringBuffer implements MuleString {
    private static final TruffleString.FromIntArrayUTF32Node FROM_INT_ARRAY_UTF_32 =
            TruffleString.FromIntArrayUTF32Node.create();
    private static final TruffleStringBuilder.AppendStringNode APPEND_STRING_LATIN1 =
            TruffleStringBuilder.AppendStringNode.create();
    private static final TruffleStringBuilder.AppendStringNode APPEND_STRING_TRUFFLE =
            TruffleStringBuilder.AppendStringNode.create();
    private static final TruffleStringBuilder.ToStringNode TO_STRING = TruffleStringBuilder.ToStringNode.create();

    private static final int MAX_COPY_LIMIT = 4096;

    private final IntArrayList startingCodePointIndices;
    private final ArrayList<MuleString> strings;

    /// State of the building array list
    /// @see #BUILDING_BYTES
    /// @see #BUILDING_UNICODE
    /// @see #BUILDING_MULE
    private int state;
    /// State `0`: byte string, chars stored in [#buildingBytes]
    /// @see #state
    private static final int BUILDING_BYTES = 0;
    /// State `1`: Unicode with code points outside Latin1, stored in [#buildingCodePoints]
    /// @see #state
    private static final int BUILDING_UNICODE = 1;
    /// State `2`: Like `1`, but with non-Unicode code points over [Character#MAX_CODE_POINT]
    /// @see #state
    private static final int BUILDING_MULE = 2;
    /// Not a valid state but a flag
    ///
    /// Used in [#appendMuleString(MuleString, int, int)] when trying to copy over [#MAX_COPY_LIMIT]
    private static final int BUILDING_COMMIT_NOW = 1 << 16;
    private final ByteArrayList buildingBytes;
    private final IntArrayList buildingCodePoints;

    public MuleStringBuffer() {
        startingCodePointIndices = new IntArrayList();
        startingCodePointIndices.add(0);
        strings = new ArrayList<>();
        buildingBytes = new ByteArrayList();
        buildingCodePoints = new IntArrayList();
        reset();
    }

    private void reset() {
        state = BUILDING_BYTES;
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
        return state == BUILDING_BYTES ? buildingBytes.size() : buildingCodePoints.size();
    }

    @Override
    public int length() {
        return startingCodePointIndices.getLast() + buildingLength();
    }

    private int getBuildingCodePoint(int relIndex) {
        return state == BUILDING_BYTES ? buildingBytes.get(relIndex) : buildingCodePoints.get(relIndex);
    }

    private int offsetToStringIndex(int index) {
        if (index < 0) {
            throw new IndexOutOfBoundsException("Index out of bounds");
        } else {
            int last = startingCodePointIndices.getLast();
            if (index >= last) {
                int buildingIndex = index - last;
                if (buildingIndex >= buildingLength()) {
                    throw new IndexOutOfBoundsException("Index out of bounds");
                }
                return -1;
            }
        }
        int stringIndex = startingCodePointIndices.binarySearch(index);
        if (stringIndex < 0) {
            stringIndex = -stringIndex - 2;
        }
        return stringIndex;
    }

    @Override
    public int codePointAt(int index) {
        int stringIndex = offsetToStringIndex(index);
        if (stringIndex == -1) {
            return getBuildingCodePoint(index - startingCodePointIndices.getLast());
        }
        int offset = startingCodePointIndices.get(stringIndex);
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

    private MuleString getBuildingString() {
        return switch (state) {
            case BUILDING_BYTES -> new MuleByteArrayString(buildingBytes.toArray());
            case BUILDING_UNICODE -> new MuleTruffleString(FROM_INT_ARRAY_UTF_32.execute(buildingCodePoints.toArray()));
            default -> new MuleIntArrayString(buildingCodePoints.toArray());
        };
    }

    public MuleStringBuffer appendCodePoint(int codePoint) {
        if (state == BUILDING_BYTES) {
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

    public MuleStringBuffer append(MuleString string, int start, int end) {
        return appendMuleString(string, start, end);
    }

    public MuleStringBuffer appendMuleString(MuleString string, int start, int end) {
        int addLength = end - start;
        if (addLength <= 0) {
            return this;
        }
        if (addLength > MAX_COPY_LIMIT && !(string instanceof MuleStringBuffer)) {
            state |= BUILDING_COMMIT_NOW;
        }
        switch (string) {
            case MuleByteArrayString bytes when state == BUILDING_BYTES ->
                    buildingBytes.addAll(bytes.bytes(), start, addLength);
            case MuleTruffleString tString when state == BUILDING_UNICODE -> {
                PrimitiveIterator.OfInt codePoints = tString.codePoints(start).iterator();
                for (int i = 0; i < addLength; i++) {
                    buildingCodePoints.add(codePoints.nextInt());
                }
            }
            case MuleIntArrayString intArray when state == BUILDING_MULE ->
                    buildingCodePoints.addAll(intArray.intArray(), start, addLength);
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

    private void appendBuffer(MuleStringBuffer buffer, int start, int end) {
        final int startI = buffer.offsetToStringIndex(start);
        final int endI = end == buffer.length() ? -1 : buffer.offsetToStringIndex(end);

        // Supplied buffer can be `this`. So we need to pre-store the states.
        int last = buffer.startingCodePointIndices.getLast();
        int bufferState = buffer.state;
        byte[] trailingBytes = null;
        int[] trailingCodePoints = null;
        if (endI == -1 && end != last) {
            if (bufferState == BUILDING_BYTES) {
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
                int substringStart;
                if (currentI == startI) {
                    substringStart = start - buffer.startingCodePointIndices.get(currentI);
                } else {
                    substringStart = 0;
                }
                int substringEnd;
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
            int substringStart = last > start ? 0 : start - last;
            int substringEnd = end - last;
            if (trailingBytes != null) {
                buildingBytes.addAll(trailingBytes, substringStart, substringEnd);
            }
            if (trailingCodePoints != null) {
                buildingCodePoints.addAll(trailingCodePoints, substringStart, substringEnd);
            }
            state = bufferState;
        }
    }

    public MuleString build() {
        if (strings.isEmpty()) {
            return getBuildingString();
        } else if (strings.size() == 1 && buildingLength() == 0) {
            return strings.getFirst();
        }
        int overallState = state;
        int length = 0;
        for (MuleString string : strings) {
            overallState |= switch (string) {
                case MuleByteArrayString _ -> BUILDING_BYTES;
                case MuleIntArrayString _ -> BUILDING_MULE;
                case MuleTruffleString _ -> BUILDING_UNICODE;
                case MuleStringBuffer _ -> throw CompilerDirectives.shouldNotReachHere();
            };
            length += string.length();
        }
        length += buildingLength();
        if (overallState == BUILDING_BYTES) {
            byte[] bytes = new byte[length];
            int start = 0;
            for (MuleString string : strings) {
                MuleByteArrayString latin1 = (MuleByteArrayString) string;
                System.arraycopy(latin1.bytes(), 0, bytes, start, latin1.length());
                start += latin1.length();
            }
            System.arraycopy(buildingBytes.inner(), 0, bytes, start, buildingBytes.size());
            return new MuleByteArrayString(bytes);
        } else if (overallState == BUILDING_UNICODE) {
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
                if (state == BUILDING_BYTES) {
                    APPEND_STRING_LATIN1.execute(builder, ((MuleByteArrayString) getBuildingString()).toTruffleString());
                } else {
                    APPEND_STRING_TRUFFLE.execute(builder, ((MuleTruffleString) getBuildingString()).truffleString());
                }
            }
            return new MuleTruffleString(TO_STRING.execute(builder));
        }
        return new MuleStringBuffer().appendMuleString(this, 0, length());
    }

    @Override
    public MuleString subSequence(int start, int end) {
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
    public IntStream codePoints(int start) {
        return StreamSupport.intStream(
                () -> Spliterators.spliterator(iterator(start), length() - start, Spliterator.ORDERED),
                Spliterator.SUBSIZED | Spliterator.SIZED | Spliterator.ORDERED,
                false
        );
    }

    public PrimitiveIterator.OfInt iterator(int start) {
        int startStringI = start >= startingCodePointIndices.getLast() ? strings.size() : offsetToStringIndex(start);
        int startOffset = start - startingCodePointIndices.get(startStringI);
        return new PrimitiveIterator.OfInt() {
            int stringI = startStringI;
            int offset = startOffset;

            @Override
            public int nextInt() {
                if (stringI >= strings.size()) {
                    return getBuildingCodePoint(offset++);
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
