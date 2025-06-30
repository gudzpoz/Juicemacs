package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispBoolVector extends ELispVectorLike<Boolean> {
    private final long[] bits;
    private final int size;

    public ELispBoolVector(long[] bits, int size) {
        this.bits = bits;
        this.size = size;
        trimTrailingBits();
    }

    public ELispBoolVector(ELispBoolVector a) {
        this(a.bits.clone(), a.size);
    }

    public static ELispBoolVector fromBytes(byte[] bits, int size) {
        int last = bits.length - 1;
        if (size < 0 || size <= last * 8 || bits.length * 8 < size) {
            throw ELispSignals.wrongLengthArgument(bits.length * 64L, size);
        }
        long[] words = new long[Math.ceilDiv(size, 64)];
        ByteBuffer bytes = ByteBuffer.wrap(bits).slice().order(ByteOrder.LITTLE_ENDIAN);
        int i = 0;
        while (bytes.remaining() >= 8) {
            words[i++] = bytes.getLong();
        }
        int remaining = bytes.remaining();
        for (int j = 0; j < remaining; j++) {
            words[i] |= (bytes.get() & 0xFFL) << (8 * j);
        }
        return new ELispBoolVector(words, size);
    }

    public void trimTrailingBits() {
        int last = bits.length - 1;
        if (size < 0 || size <= last * 64 || bits.length * 64 < size) {
            throw ELispSignals.wrongLengthArgument(bits.length * 64L, size);
        }
        if (bits.length != 0) {
            int lastBits = size - last * 64;
            bits[last] &= (1L << lastBits) - 1;
        }
    }

    @Override
    public Boolean get(int index) {
        int offset = index & 63;
        index >>= 6;
        return (index < size) && (bits[index] & (1L << offset)) != 0L;
    }

    @Override
    public Boolean set(int index, Boolean element) {
        if (index >= size) {
            return false;
        }
        int offset = index & 63;
        index >>= 6;
        long mask = 1L << offset;
        boolean prev = (bits[index] & mask) != 0L;
        if (element) {
            bits[index] |= mask;
        } else {
            bits[index] &= ~mask;
        }
        return prev;
    }

    public long cardinality() {
        long cardinality = 0;
        for (long word : bits) {
            cardinality += Long.bitCount(word);
        }
        return cardinality;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void setUntyped(int i, Object object) {
        set(i, !isNil(object));
    }

    public ELispBoolVector reverse() {
        ELispBoolVector reversed = new ELispBoolVector(this);
        for (int i = 0; i < size; i++) {
            reversed.set(size - i - 1, this.get(i));
        }
        return reversed;
    }

    public long[] getBits() {
        return bits;
    }

    @Override
    public void display(ELispPrint print) {
        print.print('#').print('&')
                .printInt(size)
                .startString();
        int remaining = Math.ceilDiv(size, 8);
        for (long bit : bits) {
            for (int i = 0; i < 8 && remaining > 0; i++, remaining--) {
                byte b = (byte) (bit & 0xFF);
                print.printRawByte(b);
                bit >>= 8;
            }
        }
        print.endString();
    }

    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispBoolVector vector && size == vector.size && Arrays.equals(bits, vector.bits);
    }
    @Override
    public int lispHashCode(int depth) {
        return size + 31 * Arrays.hashCode(bits);
    }
}
