package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.BitSet;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispBoolVector extends ELispVectorLike<Boolean> {
    private final BitSet bits;
    private final int size;

    public ELispBoolVector(BitSet bits, int size) {
        this.bits = bits;
        this.size = size;
    }

    public ELispBoolVector(ELispBoolVector a) {
        this((BitSet) a.bits.clone(), a.size);
    }

    @Override
    public Boolean get(int index) {
        return index < size && bits.get(index);
    }

    @Override
    public Boolean set(int index, Boolean element) {
        if (index >= size) {
            return false;
        }
        boolean prev = bits.get(index);
        bits.set(index, element);
        return prev;
    }

    public long cardinality() {
        return bits.cardinality();
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
        BitSet bits = new BitSet(size);
        for (int i = 0; i < size; i++) {
            bits.set(size - i - 1, this.bits.get(i));
        }
        return new ELispBoolVector(bits, size);
    }

    @Override
    public void display(ELispPrint print) {
        print.print('#').print('&')
                .printInt(size)
                .startString();
        for (byte b : bits.toByteArray()) {
            print.printRawByte(b);
        }
        print.endString();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("#&").append(size).append("\"");
        for (byte b : bits.toByteArray()) {
            builder.appendCodePoint(b);
        }
        builder.append("\"");
        return builder.toString();
    }

    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispBoolVector vector && bits.equals(vector.bits);
    }
    @Override
    public int lispHashCode() {
        return bits.hashCode();
    }
}
