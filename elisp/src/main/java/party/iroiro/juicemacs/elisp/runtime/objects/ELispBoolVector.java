package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.BitSet;

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

    @Override
    public int size() {
        return size;
    }

    @Override
    public void setUntyped(int i, Object object) {
        set(i, !ELispSymbol.isNil(object));
    }

    public ELispBoolVector reverse() {
        BitSet bits = new BitSet(size);
        for (int i = 0; i < size; i++) {
            bits.set(size - i - 1, this.bits.get(i));
        }
        return new ELispBoolVector(bits, size);
    }
}
