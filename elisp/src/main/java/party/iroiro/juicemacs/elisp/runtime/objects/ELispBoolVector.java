package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.BitSet;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

public final class ELispBoolVector extends ELispVectorLike<Boolean> {
    private final BitSet bits;
    private final int size;

    public ELispBoolVector(BitSet bits, int size) {
        this.bits = bits;
        this.size = size;
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
        set(i, object != Boolean.FALSE && object != NIL);
    }

    @Override
    public String type() {
        return "bool-vector";
    }
}
