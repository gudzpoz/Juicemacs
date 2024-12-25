package party.iroiro.juicemacs.mule.utils;

/// An append-only list of integers
public final class IntArrayList extends org.eclipse.collections.impl.list.mutable.primitive.IntArrayList {
    public void addAll(int[] values, int offset, int length) {
        ensureCapacity(size + length);
        System.arraycopy(values, offset, items, size, length);
        size += length;
    }

    @Override
    public void clear() {
        size = 0;
    }
}
