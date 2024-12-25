package party.iroiro.juicemacs.mule.utils;

/// An append-only list of bytes
public final class ByteArrayList extends org.eclipse.collections.impl.list.mutable.primitive.ByteArrayList {
    public void addAll(byte[] values, int offset, int length) {
        ensureCapacity(size + length);
        System.arraycopy(values, offset, items, size, length);
        size += length;
    }

    @Override
    public void clear() {
        size = 0;
    }

    public byte[] inner() {
        return items;
    }
}
