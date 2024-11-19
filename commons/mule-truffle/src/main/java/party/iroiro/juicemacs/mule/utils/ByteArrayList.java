package party.iroiro.juicemacs.mule.utils;

public final class ByteArrayList {
    private static final byte[] EMPTY_BYTE_ARRAY = new byte[0];

    private byte[] array;
    private int size;

    public ByteArrayList() {
        array = EMPTY_BYTE_ARRAY;
        size = 0;
    }

    public void ensureCapacity(int minCapacity) {
        if (array.length >= minCapacity) {
            return;
        }
        int capacity = array.length;
        while (capacity < minCapacity) {
            capacity += (capacity >> 1) + 8;
        }
        byte[] newArray = new byte[capacity];
        System.arraycopy(array, 0, newArray, 0, size);
        array = newArray;
    }

    public int size() {
        return size;
    }

    public void add(byte value) {
        ensureCapacity(size + 1);
        array[size++] = value;
    }

    public void addAll(byte[] values, int offset, int length) {
        ensureCapacity(size + length);
        System.arraycopy(values, offset, array, size, length);
        size += length;
    }

    public int get(int index) {
        if (index >= size) {
            throw new ArrayIndexOutOfBoundsException(index);
        }
        return array[index];
    }

    public void clear() {
        size = 0;
    }

    public byte[] toArray() {
        if (size == 0) {
            return EMPTY_BYTE_ARRAY;
        }
        byte[] result = new byte[size];
        System.arraycopy(array, 0, result, 0, size);
        return result;
    }

    public byte[] inner() {
        return array;
    }
}
