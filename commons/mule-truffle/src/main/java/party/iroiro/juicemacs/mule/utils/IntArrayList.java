package party.iroiro.juicemacs.mule.utils;

import java.util.Arrays;

/// An append-only list of integers
public final class IntArrayList {
    private static final int[] EMPTY_INT_ARRAY = new int[0];

    private int[] array;
    private int size;

    public IntArrayList() {
        array = EMPTY_INT_ARRAY;
        size = 0;
    }

    public IntArrayList(int capacity) {
        array = new int[capacity];
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
        int[] newArray = new int[capacity];
        System.arraycopy(array, 0, newArray, 0, size);
        array = newArray;
    }

    public int size() {
        return size;
    }

    public void add(int value) {
        ensureCapacity(size + 1);
        array[size++] = value;
    }

    public void addAll(int[] values, int offset, int length) {
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

    public int getLast() {
        return array[size - 1];
    }

    public int binarySearch(int value) {
        return Arrays.binarySearch(array, 0, size, value);
    }

    public void clear() {
        size = 0;
    }

    public int[] toArray() {
        if (size == 0) {
            return EMPTY_INT_ARRAY;
        }
        int[] result = new int[size];
        System.arraycopy(array, 0, result, 0, size);
        return result;
    }
}
