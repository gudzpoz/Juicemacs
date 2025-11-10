package party.iroiro.juicemacs.mule;

import java.util.Arrays;

public abstract class ByteArrayBuilder {
    private static final int SOFT_MAX_ARRAY_LENGTH = Integer.MAX_VALUE >> 1;

    protected byte[] bytes;
    protected int usedBytes;

    protected ByteArrayBuilder() {
        this(32);
    }

    protected ByteArrayBuilder(int initialCapacity) {
        this(new byte[initialCapacity], 0);
    }

    protected ByteArrayBuilder(byte[] init, int usedBytes) {
        this.bytes = init;
        this.usedBytes = usedBytes;
    }

    protected void ensureCapacity(int extra) {
        int needed = usedBytes + extra;
        if (needed < 0) { // might overflow
            throw new OutOfMemoryError();
        }
        if (needed > bytes.length) {
            int newLength = Math.max(bytes.length << 1, needed);
            if (newLength > SOFT_MAX_ARRAY_LENGTH) {
                checkOutOfMemory(needed);
                newLength = SOFT_MAX_ARRAY_LENGTH;
            }
            byte[] original = bytes;
            bytes = new byte[newLength];
            System.arraycopy(original, 0, bytes, 0, usedBytes);
        }
    }

    private void checkOutOfMemory(int needed) {
        if (needed > SOFT_MAX_ARRAY_LENGTH) {
            throw new OutOfMemoryError();
        }
    }

    protected void writeByte(byte b) {
        ensureCapacity(1);
        bytes[usedBytes++] = b;
    }

    protected void writeBytes(byte[] b, int off, int len) {
        ensureCapacity(len);
        System.arraycopy(b, off, bytes, usedBytes, len);
        usedBytes += len;
    }

    public byte[] toByteArray() {
        return Arrays.copyOf(bytes, usedBytes);
    }

    public static void checkRange(byte[] array, int from, int to) {
        if (from < 0 || to > array.length || from > to) {
            throw new IllegalArgumentException();
        }
    }
}
