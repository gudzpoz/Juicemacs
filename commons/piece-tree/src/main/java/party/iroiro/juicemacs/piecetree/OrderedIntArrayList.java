package party.iroiro.juicemacs.piecetree;

import org.eclipse.jdt.annotation.Nullable;

import java.util.Arrays;
import java.util.Objects;

public sealed interface OrderedIntArrayList {
    int get(int index);
    default void set(int index, int value) {
        throw new UnsupportedOperationException();
    }
    default void pop() {
        throw new UnsupportedOperationException();
    }
    default void addAll(OrderedIntArrayList other, int start) {
        throw new UnsupportedOperationException();
    }
    int size();

    default int getLast() {
        return get(size() - 1);
    }

    static OrderedIntArrayList ofConstant(LazyArrayList list) {
        if (list.lazyBuffer == null) {
            int[] buffer = Arrays.copyOf(list.buffer, list.size);
            return new OfIntConstant(buffer);
        } else {
            short[] buffer = Arrays.copyOf(list.lazyBuffer, list.size);
            return new OfShortConstant(buffer);
        }
    }

    static LazyArrayList ofLazy(int capacity, int max) {
        return new LazyArrayList(capacity, max > LazyArrayList.MAX_SHORT);
    }

    record OfShortConstant(short[] buffer) implements OrderedIntArrayList {
        @Override
        public int get(int index) {
            return Short.toUnsignedInt(buffer[index]);
        }

        @Override
        public int size() {
            return buffer.length;
        }
    }

    record OfIntConstant(int[] buffer) implements OrderedIntArrayList {
        @Override
        public int get(int index) {
            return buffer[index];
        }

        @Override
        public int size() {
            return buffer.length;
        }
    }

    final class LazyArrayList implements OrderedIntArrayList {
        private static final int MAX_SHORT = (1 << 16) - 1;
        private int size;
        private short @Nullable [] lazyBuffer;
        private int[] buffer;

        public LazyArrayList(int capacity, boolean forceInt) {
            capacity = Math.max(8, capacity);
            if (forceInt) {
                buffer = new int[capacity];
                lazyBuffer = null;
            } else {
                lazyBuffer = new short[capacity];
                buffer = new int[0];
            }
            size = 0;
        }

        @Override
        public int get(int index) {
            return lazyBuffer == null ? buffer[index] : Short.toUnsignedInt(lazyBuffer[index]);
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        public void pop() {
            if (size > 0) {
                size--;
            }
        }

        @Override
        public void set(int index, int value) {
            if (index >= size) {
                size = index + 1;
            }
            if (lazyBuffer == null) {
                checkExpandInt(index);
                buffer[index] = value;
            } else {
                if (value > MAX_SHORT) {
                    toInt();
                    set(index, value);
                } else {
                    checkExpandShort(index);
                    lazyBuffer[index] = (short) value;
                }
            }
        }

        private void toInt() {
            if (lazyBuffer != null) {
                buffer = new int[lazyBuffer.length];
                for (int i = 0; i < lazyBuffer.length; i++) {
                    buffer[i] = Short.toUnsignedInt(lazyBuffer[i]);
                }
                lazyBuffer = null;
            }
        }

        @Override
        public void addAll(OrderedIntArrayList other, int start) {
            int extra = other.size();
            if (extra <= start) {
                return;
            }
            switch (other) {
                case OfIntConstant(int[] integers) -> {
                    toInt();
                    checkExpandInt(size + integers.length - 1 - start);
                    System.arraycopy(integers, start, buffer, size, integers.length - start);
                }
                case OfShortConstant(short[] shorts) -> {
                    if (lazyBuffer == null) {
                        checkExpandInt(size + shorts.length - 1 - start);
                        for (int i = start; i < shorts.length; i++) {
                            buffer[size + i] = Short.toUnsignedInt(shorts[i]);
                        }
                    } else {
                        checkExpandShort(size + shorts.length - 1 - start);
                        System.arraycopy(shorts, start, lazyBuffer, size, shorts.length - start);
                    }
                }
                case LazyArrayList list -> {
                    if (lazyBuffer == null && list.lazyBuffer == null) {
                        checkExpandInt(size + list.size - 1 - start);
                        System.arraycopy(list.buffer, start, buffer, size, list.size - start);
                    } else if (lazyBuffer != null && list.lazyBuffer != null) {
                        checkExpandShort(size + list.size - 1 - start);
                        System.arraycopy(list.lazyBuffer, start, lazyBuffer, size, list.size - start);
                    } else {
                        toInt();
                        list.toInt();
                        checkExpandInt(size + list.size - 1 - start);
                        System.arraycopy(list.buffer, start, buffer, size, list.size - start);
                    }
                }
            }
            size += extra - start;
        }

        private int getNewSize(int index) {
            int newSize = Math.max(size, 8);
            do {
                newSize += newSize >> 1;
            } while (newSize <= index);
            return newSize;
        }

        private void checkExpandShort(int index) {
            if (index >= Objects.requireNonNull(lazyBuffer).length) {
                lazyBuffer = Arrays.copyOf(lazyBuffer, getNewSize(index));
            }
        }

        private void checkExpandInt(int index) {
            if (index >= buffer.length) {
                buffer = Arrays.copyOf(buffer, getNewSize(index));
            }
        }
    }
}
