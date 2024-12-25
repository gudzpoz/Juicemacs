package party.iroiro.juicemacs.piecetree;

import org.eclipse.collections.api.list.primitive.LongList;
import org.eclipse.collections.impl.list.mutable.primitive.LongArrayList;

public sealed interface LineStartList {
    long get(int index);
    int size();

    default long getLast() {
        return get(size() - 1);
    }

    default long set(int index, long value) {
        throw new UnsupportedOperationException();
    }

    default void addAll(LineStartList other, int start) {
        throw new UnsupportedOperationException();
    }

    default void pop() {
        throw new UnsupportedOperationException();
    }

    final class ImmutableIntList implements LineStartList {
        private final int[] items;

        private ImmutableIntList(LongList list) {
            items = new int[list.size()];
            for (int i = 0; i < list.size(); i++) {
                items[i] = (int) list.get(i);
            }
        }

        @Override
        public long get(int index) {
            return items[index];
        }

        @Override
        public int size() {
            return items.length;
        }
    }

    final class LongLineStarts extends LongArrayList implements LineStartList {
        @Override
        public void pop() {
            removeAtIndex(size - 1);
        }

        @Override
        public void addAll(LineStartList other, int start) {
            long[] inner = ((LongLineStarts) other).items;
            ensureCapacity(size + other.size() - start);
            System.arraycopy(inner, start, items, size, other.size() - start);
            size += other.size() - start;
        }
    }

    static LineStartList create(LongLineStarts r, boolean readonly) {
        if (!readonly || r.getLast() > Integer.MAX_VALUE) {
            return r;
        }
        return new ImmutableIntList(r);
    }
}
