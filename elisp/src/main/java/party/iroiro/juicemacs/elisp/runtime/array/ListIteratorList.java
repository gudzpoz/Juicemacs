package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;

import java.util.*;

public interface ListIteratorList extends List<Object> {
    @Override
    default int size() {
        int count = 0;
        for (Object _ : this) {
            count++;
        }
        return count;
    }

    @Override
    default boolean isEmpty() {
        return !listIterator().hasNext();
    }

    @Override
    default boolean contains(Object o) {
        for (Object object : this) {
            if (BuiltInData.FEq.eq(o, object)) {
                return true;
            }
        }
        return false;
    }

    @Override
    default ConsIterator iterator() {
        return listIterator();
    }

    @Override
    ConsIterator listIterator(int index);

    @Override
    default Object[] toArray() {
        Object[] elements = new Object[size()];
        int i = 0;
        for (Object o : this) {
            elements[i++] = o;
        }
        return elements;
    }

    @Override
    default <T> T[] toArray(T[] a) {
        throw new UnsupportedOperationException();
    }

    @Override
    default boolean add(Object object) {
        throw new UnsupportedOperationException();
    }

    @Override
    default boolean remove(Object o) {
        ListIterator<Object> i = listIterator();
        while (i.hasNext()) {
            if (BuiltInData.FEq.eq(o, i.next())) {
                i.remove();
                return true;
            }
        }
        return false;
    }

    @Override
    default boolean containsAll(Collection<?> c) {
        for (Object e : c) {
            if (!contains(e)) {
                return false;
            }
        }
        return true;
    }

    @Override
    default boolean addAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    default boolean addAll(int index, Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    default boolean removeAll(Collection<?> c) {
        boolean modified = false;
        for (Object o : c) {
            modified = remove(o);
        }
        return modified;
    }

    @Override
    @TruffleBoundary
    default boolean retainAll(Collection<?> c) {
        boolean modified = false;
        ConsIterator i = listIterator();
        while (i.hasNext()) {
            if (!c.contains(i.next())) {
                i.remove();
                modified = true;
            }
        }
        return modified;
    }

    @Override
    default void clear() {
        ListIterator<Object> i = listIterator();
        while (i.hasNext()) {
            i.next();
            i.remove();
        }
    }

    @Override
    default Object get(int index) {
        return listIterator(index).next();
    }

    @Override
    default Object set(int index, Object element) {
        ListIterator<Object> i = listIterator(index);
        Object value = i.next();
        i.set(element);
        return value;
    }

    @Override
    default void add(int index, Object element) {
        throw new UnsupportedOperationException();
    }

    @Override
    default Object remove(int index) {
        ListIterator<Object> i = listIterator(index);
        Object value = i.next();
        i.remove();
        return value;
    }

    @Override
    default int indexOf(Object o) {
        ListIterator<Object> i = listIterator();
        while (i.hasNext()) {
            if (BuiltInData.FEq.eq(o, i.next())) {
                return i.previousIndex();
            }
        }
        return -1;
    }

    @Override
    default int lastIndexOf(Object o) {
        throw new UnsupportedOperationException();
    }

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    @Override
    default ConsIterator listIterator() {
        return listIterator(0);
    }

    @Override
    default List<Object> subList(int fromIndex, int toIndex) {
        throw new UnsupportedOperationException();
    }
}
