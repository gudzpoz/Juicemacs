package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives;
import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.function.Predicate;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LISTP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

final class ObjectArrayStrategy extends ArrayStrategy {
    static ObjectArrayStrategy INSTANCE = new ObjectArrayStrategy();
    private ObjectArrayStrategy() {}

    public Object[] getArray(ELispConsArray object) {
        return (Object[]) object.array;
    }

    private void checkIndex(ELispConsArray object, int index) {
        if (index < 0 || index >= object.size) {
            throw new NoSuchElementException();
        }
    }

    @Override
    public Object car(ELispConsArray object, int index) {
        checkIndex(object, index);
        return getArray(object)[index];
    }

    @Override
    public Object cdr(ELispConsArray object, int index) {
        if (index == 0) {
            return object.cdr;
        }
        return new ELispCons(object, index - 1);
    }

    @Override
    public void setCar(ELispConsArray object, int index, Object element) {
        checkIndex(object, index);
        getArray(object)[index] = element;
    }

    @Override
    public void setCdr(ELispConsArray object, int index, Object element) {
        if (index == 0) {
            object.cdr = element;
            return;
        }
        ELispConsArray leading = deoptToForwardStrategy(object, index);
        leading.cdr = element;
    }

    private void copyHashCode(ELispConsArray from, ELispConsArray to, int offset) {
        if (from.metadata != null) {
            to.metadata = from.metadata.clone();
            to.metadata[to.metadata.length - 1] += offset;
        }
    }

    private ELispConsArray deoptToForwardStrategy(ELispConsArray object, int split) {
        Object[] elements = getArray(object);
        ELispConsArray restCons = new ELispConsArray(elements, split, this);
        copyHashCode(object, restCons, 0);
        restCons.cdr = object.cdr;

        Object[] leading = Arrays.copyOfRange(elements, split, object.size);
        Arrays.fill(elements, split, object.size, null);
        ELispConsArray leadingCons = new ELispConsArray(leading, leading.length, ObjectArrayStrategy.INSTANCE);
        copyHashCode(object, restCons, split);
        leadingCons.cdr = new ELispCons(restCons, split - 1);

        object.strategy = ForwardArrayStrategy.INSTANCE;
        object.array = new ForwardArrayStrategy.ForwardInfo(leadingCons, restCons, split);

        return leadingCons;
    }

    @Override
    public ELispCons cons(ELispConsArray array, int index, Object car) {
        if (index == array.size - 1) {
            addLast(array, car);
            return new ELispCons(array, index + 1);
        }
        // We always deopt (split the array in halves)
        // so that any dangling heads are garbage collected.
        ELispConsArray head = deoptToForwardStrategy(array, index + 1);
        return new ELispCons(car, head.cdr);
    }

    private void addLast(ELispConsArray array, Object car) {
        Object[] elements = getArray(array);
        int target = array.size++;
        int current = elements.length;
        if (current > target) {
            elements[target] = car;
        } else {
            Object[] extended = new Object[target < 8 ? target + 1 : current + (current >> 1)];
            System.arraycopy(elements, 0, extended, 0, elements.length);
            extended[target] = car;
            array.array = extended;
        }
    }

    @Override
    public int size(ELispConsArray array, int index) {
        int size = index + 1;
        while (!isNil(array.cdr)) {
            ELispCons next = asCons(array.cdr).forwarded();
            if (next != array.cdr) {
                array.cdr = next;
            }
            size += next.index + 1;
            array = next.array;
        }
        return size;
    }

    @CompilerDirectives.TruffleBoundary
    @Override
    public Object filter(ELispConsArray object, int index, Predicate<Object> predicate) {
        @Nullable ELispCons result = null;

        @Nullable ELispConsArray previous = null;
        ELispConsArray current = object;
        while (true) {
            int newIndex = filterStep(current, index, predicate);
            updateCdr(current);
            if (newIndex < 0) {
                if (previous != null) {
                    previous.cdr = current.cdr;
                }
            } else {
                ELispCons segment = new ELispCons(current, newIndex);
                if (result == null) {
                    result = segment;
                }
                if (previous != null) {
                    previous.cdr = segment;
                }
                previous = current;
            }
            if (current.cdr instanceof ELispCons next) {
                current = next.array;
                index = next.index;
            } else {
                if (isNil(current.cdr)) {
                    break;
                }
                throw ELispSignals.wrongTypeArgument(LISTP, current.cdr);
            }
        }
        return result == null ? false : result;
    }

    private void updateCdr(ELispConsArray object) {
        if (object.cdr instanceof ELispCons cons && cons.array.strategy instanceof ForwardArrayStrategy forward) {
            object.cdr = forward.forwarded(cons.array, cons.index);
        }
    }

    private int filterStep(ELispConsArray object, int index, Predicate<Object> predicate) {
        Object[] array = getArray(object);
        int reader = 0;
        while (reader <= index && predicate.test(array[reader])) {
            reader++;
        }
        int writer = reader++;
        if (reader <= index) {
            while (reader <= index) {
                Object current = array[reader++];
                if (predicate.test(current)) {
                    array[writer++] = current;
                }
            }
            Arrays.fill(array, writer, index, false);
        }
        if (index == object.size - 1) {
            object.size = writer;
        }
        // new index
        return writer - 1;
    }

    @CompilerDirectives.TruffleBoundary
    @Override
    public ELispCons nReverse(ELispConsArray array, int index) {
        if (isNil(array.cdr)) {
            ArrayUtils.reverse(getArray(array), 0, index + 1);
            return new ELispCons(array, index);
        }
        return reverse(array, index);
    }

    @CompilerDirectives.TruffleBoundary
    @Override
    public ELispCons reverse(ELispConsArray array, int index) {
        Object[] reversed = new Object[size(array, index)];
        int end = reversed.length - index - 1;
        System.arraycopy(getArray(array), 0, reversed, end, index + 1);
        while (!isNil(array.cdr)) {
            // this.size(...) already normalizes all forwarded cdr pointers
            ELispCons next = asCons(array.cdr);
            end -= next.index + 1;
            System.arraycopy(getArray(next.array), 0, reversed, end, next.index + 1);
            array = next.array;
        }
        return (ELispCons) ELispCons.listOf(reversed);
    }

    @Override
    public int hashCode(ELispConsArray array, int index) {
        int @Nullable [] metadata = array.metadata;
        int offset = 1;
        if (metadata != null) {
            if ((metadata[0] & ELispConsArray.METADATA_HAS_SOURCE_LOCATION) != 0) {
                offset = 5;
            }
            if (offset < metadata.length) {
                return metadata[offset] + index;
            }
        }
        int newHash = System.identityHashCode(array);
        array.metadata = ArrayUtils.add(metadata == null ? new int[1] : metadata, newHash);
        return newHash;
    }

    @Override
    public ELispCons.ConsIterator listIterator(ELispConsArray array, int from, int index) {
        return new ConsArrayIterator(array, from, index);
    }

    public ELispCons create(Object... elements) {
        ELispConsArray array = new ELispConsArray(elements, elements.length, this);
        return new ELispCons(array, elements.length - 1);
    }

    static final class ConsArrayIterator implements ELispCons.ConsIterator {
        ELispConsArray array;
        int totalIndex;
        int index;

        ConsArrayIterator(ELispConsArray array, int from, int index) {
            this.array = array;
            this.index = from;
            while (index > 0 && this.index >= 0) {
                if (index > this.index) {
                    index -= this.index + 1;
                    this.index = -1;
                    seekNextCons();
                } else {
                    this.index -= index;
                    index = 0;
                }
            }
            this.totalIndex = 0;
        }

        private void updateStrategy() {
            if (array.strategy instanceof ForwardArrayStrategy forward) {
                ELispCons forwarded = forward.forwarded(array, index);
                array = forwarded.array;
                index = forwarded.index;
            }
        }

        private void seekNextCons() {
            if (index < 0) {
                if (array.cdr instanceof ELispCons next) {
                    if (next.array.strategy instanceof ForwardArrayStrategy forward) {
                        array.cdr = next = forward.forwarded(next.array, next.index);
                    }
                    array = next.array;
                    index = next.index;
                }
            }
        }

        @Override
        public boolean hasNextCons() {
            return index >= 0;
        }

        @Override
        public ELispCons currentCons() {
            return new ELispCons(array, index);
        }

        @Override
        public ELispCons nextCons() {
            updateStrategy();
            if (!hasNextCons()) {
                throw new NoSuchElementException();
            }
            totalIndex++;
            ELispCons cons = new ELispCons(array, index--);
            seekNextCons();
            return cons;
        }

        @Override
        public boolean hasNext() {
            return index >= 0;
        }

        @Override
        public Object next() {
            updateStrategy();
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            totalIndex++;
            Object car = array.strategy.car(array, index--);
            seekNextCons();
            return car;
        }

        @Override
        public boolean hasPrevious() {
            return false;
        }

        @Override
        public Object previous() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int nextIndex() {
            return totalIndex - 1;
        }

        @Override
        public int previousIndex() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void set(Object object) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void add(Object object) {
            throw new UnsupportedOperationException();
        }
    }
}
