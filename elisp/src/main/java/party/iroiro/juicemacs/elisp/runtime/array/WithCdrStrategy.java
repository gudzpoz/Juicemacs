package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives;
import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.NoSuchElementException;
import java.util.function.Predicate;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LISTP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

final class WithCdrStrategy extends SingleArrayStrategy {
    static WithCdrStrategy INSTANCE = new WithCdrStrategy();
    private WithCdrStrategy() {}

    @Override
    public Object cdr(ELispConsArray object, int index) {
        if (index == 0) {
            return object.cdr;
        }
        return new ELispCons(object, index - 1);
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
