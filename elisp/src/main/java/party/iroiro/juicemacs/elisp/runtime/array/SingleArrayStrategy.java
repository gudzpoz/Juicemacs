package party.iroiro.juicemacs.elisp.runtime.array;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jdt.annotation.Nullable;

import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.function.Predicate;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

sealed class SingleArrayStrategy extends ArrayStrategy permits WithCdrStrategy {
    static final SingleArrayStrategy INSTANCE = new SingleArrayStrategy();
    protected SingleArrayStrategy() {}

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
            return false;
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
        ArrayStrategy strategy = isNil(element) ? SingleArrayStrategy.INSTANCE : WithCdrStrategy.INSTANCE;
        if (index == 0) {
            object.cdr = element;
            object.strategy = strategy;
            return;
        }
        ELispConsArray leading = deoptToForwardStrategy(object, index);
        leading.cdr = element;
        leading.strategy = strategy;
    }

    private void copyHashCode(ELispConsArray from, ELispConsArray to, int offset) {
        if (from.metadata != null) {
            to.metadata = from.metadata.clone();
            to.metadata[to.metadata.length - 1] += offset;
        }
    }

    protected ELispConsArray deoptToForwardStrategy(ELispConsArray object, int split) {
        Object[] elements = getArray(object);
        ArrayStrategy strategy = isNil(object.cdr) ? SingleArrayStrategy.INSTANCE : WithCdrStrategy.INSTANCE;
        ELispConsArray restCons = new ELispConsArray(elements, split, strategy);
        copyHashCode(object, restCons, 0);
        restCons.cdr = object.cdr;

        Object[] leading = Arrays.copyOfRange(elements, split, object.size);
        Arrays.fill(elements, split, object.size, null);
        ELispConsArray leadingCons = new ELispConsArray(leading, leading.length, WithCdrStrategy.INSTANCE);
        copyHashCode(object, leadingCons, split);
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
        ELispCons tail = asCons(deoptToForwardStrategy(array, index + 1).cdr);
        assert index == tail.array.size - 1;
        addLast(tail.array, car);
        return new ELispCons(tail.array, index + 1);
    }

    private void addLast(ELispConsArray array, Object car) {
        Object[] elements = getArray(array);
        int target = array.size++;
        int current = elements.length;
        if (current > target) {
            elements[target] = car;
        } else {
            Object[] extended = new Object[target < 8 ? target + 1 : current + (current >> 1)];
            System.arraycopy(elements, 0, extended, 0, target);
            extended[target] = car;
            array.array = extended;
        }
    }

    protected int filterStep(ELispConsArray object, int index, Predicate<Object> predicate) {
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
        return new WithCdrStrategy.ConsArrayIterator(array, from, index);
    }

    @Override
    public int size(ELispConsArray array, int index) {
        return index + 1;
    }

    @Override
    public Object filter(ELispConsArray array, int index, Predicate<Object> predicate) {
        int newIndex = filterStep(array, index, predicate);
        return newIndex < 0 ? false : new ELispCons(array, newIndex);
    }

    @Override
    public ELispCons reverse(ELispConsArray array, int index) {
        Object[] elements = getArray(array);
        Object[] reversed = new Object[index + 1];
        System.arraycopy(elements, 0, reversed, 0, index + 1);
        return (ELispCons) ELispCons.listOf(reversed);
    }

    @Override
    public ELispCons nReverse(ELispConsArray array, int index) {
        if (index == 0) {
            return new ELispCons(array, index);
        }
        return reverse(array, index);
        /*
         * So ideally we can implement nreverse with the following,
         * saving us an extra array allocation:

        Object[] elements = getArray(array);
        ArrayUtils.reverse(elements, 0, index + 1);
        return new ELispCons(array, index);

         * Except that this breaks Emacs, because GNU Emacs *is* the language specification
         * and although the manual says it "may destructively modify SEQ
         * to produce the value", `cl-copy-list` depends on its *not* modifying
         * the CAR of the cons.
         *
         * What can I say? This is the Lispy way with every implementation detail exposed.
         */
    }

    public ELispCons create(Object... elements) {
        ELispConsArray array = new ELispConsArray(elements, elements.length, SingleArrayStrategy.INSTANCE);
        return new ELispCons(array, elements.length - 1);
    }
    public ELispCons createWithCdr(Object[] elements, Object cdr) {
        if (isNil(cdr)) {
            return create(elements);
        }
        ELispConsArray array = new ELispConsArray(elements, elements.length, WithCdrStrategy.INSTANCE);
        array.cdr = cdr;
        return new ELispCons(array, elements.length - 1);
    }
}
