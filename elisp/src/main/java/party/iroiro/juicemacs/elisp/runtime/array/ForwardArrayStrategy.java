package party.iroiro.juicemacs.elisp.runtime.array;

import java.util.function.Predicate;

final class ForwardArrayStrategy extends ArrayStrategy {
    static ForwardArrayStrategy INSTANCE = new ForwardArrayStrategy();
    private ForwardArrayStrategy() {}

    public ELispCons forwarded(ELispConsArray array, int index) {
        while (true) {
            ForwardInfo forward = (ForwardInfo) array.array;
            if (index < forward.split) {
                array = forward.tail;
            } else {
                array = forward.head;
                index -= forward.split;
            }
            if (array.strategy != this) {
                return new ELispCons(array, index);
            }
        }
    }

    @Override
    public Object car(ELispConsArray object, int index) {
        ELispCons forwarded = forwarded(object, index);
        return WithCdrStrategy.INSTANCE.car(forwarded.array, forwarded.index);
    }

    @Override
    public Object cdr(ELispConsArray object, int index) {
        ELispCons forwarded = forwarded(object, index);
        return WithCdrStrategy.INSTANCE.cdr(forwarded.array, forwarded.index);
    }

    @Override
    public void setCar(ELispConsArray object, int index, Object element) {
        ELispCons forwarded = forwarded(object, index);
        WithCdrStrategy.INSTANCE.setCar(forwarded.array, forwarded.index, element);
    }

    @Override
    public void setCdr(ELispConsArray object, int index, Object element) {
        ELispCons forwarded = forwarded(object, index);
        WithCdrStrategy.INSTANCE.setCdr(forwarded.array, forwarded.index, element);
    }

    @Override
    public ELispCons cons(ELispConsArray array, int index, Object car) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.cons(forwarded.array, forwarded.index, car);
    }

    @Override
    public ELispCons.ConsIterator listIterator(ELispConsArray array, int from, int index) {
        ELispCons forwarded = forwarded(array, from);
        return WithCdrStrategy.INSTANCE.listIterator(forwarded.array, forwarded.index, index);
    }

    @Override
    public int size(ELispConsArray array, int index) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.size(forwarded.array, forwarded.index);
    }

    @Override
    public ELispCons copy(ELispConsArray array, int index) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.copy(forwarded.array, forwarded.index);
    }

    @Override
    public Object filter(ELispConsArray array, int index, Predicate<Object> predicate) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.filter(forwarded.array, forwarded.index, predicate);
    }

    @Override
    public ELispCons nReverse(ELispConsArray array, int index) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.nReverse(forwarded.array, forwarded.index);
    }

    @Override
    public ELispCons reverse(ELispConsArray array, int index) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.reverse(forwarded.array, forwarded.index);
    }

    @Override
    public int hashCode(ELispConsArray array, int index) {
        ELispCons forwarded = forwarded(array, index);
        return WithCdrStrategy.INSTANCE.hashCode(forwarded.array, forwarded.index);
    }

    record ForwardInfo(ELispConsArray head, ELispConsArray tail, int split) {
    }
}
