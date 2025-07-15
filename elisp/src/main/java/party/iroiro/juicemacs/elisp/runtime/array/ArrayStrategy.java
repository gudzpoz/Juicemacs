package party.iroiro.juicemacs.elisp.runtime.array;

import java.util.function.Predicate;

sealed abstract class ArrayStrategy permits ForwardArrayStrategy, ObjectArrayStrategy {

    public abstract Object car(ELispConsArray object, int index);
    public abstract Object cdr(ELispConsArray object, int index);
    public abstract void setCar(ELispConsArray object, int index, Object element);
    public abstract void setCdr(ELispConsArray object, int index, Object element);
    public abstract ELispCons cons(ELispConsArray array, int index, Object car);

    public abstract ELispCons.ConsIterator listIterator(ELispConsArray array, int from, int index);
    public abstract int size(ELispConsArray array, int index);

    public abstract Object filter(ELispConsArray array, int index, Predicate<Object> predicate);
    public abstract ELispCons reverse(ELispConsArray array, int index);
    public abstract ELispCons nReverse(ELispConsArray array, int index);

    public abstract int hashCode(ELispConsArray array, int index);
}
