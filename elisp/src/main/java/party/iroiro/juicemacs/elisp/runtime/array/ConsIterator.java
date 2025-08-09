package party.iroiro.juicemacs.elisp.runtime.array;

import java.util.ListIterator;

public interface ConsIterator extends ListIterator<Object> {
    boolean hasNextCons();

    ELispCons currentCons();

    ELispCons nextCons();
}
