package party.iroiro.juicemacs.elisp.runtime.array;

import java.util.ListIterator;

public sealed interface ConsIterator extends ListIterator<Object> permits BrentTortoiseHareIterator {
    boolean hasNextCons();

    ELispCons currentCons();

    ELispCons nextCons();
}
