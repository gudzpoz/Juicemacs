package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.AbstractList;
import java.util.List;

public abstract class ELispVectorLike<T> extends AbstractList<T> implements List<T>, ELispValue {
    public abstract void setUntyped(int i, Object object);
}
