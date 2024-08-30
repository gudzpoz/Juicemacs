package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.forms.BuiltInFns;

import java.util.AbstractList;
import java.util.List;

public abstract class ELispVectorLike<T> extends AbstractList<T> implements List<T>, ELispValue {
    public abstract void setUntyped(int i, Object object);

    @Override
    public boolean lispEquals(Object other) {
        if (this.getClass() == other.getClass()) {
            ELispVectorLike<?> list = (ELispVectorLike<?>) other;
            if (list.size() != size()) {
                return false;
            }
            for (int i = 0; i < size(); i++) {
                if (BuiltInFns.FEqual.equal(get(i), list.get(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean equals(Object o) {
        return this == o;
    }
}
