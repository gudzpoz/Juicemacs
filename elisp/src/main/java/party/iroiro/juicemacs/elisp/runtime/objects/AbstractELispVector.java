package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.List;

public abstract class AbstractELispVector extends ELispVectorLike<Object> {
    protected final List<Object> inner;

    public AbstractELispVector(List<Object> inner) {
        this.inner = inner;
    }

    @Override
    public void setUntyped(int i, Object object) {
        set(i, object);
    }

    @Override
    public Object get(int index) {
        return inner.get(index);
    }

    @Override
    public Object set(int index, Object element) {
        return inner.set(index, element);
    }

    @Override
    public int size() {
        return inner.size();
    }
}
