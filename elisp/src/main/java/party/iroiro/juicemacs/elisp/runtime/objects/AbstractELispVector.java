package party.iroiro.juicemacs.elisp.runtime.objects;

public abstract class AbstractELispVector extends ELispVectorLike<Object> {
    // TODO: Reduce copies in the old List API
    // TODO: Transparent long[]/double[] vectors
    protected final Object[] inner;

    public AbstractELispVector(Object[] inner) {
        this.inner = inner;
    }

    @Override
    public void setUntyped(int i, Object object) {
        set(i, object);
    }

    @Override
    public Object get(int index) {
        return inner[index];
    }

    @Override
    public Object set(int index, Object element) {
        Object old = inner[index];
        inner[index] = element;
        return old;
    }

    @Override
    public int size() {
        return inner.length;
    }
}
