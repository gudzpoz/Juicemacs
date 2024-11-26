package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.forms.BuiltInFns;

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

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    @Override
    public boolean lispEquals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof AbstractELispVector vector)) {
            return false;
        }
        return getClass() == vector.getClass() && lispEquals(inner, vector.inner);
    }

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    @Override
    public int lispHashCode() {
        return lispHashCode(inner);
    }

    public static int lispHashCode(Object[] objects) {
        int result = 1;
        for (Object o : objects) {
            result = 31 * result +ELispValue.lispHashCode(o);
        }
        return result;
    }
    public static boolean lispEquals(Object[] array1, Object[] array2) {
        if (array1.length != array2.length) {
            return false;
        }
        for (int i = 0; i < array1.length; i++) {
            if (!BuiltInFns.FEqual.equal(array1[i], array2[i])) {
                return false;
            }
        }
        return true;
    }
}
