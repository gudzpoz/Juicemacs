package party.iroiro.juicemacs.elisp.runtime.objects.cons;

public final class ArrayCons implements ICons {
    private Object[] array;
    private int size;

    @Override
    public Object car() {
        return array[0];
    }

    @Override
    public void setCar(Object car) {
        array[0] = car;
    }

    @Override
    public Object cdr() {
        return null;
    }

    @Override
    public void setCdr(Object cdr) {

    }

    public Object getUncheckedAt(int index) {
        return array[index];
    }
    public void setUncheckedAt(int index, Object value) {
        array[index] = value;
    }
}
