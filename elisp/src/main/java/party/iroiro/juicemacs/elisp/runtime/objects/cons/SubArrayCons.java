package party.iroiro.juicemacs.elisp.runtime.objects.cons;

public record SubArrayCons(ArrayCons array, int index) implements ICons {
    @Override
    public Object car() {
        return array.getUncheckedAt(index);
    }

    @Override
    public void setCar(Object car) {
        array.setUncheckedAt(index, car);
    }

    @Override
    public Object cdr() {
        return null;
    }

    @Override
    public void setCdr(Object cdr) {

    }
}
