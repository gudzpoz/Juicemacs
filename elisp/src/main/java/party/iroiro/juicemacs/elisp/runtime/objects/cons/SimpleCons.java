package party.iroiro.juicemacs.elisp.runtime.objects.cons;

public final class SimpleCons implements ICons {
    private Object car = false;
    private Object cdr = false;

    @Override
    public Object car() {
        return car;
    }

    @Override
    public void setCar(Object car) {
        this.car = car;
    }

    @Override
    public Object cdr() {
        return cdr;
    }

    @Override
    public void setCdr(Object cdr) {
        this.cdr = cdr;
    }
}
