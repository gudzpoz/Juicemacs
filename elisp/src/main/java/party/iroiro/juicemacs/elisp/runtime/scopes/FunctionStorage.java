package party.iroiro.juicemacs.elisp.runtime.scopes;

public final class FunctionStorage {
    private Object function = false;

    public Object get() {
        return function;
    }

    public void set(Object function) {
        this.function = function;
    }
}
