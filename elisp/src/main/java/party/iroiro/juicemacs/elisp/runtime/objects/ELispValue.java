package party.iroiro.juicemacs.elisp.runtime.objects;

public interface ELispValue {
    boolean lispEquals(Object other);

    default String display() {
        return toString();
    }

    static String display(Object o) {
        if (o instanceof ELispValue value) {
            return value.display();
        }
        return o.toString();
    }
}
