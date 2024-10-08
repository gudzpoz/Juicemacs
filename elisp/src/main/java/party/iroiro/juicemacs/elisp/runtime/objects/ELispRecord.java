package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.List;

public class ELispRecord extends AbstractELispVector {
    public ELispRecord(List<Object> inner) {
        super(inner);
        if (inner.isEmpty()) {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public String toString() {
        return toStringHelper("#s(", ")");
    }
}
