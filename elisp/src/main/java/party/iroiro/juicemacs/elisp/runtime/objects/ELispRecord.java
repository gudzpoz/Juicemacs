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
    public String type() {
        return "record";
    }
}
