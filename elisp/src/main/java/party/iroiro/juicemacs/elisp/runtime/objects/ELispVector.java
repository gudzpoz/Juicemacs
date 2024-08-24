package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.List;

public final class ELispVector extends AbstractELispVector {

    public ELispVector(List<Object> inner) {
        super(inner);
    }

    @Override
    public String type() {
        return "vector";
    }

}
