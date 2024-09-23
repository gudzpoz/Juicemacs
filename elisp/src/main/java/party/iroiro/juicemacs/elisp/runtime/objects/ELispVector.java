package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.ArrayList;
import java.util.List;

public final class ELispVector extends AbstractELispVector {
    public ELispVector(List<Object> inner) {
        super(inner);
    }

    public ELispVector(ELispVector other) {
        super(new ArrayList<>(other.inner));
    }

    public ELispVector reverse() {
        return new ELispVector(new ArrayList<>(inner.reversed()));
    }

    @Override
    public String toString() {
        return toStringHelper("[", "]");
    }
}
