package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public final class ELispVector extends AbstractELispVector {
    public ELispVector(List<Object> inner) {
        super(inner.toArray());
    }

    public ELispVector(Object[] inner) {
        super(inner);
    }

    public ELispVector(ELispVector other) {
        super(Arrays.copyOf(other.inner, other.inner.length));
    }

    public ELispVector(int count, Object value) {
        this(Collections.nCopies(count, value));
    }

    public ELispVector reverse() {
        return new ELispVector(Arrays.asList(inner).reversed());
    }

    @Override
    public String toString() {
        return toStringHelper("[", "]");
    }
}
