package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.List;

public class ELispRecord extends AbstractELispVector {
    public ELispRecord(List<Object> inner) {
        this(inner.toArray());
    }

    public ELispRecord(Object[] inner) {
        super(inner);
        if (inner.length == 0) {
            throw ELispSignals.invalidReadSyntax("Empty record");
        }
    }

    @Override
    public String toString() {
        return toStringHelper("#s(", ")");
    }
}
