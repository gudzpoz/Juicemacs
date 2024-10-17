package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.List;

public class ELispRecord extends AbstractELispVector {
    public ELispRecord(List<Object> inner) {
        super(inner.toArray());
        if (inner.isEmpty()) {
            throw ELispSignals.invalidReadSyntax("Empty record");
        }
    }

    @Override
    public String toString() {
        return toStringHelper("#s(", ")");
    }
}
