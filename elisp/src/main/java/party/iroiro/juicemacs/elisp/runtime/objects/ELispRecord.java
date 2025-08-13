package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.List;

public final class ELispRecord extends AbstractELispVector {
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
    public void display(ELispPrint print) {
        vectorPrintHelper(print, "#s(", ")", inner);
    }
}
