package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispByteCode extends AbstractELispVector {

    private ELispByteCode(List<Object> inner) {
        super(inner.toArray());
    }

    public static ELispByteCode create(List<Object> inner) {
        Object argList = inner.get(0);
        Object byteCode = inner.get(1);
        Object constants = inner.get(2);
        Object stack = inner.get(3);
        if (!(
                (argList instanceof Long || argList instanceof ELispCons || isNil(argList))
                        && (byteCode instanceof ELispString)
                        && (constants instanceof ELispVector)
                        && (stack instanceof Long)
        )) {
            throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
        }
        if (inner.size() >= 5) {
            Object doc = inner.get(4);
            if (!(doc instanceof ELispString || doc instanceof ELispCons)) {
                // Not string or autoload string
                throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
            }
        }
        if (inner.size() >= 7) {
            throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
        }
        return new ELispByteCode(inner);
    }

    @Override
    public void display(ELispPrint print) {
        displayHelper(print, "#<compiled-function ", ">");
    }
}
