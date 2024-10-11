package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.List;

public final class ELispByteCode extends AbstractELispVector {

    private ELispByteCode(List<Object> inner) {
        super(inner);
    }

    public static ELispByteCode create(List<Object> inner) {
        Object argList = inner.get(0);
        Object byteCode = inner.get(1);
        Object constants = inner.get(2);
        Object stack = inner.get(3);
        if (!(
                (argList instanceof Long || argList instanceof ELispCons || ELispSymbol.isNil(argList))
                        && (byteCode instanceof ELispString)
                        && (constants instanceof ELispVector)
                        && (stack instanceof Long)
        )) {
            throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
        }
        if (inner.size() >= 5) {
            Object doc = inner.get(4);
            if (!(doc instanceof ELispString)) {
                throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
            }
        }
        if (inner.size() >= 7) {
            throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
        }
        return new ELispByteCode(inner);
    }

}
