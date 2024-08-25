package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.List;

public final class ELispByteCode extends AbstractELispVector {

    private ELispByteCode(List<Object> inner) {
        super(inner);
    }

    @Override
    public String type() {
        return "compiled-function";
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
            throw new IllegalArgumentException();
        }
        if (inner.size() >= 5) {
            Object doc = inner.get(4);
            if (!(doc instanceof ELispString)) {
                throw new IllegalArgumentException();
            }
        }
        if (inner.size() >= 7) {
            throw new IllegalArgumentException();
        }
        return new ELispByteCode(inner);
    }

}
