package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.BuiltInCharSet;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.CHARSET;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class CodingSystemCharset implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return CHARSET;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetList) {
        ELispVector charsetIds = new ELispVector(256, false);
        for (Object charsetIdObject : asConsOrNil(charsetList)) {
            int charsetId = asInt(charsetIdObject);
            ELispCharset charset = BuiltInCharSet.getCharsetFromId(charsetId);
            int dim = charset.dimension;
            int idx = (dim - 1) * 4;
            if (charset.asciiCompatibleP) {
                attrs.set(CODING_ATTR_ASCII_COMPAT, true);
            }
            for (int i = charset.codeSpace[idx]; i < charset.codeSpace[idx + 1]; i++) {
                Object idSlot = charsetIds.get(i);
                if (isNil(idSlot)) {
                    idSlot = charsetIdObject;
                } else if (idSlot instanceof Long l) {
                    int prevDim = BuiltInCharSet.getCharsetFromId(l.intValue()).dimension;
                    if (dim < prevDim) {
                        idSlot = ELispCons.listOf(charsetIdObject, l);
                    } else {
                        idSlot = ELispCons.listOf(l, charsetIdObject);
                    }
                } else {
                    ELispCons prevCons = null;
                    ConsIterator iterator = asCons(idSlot).listIterator(0);
                    while (iterator.hasNextCons()) {
                        ELispCons current = iterator.nextCons();
                        int currentDim = BuiltInCharSet.getCharsetFromId(asInt(current.car())).dimension;
                        if (dim < currentDim) {
                            if (prevCons == null) {
                                idSlot = ELispCons.cons(charsetIdObject, idSlot);
                            } else {
                                prevCons.insertAfter(charsetIdObject);
                            }
                            break;
                        }
                        if (isNil(current.cdr())) {
                            current.insertAfter(charsetIdObject);
                            break;
                        }
                        prevCons = current;
                    }
                }
                charsetIds.set(i, idSlot);
            }
        }
        attrs.set(CODING_ATTR_CHARSET_VALIDS, charsetIds);
        return CODING_CATEGORY_CHARSET;
    }
}
