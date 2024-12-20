package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asSym;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class CodingSystemUtf16 implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return UTF_16;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        attrs.set(CODING_ATTR_ASCII_COMPAT, false);
        if (args.length < CODING_ARG_UTF16_MAX) {
            throw ELispCodingSystemType.shortArgs(args.length);
        }
        Object bom = args[CODING_ARG_UTF16_BOM];
        ELispCodingSystemType.checkBom(bom);
        attrs.set(CODING_ATTR_UTF_BOM, bom);

        Object endian = args[CODING_ARG_UTF16_ENDIAN];
        ELispSymbol endianSym = asSym(endian);
        if (isNil(endianSym)) {
            endianSym = BIG;
        } else if (endianSym != BIG && endianSym != LITTLE) {
            throw ELispSignals.error("Invalid endian");
        }
        attrs.set(CODING_ATTR_UTF_16_ENDIAN, endianSym);

        if (bom instanceof ELispCons) {
            return CODING_CATEGORY_UTF_16_AUTO;
        } else {
            if (isNil(bom)) {
                return endianSym == BIG ? CODING_CATEGORY_UTF_16_BE_NOSIG : CODING_CATEGORY_UTF_16_LE_NOSIG;
            } else {
                return endianSym == BIG ? CODING_CATEGORY_UTF_16_BE : CODING_CATEGORY_UTF_16_LE;
            }
        }
    }
}
