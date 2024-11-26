package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.UTF_8;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class CodingSystemUtf8 implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return UTF_8;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        if (args.length < CODING_ARG_UTF8_MAX) {
            throw ELispCodingSystemType.shortArgs(args.length);
        }
        Object bom = args[CODING_ARG_UTF8_BOM];
        ELispCodingSystemType.checkBom(bom);
        attrs.set(CODING_ATTR_UTF_BOM, bom);
        if (isNil(bom)) {
            attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        }
        if (bom instanceof ELispCons) {
            return CODING_CATEGORY_UTF_8_AUTO;
        } else if (isNil(bom)) {
            return CODING_CATEGORY_UTF_8_NOSIG;
        } else {
            return CODING_CATEGORY_UTF_8_SIG;
        }
    }
}
