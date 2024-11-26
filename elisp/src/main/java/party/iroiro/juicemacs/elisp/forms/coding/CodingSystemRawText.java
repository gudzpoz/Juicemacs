package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_ATTR_ASCII_COMPAT;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_RAW_TEXT;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.RAW_TEXT;

public final class CodingSystemRawText implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return RAW_TEXT;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        return CODING_CATEGORY_RAW_TEXT;
    }
}
