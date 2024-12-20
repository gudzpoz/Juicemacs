package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_EMACS_MULE;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.EMACS_MULE;

public final class CodingSystemEmacsMule implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return EMACS_MULE;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        if (args[CODING_ARG_CHARSET_LIST] == EMACS_MULE) {
            attrs.set(CODING_ATTR_EMACS_MULE_FULL, true);
        }
        attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        return CODING_CATEGORY_EMACS_MULE;
    }
}
