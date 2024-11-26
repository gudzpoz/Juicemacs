package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_UNDECIDED;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.UNDECIDED;

public final class CodingSystemUndecided implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return UNDECIDED;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        if (args.length < CODING_ARG_UNDECIDED_MAX) {
            throw ELispCodingSystemType.shortArgs(args.length);
        }
        attrs.set(CODING_ATTR_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION, args[CODING_ARG_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION]);
        attrs.set(CODING_ATTR_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION, args[CODING_ARG_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION]);
        attrs.set(CODING_ATTR_UNDECIDED_PREFER_UTF_8, args[CODING_ARG_UNDECIDED_PREFER_UTF_8]);
        return CODING_CATEGORY_UNDECIDED;
    }
}
