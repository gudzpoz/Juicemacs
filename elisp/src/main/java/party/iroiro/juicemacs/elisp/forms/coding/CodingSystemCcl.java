package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_CCL;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.CCL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asRanged;

public final class CodingSystemCcl implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return CCL;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        if (args.length < CODING_ARG_CCL_MAX) {
            throw ELispCodingSystemType.shortArgs(args.length);
        }

        Object decoder = args[CODING_ARG_CCL_DECODER];
        // TODO: check decoder
        if (decoder instanceof ELispVector vector) {
            decoder = BuiltInFns.FCopySequence.copySequenceVector(vector);
        }
        attrs.set(CODING_ATTR_CCL_DECODER, decoder);
        Object encoder = args[CODING_ARG_CCL_ENCODER];
        // TODO: check encoder
        if (encoder instanceof ELispVector vector) {
            encoder = BuiltInFns.FCopySequence.copySequenceVector(vector);
        }
        attrs.set(CODING_ATTR_CCL_ENCODER, encoder);

        Object validsArg = args[CODING_ARG_CCL_VALIDS];
        byte[] valids = new byte[256];
        if (!isNil(validsArg)) {
            for (Object value : asCons(validsArg)) {
                int from, to;
                if (value instanceof Long l) {
                    if (!(0 <= l && l <= 255)) {
                        throw ELispSignals.argsOutOfRange(l, 0, 255);
                    }
                    from = to = l.intValue();
                } else {
                    ELispCons range = asCons(value);
                    from = asRanged(range.car(), 0, 255);
                    to = asRanged(range.cdr(), from, 255);
                }
                for (int i = from; i <= to; i++) {
                    valids[i] = 1;
                }
            }
        }
        attrs.set(CODING_ATTR_CCL_VALIDS, new ELispString(valids));
        return CODING_CATEGORY_CCL;
    }
}
