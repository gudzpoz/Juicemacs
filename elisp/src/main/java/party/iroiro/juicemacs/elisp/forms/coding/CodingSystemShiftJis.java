package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.BuiltInCharSet;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_ATTR_ASCII_COMPAT;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_SJIS;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.SHIFT_JIS;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asInt;

public final class CodingSystemShiftJis implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return SHIFT_JIS;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        ELispCons charsetList = asCons(charsetListObject);
        int length = charsetList.size();
        if (length != 3 && length != 4) {
            throw ELispSignals.error("There should be three or four charsets");
        }

        ConsIterator i = charsetList.iterator();
        ELispCharset charset = BuiltInCharSet.getCharsetFromId(asInt(i.next()));
        if (charset.dimension != 1) {
            throw ELispSignals.error("Shift-JIS charset must be one-dimensional");
        }
        if (charset.asciiCompatibleP) {
            attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        }

        charset = BuiltInCharSet.getCharsetFromId(asInt(i.next()));
        if (charset.dimension != 1) {
            throw ELispSignals.error("Shift-JIS charset must be one-dimensional");
        }
        if (charset.asciiCompatibleP) {
            attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        }

        charset = BuiltInCharSet.getCharsetFromId(asInt(i.next()));
        if (charset.dimension != 2) {
            throw ELispSignals.error("Shift-JIS charset must be two-dimensional");
        }

        if (i.hasNext()) {
            charset = BuiltInCharSet.getCharsetFromId(asInt(i.next()));
            if (charset.dimension != 2) {
                throw ELispSignals.error("Shift-JIS charset must be two-dimensional");
            }
        }

        // TODO: Vsjis_coding_system = name;
        return CODING_CATEGORY_SJIS;
    }
}
