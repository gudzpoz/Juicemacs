package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.BuiltInCharSet;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.ISO_2022;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

final class CodingSystemIso2022 implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return ISO_2022;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        if (args.length < CODING_ARG_ISO2022_MAX) {
            throw ELispCodingSystemType.shortArgs(args.length);
        }
        ELispVector initial = BuiltInFns.FCopySequence.copySequenceVector(asVector(args[CODING_ARG_ISO2022_INITIAL]));
        for (int i = 0; i < 4; i++) {
            Object charsetSym = initial.get(i);
            if (isNil(charsetSym)) {
                initial.set(i, -1L);
            } else {
                ELispCharset charset = BuiltInCharSet.getCharset(charsetSym);
                initial.set(i, (long) charset.id);
                if (i == 0 || charset.asciiCompatibleP) {
                    attrs.set(CODING_ATTR_ASCII_COMPAT, true);
                }
            }
        }
        ELispCons regUsage = asCons(args[CODING_ARG_ISO2022_REG_USAGE]);
        asLong(regUsage.car());
        asLong(regUsage.cdr());

        ELispCons.ListBuilder requestBuilder = new ELispCons.ListBuilder();
        for (Object charsetCons : asConsOrNil(args[CODING_ARG_ISO2022_REQUEST])) {
            ELispCons cons = asCons(charsetCons);
            int id = BuiltInCharSet.getCharset(cons.car()).id;
            asRanged(cons.cdr(), 0, 3);
            requestBuilder.add((long) id);
        }
        Object request = requestBuilder.build();

        long flags = asNat(args[CODING_ARG_ISO2022_FLAGS]);
        if (args[CODING_ARG_CHARSET_LIST] == ISO_2022) {
            flags |= CODING_ISO_FLAG_FULL_SUPPORT;
        }

        attrs.set(CODING_ATTR_ISO_INITIAL, initial);
        attrs.set(CODING_ATTR_ISO_USAGE, regUsage);
        attrs.set(CODING_ATTR_ISO_REQUEST, request);
        attrs.set(CODING_ATTR_ISO_FLAGS, flags);
        // TODO: setup_iso_safe_charsets

        int category;
        if ((flags & CODING_ISO_FLAG_SEVEN_BITS) == 0) {
            int id = asInt(initial.get(1));
            if ((flags & CODING_ISO_FLAG_LOCKING_SHIFT) != 0
                    || args[CODING_ARG_CHARSET_LIST] == ISO_2022
                    || id < 0) {
                category = CODING_CATEGORY_ISO_8_ELSE;
            } else {
                category = BuiltInCharSet.getCharsetFromId(id).dimension == 1
                        ? CODING_CATEGORY_ISO_8_1
                        : CODING_CATEGORY_ISO_8_2;
            }
        } else {
            if ((flags & (CODING_ISO_FLAG_LOCKING_SHIFT | CODING_ISO_FLAG_SINGLE_SHIFT)) == 0) {
                category = args[CODING_ARG_CHARSET_LIST] == ISO_2022
                        ? CODING_CATEGORY_ISO_7
                        : CODING_CATEGORY_ISO_7_TIGHT;
            } else {
                category = CODING_CATEGORY_ISO_7_ELSE;
            }
        }

        if (category != CODING_CATEGORY_ISO_8_1 && category != CODING_CATEGORY_ISO_8_2) {
            attrs.set(CODING_ATTR_ASCII_COMPAT, false);
        }
        return category;
    }
}
