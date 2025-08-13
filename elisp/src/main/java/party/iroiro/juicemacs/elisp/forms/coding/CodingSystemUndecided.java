package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;

import java.io.IOException;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_UNDECIDED;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;

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

    @Override
    public ELispCodingSystem create(ELispCodingSystem.Spec spec, EolAwareStringBuilder.EndOfLine eol) {
        boolean preferUtf8 = spec.preferUtf8();
        return new DetectingCodingSystem(spec, eol, preferUtf8);
    }

    final class DetectingCodingSystem extends ELispCodingSystem {
        private final boolean preferUtf8;

        DetectingCodingSystem(Spec spec, EolAwareStringBuilder.EndOfLine eol, boolean preferUtf8) {
            super(CodingSystemUndecided.this, spec, eol);
            this.preferUtf8 = preferUtf8;
        }

        @Override
        MuleStringBuilder decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException {
            ELispSymbol detected = codings.getDetector().detect(input);
            if (detected == RAW_TEXT && preferUtf8) {
                detected = UTF_8;
            }
            throw new OtherCodingDetectedException(detected);
        }
    }
}
