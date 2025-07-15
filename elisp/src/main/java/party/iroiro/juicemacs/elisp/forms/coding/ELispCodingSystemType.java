package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.BuiltInCoding;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.io.IOException;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCoding.checkCodingSystem;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.EOL_SEEN_CRLF;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.DEFINE_CODING_SYSTEM_INTERNAL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

sealed interface ELispCodingSystemType
        permits CodingSystemBig5, CodingSystemCcl, CodingSystemCharset, CodingSystemEmacsMule, CodingSystemIso2022,
        CodingSystemRawText, CodingSystemShiftJis, CodingSystemUndecided, CodingSystemUtf16, CodingSystemUtf8 {
    /// Returns the coding type of the coding system.
    ///
    /// Possible values:
    /// - [ELispContext#CHARSET]
    /// - [ELispContext#CCL]
    /// - [ELispContext#UTF_16]
    /// - [ELispContext#ISO_2022]
    /// - [ELispContext#EMACS_MULE]
    /// - [ELispContext#SHIFT_JIS]
    /// - [ELispContext#BIG5]
    /// - [ELispContext#RAW_TEXT]
    /// - [ELispContext#UTF_8]
    /// - [ELispContext#UNDECIDED]
    ELispSymbol codingType();

    /// Initializes the attrs vector from the args supplied to
    /// [BuiltInCoding.FDefineCodingSystemInternal#defineCodingSystemInternal(Object\[\])]
    ///
    /// The implementations of this function are extracted from `Fdefine_coding_system_internal`
    /// in Emacs into separate files for modulization.
    ///
    /// @param attrs the vector to initialize
    /// @param args the arguments supplied to the function
    /// @param charsetListObject the charset list object
    /// @return the category of the coding system
    int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject);

    default ELispCodingSystem create(ELispCodingSystem.Spec spec, EolAwareStringBuilder.EndOfLine eol) {
        return new EolDetectingCodingSystem(spec, new ELispSymbol[]{});
    }

    static ELispSignals.ELispSignalException shortArgs(long length) {
        return ELispSignals.wrongNumberOfArguments(DEFINE_CODING_SYSTEM_INTERNAL, length);
    }

    static void checkBom(Object bom) {
        if (!isNil(bom) && !isT(bom)) {
            ELispCons bomCons = asCons(bom);
            checkCodingSystem(bomCons.car());
            checkCodingSystem(bomCons.cdr());
        }
    }

    final class EolDetectingCodingSystem extends ELispCodingSystem {
        private static final int PEEK_LIMIT = 16 * 1024;
        private final ELispSymbol[] eolEncodings;

        EolDetectingCodingSystem(Spec spec, ELispSymbol[] eolEncodings) {
            super(ELispCodings.CODING_SYSTEM_UNDECIDED, spec, EolAwareStringBuilder.EndOfLine.LF);
            this.eolEncodings = eolEncodings;
        }

        @Override
        MuleStringBuffer decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException {
            int eolSeen = 0;
            int count = 0;
            while (input.hasNext() && count < PEEK_LIMIT) {
                byte b = input.next();
                do {
                    count++;
                    if (b == '\r') {
                        if (input.hasNext()) {
                            b = input.next();
                            if (b == '\n') {
                                count++;
                                eolSeen |= EOL_SEEN_CRLF;
                                break;
                            }
                        }
                        eolSeen |= EOL_SEEN_CR;
                        continue;
                    } else if (b == '\n') {
                        eolSeen |= EOL_SEEN_LF;
                    }
                    break;
                } while (true);
            }
            ELispSymbol encoding = eolEncodings[switch (eolSeen) {
                case EOL_SEEN_CR, EOL_SEEN_CR | EOL_SEEN_CRLF -> 1;
                case EOL_SEEN_CRLF -> 2;
                default -> 0;
            }];
            throw new OtherCodingDetectedException(encoding);
        }
    }
}
