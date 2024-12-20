package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.BuiltInCoding;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCoding.checkCodingSystem;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.DEFINE_CODING_SYSTEM_INTERNAL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public sealed interface ELispCodingSystemType
        permits CodingSystemBig5, CodingSystemCcl, CodingSystemCharset, CodingSystemEmacsMule, CodingSystemIso2022, CodingSystemRawText, CodingSystemShiftJis, CodingSystemUndecided, CodingSystemUtf16, CodingSystemUtf8 {
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

    default CodingSystemCategory create(CodingSystemCategory coding) {
        return new CodingSystemCategory.RawText().copy(coding);
    }
}
