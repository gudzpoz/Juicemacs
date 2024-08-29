package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInCharSet extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharSetFactory.getFactories();
    }

    @ELispBuiltIn(name = "charsetp", minArgs = 1, maxArgs = 1, doc = "Return non-nil if and only if OBJECT is a charset.")
    @GenerateNodeFactory
    public abstract static class FCharsetp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "map-charset-chars", minArgs = 2, maxArgs = 5, doc = "Call FUNCTION for all characters in CHARSET.\nOptional 3rd argument ARG is an additional argument to be passed\nto FUNCTION, see below.\nOptional 4th and 5th arguments FROM-CODE and TO-CODE specify the\nrange of code points (in CHARSET) of target characters on which to\nmap the FUNCTION.  Note that these are not character codes, but code\npoints of CHARSET; for the difference see `decode-char' and\n`list-charset-chars'.  If FROM-CODE is nil or imitted, it stands for\nthe first code point of CHARSET; if TO-CODE is nil or omitted, it\nstands for the last code point of CHARSET.\n\nFUNCTION will be called with two arguments: RANGE and ARG.\nRANGE is a cons (FROM .  TO), where FROM and TO specify a range of\ncharacters that belong to CHARSET on which FUNCTION should do its\njob.  FROM and TO are Emacs character codes, unlike FROM-CODE and\nTO-CODE, which are CHARSET code points.")
    @GenerateNodeFactory
    public abstract static class FMapCharsetChars extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapCharsetChars(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "define-charset-internal", minArgs = 0, maxArgs = 0, varArgs = true, doc = "For internal use only.\nusage: (define-charset-internal ...)")
    @GenerateNodeFactory
    public abstract static class FDefineCharsetInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defineCharsetInternal(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "define-charset-alias", minArgs = 2, maxArgs = 2, doc = "Define ALIAS as an alias for charset CHARSET.")
    @GenerateNodeFactory
    public abstract static class FDefineCharsetAlias extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defineCharsetAlias(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-plist", minArgs = 1, maxArgs = 1, doc = "Return the property list of CHARSET.")
    @GenerateNodeFactory
    public abstract static class FCharsetPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetPlist(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-charset-plist", minArgs = 2, maxArgs = 2, doc = "Set CHARSET's property list to PLIST.")
    @GenerateNodeFactory
    public abstract static class FSetCharsetPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharsetPlist(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unify-charset", minArgs = 1, maxArgs = 3, doc = "Unify characters of CHARSET with Unicode.\nThis means reading the relevant file and installing the table defined\nby CHARSET's `:unify-map' property.\n\nOptional second arg UNIFY-MAP is a file name string or a vector.  It has\nthe same meaning as the `:unify-map' attribute in the function\n`define-charset' (which see).\n\nOptional third argument DEUNIFY, if non-nil, means to de-unify CHARSET.")
    @GenerateNodeFactory
    public abstract static class FUnifyCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unifyCharset(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-unused-iso-final-char", minArgs = 2, maxArgs = 2, doc = "Return an unused ISO final char for a charset of DIMENSION and CHARS.\nDIMENSION is the number of bytes to represent a character: 1 or 2.\nCHARS is the number of characters in a dimension: 94 or 96.\n\nThis final char is for private use, thus the range is `0' (48) .. `?' (63).\nIf there's no unused final char for the specified kind of charset,\nreturn nil.")
    @GenerateNodeFactory
    public abstract static class FGetUnusedIsoFinalChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getUnusedIsoFinalChar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "declare-equiv-charset", minArgs = 4, maxArgs = 4, doc = "Declare an equivalent charset for ISO-2022 decoding.\n\nOn decoding by an ISO-2022 base coding system, when a charset\nspecified by DIMENSION, CHARS, and FINAL-CHAR is designated, behave as\nif CHARSET is designated instead.")
    @GenerateNodeFactory
    public abstract static class FDeclareEquivCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object declareEquivCharset(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "find-charset-region", minArgs = 2, maxArgs = 3, doc = "Return a list of charsets in the region between BEG and END.\nBEG and END are buffer positions.\nOptional arg TABLE if non-nil is a translation table to look up.\n\nIf the current buffer is unibyte, the returned list may contain\nonly `ascii', `eight-bit-control', and `eight-bit-graphic'.")
    @GenerateNodeFactory
    public abstract static class FFindCharsetRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findCharsetRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "find-charset-string", minArgs = 1, maxArgs = 2, doc = "Return a list of charsets in STR.\nOptional arg TABLE if non-nil is a translation table to look up.\n\nIf STR is unibyte, the returned list may contain\nonly `ascii', `eight-bit-control', and `eight-bit-graphic'.")
    @GenerateNodeFactory
    public abstract static class FFindCharsetString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findCharsetString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "decode-char", minArgs = 2, maxArgs = 2, doc = "Decode the pair of CHARSET and CODE-POINT into a character.\nReturn nil if CODE-POINT is not valid in CHARSET.\n\nCODE-POINT may be a cons (HIGHER-16-BIT-VALUE . LOWER-16-BIT-VALUE),\nalthough this usage is obsolescent.")
    @GenerateNodeFactory
    public abstract static class FDecodeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object decodeChar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "encode-char", minArgs = 2, maxArgs = 2, doc = "Encode the character CH into a code-point of CHARSET.\nReturn the encoded code-point as an integer,\nor nil if CHARSET doesn't support CH.")
    @GenerateNodeFactory
    public abstract static class FEncodeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object encodeChar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-char", minArgs = 1, maxArgs = 5, doc = "Return a character of CHARSET whose position codes are CODEn.\n\nCODE1 through CODE4 are optional, but if you don't supply sufficient\nposition codes, it is assumed that the minimum code in each dimension\nis specified.")
    @GenerateNodeFactory
    public abstract static class FMakeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeChar(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "split-char", minArgs = 1, maxArgs = 1, doc = "Return list of charset and one to four position-codes of CH.\nThe charset is decided by the current priority order of charsets.\nA position-code is a byte value of each dimension of the code-point of\nCH in the charset.")
    @GenerateNodeFactory
    public abstract static class FSplitChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object splitChar(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-charset", minArgs = 1, maxArgs = 2, doc = "Return the charset of highest priority that contains CH.\nASCII characters are an exception: for them, this function always\nreturns `ascii'.\nIf optional 2nd arg RESTRICTION is non-nil, it is a list of charsets\nfrom which to find the charset.  It may also be a coding system.  In\nthat case, find the charset from what supported by that coding system.")
    @GenerateNodeFactory
    public abstract static class FCharCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charCharset(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-after", minArgs = 0, maxArgs = 1, doc = "Return charset of a character in the current buffer at position POS.\nIf POS is nil, it defaults to the current point.\nIf POS is out of range, the value is nil.")
    @GenerateNodeFactory
    public abstract static class FCharsetAfter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetAfter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "iso-charset", minArgs = 3, maxArgs = 3, doc = "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.\n\nISO 2022's designation sequence (escape sequence) distinguishes charsets\nby their DIMENSION, CHARS, and FINAL-CHAR,\nwhereas Emacs distinguishes them by charset symbol.\nSee the documentation of the function `charset-info' for the meanings of\nDIMENSION, CHARS, and FINAL-CHAR.")
    @GenerateNodeFactory
    public abstract static class FIsoCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object isoCharset(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clear-charset-maps", minArgs = 0, maxArgs = 0, doc = "Internal use only.\nClear temporary charset mapping tables.\nIt should be called only from temacs invoked for dumping.")
    @GenerateNodeFactory
    public abstract static class FClearCharsetMaps extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clearCharsetMaps() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-priority-list", minArgs = 0, maxArgs = 1, doc = "Return the list of charsets ordered by priority.\nHIGHESTP non-nil means just return the highest priority one.")
    @GenerateNodeFactory
    public abstract static class FCharsetPriorityList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetPriorityList(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-charset-priority", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Assign higher priority to the charsets given as arguments.\nusage: (set-charset-priority &rest charsets)")
    @GenerateNodeFactory
    public abstract static class FSetCharsetPriority extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharsetPriority(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-id-internal", minArgs = 0, maxArgs = 1, doc = "Internal use only.\nReturn charset identification number of CHARSET.")
    @GenerateNodeFactory
    public abstract static class FCharsetIdInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetIdInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sort-charsets", minArgs = 1, maxArgs = 1, doc = "Sort charset list CHARSETS by a priority of each charset.\nReturn the sorted list.  CHARSETS is modified by side effects.\nSee also `charset-priority-list' and `set-charset-priority'.")
    @GenerateNodeFactory
    public abstract static class FSortCharsets extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sortCharsets(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
