package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.HashMap;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.CHARSETP;

public class BuiltInCharSet extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharSetFactory.getFactories();
    }

    private final static HashMap<ELispSymbol, ELispVector> CHARSET_HASH_TABLE = new HashMap<>();

    public static void defineCharsetInternal(
            ELispSymbol name,
            int dimension,
            String codeSpaceChars,
            int minCode, int maxCode,
            int isoFinal, int isoRevision, int emacsMuleId,
            int boolAsciiCompatible, int boolSupplementary,
            int codeOffset
    ) {
        // TODO
    }

    /**
     * <pre>
     * Return non-nil if and only if OBJECT is a charset.
     * </pre>
     */
    @ELispBuiltIn(name = "charsetp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charsetp(ELispSymbol object) {
            return CHARSET_HASH_TABLE.containsKey(object);
        }
    }

    /**
     * <pre>
     * Call FUNCTION for all characters in CHARSET.
     * Optional 3rd argument ARG is an additional argument to be passed
     * to FUNCTION, see below.
     * Optional 4th and 5th arguments FROM-CODE and TO-CODE specify the
     * range of code points (in CHARSET) of target characters on which to
     * map the FUNCTION.  Note that these are not character codes, but code
     * points of CHARSET; for the difference see `decode-char' and
     * `list-charset-chars'.  If FROM-CODE is nil or imitted, it stands for
     * the first code point of CHARSET; if TO-CODE is nil or omitted, it
     * stands for the last code point of CHARSET.
     *
     * FUNCTION will be called with two arguments: RANGE and ARG.
     * RANGE is a cons (FROM .  TO), where FROM and TO specify a range of
     * characters that belong to CHARSET on which FUNCTION should do its
     * job.  FROM and TO are Emacs character codes, unlike FROM-CODE and
     * TO-CODE, which are CHARSET code points.
     * </pre>
     */
    @ELispBuiltIn(name = "map-charset-chars", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMapCharsetChars extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mapCharsetChars(Object function, Object charset, Object arg, Object fromCode, Object toCode) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * For internal use only.
     * usage: (define-charset-internal ...)
     * </pre>
     */
    @ELispBuiltIn(name = "define-charset-internal", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FDefineCharsetInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void defineCharsetInternal(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Define ALIAS as an alias for charset CHARSET.
     * </pre>
     */
    @ELispBuiltIn(name = "define-charset-alias", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDefineCharsetAlias extends ELispBuiltInBaseNode {
        @Specialization
        public static Void defineCharsetAlias(Object alias, Object charset) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the property list of CHARSET.
     * </pre>
     */
    @ELispBuiltIn(name = "charset-plist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispVector charsetPlist(ELispSymbol charset) {
            ELispVector vector = CHARSET_HASH_TABLE.get(charset);
            if (vector == null) {
                throw ELispSignals.wrongTypeArgument(CHARSETP, charset);
            }
            return vector;
        }
    }

    /**
     * <pre>
     * Set CHARSET's property list to PLIST.
     * </pre>
     */
    @ELispBuiltIn(name = "set-charset-plist", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetCharsetPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setCharsetPlist(Object charset, Object plist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Unify characters of CHARSET with Unicode.
     * This means reading the relevant file and installing the table defined
     * by CHARSET's `:unify-map' property.
     *
     * Optional second arg UNIFY-MAP is a file name string or a vector.  It has
     * the same meaning as the `:unify-map' attribute in the function
     * `define-charset' (which see).
     *
     * Optional third argument DEUNIFY, if non-nil, means to de-unify CHARSET.
     * </pre>
     */
    @ELispBuiltIn(name = "unify-charset", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FUnifyCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unifyCharset(Object charset, Object unifyMap, Object deunify) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an unused ISO final char for a charset of DIMENSION and CHARS.
     * DIMENSION is the number of bytes to represent a character: 1 or 2.
     * CHARS is the number of characters in a dimension: 94 or 96.
     *
     * This final char is for private use, thus the range is `0' (48) .. `?' (63).
     * If there's no unused final char for the specified kind of charset,
     * return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "get-unused-iso-final-char", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetUnusedIsoFinalChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getUnusedIsoFinalChar(Object dimension, Object chars) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Declare an equivalent charset for ISO-2022 decoding.
     *
     * On decoding by an ISO-2022 base coding system, when a charset
     * specified by DIMENSION, CHARS, and FINAL-CHAR is designated, behave as
     * if CHARSET is designated instead.
     * </pre>
     */
    @ELispBuiltIn(name = "declare-equiv-charset", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FDeclareEquivCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Void declareEquivCharset(Object dimension, Object chars, Object finalChar, Object charset) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of charsets in the region between BEG and END.
     * BEG and END are buffer positions.
     * Optional arg TABLE if non-nil is a translation table to look up.
     *
     * If the current buffer is unibyte, the returned list may contain
     * only `ascii', `eight-bit-control', and `eight-bit-graphic'.
     * </pre>
     */
    @ELispBuiltIn(name = "find-charset-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFindCharsetRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void findCharsetRegion(Object beg, Object end, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of charsets in STR.
     * Optional arg TABLE if non-nil is a translation table to look up.
     *
     * If STR is unibyte, the returned list may contain
     * only `ascii', `eight-bit-control', and `eight-bit-graphic'.
     * </pre>
     */
    @ELispBuiltIn(name = "find-charset-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFindCharsetString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void findCharsetString(Object str, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Decode the pair of CHARSET and CODE-POINT into a character.
     * Return nil if CODE-POINT is not valid in CHARSET.
     *
     * CODE-POINT may be a cons (HIGHER-16-BIT-VALUE . LOWER-16-BIT-VALUE),
     * although this usage is obsolescent.
     * </pre>
     */
    @ELispBuiltIn(name = "decode-char", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDecodeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void decodeChar(Object charset, Object codePoint) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Encode the character CH into a code-point of CHARSET.
     * Return the encoded code-point as an integer,
     * or nil if CHARSET doesn't support CH.
     * </pre>
     */
    @ELispBuiltIn(name = "encode-char", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEncodeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void encodeChar(Object ch, Object charset) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a character of CHARSET whose position codes are CODEn.
     *
     * CODE1 through CODE4 are optional, but if you don't supply sufficient
     * position codes, it is assumed that the minimum code in each dimension
     * is specified.
     * </pre>
     */
    @ELispBuiltIn(name = "make-char", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMakeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeChar(Object charset, Object code1, Object code2, Object code3, Object code4) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return list of charset and one to four position-codes of CH.
     * The charset is decided by the current priority order of charsets.
     * A position-code is a byte value of each dimension of the code-point of
     * CH in the charset.
     * </pre>
     */
    @ELispBuiltIn(name = "split-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSplitChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void splitChar(Object ch) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the charset of highest priority that contains CH.
     * ASCII characters are an exception: for them, this function always
     * returns `ascii'.
     * If optional 2nd arg RESTRICTION is non-nil, it is a list of charsets
     * from which to find the charset.  It may also be a coding system.  In
     * that case, find the charset from what supported by that coding system.
     * </pre>
     */
    @ELispBuiltIn(name = "char-charset", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charCharset(Object ch, Object restriction) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return charset of a character in the current buffer at position POS.
     * If POS is nil, it defaults to the current point.
     * If POS is out of range, the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "charset-after", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetAfter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charsetAfter(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.
     *
     * ISO 2022's designation sequence (escape sequence) distinguishes charsets
     * by their DIMENSION, CHARS, and FINAL-CHAR,
     * whereas Emacs distinguishes them by charset symbol.
     * See the documentation of the function `charset-info' for the meanings of
     * DIMENSION, CHARS, and FINAL-CHAR.
     * </pre>
     */
    @ELispBuiltIn(name = "iso-charset", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FIsoCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Void isoCharset(Object dimension, Object chars, Object finalChar) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * Clear temporary charset mapping tables.
     * It should be called only from temacs invoked for dumping.
     * </pre>
     */
    @ELispBuiltIn(name = "clear-charset-maps", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FClearCharsetMaps extends ELispBuiltInBaseNode {
        @Specialization
        public static Void clearCharsetMaps() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the list of charsets ordered by priority.
     * HIGHESTP non-nil means just return the highest priority one.
     * </pre>
     */
    @ELispBuiltIn(name = "charset-priority-list", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetPriorityList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charsetPriorityList(Object highestp) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Assign higher priority to the charsets given as arguments.
     * usage: (set-charset-priority &amp;rest charsets)
     * </pre>
     */
    @ELispBuiltIn(name = "set-charset-priority", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FSetCharsetPriority extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setCharsetPriority(Object charsets, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * Return charset identification number of CHARSET.
     * </pre>
     */
    @ELispBuiltIn(name = "charset-id-internal", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetIdInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charsetIdInternal(Object charset) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Sort charset list CHARSETS by a priority of each charset.
     * Return the sorted list.  CHARSETS is modified by side effects.
     * See also `charset-priority-list' and `set-charset-priority'.
     * </pre>
     */
    @ELispBuiltIn(name = "sort-charsets", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSortCharsets extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sortCharsets(Object charsets) {
            throw new UnsupportedOperationException();
        }
    }
}
