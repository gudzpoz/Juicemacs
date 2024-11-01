package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public class BuiltInCharSet extends ELispBuiltIns {
    public BuiltInCharSet() {
        CHARSET_HASH_TABLE.clear();
        CHARSET_LIST.clear();
    }

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharSetFactory.getFactories();
    }

    private enum CharsetMethod {
        OFFSET,
        MAP,
        SUBSET,
        SUPERSET
    }

    private record ELispCharset(
            int id,
            ELispVector attributes,
            int dimension,
            byte[] codeSpaceMask,
            int[] codeSpace,
            boolean codeLinearP,
            boolean isoChars96,
            boolean asciiCompatibleP,
            boolean supplementaryP,
            boolean compactCodesP,
            boolean unifiedP,
            int isoFinal,
            int isoRevision,
            int emacsMuleId,
            CharsetMethod method,
            long minCode,
            long maxCode,
            long charIndexOffset,
            long minChar,
            long maxChar,
            long invalidCode,
            byte[] fastMap,
            long codeOffset
    ) {
        public static long codepointToIndex(
                long codepoint,
                boolean codeLinearP, long minCode,
                byte[] codeSpaceMask, int[] codeSpace, long charIndexOffset
        ) {
            if (codeLinearP) {
                return codepoint - minCode;
            }
            if (
                    (codeSpaceMask[Math.toIntExact(codepoint >> 24)] & 0x8) == 0
                            || (codeSpaceMask[Math.toIntExact((codepoint >> 16) & 0xFF)] & 0x4) == 0
                            || (codeSpaceMask[Math.toIntExact((codepoint >> 8) & 0xFF)] & 0x2) == 0
                            || (codeSpaceMask[Math.toIntExact(codepoint & 0xFF)] & 0x1) == 0
            ) {
                return -1;
            }
            return ((((codepoint >> 24) & 0xFF) - codeSpace[12]) * codeSpace[11])
                    + (((codepoint >> 16) & 0xFF) - codeSpace[8]) * codeSpace[7]
                    + (((codepoint >> 8) & 0xFF) - codeSpace[4]) * codeSpace[3]
                    + ((codepoint & 0xFF) - codeSpace[0])
                    - charIndexOffset;
        }
    }

    private final static ArrayList<ELispCharset> CHARSET_LIST = new ArrayList<>();
    private final static HashMap<ELispSymbol, ELispVector> CHARSET_HASH_TABLE = new HashMap<>();

    private static ELispCharset getCharset(Object symbol) {
        ELispVector vec = getCharsetAttr(symbol);
        //noinspection SequencedCollectionMethodCanBeUsed
        int index = ELispBuiltInBaseNode.asInt(vec.get(CHARSET_ATTR_ID));
        return CHARSET_LIST.get(index);
    }

    private static ELispVector getCharsetAttr(Object symbol) {
        //noinspection SuspiciousMethodCalls
        ELispVector vec = CHARSET_HASH_TABLE.get(symbol);
        if (vec == null) {
            throw ELispSignals.wrongTypeArgument(CHARSETP, symbol);
        }
        return vec;
    }

    //#region: enum define_charset_arg_index
    // The following documentation is partly copied from `define-charset` (defined in `mule.el`) in GNU Emacs
    /// the name of the charset (e.g., [ELispContext#ASCII] or [ELispContext#UNICODE])
    private final static int CHARSET_ARG_NAME = 0;
    /// `0`, `1`, `2`, or `3`, the dimension (a.k.a., max byte length) of code-points of the charset
    private final static int CHARSET_ARG_DIMENSION = 1;
    /// the byte code range of each dimension (read: byte) of the charset: `[min-1 max-1 ...]`
    private final static int CHARSET_ARG_CODE_SPACE = 2;
    /// the minimum code-point of the charset
    private final static int CHARSET_ARG_MIN_CODE = 3;
    /// the maximum code-point of the charset
    private final static int CHARSET_ARG_MAX_CODE = 4;
    /// the final char of the charset for ISO-2022 encoding
    private final static int CHARSET_ARG_ISO_FINAL = 5;
    /// the revision number of the charset for ISO-2022 encoding
    private final static int CHARSET_ARG_ISO_REVISION = 6;
    /// 0, or 129..255
    private final static int CHARSET_ARG_EMACS_MULE_ID = 7;
    /// true if the first 128 code points map to ASCII
    private final static int CHARSET_ARG_ASCII_COMPATIBLE_P = 8;
    /// true if used only as a parent or a subset of some other charset
    private final static int CHARSET_ARG_SUPPLEMENTARY_P = 9;
    /// a nonnegative integer that can be used as an invalid code point of the charset.
    private final static int CHARSET_ARG_INVALID_CODE = 10;
    /// an integer added to the index number of a character to get the corresponding character code
    private final static int CHARSET_ARG_CODE_OFFSET = 11;
    /// codepoint to character byte-sequence mapping, vector or name of a file
    private final static int CHARSET_ARG_MAP = 12;
    /// a list: `( PARENT MIN-CODE MAX-CODE OFFSET )`
    private final static int CHARSET_ARG_SUBSET = 13;
    /// a list of parent charsets
    private final static int CHARSET_ARG_SUPERSET = 14;
    /// similar to `CHARSET_ARG_MAP`, but maps to Unicode
    private final static int CHARSET_ARG_UNIFY_MAP = 15;
    /// the property list of the charset
    private final static int CHARSET_ARG_PLIST = 16;
    /// the maximum number of arguments
    private final static int CHARSET_ARG_MAX = 17;
    //#endregion
    //#region: enum charset_attr_index
    private final static int CHARSET_ATTR_ID = 0;
    private final static int CHARSET_ATTR_NAME = 1;
    private final static int CHARSET_ATTR_PLIST = 2;
    private final static int CHARSET_ATTR_MAP = 3;
    private final static int CHARSET_ATTR_DECODER = 4;
    private final static int CHARSET_ATTR_ENCODER = 5;
    private final static int CHARSET_ATTR_SUBSET = 6;
    private final static int CHARSET_ATTR_SUPERSET = 7;
    private final static int CHARSET_ATTR_UNIFY_MAP = 8;
    private final static int CHARSET_ATTR_DEUNIFIER = 9;
    private final static int CHARSET_ATTR_MAX = 10;
    //#endregion

    public static void defineCharsetInternal(
            ELispSymbol name,
            int dimension,
            String codeSpaceChars,
            int minCode, int maxCode,
            int isoFinal, int isoRevision, int emacsMuleId,
            int boolAsciiCompatible, int boolSupplementary,
            int codeOffset
    ) {
        Object[] args = Collections.nCopies(CHARSET_ARG_MAX, false).toArray();
        args[CHARSET_ARG_NAME] = name;
        args[CHARSET_ARG_DIMENSION] = (long) dimension;
        args[CHARSET_ARG_CODE_SPACE] = new ELispVector(
                (codeSpaceChars + "\0").chars().mapToObj(Long::valueOf).toArray()
        );
        args[CHARSET_ARG_MIN_CODE] = (long) minCode;
        args[CHARSET_ARG_MAX_CODE] = (long) maxCode;
        args[CHARSET_ARG_ISO_FINAL] = isoFinal < 0 ? false : (long) isoFinal;
        args[CHARSET_ARG_ISO_REVISION] = (long) isoRevision;
        args[CHARSET_ARG_EMACS_MULE_ID] = emacsMuleId < 0 ? false : (long) emacsMuleId;
        args[CHARSET_ARG_ASCII_COMPATIBLE_P] = boolAsciiCompatible != 0;
        args[CHARSET_ARG_SUPPLEMENTARY_P] = boolSupplementary != 0;

        args[CHARSET_ARG_CODE_OFFSET] = (long) codeOffset;

        args[CHARSET_ARG_PLIST] = ELispCons.listOf(
                CNAME,
                args[CHARSET_ARG_NAME],
                intern(":dimension"),
                args[CHARSET_ARG_DIMENSION],
                intern(":code-space"),
                args[CHARSET_ARG_CODE_SPACE],
                intern(":iso-final-char"),
                args[CHARSET_ARG_ISO_FINAL],
                intern(":emacs-mule-id"),
                args[CHARSET_ARG_EMACS_MULE_ID],
                intern(":ascii-compatible-p"),
                args[CHARSET_ARG_ASCII_COMPATIBLE_P],
                intern(":code-offset"),
                args[CHARSET_ARG_CODE_OFFSET]
        );

        FDefineCharsetInternal.defineCharsetInternal(args);
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
        public static boolean charsetp(Object object) {
            return object instanceof ELispSymbol symbol && CHARSET_HASH_TABLE.containsKey(symbol);
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
        public static boolean defineCharsetInternal(Object[] args) {
            if (args.length != CHARSET_ARG_MAX) {
                throw ELispSignals.wrongNumberOfArguments(DEFINE_CHARSET_INTERNAL, args.length);
            }

            ELispVector attrs = new ELispVector(Collections.nCopies(CHARSET_ATTR_MAX, false));

            ELispSymbol name = asSym(args[CHARSET_ARG_NAME]);
            attrs.set(CHARSET_ATTR_NAME, name);

            Object codeSpaceVal = args[CHARSET_ARG_CODE_SPACE];
            int[] codeSpace = new int[15];
            int dimension = 1;
            for (int i = 0, nchars = 1; true; i++) {
                // TODO: codeSpaceVal can be any sequence?
                long minByte = asLong(BuiltInData.FAref.aref(codeSpaceVal, i * 2L), 0, 255);
                long maxByte = asLong(BuiltInData.FAref.aref(codeSpaceVal, i * 2L + 1), minByte, 255);
                codeSpace[i * 4] = Math.toIntExact(minByte);
                codeSpace[i * 4 + 1] = Math.toIntExact(maxByte);
                codeSpace[i * 4 + 2] = Math.toIntExact(maxByte - minByte + 1);
                if (maxByte > 0) {
                    dimension = i + 1;
                }
                if (i == 3) {
                    break;
                }
                nchars *= codeSpace[i * 4 + 2];
                codeSpace[i * 4 + 3] = nchars;
            }

            Object dimensionVal = args[CHARSET_ARG_DIMENSION];
            if (!ELispSymbol.isNil(dimensionVal)) {
                dimension = (int) asLong(dimensionVal, 1, 4);
            }

            boolean dim3Linear = dimension == 3 || codeSpace[10] == 256;
            boolean dim2Linear = dimension == 2 || (codeSpace[6] == 256 && dim3Linear);
            boolean codeLinearP = dimension == 1 || (codeSpace[2] == 256 && dim2Linear);

            byte[] codeSpaceMask;
            if (codeLinearP) {
                codeSpaceMask = new byte[0];
            } else {
                codeSpaceMask = new byte[256];
                for (int i = 0; i < 4; i++) {
                    for (int j = codeSpace[i * 4]; j < codeSpace[i * 4 + 1]; j++) {
                        codeSpaceMask[j] |= (byte) (1 << i);
                    }
                }
            }

            boolean isoChar96 = codeSpace[2] == 96;

            long minCode = (
                    codeSpace[0]
                            | ((long) codeSpace[4] << 8)
                            | ((long) codeSpace[8] << 16)
                            | (Integer.toUnsignedLong(codeSpace[12]) << 24)
            );
            long maxCode = (
                    codeSpace[1]
                            | ((long) codeSpace[5] << 8)
                            | ((long) codeSpace[9] << 16)
                            | (Integer.toUnsignedLong(codeSpace[13]) << 24)
            );
            long charIndexOffset = 0;

            Object minCodeVal = args[CHARSET_ARG_MIN_CODE];
            if (!ELispSymbol.isNil(minCodeVal)) {
                long code = asLong(consToUnsigned(minCodeVal, maxCode), minCode, maxCode);
                charIndexOffset = ELispCharset.codepointToIndex(code, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);
                minCode = code;
            }
            Object maxCodeVal = args[CHARSET_ARG_MAX_CODE];
            if (!ELispSymbol.isNil(maxCodeVal)) {
                maxCode = asLong(consToUnsigned(maxCodeVal, maxCode), minCode, maxCode);
            }

            boolean compactCodesP = true;

            Object invalidVal = args[CHARSET_ARG_INVALID_CODE];
            long invalidCode;
            if (ELispSymbol.isNil(invalidVal)) {
                if (minCode > 0) {
                    invalidCode = 0;
                } else if (maxCode < Integer.toUnsignedLong(-1)) {
                    invalidCode = maxCode + 1;
                } else {
                    throw ELispSignals.error("Attribute :invalid-code must be specified");
                }
            } else {
                invalidCode = asLong(invalidVal, 0, Integer.MAX_VALUE);
            }

            Object isoFinalVal = args[CHARSET_ARG_ISO_FINAL];
            int isoFinal;
            if (ELispSymbol.isNil(isoFinalVal)) {
                isoFinal = -1;
            } else {
                isoFinal = (int) asLong(isoFinalVal, '0', 127);
            }

            Object isoRevisionVal = args[CHARSET_ARG_ISO_REVISION];
            int isoRevision = ELispSymbol.isNil(isoRevisionVal) ? -1 : (int) asLong(isoRevisionVal, -1, 63);

            Object emacsMuleVal = args[CHARSET_ARG_EMACS_MULE_ID];
            int emacsMule;
            if (ELispSymbol.isNil(emacsMuleVal)) {
                emacsMule = -1;
            } else {
                emacsMule = asInt(emacsMuleVal);
                if ((0 < emacsMule && emacsMule <= 128) || 256 <= emacsMule) {
                    throw ELispSignals.error("Invalid emacs-mule-id: " + emacsMule);
                }
            }

            boolean asciiCompatibleP = !ELispSymbol.isNil(args[CHARSET_ARG_ASCII_COMPATIBLE_P]);
            boolean supplementaryP = !ELispSymbol.isNil(args[CHARSET_ARG_SUPPLEMENTARY_P]);
            boolean unifiedP = false;

            byte[] fastMap = new byte[190];
            CharsetMethod method;
            long codeOffset = 0, minChar = 0, maxChar = 0;
            if (!ELispSymbol.isNil(args[CHARSET_ARG_CODE_OFFSET])) {
                Object val = args[CHARSET_ARG_CODE_OFFSET];
                int c = asInt(val);
                method = CharsetMethod.OFFSET;
                codeOffset = c;

                minChar = codeOffset + ELispCharset.codepointToIndex(minCode, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);
                maxChar = codeOffset + ELispCharset.codepointToIndex(maxCode, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);

                int i;
                for (i = Math.toIntExact((minChar >> 7) << 7); i < 0x10000 && i <= maxChar; i += 128) {
                    fastMap[i >> 10] |= (byte) (1 << ((i >> 7) & 7));
                }
                for (i = (i >> 12) << 12; i <= maxChar; i += 0x1000) {
                    fastMap[(i >> 15) + 62] |= (byte) (1 << ((i >> 12) & 7));
                }
                if (codeOffset == 0 && maxChar >= 0x80) {
                    asciiCompatibleP = true;
                }
            } else if (!ELispSymbol.isNil(args[CHARSET_ARG_MAP])) {
                Object val = args[CHARSET_ARG_MAP];
                attrs.set(CHARSET_ATTR_MAP, val);
                method = CharsetMethod.MAP;
            } else if (!ELispSymbol.isNil(args[CHARSET_ARG_SUBSET])) {
                Object[] array = asCons(args[CHARSET_ARG_SUBSET]).toArray();
                Object parent = array[0];
                ELispCharset parentCharset = getCharset(parent);
                int parentMinCode = asInt(array[1]);
                int parentMaxCode = asInt(array[2]);
                int parentCodeOffset = asInt(array[3]);
                attrs.set(CHARSET_ATTR_SUBSET, new ELispVector(new Object[]{
                        (long) parentCharset.id,
                        (long) parentMinCode,
                        (long) parentMaxCode,
                        (long) parentCodeOffset
                }));
                method = CharsetMethod.SUBSET;
                fastMap = Arrays.copyOf(parentCharset.fastMap, parentCharset.fastMap.length);
                minChar = parentCharset.minChar;
                maxChar = parentCharset.maxChar;
            } else if (!ELispSymbol.isNil(args[CHARSET_ARG_SUPERSET])) {
                ELispCons val = asCons(args[CHARSET_ARG_SUPERSET]);
                method = CharsetMethod.SUPERSET;
                minChar = Integer.MAX_VALUE;
                ELispCons.ListBuilder list = new ELispCons.ListBuilder();
                for (Object elt : val) {
                    ELispCharset parentCharset;
                    int offset;
                    if (elt instanceof ELispCons cons) {
                        parentCharset = getCharset(cons.car());
                        offset = asInt(cons.cdr());
                    } else {
                        parentCharset = getCharset(elt);
                        offset = 0;
                    }
                    list.add(new ELispCons((long) parentCharset.id, (long) offset));
                    minChar = Math.min(minChar, parentCharset.minChar);
                    maxChar = Math.max(maxChar, parentCharset.maxChar);
                    for (int i = 0; i < parentCharset.fastMap.length; i++) {
                        fastMap[i] |= parentCharset.fastMap[i];
                    }
                }
                attrs.set(CHARSET_ATTR_SUPERSET, list.build());
            } else {
                throw ELispSignals.error("None of :code-offset, :map, :parents are specified");
            }

            Object unifyMapVal = args[CHARSET_ARG_UNIFY_MAP];
            if (!ELispSymbol.isNil(unifyMapVal) && !(unifyMapVal instanceof ELispString)) {
                if (!(unifyMapVal instanceof ELispVector)) {
                    throw ELispSignals.wrongTypeArgument(VECTORP, unifyMapVal);
                }
            }
            attrs.set(CHARSET_ATTR_UNIFY_MAP, unifyMapVal);

            attrs.set(CHARSET_ATTR_PLIST, asCons(args[CHARSET_ARG_PLIST]));

            boolean newDefinitionP = !CHARSET_HASH_TABLE.containsKey(name);
            CHARSET_HASH_TABLE.put(name, attrs);
            int id;
            if (newDefinitionP) {
                id = CHARSET_LIST.size();
            } else {
                id = getCharset(name).id;
            }
            ELispCharset charset = new ELispCharset(
                    id,
                    attrs,
                    dimension,
                    codeSpaceMask,
                    codeSpace,
                    codeLinearP,
                    isoChar96,
                    asciiCompatibleP,
                    supplementaryP,
                    compactCodesP,
                    unifiedP,
                    isoFinal,
                    isoRevision,
                    emacsMule,
                    method,
                    minCode,
                    maxCode,
                    charIndexOffset,
                    minChar,
                    maxChar,
                    invalidCode,
                    fastMap,
                    codeOffset
            );
            if (newDefinitionP) {
                CHARSET_LIST.add(charset);
            } else {
                CHARSET_LIST.set(id, charset);
            }
            attrs.set(CHARSET_ATTR_ID, (long) id);

            if (method == CharsetMethod.MAP) {
                // TODO
                // load_charset (&charset, 0);
                // charset_table[id] = charset;
            }

            if (isoFinal >= 0) {
                // TODO
            }

            if (emacsMule >= 0) {
                // TODO
            }

            if (newDefinitionP) {
                ELispContext.CHARSET_LIST.setValue(new ELispCons(name, ELispContext.CHARSET_LIST.getValue()));
                // TODO: Vcharset_ordered_list
            }

            return false;
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
        public static boolean defineCharsetAlias(ELispSymbol alias, Object charset) {
            ELispVector attr = getCharsetAttr(charset);
            CHARSET_HASH_TABLE.put(alias, attr);
            ELispContext.CHARSET_LIST.setValue(new ELispCons(alias, ELispContext.CHARSET_LIST.getValue()));
            return false;
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
        public static ELispCons charsetPlist(Object charset) {
            ELispVector vector = getCharsetAttr(charset);
            return asCons(vector.get(CHARSET_ATTR_PLIST));
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
        public static Object setCharsetPlist(Object charset, Object plist) {
            getCharsetAttr(charset).set(CHARSET_ATTR_PLIST, asCons(plist));
            return plist;
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
        public static boolean unifyCharset(Object charset, Object unifyMap, Object deunify) {
            // TODO
            return false;
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
