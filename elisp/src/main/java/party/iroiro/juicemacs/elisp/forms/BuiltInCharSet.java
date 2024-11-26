package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCharset;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCharset.CharsetMethod;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// Charset-related operations, basically following `charset.c` from Emacs
///
/// ## What On Earth Is A Charset?
///
/// I really had a hard time understanding what Emacs is doing here. It seems
/// to me that I18N support in GNU Emacs largely comes from other Emacs variants,
/// and the documentation of XEmacs actually better describes what GNU Emacs tries
/// to achieve with its charsets.
///
/// Following [XEmacs' documentation on MULE (MUlti-Lingual Emacs)](http://www.xemacs.org/Documentation/21.5/html/lispref_64.html#Charsets),
/// there are a few key definitions to note:
///
/// - "A **character set** is essentially a set of related characters."
///
/// - "The definition of a character set will implicitly or explicitly give it an *ordering*,
///   a way of assigning a number to each character in the set. ...
///   The number assigned to any particular character is called the character’s **code point**."
///
/// - "Sometimes, a code point is not a single number, but instead a group of numbers,
///   called **position codes**. In such cases, the number of position codes required to index
///   a particular character in a character set is called the **dimension** of the character set."
///
/// - "An **encoding** is a way of numerically representing characters from one or more
///   character sets into a stream of like-sized numerical values called words – typically
///   8-bit bytes, but sometimes 16-bit or 32-bit quantities."
///
/// - "A general method of handling text using multiple character sets ... is defined
///   in the international standard **ISO 2022**."
///
/// - "Encodings are classified as either *modal* or *non-modal*."
///   - "In a **modal encoding**, there are multiple states that the encoding can be in,
///     and the interpretation of the values in the stream depends on the current global state
///     of the encoding."
///   - "Special values in the encoding, called **escape sequences**, are used to change the global state."
///   - "A **non-modal encoding** has no global state that extends past the character currently
///     being interpreted."
///   - "Non-modal encodings are further divided into *fixed-width* and *variable-width* formats."
///
/// - "The bytes in an 8-bit encoding are often referred to as *octets* rather than simply as bytes."
///
/// ### Personal Comments
///
/// - *Position codes*: To store human-readable characters on computer, there are usually two
///   layers of abstractions involved:
///
///   1. Charsets (`charset.c`): Numbering each character in the charset;
///   2. Encoding (`coding.c`): Design a way of encoding a said number into bytes.
///
///   For example, Unicode is the first layer and UTF-8/16/32(-BE/LE) is the second layer.
///   However, position codes, in my understanding, are ways of encodings that somehow combine
///   the two layers: the number assigned to a character *is* (more or less) the encoded bytes.
public class BuiltInCharSet extends ELispBuiltIns {

    public BuiltInCharSet() {
        CHARSET_HASH_TABLE.clear();
        CHARSET_LIST.clear();

        // Emacs initializes this in unify-charset, but we move it here.
        ELispGlobals.charUnifyTable.setValue(new ELispCharTable(false, 0));
    }

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharSetFactory.getFactories();
    }

    private static final ArrayList<ELispCharset> CHARSET_LIST = new ArrayList<>();
    private static final HashMap<ELispSymbol, ELispVector> CHARSET_HASH_TABLE = new HashMap<>();

    public static ELispCharset getCharset(Object symbol) {
        ELispVector vec = getCharsetAttr(symbol);
        //noinspection SequencedCollectionMethodCanBeUsed
        int index = asInt(vec.get(CHARSET_ID));
        return CHARSET_LIST.get(index);
    }

    public static ELispCharset getCharsetFromId(int id) {
        return CHARSET_LIST.get(id);
    }

    private static ELispVector getCharsetAttr(Object symbol) {
        //noinspection SuspiciousMethodCalls
        ELispVector vec = CHARSET_HASH_TABLE.get(symbol);
        if (vec == null) {
            throw ELispSignals.wrongTypeArgument(CHARSETP, symbol);
        }
        return vec;
    }

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
        public static boolean mapCharsetChars(Object function, ELispSymbol charset, Object arg, Object fromCode, Object toCode) {
            ELispCharset cs = getCharset(charset);
            int from = (int) notNilOr(fromCode, cs.minCode);
            int to = (int) notNilOr(toCode, cs.maxCode);
            cs.mapChars((range) -> BuiltInEval.FFuncall.funcall(function, new Object[]{
                    range, arg,
            }), from, to);
            return false;
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
            attrs.set(CHARSET_NAME, name);

            Object codeSpaceVal = args[CHARSET_ARG_CODE_SPACE];
            int[] codeSpace = new int[15];
            int dimension = 1;
            for (int i = 0, nchars = 1; true; i++) {
                // TODO: codeSpaceVal can be any sequence?
                int minByte = asRanged(BuiltInData.FAref.aref(codeSpaceVal, i * 2L), 0, 255);
                int maxByte = asRanged(BuiltInData.FAref.aref(codeSpaceVal, i * 2L + 1), minByte, 255);
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
            if (!isNil(dimensionVal)) {
                dimension = asRanged(dimensionVal, 1, 4);
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
                    for (int j = codeSpace[i * 4]; j <= codeSpace[i * 4 + 1]; j++) {
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
            if (!isNil(minCodeVal)) {
                long code = asRanged(consToUnsigned(minCodeVal, maxCode), minCode, maxCode);
                charIndexOffset = ELispCharset.codePointToIndex(code, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);
                minCode = code;
            }
            Object maxCodeVal = args[CHARSET_ARG_MAX_CODE];
            if (!isNil(maxCodeVal)) {
                maxCode = asRanged(consToUnsigned(maxCodeVal, maxCode), minCode, maxCode);
            }

            Object invalidVal = args[CHARSET_ARG_INVALID_CODE];
            long invalidCode;
            if (isNil(invalidVal)) {
                if (minCode > 0) {
                    invalidCode = 0;
                } else if (maxCode < Integer.toUnsignedLong(-1)) {
                    invalidCode = maxCode + 1;
                } else {
                    throw ELispSignals.error("Attribute :invalid-code must be specified");
                }
            } else {
                invalidCode = asRanged(invalidVal, 0, Integer.MAX_VALUE);
            }

            Object isoFinalVal = args[CHARSET_ARG_ISO_FINAL];
            int isoFinal;
            if (isNil(isoFinalVal)) {
                isoFinal = -1;
            } else {
                isoFinal = asRanged(isoFinalVal, '0', 127);
            }

            Object isoRevisionVal = args[CHARSET_ARG_ISO_REVISION];
            int isoRevision = isNil(isoRevisionVal) ? -1 : asRanged(isoRevisionVal, -1, 63);

            Object emacsMuleVal = args[CHARSET_ARG_EMACS_MULE_ID];
            int emacsMule;
            if (isNil(emacsMuleVal)) {
                emacsMule = -1;
            } else {
                emacsMule = asInt(emacsMuleVal);
                if ((0 < emacsMule && emacsMule <= 128) || 256 <= emacsMule) {
                    throw ELispSignals.error("Invalid emacs-mule-id: " + emacsMule);
                }
            }

            boolean asciiCompatibleP = !isNil(args[CHARSET_ARG_ASCII_COMPATIBLE_P]);
            boolean supplementaryP = !isNil(args[CHARSET_ARG_SUPPLEMENTARY_P]);

            byte[] fastMap = new byte[190];
            CharsetMethod method;
            long codeOffset = 0, minChar = 0, maxChar = 0;
            if (!isNil(args[CHARSET_ARG_CODE_OFFSET])) {
                Object val = args[CHARSET_ARG_CODE_OFFSET];
                int c = asInt(val);
                method = CharsetMethod.OFFSET;
                codeOffset = c;

                minChar = codeOffset + ELispCharset.codePointToIndex(minCode, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);
                maxChar = codeOffset + ELispCharset.codePointToIndex(maxCode, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);

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
            } else if (!isNil(args[CHARSET_ARG_MAP])) {
                Object val = args[CHARSET_ARG_MAP];
                attrs.set(CHARSET_MAP, val);
                method = CharsetMethod.MAP;
            } else if (!isNil(args[CHARSET_ARG_SUBSET])) {
                Object[] array = asCons(args[CHARSET_ARG_SUBSET]).toArray();
                Object parent = array[0];
                ELispCharset parentCharset = getCharset(parent);
                long parentMinCode = asLong(array[1]);
                long parentMaxCode = asLong(array[2]);
                long parentCodeOffset = asLong(array[3]);
                attrs.set(CHARSET_SUBSET, new ELispVector(new Object[]{
                        (long) parentCharset.id,
                        parentMinCode,
                        parentMaxCode,
                        parentCodeOffset
                }));
                method = CharsetMethod.SUBSET;
                fastMap = Arrays.copyOf(parentCharset.fastMap, parentCharset.fastMap.length);
                minChar = parentCharset.minChar;
                maxChar = parentCharset.maxChar;
            } else if (!isNil(args[CHARSET_ARG_SUPERSET])) {
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
                attrs.set(CHARSET_SUPERSET, list.build());
            } else {
                throw ELispSignals.error("None of :code-offset, :map, :parents are specified");
            }

            Object unifyMapVal = args[CHARSET_ARG_UNIFY_MAP];
            if (!isNil(unifyMapVal) && !(unifyMapVal instanceof ELispString)) {
                if (!(unifyMapVal instanceof ELispVector)) {
                    throw ELispSignals.wrongTypeArgument(VECTORP, unifyMapVal);
                }
            }
            attrs.set(CHARSET_UNIFY_MAP, unifyMapVal);

            attrs.set(CHARSET_PLIST, asCons(args[CHARSET_ARG_PLIST]));

            boolean newDefinitionP = !CHARSET_HASH_TABLE.containsKey(name);
            CHARSET_HASH_TABLE.put(name, attrs);
            int id;
            if (newDefinitionP) {
                id = CHARSET_LIST.size();
            } else {
                id = getCharset(name).id;
            }
            boolean compactCodesP = true;
            boolean unifiedP = false;
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
            attrs.set(CHARSET_ID, (long) id);

            if (method == CharsetMethod.MAP) {
                charset.load();
                // TODO
                // load_charset (&charset, 0);
                // charset_table[id] = charset;
            }

            if (isoFinal >= 0) {
                // TODO
                if (newDefinitionP) {
                    ELispGlobals.iso2022CharsetList.setValue(BuiltInFns.FNconc.nconc(
                            new Object[]{ELispGlobals.iso2022CharsetList.getValue(), new ELispCons((long) id)}
                    ));
                }
            }

            if (emacsMule >= 0) {
                // TODO
                if (newDefinitionP) {
                    ELispGlobals.emacsMuleCharsetList.setValue(BuiltInFns.FNconc.nconc(
                            new Object[]{ELispGlobals.emacsMuleCharsetList.getValue(), new ELispCons((long) id)}
                    ));
                }
            }

            if (newDefinitionP) {
                ELispGlobals.charsetList.setValue(new ELispCons(name, ELispGlobals.charsetList.getValue()));
                Object charsetOrderedList = ELispGlobals.charsetOrderedList.getValue();
                if (charset.supplementaryP) {
                    ELispGlobals.charsetOrderedList.setValue(BuiltInFns.FNconc.nconc(new Object[]{
                            charsetOrderedList,
                            new ELispCons((long) id),
                    }));
                } else {
                    @Nullable ELispCons prev = null;
                    ELispCons.ConsIterator i = asConsIter(charsetOrderedList);
                    while (i.hasNextCons()) {
                        ELispCons cons = i.nextCons();
                        ELispCharset cs = getCharsetFromId(asInt(cons.car()));
                        if (cs.supplementaryP) {
                            break;
                        }
                        prev = cons;
                    }
                    if (prev == null) {
                        ELispGlobals.charsetOrderedList.setValue(new ELispCons((long) id, charsetOrderedList));
                    } else {
                        prev.insertAfter((long) id);
                    }
                }
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
            return asCons(vector.get(CHARSET_PLIST));
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
            getCharsetAttr(charset).set(CHARSET_PLIST, asCons(plist));
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
        public static long decodeChar(Object charset, long codePoint) {
            ELispCharset cs = getCharset(charset);
            return cs.decodeChar(codePoint);
        }
        @Specialization
        public static long decodeCharCons(Object charset, ELispCons codePoint) {
            return decodeChar(charset, (long) asInt(codePoint.car()) << 16 | asInt(codePoint.cdr()));
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
        public static Object encodeChar(long ch, Object charset) {
            ELispCharset cs = getCharset(charset);
            long code = cs.encodeChar(ch);
            return code == -1 ? false : code;
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
        public static boolean setCharsetPriority(ELispSymbol charsets, Object[] args) {
            Object oldList = ELispGlobals.charsetOrderedList.getValue();
            oldList = isNil(oldList) ? false : BuiltInFns.FCopySequence.copySequenceList(asCons(oldList));
            ELispCons.ListBuilder newBuilder = new ELispCons.ListBuilder();

            long id = getCharset(charsets).id;
            newBuilder.add(id);
            oldList = BuiltInFns.FDelq.delq(id, oldList);
            for (Object arg : args) {
                id = getCharset(arg).id;
                if (!isNil(BuiltInFns.FMemq.memq(id, oldList))) {
                    oldList = BuiltInFns.FDelq.delq(id, oldList);
                    newBuilder.add(id);
                }
            }
            ELispGlobals.charsetOrderedList.setValue(newBuilder.buildWithCdr(oldList));
            // TODO: Set other global variables
            return false;
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
