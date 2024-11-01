package party.iroiro.juicemacs.elisp.forms;

import java.util.*;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.SEQUENCEP;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.SUBFEATURES;

import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringBuilderUTF16;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

/**
 * Built-in functions from {@code src/comp.c}
 */
public class BuiltInFns extends ELispBuiltIns {
    public BuiltInFns() {
        super(true);
    }

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFnsFactory.getFactories();
    }

    public static Iterator<?> iterateSequence(Object sequence) {
        if (ELispSymbol.isNil(sequence)) {
            return Collections.emptyIterator();
        }
        return switch (sequence) {
            case ELispCons cons -> cons.iterator();
            case ELispVector vector -> vector.iterator();
            case ELispString string -> string.iterator();
            case ELispBoolVector boolVector -> boolVector.iterator();
            default -> throw ELispSignals.wrongTypeArgument(SEQUENCEP, sequence);
        };
    }

    /**
     * <pre>
     * Return the ARGUMENT unchanged.
     * </pre>
     */
    @ELispBuiltIn(name = "identity", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIdentity extends ELispBuiltInBaseNode {
        @Specialization
        public static Void identity(Object argument) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a pseudo-random integer.
     * By default, return a fixnum; all fixnums are equally likely.
     * With positive integer LIMIT, return random integer in interval [0,LIMIT).
     * With argument t, set the random number seed from the system's entropy
     * pool if available, otherwise from less-random volatile data such as the time.
     * With a string argument, set the seed based on the string's contents.
     *
     * See Info node `(elisp)Random Numbers' for more details.
     * </pre>
     */
    @ELispBuiltIn(name = "random", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRandom extends ELispBuiltInBaseNode {
        @Specialization
        public static Void random(Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the length of vector, list or string SEQUENCE.
     * A byte-code function object is also allowed.
     *
     * If the string contains multibyte characters, this is not necessarily
     * the number of bytes in the string; it is the number of characters.
     * To get the number of bytes, use `string-bytes'.
     *
     * If the length of a list is being computed to compare to a (small)
     * number, the `length&lt;', `length&gt;' and `length=' functions may be more
     * efficient.
     * </pre>
     */
    @ELispBuiltIn(name = "length", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLength extends ELispBuiltInBaseNode {
        @Specialization
        public static long lengthNil(ELispSymbol sequence) {
            if (ELispSymbol.isNil(sequence)) {
                return 0;
            }
            throw ELispSignals.wrongTypeArgument(SEQUENCEP, sequence);
        }

        @Specialization
        public static long lengthCons(ELispCons sequence) {
            return sequence.size();
        }

        @Specialization
        public static long lengthVector(ELispVector sequence) {
            return sequence.size();
        }

        @Specialization
        public static long lengthString(ELispString sequence) {
            return sequence.codepointCount();
        }
    }

    /**
     * <pre>
     * Return the length of a list, but avoid error or infinite loop.
     * This function never gets an error.  If LIST is not really a list,
     * it returns 0.  If LIST is circular, it returns an integer that is at
     * least the number of distinct elements.
     * </pre>
     */
    @ELispBuiltIn(name = "safe-length", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSafeLength extends ELispBuiltInBaseNode {
        @Specialization
        public static Void safeLength(Object list) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if SEQUENCE is shorter than LENGTH.
     * See `length' for allowed values of SEQUENCE and how elements are
     * counted.
     * </pre>
     */
    @ELispBuiltIn(name = "length<", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLengthLess extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lengthLess(Object sequence, Object length) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if SEQUENCE is longer than LENGTH.
     * See `length' for allowed values of SEQUENCE and how elements are
     * counted.
     * </pre>
     */
    @ELispBuiltIn(name = "length>", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLengthGreater extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lengthGreater(Object sequence, Object length) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if SEQUENCE has length equal to LENGTH.
     * See `length' for allowed values of SEQUENCE and how elements are
     * counted.
     * </pre>
     */
    @ELispBuiltIn(name = "length=", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLengthEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean lengthEqual(ELispCons sequence, long length) {
            return sequence.size() == length;
        }
    }

    /**
     * <pre>
     * Return OBJECT's length if it is a proper list, nil otherwise.
     * A proper list is neither circular nor dotted (i.e., its last cdr is nil).
     * </pre>
     */
    @ELispBuiltIn(name = "proper-list-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProperListP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void properListP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of bytes in STRING.
     * If STRING is multibyte, this may be greater than the length of STRING.
     * </pre>
     */
    @ELispBuiltIn(name = "string-bytes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringBytes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringBytes(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return Levenshtein distance between STRING1 and STRING2.
     * The distance is the number of deletions, insertions, and substitutions
     * required to transform STRING1 into STRING2.
     * If BYTECOMPARE is nil or omitted, compute distance in terms of characters.
     * If BYTECOMPARE is non-nil, compute distance in terms of bytes.
     * Letter-case is significant, but text properties are ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "string-distance", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FStringDistance extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringDistance(Object string1, Object string2, Object bytecompare) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if two strings have identical contents.
     * Case is significant, but text properties are ignored.
     * Symbols are also allowed; their print names are used instead.
     *
     * See also `string-equal-ignore-case'.
     * </pre>
     */
    @ELispBuiltIn(name = "string-equal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringEqual extends ELispBuiltInBaseNode {
        // TODO: Handle symbols
        @Specialization
        public static boolean stringEqual(ELispString s1, ELispString s2) {
            return s1.lispEquals(s2);
        }
    }

    /**
     * <pre>
     * Compare the contents of two strings, converting to multibyte if needed.
     * The arguments START1, END1, START2, and END2, if non-nil, are
     * positions specifying which parts of STR1 or STR2 to compare.  In
     * string STR1, compare the part between START1 (inclusive) and END1
     * \(exclusive).  If START1 is nil, it defaults to 0, the beginning of
     * the string; if END1 is nil, it defaults to the length of the string.
     * Likewise, in string STR2, compare the part between START2 and END2.
     * Like in `substring', negative values are counted from the end.
     *
     * The strings are compared by the numeric values of their characters.
     * For instance, STR1 is "less than" STR2 if its first differing
     * character has a smaller numeric value.  If IGNORE-CASE is non-nil,
     * characters are converted to upper-case before comparing them.  Unibyte
     * strings are converted to multibyte for comparison.
     *
     * The value is t if the strings (or specified portions) match.
     * If string STR1 is less, the value is a negative number N;
     *   - 1 - N is the number of characters that match at the beginning.
     * If string STR1 is greater, the value is a positive number N;
     *   N - 1 is the number of characters that match at the beginning.
     * </pre>
     */
    @ELispBuiltIn(name = "compare-strings", minArgs = 6, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompareStrings extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compareStrings(Object str1, Object start1, Object end1, Object str2, Object start2, Object end2, Object ignoreCase) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if STRING1 is less than STRING2 in lexicographic order.
     * Case is significant.
     * Symbols are also allowed; their print names are used instead.
     * </pre>
     */
    @ELispBuiltIn(name = "string-lessp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringLessp(Object string1, Object string2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if S1 is less than S2, as version strings.
     *
     * This function compares version strings S1 and S2:
     *    1) By prefix lexicographically.
     *    2) Then by version (similarly to version comparison of Debian's dpkg).
     *       Leading zeros in version numbers are ignored.
     *    3) If both prefix and version are equal, compare as ordinary strings.
     *
     * For example, \"foo2.png\" compares less than \"foo12.png\".
     * Case is significant.
     * Symbols are also allowed; their print names are used instead.
     * </pre>
     */
    @ELispBuiltIn(name = "string-version-lessp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringVersionLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringVersionLessp(Object string1, Object string2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if first arg string is less than second in collation order.
     * Symbols are also allowed; their print names are used instead.
     *
     * This function obeys the conventions for collation order in your
     * locale settings.  For example, punctuation and whitespace characters
     * might be considered less significant for sorting:
     *
     * \(sort \\='("11" "12" "1 1" "1 2" "1.1" "1.2") \\='string-collate-lessp)
     *   =&gt; ("11" "1 1" "1.1" "12" "1 2" "1.2")
     *
     * The optional argument LOCALE, a string, overrides the setting of your
     * current locale identifier for collation.  The value is system
     * dependent; a LOCALE \"en_US.UTF-8\" is applicable on POSIX systems,
     * while it would be, e.g., \"enu_USA.1252\" on MS-Windows systems.
     *
     * If IGNORE-CASE is non-nil, characters are converted to lower-case
     * before comparing them.
     *
     * To emulate Unicode-compliant collation on MS-Windows systems,
     * bind `w32-collate-ignore-punctuation' to a non-nil value, since
     * the codeset part of the locale cannot be \"UTF-8\" on MS-Windows.
     *
     * Some operating systems do not implement correct collation (in specific
     * locale environments or at all).  Then, this functions falls back to
     * case-sensitive `string-lessp' and IGNORE-CASE argument is ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "string-collate-lessp", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FStringCollateLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringCollateLessp(Object s1, Object s2, Object locale, Object ignoreCase) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if two strings have identical contents.
     * Symbols are also allowed; their print names are used instead.
     *
     * This function obeys the conventions for collation order in your locale
     * settings.  For example, characters with different coding points but
     * the same meaning might be considered as equal, like different grave
     * accent Unicode characters:
     *
     * \(string-collate-equalp (string ?\\uFF40) (string ?\\u1FEF))
     *   =&gt; t
     *
     * The optional argument LOCALE, a string, overrides the setting of your
     * current locale identifier for collation.  The value is system
     * dependent; a LOCALE \"en_US.UTF-8\" is applicable on POSIX systems,
     * while it would be \"enu_USA.1252\" on MS Windows systems.
     *
     * If IGNORE-CASE is non-nil, characters are converted to lower-case
     * before comparing them.
     *
     * To emulate Unicode-compliant collation on MS-Windows systems,
     * bind `w32-collate-ignore-punctuation' to a non-nil value, since
     * the codeset part of the locale cannot be \"UTF-8\" on MS-Windows.
     *
     * If your system does not support a locale environment, this function
     * behaves like `string-equal', and in that case the IGNORE-CASE argument
     * is ignored.
     *
     * Do NOT use this function to compare file names for equality.
     * </pre>
     */
    @ELispBuiltIn(name = "string-collate-equalp", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FStringCollateEqualp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringCollateEqualp(Object s1, Object s2, Object locale, Object ignoreCase) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Concatenate all the arguments and make the result a list.
     * The result is a list whose elements are the elements of all the arguments.
     * Each argument may be a list, vector or string.
     *
     * All arguments except the last argument are copied.  The last argument
     * is just used as the tail of the new list.  If the last argument is not
     * a list, this results in a dotted list.
     *
     * As an exception, if all the arguments except the last are nil, and the
     * last argument is not a list, the return value is that last argument
     * unaltered, not a list.
     *
     * usage: (append &amp;rest SEQUENCES)
     * </pre>
     */
    @ELispBuiltIn(name = "append", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FAppend extends ELispBuiltInBaseNode {
        @Specialization
        public static Object append(Object[] sequences) {
            if (sequences.length == 0) {
                return false;
            }
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            for (int i = 0; i < sequences.length - 1; i++) {
                Iterator<?> iterator = iterateSequence(sequences[i]);
                while (iterator.hasNext()) {
                    builder.add(iterator.next());
                }
            }
            return builder.build(sequences[sequences.length - 1]);
        }
    }

    /**
     * <pre>
     * Concatenate all the arguments and make the result a string.
     * The result is a string whose elements are the elements of all the arguments.
     * Each argument may be a string or a list or vector of characters (integers).
     *
     * Values of the `composition' property of the result are not guaranteed
     * to be `eq'.
     * usage: (concat &amp;rest SEQUENCES)
     * </pre>
     */
    @ELispBuiltIn(name = "concat", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FConcat extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString concat(Object[] sequences) {
            TruffleStringBuilderUTF16 builder = TruffleStringBuilderUTF16.createUTF16();
            for (Object arg : sequences) {
                if (arg instanceof ELispString s) {
                    builder.appendStringUncached(s.toTruffleString());
                } else {
                    Iterator<?> i = iterateSequence(arg);
                    while (i.hasNext()) {
                        builder.appendCodePointUncached(asInt(i.next()));
                    }
                }
            }
            return new ELispString(builder.toStringUncached());
        }
    }

    /**
     * <pre>
     * Concatenate all the arguments and make the result a vector.
     * The result is a vector whose elements are the elements of all the arguments.
     * Each argument may be a list, vector or string.
     * usage: (vconcat &amp;rest SEQUENCES)
     * </pre>
     */
    @ELispBuiltIn(name = "vconcat", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FVconcat extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispVector vconcat(Object[] sequences) {
            ArrayList<Object> list = new ArrayList<>();
            for (Object sequence : sequences) {
                Iterator<?> i = iterateSequence(sequence);
                while (i.hasNext()) {
                    list.add(i.next());
                }
            }
            return new ELispVector(list);
        }
    }

    /**
     * <pre>
     * Return a copy of a list, vector, string, char-table or record.
     * The elements of a list, vector or record are not copied; they are
     * shared with the original.  See Info node `(elisp) Sequence Functions'
     * for more details about this sharing and its effects.
     * If the original sequence is empty, this function may return
     * the same empty object instead of its copy.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-sequence", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopySequence extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean copySequenceNil(ELispSymbol arg) {
            if (ELispSymbol.isNil(arg)) {
                return false;
            }
            throw ELispSignals.wrongTypeArgument(SEQUENCEP, arg);
        }

        @Specialization
        public static ELispCons copySequenceList(ELispCons arg) {
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            for (Object e : arg) {
                builder.add(e);
            }
            return asCons(builder.build());
        }

        @Specialization
        public static ELispVector copySequenceVector(ELispVector arg) {
            return new ELispVector(arg);
        }

        @Specialization
        public static ELispCharTable copySequenceCharTable(ELispCharTable arg) {
            return arg.copy();
        }

        @Specialization
        public static ELispBoolVector copySequenceBoolVec(ELispBoolVector arg) {
            return new ELispBoolVector(arg);
        }

        @Specialization
        public static ELispString copySequenceString(ELispString arg) {
            return new ELispString(arg.toString());
        }
    }

    /**
     * <pre>
     * Return the multibyte equivalent of STRING.
     * If STRING is unibyte and contains non-ASCII characters, the function
     * `unibyte-char-to-multibyte' is used to convert each unibyte character
     * to a multibyte character.  In this case, the returned string is a
     * newly created string with no text properties.  If STRING is multibyte
     * or entirely ASCII, it is returned unchanged.  In particular, when
     * STRING is unibyte and entirely ASCII, the returned string is unibyte.
     * \(When the characters are all ASCII, Emacs primitives will treat the
     * string the same way whether it is unibyte or multibyte.)
     * </pre>
     */
    @ELispBuiltIn(name = "string-make-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringMakeMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringMakeMultibyte(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the unibyte equivalent of STRING.
     * Multibyte character codes above 255 are converted to unibyte
     * by taking just the low 8 bits of each character's code.
     * </pre>
     */
    @ELispBuiltIn(name = "string-make-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringMakeUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringMakeUnibyte(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a unibyte string with the same individual bytes as STRING.
     * If STRING is unibyte, the result is STRING itself.
     * Otherwise it is a newly created string, with no text properties.
     * If STRING is multibyte and contains a character of charset
     * `eight-bit', it is converted to the corresponding single byte.
     * </pre>
     */
    @ELispBuiltIn(name = "string-as-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringAsUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringAsUnibyte(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a multibyte string with the same individual bytes as STRING.
     * If STRING is multibyte, the result is STRING itself.
     * Otherwise it is a newly created string, with no text properties.
     *
     * If STRING is unibyte and contains an individual 8-bit byte (i.e. not
     * part of a correct utf-8 sequence), it is converted to the corresponding
     * multibyte character of charset `eight-bit'.
     * See also `string-to-multibyte'.
     *
     * Beware, this often doesn't really do what you think it does.
     * It is similar to (decode-coding-string STRING \\='utf-8-emacs).
     * If you're not sure, whether to use `string-as-multibyte' or
     * `string-to-multibyte', use `string-to-multibyte'.
     * </pre>
     */
    @ELispBuiltIn(name = "string-as-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringAsMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringAsMultibyte(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a multibyte string with the same individual chars as STRING.
     * If STRING is multibyte, the result is STRING itself.
     * Otherwise it is a newly created string, with no text properties.
     *
     * If STRING is unibyte and contains an 8-bit byte, it is converted to
     * the corresponding multibyte character of charset `eight-bit'.
     *
     * This differs from `string-as-multibyte' by converting each byte of a correct
     * utf-8 sequence to an eight-bit character, not just bytes that don't form a
     * correct sequence.
     * </pre>
     */
    @ELispBuiltIn(name = "string-to-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString stringToMultibyte(ELispString string) {
            // TODO: Support unibyte?
            return string;
        }
    }

    /**
     * <pre>
     * Return a unibyte string with the same individual chars as STRING.
     * If STRING is unibyte, the result is STRING itself.
     * Otherwise it is a newly created string, with no text properties,
     * where each `eight-bit' character is converted to the corresponding byte.
     * If STRING contains a non-ASCII, non-`eight-bit' character,
     * an error is signaled.
     * </pre>
     */
    @ELispBuiltIn(name = "string-to-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringToUnibyte(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a copy of ALIST.
     * This is an alist which represents the same mapping from objects to objects,
     * but does not share the alist structure with ALIST.
     * The objects mapped (cars and cdrs of elements of the alist)
     * are shared, however.
     * Elements of ALIST that are not conses are also shared.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-alist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopyAlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copyAlist(Object alist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a new string whose contents are a substring of STRING.
     * The returned string consists of the characters between index FROM
     * \(inclusive) and index TO (exclusive) of STRING.  FROM and TO are
     * zero-indexed: 0 means the first character of STRING.  Negative values
     * are counted from the end of STRING.  If TO is nil, the substring runs
     * to the end of STRING.
     *
     * The STRING argument may also be a vector.  In that case, the return
     * value is a new vector that contains the elements between index FROM
     * \(inclusive) and index TO (exclusive) of that vector argument.
     *
     * With one argument, just copy STRING (with properties, if any).
     * </pre>
     */
    @ELispBuiltIn(name = "substring", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Void substringVector(ELispVector string, Object from, Object to) {
            throw new UnsupportedOperationException();
        }

        @Specialization
        public static ELispString substring(ELispString string, Object from, Object to) {
            TruffleString s = string.toTruffleString();
            int length = s.codePointLengthUncached(ELispString.ENCODING);
            int start;
            if (ELispSymbol.isNil(from)) {
                start = 0;
            } else {
                start = asInt(from);
                if (start < 0) {
                    start = length + start;
                }
            }
            int end;
            if (ELispSymbol.isNil(to)) {
                end = length;
            } else {
                end = asInt(to);
                if (end < 0) {
                    end = length + end;
                }
            }
            return new ELispString(s.substringUncached(start, end - start, ELispString.ENCODING, false));
        }
    }

    /**
     * <pre>
     * Return a substring of STRING, without text properties.
     * It starts at index FROM and ends before TO.
     * TO may be nil or omitted; then the substring runs to the end of STRING.
     * If FROM is nil or omitted, the substring starts at the beginning of STRING.
     * If FROM or TO is negative, it counts from the end.
     *
     * With one argument, just copy STRING without its properties.
     * </pre>
     */
    @ELispBuiltIn(name = "substring-no-properties", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSubstringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void substringNoProperties(Object string, Object from, Object to) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the first N elements of LIST.
     * If N is zero or negative, return nil.
     * If N is greater or equal to the length of LIST, return LIST (or a copy).
     * </pre>
     */
    @ELispBuiltIn(name = "take", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTake extends ELispBuiltInBaseNode {
        @Specialization
        public static Void take(Object n, Object list) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Modify LIST to keep only the first N elements.
     * If N is zero or negative, return nil.
     * If N is greater or equal to the length of LIST, return LIST unmodified.
     * Otherwise, return LIST after truncating it.
     * </pre>
     */
    @ELispBuiltIn(name = "ntake", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNtake extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ntake(Object n, Object list) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Take cdr N times on LIST, return the result.
     * </pre>
     */
    @ELispBuiltIn(name = "nthcdr", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNthcdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nthcdr(long n, Object list) {
            for (; n > 0; n--) {
                list = asCons(list).cdr();
            }
            return list;
        }
    }

    /**
     * <pre>
     * Return the Nth element of LIST.
     * N counts from zero.  If LIST is not that long, nil is returned.
     * </pre>
     */
    @ELispBuiltIn(name = "nth", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNth extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nth(long n, Object list) {
            if (ELispSymbol.isNil(list)) {
                return false;
            }
            try {
                return asCons(list).get((int) n);
            } catch (IndexOutOfBoundsException ignored) {
                return false;
            }
        }
    }

    /**
     * <pre>
     * Return element of SEQUENCE at index N.
     * </pre>
     */
    @ELispBuiltIn(name = "elt", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FElt extends ELispBuiltInBaseNode {
        // TODO: Support other sequences
        @Specialization
        public static Object eltNil(ELispSymbol sequence, long n) {
            if (ELispSymbol.isNil(sequence)) {
                return false;
            }
            throw ELispSignals.wrongTypeArgument(SEQUENCEP, sequence);
        }
        @Specialization
        public static Object eltCharTable(ELispCharTable sequence, long n) {
            return sequence.getChar((int) n);
        }
        @Specialization
        public static Object eltVec(ELispVector sequence, long n) {
            return sequence.get((int) n);
        }
        @Specialization
        public static Object elt(ELispCons sequence, long n) {
            try {
                return sequence.get((int) n);
            } catch (IndexOutOfBoundsException ignored) {
                return false;
            }
        }
    }

    /**
     * <pre>
     * Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
     * The value is actually the tail of LIST whose car is ELT.
     * </pre>
     */
    @ELispBuiltIn(name = "member", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMember extends ELispBuiltInBaseNode {
        @Specialization
        public static Object member(Object elt, Object list) {
            if (ELispSymbol.isNil(list)) {
                return false;
            }
            ELispCons.BrentTortoiseHareIterator iterator = asCons(list).listIterator(0);
            while (iterator.hasNext()) {
                if (FEqual.equal(iterator.currentCons().car(), elt)) {
                    return iterator.currentCons();
                }
                iterator.next();
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
     * The value is actually the tail of LIST whose car is ELT.
     * </pre>
     */
    @ELispBuiltIn(name = "memq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMemq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memq(Object elt, Object list) {
            if (ELispSymbol.isNil(list)) {
                return false;
            }
            ELispCons.BrentTortoiseHareIterator iterator = asCons(list).listIterator(0);
            while (iterator.hasNext()) {
                if (BuiltInData.FEq.eq(iterator.currentCons().car(), elt)) {
                    return iterator.currentCons();
                }
                iterator.next();
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
     * The value is actually the tail of LIST whose car is ELT.
     * </pre>
     */
    @ELispBuiltIn(name = "memql", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMemql extends ELispBuiltInBaseNode {
        @Specialization
        public static Void memql(Object elt, Object list) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if KEY is `eq' to the car of an element of ALIST.
     * The value is actually the first element of ALIST whose car is KEY.
     * Elements of ALIST that are not conses are ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "assq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAssq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assq(Object key, Object alist) {
            if (ELispSymbol.isNil(alist)) {
                return false;
            }
            for (Object e : asCons(alist)) {
                if (e instanceof ELispCons cons && BuiltInData.FEq.eq(key, cons.car())) {
                    return cons;
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return non-nil if KEY is equal to the car of an element of ALIST.
     * The value is actually the first element of ALIST whose car equals KEY.
     *
     * Equality is defined by the function TESTFN, defaulting to `equal'.
     * TESTFN is called with 2 arguments: a car of an alist element and KEY.
     * </pre>
     */
    @ELispBuiltIn(name = "assoc", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAssoc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assoc(Object key, Object alist, Object testfn) {
            if (ELispSymbol.isNil(testfn)) {
                return FAssq.assq(key, alist);
            }
            if (ELispSymbol.isNil(alist)) {
                return false;
            }
            for (Object e : asCons(alist)) {
                if (e instanceof ELispCons cons) {
                    Object p = BuiltInEval.FFuncall.funcall(testfn, new Object[]{cons.car(), key});
                    if (!ELispSymbol.isNil(p)) {
                        return cons;
                    }
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return non-nil if KEY is `eq' to the cdr of an element of ALIST.
     * The value is actually the first element of ALIST whose cdr is KEY.
     * </pre>
     */
    @ELispBuiltIn(name = "rassq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRassq extends ELispBuiltInBaseNode {
        @Specialization
        public static Void rassq(Object key, Object alist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if KEY is `equal' to the cdr of an element of ALIST.
     * The value is actually the first element of ALIST whose cdr equals KEY.
     * </pre>
     */
    @ELispBuiltIn(name = "rassoc", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRassoc extends ELispBuiltInBaseNode {
        @Specialization
        public static Void rassoc(Object key, Object alist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete members of LIST which are `eq' to ELT, and return the result.
     * More precisely, this function skips any members `eq' to ELT at the
     * front of LIST, then removes members `eq' to ELT from the remaining
     * sublist by modifying its list structure, then returns the resulting
     * list.
     *
     * Write `(setq foo (delq element foo))' to be sure of correctly changing
     * the value of a list `foo'.  See also `remq', which does not modify the
     * argument.
     * </pre>
     */
    @ELispBuiltIn(name = "delq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDelq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object delq(Object elt, Object list) {
            if (ELispSymbol.isNil(list)) {
                return false;
            }
            ELispCons cons = asCons(list);
            while (BuiltInData.FEq.eq(cons.car(), elt)) {
                Object cdr = cons.cdr();
                if (ELispSymbol.isNil(cdr)) {
                    return false;
                }
                cons = asCons(cdr);
            }
            ELispCons.BrentTortoiseHareIterator i = cons.listIterator(1);
            ELispCons prev = cons;
            while (i.hasNext()) {
                if (BuiltInData.FEq.eq(i.currentCons().car(), elt)) {
                    prev.setCdr(i.currentCons().cdr());
                } else {
                    prev = i.currentCons();
                }
                i.next();
            }
            return cons;
        }
    }

    /**
     * <pre>
     * Delete members of SEQ which are `equal' to ELT, and return the result.
     * SEQ must be a sequence (i.e. a list, a vector, or a string).
     * The return value is a sequence of the same type.
     *
     * If SEQ is a list, this behaves like `delq', except that it compares
     * with `equal' instead of `eq'.  In particular, it may remove elements
     * by altering the list structure.
     *
     * If SEQ is not a list, deletion is never performed destructively;
     * instead this function creates and returns a new vector or string.
     *
     * Write `(setq foo (delete element foo))' to be sure of correctly
     * changing the value of a sequence `foo'.  See also `remove', which
     * does not modify the argument.
     * </pre>
     */
    @ELispBuiltIn(name = "delete", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDelete extends ELispBuiltInBaseNode {
        @Specialization
        public static Void delete(Object elt, Object seq) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Reverse order of items in a list, vector or string SEQ.
     * If SEQ is a list, it should be nil-terminated.
     * This function may destructively modify SEQ to produce the value.
     * </pre>
     */
    @ELispBuiltIn(name = "nreverse", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNreverse extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nreverseNil(ELispSymbol seq) {
            if (ELispSymbol.isNil(seq)) {
                return false;
            }
            throw ELispSignals.wrongTypeArgument(SEQUENCEP, seq);
        }

        @Specialization
        public static ELispBoolVector nreverseBoolVec(ELispBoolVector seq) {
            return seq.reverse();
        }

        @Specialization
        public static ELispVector nreverseVec(ELispVector seq) {
            return seq.reverse();
        }

        @Specialization
        public static ELispString nreverseString(ELispString seq) {
            return seq.reverse();
        }

        @Specialization
        public static ELispCons nreverseList(ELispCons seq) {
            ELispCons head = new ELispCons(seq.car());
            if (ELispSymbol.isNil(seq.cdr())) {
                return head;
            }
            for (Object e : asCons(seq.cdr())) {
                ELispCons cons = new ELispCons(e);
                cons.setCdr(head);
                head = cons;
            }
            return head;
        }
    }

    /**
     * <pre>
     * Return the reversed copy of list, vector, or string SEQ.
     * See also the function `nreverse', which is used more often.
     * </pre>
     */
    @ELispBuiltIn(name = "reverse", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReverse extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean reverseNil(ELispSymbol seq) {
            return FNreverse.nreverseNil(seq);
        }
        @Specialization
        public static ELispBoolVector reverseBoolVec(ELispBoolVector seq) {
            return FNreverse.nreverseBoolVec(seq);
        }
        @Specialization
        public static ELispVector reverseVec(ELispVector seq) {
            return FNreverse.nreverseVec(seq);
        }
        @Specialization
        public static ELispString reverseString(ELispString seq) {
            return FNreverse.nreverseString(seq);
        }
        @Specialization
        public static ELispCons reverseList(ELispCons seq) {
            return FNreverse.nreverseList(seq);
        }
    }

    /**
     * <pre>
     * Sort SEQ, stably, and return the sorted sequence.
     * SEQ should be a list or vector.
     * Optional arguments are specified as keyword/argument pairs.  The following
     * arguments are defined:
     *
     * :key FUNC -- FUNC is a function that takes a single element from SEQ and
     *   returns the key value to be used in comparison.  If absent or nil,
     *   `identity' is used.
     *
     * :lessp FUNC -- FUNC is a function that takes two arguments and returns
     *   non-nil if the first element should come before the second.
     *   If absent or nil, `value&lt;' is used.
     *
     * :reverse BOOL -- if BOOL is non-nil, the sorting order implied by FUNC is
     *   reversed.  This does not affect stability: equal elements still retain
     *   their order in the input sequence.
     *
     * :in-place BOOL -- if BOOL is non-nil, SEQ is sorted in-place and returned.
     *   Otherwise, a sorted copy of SEQ is returned and SEQ remains unmodified;
     *   this is the default.
     *
     * For compatibility, the calling convention (sort SEQ LESSP) can also be used;
     * in this case, sorting is always done in-place.
     *
     * usage: (sort SEQ &amp;key KEY LESSP REVERSE IN-PLACE)
     * </pre>
     */
    @ELispBuiltIn(name = "sort", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FSort extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sort(Object seq, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Extract a value from a property list.
     * PLIST is a property list, which is a list of the form
     * \(PROP1 VALUE1 PROP2 VALUE2...).
     *
     * This function returns the value corresponding to the given PROP, or
     * nil if PROP is not one of the properties on the list.  The comparison
     * with PROP is done using PREDICATE, which defaults to `eq'.
     *
     * This function doesn't signal an error if PLIST is invalid.
     * </pre>
     */
    @ELispBuiltIn(name = "plist-get", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPlistGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistGet(Object plist, Object prop, Object predicate) {
            if (!(plist instanceof ELispCons cons)) {
                return false;
            }
            ELispSymbol eq = ELispSymbol.isNil(predicate) ? ELispContext.EQ : asSym(predicate);
            Iterator<Object> iterator = cons.iterator();
            try {
                Object[] args = new Object[2];
                args[0] = prop;
                while (iterator.hasNext()) {
                    args[1] = iterator.next();
                    if (!ELispSymbol.isNil(BuiltInEval.FFuncall.funcall(eq, args))) {
                        return iterator.next();
                    }
                    iterator.next();
                }
                return false;
            } catch (NoSuchElementException e) {
                return false;
            }
        }
    }

    /**
     * <pre>
     * Return the value of SYMBOL's PROPNAME property.
     * This is the last value stored with `(put SYMBOL PROPNAME VALUE)'.
     * </pre>
     */
    @ELispBuiltIn(name = "get", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object get(Object symbol, Object propname) {
            if (symbol instanceof ELispSymbol sym) {
                return sym.getProperty(propname);
            }
            return false;
        }
    }

    /**
     * <pre>
     * Change value in PLIST of PROP to VAL.
     * PLIST is a property list, which is a list of the form
     * \(PROP1 VALUE1 PROP2 VALUE2 ...).
     *
     * The comparison with PROP is done using PREDICATE, which defaults to `eq'.
     *
     * If PROP is already a property on the list, its value is set to VAL,
     * otherwise the new PROP VAL pair is added.  The new plist is returned;
     * use `(setq x (plist-put x prop val))' to be sure to use the new value.
     * The PLIST is modified by side effects.
     * </pre>
     */
    @ELispBuiltIn(name = "plist-put", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPlistPut extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCons plistPut(Object plist, Object prop, Object val, Object predicate) {
            if (ELispSymbol.isNil(predicate)) {
                return plistPut(plist, prop, val);
            }
            throw new UnsupportedOperationException();
        }

        private static ELispCons plistPut(Object plist, Object prop, Object val) {
            if (ELispSymbol.isNil(plist)) {
                return ELispCons.listOf(prop, val);
            }
            ELispCons list = asCons(plist);
            ELispCons.BrentTortoiseHareIterator i = list.listIterator(0);
            ELispCons tail;
            do {
                Object key = i.next();
                tail = i.currentCons();
                if (key.equals(prop)) {
                    tail.setCar(val);
                    return list;
                    }
                i.next();
            } while (i.hasNext());
            tail.setCdr(ELispCons.listOf(prop, val));
            return list;
        }
    }

    /**
     * <pre>
     * Store SYMBOL's PROPNAME property with value VALUE.
     * It can be retrieved with `(get SYMBOL PROPNAME)'.
     * </pre>
     */
    @ELispBuiltIn(name = "put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object put(ELispSymbol symbol, Object propname, Object value) {
            symbol.putProperty(propname, value);
            return value;
        }
    }

    /**
     * <pre>
     * Return non-nil if PLIST has the property PROP.
     * PLIST is a property list, which is a list of the form
     * \(PROP1 VALUE1 PROP2 VALUE2 ...).
     *
     * The comparison with PROP is done using PREDICATE, which defaults to
     * `eq'.
     *
     * Unlike `plist-get', this allows you to distinguish between a missing
     * property and a property with the value nil.
     * The value is actually the tail of PLIST whose car is PROP.
     * </pre>
     */
    @ELispBuiltIn(name = "plist-member", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPlistMember extends ELispBuiltInBaseNode {
        @Specialization
        public static Void plistMember(Object plist, Object prop, Object predicate) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if the two args are `eq' or are indistinguishable numbers.
     * Integers with the same value are `eql'.
     * Floating-point values with the same sign, exponent and fraction are `eql'.
     * This differs from numeric comparison: (eql 0.0 -0.0) returns nil and
     * \(eql 0.0e+NaN 0.0e+NaN) returns t, whereas `=' does the opposite.
     * </pre>
     */
    @ELispBuiltIn(name = "eql", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEql extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eql(Object obj1, Object obj2) {
            return BuiltInData.FEq.eq(obj1, obj2)
                    || (obj1 instanceof Double da && obj2 instanceof Double db &&
                    Double.doubleToRawLongBits(da) == Double.doubleToRawLongBits(db))
                    || (obj1 instanceof ELispBigNum ia && obj2 instanceof ELispBigNum ib && ia.lispEquals(ib));
        }
    }

    /**
     * <pre>
     * Return t if two Lisp objects have similar structure and contents.
     * They must have the same data type.
     * Conses are compared by comparing the cars and the cdrs.
     * Vectors and strings are compared element by element.
     * Numbers are compared via `eql', so integers do not equal floats.
     * \(Use `=' if you want integers and floats to be able to be equal.)
     * Symbols must match exactly.
     * </pre>
     */
    @ELispBuiltIn(name = "equal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean equal(Object o1, Object o2) {
            return switch (o1) {
                case Long l when o2 instanceof Long n -> l.equals(n);
                case Long l when o2 instanceof Double d -> d.equals(l.doubleValue());
                case Double d when o2 instanceof Long n -> d.equals(n.doubleValue());
                case Double d when o2 instanceof Double n -> d.equals(n);
                case ELispValue v -> v.lispEquals(o2);
                default -> BuiltInData.FEq.eq(o1, o2);
            };
        }
    }

    /**
     * <pre>
     * Return t if two Lisp objects have similar structure and contents.
     * This is like `equal' except that it compares the text properties
     * of strings.  (`equal' ignores text properties.)
     * </pre>
     */
    @ELispBuiltIn(name = "equal-including-properties", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEqualIncludingProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void equalIncludingProperties(Object o1, Object o2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if A precedes B in standard value order.
     * A and B must have the same basic type.
     * Numbers are compared with `&lt;'.
     * Strings and symbols are compared with `string-lessp'.
     * Lists, vectors, bool-vectors and records are compared lexicographically.
     * Markers are compared lexicographically by buffer and position.
     * Buffers and processes are compared by name.
     * Other types are considered unordered and the return value will be `nil'.
     * </pre>
     */
    @ELispBuiltIn(name = "value<", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FValuelt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void valuelt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Store each element of ARRAY with ITEM.
     * ARRAY is a vector, string, char-table, or bool-vector.
     * </pre>
     */
    @ELispBuiltIn(name = "fillarray", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFillarray extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fillarray(Object array, Object item) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Clear the contents of STRING.
     * This makes STRING unibyte and may change its length.
     * </pre>
     */
    @ELispBuiltIn(name = "clear-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClearString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void clearString(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Concatenate any number of lists by altering them.
     * Only the last argument is not altered, and need not be a list.
     * usage: (nconc &amp;rest LISTS)
     * </pre>
     */
    @ELispBuiltIn(name = "nconc", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FNconc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nconc(Object[] lists) {
            Object result = false;
            @Nullable Object prev = null;
            for (Object arg : lists) {
                if (ELispSymbol.isNil(arg)) {
                    continue;
                }
                if (prev == null) {
                    result = arg;
                } else {
                    asCons(prev).tail().setCdr(arg);
                }
                prev = arg;
            }
            return result;
        }
    }

    /**
     * <pre>
     * Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
     * In between each pair of results, stick in SEPARATOR.  Thus, " " as
     *   SEPARATOR results in spaces between the values returned by FUNCTION.
     *
     * SEQUENCE may be a list, a vector, a bool-vector, or a string.
     *
     * Optional argument SEPARATOR must be a string, a vector, or a list of
     * characters; nil stands for the empty string.
     *
     * FUNCTION must be a function of one argument, and must return a value
     *   that is a sequence of characters: either a string, or a vector or
     *   list of numbers that are valid character codepoints.
     * </pre>
     */
    @ELispBuiltIn(name = "mapconcat", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMapconcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mapconcat(Object function, Object sequence, Object separator) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
     * The result is a list just as long as SEQUENCE.
     * SEQUENCE may be a list, a vector, a bool-vector, or a string.
     * </pre>
     */
    @ELispBuiltIn(name = "mapcar", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapcar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapcar(Object function, Object sequence) {
            Iterator<?> i = iterateSequence(sequence);
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            while (i.hasNext()) {
                builder.add(BuiltInEval.FFuncall.funcall(function, new Object[]{i.next()}));
            }
            return builder.build();
        }
    }

    /**
     * <pre>
     * Apply FUNCTION to each element of SEQUENCE for side effects only.
     * Unlike `mapcar', don't accumulate the results.  Return SEQUENCE.
     * SEQUENCE may be a list, a vector, a bool-vector, or a string.
     * </pre>
     */
    @ELispBuiltIn(name = "mapc", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapc(Object function, Object sequence) {
            Iterator<?> i = iterateSequence(sequence);
            while (i.hasNext()) {
                BuiltInEval.FFuncall.funcall(function, new Object[]{i.next()});
            }
            return sequence;
        }
    }

    /**
     * <pre>
     * Apply FUNCTION to each element of SEQUENCE, and concatenate
     * the results by altering them (using `nconc').
     * SEQUENCE may be a list, a vector, a bool-vector, or a string.
     * </pre>
     */
    @ELispBuiltIn(name = "mapcan", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapcan extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mapcan(Object function, Object sequence) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Ask user a yes-or-no question.
     * Return t if answer is yes, and nil if the answer is no.
     *
     * PROMPT is the string to display to ask the question; `yes-or-no-p'
     * appends `yes-or-no-prompt' (default \"(yes or no) \") to it.  If
     * PROMPT is a non-empty string, and it ends with a non-space character,
     * a space character will be appended to it.
     *
     * The user must confirm the answer with RET, and can edit it until it
     * has been confirmed.
     *
     * If the `use-short-answers' variable is non-nil, instead of asking for
     * \"yes\" or \"no\", this function will ask for \"y\" or \"n\" (and
     * ignore the value of `yes-or-no-prompt').
     *
     * If dialog boxes are supported, this function will use a dialog box
     * if `use-dialog-box' is non-nil and the last input event was produced
     * by a mouse, or by some window-system gesture, or via a menu.
     * </pre>
     */
    @ELispBuiltIn(name = "yes-or-no-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FYesOrNoP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void yesOrNoP(Object prompt) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return list of 1 minute, 5 minute and 15 minute load averages.
     *
     * Each of the three load averages is multiplied by 100, then converted
     * to integer.
     *
     * When USE-FLOATS is non-nil, floats will be used instead of integers.
     * These floats are not multiplied by 100.
     *
     * If the 5-minute or 15-minute load averages are not available, return a
     * shortened list, containing only those averages which are available.
     *
     * An error is thrown if the load average can't be obtained.  In some
     * cases making it work would require Emacs being installed setuid or
     * setgid so that it can read kernel information, and that usually isn't
     * advisable.
     * </pre>
     */
    @ELispBuiltIn(name = "load-average", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLoadAverage extends ELispBuiltInBaseNode {
        @Specialization
        public static Void loadAverage(Object useFloats) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if FEATURE is present in this Emacs.
     *
     * Use this to conditionalize execution of lisp code based on the
     * presence or absence of Emacs or environment extensions.
     * Use `provide' to declare that a feature is available.  This function
     * looks at the value of the variable `features'.  The optional argument
     * SUBFEATURE can be used to check a specific subfeature of FEATURE.
     * </pre>
     */
    @ELispBuiltIn(name = "featurep", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFeaturep extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean featurep(ELispSymbol feature, Object subfeature) {
            Object isMem = FMemq.memq(feature, ELispContext.FEATURES.getValue());
            if (ELispSymbol.isNil(isMem)) {
                return !ELispSymbol.isNil(isMem);
            }
            if (ELispSymbol.isNil(subfeature)) {
                return true;
            }
            ELispSymbol feat = asSym(BuiltInData.FCar.car(isMem));
            return !ELispSymbol.isNil(FMemq.memq(subfeature, feat.getProperty(SUBFEATURES)));
        }
    }

    /**
     * <pre>
     * Announce that FEATURE is a feature of the current Emacs.
     * The optional argument SUBFEATURES should be a list of symbols listing
     * particular subfeatures supported in this version of FEATURE.
     * </pre>
     */
    @ELispBuiltIn(name = "provide", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FProvide extends ELispBuiltInBaseNode {
        @Specialization
        public static Object provide(ELispSymbol feature, Object subfeatures) {
            ELispSymbol features = ELispContext.FEATURES;
            Object isMem = FMemq.memq(feature, features.getValue());
            if (ELispSymbol.isNil(isMem)) {
                features.setValue(new ELispCons(feature, features.getValue()));
            }
            if (!ELispSymbol.isNil(subfeatures)) {
                feature.putProperty(SUBFEATURES, subfeatures);
            }
            // TODO: Run after-load-alist hooks
            return feature;
        }
    }

    /**
     * <pre>
     * If FEATURE is not already loaded, load it from FILENAME.
     * If FEATURE is not a member of the list `features', then the feature was
     * not yet loaded; so load it from file FILENAME.
     *
     * If FILENAME is omitted, the printname of FEATURE is used as the file
     * name, and `load' is called to try to load the file by that name, after
     * appending the suffix `.elc', `.el', or the system-dependent suffix for
     * dynamic module files, in that order; but the function will not try to
     * load the file without any suffix.  See `get-load-suffixes' for the
     * complete list of suffixes.
     *
     * To find the file, this function searches the directories in `load-path'.
     *
     * If the optional third argument NOERROR is non-nil, then, if
     * the file is not found, the function returns nil instead of signaling
     * an error.  Normally the return value is FEATURE.
     *
     * The normal messages issued by `load' at start and end of loading
     * FILENAME are suppressed.
     * </pre>
     */
    @ELispBuiltIn(name = "require", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FRequire extends ELispBuiltInBaseNode {
        @Specialization
        public boolean require(ELispSymbol feature, Object filename, Object noerror) {
            if (FFeaturep.featurep(feature, false)) {
                return true;
            }
            return BuiltInLRead.loadFile(ELispLanguage.get(this), filename);
        }
    }

    /**
     * <pre>
     * In WIDGET, set PROPERTY to VALUE.
     * The value can later be retrieved with `widget-get'.
     * </pre>
     */
    @ELispBuiltIn(name = "widget-put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FWidgetPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Void widgetPut(Object widget, Object property, Object value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * In WIDGET, get the value of PROPERTY.
     * The value could either be specified when the widget was created, or
     * later with `widget-put'.
     * </pre>
     */
    @ELispBuiltIn(name = "widget-get", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWidgetGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Void widgetGet(Object widget, Object property) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Apply the value of WIDGET's PROPERTY to the widget itself.
     * Return the result of applying the value of PROPERTY to WIDGET.
     * ARGS are passed as extra arguments to the function.
     * usage: (widget-apply WIDGET PROPERTY &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "widget-apply", minArgs = 2, maxArgs = 2, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FWidgetApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Void widgetApply(Object widget, Object property, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Access locale data ITEM for the current C locale, if available.
     * ITEM should be one of the following:
     *
     * `codeset', returning the character set as a string (locale item CODESET);
     *
     * `days', returning a 7-element vector of day names (locale items DAY_n);
     *
     * `months', returning a 12-element vector of month names (locale items MON_n);
     *
     * `paper', returning a list of 2 integers (WIDTH HEIGHT) for the default
     *   paper size, both measured in millimeters (locale items _NL_PAPER_WIDTH,
     *   _NL_PAPER_HEIGHT).
     *
     * If the system can't provide such information through a call to
     * `nl_langinfo', or if ITEM isn't from the list above, return nil.
     *
     * See also Info node `(libc)Locales'.
     *
     * The data read from the system are decoded using `locale-coding-system'.
     * </pre>
     */
    @ELispBuiltIn(name = "locale-info", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLocaleInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Void localeInfo(Object item) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Base64-encode the region between BEG and END.
     * The data in the region is assumed to represent bytes, not text.  If
     * you want to base64-encode text, the text has to be converted into data
     * first by using `encode-coding-region' with the appropriate coding
     * system first.
     *
     * Return the length of the encoded data.
     *
     * Optional third argument NO-LINE-BREAK means do not break long lines
     * into shorter lines.
     * </pre>
     */
    @ELispBuiltIn(name = "base64-encode-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBase64EncodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void base64EncodeRegion(Object beg, Object end, Object noLineBreak) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Base64url-encode the region between BEG and END.
     * Return the length of the encoded text.
     * Optional second argument NO-PAD means do not add padding char =.
     *
     * This produces the URL variant of base 64 encoding defined in RFC 4648.
     * </pre>
     */
    @ELispBuiltIn(name = "base64url-encode-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBase64urlEncodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void base64urlEncodeRegion(Object beg, Object end, Object noPad) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Base64-encode STRING and return the result.
     * Optional second argument NO-LINE-BREAK means do not break long lines
     * into shorter lines.
     * </pre>
     */
    @ELispBuiltIn(name = "base64-encode-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBase64EncodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void base64EncodeString(Object string, Object noLineBreak) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Base64url-encode STRING and return the result.
     * Optional second argument NO-PAD means do not add padding char =.
     *
     * This produces the URL variant of base 64 encoding defined in RFC 4648.
     * </pre>
     */
    @ELispBuiltIn(name = "base64url-encode-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBase64urlEncodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void base64urlEncodeString(Object string, Object noPad) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Base64-decode the region between BEG and END.
     * Return the length of the decoded data.
     *
     * Note that after calling this function, the data in the region will
     * represent bytes, not text.  If you want to end up with text, you have
     * to call `decode-coding-region' afterwards with an appropriate coding
     * system.
     *
     * If the region can't be decoded, signal an error and don't modify the buffer.
     * Optional third argument BASE64URL determines whether to use the URL variant
     * of the base 64 encoding, as defined in RFC 4648.
     * If optional fourth argument IGNORE-INVALID is non-nil invalid characters
     * are ignored instead of signaling an error.
     * </pre>
     */
    @ELispBuiltIn(name = "base64-decode-region", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FBase64DecodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void base64DecodeRegion(Object beg, Object end, Object base64url, Object ignoreInvalid) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Base64-decode STRING and return the result as a string.
     * Optional argument BASE64URL determines whether to use the URL variant of
     * the base 64 encoding, as defined in RFC 4648.
     * If optional third argument IGNORE-INVALID is non-nil invalid characters are
     * ignored instead of signaling an error.
     * </pre>
     */
    @ELispBuiltIn(name = "base64-decode-string", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBase64DecodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void base64DecodeString(Object string, Object base64url, Object ignoreInvalid) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an integer hash code for OBJ suitable for `eq'.
     * If (eq A B), then (= (sxhash-eq A) (sxhash-eq B)).
     *
     * Hash codes are not guaranteed to be preserved across Emacs sessions.
     * </pre>
     */
    @ELispBuiltIn(name = "sxhash-eq", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEq extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sxhashEq(Object obj) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an integer hash code for OBJ suitable for `eql'.
     * If (eql A B), then (= (sxhash-eql A) (sxhash-eql B)), but the opposite
     * isn't necessarily true.
     *
     * Hash codes are not guaranteed to be preserved across Emacs sessions.
     * </pre>
     */
    @ELispBuiltIn(name = "sxhash-eql", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEql extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sxhashEql(Object obj) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an integer hash code for OBJ suitable for `equal'.
     * If (equal A B), then (= (sxhash-equal A) (sxhash-equal B)), but the
     * opposite isn't necessarily true.
     *
     * Hash codes are not guaranteed to be preserved across Emacs sessions.
     * </pre>
     */
    @ELispBuiltIn(name = "sxhash-equal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sxhashEqual(Object obj) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an integer hash code for OBJ suitable for
     * `equal-including-properties'.
     * If (sxhash-equal-including-properties A B), then
     * (= (sxhash-equal-including-properties A) (sxhash-equal-including-properties B)).
     *
     * Hash codes are not guaranteed to be preserved across Emacs sessions.
     * </pre>
     */
    @ELispBuiltIn(name = "sxhash-equal-including-properties", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEqualIncludingProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sxhashEqualIncludingProperties(Object obj) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create and return a new hash table.
     *
     * Arguments are specified as keyword/argument pairs.  The following
     * arguments are defined:
     *
     * :test TEST -- TEST must be a symbol that specifies how to compare
     * keys.  Default is `eql'.  Predefined are the tests `eq', `eql', and
     * `equal'.  User-supplied test and hash functions can be specified via
     * `define-hash-table-test'.
     *
     * :size SIZE -- A hint as to how many elements will be put in the table.
     * The table will always grow as needed; this argument may help performance
     * slightly if the size is known in advance but is never required.
     *
     * :weakness WEAK -- WEAK must be one of nil, t, `key', `value',
     * `key-or-value', or `key-and-value'.  If WEAK is not nil, the table
     * returned is a weak table.  Key/value pairs are removed from a weak
     * hash table when there are no non-weak references pointing to their
     * key, value, one of key or value, or both key and value, depending on
     * WEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK
     * is nil.
     *
     * :purecopy PURECOPY -- If PURECOPY is non-nil, the table can be copied
     * to pure storage when Emacs is being dumped, making the contents of the
     * table read only. Any further changes to purified tables will result
     * in an error.
     *
     * The keywords arguments :rehash-threshold and :rehash-size are obsolete
     * and ignored.
     *
     * usage: (make-hash-table &amp;rest KEYWORD-ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "make-hash-table", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispHashtable makeHashTable(Object[] keywordArgs) {
            // TODO
            return new ELispHashtable();
        }
    }

    /**
     * <pre>
     * Return a copy of hash table TABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-hash-table", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopyHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copyHashTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of elements in TABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-count", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableCount extends ELispBuiltInBaseNode {
        @Specialization
        public static Void hashTableCount(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the rehash size of TABLE.
     * This function is for compatibility only; it returns a nominal value
     * without current significance.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-rehash-size", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableRehashSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void hashTableRehashSize(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the rehash threshold of TABLE.
     * This function is for compatibility only; it returns a nominal value
     * without current significance.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-rehash-threshold", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableRehashThreshold extends ELispBuiltInBaseNode {
        @Specialization
        public static Void hashTableRehashThreshold(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current allocation size of TABLE.
     *
     * This is probably not the function that you are looking for.  To get the
     * number of entries in a table, use `hash-table-count' instead.
     *
     * The returned value is the number of entries that TABLE can currently
     * hold without growing, but since hash tables grow automatically, this
     * number is rarely of interest.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-size", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void hashTableSize(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the test TABLE uses.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-test", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableTest extends ELispBuiltInBaseNode {
        @Specialization
        public static Void hashTableTest(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the weakness of TABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-weakness", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableWeakness extends ELispBuiltInBaseNode {
        @Specialization
        public static Void hashTableWeakness(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJ is a Lisp hash table object.
     * </pre>
     */
    @ELispBuiltIn(name = "hash-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean hashTableP(Object obj) {
            return obj instanceof ELispHashtable;
        }
    }

    /**
     * <pre>
     * Clear hash table TABLE and return it.
     * </pre>
     */
    @ELispBuiltIn(name = "clrhash", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClrhash extends ELispBuiltInBaseNode {
        @Specialization
        public static Void clrhash(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Look up KEY in TABLE and return its associated value.
     * If KEY is not found, return DFLT which defaults to nil.
     * </pre>
     */
    @ELispBuiltIn(name = "gethash", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGethash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gethash(Object key, ELispHashtable table, Object dflt) {
            return table.get(key, dflt);
        }
    }

    /**
     * <pre>
     * Associate KEY with VALUE in hash table TABLE.
     * If KEY is already present in table, replace its current value with
     * VALUE.  In any case, return VALUE.
     * </pre>
     */
    @ELispBuiltIn(name = "puthash", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPuthash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object puthash(Object key, Object value, ELispHashtable table) {
            table.put(key, value);
            return value;
        }
    }

    /**
     * <pre>
     * Remove KEY from TABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "remhash", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRemhash extends ELispBuiltInBaseNode {
        @Specialization
        public static Void remhash(Object key, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Call FUNCTION for all entries in hash table TABLE.
     * FUNCTION is called with two arguments, KEY and VALUE.
     * It should not alter TABLE in any way other than using `puthash' to
     * set a new value for KEY, or `remhash' to remove KEY.
     * `maphash' always returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "maphash", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMaphash extends ELispBuiltInBaseNode {
        @Specialization
        public static Void maphash(Object function, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Define a new hash table test with name NAME, a symbol.
     *
     * In hash tables created with NAME specified as test, use TEST to
     * compare keys, and HASH for computing hash codes of keys.
     *
     * TEST must be a function taking two arguments and returning non-nil if
     * both arguments are the same.  HASH must be a function taking one
     * argument and returning an object that is the hash code of the argument.
     * It should be the case that if (eq (funcall HASH x1) (funcall HASH x2))
     * returns nil, then (funcall TEST x1 x2) also returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "define-hash-table-test", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefineHashTableTest extends ELispBuiltInBaseNode {
        @Specialization
        public static Void defineHashTableTest(Object name, Object test, Object hash) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Bucket size histogram of HASH-TABLE.  Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--hash-table-histogram", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalHashTableHistogram extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalHashTableHistogram(Object hashTable) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * (KEY . HASH) in HASH-TABLE, grouped by bucket.
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--hash-table-buckets", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalHashTableBuckets extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalHashTableBuckets(Object hashTable) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Index size of HASH-TABLE.  Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--hash-table-index-size", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalHashTableIndexSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalHashTableIndexSize(Object hashTable) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of all the supported `secure-hash' algorithms.
     * </pre>
     */
    @ELispBuiltIn(name = "secure-hash-algorithms", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSecureHashAlgorithms extends ELispBuiltInBaseNode {
        @Specialization
        public static Void secureHashAlgorithms() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return MD5 message digest of OBJECT, a buffer or string.
     *
     * A message digest is the string representation of the cryptographic checksum
     * of a document, and the algorithm to calculate it is defined in RFC 1321.
     * The MD5 digest is 32-character long.
     *
     * The two optional arguments START and END are character positions
     * specifying for which part of OBJECT the message digest should be
     * computed.  If nil or omitted, the digest is computed for the whole
     * OBJECT.
     *
     * The MD5 message digest is computed from the result of encoding the
     * text in a coding system, not directly from the internal Emacs form of
     * the text.  The optional fourth argument CODING-SYSTEM specifies which
     * coding system to encode the text with.  It should be the same coding
     * system that you used or will use when actually writing the text into a
     * file.
     *
     * If CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If
     * OBJECT is a buffer, the default for CODING-SYSTEM is whatever coding
     * system would be chosen by default for writing this text into a file.
     *
     * If OBJECT is a string, the most preferred coding system (see the
     * command `prefer-coding-system') is used.
     *
     * If NOERROR is non-nil, silently assume the `raw-text' coding if the
     * guesswork fails.  Normally, an error is signaled in such case.
     *
     * Note that MD5 is not collision resistant and should not be used for
     * anything security-related.  See `secure-hash' for alternatives.
     * </pre>
     */
    @ELispBuiltIn(name = "md5", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMd5 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void md5(Object object, Object start, Object end, Object codingSystem, Object noerror) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the secure hash of OBJECT, a buffer or string.
     * ALGORITHM is a symbol specifying the hash to use:
     * - md5    corresponds to MD5, produces a 32-character signature
     * - sha1   corresponds to SHA-1, produces a 40-character signature
     * - sha224 corresponds to SHA-2 (SHA-224), produces a 56-character signature
     * - sha256 corresponds to SHA-2 (SHA-256), produces a 64-character signature
     * - sha384 corresponds to SHA-2 (SHA-384), produces a 96-character signature
     * - sha512 corresponds to SHA-2 (SHA-512), produces a 128-character signature
     *
     * The two optional arguments START and END are positions specifying for
     * which part of OBJECT to compute the hash.  If nil or omitted, uses the
     * whole OBJECT.
     *
     * The full list of algorithms can be obtained with `secure-hash-algorithms'.
     *
     * If BINARY is non-nil, returns a string in binary form.  In this case,
     * the function returns a unibyte string whose length is half the number
     * of characters it returns when BINARY is nil.
     *
     * Note that MD5 and SHA-1 are not collision resistant and should not be
     * used for anything security-related.  For these applications, use one
     * of the other hash types instead, e.g. sha256 or sha512.
     * </pre>
     */
    @ELispBuiltIn(name = "secure-hash", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FSecureHash extends ELispBuiltInBaseNode {
        @Specialization
        public static Void secureHash(Object algorithm, Object object, Object start, Object end, Object binary) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a hash of the contents of BUFFER-OR-NAME.
     * This hash is performed on the raw internal format of the buffer,
     * disregarding any coding systems.  If nil, use the current buffer.
     *
     * This function is useful for comparing two buffers running in the same
     * Emacs, but is not guaranteed to return the same hash between different
     * Emacs versions.  It should be somewhat more efficient on larger
     * buffers than `secure-hash' is, and should not allocate more memory.
     *
     * It should not be used for anything security-related.  See
     * `secure-hash' for these applications.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-hash", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferHash extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferHash(Object bufferOrName) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return data about lines in BUFFER.
     * The data is returned as a list, and the first element is the number of
     * lines in the buffer, the second is the length of the longest line, and
     * the third is the mean line length.  The lengths returned are in bytes, not
     * characters.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-line-statistics", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLineStatistics extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferLineStatistics(Object bufferOrName) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Search for the string NEEDLE in the string HAYSTACK.
     * The return value is the position of the first occurrence of NEEDLE in
     * HAYSTACK, or nil if no match was found.
     *
     * The optional START-POS argument says where to start searching in
     * HAYSTACK and defaults to zero (start at the beginning).
     * It must be between zero and the length of HAYSTACK, inclusive.
     *
     * Case is always significant and text properties are ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "string-search", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FStringSearch extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringSearch(Object needle, Object haystack, Object startPos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a copy of the text properties of OBJECT.
     * OBJECT must be a buffer or a string.
     *
     * Altering this copy does not change the layout of the text properties
     * in OBJECT.
     * </pre>
     */
    @ELispBuiltIn(name = "object-intervals", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObjectIntervals extends ELispBuiltInBaseNode {
        @Specialization
        public static Void objectIntervals(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the line number at POSITION in the current buffer.
     * If POSITION is nil or omitted, it defaults to point's position in the
     * current buffer.
     *
     * If the buffer is narrowed, the return value by default counts the lines
     * from the beginning of the accessible portion of the buffer.  But if the
     * second optional argument ABSOLUTE is non-nil, the value counts the lines
     * from the absolute start of the buffer, disregarding the narrowing.
     * </pre>
     */
    @ELispBuiltIn(name = "line-number-at-pos", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLineNumberAtPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lineNumberAtPos(Object position, Object absolute) {
            throw new UnsupportedOperationException();
        }
    }
}
