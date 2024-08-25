package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.Iterator;
import java.util.List;

/**
 * Built-in functions from {@code src/comp.c}
 */
public class BuiltInFns extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFnsFactory.getFactories();
    }

    @ELispBuiltIn(name = "identity", minArgs = 1, maxArgs = 1, doc = "Return the ARGUMENT unchanged.")
    @GenerateNodeFactory
    public abstract static class FIdentity extends ELispBuiltInBaseNode {
        @Specialization
        public static Object identity(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "random", minArgs = 0, maxArgs = 1, doc = "Return a pseudo-random integer.\nBy default, return a fixnum; all fixnums are equally likely.\nWith positive integer LIMIT, return random integer in interval [0,LIMIT).\nWith argument t, set the random number seed from the system's entropy\npool if available, otherwise from less-random volatile data such as the time.\nWith a string argument, set the seed based on the string's contents.\n\nSee Info node `(elisp)Random Numbers' for more details.")
    @GenerateNodeFactory
    public abstract static class FRandom extends ELispBuiltInBaseNode {
        @Specialization
        public static Object random(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length", minArgs = 1, maxArgs = 1, doc = "Return the length of vector, list or string SEQUENCE.\nA byte-code function object is also allowed.\n\nIf the string contains multibyte characters, this is not necessarily\nthe number of bytes in the string; it is the number of characters.\nTo get the number of bytes, use `string-bytes'.\n\nIf the length of a list is being computed to compare to a (small)\nnumber, the `length<', `length>' and `length=' functions may be more\nefficient.")
    @GenerateNodeFactory
    public abstract static class FLength extends ELispBuiltInBaseNode {
        @Specialization
        public static Object length(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "safe-length", minArgs = 1, maxArgs = 1, doc = "Return the length of a list, but avoid error or infinite loop.\nThis function never gets an error.  If LIST is not really a list,\nit returns 0.  If LIST is circular, it returns an integer that is at\nleast the number of distinct elements.")
    @GenerateNodeFactory
    public abstract static class FSafeLength extends ELispBuiltInBaseNode {
        @Specialization
        public static Object safeLength(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length<", minArgs = 2, maxArgs = 2, doc = "Return non-nil if SEQUENCE is shorter than LENGTH.\nSee `length' for allowed values of SEQUENCE and how elements are\ncounted.")
    @GenerateNodeFactory
    public abstract static class FLengthLess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lengthLess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length>", minArgs = 2, maxArgs = 2, doc = "Return non-nil if SEQUENCE is longer than LENGTH.\nSee `length' for allowed values of SEQUENCE and how elements are\ncounted.")
    @GenerateNodeFactory
    public abstract static class FLengthGreater extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lengthGreater(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length=", minArgs = 2, maxArgs = 2, doc = "Return non-nil if SEQUENCE has length equal to LENGTH.\nSee `length' for allowed values of SEQUENCE and how elements are\ncounted.")
    @GenerateNodeFactory
    public abstract static class FLengthEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lengthEqual(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "proper-list-p", minArgs = 1, maxArgs = 1, doc = "Return OBJECT's length if it is a proper list, nil otherwise.\nA proper list is neither circular nor dotted (i.e., its last cdr is nil).")
    @GenerateNodeFactory
    public abstract static class FProperListP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object properListP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-bytes", minArgs = 1, maxArgs = 1, doc = "Return the number of bytes in STRING.\nIf STRING is multibyte, this may be greater than the length of STRING.")
    @GenerateNodeFactory
    public abstract static class FStringBytes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringBytes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-distance", minArgs = 2, maxArgs = 3, doc = "Return Levenshtein distance between STRING1 and STRING2.\nThe distance is the number of deletions, insertions, and substitutions\nrequired to transform STRING1 into STRING2.\nIf BYTECOMPARE is nil or omitted, compute distance in terms of characters.\nIf BYTECOMPARE is non-nil, compute distance in terms of bytes.\nLetter-case is significant, but text properties are ignored.")
    @GenerateNodeFactory
    public abstract static class FStringDistance extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringDistance(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-equal", minArgs = 2, maxArgs = 2, doc = "Return t if two strings have identical contents.\nCase is significant, but text properties are ignored.\nSymbols are also allowed; their print names are used instead.\n\nSee also `string-equal-ignore-case'.")
    @GenerateNodeFactory
    public abstract static class FStringEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringEqual(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "compare-strings", minArgs = 6, maxArgs = 7, doc = "Compare the contents of two strings, converting to multibyte if needed.\nThe arguments START1, END1, START2, and END2, if non-nil, are\npositions specifying which parts of STR1 or STR2 to compare.  In\nstring STR1, compare the part between START1 (inclusive) and END1\n\\(exclusive).  If START1 is nil, it defaults to 0, the beginning of\nthe string; if END1 is nil, it defaults to the length of the string.\nLikewise, in string STR2, compare the part between START2 and END2.\nLike in `substring', negative values are counted from the end.\n\nThe strings are compared by the numeric values of their characters.\nFor instance, STR1 is \"less than\" STR2 if its first differing\ncharacter has a smaller numeric value.  If IGNORE-CASE is non-nil,\ncharacters are converted to upper-case before comparing them.  Unibyte\nstrings are converted to multibyte for comparison.\n\nThe value is t if the strings (or specified portions) match.\nIf string STR1 is less, the value is a negative number N;\n  - 1 - N is the number of characters that match at the beginning.\nIf string STR1 is greater, the value is a positive number N;\n  N - 1 is the number of characters that match at the beginning.")
    @GenerateNodeFactory
    public abstract static class FCompareStrings extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compareStrings(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-lessp", minArgs = 2, maxArgs = 2, doc = "Return non-nil if STRING1 is less than STRING2 in lexicographic order.\nCase is significant.\nSymbols are also allowed; their print names are used instead.")
    @GenerateNodeFactory
    public abstract static class FStringLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringLessp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-version-lessp", minArgs = 2, maxArgs = 2, doc = "Return non-nil if S1 is less than S2, as version strings.\n\nThis function compares version strings S1 and S2:\n   1) By prefix lexicographically.\n   2) Then by version (similarly to version comparison of Debian's dpkg).\n      Leading zeros in version numbers are ignored.\n   3) If both prefix and version are equal, compare as ordinary strings.\n\nFor example, \\\"foo2.png\\\" compares less than \\\"foo12.png\\\".\nCase is significant.\nSymbols are also allowed; their print names are used instead.")
    @GenerateNodeFactory
    public abstract static class FStringVersionLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringVersionLessp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-collate-lessp", minArgs = 2, maxArgs = 4, doc = "Return t if first arg string is less than second in collation order.\nSymbols are also allowed; their print names are used instead.\n\nThis function obeys the conventions for collation order in your\nlocale settings.  For example, punctuation and whitespace characters\nmight be considered less significant for sorting:\n\n\\(sort \\\\='(\"11\" \"12\" \"1 1\" \"1 2\" \"1.1\" \"1.2\") \\\\='string-collate-lessp)\n  => (\"11\" \"1 1\" \"1.1\" \"12\" \"1 2\" \"1.2\")\n\nThe optional argument LOCALE, a string, overrides the setting of your\ncurrent locale identifier for collation.  The value is system\ndependent; a LOCALE \\\"en_US.UTF-8\\\" is applicable on POSIX systems,\nwhile it would be, e.g., \\\"enu_USA.1252\\\" on MS-Windows systems.\n\nIf IGNORE-CASE is non-nil, characters are converted to lower-case\nbefore comparing them.\n\nTo emulate Unicode-compliant collation on MS-Windows systems,\nbind `w32-collate-ignore-punctuation' to a non-nil value, since\nthe codeset part of the locale cannot be \\\"UTF-8\\\" on MS-Windows.\n\nSome operating systems do not implement correct collation (in specific\nlocale environments or at all).  Then, this functions falls back to\ncase-sensitive `string-lessp' and IGNORE-CASE argument is ignored.")
    @GenerateNodeFactory
    public abstract static class FStringCollateLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringCollateLessp(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-collate-equalp", minArgs = 2, maxArgs = 4, doc = "Return t if two strings have identical contents.\nSymbols are also allowed; their print names are used instead.\n\nThis function obeys the conventions for collation order in your locale\nsettings.  For example, characters with different coding points but\nthe same meaning might be considered as equal, like different grave\naccent Unicode characters:\n\n\\(string-collate-equalp (string ?\\\\uFF40) (string ?\\\\u1FEF))\n  => t\n\nThe optional argument LOCALE, a string, overrides the setting of your\ncurrent locale identifier for collation.  The value is system\ndependent; a LOCALE \\\"en_US.UTF-8\\\" is applicable on POSIX systems,\nwhile it would be \\\"enu_USA.1252\\\" on MS Windows systems.\n\nIf IGNORE-CASE is non-nil, characters are converted to lower-case\nbefore comparing them.\n\nTo emulate Unicode-compliant collation on MS-Windows systems,\nbind `w32-collate-ignore-punctuation' to a non-nil value, since\nthe codeset part of the locale cannot be \\\"UTF-8\\\" on MS-Windows.\n\nIf your system does not support a locale environment, this function\nbehaves like `string-equal', and in that case the IGNORE-CASE argument\nis ignored.\n\nDo NOT use this function to compare file names for equality.")
    @GenerateNodeFactory
    public abstract static class FStringCollateEqualp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringCollateEqualp(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "append", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Concatenate all the arguments and make the result a list.\nThe result is a list whose elements are the elements of all the arguments.\nEach argument may be a list, vector or string.\n\nAll arguments except the last argument are copied.  The last argument\nis just used as the tail of the new list.\n\nusage: (append &rest SEQUENCES)")
    @GenerateNodeFactory
    public abstract static class FAppend extends ELispBuiltInBaseNode {
        @Specialization
        public static Object append(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "concat", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Concatenate all the arguments and make the result a string.\nThe result is a string whose elements are the elements of all the arguments.\nEach argument may be a string or a list or vector of characters (integers).\n\nValues of the `composition' property of the result are not guaranteed\nto be `eq'.\nusage: (concat &rest SEQUENCES)")
    @GenerateNodeFactory
    public abstract static class FConcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object concat(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "vconcat", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Concatenate all the arguments and make the result a vector.\nThe result is a vector whose elements are the elements of all the arguments.\nEach argument may be a list, vector or string.\nusage: (vconcat &rest SEQUENCES)")
    @GenerateNodeFactory
    public abstract static class FVconcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object vconcat(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-sequence", minArgs = 1, maxArgs = 1, doc = "Return a copy of a list, vector, string, char-table or record.\nThe elements of a list, vector or record are not copied; they are\nshared with the original.  See Info node `(elisp) Sequence Functions'\nfor more details about this sharing and its effects.\nIf the original sequence is empty, this function may return\nthe same empty object instead of its copy.")
    @GenerateNodeFactory
    public abstract static class FCopySequence extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copySequence(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-make-multibyte", minArgs = 1, maxArgs = 1, doc = "Return the multibyte equivalent of STRING.\nIf STRING is unibyte and contains non-ASCII characters, the function\n`unibyte-char-to-multibyte' is used to convert each unibyte character\nto a multibyte character.  In this case, the returned string is a\nnewly created string with no text properties.  If STRING is multibyte\nor entirely ASCII, it is returned unchanged.  In particular, when\nSTRING is unibyte and entirely ASCII, the returned string is unibyte.\n\\(When the characters are all ASCII, Emacs primitives will treat the\nstring the same way whether it is unibyte or multibyte.)")
    @GenerateNodeFactory
    public abstract static class FStringMakeMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringMakeMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-make-unibyte", minArgs = 1, maxArgs = 1, doc = "Return the unibyte equivalent of STRING.\nMultibyte character codes above 255 are converted to unibyte\nby taking just the low 8 bits of each character's code.")
    @GenerateNodeFactory
    public abstract static class FStringMakeUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringMakeUnibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-as-unibyte", minArgs = 1, maxArgs = 1, doc = "Return a unibyte string with the same individual bytes as STRING.\nIf STRING is unibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties.\nIf STRING is multibyte and contains a character of charset\n`eight-bit', it is converted to the corresponding single byte.")
    @GenerateNodeFactory
    public abstract static class FStringAsUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringAsUnibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-as-multibyte", minArgs = 1, maxArgs = 1, doc = "Return a multibyte string with the same individual bytes as STRING.\nIf STRING is multibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties.\n\nIf STRING is unibyte and contains an individual 8-bit byte (i.e. not\npart of a correct utf-8 sequence), it is converted to the corresponding\nmultibyte character of charset `eight-bit'.\nSee also `string-to-multibyte'.\n\nBeware, this often doesn't really do what you think it does.\nIt is similar to (decode-coding-string STRING \\\\='utf-8-emacs).\nIf you're not sure, whether to use `string-as-multibyte' or\n`string-to-multibyte', use `string-to-multibyte'.")
    @GenerateNodeFactory
    public abstract static class FStringAsMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringAsMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-multibyte", minArgs = 1, maxArgs = 1, doc = "Return a multibyte string with the same individual chars as STRING.\nIf STRING is multibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties.\n\nIf STRING is unibyte and contains an 8-bit byte, it is converted to\nthe corresponding multibyte character of charset `eight-bit'.\n\nThis differs from `string-as-multibyte' by converting each byte of a correct\nutf-8 sequence to an eight-bit character, not just bytes that don't form a\ncorrect sequence.")
    @GenerateNodeFactory
    public abstract static class FStringToMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-unibyte", minArgs = 1, maxArgs = 1, doc = "Return a unibyte string with the same individual chars as STRING.\nIf STRING is unibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties,\nwhere each `eight-bit' character is converted to the corresponding byte.\nIf STRING contains a non-ASCII, non-`eight-bit' character,\nan error is signaled.")
    @GenerateNodeFactory
    public abstract static class FStringToUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToUnibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-alist", minArgs = 1, maxArgs = 1, doc = "Return a copy of ALIST.\nThis is an alist which represents the same mapping from objects to objects,\nbut does not share the alist structure with ALIST.\nThe objects mapped (cars and cdrs of elements of the alist)\nare shared, however.\nElements of ALIST that are not conses are also shared.")
    @GenerateNodeFactory
    public abstract static class FCopyAlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copyAlist(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "substring", minArgs = 1, maxArgs = 3, doc = "Return a new string whose contents are a substring of STRING.\nThe returned string consists of the characters between index FROM\n\\(inclusive) and index TO (exclusive) of STRING.  FROM and TO are\nzero-indexed: 0 means the first character of STRING.  Negative values\nare counted from the end of STRING.  If TO is nil, the substring runs\nto the end of STRING.\n\nThe STRING argument may also be a vector.  In that case, the return\nvalue is a new vector that contains the elements between index FROM\n\\(inclusive) and index TO (exclusive) of that vector argument.\n\nWith one argument, just copy STRING (with properties, if any).")
    @GenerateNodeFactory
    public abstract static class FSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substring(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "substring-no-properties", minArgs = 1, maxArgs = 3, doc = "Return a substring of STRING, without text properties.\nIt starts at index FROM and ends before TO.\nTO may be nil or omitted; then the substring runs to the end of STRING.\nIf FROM is nil or omitted, the substring starts at the beginning of STRING.\nIf FROM or TO is negative, it counts from the end.\n\nWith one argument, just copy STRING without its properties.")
    @GenerateNodeFactory
    public abstract static class FSubstringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substringNoProperties(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "take", minArgs = 2, maxArgs = 2, doc = "Return the first N elements of LIST.\nIf N is zero or negative, return nil.\nIf N is greater or equal to the length of LIST, return LIST (or a copy).")
    @GenerateNodeFactory
    public abstract static class FTake extends ELispBuiltInBaseNode {
        @Specialization
        public static Object take(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "ntake", minArgs = 2, maxArgs = 2, doc = "Modify LIST to keep only the first N elements.\nIf N is zero or negative, return nil.\nIf N is greater or equal to the length of LIST, return LIST unmodified.\nOtherwise, return LIST after truncating it.")
    @GenerateNodeFactory
    public abstract static class FNtake extends ELispBuiltInBaseNode {
        @Specialization
        public static Object ntake(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nthcdr", minArgs = 2, maxArgs = 2, doc = "Take cdr N times on LIST, return the result.")
    @GenerateNodeFactory
    public abstract static class FNthcdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nthcdr(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nth", minArgs = 2, maxArgs = 2, doc = "Return the Nth element of LIST.\nN counts from zero.  If LIST is not that long, nil is returned.")
    @GenerateNodeFactory
    public abstract static class FNth extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nth(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "elt", minArgs = 2, maxArgs = 2, doc = "Return element of SEQUENCE at index N.")
    @GenerateNodeFactory
    public abstract static class FElt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object elt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "member", minArgs = 2, maxArgs = 2, doc = "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.\nThe value is actually the tail of LIST whose car is ELT.")
    @GenerateNodeFactory
    public abstract static class FMember extends ELispBuiltInBaseNode {
        @Specialization
        public static Object member(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "memq", minArgs = 2, maxArgs = 2, doc = "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.\nThe value is actually the tail of LIST whose car is ELT.")
    @GenerateNodeFactory
    public abstract static class FMemq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memq(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "memql", minArgs = 2, maxArgs = 2, doc = "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.\nThe value is actually the tail of LIST whose car is ELT.")
    @GenerateNodeFactory
    public abstract static class FMemql extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memql(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "assq", minArgs = 2, maxArgs = 2, doc = "Return non-nil if KEY is `eq' to the car of an element of ALIST.\nThe value is actually the first element of ALIST whose car is KEY.\nElements of ALIST that are not conses are ignored.")
    @GenerateNodeFactory
    public abstract static class FAssq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assq(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "assoc", minArgs = 2, maxArgs = 3, doc = "Return non-nil if KEY is equal to the car of an element of ALIST.\nThe value is actually the first element of ALIST whose car equals KEY.\n\nEquality is defined by the function TESTFN, defaulting to `equal'.\nTESTFN is called with 2 arguments: a car of an alist element and KEY.")
    @GenerateNodeFactory
    public abstract static class FAssoc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assoc(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rassq", minArgs = 2, maxArgs = 2, doc = "Return non-nil if KEY is `eq' to the cdr of an element of ALIST.\nThe value is actually the first element of ALIST whose cdr is KEY.")
    @GenerateNodeFactory
    public abstract static class FRassq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object rassq(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rassoc", minArgs = 2, maxArgs = 2, doc = "Return non-nil if KEY is `equal' to the cdr of an element of ALIST.\nThe value is actually the first element of ALIST whose cdr equals KEY.")
    @GenerateNodeFactory
    public abstract static class FRassoc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object rassoc(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delq", minArgs = 2, maxArgs = 2, doc = "Delete members of LIST which are `eq' to ELT, and return the result.\nMore precisely, this function skips any members `eq' to ELT at the\nfront of LIST, then removes members `eq' to ELT from the remaining\nsublist by modifying its list structure, then returns the resulting\nlist.\n\nWrite `(setq foo (delq element foo))' to be sure of correctly changing\nthe value of a list `foo'.  See also `remq', which does not modify the\nargument.")
    @GenerateNodeFactory
    public abstract static class FDelq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object delq(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete", minArgs = 2, maxArgs = 2, doc = "Delete members of SEQ which are `equal' to ELT, and return the result.\nSEQ must be a sequence (i.e. a list, a vector, or a string).\nThe return value is a sequence of the same type.\n\nIf SEQ is a list, this behaves like `delq', except that it compares\nwith `equal' instead of `eq'.  In particular, it may remove elements\nby altering the list structure.\n\nIf SEQ is not a list, deletion is never performed destructively;\ninstead this function creates and returns a new vector or string.\n\nWrite `(setq foo (delete element foo))' to be sure of correctly\nchanging the value of a sequence `foo'.  See also `remove', which\ndoes not modify the argument.")
    @GenerateNodeFactory
    public abstract static class FDelete extends ELispBuiltInBaseNode {
        @Specialization
        public static Object delete(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nreverse", minArgs = 1, maxArgs = 1, doc = "Reverse order of items in a list, vector or string SEQ.\nIf SEQ is a list, it should be nil-terminated.\nThis function may destructively modify SEQ to produce the value.")
    @GenerateNodeFactory
    public abstract static class FNreverse extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nreverse(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "reverse", minArgs = 1, maxArgs = 1, doc = "Return the reversed copy of list, vector, or string SEQ.\nSee also the function `nreverse', which is used more often.")
    @GenerateNodeFactory
    public abstract static class FReverse extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reverse(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sort", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Sort SEQ, stably, and return the sorted sequence.\nSEQ should be a list or vector.\nOptional arguments are specified as keyword/argument pairs.  The following\narguments are defined:\n\n:key FUNC -- FUNC is a function that takes a single element from SEQ and\n  returns the key value to be used in comparison.  If absent or nil,\n  `identity' is used.\n\n:lessp FUNC -- FUNC is a function that takes two arguments and returns\n  non-nil if the first element should come before the second.\n  If absent or nil, `value<' is used.\n\n:reverse BOOL -- if BOOL is non-nil, the sorting order implied by FUNC is\n  reversed.  This does not affect stability: equal elements still retain\n  their order in the input sequence.\n\n:in-place BOOL -- if BOOL is non-nil, SEQ is sorted in-place and returned.\n  Otherwise, a sorted copy of SEQ is returned and SEQ remains unmodified;\n  this is the default.\n\nFor compatibility, the calling convention (sort SEQ LESSP) can also be used;\nin this case, sorting is always done in-place.\n\nusage: (sort SEQ &key KEY LESSP REVERSE IN-PLACE)")
    @GenerateNodeFactory
    public abstract static class FSort extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sort(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "plist-get", minArgs = 2, maxArgs = 3, doc = "Extract a value from a property list.\nPLIST is a property list, which is a list of the form\n\\(PROP1 VALUE1 PROP2 VALUE2...).\n\nThis function returns the value corresponding to the given PROP, or\nnil if PROP is not one of the properties on the list.  The comparison\nwith PROP is done using PREDICATE, which defaults to `eq'.\n\nThis function doesn't signal an error if PLIST is invalid.")
    @GenerateNodeFactory
    public abstract static class FPlistGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistGet(ELispCons list, Object prop, Object predicate) {
            // TODO: Handle different predicate
            Iterator<Object> iterator = list.iterator();
            try {
                while (iterator.hasNext()) {
                    if (prop == iterator.next()) {
                        return iterator.next();
                    }
                    iterator.next();
                }
                return false;
            } catch (IllegalStateException e) {
                return false;
            }
        }
    }

    @ELispBuiltIn(name = "get", minArgs = 2, maxArgs = 2, doc = "Return the value of SYMBOL's PROPNAME property.\nThis is the last value stored with `(put SYMBOL PROPNAME VALUE)'.")
    @GenerateNodeFactory
    public abstract static class FGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object get(Object symbol, Object prop) {
            if (symbol instanceof ELispSymbol sym && sym.getProperties() instanceof ELispCons) {
                FPlistGet.plistGet((ELispCons) sym.getProperties(), prop, false);
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "plist-put", minArgs = 3, maxArgs = 4, doc = "Change value in PLIST of PROP to VAL.\nPLIST is a property list, which is a list of the form\n\\(PROP1 VALUE1 PROP2 VALUE2 ...).\n\nThe comparison with PROP is done using PREDICATE, which defaults to `eq'.\n\nIf PROP is already a property on the list, its value is set to VAL,\notherwise the new PROP VAL pair is added.  The new plist is returned;\nuse `(setq x (plist-put x prop val))' to be sure to use the new value.\nThe PLIST is modified by side effects.")
    @GenerateNodeFactory
    public abstract static class FPlistPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistPut(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "put", minArgs = 3, maxArgs = 3, doc = "Store SYMBOL's PROPNAME property with value VALUE.\nIt can be retrieved with `(get SYMBOL PROPNAME)'.")
    @GenerateNodeFactory
    public abstract static class FPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object put(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "plist-member", minArgs = 2, maxArgs = 3, doc = "Return non-nil if PLIST has the property PROP.\nPLIST is a property list, which is a list of the form\n\\(PROP1 VALUE1 PROP2 VALUE2 ...).\n\nThe comparison with PROP is done using PREDICATE, which defaults to\n`eq'.\n\nUnlike `plist-get', this allows you to distinguish between a missing\nproperty and a property with the value nil.\nThe value is actually the tail of PLIST whose car is PROP.")
    @GenerateNodeFactory
    public abstract static class FPlistMember extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistMember(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eql", minArgs = 2, maxArgs = 2, doc = "Return t if the two args are `eq' or are indistinguishable numbers.\nIntegers with the same value are `eql'.\nFloating-point values with the same sign, exponent and fraction are `eql'.\nThis differs from numeric comparison: (eql 0.0 -0.0) returns nil and\n\\(eql 0.0e+NaN 0.0e+NaN) returns t, whereas `=' does the opposite.")
    @GenerateNodeFactory
    public abstract static class FEql extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eql(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "equal", minArgs = 2, maxArgs = 2, doc = "Return t if two Lisp objects have similar structure and contents.\nThey must have the same data type.\nConses are compared by comparing the cars and the cdrs.\nVectors and strings are compared element by element.\nNumbers are compared via `eql', so integers do not equal floats.\n\\(Use `=' if you want integers and floats to be able to be equal.)\nSymbols must match exactly.")
    @GenerateNodeFactory
    public abstract static class FEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object equal(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "equal-including-properties", minArgs = 2, maxArgs = 2, doc = "Return t if two Lisp objects have similar structure and contents.\nThis is like `equal' except that it compares the text properties\nof strings.  (`equal' ignores text properties.)")
    @GenerateNodeFactory
    public abstract static class FEqualIncludingProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object equalIncludingProperties(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "value<", minArgs = 2, maxArgs = 2, doc = "Return non-nil if A precedes B in standard value order.\nA and B must have the same basic type.\nNumbers are compared with `<'.\nStrings and symbols are compared with `string-lessp'.\nLists, vectors, bool-vectors and records are compared lexicographically.\nMarkers are compared lexicographically by buffer and position.\nBuffers and processes are compared by name.\nOther types are considered unordered and the return value will be `nil'.")
    @GenerateNodeFactory
    public abstract static class FValuelt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object valuelt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "fillarray", minArgs = 2, maxArgs = 2, doc = "Store each element of ARRAY with ITEM.\nARRAY is a vector, string, char-table, or bool-vector.")
    @GenerateNodeFactory
    public abstract static class FFillarray extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fillarray(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clear-string", minArgs = 1, maxArgs = 1, doc = "Clear the contents of STRING.\nThis makes STRING unibyte and may change its length.")
    @GenerateNodeFactory
    public abstract static class FClearString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clearString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nconc", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Concatenate any number of lists by altering them.\nOnly the last argument is not altered, and need not be a list.\nusage: (nconc &rest LISTS)")
    @GenerateNodeFactory
    public abstract static class FNconc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nconc(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapconcat", minArgs = 2, maxArgs = 3, doc = "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.\nIn between each pair of results, stick in SEPARATOR.  Thus, \" \" as\n  SEPARATOR results in spaces between the values returned by FUNCTION.\n\nSEQUENCE may be a list, a vector, a bool-vector, or a string.\n\nOptional argument SEPARATOR must be a string, a vector, or a list of\ncharacters; nil stands for the empty string.\n\nFUNCTION must be a function of one argument, and must return a value\n  that is a sequence of characters: either a string, or a vector or\n  list of numbers that are valid character codepoints.")
    @GenerateNodeFactory
    public abstract static class FMapconcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapconcat(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapcar", minArgs = 2, maxArgs = 2, doc = "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.\nThe result is a list just as long as SEQUENCE.\nSEQUENCE may be a list, a vector, a bool-vector, or a string.")
    @GenerateNodeFactory
    public abstract static class FMapcar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapcar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapc", minArgs = 2, maxArgs = 2, doc = "Apply FUNCTION to each element of SEQUENCE for side effects only.\nUnlike `mapcar', don't accumulate the results.  Return SEQUENCE.\nSEQUENCE may be a list, a vector, a bool-vector, or a string.")
    @GenerateNodeFactory
    public abstract static class FMapc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapc(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapcan", minArgs = 2, maxArgs = 2, doc = "Apply FUNCTION to each element of SEQUENCE, and concatenate\nthe results by altering them (using `nconc').\nSEQUENCE may be a list, a vector, a bool-vector, or a string.")
    @GenerateNodeFactory
    public abstract static class FMapcan extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapcan(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "yes-or-no-p", minArgs = 1, maxArgs = 1, doc = "Ask user a yes-or-no question.\nReturn t if answer is yes, and nil if the answer is no.\n\nPROMPT is the string to display to ask the question; `yes-or-no-p'\nappends `yes-or-no-prompt' (default \\\"(yes or no) \\\") to it.  If\nPROMPT is a non-empty string, and it ends with a non-space character,\na space character will be appended to it.\n\nThe user must confirm the answer with RET, and can edit it until it\nhas been confirmed.\n\nIf the `use-short-answers' variable is non-nil, instead of asking for\n\\\"yes\\\" or \\\"no\\\", this function will ask for \\\"y\\\" or \\\"n\\\" (and\nignore the value of `yes-or-no-prompt').\n\nIf dialog boxes are supported, this function will use a dialog box\nif `use-dialog-box' is non-nil and the last input event was produced\nby a mouse, or by some window-system gesture, or via a menu.")
    @GenerateNodeFactory
    public abstract static class FYesOrNoP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object yesOrNoP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "load-average", minArgs = 0, maxArgs = 1, doc = "Return list of 1 minute, 5 minute and 15 minute load averages.\n\nEach of the three load averages is multiplied by 100, then converted\nto integer.\n\nWhen USE-FLOATS is non-nil, floats will be used instead of integers.\nThese floats are not multiplied by 100.\n\nIf the 5-minute or 15-minute load averages are not available, return a\nshortened list, containing only those averages which are available.\n\nAn error is thrown if the load average can't be obtained.  In some\ncases making it work would require Emacs being installed setuid or\nsetgid so that it can read kernel information, and that usually isn't\nadvisable.")
    @GenerateNodeFactory
    public abstract static class FLoadAverage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object loadAverage(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "featurep", minArgs = 1, maxArgs = 2, doc = "Return t if FEATURE is present in this Emacs.\n\nUse this to conditionalize execution of lisp code based on the\npresence or absence of Emacs or environment extensions.\nUse `provide' to declare that a feature is available.  This function\nlooks at the value of the variable `features'.  The optional argument\nSUBFEATURE can be used to check a specific subfeature of FEATURE.")
    @GenerateNodeFactory
    public abstract static class FFeaturep extends ELispBuiltInBaseNode {
        @Specialization
        public static Object featurep(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "provide", minArgs = 1, maxArgs = 2, doc = "Announce that FEATURE is a feature of the current Emacs.\nThe optional argument SUBFEATURES should be a list of symbols listing\nparticular subfeatures supported in this version of FEATURE.")
    @GenerateNodeFactory
    public abstract static class FProvide extends ELispBuiltInBaseNode {
        @Specialization
        public static Object provide(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "require", minArgs = 1, maxArgs = 3, doc = "If FEATURE is not already loaded, load it from FILENAME.\nIf FEATURE is not a member of the list `features', then the feature was\nnot yet loaded; so load it from file FILENAME.\n\nIf FILENAME is omitted, the printname of FEATURE is used as the file\nname, and `load' is called to try to load the file by that name, after\nappending the suffix `.elc', `.el', or the system-dependent suffix for\ndynamic module files, in that order; but the function will not try to\nload the file without any suffix.  See `get-load-suffixes' for the\ncomplete list of suffixes.\n\nTo find the file, this function searches the directories in `load-path'.\n\nIf the optional third argument NOERROR is non-nil, then, if\nthe file is not found, the function returns nil instead of signaling\nan error.  Normally the return value is FEATURE.\n\nThe normal messages issued by `load' at start and end of loading\nFILENAME are suppressed.")
    @GenerateNodeFactory
    public abstract static class FRequire extends ELispBuiltInBaseNode {
        @Specialization
        public static Object require(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widget-put", minArgs = 3, maxArgs = 3, doc = "In WIDGET, set PROPERTY to VALUE.\nThe value can later be retrieved with `widget-get'.")
    @GenerateNodeFactory
    public abstract static class FWidgetPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widgetPut(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widget-get", minArgs = 2, maxArgs = 2, doc = "In WIDGET, get the value of PROPERTY.\nThe value could either be specified when the widget was created, or\nlater with `widget-put'.")
    @GenerateNodeFactory
    public abstract static class FWidgetGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widgetGet(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widget-apply", minArgs = 2, maxArgs = 2, varArgs = true, doc = "Apply the value of WIDGET's PROPERTY to the widget itself.\nReturn the result of applying the value of PROPERTY to WIDGET.\nARGS are passed as extra arguments to the function.\nusage: (widget-apply WIDGET PROPERTY &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FWidgetApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widgetApply(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "locale-info", minArgs = 1, maxArgs = 1, doc = "Access locale data ITEM for the current C locale, if available.\nITEM should be one of the following:\n\n`codeset', returning the character set as a string (locale item CODESET);\n\n`days', returning a 7-element vector of day names (locale items DAY_n);\n\n`months', returning a 12-element vector of month names (locale items MON_n);\n\n`paper', returning a list of 2 integers (WIDTH HEIGHT) for the default\n  paper size, both measured in millimeters (locale items _NL_PAPER_WIDTH,\n  _NL_PAPER_HEIGHT).\n\nIf the system can't provide such information through a call to\n`nl_langinfo', or if ITEM isn't from the list above, return nil.\n\nSee also Info node `(libc)Locales'.\n\nThe data read from the system are decoded using `locale-coding-system'.")
    @GenerateNodeFactory
    public abstract static class FLocaleInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object localeInfo(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-encode-region", minArgs = 2, maxArgs = 3, doc = "Base64-encode the region between BEG and END.\nThe data in the region is assumed to represent bytes, not text.  If\nyou want to base64-encode text, the text has to be converted into data\nfirst by using `encode-coding-region' with the appropriate coding\nsystem first.\n\nReturn the length of the encoded data.\n\nOptional third argument NO-LINE-BREAK means do not break long lines\ninto shorter lines.")
    @GenerateNodeFactory
    public abstract static class FBase64EncodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64EncodeRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64url-encode-region", minArgs = 2, maxArgs = 3, doc = "Base64url-encode the region between BEG and END.\nReturn the length of the encoded text.\nOptional second argument NO-PAD means do not add padding char =.\n\nThis produces the URL variant of base 64 encoding defined in RFC 4648.")
    @GenerateNodeFactory
    public abstract static class FBase64urlEncodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64urlEncodeRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-encode-string", minArgs = 1, maxArgs = 2, doc = "Base64-encode STRING and return the result.\nOptional second argument NO-LINE-BREAK means do not break long lines\ninto shorter lines.")
    @GenerateNodeFactory
    public abstract static class FBase64EncodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64EncodeString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64url-encode-string", minArgs = 1, maxArgs = 2, doc = "Base64url-encode STRING and return the result.\nOptional second argument NO-PAD means do not add padding char =.\n\nThis produces the URL variant of base 64 encoding defined in RFC 4648.")
    @GenerateNodeFactory
    public abstract static class FBase64urlEncodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64urlEncodeString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-decode-region", minArgs = 2, maxArgs = 4, doc = "Base64-decode the region between BEG and END.\nReturn the length of the decoded data.\n\nNote that after calling this function, the data in the region will\nrepresent bytes, not text.  If you want to end up with text, you have\nto call `decode-coding-region' afterwards with an appropriate coding\nsystem.\n\nIf the region can't be decoded, signal an error and don't modify the buffer.\nOptional third argument BASE64URL determines whether to use the URL variant\nof the base 64 encoding, as defined in RFC 4648.\nIf optional fourth argument IGNORE-INVALID is non-nil invalid characters\nare ignored instead of signaling an error.")
    @GenerateNodeFactory
    public abstract static class FBase64DecodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64DecodeRegion(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-decode-string", minArgs = 1, maxArgs = 3, doc = "Base64-decode STRING and return the result as a string.\nOptional argument BASE64URL determines whether to use the URL variant of\nthe base 64 encoding, as defined in RFC 4648.\nIf optional third argument IGNORE-INVALID is non-nil invalid characters are\nignored instead of signaling an error.")
    @GenerateNodeFactory
    public abstract static class FBase64DecodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64DecodeString(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-eq", minArgs = 1, maxArgs = 1, doc = "Return an integer hash code for OBJ suitable for `eq'.\nIf (eq A B), then (= (sxhash-eq A) (sxhash-eq B)).\n\nHash codes are not guaranteed to be preserved across Emacs sessions.")
    @GenerateNodeFactory
    public abstract static class FSxhashEq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEq(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-eql", minArgs = 1, maxArgs = 1, doc = "Return an integer hash code for OBJ suitable for `eql'.\nIf (eql A B), then (= (sxhash-eql A) (sxhash-eql B)), but the opposite\nisn't necessarily true.\n\nHash codes are not guaranteed to be preserved across Emacs sessions.")
    @GenerateNodeFactory
    public abstract static class FSxhashEql extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEql(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-equal", minArgs = 1, maxArgs = 1, doc = "Return an integer hash code for OBJ suitable for `equal'.\nIf (equal A B), then (= (sxhash-equal A) (sxhash-equal B)), but the\nopposite isn't necessarily true.\n\nHash codes are not guaranteed to be preserved across Emacs sessions.")
    @GenerateNodeFactory
    public abstract static class FSxhashEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEqual(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-equal-including-properties", minArgs = 1, maxArgs = 1, doc = "Return an integer hash code for OBJ suitable for\n`equal-including-properties'.\nIf (sxhash-equal-including-properties A B), then\n(= (sxhash-equal-including-properties A) (sxhash-equal-including-properties B)).\n\nHash codes are not guaranteed to be preserved across Emacs sessions.")
    @GenerateNodeFactory
    public abstract static class FSxhashEqualIncludingProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEqualIncludingProperties(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-hash-table", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Create and return a new hash table.\n\nArguments are specified as keyword/argument pairs.  The following\narguments are defined:\n\n:test TEST -- TEST must be a symbol that specifies how to compare\nkeys.  Default is `eql'.  Predefined are the tests `eq', `eql', and\n`equal'.  User-supplied test and hash functions can be specified via\n`define-hash-table-test'.\n\n:size SIZE -- A hint as to how many elements will be put in the table.\nThe table will always grow as needed; this argument may help performance\nslightly if the size is known in advance but is never required.\n\n:weakness WEAK -- WEAK must be one of nil, t, `key', `value',\n`key-or-value', or `key-and-value'.  If WEAK is not nil, the table\nreturned is a weak table.  Key/value pairs are removed from a weak\nhash table when there are no non-weak references pointing to their\nkey, value, one of key or value, or both key and value, depending on\nWEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK\nis nil.\n\n:purecopy PURECOPY -- If PURECOPY is non-nil, the table can be copied\nto pure storage when Emacs is being dumped, making the contents of the\ntable read only. Any further changes to purified tables will result\nin an error.\n\nThe keywords arguments :rehash-threshold and :rehash-size are obsolete\nand ignored.\n\nusage: (make-hash-table &rest KEYWORD-ARGS)")
    @GenerateNodeFactory
    public abstract static class FMakeHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeHashTable(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-hash-table", minArgs = 1, maxArgs = 1, doc = "Return a copy of hash table TABLE.")
    @GenerateNodeFactory
    public abstract static class FCopyHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copyHashTable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-count", minArgs = 1, maxArgs = 1, doc = "Return the number of elements in TABLE.")
    @GenerateNodeFactory
    public abstract static class FHashTableCount extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableCount(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-rehash-size", minArgs = 1, maxArgs = 1, doc = "Return the rehash size of TABLE.\nThis function is for compatibility only; it returns a nominal value\nwithout current significance.")
    @GenerateNodeFactory
    public abstract static class FHashTableRehashSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableRehashSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-rehash-threshold", minArgs = 1, maxArgs = 1, doc = "Return the rehash threshold of TABLE.\nThis function is for compatibility only; it returns a nominal value\nwithout current significance.")
    @GenerateNodeFactory
    public abstract static class FHashTableRehashThreshold extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableRehashThreshold(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-size", minArgs = 1, maxArgs = 1, doc = "Return the current allocation size of TABLE.\n\nThis is probably not the function that you are looking for.  To get the\nnumber of entries in a table, use `hash-table-count' instead.\n\nThe returned value is the number of entries that TABLE can currently\nhold without growing, but since hash tables grow automatically, this\nnumber is rarely of interest.")
    @GenerateNodeFactory
    public abstract static class FHashTableSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-test", minArgs = 1, maxArgs = 1, doc = "Return the test TABLE uses.")
    @GenerateNodeFactory
    public abstract static class FHashTableTest extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableTest(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-weakness", minArgs = 1, maxArgs = 1, doc = "Return the weakness of TABLE.")
    @GenerateNodeFactory
    public abstract static class FHashTableWeakness extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableWeakness(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJ is a Lisp hash table object.")
    @GenerateNodeFactory
    public abstract static class FHashTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clrhash", minArgs = 1, maxArgs = 1, doc = "Clear hash table TABLE and return it.")
    @GenerateNodeFactory
    public abstract static class FClrhash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clrhash(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "gethash", minArgs = 2, maxArgs = 3, doc = "Look up KEY in TABLE and return its associated value.\nIf KEY is not found, return DFLT which defaults to nil.")
    @GenerateNodeFactory
    public abstract static class FGethash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gethash(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "puthash", minArgs = 3, maxArgs = 3, doc = "Associate KEY with VALUE in hash table TABLE.\nIf KEY is already present in table, replace its current value with\nVALUE.  In any case, return VALUE.")
    @GenerateNodeFactory
    public abstract static class FPuthash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object puthash(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "remhash", minArgs = 2, maxArgs = 2, doc = "Remove KEY from TABLE.")
    @GenerateNodeFactory
    public abstract static class FRemhash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object remhash(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "maphash", minArgs = 2, maxArgs = 2, doc = "Call FUNCTION for all entries in hash table TABLE.\nFUNCTION is called with two arguments, KEY and VALUE.\nIt should not alter TABLE in any way other than using `puthash' to\nset a new value for KEY, or `remhash' to remove KEY.\n`maphash' always returns nil.")
    @GenerateNodeFactory
    public abstract static class FMaphash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object maphash(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "define-hash-table-test", minArgs = 3, maxArgs = 3, doc = "Define a new hash table test with name NAME, a symbol.\n\nIn hash tables created with NAME specified as test, use TEST to\ncompare keys, and HASH for computing hash codes of keys.\n\nTEST must be a function taking two arguments and returning non-nil if\nboth arguments are the same.  HASH must be a function taking one\nargument and returning an object that is the hash code of the argument.\nIt should be the case that if (eq (funcall HASH x1) (funcall HASH x2))\nreturns nil, then (funcall TEST x1 x2) also returns nil.")
    @GenerateNodeFactory
    public abstract static class FDefineHashTableTest extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defineHashTableTest(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--hash-table-histogram", minArgs = 1, maxArgs = 1, doc = "Bucket size histogram of HASH-TABLE.  Internal use only.")
    @GenerateNodeFactory
    public abstract static class FInternalHashTableHistogram extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalHashTableHistogram(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--hash-table-buckets", minArgs = 1, maxArgs = 1, doc = "(KEY . HASH) in HASH-TABLE, grouped by bucket.\nInternal use only.")
    @GenerateNodeFactory
    public abstract static class FInternalHashTableBuckets extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalHashTableBuckets(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--hash-table-index-size", minArgs = 1, maxArgs = 1, doc = "Index size of HASH-TABLE.  Internal use only.")
    @GenerateNodeFactory
    public abstract static class FInternalHashTableIndexSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalHashTableIndexSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "secure-hash-algorithms", minArgs = 0, maxArgs = 0, doc = "Return a list of all the supported `secure-hash' algorithms.")
    @GenerateNodeFactory
    public abstract static class FSecureHashAlgorithms extends ELispBuiltInBaseNode {
        @Specialization
        public static Object secureHashAlgorithms() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "md5", minArgs = 1, maxArgs = 5, doc = "Return MD5 message digest of OBJECT, a buffer or string.\n\nA message digest is the string representation of the cryptographic checksum\nof a document, and the algorithm to calculate it is defined in RFC 1321.\nThe MD5 digest is 32-character long.\n\nThe two optional arguments START and END are character positions\nspecifying for which part of OBJECT the message digest should be\ncomputed.  If nil or omitted, the digest is computed for the whole\nOBJECT.\n\nThe MD5 message digest is computed from the result of encoding the\ntext in a coding system, not directly from the internal Emacs form of\nthe text.  The optional fourth argument CODING-SYSTEM specifies which\ncoding system to encode the text with.  It should be the same coding\nsystem that you used or will use when actually writing the text into a\nfile.\n\nIf CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If\nOBJECT is a buffer, the default for CODING-SYSTEM is whatever coding\nsystem would be chosen by default for writing this text into a file.\n\nIf OBJECT is a string, the most preferred coding system (see the\ncommand `prefer-coding-system') is used.\n\nIf NOERROR is non-nil, silently assume the `raw-text' coding if the\nguesswork fails.  Normally, an error is signaled in such case.\n\nNote that MD5 is not collision resistant and should not be used for\nanything security-related.  See `secure-hash' for alternatives.")
    @GenerateNodeFactory
    public abstract static class FMd5 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object md5(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "secure-hash", minArgs = 2, maxArgs = 5, doc = "Return the secure hash of OBJECT, a buffer or string.\nALGORITHM is a symbol specifying the hash to use:\n- md5    corresponds to MD5, produces a 32-character signature\n- sha1   corresponds to SHA-1, produces a 40-character signature\n- sha224 corresponds to SHA-2 (SHA-224), produces a 56-character signature\n- sha256 corresponds to SHA-2 (SHA-256), produces a 64-character signature\n- sha384 corresponds to SHA-2 (SHA-384), produces a 96-character signature\n- sha512 corresponds to SHA-2 (SHA-512), produces a 128-character signature\n\nThe two optional arguments START and END are positions specifying for\nwhich part of OBJECT to compute the hash.  If nil or omitted, uses the\nwhole OBJECT.\n\nThe full list of algorithms can be obtained with `secure-hash-algorithms'.\n\nIf BINARY is non-nil, returns a string in binary form.  In this case,\nthe function returns a unibyte string whose length is half the number\nof characters it returns when BINARY is nil.\n\nNote that MD5 and SHA-1 are not collision resistant and should not be\nused for anything security-related.  For these applications, use one\nof the other hash types instead, e.g. sha256 or sha512.")
    @GenerateNodeFactory
    public abstract static class FSecureHash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object secureHash(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-hash", minArgs = 0, maxArgs = 1, doc = "Return a hash of the contents of BUFFER-OR-NAME.\nThis hash is performed on the raw internal format of the buffer,\ndisregarding any coding systems.  If nil, use the current buffer.\n\nThis function is useful for comparing two buffers running in the same\nEmacs, but is not guaranteed to return the same hash between different\nEmacs versions.  It should be somewhat more efficient on larger\nbuffers than `secure-hash' is, and should not allocate more memory.\n\nIt should not be used for anything security-related.  See\n`secure-hash' for these applications.")
    @GenerateNodeFactory
    public abstract static class FBufferHash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferHash(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-line-statistics", minArgs = 0, maxArgs = 1, doc = "Return data about lines in BUFFER.\nThe data is returned as a list, and the first element is the number of\nlines in the buffer, the second is the length of the longest line, and\nthe third is the mean line length.  The lengths returned are in bytes, not\ncharacters.")
    @GenerateNodeFactory
    public abstract static class FBufferLineStatistics extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLineStatistics(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-search", minArgs = 2, maxArgs = 3, doc = "Search for the string NEEDLE in the string HAYSTACK.\nThe return value is the position of the first occurrence of NEEDLE in\nHAYSTACK, or nil if no match was found.\n\nThe optional START-POS argument says where to start searching in\nHAYSTACK and defaults to zero (start at the beginning).\nIt must be between zero and the length of HAYSTACK, inclusive.\n\nCase is always significant and text properties are ignored.")
    @GenerateNodeFactory
    public abstract static class FStringSearch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringSearch(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "object-intervals", minArgs = 1, maxArgs = 1, doc = "Return a copy of the text properties of OBJECT.\nOBJECT must be a buffer or a string.\n\nAltering this copy does not change the layout of the text properties\nin OBJECT.")
    @GenerateNodeFactory
    public abstract static class FObjectIntervals extends ELispBuiltInBaseNode {
        @Specialization
        public static Object objectIntervals(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "line-number-at-pos", minArgs = 0, maxArgs = 2, doc = "Return the line number at POSITION in the current buffer.\nIf POSITION is nil or omitted, it defaults to point's position in the\ncurrent buffer.\n\nIf the buffer is narrowed, the return value by default counts the lines\nfrom the beginning of the accessible portion of the buffer.  But if the\nsecond optional argument ABSOLUTE is non-nil, the value counts the lines\nfrom the absolute start of the buffer, disregarding the narrowing.")
    @GenerateNodeFactory
    public abstract static class FLineNumberAtPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lineNumberAtPos(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }
}
