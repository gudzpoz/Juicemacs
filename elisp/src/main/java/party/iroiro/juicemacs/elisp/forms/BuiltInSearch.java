package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;
import static party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol.isNil;

public class BuiltInSearch extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInSearchFactory.getFactories();
    }

    public static Pattern compileEmacsRegExp(AbstractTruffleString eRegExp, boolean ignoreCase) {
        TruffleStringIterator iterator = eRegExp.createCodePointIteratorUncached(ELispString.ENCODING);
        StringBuilder builder = new StringBuilder();
        while (iterator.hasNext()) {
            int codepoint = iterator.nextUncached();
            switch (codepoint) {
                case '\\' -> {
                    int next = iterator.nextUncached();
                    switch (next) {
                        case '(', ')', '|', '{', '}' -> builder.appendCodePoint(next);
                        case 'w' -> builder.append("\\w");
                        case 'W' -> builder.append("\\W");
                        case 's', 'S', 'c', 'C' -> throw new UnsupportedOperationException();
                        case '`' -> builder.append("\\A");
                        case '\'' -> builder.append("\\z");
                        case '=' -> builder.append("(?<point>)");
                        case 'b' -> builder.append("\\b");
                        case 'B' -> builder.append("\\B");
                        case '<' -> builder.append("(?=\\w)");
                        case '>' -> builder.append("(?<=\\w)");
                        case '_' -> {
                            switch (iterator.nextUncached()) {
                                case '<' -> builder.append("(?=[^\\s\"';#()[\\]`,])");
                                case '>' -> builder.append("(?<=[^\\s\"';#()[\\]`,])");
                                default -> throw new IllegalArgumentException();
                            }
                        }
                        default -> {
                            if (Character.isDigit(next)) {
                                builder.append('\\').appendCodePoint(next);
                            } else {
                                throw new IllegalArgumentException();
                            }
                        }
                    }
                }
                case '(', ')', '|', '{', '}' -> builder.append('\\').appendCodePoint(codepoint);
                case '[' -> {
                    builder.append('[');
                    int next = iterator.nextUncached();
                    if (next != ':') {
                        builder.appendCodePoint(next);
                        continue;
                    }
                    StringBuilder clazz = new StringBuilder();
                    int c;
                    while ((c = iterator.nextUncached()) != ':') {
                        clazz.appendCodePoint(c);
                    }
                    builder.append(switch (clazz.toString()) {
                        // TODO: Use syntax-table & Unicode
                        case "alnum" -> "\\p{Alnum}";
                        case "alpha" -> "\\p{Alpha}";
                        case "ascii" -> "\\p{ASCII}";
                        case "blank" -> "\\p{Blank}";
                        case "cntrl" -> "\\p{Cntrl}";
                        case "digit" -> "\\p{Digit}";
                        case "graph" -> "\\p{Graph}";
                        case "lower" -> "\\p{Lower}";
                        case "multibyte" -> "&&[^\\p{IsLatin}]";
                        case "nonascii" -> "&&[^\\p{ASCII}]";
                        case "print" -> "\\p{Print}";
                        case "punct" -> "\\p{Punct}";
                        case "space" -> "\\p{javaWhitespace}";
                        case "unibyte" -> "\\p{IsLatin}";
                        case "upper" -> "\\p{Upper}";
                        case "word" -> "\\w";
                        case "xdigit" -> "\\p{XDigit}";
                        default -> throw new IllegalArgumentException();
                    });
                    if (iterator.nextUncached() != ']') {
                        throw new IllegalArgumentException();
                    }
                }
                default -> builder.appendCodePoint(codepoint);
            }
        }
        return Pattern.compile(builder.toString(), ignoreCase ? Pattern.CASE_INSENSITIVE : 0);
    }

    @ELispBuiltIn(name = "looking-at", minArgs = 1, maxArgs = 2, doc = "Return t if text after point matches regular expression REGEXP.\nBy default, this function modifies the match data that\n`match-beginning', `match-end' and `match-data' access.  If\nINHIBIT-MODIFY is non-nil, don't modify the match data.")
    @GenerateNodeFactory
    public abstract static class FLookingAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lookingAt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "posix-looking-at", minArgs = 1, maxArgs = 2, doc = "Return t if text after point matches REGEXP according to Posix rules.\nFind the longest match, in accordance with Posix regular expression rules.\n\nBy default, this function modifies the match data that\n`match-beginning', `match-end' and `match-data' access.  If\nINHIBIT-MODIFY is non-nil, don't modify the match data.")
    @GenerateNodeFactory
    public abstract static class FPosixLookingAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixLookingAt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-match", minArgs = 2, maxArgs = 4, doc = "Return index of start of first match for REGEXP in STRING, or nil.\nMatching ignores case if `case-fold-search' is non-nil.\nIf third arg START is non-nil, start search at that index in STRING.\n\nIf INHIBIT-MODIFY is non-nil, match data is not changed.\n\nIf INHIBIT-MODIFY is nil or missing, match data is changed, and\n`match-end' and `match-beginning' give indices of substrings matched\nby parenthesis constructs in the pattern.  You can use the function\n`match-string' to extract the substrings matched by the parenthesis\nconstructions in REGEXP.  For index of first char beyond the match, do\n(match-end 0).")
    @GenerateNodeFactory
    public abstract static class FStringMatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringMatch(ELispString pattern, ELispString string, Object start, boolean noModify) {
            Pattern regex = compileEmacsRegExp(pattern.value(), false); // TODO: case-fold-search
            Matcher matcher = regex.matcher(string.toString());
            if (matcher.find(isNil(start) ? 0 : (int) (long) (Long) start)) {
                // TODO: Update match data
                return (long) matcher.start();
            }
            return NIL;
        }
    }

    @ELispBuiltIn(name = "posix-string-match", minArgs = 2, maxArgs = 4, doc = "Return index of start of first match for Posix REGEXP in STRING, or nil.\nFind the longest match, in accord with Posix regular expression rules.\nCase is ignored if `case-fold-search' is non-nil in the current buffer.\n\nIf INHIBIT-MODIFY is non-nil, match data is not changed.\n\nIf INHIBIT-MODIFY is nil or missing, match data is changed, and\n`match-end' and `match-beginning' give indices of substrings matched\nby parenthesis constructs in the pattern.  You can use the function\n`match-string' to extract the substrings matched by the parenthesis\nconstructions in REGEXP.  For index of first char beyond the match, do\n(match-end 0).")
    @GenerateNodeFactory
    public abstract static class FPosixStringMatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixStringMatch(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "search-backward", minArgs = 1, maxArgs = 4, doc = "Search backward from point for STRING.\nSet point to the beginning of the occurrence found, and return point.\nAn optional second argument bounds the search; it is a buffer position.\n  The match found must not begin before that position.  A value of nil\n  means search to the beginning of the accessible portion of the buffer.\nOptional third argument, if t, means if fail just return nil (no error).\n  If not nil and not t, position at limit of search and return nil.\nOptional fourth argument COUNT, if a positive number, means to search\n  for COUNT successive occurrences.  If COUNT is negative, search\n  forward, instead of backward, for -COUNT occurrences.  A value of\n  nil means the same as 1.\nWith COUNT positive, the match found is the COUNTth to last one (or\n  last, if COUNT is 1 or nil) in the buffer located entirely before\n  the origin of the search; correspondingly with COUNT negative.\n\nSearch case-sensitivity is determined by the value of the variable\n`case-fold-search', which see.\n\nSee also the functions `match-beginning', `match-end' and `replace-match'.")
    @GenerateNodeFactory
    public abstract static class FSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object searchBackward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "search-forward", minArgs = 1, maxArgs = 4, doc = "Search forward from point for STRING.\nSet point to the end of the occurrence found, and return point.\nAn optional second argument bounds the search; it is a buffer position.\n  The match found must not end after that position.  A value of nil\n  means search to the end of the accessible portion of the buffer.\nOptional third argument, if t, means if fail just return nil (no error).\n  If not nil and not t, move to limit of search and return nil.\nOptional fourth argument COUNT, if a positive number, means to search\n  for COUNT successive occurrences.  If COUNT is negative, search\n  backward, instead of forward, for -COUNT occurrences.  A value of\n  nil means the same as 1.\nWith COUNT positive, the match found is the COUNTth one (or first,\n  if COUNT is 1 or nil) in the buffer located entirely after the\n  origin of the search; correspondingly with COUNT negative.\n\nSearch case-sensitivity is determined by the value of the variable\n`case-fold-search', which see.\n\nSee also the functions `match-beginning', `match-end' and `replace-match'.")
    @GenerateNodeFactory
    public abstract static class FSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object searchForward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "re-search-backward", minArgs = 1, maxArgs = 4, doc = "Search backward from point for regular expression REGEXP.\nThis function is almost identical to `re-search-forward', except that\nby default it searches backward instead of forward, and the sign of\nCOUNT also indicates exactly the opposite searching direction.\nSee `re-search-forward' for details.\n\nNote that searching backwards may give a shorter match than expected,\nbecause REGEXP is still matched in the forward direction.  See Info\nanchor `(elisp) re-search-backward' for details.")
    @GenerateNodeFactory
    public abstract static class FReSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reSearchBackward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "re-search-forward", minArgs = 1, maxArgs = 4, doc = "Search forward from point for regular expression REGEXP.\nSet point to the end of the occurrence found, and return point.\nThe optional second argument BOUND is a buffer position that bounds\n  the search.  The match found must not end after that position.  A\n  value of nil means search to the end of the accessible portion of\n  the buffer.\nThe optional third argument NOERROR indicates how errors are handled\n  when the search fails: if it is nil or omitted, emit an error; if\n  it is t, simply return nil and do nothing; if it is neither nil nor\n  t, move to the limit of search and return nil.\nThe optional fourth argument COUNT is a number that indicates the\n  search direction and the number of occurrences to search for.  If it\n  is positive, search forward for COUNT successive occurrences; if it\n  is negative, search backward, instead of forward, for -COUNT\n  occurrences.  A value of nil means the same as 1.\nWith COUNT positive/negative, the match found is the COUNTth/-COUNTth\n  one in the buffer located entirely after/before the origin of the\n  search.\n\nSearch case-sensitivity is determined by the value of the variable\n`case-fold-search', which see.\n\nSee also the functions `match-beginning', `match-end', `match-string',\nand `replace-match'.")
    @GenerateNodeFactory
    public abstract static class FReSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reSearchForward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "posix-search-backward", minArgs = 1, maxArgs = 4, doc = "Search backward from point for match for REGEXP according to Posix rules.\nFind the longest match in accord with Posix regular expression rules.\nSet point to the beginning of the occurrence found, and return point.\nAn optional second argument bounds the search; it is a buffer position.\n  The match found must not begin before that position.  A value of nil\n  means search to the beginning of the accessible portion of the buffer.\nOptional third argument, if t, means if fail just return nil (no error).\n  If not nil and not t, position at limit of search and return nil.\nOptional fourth argument COUNT, if a positive number, means to search\n  for COUNT successive occurrences.  If COUNT is negative, search\n  forward, instead of backward, for -COUNT occurrences.  A value of\n  nil means the same as 1.\nWith COUNT positive, the match found is the COUNTth to last one (or\n  last, if COUNT is 1 or nil) in the buffer located entirely before\n  the origin of the search; correspondingly with COUNT negative.\n\nSearch case-sensitivity is determined by the value of the variable\n`case-fold-search', which see.\n\nSee also the functions `match-beginning', `match-end', `match-string',\nand `replace-match'.")
    @GenerateNodeFactory
    public abstract static class FPosixSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixSearchBackward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "posix-search-forward", minArgs = 1, maxArgs = 4, doc = "Search forward from point for REGEXP according to Posix rules.\nFind the longest match in accord with Posix regular expression rules.\nSet point to the end of the occurrence found, and return point.\nAn optional second argument bounds the search; it is a buffer position.\n  The match found must not end after that position.  A value of nil\n  means search to the end of the accessible portion of the buffer.\nOptional third argument, if t, means if fail just return nil (no error).\n  If not nil and not t, move to limit of search and return nil.\nOptional fourth argument COUNT, if a positive number, means to search\n  for COUNT successive occurrences.  If COUNT is negative, search\n  backward, instead of forward, for -COUNT occurrences.  A value of\n  nil means the same as 1.\nWith COUNT positive, the match found is the COUNTth one (or first,\n  if COUNT is 1 or nil) in the buffer located entirely after the\n  origin of the search; correspondingly with COUNT negative.\n\nSearch case-sensitivity is determined by the value of the variable\n`case-fold-search', which see.\n\nSee also the functions `match-beginning', `match-end', `match-string',\nand `replace-match'.")
    @GenerateNodeFactory
    public abstract static class FPosixSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixSearchForward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "replace-match", minArgs = 1, maxArgs = 5, doc = "Replace text matched by last search with NEWTEXT.\nLeave point at the end of the replacement text.\n\nIf optional second arg FIXEDCASE is non-nil, do not alter the case of\nthe replacement text.  Otherwise, maybe capitalize the whole text, or\nmaybe just word initials, based on the replaced text.  If the replaced\ntext has only capital letters and has at least one multiletter word,\nconvert NEWTEXT to all caps.  Otherwise if all words are capitalized\nin the replaced text, capitalize each word in NEWTEXT.  Note that\nwhat exactly is a word is determined by the syntax tables in effect\nin the current buffer, and the variable `case-symbols-as-words'.\n\nIf optional third arg LITERAL is non-nil, insert NEWTEXT literally.\nOtherwise treat `\\\\' as special:\n  `\\\\&' in NEWTEXT means substitute original matched text.\n  `\\\\N' means substitute what matched the Nth `\\\\(...\\\\)'.\n       If Nth parens didn't match, substitute nothing.\n  `\\\\\\\\' means insert one `\\\\'.\n  `\\\\?' is treated literally\n       (for compatibility with `query-replace-regexp').\n  Any other character following `\\\\' signals an error.\nCase conversion does not apply to these substitutions.\n\nIf optional fourth argument STRING is non-nil, it should be a string\nto act on; this should be the string on which the previous match was\ndone via `string-match'.  In this case, `replace-match' creates and\nreturns a new string, made by copying STRING and replacing the part of\nSTRING that was matched (the original STRING itself is not altered).\n\nThe optional fifth argument SUBEXP specifies a subexpression;\nit says to replace just that subexpression with NEWTEXT,\nrather than replacing the entire matched text.\nThis is, in a vague sense, the inverse of using `\\\\N' in NEWTEXT;\n`\\\\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts\nNEWTEXT in place of subexp N.\nThis is useful only after a regular expression search or match,\nsince only regular expressions have distinguished subexpressions.")
    @GenerateNodeFactory
    public abstract static class FReplaceMatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object replaceMatch(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-beginning", minArgs = 1, maxArgs = 1, doc = "Return position of start of text matched by last search.\nSUBEXP, a number, specifies the parenthesized subexpression in the last\n  regexp for which to return the start position.\nValue is nil if SUBEXPth subexpression didn't match, or there were fewer\n  than SUBEXP subexpressions.\nSUBEXP zero means the entire text matched by the whole regexp or whole\n  string.\n\nReturn value is undefined if the last search failed.")
    @GenerateNodeFactory
    public abstract static class FMatchBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchBeginning(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-end", minArgs = 1, maxArgs = 1, doc = "Return position of end of text matched by last search.\nSUBEXP, a number, specifies the parenthesized subexpression in the last\n  regexp for which to return the start position.\nValue is nil if SUBEXPth subexpression didn't match, or there were fewer\n  than SUBEXP subexpressions.\nSUBEXP zero means the entire text matched by the whole regexp or whole\n  string.\n\nReturn value is undefined if the last search failed.")
    @GenerateNodeFactory
    public abstract static class FMatchEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchEnd(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-data", minArgs = 0, maxArgs = 3, doc = "Return a list of positions that record text matched by the last search.\nElement 2N of the returned list is the position of the beginning of the\nmatch of the Nth subexpression; it corresponds to `(match-beginning N)';\nelement 2N + 1 is the position of the end of the match of the Nth\nsubexpression; it corresponds to `(match-end N)'.  See `match-beginning'\nand `match-end'.\nIf the last search was on a buffer, all the elements are by default\nmarkers or nil (nil when the Nth pair didn't match); they are integers\nor nil if the search was on a string.  But if the optional argument\nINTEGERS is non-nil, the elements that represent buffer positions are\nalways integers, not markers, and (if the search was on a buffer) the\nbuffer itself is appended to the list as one additional element.\n\nUse `set-match-data' to reinstate the match data from the elements of\nthis list.\n\nNote that non-matching optional groups at the end of the regexp are\nelided instead of being represented with two `nil's each.  For instance:\n\n  (progn\n    (string-match \"^\\\\(a\\\\)?\\\\(b\\\\)\\\\(c\\\\)?$\" \"b\")\n    (match-data))\n  => (0 1 nil nil 0 1)\n\nIf REUSE is a list, store the value in REUSE by destructively modifying it.\nIf REUSE is long enough to hold all the values, its length remains the\nsame, and any unused elements are set to nil.  If REUSE is not long\nenough, it is extended.  Note that if REUSE is long enough and INTEGERS\nis non-nil, no consing is done to make the return value; this minimizes GC.\n\nIf optional third argument RESEAT is non-nil, any previous markers on the\nREUSE list will be modified to point to nowhere.\n\nReturn value is undefined if the last search failed.")
    @GenerateNodeFactory
    public abstract static class FMatchData extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchData(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-match-data", minArgs = 1, maxArgs = 2, doc = "Set internal data on last search match from elements of LIST.\nLIST should have been created by calling `match-data' previously.\n\nIf optional arg RESEAT is non-nil, make markers on LIST point nowhere.")
    @GenerateNodeFactory
    public abstract static class FSetMatchData extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setMatchData(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-data--translate", minArgs = 1, maxArgs = 1, doc = "Add N to all positions in the match data.  Internal.")
    @GenerateNodeFactory
    public abstract static class FMatchDataTranslate extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchDataTranslate(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "regexp-quote", minArgs = 1, maxArgs = 1, doc = "Return a regexp string which matches exactly STRING and nothing else.")
    @GenerateNodeFactory
    public abstract static class FRegexpQuote extends ELispBuiltInBaseNode {
        @Specialization
        public static Object regexpQuote(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "newline-cache-check", minArgs = 0, maxArgs = 1, doc = "Check the newline cache of BUFFER against buffer contents.\n\nBUFFER defaults to the current buffer.\n\nValue is an array of 2 sub-arrays of buffer positions for newlines,\nthe first based on the cache, the second based on actually scanning\nthe buffer.  If the buffer doesn't have a cache, the value is nil.")
    @GenerateNodeFactory
    public abstract static class FNewlineCacheCheck extends ELispBuiltInBaseNode {
        @Specialization
        public static Object newlineCacheCheck(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "re--describe-compiled", minArgs = 1, maxArgs = 2, doc = "Return a string describing the compiled form of REGEXP.\nIf RAW is non-nil, just return the actual bytecode.")
    @GenerateNodeFactory
    public abstract static class FReDescribeCompiled extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reDescribeCompiled(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }
}
