package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.regex.ELispRegExp;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.ThreadLocalStorage;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.CASE_FOLD_SEARCH;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LISTP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInSearch extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInSearchFactory.getFactories();
    }

    private final ThreadLocalStorage matchData = new ThreadLocalStorage(false);
    private final ThreadLocalStorage matchedStr = new ThreadLocalStorage(false);

    private static Object matchData(Node node) {
        return ELispContext.get(node).globals().builtInSearch.matchData.getValue();
    }

    private static Object matchStr(Node node) {
        return ELispContext.get(node).globals().builtInSearch.matchedStr.getValue();
    }

    private static void setMatch(Node node, Object data, Object str) {
        BuiltInSearch builtInSearch = ELispContext.get(node).globals().builtInSearch;
        builtInSearch.matchData.setValue(data);
        builtInSearch.matchedStr.setValue(str);
    }

    private record RegExpKey(MuleString regExp, @Nullable MuleString whitespaceRegExp, @Nullable ELispCharTable canon) {
    }

    private static class RegexpCache extends LinkedHashMap<RegExpKey, ELispRegExp.CompiledRegExp> {
        private static final int COMPILED_REGEXP_CACHE_SIZE = 1024;
        public static final int CACHE_SIZE_INC_STEP = 128;

        private int max;
        private int requests, misses;

        public RegexpCache() {
            super(COMPILED_REGEXP_CACHE_SIZE, 0.75f, true);
            max = COMPILED_REGEXP_CACHE_SIZE;
            requests = 0;
            misses = 0;
        }

        @Override
        protected boolean removeEldestEntry(Map.Entry<RegExpKey, ELispRegExp.CompiledRegExp> eldest) {
            return size() > max;
        }

        @Override
        public ELispRegExp.@Nullable CompiledRegExp get(Object key) {
            ELispRegExp.CompiledRegExp regexp = super.get(key);
            requests++;
            if (regexp == null) {
                misses++;
            }
            if (requests > CACHE_SIZE_INC_STEP && requests % CACHE_SIZE_INC_STEP == 0) {
                if (misses * 5 > requests) {
                    misses = 0;
                    requests = 0;
                    max += CACHE_SIZE_INC_STEP;
                }
            }
            return regexp;
        }
    }

    private static final RegexpCache COMPILED_REGEXPS = new RegexpCache();

    @CompilerDirectives.TruffleBoundary
    public static ELispRegExp.CompiledRegExp compileRegExp(
            ELispLanguage language,
            ELispString regexp,
            @Nullable MuleString whitespaceRegExp
    ) {
        ELispContext context = ELispContext.get(null);
        boolean caseSensitive = isNil(context.getValue(CASE_FOLD_SEARCH));
        ELispCharTable canon = caseSensitive ? null : asCharTable(context.currentBuffer().getCaseCanonTable());
        RegExpKey key = new RegExpKey(
                regexp.value(),
                whitespaceRegExp,
                canon
        );
        ELispRegExp.CompiledRegExp pattern = COMPILED_REGEXPS.get(key);
        if (pattern == null) {
            pattern = ELispRegExp.compile(
                    language,
                    regexp.value(),
                    whitespaceRegExp,
                    canon
            );
            COMPILED_REGEXPS.put(key, pattern);
        }
        return pattern;
    }

    /**
     * <pre>
     * Return t if text after point matches regular expression REGEXP.
     * By default, this function modifies the match data that
     * `match-beginning', `match-end' and `match-data' access.  If
     * INHIBIT-MODIFY is non-nil, don't modify the match data.
     * </pre>
     */
    @ELispBuiltIn(name = "looking-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLookingAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lookingAt(Object regexp, Object inhibitModify) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if text after point matches REGEXP according to Posix rules.
     * Find the longest match, in accordance with Posix regular expression rules.
     *
     * By default, this function modifies the match data that
     * `match-beginning', `match-end' and `match-data' access.  If
     * INHIBIT-MODIFY is non-nil, don't modify the match data.
     * </pre>
     */
    @ELispBuiltIn(name = "posix-looking-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPosixLookingAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posixLookingAt(Object regexp, Object inhibitModify) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return index of start of first match for REGEXP in STRING, or nil.
     * Matching ignores case if `case-fold-search' is non-nil.
     * If third arg START is non-nil, start search at that index in STRING.
     *
     * If INHIBIT-MODIFY is non-nil, match data is not changed.
     *
     * If INHIBIT-MODIFY is nil or missing, match data is changed, and
     * `match-end' and `match-beginning' give indices of substrings matched
     * by parenthesis constructs in the pattern.  You can use the function
     * `match-string' to extract the substrings matched by the parenthesis
     * constructions in REGEXP.  For index of first char beyond the match, do
     * (match-end 0).
     * </pre>
     */
    @ELispBuiltIn(name = "string-match", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FStringMatch extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public Object stringMatch(ELispString regexp, ELispString string, Object start, boolean inhibitModify) {
            ELispRegExp.CompiledRegExp pattern = compileRegExp(getLanguage(), regexp, null);
            long from = notNilOr(start, 0);
            Object result = pattern.call(string.value(), true, from, -1, getLanguage().currentBuffer().getValue());
            if (result instanceof ELispCons cons) {
                if (!inhibitModify) {
                    setMatch(this, result, string);
                }
                return cons.car();
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return index of start of first match for Posix REGEXP in STRING, or nil.
     * Find the longest match, in accord with Posix regular expression rules.
     * Case is ignored if `case-fold-search' is non-nil in the current buffer.
     *
     * If INHIBIT-MODIFY is non-nil, match data is not changed.
     *
     * If INHIBIT-MODIFY is nil or missing, match data is changed, and
     * `match-end' and `match-beginning' give indices of substrings matched
     * by parenthesis constructs in the pattern.  You can use the function
     * `match-string' to extract the substrings matched by the parenthesis
     * constructions in REGEXP.  For index of first char beyond the match, do
     * (match-end 0).
     * </pre>
     */
    @ELispBuiltIn(name = "posix-string-match", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPosixStringMatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posixStringMatch(Object regexp, Object string, Object start, Object inhibitModify) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Search backward from point for STRING.
     * Set point to the beginning of the occurrence found, and return point.
     * An optional second argument bounds the search; it is a buffer position.
     *   The match found must not begin before that position.  A value of nil
     *   means search to the beginning of the accessible portion of the buffer.
     * Optional third argument, if t, means if fail just return nil (no error).
     *   If not nil and not t, position at limit of search and return nil.
     * Optional fourth argument COUNT, if a positive number, means to search
     *   for COUNT successive occurrences.  If COUNT is negative, search
     *   forward, instead of backward, for -COUNT occurrences.  A value of
     *   nil means the same as 1.
     * With COUNT positive, the match found is the COUNTth to last one (or
     *   last, if COUNT is 1 or nil) in the buffer located entirely before
     *   the origin of the search; correspondingly with COUNT negative.
     *
     * Search case-sensitivity is determined by the value of the variable
     * `case-fold-search', which see.
     *
     * See also the functions `match-beginning', `match-end' and `replace-match'.
     * </pre>
     */
    @ELispBuiltIn(name = "search-backward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object searchBackward(ELispString string, Object bound, Object noerror, Object count) {
            long repeat = notNilOr(count, 1);
            return FSearchForward.searchForward(string, bound, noerror, -repeat);
        }
    }

    /**
     * <pre>
     * Search forward from point for STRING.
     * Set point to the end of the occurrence found, and return point.
     * An optional second argument bounds the search; it is a buffer position.
     *   The match found must not end after that position.  A value of nil
     *   means search to the end of the accessible portion of the buffer.
     * Optional third argument, if t, means if fail just return nil (no error).
     *   If not nil and not t, move to limit of search and return nil.
     * Optional fourth argument COUNT, if a positive number, means to search
     *   for COUNT successive occurrences.  If COUNT is negative, search
     *   backward, instead of forward, for -COUNT occurrences.  A value of
     *   nil means the same as 1.
     * With COUNT positive, the match found is the COUNTth one (or first,
     *   if COUNT is 1 or nil) in the buffer located entirely after the
     *   origin of the search; correspondingly with COUNT negative.
     *
     * Search case-sensitivity is determined by the value of the variable
     * `case-fold-search', which see.
     *
     * See also the functions `match-beginning', `match-end' and `replace-match'.
     * </pre>
     */
    @ELispBuiltIn(name = "search-forward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object searchForward(ELispString string, Object bound, Object noerror, Object count) {
            long repeat = notNilOr(count, 1);
            boolean forward = repeat >= 0;
            repeat = Math.abs(repeat);
            boolean caseFold = !isNil(CASE_FOLD_SEARCH.getValue());

            ELispBuffer buffer = currentBuffer();
            long start = buffer.getPoint() - 1;
            long limit = FReSearchForward.getBound(bound, forward);
            for (int i = 0; i < repeat; i++) {
                if (currentMatch(buffer, string.value(), start, caseFold)) {
                    if (forward) {
                        start++;
                    } else {
                        start--;
                    }
                    continue;
                }
                if (isNil(noerror)) {
                    throw ELispSignals.searchFailed();
                }
                return false;
            }
            buffer.setPoint(start + string.length() + 1);
            return buffer.getPoint();
        }

        private static boolean currentMatch(ELispBuffer buffer, MuleString string, long start, boolean caseFold) {
            PrimitiveIterator.OfInt iterator = string.iterator(0);
            @Nullable ELispCharTable canon = caseFold ? asCharTable(buffer.getCaseCanonTable()) : null;
            while (iterator.hasNext()) {
                int c1 = iterator.nextInt();
                long c2 = buffer.getChar(start);
                if (caseFold) {
                    c1 = (int) notNilOr(canon.getChar(c1), c1);
                    c2 = (int) notNilOr(canon.getChar((int) c2), c2);
                }
                if (c1 != c2) {
                    return false;
                }
                start++;
            }
            return true;
        }
    }

    /**
     * <pre>
     * Search backward from point for regular expression REGEXP.
     * This function is almost identical to `re-search-forward', except that
     * by default it searches backward instead of forward, and the sign of
     * COUNT also indicates exactly the opposite searching direction.
     * See `re-search-forward' for details.
     *
     * Note that searching backwards may give a shorter match than expected,
     * because REGEXP is still matched in the forward direction.  See Info
     * anchor `(elisp) re-search-backward' for details.
     * </pre>
     */
    @ELispBuiltIn(name = "re-search-backward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FReSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reSearchBackward(ELispString regexp, Object bound, Object noerror, Object count) {
            long repeat = notNilOr(count, 1);
            return FReSearchForward.reSearchForward(regexp, bound, noerror, -repeat);
        }
    }

    /**
     * <pre>
     * Search forward from point for regular expression REGEXP.
     * Set point to the end of the occurrence found, and return point.
     * The optional second argument BOUND is a buffer position that bounds
     *   the search.  The match found must not end after that position.  A
     *   value of nil means search to the end of the accessible portion of
     *   the buffer.
     * The optional third argument NOERROR indicates how errors are handled
     *   when the search fails: if it is nil or omitted, emit an error; if
     *   it is t, simply return nil and do nothing; if it is neither nil nor
     *   t, move to the limit of search and return nil.
     * The optional fourth argument COUNT is a number that indicates the
     *   search direction and the number of occurrences to search for.  If it
     *   is positive, search forward for COUNT successive occurrences; if it
     *   is negative, search backward, instead of forward, for -COUNT
     *   occurrences.  A value of nil means the same as 1.
     * With COUNT positive/negative, the match found is the COUNTth/-COUNTth
     *   one in the buffer located entirely after/before the origin of the
     *   search.
     *
     * Search case-sensitivity is determined by the value of the variable
     * `case-fold-search', which see.
     *
     * See also the functions `match-beginning', `match-end', `match-string',
     * and `replace-match'.
     * </pre>
     */
    @ELispBuiltIn(name = "re-search-forward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FReSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reSearchForward(ELispString regexp, Object bound, Object noerror, Object count) {
            long repeat = notNilOr(count, 1);
            boolean forward = repeat >= 0;
            repeat = Math.abs(repeat);

            ELispBuffer buffer = currentBuffer();
            ELispRegExp.CompiledRegExp pattern = compileRegExp(ELispLanguage.get(null), regexp, null);
            long limit = getBound(bound, forward);

            long at = buffer.getPoint() - 1;
            for (long i = 0; i < repeat; i++) {
                at = forward
                        ? searchForwardOnce(pattern, buffer, at, limit)
                        : searchBackwardOnce(pattern, buffer, at, limit);
                if (at == -1) {
                    if (isNil(noerror)) {
                        throw ELispSignals.searchFailed();
                    }
                    return false;
                }
            }
            buffer.setPoint(at + 1);
            return buffer.getPoint();
        }

        public static long getBound(Object bound, boolean forward) {
            return forward
                    ? (isNil(bound) ? -1 : asLong(bound) - 1)
                    : notNilOr(bound, 0);
        }

        private static long searchBackwardOnce(
                ELispRegExp.CompiledRegExp pattern, ELispBuffer buffer,
                long from, long limit
        ) {
            while (from >= limit) {
                Object result = pattern.call(buffer, false, from, -1);
                if (result instanceof ELispCons cons) {
                    return asLong(cons.get(1));
                }
                from--;
            }
            return -1;
        }

        private static long searchForwardOnce(
                ELispRegExp.CompiledRegExp pattern, ELispBuffer buffer,
                long from, long limit
        ) {
            Object result = pattern.call(buffer, true, from, limit);
            if (result instanceof ELispCons cons) {
                return asLong(cons.get(1));
            }
            return -1;
        }
    }

    /**
     * <pre>
     * Search backward from point for match for REGEXP according to Posix rules.
     * Find the longest match in accord with Posix regular expression rules.
     * Set point to the beginning of the occurrence found, and return point.
     * An optional second argument bounds the search; it is a buffer position.
     *   The match found must not begin before that position.  A value of nil
     *   means search to the beginning of the accessible portion of the buffer.
     * Optional third argument, if t, means if fail just return nil (no error).
     *   If not nil and not t, position at limit of search and return nil.
     * Optional fourth argument COUNT, if a positive number, means to search
     *   for COUNT successive occurrences.  If COUNT is negative, search
     *   forward, instead of backward, for -COUNT occurrences.  A value of
     *   nil means the same as 1.
     * With COUNT positive, the match found is the COUNTth to last one (or
     *   last, if COUNT is 1 or nil) in the buffer located entirely before
     *   the origin of the search; correspondingly with COUNT negative.
     *
     * Search case-sensitivity is determined by the value of the variable
     * `case-fold-search', which see.
     *
     * See also the functions `match-beginning', `match-end', `match-string',
     * and `replace-match'.
     * </pre>
     */
    @ELispBuiltIn(name = "posix-search-backward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPosixSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posixSearchBackward(Object regexp, Object bound, Object noerror, Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Search forward from point for REGEXP according to Posix rules.
     * Find the longest match in accord with Posix regular expression rules.
     * Set point to the end of the occurrence found, and return point.
     * An optional second argument bounds the search; it is a buffer position.
     *   The match found must not end after that position.  A value of nil
     *   means search to the end of the accessible portion of the buffer.
     * Optional third argument, if t, means if fail just return nil (no error).
     *   If not nil and not t, move to limit of search and return nil.
     * Optional fourth argument COUNT, if a positive number, means to search
     *   for COUNT successive occurrences.  If COUNT is negative, search
     *   backward, instead of forward, for -COUNT occurrences.  A value of
     *   nil means the same as 1.
     * With COUNT positive, the match found is the COUNTth one (or first,
     *   if COUNT is 1 or nil) in the buffer located entirely after the
     *   origin of the search; correspondingly with COUNT negative.
     *
     * Search case-sensitivity is determined by the value of the variable
     * `case-fold-search', which see.
     *
     * See also the functions `match-beginning', `match-end', `match-string',
     * and `replace-match'.
     * </pre>
     */
    @ELispBuiltIn(name = "posix-search-forward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPosixSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posixSearchForward(Object regexp, Object bound, Object noerror, Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Replace text matched by last search with NEWTEXT.
     * Leave point at the end of the replacement text.
     *
     * If optional second arg FIXEDCASE is non-nil, do not alter the case of
     * the replacement text.  Otherwise, maybe capitalize the whole text, or
     * maybe just word initials, based on the replaced text.  If the replaced
     * text has only capital letters and has at least one multiletter word,
     * convert NEWTEXT to all caps.  Otherwise if all words are capitalized
     * in the replaced text, capitalize each word in NEWTEXT.  Note that
     * what exactly is a word is determined by the syntax tables in effect
     * in the current buffer, and the variable `case-symbols-as-words'.
     *
     * If optional third arg LITERAL is non-nil, insert NEWTEXT literally.
     * Otherwise treat `\\' as special:
     *   `\\&amp;' in NEWTEXT means substitute original matched text.
     *   `\\N' means substitute what matched the Nth `\\(...\\)'.
     *        If Nth parens didn't match, substitute nothing.
     *   `\\\\' means insert one `\\'.
     *   `\\?' is treated literally
     *        (for compatibility with `query-replace-regexp').
     *   Any other character following `\\' signals an error.
     * Case conversion does not apply to these substitutions.
     *
     * If optional fourth argument STRING is non-nil, it should be a string
     * to act on; this should be the string on which the previous match was
     * done via `string-match'.  In this case, `replace-match' creates and
     * returns a new string, made by copying STRING and replacing the part of
     * STRING that was matched (the original STRING itself is not altered).
     *
     * The optional fifth argument SUBEXP specifies a subexpression;
     * it says to replace just that subexpression with NEWTEXT,
     * rather than replacing the entire matched text.
     * This is, in a vague sense, the inverse of using `\\N' in NEWTEXT;
     * `\\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
     * NEWTEXT in place of subexp N.
     * This is useful only after a regular expression search or match,
     * since only regular expressions have distinguished subexpressions.
     * </pre>
     */
    @ELispBuiltIn(name = "replace-match", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FReplaceMatch extends ELispBuiltInBaseNode {
        @Specialization
        public ELispString replaceMatch(ELispString newtext, Object fixedcase, Object literal, Object string, Object subexp) {
            // TODO: fixedcase, literal...
            long subexpN = notNilOr(subexp, 0);
            ELispString s = asStr(isNil(string) ? matchStr(this) : string);
            ELispCons cons = asCons(matchData(this)).getCons((int) (subexpN * 2));
            long start = asLong(cons.car());
            long end = asLong(asCons(cons.cdr()).car());
            MuleString before = s.value().subSequence(0, start);
            MuleString after = s.value().subSequence(end, s.value().length());
            MuleString result = new MuleStringBuffer()
                    .append(before)
                    .append(newtext.value())
                    .append(after).build();
            return new ELispString(result);
        }
    }

    /**
     * <pre>
     * Return position of start of text matched by last search.
     * SUBEXP, a number, specifies the parenthesized subexpression in the last
     *   regexp for which to return the start position.
     * Value is nil if SUBEXPth subexpression didn't match, or there were fewer
     *   than SUBEXP subexpressions.
     * SUBEXP zero means the entire text matched by the whole regexp or whole
     *   string.
     *
     * Return value is undefined if the last search failed.
     * </pre>
     */
    @ELispBuiltIn(name = "match-beginning", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public Object matchBeginning(long subexp) {
            Object value = matchData(this);
            return BuiltInFns.FNth.nth(2 * subexp, value);
        }
    }

    /**
     * <pre>
     * Return position of end of text matched by last search.
     * SUBEXP, a number, specifies the parenthesized subexpression in the last
     *   regexp for which to return the start position.
     * Value is nil if SUBEXPth subexpression didn't match, or there were fewer
     *   than SUBEXP subexpressions.
     * SUBEXP zero means the entire text matched by the whole regexp or whole
     *   string.
     *
     * Return value is undefined if the last search failed.
     * </pre>
     */
    @ELispBuiltIn(name = "match-end", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchEnd extends ELispBuiltInBaseNode {
        @Specialization
        public Object matchEnd(long subexp) {
            Object value = matchData(this);
            return BuiltInFns.FNth.nth(2 * subexp + 1, value);
        }
    }

    /**
     * <pre>
     * Return a list of positions that record text matched by the last search.
     * Element 2N of the returned list is the position of the beginning of the
     * match of the Nth subexpression; it corresponds to `(match-beginning N)';
     * element 2N + 1 is the position of the end of the match of the Nth
     * subexpression; it corresponds to `(match-end N)'.  See `match-beginning'
     * and `match-end'.
     * If the last search was on a buffer, all the elements are by default
     * markers or nil (nil when the Nth pair didn't match); they are integers
     * or nil if the search was on a string.  But if the optional argument
     * INTEGERS is non-nil, the elements that represent buffer positions are
     * always integers, not markers, and (if the search was on a buffer) the
     * buffer itself is appended to the list as one additional element.
     *
     * Use `set-match-data' to reinstate the match data from the elements of
     * this list.
     *
     * Note that non-matching optional groups at the end of the regexp are
     * elided instead of being represented with two `nil's each.  For instance:
     *
     *   (progn
     *     (string-match "^\\(a\\)?\\(b\\)\\(c\\)?$" "b")
     *     (match-data))
     *   =&gt; (0 1 nil nil 0 1)
     *
     * If REUSE is a list, store the value in REUSE by destructively modifying it.
     * If REUSE is long enough to hold all the values, its length remains the
     * same, and any unused elements are set to nil.  If REUSE is not long
     * enough, it is extended.  Note that if REUSE is long enough and INTEGERS
     * is non-nil, no consing is done to make the return value; this minimizes GC.
     *
     * If optional third argument RESEAT is non-nil, any previous markers on the
     * REUSE list will be modified to point to nowhere.
     *
     * Return value is undefined if the last search failed.
     * </pre>
     */
    @ELispBuiltIn(name = "match-data", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMatchData extends ELispBuiltInBaseNode {
        @Specialization
        public Object matchData(boolean integers, Object reuse, Object reseat) {
            if (!isNil(reuse)) {
                throw new UnsupportedOperationException();
            }
            Object value = BuiltInSearch.matchData(this);
            if (value instanceof ELispCons cons) {
                return cons;
            }
            return false;
        }
    }

    /**
     * <pre>
     * Set internal data on last search match from elements of LIST.
     * LIST should have been created by calling `match-data' previously.
     *
     * If optional arg RESEAT is non-nil, make markers on LIST point nowhere.
     * </pre>
     */
    @ELispBuiltIn(name = "set-match-data", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetMatchData extends ELispBuiltInBaseNode {
        @Specialization
        public boolean setMatchData(Object list, Object reseat) {
            if (!BuiltInData.FListp.listp(list)) {
                throw ELispSignals.wrongTypeArgument(LISTP, list);
            }
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            Iterator<Object> iterator = asCons(list).iterator();
            while (iterator.hasNext()) {
                Object o = iterator.next();
                if (iterator.hasNext()) {
                    Object p = iterator.next();
                    if ((BuiltInData.FNatnump.natnump(o) && BuiltInData.FNatnump.natnump(p)) || (isNil(o) && isNil(p))) {
                        builder.add(o).add(p);
                    } else {
                        throw new UnsupportedOperationException();
                    }
                } else {
                    break;
                }
            }
            setMatch(this, builder.build(), false);
            return false;
        }
    }

    /**
     * <pre>
     * Add N to all positions in the match data.  Internal.
     * </pre>
     */
    @ELispBuiltIn(name = "match-data--translate", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchDataTranslate extends ELispBuiltInBaseNode {
        @Specialization
        public Object matchDataTranslate(long n) {
            Object value = matchData(this);
            if (value instanceof ELispCons cons) {
                ELispCons.ConsIterator i = cons.consIterator(0);
                while (i.hasNextCons()) {
                    ELispCons current = i.nextCons();
                    if (current.car() instanceof Long l) {
                        current.setCar(Math.max(0, l + n));
                    }
                }
            }
            return value;
        }
    }

    /**
     * <pre>
     * Return a regexp string which matches exactly STRING and nothing else.
     * </pre>
     */
    @ELispBuiltIn(name = "regexp-quote", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRegexpQuote extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString regexpQuote(ELispString string) {
            return ELispRegExp.quote(string.value());
        }
    }

    /**
     * <pre>
     * Check the newline cache of BUFFER against buffer contents.
     *
     * BUFFER defaults to the current buffer.
     *
     * Value is an array of 2 sub-arrays of buffer positions for newlines,
     * the first based on the cache, the second based on actually scanning
     * the buffer.  If the buffer doesn't have a cache, the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "newline-cache-check", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNewlineCacheCheck extends ELispBuiltInBaseNode {
        @Specialization
        public static Void newlineCacheCheck(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a string describing the compiled form of REGEXP.
     * If RAW is non-nil, just return the actual bytecode.
     * </pre>
     */
    @ELispBuiltIn(name = "re--describe-compiled", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FReDescribeCompiled extends ELispBuiltInBaseNode {
        @Specialization
        public static Void reDescribeCompiled(Object regexp, Object raw) {
            throw new UnsupportedOperationException();
        }
    }
}
