package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.strings.TruffleString;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.collection.MapSerializer;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.regex.ELispRegExp;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.ThreadLocalStorage;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;
import party.iroiro.juicemacs.piecetree.StringNodes;

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

    public static Object matchData(Node node) {
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

    private record RegExpKey(ELispString regExp, @Nullable ELispString whitespaceRegExp, @Nullable ELispCharTable canon) {
    }

    private static class RegexpCache extends LinkedHashMap<RegExpKey, ELispRegExp.CompiledRegExp> {
        private static final int COMPILED_REGEXP_CACHE_SIZE = 1024;
        static final int CACHE_SIZE_INC_STEP = 128;

        private int max;
        private int requests, misses;

        RegexpCache() {
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

    public static void registerSerializer(Fory fory) {
        fory.registerSerializer(RegexpCache.class, new MapSerializer<>(fory, RegexpCache.class) {
            @Override
            public void write(MemoryBuffer buffer, RegexpCache value) {
            }
            @Override
            public RegexpCache read(MemoryBuffer buffer) {
                return new RegexpCache();
            }
        });
    }

    private static final RegexpCache COMPILED_REGEXPS = new RegexpCache();

    @TruffleBoundary
    public static ELispRegExp.CompiledRegExp compileRegExp(
            ELispLanguage language,
            ELispString regexp,
            @Nullable ELispString whitespaceRegExp
    ) {
        ELispContext context = ELispContext.get(null);
        boolean caseSensitive = isNil(context.getValue(CASE_FOLD_SEARCH));
        ELispCharTable canon = caseSensitive ? null : asCharTable(context.currentBuffer().getCaseCanonTable());
        RegExpKey key = new RegExpKey(
                regexp,
                whitespaceRegExp,
                canon
        );
        ELispRegExp.CompiledRegExp pattern = COMPILED_REGEXPS.get(key);
        if (pattern == null) {
            pattern = ELispRegExp.compile(
                    language,
                    regexp,
                    whitespaceRegExp,
                    canon
            );
            COMPILED_REGEXPS.put(key, pattern);
        }
        return pattern;
    }

    /// Skeleton interface with Emacs search convention util functions
    private sealed abstract static class SearchConvention<T> {
        final T pattern;

        private SearchConvention(T pattern) {
            this.pattern = pattern;
        }

        Object search(ELispBuffer buffer, Object bound, Object noError, Object count) {
            long repeat = notNilOr(count, 1);
            boolean forward = repeat >= 0;
            repeat = Math.abs(repeat);

            long at = buffer.getPoint();
            long startEnd, end;
            if (forward) {
                startEnd = end = notNilOr(bound, buffer.pointMax());
            } else {
                startEnd = notNilOr(bound, buffer.pointMin());
                end = buffer.getPoint();
            }

            for (long i = 0; i < repeat; i++) {
                at = searchOnce(buffer, forward, at, startEnd, end);
                if (at == -1) {
                    if (isNil(noError)) {
                        throw ELispSignals.searchFailed();
                    }
                    if (!isT(noError)) {
                        buffer.setPoint(startEnd);
                    }
                    return false;
                }
            }
            buffer.setPoint(at);
            return buffer.getPoint();
        }

        /// Searches according to [#pattern]
        ///
        /// @return `-1` if not found; the end of the match if `forward`; `start - 1` if `!forward`
        abstract long searchOnce(ELispBuffer buffer, boolean forward, long start, long startEnd, long searchEnd);

        void setMatchImpl(Node node, Object matchData) {
            BuiltInSearch.setMatch(node, matchData, false);
        }

        abstract void setMatch(Node node);

        static final class StringSearch extends SearchConvention<ELispString> {
            private final boolean caseFold;
            long matchStart = -1;

            private StringSearch(ELispString pattern, boolean caseFold) {
                super(pattern);
                this.caseFold = caseFold;
            }

            @Override
            long searchOnce(ELispBuffer buffer, boolean forward, long start, long startEnd, long searchEnd) {
                while (start != startEnd) {
                    if (currentMatch(buffer, start, searchEnd)) {
                        matchStart = start;
                        return forward ? start + pattern.length() : start - 1;
                    }
                    start += forward ? 1 : -1;
                }
                matchStart = -1;
                return -1;
            }

            private boolean currentMatch(ELispBuffer buffer, long start, long end) {
                PrimitiveIterator.OfInt iterator = pattern.iterator(0);
                PrimitiveIterator.OfInt bufferI = buffer.iterator(start, end);
                ELispCharTable canon = caseFold ? asCharTable(buffer.getCaseCanonTable()) : null;
                long remaining = end - start;
                while (iterator.hasNext()) {
                    if (remaining <= 0) {
                        return false;
                    }
                    remaining--;

                    int c1 = iterator.nextInt();
                    int c2 = bufferI.nextInt();
                    if (caseFold) {
                        ELispCharTable canonNonNull = assertNotNull(canon);
                        c1 = (int) notNilOr(canonNonNull.getChar(c1), c1);
                        c2 = (int) notNilOr(canonNonNull.getChar(c2), c2);
                    }
                    if (c1 != c2) {
                        return false;
                    }
                    start++;
                }
                return true;
            }

            @Override
            void setMatch(Node node) {
                setMatchImpl(node, matchStart == -1 ? false : ELispCons.listOf(matchStart, matchStart + pattern.length()));
            }
        }

        static final class RegexpSearch extends SearchConvention<ELispRegExp.CompiledRegExp> {
            @Nullable
            Object matchData = null;

            private RegexpSearch(ELispRegExp.CompiledRegExp pattern) {
                super(pattern);
            }

            @Override
            long searchOnce(ELispBuffer buffer, boolean forward, long start, long startEnd, long searchEnd) {
                if (forward) {
                    return searchForwardOnce(buffer, start, startEnd);
                }
                while (start >= startEnd) {
                    if (currentMatch(buffer, start, searchEnd)) {
                        return start;
                    }
                    start--;
                }
                return -1;
            }

            private boolean currentMatch(ELispBuffer buffer, long from, long limit) {
                Object result = pattern.call(buffer, false, from, limit);
                if (result instanceof ELispCons cons) {
                    matchData = cons;
                    return true;
                }
                matchData = null;
                return false;
            }

            private long searchForwardOnce(ELispBuffer buffer, long from, long limit) {
                Object result = pattern.call(buffer, true, from, limit);
                if (result instanceof ELispCons cons) {
                    matchData = cons;
                    return asLong(cons.get(1));
                }
                matchData = null;
                return -1;
            }

            @Override
            void setMatch(Node node) {
                setMatchImpl(node, Objects.requireNonNullElse(matchData, false));
            }
        }
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
        public boolean lookingAt(ELispString regexp, Object inhibitModify) {
            ELispRegExp.CompiledRegExp regExp = compileRegExp(getLanguage(), regexp, null);
            ELispBuffer buffer = getContext().currentBuffer();
            Object result = regExp.call(buffer, false, buffer.getPoint(), -1);
            if (isNil(inhibitModify)) {
                setMatch(this, result, false);
            }
            return !isNil(result);
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
        @TruffleBoundary
        @Specialization
        public Object stringMatch(ELispString regexp, ELispString string, Object start, boolean inhibitModify) {
            ELispRegExp.CompiledRegExp pattern = compileRegExp(getLanguage(), regexp, null);
            long from = notNilOr(start, 0);
            Object result = pattern.call(string, true, from, -1, getLanguage().currentBuffer().getValue());
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
        public Object searchBackward(ELispString string, Object bound, Object noerror, Object count) {
            long repeat = notNilOr(count, 1);
            return FSearchForward.searchForward(this, string, bound, noerror, -repeat);
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
        public Object searchForwardFunc(ELispString string, Object bound, Object noerror, Object count) {
            return searchForward(this, string, bound, noerror, count);
        }

        public static Object searchForward(Node node, ELispString string, Object bound, Object noerror, Object count) {
            SearchConvention.StringSearch s =
                    new SearchConvention.StringSearch(string, !isNil(CASE_FOLD_SEARCH.getValue()));
            ELispBuffer buffer = currentBuffer();
            Object result = s.search(buffer, bound, noerror, count);
            s.setMatch(node);
            return result;
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
        public Object reSearchBackward(ELispString regexp, Object bound, Object noerror, Object count) {
            long repeat = notNilOr(count, 1);
            return FReSearchForward.reSearchForward(this, regexp, bound, noerror, -repeat);
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
        public Object reSearchForwardNode(ELispString regexp, Object bound, Object noerror, Object count) {
            return reSearchForward(this, regexp, bound, noerror, count);
        }

        public static Object reSearchForward(
                Node node, ELispString regexp, Object bound, Object noerror, Object count
        ) {
            ELispBuffer buffer = currentBuffer();
            ELispRegExp.CompiledRegExp pattern = compileRegExp(ELispLanguage.get(null), regexp, null);
            SearchConvention<ELispRegExp.CompiledRegExp> s = new SearchConvention.RegexpSearch(pattern);
            Object result = s.search(buffer, bound, noerror, count);
            s.setMatch(node);
            return result;
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
        public ELispString replaceMatch(
                ELispString newtext, Object fixedcase, Object literal, Object string, Object subexp,
                @Cached TruffleString.SubstringNode substring
        ) {
            // TODO: fixedcase, literal...
            long subexpN = notNilOr(subexp, 0);
            ELispString s = asStr(isNil(string) ? matchStr(this) : string);
            ConsIterator cons = asCons(matchData(this)).listIterator((int) (subexpN * 2));
            int start = asInt(cons.next());
            int end = asInt(cons.next());
            int length = StringNodes.length(s.value());
            TruffleString before = substring.execute(s.value(), 0, start, StringSupport.UTF_32, true);
            TruffleString after = substring.execute(s.value(), end, length - end, StringSupport.UTF_32, true);
            return new MuleStringBuilder()
                    .append(before, s.state())
                    .appendString(newtext)
                    .append(after, s.state())
                    .buildString();
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
            if (isNil(list)) {
                BuiltInSearch.setMatch(this, false, false);
                return false;
            }
            if (!BuiltInData.FListp.listp(list)) {
                throw ELispSignals.wrongTypeArgument(LISTP, list);
            }
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            ConsIterator iterator = asCons(list).iterator();
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
                ConsIterator i = cons.listIterator(0);
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
            return ELispRegExp.quote(string);
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
