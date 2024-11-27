package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.regex.ELispRegExp;
import party.iroiro.juicemacs.elisp.nodes.FunctionDispatchNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.ArrayList;
import java.util.List;
import java.util.PrimitiveIterator;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static party.iroiro.juicemacs.elisp.forms.BuiltInEditFns.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.LAMBDA;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInMiniBuf extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInMiniBufFactory.getFactories();
    }

    static abstract sealed class CompletionMatcher implements Consumer<Object>, BiConsumer<Object, Object> {
        protected final MuleString target;
        protected final boolean ignoreCase;
        private final Object predicate;
        private final FunctionDispatchNode dispatchNode;
        private final Node node;
        private final ELispRegExp.CompiledRegExp[] regExps;

        CompletionMatcher(ELispString target, Object predicate, FunctionDispatchNode dispatchNode, Node node) {
            this.predicate = predicate;
            this.dispatchNode = dispatchNode;
            this.node = node;
            this.ignoreCase = !isNil(ELispGlobals.completionIgnoreCase.getValue());
            this.regExps = getRegExps(node);
            this.target = (this.ignoreCase ? BuiltInCaseFiddle.FUpcase.upcaseString(target) : target).value();
        }

        public static ELispRegExp.CompiledRegExp[] getRegExps(Node node) {
            ArrayList<ELispRegExp.CompiledRegExp> compiledRegExps = new ArrayList<>();
            ELispLanguage language = ELispLanguage.get(node);
            for (Object regExp : asConsOrNil(ELispGlobals.completionRegexpList.getValue())) {
                if (regExp instanceof ELispString s) {
                    ELispRegExp.CompiledRegExp compiledRegExp = BuiltInSearch.compileRegExp(language, s, null);
                    compiledRegExps.add(compiledRegExp);
                }
            }
            return compiledRegExps.toArray(new ELispRegExp.CompiledRegExp[0]);
        }

        @SuppressWarnings("BooleanMethodIsAlwaysInverted")
        public boolean forEachInCollection(Object collection) {
            switch (collection) {
                case ELispCons assocList -> assocList.forEach(this);
                case ELispHashtable hashtable -> hashtable.forEach(this);
                case ELispVector obarray -> ELispContext.getObarrayInner(obarray).forEach(this);
                default -> {
                    return false;
                }
            }
            return true;
        }

        @Override
        public void accept(Object key) {
            Object s = key instanceof ELispCons cons ? cons.car() : key;
            tryMatch(getString(s), key, false);
        }

        @Override
        public void accept(Object key, Object value) {
            if (key instanceof MuleString s) {
                // obarray
                tryMatch(s, value, false);
            }
            // hashtable
            tryMatch(getString(key), key, value);
        }

        public void tryMatch(@Nullable MuleString s, Object key, Object value) {
            if (s == null) {
                return;
            }
            if (s.length() < target.length()) {
                return;
            }
            PrimitiveIterator.OfInt expected = target.iterator(0);
            PrimitiveIterator.OfInt actual = s.iterator(0);
            while (expected.hasNext()) {
                int e = expected.nextInt();
                int a = actual.nextInt();
                if (ignoreCase) {
                    a = Math.toIntExact(BuiltInCaseFiddle.FUpcase.upcaseChar(a));
                }
                if (e != a) {
                    return;
                }
            }
            if (!isNil(predicate)) {
                if (isNil(dispatchNode.executeDispatch(node, predicate, new Object[]{key, value}))) {
                    return;
                }
            }
            ELispBuffer buffer = currentBuffer();
            for (ELispRegExp.CompiledRegExp regExp : regExps) {
                if (isNil(regExp.call(s, true, 0, -1, buffer))) {
                    return;
                }
            }
            record(s, key);
        }

        protected abstract void record(MuleString string, Object key);

        private static @Nullable MuleString getString(Object o) {
            return switch (o) {
                case ELispString eStr -> eStr.value();
                case ELispSymbol symbol -> symbol.name();
                default -> null;
            };
        }
    }

    /**
     * <pre>
     * Return the currently active minibuffer window, or nil if none.
     * </pre>
     */
    @ELispBuiltIn(name = "active-minibuffer-window", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FActiveMinibufferWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void activeMinibufferWindow() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Specify which minibuffer window to use for the minibuffer.
     * This affects where the minibuffer is displayed if you put text in it
     * without invoking the usual minibuffer commands.
     * </pre>
     */
    @ELispBuiltIn(name = "set-minibuffer-window", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetMinibufferWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setMinibufferWindow(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if BUFFER is a minibuffer.
     * No argument or nil as argument means use current buffer as BUFFER.
     * BUFFER can be a buffer or a buffer name.  If LIVE is non-nil, then
     * return t only if BUFFER is an active minibuffer.
     * </pre>
     */
    @ELispBuiltIn(name = "minibufferp", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMinibufferp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferp(Object buffer, Object live) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if BUFFER is the most nested active minibuffer.
     * No argument or nil as argument means use the current buffer as BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "innermost-minibuffer-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInnermostMinibufferP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void innermostMinibufferP(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if BUFFER is a minibuffer at the current command loop level.
     * No argument or nil as argument means use the current buffer as BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-innermost-command-loop-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMinibufferInnermostCommandLoopP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferInnermostCommandLoopP(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Abort the current minibuffer.
     * If we are not currently in the innermost minibuffer, prompt the user to
     * confirm the aborting of the current minibuffer and all contained ones.
     * </pre>
     */
    @ELispBuiltIn(name = "abort-minibuffers", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FAbortMinibuffers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void abortMinibuffers() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer position of the end of the minibuffer prompt.
     * Return (point-min) if current buffer is not a minibuffer.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-prompt-end", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMinibufferPromptEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferPromptEnd() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the user input in a minibuffer as a string.
     * If the current buffer is not a minibuffer, return its entire contents.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-contents", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMinibufferContents extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferContents() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the user input in a minibuffer as a string, without text-properties.
     * If the current buffer is not a minibuffer, return its entire contents.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-contents-no-properties", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMinibufferContentsNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferContentsNoProperties() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read a string from the minibuffer, prompting with string PROMPT.
     * The optional second arg INITIAL-CONTENTS is an obsolete alternative to
     *   DEFAULT-VALUE.  It normally should be nil in new code, except when
     *   HIST is a cons.  It is discussed in more detail below.
     *
     * Third arg KEYMAP is a keymap to use whilst reading;
     *   if omitted or nil, the default is `minibuffer-local-map'.
     *
     * If fourth arg READ is non-nil, interpret the result as a Lisp object
     *   and return that object:
     *   in other words, do `(car (read-from-string INPUT-STRING))'
     *
     * Fifth arg HIST, if non-nil, specifies a history list and optionally
     *   the initial position in the list.  It can be a symbol, which is the
     *   history list variable to use, or a cons cell (HISTVAR . HISTPOS).
     *   In that case, HISTVAR is the history list variable to use, and
     *   HISTPOS is the initial position for use by the minibuffer history
     *   commands.  For consistency, you should also specify that element of
     *   the history as the value of INITIAL-CONTENTS.  Positions are counted
     *   starting from 1 at the beginning of the list.  If HIST is nil, the
     *   default history list `minibuffer-history' is used.  If HIST is t,
     *   history is not recorded.
     *
     *   If `history-add-new-input' is non-nil (the default), the result will
     *   be added to the history list using `add-to-history'.
     *
     * Sixth arg DEFAULT-VALUE, if non-nil, should be a string, which is used
     *   as the default to `read' if READ is non-nil and the user enters
     *   empty input.  But if READ is nil, this function does _not_ return
     *   DEFAULT-VALUE for empty input!  Instead, it returns the empty string.
     *
     *   Whatever the value of READ, DEFAULT-VALUE is made available via the
     *   minibuffer history commands.  DEFAULT-VALUE can also be a list of
     *   strings, in which case all the strings are available in the history,
     *   and the first string is the default to `read' if READ is non-nil.
     *
     * Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
     *  the current input method and the setting of `enable-multibyte-characters'.
     *
     * If the variable `minibuffer-allow-text-properties' is non-nil
     *  (either let-bound or buffer-local in the minibuffer),
     *  then the string which is returned includes whatever text properties
     *  were present in the minibuffer.  Otherwise the value has no text properties.
     *
     * If `inhibit-interaction' is non-nil, this function will signal an
     *   `inhibited-interaction' error.
     *
     * The remainder of this documentation string describes the
     * INITIAL-CONTENTS argument in more detail.  It is only relevant when
     * studying existing code, or when HIST is a cons.  If non-nil,
     * INITIAL-CONTENTS is a string to be inserted into the minibuffer before
     * reading input.  Normally, point is put at the end of that string.
     * However, if INITIAL-CONTENTS is (STRING . POSITION), the initial
     * input is STRING, but point is placed at _one-indexed_ position
     * POSITION in the minibuffer.  Any integer value less than or equal to
     * one puts point at the beginning of the string.  *Note* that this
     * behavior differs from the way such arguments are used in `completing-read'
     * and some related functions, which use zero-indexing for POSITION.
     * </pre>
     */
    @ELispBuiltIn(name = "read-from-minibuffer", minArgs = 1, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FReadFromMinibuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readFromMinibuffer(Object prompt, Object initialContents, Object keymap, Object read, Object hist, Object defaultValue, Object inheritInputMethod) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read a string from the minibuffer, prompting with string PROMPT.
     * If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
     *   This argument has been superseded by DEFAULT-VALUE and should normally be nil
     *   in new code.  It behaves as INITIAL-CONTENTS in `read-from-minibuffer' (which
     *   see).
     * The third arg HISTORY, if non-nil, specifies a history list
     *   and optionally the initial position in the list.
     * See `read-from-minibuffer' for details of HISTORY argument.
     * Fourth arg DEFAULT-VALUE is the default value or the list of default values.
     *  If non-nil, it is used for history commands, and as the value (or the first
     *  element of the list of default values) to return if the user enters the
     *  empty string.
     * Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
     *  the current input method and the setting of `enable-multibyte-characters'.
     * </pre>
     */
    @ELispBuiltIn(name = "read-string", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FReadString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readString(Object prompt, Object initialInput, Object history, Object defaultValue, Object inheritInputMethod) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read the name of a command and return as a symbol.
     * Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
     * if it is a list.  If DEFAULT-VALUE is omitted or nil, and the user enters
     * null input, return a symbol whose name is an empty string.
     * </pre>
     */
    @ELispBuiltIn(name = "read-command", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FReadCommand extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readCommand(Object prompt, Object defaultValue) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * One arg PROMPT, a string.  Read the name of a function and return as a symbol.
     * Prompt with PROMPT.
     * </pre>
     */
    @ELispBuiltIn(name = "read-function", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReadFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readFunction(Object prompt) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read the name of a user option and return it as a symbol.
     * Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
     * if it is a list of strings.
     * A user option, or customizable variable, is one for which
     * `custom-variable-p' returns non-nil.
     * </pre>
     */
    @ELispBuiltIn(name = "read-variable", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FReadVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readVariable(Object prompt, Object defaultValue) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read the name of a buffer and return it as a string.
     * Prompt with PROMPT, which should be a string ending with a colon and a space.
     * Provides completion on buffer names the user types.
     * Optional second arg DEF is value to return if user enters an empty line,
     *  instead of that empty string.
     *  If DEF is a list of default values, return its first element.
     * Optional third arg REQUIRE-MATCH has the same meaning as the
     *  REQUIRE-MATCH argument of `completing-read'.
     * Optional arg PREDICATE, if non-nil, is a function limiting the buffers that
     * can be considered.  It will be called with each potential candidate, in
     * the form of either a string or a cons cell whose `car' is a string, and
     * should return non-nil to accept the candidate for completion, nil otherwise.
     * If `read-buffer-completion-ignore-case' is non-nil, completion ignores
     * case while reading the buffer name.
     * If `read-buffer-function' is non-nil, this works by calling it as a
     * function, instead of the usual behavior.
     * </pre>
     */
    @ELispBuiltIn(name = "read-buffer", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FReadBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readBuffer(Object prompt, Object def, Object requireMatch, Object predicate) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return longest common substring of all completions of STRING in COLLECTION.
     *
     * Test each possible completion specified by COLLECTION
     * to see if it begins with STRING.  The possible completions may be
     * strings or symbols.  Symbols are converted to strings before testing,
     * by using `symbol-name'.
     *
     * If no possible completions match, the function returns nil; if
     * there's just one exact match, it returns t; otherwise it returns
     * the longest initial substring common to all possible completions
     * that begin with STRING.
     *
     * If COLLECTION is an alist, the keys (cars of elements) are the
     * possible completions.  If an element is not a cons cell, then the
     * element itself is a possible completion.
     * If COLLECTION is a hash-table, all the keys that are either strings
     * or symbols are the possible completions.
     * If COLLECTION is an obarray, the names of all symbols in the obarray
     * are the possible completions.
     *
     * COLLECTION can also be a function to do the completion itself.
     * It receives three arguments: STRING, PREDICATE and nil.
     * Whatever it returns becomes the value of `try-completion'.
     *
     * If optional third argument PREDICATE is non-nil, it must be a function
     * of one or two arguments, and is used to test each possible completion.
     * A possible completion is accepted only if PREDICATE returns non-nil.
     *
     * The argument given to PREDICATE is either a string or a cons cell (whose
     * car is a string) from the alist, or a symbol from the obarray.
     * If COLLECTION is a hash-table, PREDICATE is called with two arguments:
     * the string key and the associated value.
     *
     * To be acceptable, a possible completion must also match all the regexps
     * in `completion-regexp-list' (unless COLLECTION is a function, in
     * which case that function should itself handle `completion-regexp-list').
     *
     * If `completion-ignore-case' is non-nil, possible completions are matched
     * while ignoring letter-case, but no guarantee is made about the letter-case
     * of the return value, except that it comes either from the user's input
     * or from one of the possible completions.
     * </pre>
     */
    @ELispBuiltIn(name = "try-completion", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTryCompletion extends ELispBuiltInBaseNode {
        @Specialization
        public Object tryCompletion(ELispString string, Object collection, Object predicate,
                                    @Cached FunctionDispatchNode dispatchNode) {
            if (isNil(collection)) {
                return false;
            }
            TryCompletionMatcher matcher = new TryCompletionMatcher(string, predicate, dispatchNode, this);
            if (!matcher.forEachInCollection(collection)) {
                return BuiltInEval.FFuncall.funcall(collection, new Object[]{string, predicate, false});
            }
            return matcher.reduce();
        }

        private static final class TryCompletionMatcher extends CompletionMatcher {
            private final ArrayList<MuleString> matches = new ArrayList<>();

            TryCompletionMatcher(ELispString target, Object predicate, FunctionDispatchNode dispatchNode, Node node) {
                super(target, predicate, dispatchNode, node);
            }

            @Override
            protected void record(MuleString string, Object key) {
                matches.add(string);
            }

            public Object reduce() {
                if (matches.isEmpty()) {
                    return false;
                }
                if (matches.size() == 1) {
                    return true;
                }
                MuleStringBuffer buffer = new MuleStringBuffer();
                int index = target.length();
                MuleString first = matches.getFirst();
                buffer.appendMuleString(first, 0, index);
                appendCommonChar:
                while (index < first.length()) {
                    int originalChar = first.charAt(index);
                    int upper = ignoreCase ? Math.toIntExact(BuiltInCaseFiddle.FUpcase.upcaseChar(originalChar)) : originalChar;
                    for (int i = 1; i < matches.size(); i++) {
                        MuleString matched = matches.get(i);
                        if (index >= matched.length()) {
                            break appendCommonChar;
                        }
                        int c = matched.charAt(index);
                        if (ignoreCase) {
                            c = Math.toIntExact(BuiltInCaseFiddle.FUpcase.upcaseChar(c));
                        }
                        if (c != upper) {
                            break appendCommonChar;
                        }
                    }
                    buffer.append(originalChar);
                    index++;
                }
                return new ELispString(buffer.build());
            }
        }

    }

    /**
     * <pre>
     * Search for partial matches of STRING in COLLECTION.
     *
     * Test each possible completion specified by COLLECTION
     * to see if it begins with STRING.  The possible completions may be
     * strings or symbols.  Symbols are converted to strings before testing,
     * by using `symbol-name'.
     *
     * The value is a list of all the possible completions that match STRING.
     *
     * If COLLECTION is an alist, the keys (cars of elements) are the
     * possible completions.  If an element is not a cons cell, then the
     * element itself is the possible completion.
     * If COLLECTION is a hash-table, all the keys that are strings or symbols
     * are the possible completions.
     * If COLLECTION is an obarray, the names of all symbols in the obarray
     * are the possible completions.
     *
     * COLLECTION can also be a function to do the completion itself.
     * It receives three arguments: STRING, PREDICATE and t.
     * Whatever it returns becomes the value of `all-completions'.
     *
     * If optional third argument PREDICATE is non-nil, it must be a function
     * of one or two arguments, and is used to test each possible completion.
     * A possible completion is accepted only if PREDICATE returns non-nil.
     *
     * The argument given to PREDICATE is either a string or a cons cell (whose
     * car is a string) from the alist, or a symbol from the obarray.
     * If COLLECTION is a hash-table, PREDICATE is called with two arguments:
     * the string key and the associated value.
     *
     * To be acceptable, a possible completion must also match all the regexps
     * in `completion-regexp-list' (unless COLLECTION is a function, in
     * which case that function should itself handle `completion-regexp-list').
     *
     * An obsolete optional fourth argument HIDE-SPACES is still accepted for
     * backward compatibility.  If non-nil, strings in COLLECTION that start
     * with a space are ignored unless STRING itself starts with a space.
     * </pre>
     */
    @ELispBuiltIn(name = "all-completions", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FAllCompletions extends ELispBuiltInBaseNode {
        @Specialization
        public Object allCompletions(ELispString string, Object collection, Object predicate, Object hideSpaces,
                                     @Cached FunctionDispatchNode dispatchNode) {
            if (!isNil(hideSpaces)) {
                throw new UnsupportedOperationException("all-completions: HIDE-SPACES is not supported");
            }
            if (isNil(collection)) {
                return false;
            }
            AllCompletionMatcher matcher = new AllCompletionMatcher(string, predicate, dispatchNode, this);
            if (!matcher.forEachInCollection(collection)) {
                return BuiltInEval.FFuncall.funcall(collection, new Object[]{string, predicate, true});
            }
            return matcher.reduce();
        }

        private static final class AllCompletionMatcher extends CompletionMatcher {
            private final ArrayList<Object> matches = new ArrayList<>();

            AllCompletionMatcher(ELispString target, Object predicate, FunctionDispatchNode dispatchNode, Node node) {
                super(target, predicate, dispatchNode, node);
            }

            @Override
            protected void record(MuleString string, Object key) {
                matches.add(key);
            }

            public Object reduce() {
                ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
                for (Object match : matches) {
                    builder.add(match);
                }
                return builder.build();
            }
        }
    }

    /**
     * <pre>
     * Read a string in the minibuffer, with completion.
     * PROMPT is a string to prompt with; normally it ends in a colon and a space.
     * COLLECTION can be a list of strings, an alist, an obarray or a hash table.
     * COLLECTION can also be a function to do the completion itself.
     * PREDICATE limits completion to a subset of COLLECTION.
     * See `try-completion', `all-completions', `test-completion',
     * and `completion-boundaries', for more details on completion,
     * COLLECTION, and PREDICATE.  See also Info node `(elisp)Basic Completion'
     * for the details about completion, and Info node `(elisp)Programmed
     * Completion' for expectations from COLLECTION when it's a function.
     *
     * REQUIRE-MATCH can take the following values:
     * - t means that the user is not allowed to exit unless the input is (or
     *   completes to) an element of COLLECTION or is null.
     * - nil means that the user can exit with any input.
     * - `confirm' means that the user can exit with any input, but she needs
     *   to confirm her choice if the input is not an element of COLLECTION.
     * - `confirm-after-completion' means that the user can exit with any
     *   input, but she needs to confirm her choice if she called
     *   `minibuffer-complete' right before `minibuffer-complete-and-exit'
     *   and the input is not an element of COLLECTION.
     * - a function, which will be called with the input as the
     *   argument.  If the function returns a non-nil value, the
     *   minibuffer is exited with that argument as the value.
     * - anything else behaves like t except that typing RET does not exit if it
     *   does non-null completion.
     *
     * If the input is null, `completing-read' returns DEF, or the first
     * element of the list of default values, or an empty string if DEF is
     * nil, regardless of the value of REQUIRE-MATCH.
     *
     * If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
     *   with point positioned at the end.  If it is (STRING . POSITION), the
     *   initial input is STRING, but point is placed at _zero-indexed_
     *   position POSITION in STRING.  (*Note* that this is different from
     *   `read-from-minibuffer' and related functions, which use one-indexing
     *   for POSITION.)  This feature is deprecated--it is best to pass nil
     *   for INITIAL-INPUT and supply the default value DEF instead.  The
     *   user can yank the default value into the minibuffer easily using
     *   \\&lt;minibuffer-local-map&gt;\\[next-history-element].
     *
     * HIST, if non-nil, specifies a history list and optionally the initial
     *   position in the list.  It can be a symbol, which is the history list
     *   variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
     *   that case, HISTVAR is the history list variable to use, and HISTPOS
     *   is the initial position (the position in the list used by the
     *   minibuffer history commands).  For consistency, you should also
     *   specify that element of the history as the value of INITIAL-INPUT.
     *   (This is the only case in which you should use INITIAL-INPUT instead
     *   of DEF.)  Positions are counted starting from 1 at the beginning of
     *   the list.  The variable `history-length' controls the maximum length
     *   of a history list.  If HIST is t, history is not recorded.
     *
     * DEF, if non-nil, is the default value or the list of default values.
     *
     * If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
     *   current input method and the setting of `enable-multibyte-characters'.
     *
     * Completion ignores case if the ambient value of
     *   `completion-ignore-case' is non-nil.
     *
     * See also `completing-read-function'.
     * </pre>
     */
    @ELispBuiltIn(name = "completing-read", minArgs = 2, maxArgs = 8)
    @GenerateNodeFactory
    public abstract static class FCompletingRead extends ELispBuiltInBaseNode {
        @Specialization
        public static Void completingRead(Object prompt, Object collection, Object predicate, Object requireMatch, Object initialInput, Object hist, Object def, Object inheritInputMethod) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if STRING is a valid completion.
     * For instance, if COLLECTION is a list of strings, STRING is a
     * valid completion if it appears in the list and PREDICATE is satisfied.
     *
     * Takes the same arguments as `all-completions' and `try-completion'.
     *
     * If COLLECTION is a function, it is called with three arguments:
     * the values STRING, PREDICATE and `lambda'.
     * </pre>
     */
    @ELispBuiltIn(name = "test-completion", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTestCompletion extends ELispBuiltInBaseNode {
        @Specialization
        public Object testCompletion(ELispString string, Object collection, Object predicate,
                                     @Cached FunctionDispatchNode dispatchNode) {
            if (isNil(collection)) {
                return false;
            }
            // TODO: Does Emacs work like this?
            //   "STRING is a valid completion if it appears in the list and PREDICATE is satisfied."
            //   No ignore case, no regexp or anything?
            TestCompletionMatcher matcher = new TestCompletionMatcher(string, predicate, dispatchNode, this);
            if (!matcher.forEachInCollection(collection)) {
                return BuiltInEval.FFuncall.funcall(collection, new Object[]{string, predicate, LAMBDA});
            }
            return matcher.matched;
        }

        private static final class TestCompletionMatcher extends CompletionMatcher {
            boolean matched = false;

            TestCompletionMatcher(ELispString target, Object predicate, FunctionDispatchNode dispatchNode, Node node) {
                super(target, predicate, dispatchNode, node);
            }

            @Override
            protected void record(MuleString string, Object key) {
                matched = true;
            }
        }
    }

    /**
     * <pre>
     * Perform completion on buffer names.
     * STRING and PREDICATE have the same meanings as in `try-completion',
     * `all-completions', and `test-completion'.
     *
     * If FLAG is nil, invoke `try-completion'; if it is t, invoke
     * `all-completions'; otherwise invoke `test-completion'.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-complete-buffer", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalCompleteBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalCompleteBuffer(Object string, Object predicate, Object flag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Like `assoc' but specifically for strings (and symbols).
     *
     * This returns the first element of LIST whose car matches the string or
     * symbol KEY, or nil if no match exists.  When performing the
     * comparison, symbols are first converted to strings, and unibyte
     * strings to multibyte.  If the optional arg CASE-FOLD is non-nil, both
     * KEY and the elements of LIST are upcased for comparison.
     *
     * Unlike `assoc', KEY can also match an entry in LIST consisting of a
     * single string, rather than a cons cell whose car is a string.
     * </pre>
     */
    @ELispBuiltIn(name = "assoc-string", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAssocString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assocString(Object key, Object list, Object caseFold) {
            MuleString keyString = key instanceof ELispSymbol sym ? sym.name() : asStr(key).value();
            boolean upcase = !isNil(caseFold);
            if (upcase) {
                keyString = asStr(BuiltInCaseFiddle.FUpcase.upcaseString(new ELispString(keyString))).value();
            }
            for (Object entry : asConsOrNil(list)) {
                Object target = entry;
                if (target instanceof ELispCons cons) {
                    target = cons.car();
                }
                MuleString rhs;
                if (target instanceof ELispSymbol sym) {
                    rhs = sym.name();
                } else if (target instanceof ELispString str) {
                    rhs = str.value();
                } else {
                    continue;
                }
                if (upcase) {
                    rhs = asStr(BuiltInCaseFiddle.FUpcase.upcaseString(new ELispString(rhs))).value();
                }
                if (keyString.equals(rhs)) {
                    return entry;
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return current depth of activations of minibuffer, a nonnegative integer.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-depth", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMinibufferDepth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferDepth() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the prompt string of the currently-active minibuffer.
     * If no minibuffer is active, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-prompt", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMinibufferPrompt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferPrompt() {
            throw new UnsupportedOperationException();
        }
    }
}
