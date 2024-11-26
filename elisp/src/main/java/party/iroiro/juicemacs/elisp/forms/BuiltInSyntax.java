package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.Collections;
import java.util.List;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.BuiltInEditFns.currentBuffer;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInSyntax extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        for (int i = 0; i < SMAX; i++) {
            SYNTAX_CODE_OBJECT.set(i, new ELispCons((long) i));
        }
        BuiltInFns.FPut.put(SYNTAX_TABLE, CHAR_TABLE_EXTRA_SLOTS, 0L);
        //noinspection SequencedCollectionMethodCanBeUsed
        Object whitespace = SYNTAX_CODE_OBJECT.get(SWHITESPACE);
        standardSyntaxTable = BuiltInCharTab.FMakeCharTable.makeCharTable(SYNTAX_TABLE, whitespace);
        ELispBuffer.DEFAULT_VALUES.setSyntaxTable(standardSyntaxTable);

        // Control characters
        Object punctuation = SYNTAX_CODE_OBJECT.get(SPUNCT);
        for (int i = 0; i < ' ' - 1; i++) {
            standardSyntaxTable.setChar(i, punctuation);
        }
        standardSyntaxTable.setChar(127, punctuation);

        // Whitespace control characters
        standardSyntaxTable.setChar(' ', whitespace);
        standardSyntaxTable.setChar('\t', whitespace);
        standardSyntaxTable.setChar('\n', whitespace);
        standardSyntaxTable.setChar('\r', whitespace);
        standardSyntaxTable.setChar('\f', whitespace);

        // Word
        Object word = SYNTAX_CODE_OBJECT.get(SWORD);
        for (int i = 'a'; i <= 'z'; i++) {
            standardSyntaxTable.setChar(i, word);
        }
        for (int i = 'A'; i <= 'Z'; i++) {
            standardSyntaxTable.setChar(i, word);
        }
        for (int i = '0'; i <= '9'; i++) {
            standardSyntaxTable.setChar(i, word);
        }
        standardSyntaxTable.setChar('$', word);
        standardSyntaxTable.setChar('%', word);

        standardSyntaxTable.setChar('(', new ELispCons((long) SOPEN, (long) ')'));
        standardSyntaxTable.setChar(')', new ELispCons((long) SCLOSE, (long) '('));
        standardSyntaxTable.setChar('[', new ELispCons((long) SOPEN, (long) ']'));
        standardSyntaxTable.setChar(']', new ELispCons((long) SCLOSE, (long) '['));
        standardSyntaxTable.setChar('{', new ELispCons((long) SOPEN, (long) '}'));
        standardSyntaxTable.setChar('}', new ELispCons((long) SCLOSE, (long) '{'));
        standardSyntaxTable.setChar('"', new ELispCons((long) SSTRING));
        standardSyntaxTable.setChar('\\', new ELispCons((long) SESCAPE));

        Object symbol = SYNTAX_CODE_OBJECT.get(SSYMBOL);
        String symbols = "_-+*/&|<>=";
        for (char c : symbols.toCharArray()) {
            standardSyntaxTable.setChar(c, symbol);
        }

        String punctuations = ".,;:?!#@~^'`";
        for (char c : punctuations.toCharArray()) {
            standardSyntaxTable.setChar(c, punctuation);
        }

        standardSyntaxTable.setRange(0x80, ELispCharTable.MAX_CHAR, word);

        return BuiltInSyntaxFactory.getFactories();
    }

    private static ELispCharTable standardSyntaxTable;

    private final static ELispVector SYNTAX_CODE_OBJECT = new ELispVector(Collections.nCopies(SMAX, false));

    public static byte checkSyntaxChar(int next) {
        return switch (next) {
            case ' ', '-' -> SWHITESPACE; // Whitespace
            case '.' -> SPUNCT; // Punctuation
            case 'w' -> SWORD; // Word
            case '_' -> SSYMBOL; // Symbol
            case '(' -> SOPEN; // Open paren
            case ')' -> SCLOSE; // Close paren
            case '\'' -> SQUOTE; // Expression prefix
            case '"' -> SSTRING; // String quote
            case '$' -> SMATH; // Paired delim
            case '\\' -> SESCAPE; // Escape
            case '/' -> SCHARQUOTE; // Character quote
            case '<' -> SCOMMENT; // Comment start
            case '>' -> SENDCOMMENT; // Comment end
            case '@' -> SINHERIT; // Inherit standard syntax
            case '!' -> SCOMMENT_FENCE; // Generic comment delimiters
            case '|' -> SSTRING_FENCE; // Generic string delimiters
            default -> throw ELispSignals.error("Invalid syntax character");
        };
    }

    /**
     * <pre>
     * Return t if OBJECT is a syntax table.
     * Currently, any char-table counts as a syntax table.
     * </pre>
     */
    @ELispBuiltIn(name = "syntax-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSyntaxTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void syntaxTableP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current syntax table.
     * This is the one specified by the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "syntax-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSyntaxTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void syntaxTable() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the standard syntax table.
     * This is the one used for new buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "standard-syntax-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FStandardSyntaxTable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCharTable standardSyntaxTable() {
            return standardSyntaxTable;
        }
    }

    /**
     * <pre>
     * Construct a new syntax table and return it.
     * It is a copy of the TABLE, which defaults to the standard syntax table.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-syntax-table", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopySyntaxTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copySyntaxTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Select a new syntax table for the current buffer.
     * One argument, a syntax table.
     * </pre>
     */
    @ELispBuiltIn(name = "set-syntax-table", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetSyntaxTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setSyntaxTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the syntax code of CHARACTER, described by a character.
     * For example, if CHARACTER is a word constituent, the
     * character `w' (119) is returned.
     * The characters that correspond to various syntax codes
     * are listed in the documentation of `modify-syntax-entry'.
     *
     * If you're trying to determine the syntax of characters in the buffer,
     * this is probably the wrong function to use, because it can't take
     * `syntax-table' text properties into account.  Consider using
     * `syntax-after' instead.
     * </pre>
     */
    @ELispBuiltIn(name = "char-syntax", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharSyntax extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charSyntax(Object character) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the syntax char of CLASS, described by an integer.
     * For example, if SYNTAX is word constituent (the integer 2), the
     * character `w' (119) is returned.
     * </pre>
     */
    @ELispBuiltIn(name = "syntax-class-to-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSyntaxClassToChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void syntaxClassToChar(Object syntax) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the matching parenthesis of CHARACTER, or nil if none.
     * </pre>
     */
    @ELispBuiltIn(name = "matching-paren", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchingParen extends ELispBuiltInBaseNode {
        @Specialization
        public static Void matchingParen(Object character) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert a syntax descriptor STRING into a raw syntax descriptor.
     * STRING should be a string of the form allowed as argument of
     * `modify-syntax-entry'.  The return value is a raw syntax descriptor: a
     * cons cell (CODE . MATCHING-CHAR) which can be used, for example, as
     * the value of a `syntax-table' text property.
     * </pre>
     */
    @ELispBuiltIn(name = "string-to-syntax", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToSyntax extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToSyntax(ELispString string) {
            PrimitiveIterator.OfInt iterator = string.value().iterator(0);
            // char 1: syntax char
            long syntax = checkSyntaxChar(iterator.nextInt());
            if (syntax == SINHERIT) {
                return false;
            }
            // (optional) char 2: matching char
            long match = -1;
            if (iterator.hasNext()) {
                match = iterator.nextInt();
                if (match == ' ') {
                    match = -1;
                }
            }
            // (optional) flags
            while (iterator.hasNext()) {
                int shift = switch (iterator.nextInt()) {
                    case '1' -> 16;
                    case '2' -> 17;
                    case '3' -> 18;
                    case '4' -> 19;
                    case 'p' -> 20;
                    case 'b' -> 21;
                    case 'n' -> 22;
                    case 'c' -> 23;
                    default -> 0;
                };
                if (shift != 0) {
                    syntax |= 1 << shift;
                }
            }
            return new ELispCons(syntax, match == -1 ? false : match);
        }
    }

    /**
     * <pre>
     * Set syntax for character CHAR according to string NEWENTRY.
     * The syntax is changed only for table SYNTAX-TABLE, which defaults to
     *  the current buffer's syntax table.
     * CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
     * in the range MIN to MAX are changed.
     * The first character of NEWENTRY should be one of the following:
     *   Space or -  whitespace syntax.    w   word constituent.
     *   _           symbol constituent.   .   punctuation.
     *   (           open-parenthesis.     )   close-parenthesis.
     *   "           string quote.         \\   escape.
     *   $           paired delimiter.     \\='   expression quote or prefix operator.
     *   &lt;           comment starter.      &gt;   comment ender.
     *   /           character-quote.      @   inherit from parent table.
     *   |           generic string fence. !   generic comment fence.
     *
     * Only single-character comment start and end sequences are represented thus.
     * Two-character sequences are represented as described below.
     * The second character of NEWENTRY is the matching parenthesis,
     *  used only if the first character is `(' or `)'.
     * Any additional characters are flags.
     * Defined flags are the characters 1, 2, 3, 4, b, p, and n.
     *  1 means CHAR is the start of a two-char comment start sequence.
     *  2 means CHAR is the second character of such a sequence.
     *  3 means CHAR is the start of a two-char comment end sequence.
     *  4 means CHAR is the second character of such a sequence.
     *
     * There can be several orthogonal comment sequences.  This is to support
     * language modes such as C++.  By default, all comment sequences are of style
     * a, but you can set the comment sequence style to b (on the second character
     * of a comment-start, and the first character of a comment-end sequence) and/or
     * c (on any of its chars) using this flag:
     *  b means CHAR is part of comment sequence b.
     *  c means CHAR is part of comment sequence c.
     *  n means CHAR is part of a nestable comment sequence.
     *
     *  p means CHAR is a prefix character for `backward-prefix-chars';
     *    such characters are treated as whitespace when they occur
     *    between expressions.
     * usage: (modify-syntax-entry CHAR NEWENTRY &amp;optional SYNTAX-TABLE)
     * </pre>
     */
    @ELispBuiltIn(name = "modify-syntax-entry", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FModifySyntaxEntry extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean modifySyntaxEntry(Object char_, Object newentry, Object syntaxTable) {
            ELispCharTable table = asCharTable(isNil(syntaxTable) ? currentBuffer().getSyntaxTable() : syntaxTable);
            newentry = FStringToSyntax.stringToSyntax(asStr(newentry));
            if (char_ instanceof ELispCons cons) {
                BuiltInCharTab.FSetCharTableRange.setCharTableRange(table, cons, newentry);
            } else {
                table.setChar(asChar(char_), newentry);
            }
            return false;
        }
    }

    /**
     * <pre>
     * Insert a description of the internal syntax description SYNTAX at point.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-describe-syntax-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalDescribeSyntaxValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalDescribeSyntaxValue(Object syntax) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point forward ARG words (backward if ARG is negative).
     * If ARG is omitted or nil, move point forward one word.
     * Normally returns t.
     * If an edge of the buffer or a field boundary is reached, point is
     * left there and the function returns nil.  Field boundaries are not
     * noticed if `inhibit-field-text-motion' is non-nil.
     *
     * The word boundaries are normally determined by the buffer's syntax
     * table and character script (according to `char-script-table'), but
     * `find-word-boundary-function-table', such as set up by `subword-mode',
     * can change that.  If a Lisp program needs to move by words determined
     * strictly by the syntax table, it should use `forward-word-strictly'
     * instead.  See Info node `(elisp) Word Motion' for details.
     * </pre>
     */
    @ELispBuiltIn(name = "forward-word", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForwardWord extends ELispBuiltInBaseNode {
        @Specialization
        public static Void forwardWord(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point forward, stopping before a char not in STRING, or at pos LIM.
     * STRING is like the inside of a `[...]' in a regular expression
     * except that `]' is never special and `\\' quotes `^', `-' or `\\'
     *  (but not at the end of a range; quoting is never needed there).
     * Thus, with arg "a-zA-Z", this skips letters stopping before first nonletter.
     * With arg "^a-zA-Z", skips nonletters stopping before first letter.
     * Char classes, e.g. `[:alpha:]', are supported.
     *
     * Returns the distance traveled, either zero or positive.
     * </pre>
     */
    @ELispBuiltIn(name = "skip-chars-forward", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSkipCharsForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void skipCharsForward(Object string, Object lim) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point backward, stopping after a char not in STRING, or at pos LIM.
     * See `skip-chars-forward' for details.
     * Returns the distance traveled, either zero or negative.
     * </pre>
     */
    @ELispBuiltIn(name = "skip-chars-backward", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSkipCharsBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void skipCharsBackward(Object string, Object lim) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point forward across chars in specified syntax classes.
     * SYNTAX is a string of syntax code characters.
     * Stop before a char whose syntax is not in SYNTAX, or at position LIM.
     * If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
     * This function returns the distance traveled, either zero or positive.
     * </pre>
     */
    @ELispBuiltIn(name = "skip-syntax-forward", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSkipSyntaxForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void skipSyntaxForward(Object syntax, Object lim) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point backward across chars in specified syntax classes.
     * SYNTAX is a string of syntax code characters.
     * Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
     * If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
     * This function returns either zero or a negative number, and the absolute value
     * of this is the distance traveled.
     * </pre>
     */
    @ELispBuiltIn(name = "skip-syntax-backward", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSkipSyntaxBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void skipSyntaxBackward(Object syntax, Object lim) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move forward across up to COUNT comments.  If COUNT is negative, move backward.
     * Stop scanning if we find something other than a comment or whitespace.
     * Set point to where scanning stops.
     * If COUNT comments are found as expected, with nothing except whitespace
     * between them, return t; otherwise return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "forward-comment", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForwardComment extends ELispBuiltInBaseNode {
        @Specialization
        public static Void forwardComment(Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Scan from character number FROM by COUNT lists.
     * Scan forward if COUNT is positive, backward if COUNT is negative.
     * Return the character number of the position thus found.
     *
     * A \"list", in this context, refers to a balanced parenthetical
     * grouping, as determined by the syntax table.
     *
     * If DEPTH is nonzero, treat that as the nesting depth of the starting
     * point (i.e. the starting point is DEPTH parentheses deep).  This
     * function scans over parentheses until the depth goes to zero COUNT
     * times.  Hence, positive DEPTH moves out that number of levels of
     * parentheses, while negative DEPTH moves to a deeper level.
     *
     * Comments are ignored if `parse-sexp-ignore-comments' is non-nil.
     *
     * If we reach the beginning or end of the accessible part of the buffer
     * before we have scanned over COUNT lists, return nil if the depth at
     * that point is zero, and signal an error if the depth is nonzero.
     * </pre>
     */
    @ELispBuiltIn(name = "scan-lists", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FScanLists extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scanLists(Object from, Object count, Object depth) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Scan from character number FROM by COUNT balanced expressions.
     * If COUNT is negative, scan backwards.
     * Returns the character number of the position thus found.
     *
     * Comments are ignored if `parse-sexp-ignore-comments' is non-nil.
     *
     * If the beginning or end of (the accessible part of) the buffer is reached
     * in the middle of a parenthetical grouping, an error is signaled.
     * If the beginning or end is reached between groupings
     * but before count is used up, nil is returned.
     * </pre>
     */
    @ELispBuiltIn(name = "scan-sexps", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FScanSexps extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scanSexps(Object from, Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point backward over any number of chars with prefix syntax.
     * This includes chars with expression prefix syntax class (\\=') and those with
     * the prefix syntax flag (p).
     * </pre>
     */
    @ELispBuiltIn(name = "backward-prefix-chars", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBackwardPrefixChars extends ELispBuiltInBaseNode {
        @Specialization
        public static Void backwardPrefixChars() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
     * Parsing stops at TO or when certain criteria are met;
     *  point is set to where parsing stops.
     *
     * If OLDSTATE is omitted or nil, parsing assumes that FROM is the
     *  beginning of a function.  If not, OLDSTATE should be the state at
     *  FROM.
     *
     * Value is a list of elements describing final state of parsing:
     *  0. depth in parens.
     *  1. character address of start of innermost containing list; nil if none.
     *  2. character address of start of last complete sexp terminated.
     *  3. non-nil if inside a string.
     *     (it is the character that will terminate the string,
     *      or t if the string should be terminated by a generic string delimiter.)
     *  4. nil if outside a comment, t if inside a non-nestable comment,
     *     else an integer (the current comment nesting).
     *  5. t if following a quote character.
     *  6. the minimum paren-depth encountered during this scan.
     *  7. style of comment, if any.
     *  8. character address of start of comment or string; nil if not in one.
     *  9. List of positions of currently open parens, outermost first.
     * 10. When the last position scanned holds the first character of a
     *     (potential) two character construct, the syntax of that position,
     *     otherwise nil.  That construct can be a two character comment
     *     delimiter or an Escaped or Char-quoted character.
     * 11..... Possible further internal information used by `parse-partial-sexp'.
     *
     * If third arg TARGETDEPTH is non-nil, parsing stops if the depth
     * in parentheses becomes equal to TARGETDEPTH.
     * Fourth arg STOPBEFORE non-nil means stop when we come to
     *  any character that starts a sexp.
     * Fifth arg OLDSTATE is a list like what this function returns.
     *  It is used to initialize the state of the parse.  Elements number 1, 2, 6
     *  are ignored.
     * Sixth arg COMMENTSTOP non-nil means stop after the start of a comment.
     *  If it is the symbol `syntax-table', stop after the start of a comment or a
     *  string, or after end of a comment or a string.
     * </pre>
     */
    @ELispBuiltIn(name = "parse-partial-sexp", minArgs = 2, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FParsePartialSexp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void parsePartialSexp(Object from, Object to, Object targetdepth, Object stopbefore, Object oldstate, Object commentstop) {
            throw new UnsupportedOperationException();
        }
    }
}
