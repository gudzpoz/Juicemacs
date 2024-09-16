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

    @ELispBuiltIn(name = "looking-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLookingAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lookingAt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "posix-looking-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPosixLookingAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixLookingAt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-match", minArgs = 2, maxArgs = 4)
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

    @ELispBuiltIn(name = "posix-string-match", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPosixStringMatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixStringMatch(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "search-backward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object searchBackward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "search-forward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object searchForward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "re-search-backward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FReSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reSearchBackward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "re-search-forward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FReSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reSearchForward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "posix-search-backward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPosixSearchBackward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixSearchBackward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "posix-search-forward", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPosixSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posixSearchForward(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "replace-match", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FReplaceMatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object replaceMatch(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-beginning", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchBeginning(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-end", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchEnd(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-data", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMatchData extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchData(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-match-data", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetMatchData extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setMatchData(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "match-data--translate", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMatchDataTranslate extends ELispBuiltInBaseNode {
        @Specialization
        public static Object matchDataTranslate(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "regexp-quote", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRegexpQuote extends ELispBuiltInBaseNode {
        @Specialization
        public static Object regexpQuote(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "newline-cache-check", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNewlineCacheCheck extends ELispBuiltInBaseNode {
        @Specialization
        public static Object newlineCacheCheck(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "re--describe-compiled", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FReDescribeCompiled extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reDescribeCompiled(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }
}
