package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSyntaxTable;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class ELispRegExp {
    public abstract MatcherResult matcher(CharSequence input, @Nullable ELispSyntaxTable syntaxTable);

    public record MatcherResult(Matcher matcher, int pointGroup) {
        public int getPointPosition() {
            if (pointGroup == -1) {
                return -1;
            }
            return matcher.start("point");
        }

        public int groupCount() {
            return pointGroup == -1 ? matcher.groupCount() : matcher.groupCount() - 1;
        }

        public int normalizeGroup(int group) {
            if (pointGroup == -1) {
                return group;
            }
            if (group < pointGroup) {
                return group;
            }
            return group + 1;
        }
    }

    private static final class OnDemandRegExp extends ELispRegExp {
        private final AbstractTruffleString exp;
        private final boolean ignoreCase;
        @Nullable
        private ELispSyntaxTable cachedSyntaxTable;
        @Nullable
        private PrecompiledRegExp cachedPattern;

        public OnDemandRegExp(AbstractTruffleString exp, boolean ignoreCase) {
            this.exp = exp;
            this.ignoreCase = ignoreCase;
        }

        @Override
        public MatcherResult matcher(CharSequence input, ELispSyntaxTable syntaxTable) {
            if (cachedSyntaxTable != syntaxTable || cachedPattern == null) {
                cachedSyntaxTable = syntaxTable;
                cachedPattern = compileWithSyntax(exp, ignoreCase, syntaxTable);
            }
            return Objects.requireNonNull(cachedPattern).matcher(input, syntaxTable);
        }
    }

    private static final class PrecompiledRegExp extends ELispRegExp {
        private final Pattern pattern;
        private final int pointGroup;

        public PrecompiledRegExp(Pattern pattern, int pointGroup) {
            this.pattern = pattern;
            this.pointGroup = pointGroup;
        }

        @Override
        public MatcherResult matcher(CharSequence input, ELispSyntaxTable syntaxTable) {
            return new MatcherResult(pattern.matcher(input), pointGroup);
        }
    }

    public static ELispRegExp compile(AbstractTruffleString exp, boolean ignoreCase) {
        try {
            return compileWithSyntax(exp, ignoreCase, null);
        } catch (IllegalArgumentException e) {
            return new OnDemandRegExp(exp, ignoreCase);
        }
    }

    private static PrecompiledRegExp compileWithSyntax(AbstractTruffleString exp, boolean ignoreCase, @Nullable ELispSyntaxTable syntaxTable) {
        TruffleStringIterator iterator = exp.createCodePointIteratorUncached(ELispString.ENCODING);
        StringBuilder builder = new StringBuilder();
        int groupI = 0;
        int pointGroup = -1;
        while (iterator.hasNext()) {
            int codepoint = iterator.nextUncached();
            switch (codepoint) {
                case '\\' -> {
                    int next = iterator.nextUncached();
                    switch (next) {
                        case '(' -> {
                            groupI++;
                            if (iterator.hasNext()) {
                                if (iterator.nextUncached() == '?') {
                                    if (iterator.hasNext()) {
                                        if (iterator.nextUncached() == ':') {
                                            groupI--;
                                        }
                                        iterator.previousUncached();
                                    }
                                }
                                iterator.previousUncached();
                            }
                            builder.appendCodePoint(next);
                        }
                        case ')', '|', '{', '}' -> builder.appendCodePoint(next);
                        // Lisp: matches any word-constituent character.
                        // Java: A word character: [\p{Alpha}\p{gc=Mn}\p{gc=Me}\p{gc=Mc}\p{Digit}\p{gc=Pc}\p{IsJoin_Control}]
                        // TODO: Discrepancy: syntax table
                        case 'w' -> builder.append("\\w");
                        // Lisp: matches any character that is not a word constituent.
                        // TODO: Discrepancy: syntax table
                        case 'W' -> builder.append("\\W");
                        case 's', 'S', 'c', 'C' -> {
                            // TODO: syntax tables, category tables, etc.
                            throw new IllegalArgumentException();
                        }
                        // matches the empty string, but only at the beginning of the buffer or string being matched against.
                        case '`' -> builder.append("\\A");
                        // matches the empty string, but only at the end of the buffer or string being matched against.
                        case '\'' -> builder.append("\\z");
                        // matches the empty string, but only at point. (This construct is not defined when matching against a string.)
                        case '=' -> {
                            builder.append("(?<point>)");
                            groupI++;
                            pointGroup = groupI;
                        }
                        // matches the empty string, but only at the beginning or end of a word.
                        case 'b' -> builder.append("\\b");
                        // matches the empty string, but not at the beginning or end of a word, nor at the beginning or end of the buffer (or string).
                        case 'B' -> builder.append("\\B");
                        // matches the empty string, but only at the beginning of a word.
                        case '<' -> builder.append("(?=\\w)");
                        // matches the empty string, but only at the end of a word.
                        case '>' -> builder.append("(?<=\\w)");
                        case '_' -> {
                            switch (iterator.nextUncached()) {
                                // matches the empty string, but only at the beginning of a symbol.
                                case '<' -> builder.append("(?=[^\\s\"';#()\\[\\]`,])");
                                // matches the empty string, but only at the end of a symbol.
                                case '>' -> builder.append("(?<=[^\\s\"';#()\\[\\]`,])");
                                default -> throw new IllegalArgumentException();
                            }
                        }
                        default -> {
                            if (Character.isDigit(next)) {
                                // matches the same text that matched the digitth occurrence of a grouping (‘\( … \)’) construct.
                                builder.append('\\').appendCodePoint(next);
                            } else {
                                // quotes the special characters
                                builder.appendCodePoint(next);
                            }
                        }
                    }
                }
                case '(', ')', '|', '{', '}' -> builder.append('\\').appendCodePoint(codepoint);
                case '[' -> {
                    builder.appendCodePoint('[');
                    int first = iterator.nextUncached();
                    if (first == ']') {
                        builder.append("\\]");
                    } else {
                        iterator.previousUncached();
                    }
                    while (true) {
                        int next = iterator.nextUncached();
                        if (next == '\\') {
                            builder.append("\\\\");
                            continue;
                        }
                        if (next == ']') {
                            builder.append(']');
                            break;
                        }
                        // character class: [:alnum:], etc.
                        if (next != '[') {
                            builder.appendCodePoint(next);
                            continue;
                        }
                        StringBuilder clazz = new StringBuilder();
                        int c = iterator.nextUncached();
                        if (c != ':') {
                            builder.append("\\[");
                            iterator.previousUncached();
                            continue;
                        }
                        // throwing away '[:'
                        int codepoints = 0;
                        while ((c = iterator.nextUncached()) != ':' && c != ']') {
                            codepoints++;
                            clazz.appendCodePoint(c);
                        }
                        if (c == ']') {
                            // not a character class: append as is
                            builder.append("\\[:");
                            // use previousUncached to process again
                            for (int i = 0; i < codepoints + 1; i++) {
                                iterator.previousUncached();
                            }
                            break;
                        }
                        // throwing away ':'
                        c = iterator.nextUncached();
                        if (c != ']') {
                            // still not a character class: append as is
                            builder.append("\\[:");
                            // use previousUncached to process again
                            for (int i = 0; i < codepoints + 2; i++) {
                                iterator.previousUncached();
                            }
                            continue;
                        }
                        // throwning away ']'
                        builder.append(switch (clazz.toString()) {
                            // TODO: Use syntax-table & Unicode
                            // Lisp: This matches any ASCII character (codes 0–127).
                            // Java: All ASCII:[\x00-\x7F]
                            case "ascii" -> "\\p{ASCII}";
                            // Lisp: This matches any letter or digit. For multibyte characters, it matches characters whose Unicode ‘general-category’ property
                            //       (see Character Properties) indicates they are alphabetic or decimal number characters.
                            // Java: An alphanumeric character:[\p{IsAlphabetic}\p{IsDigit}]
                            case "alnum" -> "\\p{Alnum}";
                            // Lisp: This matches any letter. For multibyte characters, it matches characters whose
                            //       Unicode ‘general-category’ property (see Character Properties) indicates they are alphabetic characters.
                            // Java: An alphabetic character:\p{IsAlphabetic}
                            case "alpha" -> "\\p{Alpha}";
                            // Lisp: This matches horizontal whitespace, as defined by Annex C of the Unicode Technical Standard #18.
                            //       In particular, it matches spaces, tabs, and other characters whose Unicode ‘general-category’ property
                            //       (see Character Properties) indicates they are spacing separators.
                            // Java: A space or a tab: [\p{IsWhite_Space}&&[^\p{gc=Zl}\p{gc=Zp}\x0a\x0b\x0c\x0d\x85]]
                            case "blank" -> "\\p{Blank}";
                            // Lisp: This matches any character whose code is in the range 0–31.
                            // Java: A control character:\p{gc=Cc} // TODO: Discrepancy?
                            case "cntrl" -> "\\p{Cntrl}";
                            // Lisp: This matches ‘0’ through ‘9’. Thus, ‘[-+[:digit:]]’ matches any digit, as well as ‘+’ and ‘-’.
                            // Java: A decimal digit character:\p{IsDigit}
                            case "digit" -> "\\p{Digit}";
                            // Lisp: This matches graphic characters—everything except spaces, ASCII and non-ASCII control characters, surrogates,
                            //       and codepoints unassigned by Unicode, as indicated by the Unicode ‘general-category’ property (see Character Properties).
                            // Java:A  visible character: [^\p{IsWhite_Space}\p{gc=Cc}\p{gc=Cs}\p{gc=Cn}]
                            case "graph" -> "\\p{Graph}";
                            // Lisp: This matches any lower-case letter, as determined by the current case table (see The Case Table).
                            //       If case-fold-search is non-nil, this also matches any upper-case letter. Note that a buffer can have
                            //       its own local case table different from the default one.
                            // Java: A lowercase character:\p{IsLowercase} // TODO: Discrepancy: case table
                            case "lower" -> "\\p{Lower}";
                            // Lisp: This matches any multibyte character (see Text Representations).
                            // Java: A Latin script character:\p{IsLatin}
                            case "multibyte" -> "\\P{IsLatin}";
                            // Lisp: This matches any non-ASCII character.
                            // Java: All ASCII:[\x00-\x7F]:\p{ASCII}
                            case "nonascii" -> "\\P{ASCII}";
                            // Lisp: This matches any printing character—either spaces or graphic characters matched by ‘[:graph:]’.
                            // Java: A printable character: [\p{Graph}\p{Blank}&&[^\p{Cntrl}]]
                            case "print" -> "\\p{Print}";
                            // Lisp: This matches any punctuation character. (At present, for multibyte characters, it matches anything
                            //       that has non-word syntax, and thus its exact definition can vary from one major mode to another,
                            //       since the syntax of a character depends on the major mode.)
                            // Java: A punctuation character:\p{IsPunctuation} // TODO: Discrepancy: syntax table
                            case "punct" -> "\\p{Punct}";
                            // Lisp: This matches any character that has whitespace syntax (see Table of Syntax Classes).
                            // Java: Equivalent to java.lang.Character.isLowerCase() // TODO: Discrepancy: syntax table
                            case "space" -> "\\p{javaWhitespace}";
                            // Lisp: This matches any unibyte character (see Text Representations).
                            // Java: A Latin script character:\p{IsLatin}
                            case "unibyte" -> "\\p{IsLatin}";
                            // Lisp: This matches any upper-case letter, as determined by the current case table.
                            // Java: An uppercase character:\p{IsUppercase} // TODO: Discrepancy: case table
                            case "upper" -> "\\p{Upper}";
                            // Lisp: This matches any character that has word syntax.
                            // Java: A word character: [\p{Alpha}\p{gc=Mn}\p{gc=Me}\p{gc=Mc}\p{Digit}\p{gc=Pc}\p{IsJoin_Control}]
                            // TODO: Discrepancy: syntax table
                            case "word" -> "\\w";
                            // Lisp: This matches the hexadecimal digits: ‘0’ through ‘9’, ‘a’ through ‘f’ and ‘A’ through ‘F’.
                            // Java: A hexadecimal digit: [\p{gc=Nd}\p{IsHex_Digit}]
                            case "xdigit" -> "\\p{XDigit}";
                            default -> throw new IllegalArgumentException();
                        });
                    }
                }
                default -> builder.appendCodePoint(codepoint);
            }
        }
        Pattern pattern = Pattern.compile(
                builder.toString(),
                (ignoreCase ? Pattern.CASE_INSENSITIVE : 0) | Pattern.UNICODE_CHARACTER_CLASS
        );
        return new PrecompiledRegExp(pattern, pointGroup);
    }
}
