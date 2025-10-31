package party.iroiro.juicemacs.elisp.forms.regex;

import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInSyntax;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.ArrayList;
import java.util.Iterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;

final class ELispRegExpLexer implements Iterator<REToken> {
    private final MuleStringReader reader;
    private boolean hasPreviousPattern = false;

    @Nullable
    private REToken peeked = null;

    public ELispRegExpLexer(ELispString regExp) {
        this.reader = new MuleStringReader(regExp);
    }

    @Override
    public boolean hasNext() {
        return peeked != null || reader.hasNext();
    }

    @Override
    public REToken next() {
        if (peeked != null) {
            REToken ret = peeked;
            peeked = null;
            return ret;
        }
        return readNext();
    }

    public REToken peek() {
        if (peeked == null) {
            peeked = readNext();
        }
        return peeked;
    }

    private REToken readNext() {
        int c = reader.next();
        return switch (c) {
            case '^' -> lineStart();
            case '$' -> lineEnd();
            case '+', '?', '*' -> repetitionChar(c);
            case '.' -> {
                hasPreviousPattern = true;
                yield new REToken.AnyChar();
            }
            case '[' -> charClass();
            case '\\' -> escaped();
            default -> normalChar(c);
        };
    }

    private REToken escaped() {
        int c = reader.next();
        return switch (c) {
            case '(' -> groupStart();
            case ')' -> {
                hasPreviousPattern = true;
                yield new REToken.GroupEnd();
            }
            case '|' -> {
                hasPreviousPattern = false;
                yield new REToken.Alternation();
            }
            case '{' -> quantifier();
            case '=' -> {
                hasPreviousPattern = true; // Emacs does this, but why?
                yield new REToken.BufferPoint();
            }
            case 's', 'S' -> {
                hasPreviousPattern = true;
                yield new REToken.SyntaxChar(BuiltInSyntax.checkSyntaxChar(reader.next()), c == 'S');
            }
            case 'c', 'C' -> {
                hasPreviousPattern = true;
                yield new REToken.CategoryChar(checkCategoryChar(reader.next()), c == 'C');
            }
            case 'w', 'W' -> {
                hasPreviousPattern = true;
                yield new REToken.SyntaxChar((byte) SWORD, c == 'W');
            }
            case '<' -> {
                hasPreviousPattern = true;
                yield new REToken.StartOfWord();
            }
            case '>' -> {
                hasPreviousPattern = true;
                yield new REToken.EndOfWord();
            }
            case '_' -> {
                hasPreviousPattern = true;
                int indicator = reader.next();
                if (indicator != '<' && indicator != '>') {
                    throw ELispSignals.error("Invalid symbol boundary indicator");
                }
                yield indicator == '<' ? new REToken.StartOfSymbol() : new REToken.EndOfSymbol();
            }
            case 'b', 'B' -> {
                hasPreviousPattern = true;
                yield new REToken.WordBoundary(c == 'B');
            }
            case '`' -> {
                hasPreviousPattern = true;
                yield new REToken.StartOfString();
            }
            case '\'' -> {
                hasPreviousPattern = true;
                yield new REToken.EndOfString();
            }
            case '1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
                hasPreviousPattern = true;
                int backRef = c - '0'; // Emacs only supports 1-9
                yield new REToken.BackReference(backRef);
            }
            default -> normalChar(c);
        };
    }

    private byte checkCategoryChar(int next) {
        if (next >= 128) {
            throw ELispSignals.error("Invalid category character");
        }
        return (byte) next;
    }

    private REToken quantifier() {
        int min = Math.max(readInt(), 0);
        int c = reader.peek();
        int max = min;
        if (c == ',') {
            reader.consume(1);
            max = readInt();
            if (max == -1) {
                max = Integer.MAX_VALUE;
            }
        }
        if (min > max) {
            throw ELispSignals.error("Invalid content of \\{\\}");
        }
        if (reader.unexpectedNext((byte) '\\', (byte) '}')) {
            throw ELispSignals.error("Unmatched \\{");
        }
        reader.consume(2);
        return new REToken.Quantifier(min, max, true);
    }
    private int readInt() {
        int c = reader.peek();
        if (c < '0' || '9' < c) {
            return -1;
        }
        reader.consume(1);
        int i = c - '0';
        while (true) {
            c = reader.peek();
            if (c < '0' || '9' < c) {
                return i;
            }
            reader.consume(1);
            i = Math.addExact(Math.multiplyExact(i, 10), c - '0');
        }
    }

    private REToken groupStart() {
        hasPreviousPattern = false;
        if (reader.peek() == '?') {
            reader.consume(1);
            int c = reader.next();
            if (c == ':') {
                return new REToken.GroupStart(-1);
            }
            if (c == '0') {
                throw ELispSignals.error("Invalid group number");
            }
            int explicitNumber = 0;
            do {
                if (c < '0' || '9' < c) {
                    throw ELispSignals.error("Invalid group number");
                }
                explicitNumber = Math.addExact(Math.multiplyExact(explicitNumber, 10), c - '0');
                c = reader.next();
            } while (c != ':');
            return new REToken.GroupStart(explicitNumber);
        }
        return new REToken.GroupStart(0);
    }

    private REToken charClass() {
        boolean invert = reader.peek() == '^';
        if (invert) {
            reader.consume(1);
        }

        int lastChar;
        if (reader.peek() == ']') {
            lastChar = reader.next();
        } else {
            lastChar = -1;
        }

        ArrayList<CharClassContent> contents = new ArrayList<>();
        while (true) {
            CharClassContent content = namedCharClass();
            if (content != null) {
                if (lastChar != -1) {
                    contents.add(new CharClassContent.Range(lastChar));
                    lastChar = -1;
                }
                contents.add(content);
                continue;
            }
            if (!reader.hasNext()) {
                throw ELispSignals.error("Unmatched [ or [^");
            }
            int c = reader.next();
            if (c == ']') {
                if (lastChar != -1) {
                    contents.add(new CharClassContent.Range(lastChar));
                }
                break;
            }
            if (c == '-') {
                if (lastChar == -1 || reader.peek() == ']') {
                    contents.add(new CharClassContent.Range('-', '-'));
                } else {
                    contents.add(new CharClassContent.Range(lastChar, reader.next()));
                }
                lastChar = -1;
                continue;
            }
            if (lastChar != -1) {
                contents.add(new CharClassContent.Range(lastChar));
            }
            lastChar = c;
        }

        hasPreviousPattern = true;
        return REToken.CharClass.preprocess(contents.toArray(new CharClassContent[0]), invert);
    }
    private CharClassContent.@Nullable Named namedCharClass() {
        if (reader.unexpectedNext((byte) '[', (byte) ':')) {
            return null;
        }
        int index = reader.index + 2; // skip opening "[:"
        ELispString cache = reader.string;
        int limit = Math.min(index + CharClassContent.Named.MAX_NAME_LENGTH, cache.length() - 2);
        int end = index;
        for (; end <= limit; end++) {
            if (cache.codePointAt(end) == ':' && cache.codePointAt(end + 1) == ']') {
                StringBuilder s = new StringBuilder(Math.toIntExact(end - index));
                for (int i = index; i < end; i++) {
                    s.append((char) cache.codePointAt(i));
                }
                reader.consume(end - reader.index + 2);
                // TODO: Handle invalid exceptions
                return CharClassContent.Named.valueOf(s.toString());
            }
        }
        return null;
    }

    private REToken repetitionChar(int c) {
        if (!hasPreviousPattern) {
            return normalChar(c);
        }
        // Merge multiple repetition chars
        boolean zeroTimes = false;
        boolean manyTimes = false;
        boolean greedy = true;
        while (true) {
            if (c == '?' && (zeroTimes || manyTimes)) {
                greedy = false;
            } else {
                zeroTimes |= c != '+';
                manyTimes |= c != '?';
            }
            int peek = reader.peek();
            if (!(peek == '*' || peek == '+' || peek == '?')) {
                break;
            }
            c = reader.next();
        }
        return new REToken.Quantifier(zeroTimes ? 0 : 1, manyTimes ? Integer.MAX_VALUE : 1, greedy);
    }

    /// Emacs `at_begline_loc_p`
    ///
    /// This seems a very, very, very bad way to detect start of the pattern.
    /// But, it's always backwards compatibility.
    private boolean atStartOfLine(int index) {
        if (index == 0) {
            return true;
        }
        int prev = index - 1;
        ELispString cache = reader.string;
        switch (cache.codePointAt(prev)) {
            case '(', '|' -> {}
            case ':' -> {
                while (prev > 0 && '0'<= cache.codePointAt(prev - 1) && cache.codePointAt(prev - 1) <= '9') {
                    prev--;
                }
                if (!(prev > 1 && cache.codePointAt(prev - 1) == '?' && cache.codePointAt(prev - 2) == '(')) {
                    return false;
                }
                prev -= 2;
            }
            default -> {
                return false;
            }
        }
        index = prev;
        while (prev > 0 && cache.codePointAt(prev - 1) == '\\') {
            prev--;
        }
        return ((index - prev) & 1) != 0;
    }
    private REToken lineStart() {
        if (atStartOfLine(reader.index - 1)) {
            hasPreviousPattern = false;
            return new REToken.StartOfLine();
        } else {
            return normalChar('^');
        }
    }

    /// Emacs `at_endline_loc_p`
    private REToken lineEnd() {
        int index = reader.index;
        ELispString cache = reader.string;
        if (index == cache.length()
                || (cache.codePointAt(index) == '\\' && index + 1 < cache.length()
                && (cache.codePointAt(index + 1) == '|' || cache.codePointAt(index + 1) == ')'))
        ) {
            hasPreviousPattern = true;
            return new REToken.EndOfLine();
        } else {
            return normalChar('$');
        }
    }

    private REToken normalChar(int c) {
        hasPreviousPattern = true;
        return new REToken.Char(c);
    }

    private static final class MuleStringReader {
        private final ELispString string;
        private int index = 0;

        private MuleStringReader(ELispString string) {
            this.string = string;
        }

        boolean hasNext() {
            return index < string.length();
        }

        int next() {
            if (!hasNext()) {
                throw ELispSignals.invalidRegexp("Premature end of regular expression");
            }
            return string.codePointAt(index++);
        }

        void consume(int n) {
            index += n;
        }

        int peek() {
            return index < string.length() ? string.codePointAt(index) : -1;
        }

        boolean unexpectedNext(byte b1, byte b2) {
            long rest = (long) string.length() - index;
            if (rest < 2) {
                return true;
            }
            return string.codePointAt(index) != b1 || string.codePointAt(index + 1) != b2;
        }
    }
}
