package party.iroiro.juicemacs.elisp.forms.regex;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInSyntax;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;

final class ELispRegExpLexer implements Iterator<ELispRegExpLexer.REToken> {
    private final TruffleStringReader reader;
    private boolean hasPreviousPattern = false;

    @Nullable
    private REToken peeked = null;

    public ELispRegExpLexer(MuleString regExp) {
        this.reader = new TruffleStringReader(regExp);
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
        if (min > max || reader.unexpectedNext((byte) '\\', (byte) '}')) {
            throw ELispSignals.error("Invalid quantifier");
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

        List<CharClassContent> contents = new ArrayList<>();
        while (true) {
            CharClassContent content = namedCharClass();
            if (content != null) {
                if (lastChar != -1) {
                    contents.add(new CharClassContent.CharRange(lastChar));
                    lastChar = -1;
                }
                contents.add(content);
                continue;
            }
            if (!reader.hasNext()) {
                throw ELispSignals.error("Unmatched [");
            }
            int c = reader.next();
            if (c == ']') {
                if (lastChar != -1) {
                    contents.add(new CharClassContent.CharRange(lastChar));
                }
                break;
            }
            if (c == '-') {
                if (lastChar == -1 || reader.peek() == ']') {
                    contents.add(new CharClassContent.CharRange('-', '-'));
                } else {
                    contents.add(new CharClassContent.CharRange(lastChar, reader.next()));
                }
                lastChar = -1;
                continue;
            }
            if (lastChar != -1) {
                contents.add(new CharClassContent.CharRange(lastChar));
            }
            lastChar = c;
        }

        hasPreviousPattern = true;
        return REToken.CharClass.preprocess(contents.toArray(new CharClassContent[0]), invert);
    }
    private CharClassContent.@Nullable NamedCharClass namedCharClass() {
        if (reader.unexpectedNext((byte) '[', (byte) ':')) {
            return null;
        }
        int index = reader.index + 2; // skip opening "[:"
        MuleString cache = reader.string;
        int limit = Math.min(index + CharClassContent.NamedCharClass.MAX_NAME_LENGTH, cache.length() - 2);
        int end = index;
        for (; end <= limit; end++) {
            if (cache.codePointAt(end) == ':' && cache.codePointAt(end + 1) == ']') {
                StringBuilder s = new StringBuilder(end - index);
                for (int i = index; i < end; i++) {
                    s.append((char) cache.codePointAt(i));
                }
                reader.consume(end - reader.index + 2);
                // TODO: Handle invalid exceptions
                return CharClassContent.NamedCharClass.valueOf(s.toString());
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
        MuleString cache = reader.string;
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
        MuleString cache = reader.string;
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

    sealed interface CharClassContent {
        @SuppressWarnings("PMD.FieldNamingConventions")
        enum NamedCharClass implements CharClassContent {
            alnum(0),
            alpha(1),
            ascii(2),
            blank(3),
            cntrl(4),
            digit(5),
            graph(6),
            lower(7),
            multibyte(8),
            nonascii(9),
            print(10),
            punct(11),
            space(12),
            unibyte(13),
            upper(14),
            word(15),
            xdigit(16);

            static final int MAX_NAME_LENGTH = Arrays.stream(values()).mapToInt((e) -> e.name().length()).max().orElse(16);
            public final int mask;

            NamedCharClass(int i) {
                this.mask = 1 << i;
            }

            public boolean match(int bits) {
                return (mask & bits) != 0;
            }
        }
        record CharRange(int min, int max) implements CharClassContent {
            CharRange(int single) {
                this(single, single);
            }
        }
    }

    sealed interface REToken {
        record AnyChar() implements REToken {}
        /// @param c a Unicode codepoint
        record Char(int c) implements REToken {}
        record CharClass(
                CharClassContent.NamedCharClass[] namedClasses,
                CharClassContent.CharRange[] charRanges,
                boolean charRangesFitInInt,
                boolean invert
        ) implements REToken {
            static REToken preprocess(CharClassContent[] array, boolean invert) {
                ArrayList<CharClassContent.NamedCharClass> namedClasses = new ArrayList<>();
                ArrayList<CharClassContent.CharRange> charRanges = new ArrayList<>();
                boolean charRangesFitInInt = true;
                for (CharClassContent content : array) {
                    switch (content) {
                        case CharClassContent.CharRange range when range.min <= range.max -> {
                            charRangesFitInInt &= range.max <= 0xFFFF;
                            charRanges.add(range);
                        }
                        case CharClassContent.NamedCharClass named -> namedClasses.add(named);
                        default -> {}
                    }
                }
                if (namedClasses.isEmpty() && charRanges.size() == 1 && !invert) {
                    CharClassContent.CharRange first = charRanges.getFirst();
                    if (first.min == first.max) {
                        return new Char(first.min);
                    }
                }
                return new CharClass(
                        namedClasses.toArray(CharClassContent.NamedCharClass[]::new),
                        charRanges.toArray(CharClassContent.CharRange[]::new),
                        charRangesFitInInt,
                        invert
                );
            }
        }
        record SyntaxChar(byte kind, boolean invert) implements REToken {}
        record CategoryChar(byte kind, boolean invert) implements REToken {}
        /// @param index the explicit group index, -1 if non-capturing, 0 if auto-numbered
        record GroupStart(int index) implements REToken {}
        record GroupEnd() implements REToken {}
        /// @param min the minimum number of repetitions
        /// @param max the maximum number of repetitions
        /// @param greedy true if the quantifier is greedy
        record Quantifier(int min, int max, boolean greedy) implements REToken {}

        record Alternation() implements REToken {}

        record StartOfString() implements REToken {}
        record EndOfString() implements REToken {}
        record StartOfLine() implements REToken {}
        record EndOfLine() implements REToken {}
        record BufferPoint() implements REToken {}

        record StartOfWord() implements REToken {}
        record EndOfWord() implements REToken {}
        record WordBoundary(boolean invert) implements REToken {}

        record StartOfSymbol() implements REToken {}
        record EndOfSymbol() implements REToken {}

        record BackReference(int index) implements REToken {}
    }

    private static final class TruffleStringReader {
        private final MuleString string;
        private int index = 0;

        private TruffleStringReader(MuleString string) {
            this.string = string;
            int length = string.length();
        }

        public boolean hasNext() {
            return index < string.length();
        }

        public int next() {
            return string.codePointAt(index++);
        }

        public void consume(int n) {
            index += n;
        }

        public int peek() {
            return index < string.length() ? string.codePointAt(index) : -1;
        }

        public boolean unexpectedNext(byte b1, byte b2) {
            int rest = string.length() - index;
            if (rest < 2) {
                return true;
            }
            return string.codePointAt(index) != b1 || string.codePointAt(index + 1) != b2;
        }
    }
}
