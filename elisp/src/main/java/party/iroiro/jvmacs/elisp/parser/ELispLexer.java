package party.iroiro.jvmacs.elisp.parser;

import java.io.IOException;
import java.io.Reader;
import java.math.BigInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

/**
 * A ELisp lexer
 *
 * <p>
 * This lexer is more or less based on the ELisp manual,
 * and borrows heavily from the following two implementation:
 * </p>
 * <ul>
 * <li><a href="https://git.savannah.gnu.org/cgit/guile.git/tree/module/language/elisp/lexer.scm">
 * The ELisp lexer in Guile</a></li>
 * <li><a href="https://github.com/talyz/fromElisp">talyz/fromElisp in Nix</a></li>
 * </ul>
 *
 * <p>
 * Not all ELisp grammar is supported.
 * </p>
 */
class ELispLexer {

    sealed interface NumberVariant {

        record BigNum(BigInteger value) implements NumberVariant {
        }

        record FixNum(long value) implements NumberVariant {
        }

        record Float(double value) implements NumberVariant {
        }

        static NumberVariant from(BigInteger value) {
            if (value.bitLength() <= (Long.SIZE - 1)) {
                return new FixNum(value.longValue());
            }
            return new BigNum(value);
        }

    }

    sealed interface TokenData {

        /**
         * End-of-file indicator
         */
        record EOF() implements TokenData {
        }

        /**
         * Indicator of a line like {@code -*- lexical-binding: t -*-}
         *
         * <p>
         * It indicates that the source file sets lexical-binding mode
         * in the comment at the first line of the file.
         * </p>
         *
         * <p>
         * It seems Emacs treats only {@code -*- lexical-binding: nil -*-} as false.
         * So we follow that.
         * </p>
         */
        record SetLexicalBindingMode(boolean value) implements TokenData {
        }

        /**
         * A dot as is in {@code (1.0 . 2.0)}
         */
        record Dot() implements TokenData {
        }

        /**
         * Opening parenthesis
         */
        record ParenOpen() implements TokenData {
        }

        /**
         * Opening record as is in {@code #s(}
         */
        record RecordOpen() implements TokenData {
        }

        /**
         * Closing parenthesis
         *
         * <p>
         * Please note that it also closes {@link RecordOpen}.
         * </p>
         */
        record ParenClose() implements TokenData {
        }

        /**
         * Opening square bracket
         */
        record SquareOpen() implements TokenData {
        }

        /**
         * Opening byte code as is in {@code #[}
         */
        record ByteCodeOpen() implements TokenData {
        }

        /**
         * Closing square bracket
         *
         * <p>
         * Please note that it also closes {@link ByteCodeOpen}.
         * </p>
         */
        record SquareClose() implements TokenData {
        }

        /**
         * Function quote as is in {@code #'func}
         */
        record Function() implements TokenData {
        }

        /**
         * Symbol quote as is in {@code 'symbol}
         */
        record Quote() implements TokenData {
        }

        /**
         * Back quote as is in {@code `symbol}
         */
        record BackQuote() implements TokenData {
        }

        /**
         * Comma as is in {@code `(,var)}
         */
        record Unquote() implements TokenData {
        }

        /**
         * Comma at back quote as is in {@code `(,@var)}
         */
        record UnquoteSplicing() implements TokenData {
        }

        /**
         * Circular reference as is in {@code #1#}
         */
        record CircularRef(long id) implements TokenData {
        }

        /**
         * Circular definition as is in {@code #1=func}
         */
        record CircularDef(long id) implements TokenData {
        }

        record Char(int value) implements TokenData {
        }

        record Str(String value) implements TokenData {
        }

        record Num(NumberVariant value) implements TokenData {
        }

        record Symbol(String value) implements TokenData {
        }

        /**
         * Bool vector as is in {@code #&10"value"}
         */
        record BoolVec(long length, String value) implements TokenData {
        }
    }

    /**
     * @param data the actual token data
     * @param source the source section of the token
     */
    record Token(TokenData data, SourceSection source) {
    }

    private static final TokenData.EOF EOF = new TokenData.EOF();
    private static final TokenData.Dot DOT = new TokenData.Dot();
    private static final TokenData.ParenOpen PAREN_OPEN = new TokenData.ParenOpen();
    private static final TokenData.RecordOpen RECORD_OPEN = new TokenData.RecordOpen();
    private static final TokenData.ParenClose PAREN_CLOSE = new TokenData.ParenClose();
    private static final TokenData.SquareOpen SQUARE_OPEN = new TokenData.SquareOpen();
    private static final TokenData.ByteCodeOpen BYTE_CODE_OPEN = new TokenData.ByteCodeOpen();
    private static final TokenData.SquareClose SQUARE_CLOSE = new TokenData.SquareClose();
    private static final TokenData.Function FUNCTION = new TokenData.Function();
    private static final TokenData.Quote QUOTE = new TokenData.Quote();
    private static final TokenData.BackQuote BACK_QUOTE = new TokenData.BackQuote();
    private static final TokenData.Unquote UNQUOTE = new TokenData.Unquote();
    private static final TokenData.UnquoteSplicing UNQUOTE_SPLICING = new TokenData.UnquoteSplicing();

    private static final Pattern LEXICAL_BINDING_PATTERN = Pattern.compile(
        "-\\*-(?:|.*;)[ \t]*lexical-binding:[ \t]*([^;]*[^ \t;]).*-\\*-"
    );
    private static final Pattern INTEGER_PATTERN = Pattern.compile("^([+-]?[0-9]+)\\.?$");
    private static final Pattern FLOAT_PATTERN = Pattern.compile("^[+-]?(?:[0-9]+\\.?[0-9]*|[0-9]*\\.?[0-9]+)(?:e(?:[+-]?[0-9]+|\\+INF|\\+NaN))?$");

    private final Reader reader;
    private final Source source;

    public ELispLexer(Source source) {
        this.reader = source.getReader();
        this.source = source;
    }

    boolean hasNext() {
        return !eof;
    }

    /**
     * Maintained by {@link #readCodepoint()}. See also {@link Token}.
     */
    private int line = 1;
    /**
     * Maintained by {@link #readCodepoint()}. See also {@link Token}.
     */
    private int offset = 0;
    /**
     * Maintained by {@link #readCodepoint()}.
     */
    private boolean eof = false;
    /**
     * Maintained by {@link #readCodepoint()} and {@link #peekCodepoint()}.
     */
    private int peekedCodepoint = -1;

    /**
     * Reads a full Unicode codepoint from the reader.
     *
     * <p>
     * The {@link eof}, {@link #line} and {@link #offset} fields are
     * maintained in this method only. One should take care to not modify
     * them elsewhere.
     * </p>
     */
    private int readCodepoint() throws IOException {
        int c = peekCodepoint();
        peekedCodepoint = -1;
        if (c == -1) {
            eof = true;
            return -1;
        }
        if (c == '\n') {
            line++;
        }
        offset += Character.charCount(c);
        return c;
    }

    private static int readCodepointFromReader(Reader reader) throws IOException {
        int c1 = reader.read();
        if (c1 == -1) {
            return -1;
        }
        if (!Character.isHighSurrogate((char) c1)) {
            return c1;
        }
        int c2 = noEOF(reader.read());
        if (!Character.isLowSurrogate((char) c2)) {
            throw new IOException("Invalid Unicode surrogate pair");
        }
        return Character.toCodePoint((char) c1, (char) c2);
    }

    private int peekCodepoint() throws IOException {
        if (peekedCodepoint != -1) {
            return peekedCodepoint;
        }

        peekedCodepoint = readCodepointFromReader(reader);
        return peekedCodepoint;
    }

    private String readLine() throws IOException {
        StringBuilder sb = new StringBuilder();
        while (true) {
            int c = peekCodepoint();
            if (c == -1 || c == '\n') {
                break;
            }
            readCodepoint();
            sb.appendCodePoint(c);
        }
        return sb.toString();
    }

    /**
     * @see https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html
     */
    private static int mapBasicEscapeCode(int c) {
        return switch(c) {
            case 'a' -> '\u0007';
            case 'b' -> '\b';
            case 't' -> '\t';
            case 'n' -> '\n';
            case 'v' -> '\u000b';
            case 'f' -> '\f';
            case 'r' -> '\r';
            case 'e' -> '\u001b';
            case 's' -> ' ';
            case 'd' -> '\u007f';
            default -> -1;
        };
    }

    /**
     * @see https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Char-Bits.html
     */
    private static int mapMetaBits(int c, boolean inString) {
        return switch(c) {
            case 'A' -> 22;
            case 's' -> 23;
            case 'H' -> 24;
            case 'S' -> 25;
            case 'M' -> inString ? 7 : 27;
            default -> -1;
        };
    }

    private BigInteger readInteger(int base, int digits, boolean earlyReturn) throws IOException {
        BigInteger value = new BigInteger("0", base);
        // When `digit == -1`, `i != digits` allows reading a infinitely large number.
        for (int i = 0; i != digits; i++) {
            int c = peekCodepoint();
            if (earlyReturn) {
                if (c == -1) {
                    return value;
                }
            } else {
                c = noEOF(c);
            }
            int digit;
            if ('0' <= c && c <= '9') {
                digit = c - '0';
            } else if ('a' <= c && c <= 'z') {
                digit = c - 'a' + 10;
            } else if ('A' <= c && c <= 'Z') {
                digit = c - 'A' + 10;
            } else {
                digit = -1;
            }
            if (digit == -1 || digit >= base) {
                if (earlyReturn) {
                    return value;
                }
                throw new IOException("Expecting fixed number of digits");
            }
            readCodepoint();
            value = value.multiply(BigInteger.valueOf(base)).add(BigInteger.valueOf(digit));
        }
        return value;
    }

    private int readEscapedCodepoint(int base, int digits, boolean earlyReturn) throws IOException {
        BigInteger value = readInteger(base, digits, earlyReturn);
        if (value.compareTo(BigInteger.valueOf(Character.MAX_CODE_POINT)) > 0) {
            throw new IOException("Not a valid Unicode code point");
        }
        return value.intValue();
    }

    private int readControlChar() throws IOException {
        int c = readCodepoint();
        if (c == -1) {
            // I would like to yield an error here, but Emacs does seem to return -1...?
            return -1;
        }
        if (c < 256) {
            if (c == '?') {
                return 127;
            }
            if (c == '@') {
                return 0;
            }
            if (Character.isAlphabetic(c)) {
                return Character.toUpperCase(c) - '@';
            }
        }
        return c | (0x1 << 26);
    }

    /**
     * Read the remaining encoded ELisp character following the {@code ?} character.
     */
    private int readChar(boolean inString) throws IOException {
        int c = noEOF(readCodepoint());
        // Normal characters: ?a => 'a'
        if (c != '\\') {
            return c;
        }
        int escaped = noEOF(peekCodepoint());
        // Octal escape: ?\001 => '\001', ?\1 => '\001'
        if ('0' <= escaped && escaped <= '7') {
            return readEscapedCodepoint(8, 3, true);
        }
        readCodepoint();
        if (inString) {
            // Line continuation, ignored.
            if (escaped == ' ' || escaped == '\n') {
                return -1;
            }
        }
        // Meta-prefix: ?\s-a => Bit annotated character
        int meta = mapMetaBits(escaped, inString);
        if (meta != -1 && peekCodepoint() == '-') {
            readCodepoint();
            return (0x1 << meta) | readChar(inString);
        }
        // Escape: ?\n => '\n'
        int escapeCode = mapBasicEscapeCode(escaped);
        if (escapeCode != -1) {
            return escapeCode;
        }
        // Control characters: ?\^I or ?\C-I
        return switch (escaped) {
            case '^' -> readControlChar();
            case 'C' -> {
                if (peekCodepoint() == '-') {
                    readCodepoint();
                    yield readControlChar();
                } else {
                    yield escaped;
                }
            }
            case 'x' -> readEscapedCodepoint(16, -1, true);
            case 'u' -> readEscapedCodepoint(16, 4, false);
            case 'U' -> readEscapedCodepoint(16, 8, false);
            case 'N' -> {
                if (peekCodepoint() == '{') {
                    readCodepoint();
                    StringBuilder unicodeName = new StringBuilder();
                    boolean whitespace = false;
                    int u;
                    while ((u = readCodepoint()) != '}') {
                        if (!Character.isWhitespace(u)) {
                            whitespace = false;
                            unicodeName.appendCodePoint(u);
                        } else if (!whitespace) {
                            whitespace = true;
                            unicodeName.append(' ');
                        }
                    }
                    yield Character.codePointOf(unicodeName.toString());
                }
                yield escaped;
            }
            default -> escaped;
        };
    }

    /**
     * Read the remaining ELisp string after the initial {@code "} character.
     */
    private String readStr() throws IOException {
        StringBuilder sb = new StringBuilder();
        while (true) {
            int c = noEOF(peekCodepoint());
            switch (c) {
                case '"' -> {
                    readCodepoint();
                    return sb.toString();
                }
                case '\\' -> {
                    c = readChar(true);
                    if (c != -1) {
                        sb.appendCodePoint(c);
                    }
                }
                default -> {
                    readCodepoint();
                    sb.appendCodePoint(c);
                }
            }
        }
    }

    private boolean needsNoEscape(int c) throws IOException {
        return Character.isDigit(c) || Character.isAlphabetic(c) || "-+=*/_~!@$%^&:<>{}?.".indexOf(c) != -1;
    }

    private TokenData readHashNumToken(int base) throws IOException {
        BigInteger value = readInteger(base, -1, true);
        switch (peekCodepoint()) {
            // #1#
            case '#' -> {
                readCodepoint();
                return  new TokenData.CircularRef(value.longValueExact());
            }
            // #1=
            case '=' -> {
                readCodepoint();
                return  new TokenData.CircularDef(value.longValueExact());
            }
            // #24r1k => base-24 integer
            case 'r' -> {
                if (base != 10) {
                    throw new IOException("Invalid base");
                }
                readCodepoint();
                int realBase = value.intValueExact();
                if (realBase > 36) {
                    throw new IOException("Invalid base");
                }
                BigInteger number = readInteger(realBase, -1, true);
                if (needsNoEscape(peekCodepoint())) {
                    throw new IOException("Invalid character");
                }
                return new TokenData.Num(NumberVariant.from(number));
            }
            default -> {
                if (needsNoEscape(peekCodepoint())) {
                    throw new IOException("Invalid character");
                }
                return new TokenData.Num(NumberVariant.from(value));
            }
        }
    }

    private TokenData readSymbolOrNumber(int alreadyRead, boolean symbolOnly) throws IOException {
        StringBuilder sb = new StringBuilder();
        if (alreadyRead != -1) {
            // TODO: Check.
            sb.appendCodePoint(alreadyRead);
        }
        boolean escaped = false;
        while (true) {
            int c = peekCodepoint();
            if (c == -1) {
                break;
            }
            // TODO: Deal Unicode like "，" (not alphabetic in Java but acceptable in ELisp)
            // Try: (setq ， 1)
            if (needsNoEscape(c)) {
                readCodepoint();
                sb.appendCodePoint(c);
            } else if (c == '\\') {
                readCodepoint();
                escaped = true;
                sb.appendCodePoint(readCodepoint());
            } else {
                break;
            }
        }
        String symbol = sb.toString();
        if (!symbolOnly && !escaped) {
            Matcher integer = INTEGER_PATTERN.matcher(symbol);
            if (integer.matches()) {
                return new TokenData.Num(NumberVariant.from(new BigInteger(integer.group(1))));
            }
            if (FLOAT_PATTERN.matcher(symbol).matches()) {
                // Handle 1.0e+INF or 1.0e+NaN
                double f;
                if (symbol.endsWith("+INF")) {
                    f = symbol.startsWith("-") ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
                } else if (symbol.endsWith("+NaN")) {
                    // Emacs encodes (number)e+NaN into the significand of the NaN...
                    double base = Double.parseDouble(symbol.substring(0, symbol.length() - 5));
                    boolean neg = Math.copySign(1.0, base) == -1.0;
                    long SIGNIFICAND_BITS = 52;
                    long SIGNIFICAND_MASK = (0x1 << (SIGNIFICAND_BITS - 1)) - 1;
                    long EXP_BITS = 11;
                    long NAN_EXP = (0x1 << EXP_BITS) - 1;
                    long significand = ((long) base) & SIGNIFICAND_MASK;
                    f = Double.longBitsToDouble(
                        ((neg ? 0x1L : 0x0L) << 63) | ((significand + 1) << 1) | (NAN_EXP << (SIGNIFICAND_BITS))
                    );
                } else {
                    f = Double.parseDouble(symbol);
                }
                return new TokenData.Num(new NumberVariant.Float(f));
            }
        }
        return new TokenData.Symbol(symbol);
    }

    /**
     * Returns the next ELisp token from the reader.
     *
     * <p>
     * It is based on the ELisp lexer from Guile.
     * See <a href="https://git.savannah.gnu.org/cgit/guile.git/tree/module/language/elisp/lexer.scm">
     * guile.git/tree/module/language/elisp/lexer.scm</a>.
     * </p>
     *
     * <p>
     * It assumes it is called by {@link #next()} with all whitespaces already consumed.
     * </p>
     *
     * @return {@code null} if there is a comment, or the next token otherwise.
     */
    private TokenData lexNext() throws IOException {
        int c = readCodepoint();
        if (c == -1) {
            return EOF;
        }
        if (c == '.' && Character.isWhitespace(peekCodepoint())) {
            return DOT;
        }
        return switch (c) {
            case ';' -> {
                String comment = readLine();
                if (line == 1) {
                    Matcher matcher = LEXICAL_BINDING_PATTERN.matcher(comment);
                    if (matcher.find()) {
                        yield new TokenData.SetLexicalBindingMode(
                                !matcher.group(1).equals("nil")
                        );
                    }
                }
                yield null;
            }
            case '?' -> new TokenData.Char(readChar(false));
            case '"' -> new TokenData.Str(readStr());
            case '#' -> {
                int next = peekCodepoint();
                if ('0' <= next && next <= '9') {
                    yield readHashNumToken(10);
                }
                readCodepoint();
                yield switch (next) {
                    case '\'' -> FUNCTION;
                    case ':' -> readSymbolOrNumber(-1, true);
                    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte_002dCode-Objects.html
                    case '[' -> BYTE_CODE_OPEN;
                    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Bool_002dVectors.html
                    case '&' -> {
                        BigInteger length = readInteger(10, -1, true);
                        long l = length.longValueExact();
                        if (readCodepoint() != '\"') {
                            throw new IOException("Expected '\"'");
                        }
                        yield new TokenData.BoolVec(l, readStr());
                    }
                    case 's' -> {
                        // https://www.gnu.org/software/emacs/manual/html_node/elisp/Records.html
                        if (readCodepoint() != '(') {
                            throw new IOException("Expected '('");
                        }
                        yield RECORD_OPEN;
                    }
                    default -> switch (Character.toUpperCase(next)) {
                        case 'B' -> readHashNumToken(2);
                        case 'O' -> readHashNumToken(8);
                        case 'X' -> readHashNumToken(16);
                        default -> throw new IOException("Expected a number base indicator");
                    };
                };
            }
            case '(' -> PAREN_OPEN;
            case ')' -> PAREN_CLOSE;
            case '[' -> SQUARE_OPEN;
            case ']' -> SQUARE_CLOSE;
            case '\'' -> QUOTE;
            case '`' -> BACK_QUOTE;
            case ',' -> {
                if (peekCodepoint() == '@') {
                    readCodepoint();
                    yield UNQUOTE_SPLICING;
                }
                yield UNQUOTE;
            }
            default -> readSymbolOrNumber(c, false);
        };
    }

    Token next() throws IOException {
        if (eof) {
            return null;
        }
        while (true) {
            while (true) {
                int c = peekCodepoint();
                if (c == -1 || !Character.isWhitespace(c)) {
                    break;
                }
                readCodepoint();
            }
            int startOffset = this.offset;
            TokenData data = lexNext();
            if (data != null) {
                int endOffset = this.offset;
                return new Token(data, source.createSection(startOffset, endOffset - startOffset));
            }
        }
    }

    private static int noEOF(int c) throws IOException {
        if (c == -1) {
            throw new IOException("Unexpected EOF");
        }
        return c;
    }

}
