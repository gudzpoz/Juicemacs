package party.iroiro.juicemacs.elisp.parser;

import java.io.IOException;
import java.io.Reader;
import java.math.BigInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.strings.MutableTruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

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
 * <li><a href="http://git.savannah.gnu.org/cgit/emacs.git/tree/src/lread.c">
 * The ELisp lexer & parser in Emacs</a></li>
 * </ul>
 *
 * <p>
 * This lexer tries to support all ELisp grammar as is listed below.
 * If you are to understand how many features are actually
 * supported by canonical ELisp (as is in Emacs), I would recommend reading the {@code read0}
 * function in the Emacs source code.
 * </p>
 *
 * <h2>Emacs ELisp Features</h2>
 *
 * <p>
 * The following lists the features supported by Emacs ELisp,
 * serving as my personal notes as well as a reference for future readers.
 * Because the Emacs implementation integrates lexer and parser into the same function,
 * the logic can be a bit convoluted.
 * </p>
 *
 * <ul>
 * <li>Unless otherwise noted, the "(" character marks {@code RE_list_start}.</li>
 * <li>The ")" character marks the end of a {@code RE_list} (or {@code RE_list_start}),
 * a {@code RE_record} or a {@code RE_string_props}.</li>
 * <li>The "[" character marks the start of a {@code RE_vector}.</li>
 * <li>The "]" character marks the end of a {@code RE_vector}, a {@code RE_byte_code},
 * a {@code RE_char_table} or a {@code RE_sub_char_table}.</li>
 * <li>The "#" character marks the start of many, many things.
 * <ul>
 * <li>{@code #'X}: Special syntax for function symbol reference.</li>
 * <li>{@code ##}: The empty symbol.</li>
 * <li>{@code #s(...)}: A record or hash-table.</li>
 * <li>{@code #^[...]}: A character table.</li>
 * <li>{@code #^^[...]}: A sub-char-table.</li>
 * <li>{@code #(...)}: String with properties.</li>
 * <li>{@code #[...]}: Byte code.</li>
 * <li>{@code #&N"..."}: A bool vector.</li>
 * <li>{@code #!}: The shebang.</li>
 * <li>{@code #xFFFF / #XFFFF}: A hex integer.</li>
 * <li>{@code #b1010 / #B1010}: A binary integer.</li>
 * <li>{@code #o7070 / #O7070}: An octal integer.</li>
 * <li>{@code #@NUMBER}: Used to skip {@code NUMBER} following bytes.</li>
 * <li>{@code #$}: Reference to lazy-loaded string.</li>
 * <li>{@code #:X}: Uninterned symbol.</li>
 * <li>{@code #_X}: Symbol without shorthand.</li>
 * <li>{@code #36rX}: A custom-radix integer.</li>
 * <li>{@code #1= / #1#}: A cyclic definition / reference.</li>
 * </ul>
 * </li>
 * <li>The "?" character marks the start of a character literal.</li>
 * <li>The {@code "} character marks the start of a string.</li>
 * <li>The "'" character marks a {@code RE_special(Qquote)}.</li>
 * <li>The "`" character marks a {@code RE_special(Qbackquote)}.</li>
 * <li>The ",@" string marks a {@code RE_special(Qcomma_at)}.</li>
 * <li>The "," character marks a {@code RE_special(Qcomma)}.</li>
 * <li>The ";" character marks a comment.</li>
 * <li>The "." character is used in a cons pair, only if it is followed by the following chars:
 * {@code [\0-\32]|NO_BREAK_SPACE|["';()\[#?`,]} ({@code )]} are not in the list).</li>
 * <li>Otherwise, the characters that follows will be first parsed as numbers.</li>
 * <li>If it turns out to be not a number, then it is a symbol, terminated by
 * an unescaped non-symbol characters: {@code [\0-\32]|NO_BREAK_SPACE|["';#()\[\]`,]}.</li>
 * </ul>
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
         * Somehow equivalent to {@link EOF}, but causes the parser to return nil
         */
        record SkipToEnd() implements TokenData {
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
         * Opening string with properties as is in {@code #(}
         */
        record StrWithPropsOpen() implements TokenData {
        }

        /**
         * Closing parenthesis
         *
         * <p>
         * Please note that it also closes {@link RecordOpen}, etc.
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
         * Opening char table as is in {@code #^[}
         */
        record CharTableOpen() implements TokenData {
        }

        /**
         * Opening sub char table as is in {@code #^^[}
         */
        record SubCharTableOpen() implements TokenData {
        }

        /**
         * Closing square bracket
         *
         * <p>
         * Please note that it also closes {@link ByteCodeOpen}, etc.
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

        record Str(MutableTruffleString value) implements TokenData {
        }

        record Num(NumberVariant value) implements TokenData {
        }

        record Symbol(String value, boolean intern, boolean shorthand) implements TokenData {
        }

        /**
         * Bool vector as is in {@code #&10"value"}
         */
        record BoolVec(long length, MutableTruffleString value) implements TokenData {
        }
    }

    /**
     * @param data   the actual token data
     * @param source the source section of the token
     */
    record Token(TokenData data, SourceSection source) {
    }

    private static final TokenData.EOF EOF = new TokenData.EOF();
    private static final TokenData.SkipToEnd SKIP_TO_END = new TokenData.SkipToEnd();
    private static final TokenData.Dot DOT = new TokenData.Dot();
    private static final TokenData.ParenOpen PAREN_OPEN = new TokenData.ParenOpen();
    private static final TokenData.RecordOpen RECORD_OPEN = new TokenData.RecordOpen();
    private static final TokenData.StrWithPropsOpen STR_WITH_PROPS_OPEN = new TokenData.StrWithPropsOpen();
    private static final TokenData.ParenClose PAREN_CLOSE = new TokenData.ParenClose();
    private static final TokenData.SquareOpen SQUARE_OPEN = new TokenData.SquareOpen();
    private static final TokenData.ByteCodeOpen BYTE_CODE_OPEN = new TokenData.ByteCodeOpen();
    private static final TokenData.CharTableOpen CHAR_TABLE_OPEN = new TokenData.CharTableOpen();
    private static final TokenData.SubCharTableOpen SUB_CHAR_TABLE_OPEN = new TokenData.SubCharTableOpen();
    private static final TokenData.SquareClose SQUARE_CLOSE = new TokenData.SquareClose();
    private static final TokenData.Function FUNCTION = new TokenData.Function();
    private static final TokenData.Quote QUOTE = new TokenData.Quote();
    private static final TokenData.BackQuote BACK_QUOTE = new TokenData.BackQuote();
    private static final TokenData.Unquote UNQUOTE = new TokenData.Unquote();
    private static final TokenData.UnquoteSplicing UNQUOTE_SPLICING = new TokenData.UnquoteSplicing();
    static final TokenData.Symbol EMPTY_SYMBOL = new TokenData.Symbol("", true, true);
    static final TokenData.Symbol NIL_SYMBOL = new TokenData.Symbol("nil", true, false);

    private static final int NO_BREAK_SPACE = 0x00A0;
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
    private boolean shebang = false;

    /**
     * Reads a full Unicode codepoint from the reader.
     *
     * <p>
     * The {@link #eof}, {@link #line} and {@link #offset} fields are
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
     * @see <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html">
     * elisp/Basic Char Syntax</a>
     */
    private static int mapBasicEscapeCode(int c) {
        return switch (c) {
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
     * @see <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Char-Bits.html">
     * elisp/Other Char Bits</a>
     */
    private static int mapMetaBits(int c, boolean inString) {
        return switch (c) {
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
        // When `digit == -1`, `i != digits` allows reading an infinitely large number.
        for (int i = 0; i != digits; i++) {
            int c = peekCodepoint();
            if (earlyReturn) {
                if (c == -1) {
                    return value;
                }
            } else {
                noEOF(c);
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
        if (inString && meta > 8) {
            throw new IOException("Invalid modifier in string");
        }
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
    private MutableTruffleString readStr() throws IOException {
        ELispString.Builder sb = new ELispString.Builder();
        while (true) {
            int c = noEOF(peekCodepoint());
            switch (c) {
                case '"' -> {
                    readCodepoint();
                    return sb.toTruffleString();
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

    private static boolean isAlphaNumeric(int c) {
        return '0' <= c && c <= '9' || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z';
    }

    private TokenData readHashNumToken(int base) throws IOException {
        BigInteger value = readInteger(base, -1, true);
        switch (peekCodepoint()) {
            // #1#
            case '#' -> {
                readCodepoint();
                return new TokenData.CircularRef(value.longValueExact());
            }
            // #1=
            case '=' -> {
                readCodepoint();
                return new TokenData.CircularDef(value.longValueExact());
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
                if (isAlphaNumeric(peekCodepoint())) {
                    throw new IOException("Invalid character");
                }
                return new TokenData.Num(NumberVariant.from(number));
            }
            default -> {
                if (isAlphaNumeric(peekCodepoint())) {
                    throw new IOException("Invalid character");
                }
                return new TokenData.Num(NumberVariant.from(value));
            }
        }
    }

    private static boolean potentialUnescapedSymbolChar(int c) {
        //noinspection ConditionCoveredByFurtherCondition
        return c > 32 && c != NO_BREAK_SPACE
                && (c >= 128 // noinspection: A quick check
                || !(
                c == '"' || c == '\'' || c == ';' || c == '#'
                        || c == '(' || c == ')' || c == '[' || c == ']'
                        || c == '`' || c == ','
        ));
    }

    private TokenData readSymbolOrNumber(int alreadyRead, boolean uninterned, boolean noShorthand)
            throws IOException {
        boolean symbolOnly = uninterned || noShorthand;
        StringBuilder sb = new StringBuilder();
        boolean escaped = false;
        int c = alreadyRead;
        while (potentialUnescapedSymbolChar(c)) {
            if (alreadyRead == -1) {
                readCodepoint();
            } else {
                alreadyRead = -1;
            }
            if (c == '\\') {
                escaped = true;
                sb.appendCodePoint(readCodepoint());
            } else {
                sb.appendCodePoint(c);
            }
            c = peekCodepoint();
        }
        String symbol = sb.toString();
        if (!symbolOnly && !escaped) {
            Matcher integer = INTEGER_PATTERN.matcher(symbol);
            if (integer.matches()) {
                NumberVariant inner = NumberVariant.from(new BigInteger(integer.group(1)));
                return new TokenData.Num(inner);
            }
            if (FLOAT_PATTERN.matcher(symbol).matches()) {
                return new TokenData.Num(new NumberVariant.Float(parseFloat(symbol)));
            }
        }
        return new TokenData.Symbol(symbol, !uninterned, !noShorthand);
    }

    private static double parseFloat(String symbol) {
        double f;
        // Handle 1.0e+INF or 1.0e+NaN
        if (symbol.endsWith("+INF")) {
            f = symbol.startsWith("-") ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        } else if (symbol.endsWith("+NaN")) {
            // Emacs encodes (number)e+NaN into the significand of the NaN...
            double base = Double.parseDouble(symbol.substring(0, symbol.length() - 5));
            boolean neg = Math.copySign(1.0, base) == -1.0;
            long SIGNIFICAND_BITS = 52;
            long SIGNIFICAND_MASK = (0x1L << (SIGNIFICAND_BITS - 1)) - 1;
            long EXP_BITS = 11;
            long NAN_EXP = (0x1 << (EXP_BITS + 1)) - 1;
            long significand = ((long) Math.abs(base)) & SIGNIFICAND_MASK;
            f = Double.longBitsToDouble(
                    ((neg ? 0x1L : 0x0L) << 63) | significand | (NAN_EXP << (SIGNIFICAND_BITS - 1))
            );
        } else {
            f = Double.parseDouble(symbol);
        }
        return f;
    }

    private static boolean charNextToDotAllowed(int c) {
        return c <= 32 || c == NO_BREAK_SPACE
                || c == '"' || c == '\'' || c == ';'
                || c == '(' || c == '[' || c == '#'
                || c == '?' || c == '`' || c == ',';
    }

    /**
     * Returns the next ELisp token from the reader.
     *
     * <p>
     * It is loosely based on the Emacs implementation in {@code read0 @ src/lread.c}.
     * You should definitely read that if you are trying to understand what this code does
     * as well as what semantically each {@link TokenData} means.
     * A summary is given at the Javadoc of this class ({@link ELispLexer}).
     * </p>
     *
     * @return {@code null} if there is a comment, or the next token otherwise.
     */
    @Nullable
    private TokenData lexNext() throws IOException {
        int c = readCodepoint();
        if (c == -1) {
            return EOF;
        }
        if (c == '.' && charNextToDotAllowed(peekCodepoint())) {
            return DOT;
        }
        return switch (c) {
            case '(' -> PAREN_OPEN;
            case ')' -> PAREN_CLOSE;
            case '[' -> SQUARE_OPEN;
            case ']' -> SQUARE_CLOSE;
            case '#' -> {
                int next = peekCodepoint();
                if ('0' <= next && next <= '9') {
                    yield readHashNumToken(10);
                }
                readCodepoint();
                yield switch (next) {
                    case '\'' -> FUNCTION;
                    case '#' -> EMPTY_SYMBOL;
                    case 's' -> {
                        if (noEOF(readCodepoint()) != '(') {
                            throw new IOException("Expected '('");
                        }
                        yield RECORD_OPEN;
                    }
                    case '^' -> {
                        int subChar = noEOF(readCodepoint());
                        if (subChar == '^') {
                            if (noEOF(readCodepoint()) == '[') {
                                yield SUB_CHAR_TABLE_OPEN;
                            }
                        } else if (subChar == '[') {
                            yield CHAR_TABLE_OPEN;
                        }
                        throw new IOException("Expected '^' or '['");
                    }
                    case '(' -> STR_WITH_PROPS_OPEN;
                    case '[' -> BYTE_CODE_OPEN;
                    case '&' -> {
                        BigInteger length = readInteger(10, -1, true);
                        long l = length.longValueExact();
                        if (readCodepoint() != '\"') {
                            throw new IOException("Expected '\"'");
                        }
                        yield new TokenData.BoolVec(l, readStr());
                    }
                    case '!' -> {
                        readLine();
                        if (line == 1) {
                            shebang = true;
                        }
                        yield null;
                    }
                    case 'x', 'X' -> readHashNumToken(16);
                    case 'o', 'O' -> readHashNumToken(8);
                    case 'b', 'B' -> readHashNumToken(2);
                    case '@' -> {
                        long start = offset;
                        long skip = readInteger(10, -1, true).longValueExact();
                        if (skip == 0 && offset - start == 2) {
                            // #@00 skips to the end of the file
                            eof = true;
                            yield SKIP_TO_END;
                        }
                        //noinspection StatementWithEmptyBody
                        while (readCodepoint() != '\037') {
                            // Yes, it is how Emacs does it...
                        }
                        yield null;
                    }
                    // TODO: #$ - Vload_file_name
                    case '$' -> NIL_SYMBOL;
                    case ':' -> potentialUnescapedSymbolChar(peekCodepoint())
                            ? readSymbolOrNumber(readCodepoint(), true, false)
                            : new TokenData.Symbol("", false, true);
                    case '_' -> potentialUnescapedSymbolChar(peekCodepoint())
                            ? readSymbolOrNumber(readCodepoint(), false, true)
                            : EMPTY_SYMBOL;
                    default -> throw new IOException("Expected a number base indicator");
                };
            }
            case '?' -> new TokenData.Char(readChar(false));
            case '"' -> new TokenData.Str(readStr());
            case '\'' -> QUOTE;
            case '`' -> BACK_QUOTE;
            case ',' -> {
                if (peekCodepoint() == '@') {
                    readCodepoint();
                    yield UNQUOTE_SPLICING;
                }
                yield UNQUOTE;
            }
            case ';' -> {
                String comment = readLine();
                if (line == 1 || (shebang && line == 2)) {
                    Matcher matcher = LEXICAL_BINDING_PATTERN.matcher(comment);
                    if (matcher.find()) {
                        yield new TokenData.SetLexicalBindingMode(
                                !matcher.group(1).equals("nil")
                        );
                    }
                }
                yield null;
            }
            default -> potentialUnescapedSymbolChar(c) ? readSymbolOrNumber(c, false, false) : null;
        };
    }

    Token next() throws IOException {
        if (eof) {
            throw new IOException("Unexpected EOF");
        }
        while (true) {
            while (true) {
                int c = peekCodepoint();
                if (c != ' ' && c != '\n' && c != '\r' && c != '\t') {
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
