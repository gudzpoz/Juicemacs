package party.iroiro.juicemacs.elisp.parser;

import java.io.EOFException;
import java.io.IOException;
import java.io.Reader;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.strings.MutableTruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

/// A ELisp lexer
///
/// This lexer is more or less based on the ELisp manual,
/// and borrows heavily from the following two implementation:
///
///
/// - <a href="https://git.savannah.gnu.org/cgit/guile.git/tree/module/language/elisp/lexer.scm">
///   The ELisp lexer in Guile</a>
/// - <a href="https://github.com/talyz/fromElisp">talyz/fromElisp in Nix</a>
/// - <a href="http://git.savannah.gnu.org/cgit/emacs.git/tree/src/lread.c">
///   The ELisp lexer & parser in Emacs</a>
///
///
/// This lexer tries to support all ELisp grammar as is listed below.
/// If you are to understand how many features are actually
/// supported by canonical ELisp (as is in Emacs), I would recommend reading the `read0`
/// function in the Emacs source code.
///
/// ## Emacs ELisp Features
///
/// The following lists the features supported by Emacs ELisp,
/// serving as my personal notes as well as a reference for future readers.
/// Because the Emacs implementation integrates lexer and parser into the same function,
/// the logic can be a bit convoluted.
///
///
/// - Unless otherwise noted, the "(" character marks `RE_list_start`.
/// - The ")" character marks the end of a `RE_list` (or `RE_list_start`),
///   a `RE_record` or a `RE_string_props`.
/// - The "\[" character marks the start of a `RE_vector`.
/// - The "]" character marks the end of a `RE_vector`, a `RE_byte_code`,
///   a `RE_char_table` or a `RE_sub_char_table`.
/// - The "#" character marks the start of many, many things.
///
///   - `#'X`: Special syntax for function symbol reference.
///   - `##`: The empty symbol.
///   - `#s(...)`: A record or hash-table.
///   - `#^[...]`: A character table.
///   - `#^^[...]`: A sub-char-table.
///   - `#(...)`: String with properties.
///   - `#[...]`: Byte code.
///   - `#&N"..."`: A bool vector.
///   - `#!`: The shebang.
///   - `#xFFFF / #XFFFF`: A hex integer.
///   - `#b1010 / #B1010`: A binary integer.
///   - `#o7070 / #O7070`: An octal integer.
///   - `#@NUMBER`: Used to skip `NUMBER` following bytes.
///   - `#$`: Reference to lazy-loaded string.
///   - `#:X`: Uninterned symbol.
///   - `#_X`: Symbol without shorthand.
///   - `#36rX`: A custom-radix integer.
///   - `#1= / #1#`: A cyclic definition / reference.
///
/// - The "?" character marks the start of a character literal.
/// - The `"` character marks the start of a string.
/// - The "'" character marks a `RE_special(Qquote)` .
/// - The {@code `} character marks a `RE_special(Qbackquote)` .
/// - The ",@" string marks a `RE_special(Qcomma_at)` .
/// - The "," character marks a `RE_special(Qcomma)` .
/// - The ";" character marks a comment.
/// - The "." character is used in a cons pair, only if it is followed by the following chars:
///   {@code [\0-\32]|NO_BREAK_SPACE|["';()\[#?`,]} (`)]` are not in the list).
/// - Otherwise, the characters that follows will be first parsed as numbers.
/// - If it turns out to be not a number, then it is a symbol, terminated by
///   an unescaped non-symbol characters: {@code [\0-\32]|NO_BREAK_SPACE|["';#()\[\]`,]}.
///
/// ## Error Handling
///
/// Emacs mainly throws `invalid-read-syntax` errors but sometimes throws plain `error`s.
/// We tend to use `invalid-read-syntax` errors for all errors, but reserve `error`s for
/// Unicode errors in the input. (`"?\UFFFFFFFF"` -> `invalid-read-syntax`,
/// input file has invalid Unicode -> `error`).
///
/// One more thing to note is that Emacs seems to read input files with Unicode errors
/// just fine. But we choose to throw an error in such cases (see [#readCodepointFromCharReader()].
///
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

    sealed interface Token {

        /**
         * End-of-file indicator
         */
        record EOF() implements Token {
        }

        /**
         * Somehow equivalent to {@link EOF}, but causes the parser to return nil
         */
        record SkipToEnd() implements Token {
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
        record SetLexicalBindingMode(boolean value) implements Token {
        }

        /**
         * A dot as is in {@code (1.0 . 2.0)}
         */
        record Dot() implements Token {
        }

        /**
         * Opening parenthesis
         */
        record ParenOpen() implements Token {
        }

        /**
         * Opening record as is in {@code #s(}
         */
        record RecordOpen() implements Token {
        }

        /**
         * Opening string with properties as is in {@code #(}
         */
        record StrWithPropsOpen() implements Token {
        }

        /**
         * Closing parenthesis
         *
         * <p>
         * Please note that it also closes {@link RecordOpen}, etc.
         * </p>
         */
        record ParenClose() implements Token {
        }

        /**
         * Opening square bracket
         */
        record SquareOpen() implements Token {
        }

        /**
         * Opening byte code as is in {@code #[}
         */
        record ByteCodeOpen() implements Token {
        }

        /**
         * Opening char table as is in {@code #^[}
         */
        record CharTableOpen() implements Token {
        }

        /**
         * Opening sub char table as is in {@code #^^[}
         */
        record SubCharTableOpen() implements Token {
        }

        /**
         * Closing square bracket
         *
         * <p>
         * Please note that it also closes {@link ByteCodeOpen}, etc.
         * </p>
         */
        record SquareClose() implements Token {
        }

        /**
         * Function quote as is in {@code #'func}
         */
        record Function() implements Token {
        }

        /**
         * Symbol quote as is in {@code 'symbol}
         */
        record Quote() implements Token {
        }

        /**
         * Back quote as is in {@code `symbol}
         */
        record BackQuote() implements Token {
        }

        /**
         * Comma as is in {@code `(,var)}
         */
        record Unquote() implements Token {
        }

        /**
         * Comma at back quote as is in {@code `(,@var)}
         */
        record UnquoteSplicing() implements Token {
        }

        /**
         * Circular reference as is in {@code #1#}
         */
        record CircularRef(long id) implements Token {
        }

        /**
         * Circular definition as is in {@code #1=func}
         */
        record CircularDef(long id) implements Token {
        }

        record Char(int value) implements Token {
        }

        record Str(MutableTruffleString value) implements Token {
        }

        record Num(NumberVariant value) implements Token {
        }

        record Symbol(String value, boolean intern, boolean shorthand) implements Token {
        }

        /**
         * Bool vector as is in {@code #&10"value"}
         */
        record BoolVec(long length, MutableTruffleString value) implements Token {
        }
    }

    private static final Token.EOF EOF = new Token.EOF();
    private static final Token.SkipToEnd SKIP_TO_END = new Token.SkipToEnd();
    private static final Token.Dot DOT = new Token.Dot();
    private static final Token.ParenOpen PAREN_OPEN = new Token.ParenOpen();
    private static final Token.RecordOpen RECORD_OPEN = new Token.RecordOpen();
    private static final Token.StrWithPropsOpen STR_WITH_PROPS_OPEN = new Token.StrWithPropsOpen();
    private static final Token.ParenClose PAREN_CLOSE = new Token.ParenClose();
    private static final Token.SquareOpen SQUARE_OPEN = new Token.SquareOpen();
    private static final Token.ByteCodeOpen BYTE_CODE_OPEN = new Token.ByteCodeOpen();
    private static final Token.CharTableOpen CHAR_TABLE_OPEN = new Token.CharTableOpen();
    private static final Token.SubCharTableOpen SUB_CHAR_TABLE_OPEN = new Token.SubCharTableOpen();
    private static final Token.SquareClose SQUARE_CLOSE = new Token.SquareClose();
    private static final Token.Function FUNCTION = new Token.Function();
    private static final Token.Quote QUOTE = new Token.Quote();
    private static final Token.BackQuote BACK_QUOTE = new Token.BackQuote();
    private static final Token.Unquote UNQUOTE = new Token.Unquote();
    private static final Token.UnquoteSplicing UNQUOTE_SPLICING = new Token.UnquoteSplicing();
    static final Token.Symbol EMPTY_SYMBOL = new Token.Symbol("", true, true);
    static final Token.Symbol NIL_SYMBOL = new Token.Symbol("nil", true, false);

    private static final int NO_BREAK_SPACE = 0x00A0;
    private static final Pattern LEXICAL_BINDING_PATTERN = Pattern.compile(
            "-\\*-(?:|.*;)[ \t]*lexical-binding:[ \t]*([^;]*[^ \t;]).*-\\*-"
    );
    private static final Pattern INTEGER_PATTERN = Pattern.compile("^([+-]?[0-9]+)\\.?$");
    private static final Pattern FLOAT_PATTERN = Pattern.compile("^[+-]?(?:[0-9]+\\.?[0-9]*|[0-9]*\\.?[0-9]+)(?:e(?:[+-]?[0-9]+|\\+INF|\\+NaN))?$");

    @Nullable
    private final ByteSequenceReader byteReader;
    @Nullable
    private final Reader charReader;

    public ELispLexer(Source source) {
        if (source.hasBytes()) {
            // TODO: Support other encodings
            this.byteReader = new ByteSequenceReader(source.getBytes(), StandardCharsets.UTF_8);
            this.charReader = null;
        } else {
            this.byteReader = null;
            this.charReader = source.getReader();
        }
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
    private int column = 1;
    private int codepointOffset = 0;
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
     * The {@link #eof}, {@link #line} and {@link #column} fields are
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
            column = 1;
        }
        column++;
        codepointOffset++;
        return c;
    }

    /**
     * @return 1-based line count
     */
    public int getLine() {
        return line;
    }

    /**
     * @return 1-based column count
     */
    public int getColumn() {
        return column;
    }

    public int getCodepointOffset() {
        return codepointOffset;
    }

    private int readCodepointFromCharReader() throws IOException {
        assert charReader != null;
        int c1 = charReader.read();
        if (c1 == -1) {
            return -1;
        }
        if (!Character.isHighSurrogate((char) c1)) {
            return c1;
        }
        int c2 = noEOF(charReader.read());
        if (!Character.isLowSurrogate((char) c2)) {
            // Emacs seems to read text on a best-effort basis...
            // But we choose to differ here.
            throw ELispSignals.error("Invalid Unicode surrogate pair");
        }
        return Character.toCodePoint((char) c1, (char) c2);
    }

    private int peekCodepoint() throws IOException {
        if (peekedCodepoint != -1) {
            return peekedCodepoint;
        }

        peekedCodepoint = byteReader == null ? readCodepointFromCharReader() : byteReader.read();
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

    private final static int CHAR_ALT = 0x0400000;
    private final static int CHAR_SUPER = 0x0800000;
    private final static int CHAR_HYPER = 0x1000000;
    private final static int CHAR_SHIFT = 0x2000000;
    private final static int CHAR_CTL = 0x4000000;
    private final static int CHAR_META = 0x8000000;
    private final static int MODIFIER_MASK = CHAR_ALT | CHAR_SUPER | CHAR_HYPER | CHAR_SHIFT | CHAR_CTL | CHAR_META;

    /**
     * @see <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Char-Bits.html">
     * elisp/Other Char Bits</a>
     */
    private static int mapMetaMask(int c, boolean inString) {
        return switch (c) {
            case 'A' -> CHAR_ALT;
            case 's' -> CHAR_SUPER;
            case 'H' -> CHAR_HYPER;
            case 'S' -> CHAR_SHIFT;
            case 'M' -> inString ? 0x80 : CHAR_META;
            default -> -1;
        };
    }

    private BigInteger readInteger(int base, int digits, boolean earlyReturn) throws IOException {
        BigInteger value = new BigInteger("0", base);
        boolean negative = peekCodepoint() == '-';
        if (negative) {
            readCodepoint();
        }
        // When `digit == -1`, `i != digits` allows reading an infinitely large number.
        for (int i = 0; i != digits; i++) {
            int c = peekCodepoint();
            if (earlyReturn) {
                if (c == -1) {
                    return negative ? value.negate() : value;
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
                    return negative ? value.negate() : value;
                }
                throw ELispSignals.invalidReadSyntax("Expecting fixed number of digits");
            }
            readCodepoint();
            value = value.multiply(BigInteger.valueOf(base)).add(BigInteger.valueOf(digit));
        }
        return negative ? value.negate() : value;
    }

    private int readEscapedCodepoint(int base, int digits, boolean earlyReturn) throws IOException {
        BigInteger value = readInteger(base, digits, earlyReturn);
        if (value.signum() < 0 || value.compareTo(BigInteger.valueOf(Character.MAX_CODE_POINT)) > 0) {
            throw ELispSignals.error("Not a valid Unicode code point");
        }
        return value.intValue();
    }

    private int readControlChar(boolean inString) throws IOException {
        if (peekCodepoint() == -1) {
            return -1;
        }
        int c = readChar(inString);
        int modifier = c & MODIFIER_MASK;
        c = c & ~MODIFIER_MASK;
        if (('@' <= c && c <= '_') || ('a' <= c && c <= 'z')) {
            c &= 0x1F;
        } else if (c == '?') {
            c = 127;
        } else {
            modifier |= CHAR_CTL;
        }
        return c | modifier;
    }

    private void assertNoSymbolBehindChar() throws IOException {
        int nch = peekCodepoint();
        if (nch <= 32
                || nch == '"' || nch == '\'' || nch == ';' || nch == '('
                || nch == ')' || nch == '['  || nch == ']' || nch == '#'
                || nch == '?' || nch == '`'  || nch == ',' || nch == '.') {
            return;
        }
        throw ELispSignals.invalidReadSyntax("Invalid char");
    }

    /**
     * Read the remaining encoded ELisp character following the {@code ?} character.
     */
    @CompilerDirectives.TruffleBoundary
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
        } else {
            if (escaped == '\n') {
                throw ELispSignals.invalidReadSyntax("Unexpected newline");
            }
        }
        // Meta-prefix: ?\s-a => Bit annotated character
        int meta = mapMetaMask(escaped, inString);
        if (meta != -1) {
            if (peekCodepoint() == '-') {
                readCodepoint();
                if (inString && meta > 0x80) {
                    throw ELispSignals.invalidReadSyntax("Invalid modifier in string");
                }
                return meta | readChar(inString);
            } else if (escaped != 's') {
                throw ELispSignals.invalidReadSyntax("Invalid modifier");
            }
        }
        // Escape: ?\n => '\n'
        int escapeCode = mapBasicEscapeCode(escaped);
        if (escapeCode != -1) {
            return escapeCode;
        }
        return switch (escaped) {
            // Control characters: ?\^I or ?\C-I
            case '^' -> readControlChar(inString);
            case 'C' -> {
                if (peekCodepoint() == '-') {
                    readCodepoint();
                    yield readControlChar(inString);
                } else {
                    throw ELispSignals.invalidReadSyntax("Invalid modifier");
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
                        if (Character.isWhitespace(u)) {
                            if (!whitespace) {
                                whitespace = true;
                                unicodeName.append(' ');
                            }
                        } else {
                            whitespace = false;
                            unicodeName.appendCodePoint(u);
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

    private Token readHashNumToken(int base) throws IOException {
        BigInteger value = readInteger(base, -1, true);
        switch (peekCodepoint()) {
            // #1#
            case '#' -> {
                readCodepoint();
                return new Token.CircularRef(value.longValueExact());
            }
            // #1=
            case '=' -> {
                readCodepoint();
                return new Token.CircularDef(value.longValueExact());
            }
            // #24r1k => base-24 integer
            case 'r' -> {
                if (base != 10) {
                    throw ELispSignals.invalidReadSyntax("Invalid base");
                }
                readCodepoint();
                int realBase = value.intValueExact();
                if (realBase > 36) {
                    throw ELispSignals.invalidReadSyntax("Invalid base");
                }
                BigInteger number = readInteger(realBase, -1, true);
                if (isAlphaNumeric(peekCodepoint())) {
                    throw ELispSignals.invalidReadSyntax("Invalid character");
                }
                return new Token.Num(NumberVariant.from(number));
            }
            default -> {
                if (isAlphaNumeric(peekCodepoint())) {
                    throw ELispSignals.invalidReadSyntax("Invalid character");
                }
                return new Token.Num(NumberVariant.from(value));
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

    private Token readSymbolOrNumber(int alreadyRead, boolean uninterned, boolean noShorthand)
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
                return new Token.Num(inner);
            }
            if (FLOAT_PATTERN.matcher(symbol).matches()) {
                return new Token.Num(new NumberVariant.Float(parseFloat(symbol)));
            }
        }
        return new Token.Symbol(symbol, !uninterned, !noShorthand);
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
     * as well as what semantically each {@link Token} means.
     * A summary is given at the Javadoc of this class ({@link ELispLexer}).
     * </p>
     *
     * @return {@code null} if there is a comment, or the next token otherwise.
     */
    @Nullable
    private Token lexNext() throws IOException {
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
                            throw ELispSignals.invalidReadSyntax("Expected '('");
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
                        throw ELispSignals.invalidReadSyntax("Expected '^' or '['");
                    }
                    case '(' -> STR_WITH_PROPS_OPEN;
                    case '[' -> BYTE_CODE_OPEN;
                    case '&' -> {
                        BigInteger length = readInteger(10, -1, true);
                        long l = length.longValueExact();
                        if (readCodepoint() != '\"') {
                            throw ELispSignals.invalidReadSyntax("Expected '\"'");
                        }
                        yield new Token.BoolVec(l, readStr());
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
                        if (peekCodepoint() == '0') {
                            readCodepoint();
                            if (readCodepoint() == '0') {
                                // #@00 skips to the end of the file
                                readCodepoint();
                                eof = true;
                                yield SKIP_TO_END;
                            }
                        }
                        long skip = readInteger(10, -1, true).longValueExact();
                        if (byteReader != null) {
                            byteReader.skipBytes((int) skip, peekedCodepoint != -1);
                            peekedCodepoint = -1;
                        } else {
                            //noinspection StatementWithEmptyBody
                            while (readCodepoint() != '\037') {
                                // Yes, it is how Emacs does it...
                            }
                        }
                        yield null;
                    }
                    // TODO: #$ - Vload_file_name
                    case '$' -> NIL_SYMBOL;
                    case ':' -> potentialUnescapedSymbolChar(peekCodepoint())
                            ? readSymbolOrNumber(readCodepoint(), true, false)
                            : new Token.Symbol("", false, true);
                    case '_' -> potentialUnescapedSymbolChar(peekCodepoint())
                            ? readSymbolOrNumber(readCodepoint(), false, true)
                            : EMPTY_SYMBOL;
                    default -> throw ELispSignals.invalidReadSyntax("Expected a number base indicator");
                };
            }
            case '?' -> {
                int value = readChar(false);
                assertNoSymbolBehindChar();
                yield new Token.Char(value);
            }
            case '"' -> new Token.Str(readStr());
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
                        yield new Token.SetLexicalBindingMode(
                                !matcher.group(1).equals("nil")
                        );
                    }
                }
                yield null;
            }
            default -> potentialUnescapedSymbolChar(c) ? readSymbolOrNumber(c, false, false) : null;
        };
    }

    @CompilerDirectives.TruffleBoundary
    Token next() throws IOException {
        if (eof) {
            throw new EOFException();
        }
        while (true) {
            while (true) {
                int c = peekCodepoint();
                if (c != ' ' && c != '\n' && c != '\r' && c != '\t') {
                    break;
                }
                readCodepoint();
            }
            Token data = lexNext();
            if (data != null) {
                return data;
            }
        }
    }

    private static int noEOF(int c) throws IOException {
        if (c == -1) {
            throw new EOFException();
        }
        return c;
    }

}
