package party.iroiro.juicemacs.elisp.parser;

import java.io.EOFException;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_CHAR;
import static party.iroiro.juicemacs.elisp.parser.CodePointReader.noEOF;

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
///   - `#[...]`: A interpreted closure or a bytecode function.
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
/// just fine. But we choose to throw an error in such cases (see [#reader.readFromCharReader()].
///
public class ELispLexer {

    public static final int LINE_CONTINUATION = Integer.MIN_VALUE;

    public record LocatedToken(Token token, int startLine, int startColumn, int endLine, int endColumn) {
        public void attachLocation(ELispCons cons) {
            cons.setSourceLocation(startLine, startColumn, endLine, endColumn);
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
         * Opening a closure or bytecode function as is in {@code #[}
         */
        record ClosureOpen() implements Token {
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
         * Please note that it also closes {@link ClosureOpen}, etc.
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

        /**
         * Reference to {@link party.iroiro.juicemacs.elisp.runtime.ELispGlobals#LOAD_FILE_NAME}
         * as is in {@code #$} in lazy docstrings
         */
        record LoadFileName() implements Token {
        }

        record Char(int value) implements Token {
        }

        record Str(MuleString value) implements Token {
        }

        record FixNum(long value) implements Token {
        }

        record BigNum(BigInteger value) implements Token {
        }

        record FloatNum(double value) implements Token {
        }

        record Symbol(MuleString value, boolean intern, boolean shorthand) implements Token {
            public Symbol(String value, boolean intern, boolean shorthand) {
                this(MuleString.fromString(value), intern, shorthand);
            }
        }

        /**
         * Bool vector as is in {@code #&10"value"}
         */
        record BoolVec(long length, MuleString value) implements Token {
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
    private static final Token.ClosureOpen CLOSURE_OPEN = new Token.ClosureOpen();
    private static final Token.CharTableOpen CHAR_TABLE_OPEN = new Token.CharTableOpen();
    private static final Token.SubCharTableOpen SUB_CHAR_TABLE_OPEN = new Token.SubCharTableOpen();
    private static final Token.SquareClose SQUARE_CLOSE = new Token.SquareClose();
    private static final Token.Function FUNCTION = new Token.Function();
    private static final Token.Quote QUOTE = new Token.Quote();
    private static final Token.BackQuote BACK_QUOTE = new Token.BackQuote();
    private static final Token.Unquote UNQUOTE = new Token.Unquote();
    private static final Token.UnquoteSplicing UNQUOTE_SPLICING = new Token.UnquoteSplicing();
    private static final Token.LoadFileName LOAD_FILE_NAME = new Token.LoadFileName();
    static final Token.Symbol EMPTY_SYMBOL = new Token.Symbol("", true, true);
    static final Token.Symbol NIL_SYMBOL = new Token.Symbol("nil", true, false);

    private static final int NO_BREAK_SPACE = 0x00A0;
    private static final Pattern LEXICAL_BINDING_PATTERN = Pattern.compile(
            "-\\*-(?:|.*;)[ \t]*lexical-binding:[ \t]*([^;]*[^ \t;]).*-\\*-"
    );
    public static final Pattern INTEGER_PATTERN = Pattern.compile("^([+-]?[0-9]+)\\.?$");
    public static final Pattern FLOAT_PATTERN = Pattern.compile("^[+-]?(?:[0-9]+\\.?[0-9]*|[0-9]*\\.?[0-9]+)(?:e(?:[+-]?[0-9]+|\\+INF|\\+NaN))?$");

    private final CodePointReader reader;

    public ELispLexer(Source source) {
        if (source.hasBytes()) {
            this.reader = CodePointReader.from(new ByteSequenceReader(source.getBytes(), StandardCharsets.UTF_8));
        } else {
            this.reader = CodePointReader.from(source.getReader());
        }
    }

    public ELispLexer(CodePointReader reader) {
        this.reader = reader;
    }

    boolean hasNext() {
        return !reader.isEof();
    }

    private boolean shebang = false;

    public long getCodePointOffset() {
        return reader.getCodePointOffset();
    }

    private String readLine() throws IOException {
        StringBuilder sb = new StringBuilder();
        while (true) {
            int c = reader.peek();
            if (c == -1 || c == '\n') {
                break;
            }
            reader.read();
            sb.appendCodePoint(Character.isValidCodePoint(c) ? c : ' ');
        }
        return sb.toString();
    }

    private void skipLine() throws IOException {
        while (true) {
            int c = reader.peek();
            if (c == -1 || c == '\n') {
                break;
            }
            reader.read();
        }
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

    /// Reads in an integer
    ///
    /// When `digits == -1`, we should support reading in an infinitely large number.
    /// In this case, this function reads in one `long` portion at a time, and the caller
    /// is responsible for concatenating all `long` integers into a [BigInteger] (by
    /// detecting if there are unprocessed trailing numbers).
    /// (Currently only [#readIntToken(int)] handles this, because characters, back refs,
    /// etc. are not expected to surpass [Long#MAX_VALUE].)
    private long readInteger(int base, int digits, boolean earlyReturn) throws IOException {
        long value = 0;
        // When `digit == -1`, `i != digits` allows reading an infinitely large number.
        for (int i = 0; i != digits; i++) {
            int c = reader.peek();
            if (earlyReturn) {
                if (c == -1) {
                    break;
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
                    break;
                }
                throw ELispSignals.invalidReadSyntax("Invalid number");
            }
            // 0 <= digit < base <= 37 < 64
            if (value > (Long.MAX_VALUE >> 6)) {
                try {
                    value = Math.addExact(Math.multiplyExact(value, base), digit);
                } catch (ArithmeticException e) {
                    // Current char is not consumed, to be processed by caller.
                    return value;
                }
            } else {
                value = value * base + digit;
            }
            reader.read();
        }
        return value;
    }

    private Token readIntToken(int base) throws IOException {
        int leading = reader.peek();
        boolean negative = leading == '-';
        if (negative || leading == '+') {
            reader.read();
        }
        long value = readInteger(base, -1, true);
        if (!isAlphaNumeric(reader.peek())) {
            return new Token.FixNum(negative ? -value : value);
        }
        BigInteger bigBase = BigInteger.valueOf(base);
        BigInteger bigValue = BigInteger.valueOf(value);
        do {
            value = readInteger(base, 1, false);
            bigValue = bigValue.multiply(bigBase).add(BigInteger.valueOf(value));
        } while (isAlphaNumeric(reader.peek()));
        return new Token.BigNum(negative ? bigValue.negate() : bigValue);
    }

    private int readEscapedCodepoint(int base, int digits, boolean earlyReturn) throws IOException {
        long value = readInteger(base, digits, earlyReturn);
        if (value < 0 || MAX_CHAR < value) {
            throw ELispSignals.error("Not a valid Unicode code point");
        }
        return (int) value;
    }

    private int readControlChar(boolean inString) throws IOException {
        if (reader.peek() == -1) {
            throw ELispSignals.invalidReadSyntax("Invalid modifier");
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
        return maybeRawByte(c | modifier, inString);
    }

    private void assertNoSymbolBehindChar() throws IOException {
        int nch = reader.peek();
        if (nch <= 32
                || nch == '"' || nch == '\'' || nch == ';' || nch == '('
                || nch == ')' || nch == '['  || nch == ']' || nch == '#'
                || nch == '?' || nch == '`'  || nch == ',' || nch == '.') {
            return;
        }
        throw ELispSignals.invalidReadSyntax("Invalid char");
    }

    private int maybeRawByte(int c, boolean inString) {
        if (!inString) {
            return c;
        }
        if (0x80 <= c && c <= 0xFF) {
            return -c;
        }
        return c;
    }

    /**
     * Read the remaining encoded ELisp character following the {@code ?} character.
     */
    @CompilerDirectives.TruffleBoundary
    private int readChar(boolean inString) throws IOException {
        int c = noEOF(reader.read());
        // Normal characters: ?a => 'a'
        if (c != '\\') {
            return c;
        }
        int escaped = noEOF(reader.peek());
        // Octal escape: ?\001 => '\001', ?\1 => '\001'
        if ('0' <= escaped && escaped <= '7') {
            return maybeRawByte(readEscapedCodepoint(8, 3, true), inString);
        }
        reader.read();
        if (inString) {
            // Line continuation, ignored.
            if (escaped == ' ' || escaped == '\n') {
                return LINE_CONTINUATION;
            }
        } else {
            if (escaped == '\n') {
                throw ELispSignals.invalidReadSyntax("Unexpected newline");
            }
        }
        // Meta-prefix: ?\s-a => Bit annotated character
        int meta = mapMetaMask(escaped, inString);
        if (meta != -1) {
            if (reader.peek() == '-' && !(inString && escaped == 's')) { // "\s-x" => " -x"
                reader.read();
                if (inString && meta > 0x80) {
                    throw ELispSignals.invalidReadSyntax("Invalid modifier in string");
                }
                return maybeRawByte(meta | readChar(inString), inString);
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
                if (reader.peek() == '-') {
                    reader.read();
                    yield readControlChar(inString);
                } else {
                    throw ELispSignals.invalidReadSyntax("Invalid modifier");
                }
            }
            case 'x' -> maybeRawByte(readEscapedCodepoint(16, -1, true), inString);
            case 'u' -> readEscapedCodepoint(16, 4, false);
            case 'U' -> readEscapedCodepoint(16, 8, false);
            case 'N' -> {
                if (reader.peek() == '{') {
                    reader.read();
                    StringBuilder unicodeName = new StringBuilder();
                    boolean whitespace = false;
                    int u;
                    while ((u = reader.read()) != '}') {
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
                    String unicode = unicodeName.toString();
                    try {
                        if (unicode.startsWith("U+")) {
                            int codepoint = Integer.parseInt(unicode, 2, unicode.length(), 16);
                            if (codepoint > Character.MAX_CODE_POINT) {
                                throw ELispSignals.invalidReadSyntax("Invalid Unicode codepoint");
                            }
                            yield codepoint;
                        }
                        yield Character.codePointOf(unicode);
                    } catch (IllegalArgumentException e) {
                        throw ELispSignals.invalidReadSyntax("Invalid Unicode name");
                    }
                }
                yield escaped;
            }
            default -> escaped;
        };
    }

    /**
     * Read the remaining ELisp string after the initial {@code "} character.
     */
    private MuleString readStr() throws IOException {
        MuleStringBuffer sb = new MuleStringBuffer();
        while (true) {
            int c = noEOF(reader.peek());
            switch (c) {
                case '"' -> {
                    reader.read();
                    return sb.build();
                }
                case '\\' -> {
                    c = readChar(true);
                    if (c == LINE_CONTINUATION) {
                        continue;
                    }
                    if (c < 0) {
                        sb.appendRawByte((byte) -c);
                    } else {
                        sb.appendCodePoint(c);
                    }
                }
                default -> {
                    reader.read();
                    sb.appendCodePoint(c);
                }
            }
        }
    }

    private static boolean isAlphaNumeric(int c) {
        return ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
    }

    private Token readHashNumToken() throws IOException {
        long value = readInteger(10, -1, true);
        switch (reader.peek()) {
            // #1#
            case '#' -> {
                reader.read();
                return new Token.CircularRef(value);
            }
            // #1=
            case '=' -> {
                reader.read();
                return new Token.CircularDef(value);
            }
            // #24r1k => base-24 integer
            case 'r' -> {
                reader.read();
                if (value < 0 || 36 < value) {
                    throw ELispSignals.invalidReadSyntax("Invalid base");
                }
                return readIntToken((int) value);
            }
            default -> throw ELispSignals.invalidReadSyntax("Invalid character");
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
        MuleStringBuffer sb = new MuleStringBuffer();
        boolean escaped = false;
        boolean nonNumber = false;
        int c = alreadyRead;
        while (potentialUnescapedSymbolChar(c)) {
            if (alreadyRead == -1) {
                reader.read();
            } else {
                alreadyRead = -1;
            }
            int codePoint;
            if (c == '\\') {
                escaped = true;
                codePoint = reader.read();
            } else {
                codePoint = c;
            }
            sb.appendCodePoint(codePoint);
            nonNumber |= codePoint > 'e';
            c = reader.peek();
        }
        MuleString symbol = sb.build();
        if (!symbolOnly && !escaped && !nonNumber) {
            String number = symbol.toString();
            Matcher integer = INTEGER_PATTERN.matcher(number);
            if (integer.matches()) {
                String text = integer.group(1);
                if (text.length() <= 18) {
                    return new Token.FixNum(Long.parseLong(
                            text,
                            0,
                            text.endsWith(".") ? text.length() - 1 : text.length(),
                            10
                    ));
                }
                return new Token.BigNum(new BigInteger(text));
            }
            if (FLOAT_PATTERN.matcher(number).matches()) {
                return new Token.FloatNum(parseFloat(number));
            }
        }
        return new Token.Symbol(symbol, !uninterned, !noShorthand);
    }

    private static final long SIGNIFICAND_BITS = 52;
    private static final long SIGNIFICAND_MASK = (0x1L << (SIGNIFICAND_BITS - 1)) - 1;
    private static final long EXP_BITS = 11;
    private static final long NAN_EXP = (0x1 << (EXP_BITS + 1)) - 1;

    private static double parseFloat(String symbol) {
        double f;
        // Handle 1.0e+INF or 1.0e+NaN
        if (symbol.endsWith("+INF")) {
            f = symbol.startsWith("-") ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        } else if (symbol.endsWith("+NaN")) {
            // Emacs encodes (number)e+NaN into the significand of the NaN...
            double base = Double.parseDouble(symbol.substring(0, symbol.length() - 5));
            boolean neg = Math.copySign(1.0, base) == -1.0;
            long significand = ((long) Math.abs(base)) & SIGNIFICAND_MASK;
            long rawLong = ((neg ? 0x1L : 0x0L) << 63) | significand | (NAN_EXP << (SIGNIFICAND_BITS - 1));
            f = Double.longBitsToDouble(rawLong);
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
        int c = reader.read();
        if (c == -1) {
            return EOF;
        }
        if (c == '.' && charNextToDotAllowed(reader.peek())) {
            return DOT;
        }
        return switch (c) {
            case '(' -> PAREN_OPEN;
            case ')' -> PAREN_CLOSE;
            case '[' -> SQUARE_OPEN;
            case ']' -> SQUARE_CLOSE;
            case '#' -> {
                int next = reader.peek();
                if ('0' <= next && next <= '9') {
                    yield readHashNumToken();
                }
                reader.read();
                yield switch (next) {
                    case '\'' -> FUNCTION;
                    case '#' -> EMPTY_SYMBOL;
                    case 's' -> {
                        if (noEOF(reader.read()) != '(') {
                            throw ELispSignals.invalidReadSyntax("Expected '('");
                        }
                        yield RECORD_OPEN;
                    }
                    case '^' -> {
                        int subChar = noEOF(reader.read());
                        if (subChar == '^') {
                            if (noEOF(reader.read()) == '[') {
                                yield SUB_CHAR_TABLE_OPEN;
                            }
                        } else if (subChar == '[') {
                            yield CHAR_TABLE_OPEN;
                        }
                        throw ELispSignals.invalidReadSyntax("Expected '^' or '['");
                    }
                    case '(' -> STR_WITH_PROPS_OPEN;
                    case '[' -> CLOSURE_OPEN;
                    case '&' -> {
                        long length = readInteger(10, -1, true);
                        if (reader.read() != '\"') {
                            throw ELispSignals.invalidReadSyntax("Expected '\"'");
                        }
                        yield new Token.BoolVec(length, readStr());
                    }
                    case '!' -> {
                        skipLine();
                        if (reader.getLine() == 1) {
                            shebang = true;
                        }
                        yield null;
                    }
                    case 'x', 'X' -> readIntToken(16);
                    case 'o', 'O' -> readIntToken(8);
                    case 'b', 'B' -> readIntToken(2);
                    case '@' -> {
                        if (reader.peek() == '0') {
                            reader.read();
                            if (reader.read() == '0') {
                                // #@00 skips to the end of the file
                                reader.read();
                                reader.setEof(true);
                                yield SKIP_TO_END;
                            }
                        }
                        /* long skip = */ readInteger(10, -1, true);
                        // TODO: More efficient way to skip?
                        //noinspection StatementWithEmptyBody
                        while (reader.read() != '\037') {
                            // Yes, it is how Emacs does it...
                        }
                        yield null;
                    }
                    case '$' -> LOAD_FILE_NAME;
                    case ':' -> potentialUnescapedSymbolChar(reader.peek())
                            ? readSymbolOrNumber(reader.read(), true, false)
                            : new Token.Symbol("", false, true);
                    case '_' -> potentialUnescapedSymbolChar(reader.peek())
                            ? readSymbolOrNumber(reader.read(), false, true)
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
                if (reader.peek() == '@') {
                    reader.read();
                    yield UNQUOTE_SPLICING;
                }
                yield UNQUOTE;
            }
            case ';' -> {
                if (reader.getLine() == 1 || (shebang && reader.getLine() == 2)) {
                    String comment = readLine();
                    Matcher matcher = LEXICAL_BINDING_PATTERN.matcher(comment);
                    if (matcher.find()) {
                        yield new Token.SetLexicalBindingMode(
                                !matcher.group(1).equals("nil")
                        );
                    }
                } else {
                    skipLine();
                }
                yield null;
            }
            default -> potentialUnescapedSymbolChar(c) ? readSymbolOrNumber(c, false, false) : null;
        };
    }

    @CompilerDirectives.TruffleBoundary
    LocatedToken next() throws IOException {
        if (!hasNext()) {
            throw new EOFException();
        }
        while (true) {
            while (true) {
                int c = reader.peek();
                if (c != ' ' && c != '\n' && c != '\r' && c != '\t') {
                    break;
                }
                reader.read();
            }
            int statLine = reader.getLine();
            int startColumn = reader.getColumn();
            Token data = lexNext();
            if (data != null) {
                int endLine = reader.getLine();
                int endColumn = reader.getColumn();
                if (endColumn == 1) {
                    endLine--;
                    if (endLine == statLine) {
                        endColumn = startColumn;
                    }
                } else {
                    endColumn--;
                }
                return new LocatedToken(data, statLine, startColumn, endLine, endColumn);
            }
        }
    }
}
