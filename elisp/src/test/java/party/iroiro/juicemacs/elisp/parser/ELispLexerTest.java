package party.iroiro.juicemacs.elisp.parser;

import java.io.EOFException;
import java.io.IOException;
import java.io.StringReader;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.polyglot.io.ByteSequence;
import org.junit.jupiter.api.Test;

import com.oracle.truffle.api.source.Source;

import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.BackQuote;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.BoolVec;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.ClosureOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Char;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.CharTableOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.CircularDef;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.CircularRef;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Dot;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.EOF;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Function;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.ParenClose;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.ParenOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Quote;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.RecordOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.SetLexicalBindingMode;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.SkipToEnd;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.SquareClose;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.SquareOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Str;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.StrWithPropsOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.SubCharTableOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Symbol;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.Unquote;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.UnquoteSplicing;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.mule.MuleString;

import static org.junit.jupiter.api.Assertions.*;

public class ELispLexerTest {

    private ELispLexer lexer(String s) throws IOException {
        return new ELispLexer(Source.newBuilder("elisp", new StringReader(s), "test").build());
    }

    private List<Token> lex(String s) throws IOException {
        ELispLexer lexer = lexer(s);
        List<Token> tokens = new ArrayList<>();
        while (lexer.hasNext()) {
            tokens.add(lexer.next().token());
        }
        return tokens;
    }

    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html
    private static final Object[] INTEGER_TESTS = {
            "1", 1,
            "1.", 1,
            "+1", 1,
            "-1", -1,
            "0", 0,
            "-0", 0,
            "#b101100", 44,
            "#b-101100", -44,
            "#o54", 44,
            "#o-54", -44,
            "#x2c", 44,
            "#x7fffffffffffffff", Long.MAX_VALUE,
            "#x+2c", 44,
            "#x-2c", -44,
            "#24r1k", 44,
            "#24r+1k", 44,
            "#24r-1k", -44,
            "#xffffffffffffffffffffffffffffffffffffffffffffffff",
            new BigInteger("ffffffffffffffffffffffffffffffffffffffffffffffff", 16),
    };

    @Test
    public void testFixNumToken() throws IOException {
        for (String suffix : new String[]{"", " ", ")", "("}) {
            for (int i = 0; i < INTEGER_TESTS.length; i += 2) {
                ELispLexer lexer = lexer(INTEGER_TESTS[i] + suffix);
                assertTrue(lexer.hasNext());
                Object expected = INTEGER_TESTS[i + 1];
                Token expectedNum;
                if (expected instanceof BigInteger big) {
                    expectedNum = new Token.BigNum(big);
                } else {
                    expectedNum = new Token.FixNum(((Number) expected).longValue());
                }
                assertEquals(
                        expectedNum,
                        lexer.next().token(),
                        INTEGER_TESTS[i] + suffix
                );
                assertTrue(lexer.hasNext());
                if (suffix.isEmpty() || suffix.equals(" ")) {
                    assertEquals(new EOF(), lexer.next().token());
                    assertFalse(lexer.hasNext());
                }
            }
        }
    }

    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Float-Basics.html
    private static final Object[] FLOAT_TESTS = {
            "1500.0", 1500.0,
            "+15e2", 1500.0,
            "15.0e+2", 1500.0,
            "+1500000e-3", 1500.0,
            ".15e4", 1500.0,
            "-0.0", -0.0,
            "0.0", 0.0,
            "+0.0", 0.0,
            "1.0e+INF", Double.POSITIVE_INFINITY,
            "-1.0e+INF", Double.NEGATIVE_INFINITY,
            "0.0e+NaN", Double.NaN,
            "-0.0e+NaN", Double.NaN,
            "1.0e+NaN", Double.NaN,
            "-1.0e+NaN", Double.NaN,
            "2251799813685247.0e+NaN", Double.NaN,
            "2251799813685248.0e+NaN", Double.NaN,
    };

    @Test
    public void testFloatToken() throws IOException {
        for (int i = 0; i < FLOAT_TESTS.length; i += 2) {
            ELispLexer lexer = lexer((String) FLOAT_TESTS[i]);
            double expected = (Double) FLOAT_TESTS[i + 1];
            Token actual = lexer.next().token();
            if (Double.isNaN(expected)) {
                assertTrue(
                        Double.isNaN(((Token.FloatNum) actual).value()),
                        (String) FLOAT_TESTS[i]
                );
            } else {
                assertEquals(
                        new Token.FloatNum(expected),
                        actual
                );
            }
        }
    }

    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html
    private static final Object[] CHAR_TESTS = {
            "?A", (int) 'A',
            "?B", (int) 'B',
            "?a", (int) 'a',
            "?Q", 81,
            "?q", 113,
            "?\\(", (int) '(',
            "?\\\\", (int) '\\',
            "?\\a", 7,
            "?\\b", 8,
            "?\\t", 9,
            "?\\n", 10,
            "?\\v", 11,
            "?\\f", 12,
            "?\\r", 13,
            "?\\e", 27,
            "?\\s", 32,
            "?\\d", 127,
            "?\\+", (int) '+',
            "?\\;", (int) ';',
            "?\\|", (int) '|',
            "?\\'", (int) '\'',
            "?\\`", (int) '`',
            "?\\#", (int) '#',
            "?\\u00e0", (int) 'à',
            "?\\U000000E0", (int) 'à',
            "?\\N{LATIN SMALL LETTER A WITH GRAVE}", (int) 'à',
            "?\\N{LATIN SMALL  LETTER   A\n\nWITH\r\n\tGRAVE}", (int) 'à',
            "?\\N", (int) 'N',
            "?\\x41", (int) 'A',
            "?\\x1", 1,
            "?\\xe0", (int) 'à',
            "?\\001", 1,
            "?\\002", 2,
            // TODO: Emacs behaviour: ?\\^ -> -1 for chars, but throws error for strings.
            //   Currently we reuse the logic for strings, and maybe it is not worth it to fix it.
            "?\\^中", 67128877,
            "?\\^I", 9,
            "?\\^i", 9,
            "?\\C-J", 10,
            "?\\C-j", 10,
            "?\\^@", 0,
            "?\\^1", 67108913,
            "?\\^?", 127,
            "?\\C-?", 127,
            "?\\C-a", 1,
            "?\\C-\\C-a", 0x4000001,
            "?\\M-\\C-b", 134217730,
            "?\\M-\\002", 134217730,
            "?\\S-\\002", 33554434,
            "?\\H-\\M-\\A-x", 155189368,
            "?\\A-\\0", 4194304,
            "?\\C-\\0", 67108864,
            "?\\H-\\0", 16777216,
            "?\\M-\\0", 134217728,
            "?\\s-\\0", 8388608,
            "?\\S-\\0", 33554432,
    };

    @Test
    public void testCharToken() throws IOException {
        for (int i = 0; i < CHAR_TESTS.length; i += 2) {
            String s = (String) CHAR_TESTS[i];
            int c = (int) CHAR_TESTS[i + 1];
            ELispLexer lexer = lexer(s);
            Char actual = (Char) lexer.next().token();
            assertEquals(s.length(), lexer.getCodePointOffset(), s);
            assertEquals(c, actual.value(), s);
        }
        "\n \"';()[]#?`,.".chars().forEach((c) -> assertDoesNotThrow(() -> {
            ELispLexer lexer = lexer("?a" + Character.toString(c));
            Char actual = (Char) lexer.next().token();
            assertEquals('a', actual.value());
        }));
    }

    @Test
    public void testStructuralToken() throws IOException {
        // Paren
        assertEquals(Arrays.asList(
                new ParenOpen(),
                new Symbol("a", true, true),
                new ParenClose(),
                new EOF()
        ), lex("(a)"));
        // Square
        assertEquals(Arrays.asList(
                new SquareOpen(),
                new Symbol("a", true, true),
                new SquareClose(),
                new EOF()
        ), lex("[a]"));
        // Quotes
        assertEquals(Arrays.asList(
                new Quote(),
                new Symbol("a", true, true),
                new EOF()
        ), lex("'a"));
        assertEquals(Arrays.asList(
                new BackQuote(),
                new Symbol("a", true, true),
                new EOF()
        ), lex("`a"));
        assertEquals(Arrays.asList(
                new Unquote(),
                new Symbol("a", true, true),
                new EOF()
        ), lex(",a"));
        assertEquals(Arrays.asList(
                new UnquoteSplicing(),
                new Symbol("a", true, true),
                new EOF()
        ), lex(",@a"));
        // Dot
        assertEquals(Arrays.asList(
                new ParenOpen(),
                new Symbol("a", true, true),
                new Dot(),
                new Symbol("b", true, true),
                new ParenClose(),
                new EOF()
        ), lex("(a . b)"));
    }

    @Test
    public void testEOF() throws IOException {
        ELispLexer lexer = lexer("");
        assertTrue(lexer.hasNext());
        assertEquals(new EOF(), lexer.next().token());
        assertFalse(lexer.hasNext());
        assertThrows(IOException.class, lexer::next);
    }

    @Test
    public void testComment() throws IOException {
        assertEquals(List.of(
                new EOF()
        ), lex(";a"));
        assertEquals(List.of(
                new EOF()
        ), lex("\n;a\n"));
        assertEquals(List.of(
                new EOF()
        ), lex("\n\n;a\n"));
        assertEquals(Arrays.asList(
                new SetLexicalBindingMode(true),
                new EOF()
        ), lex(";; -*- lexical-binding: t -*-"));
        assertEquals(Arrays.asList(
                new SetLexicalBindingMode(true),
                new EOF()
        ), lex("#!/bin/emacs -e\n;; -*- lexical-binding: t -*-"));
        assertEquals(List.of(
                new EOF()
        ), lex("#!/bin/emacs -e\n;;\n;; -*- lexical-binding: t -*-"));
        assertEquals(Arrays.asList(
                new SetLexicalBindingMode(false),
                new EOF()
        ), lex(";; -*- lexical-binding: nil -*-"));
        assertEquals(List.of(
                new EOF()
        ), lex("#!\n#!\n#! effectively a comment"));
    }

    @Test
    public void testHashTokens() throws IOException {
        assertEquals(Arrays.asList(
                new Function(),
                new Symbol("a", true, true),
                new EOF()
        ), lex("#'a"));
        assertEquals(Arrays.asList(
                new Symbol("a", false, true),
                new EOF()
        ), lex("#:a"));
        assertEquals(Arrays.asList(
                new Symbol("", false, true),
                new EOF()
        ), lex("#:"));
        assertEquals(Arrays.asList(
                new Symbol("a", true, false),
                new EOF()
        ), lex("#_a"));
        assertEquals(Arrays.asList(
                new Symbol("", true, true),
                new EOF()
        ), lex("#_"));
        assertEquals(Arrays.asList(
                new ClosureOpen(),
                new Symbol("a", true, true),
                new SquareClose(),
                new EOF()
        ), lex("#[a]"));
        assertEquals(Arrays.asList(
                new BoolVec(10, MuleString.fromString("test")),
                new EOF()
        ), lex("#&10\"test\""));
        assertEquals(Arrays.asList(
                new RecordOpen(),
                new Symbol("a", true, true),
                new ParenClose(),
                new EOF()
        ), lex("#s(a)"));
        assertEquals(Arrays.asList(
                new ParenOpen(),
                new CircularDef(1),
                new Symbol("a", true, true),
                new CircularRef(1),
                new ParenClose(),
                new EOF()
        ), lex("(#1=a #1#)"));
        assertEquals(Arrays.asList(
                new Symbol("", true, true),
                new EOF()
        ), lex("##"));
        assertEquals(Arrays.asList(
                new CharTableOpen(),
                new SquareClose(),
                new EOF()
        ), lex("#^[]"));
        assertEquals(Arrays.asList(
                new SubCharTableOpen(),
                new SquareClose(),
                new EOF()
        ), lex("#^^[]"));
        assertEquals(Arrays.asList(
                new StrWithPropsOpen(),
                new ParenClose(),
                new EOF()
        ), lex("#()"));
        assertEquals(List.of(
                new EOF()
        ), lex("#!"));
        assertEquals(List.of(
                new SkipToEnd()
        ), lex("#@00"));
        assertEquals(Arrays.asList(
                new Symbol("test", true, true),
                new EOF()
        ), lex("#@0skipped\037test"));
        assertEquals(Arrays.asList(
                new Symbol("test", true, true),
                new EOF()
        ), lex("#@07skipped\037test"));
        assertEquals(Arrays.asList(
                ELispLexer.NIL_SYMBOL,
                new EOF()
        ), lex("#$"));
    }

    @Test
    public void testDot() throws IOException {
        for (String s : new String[]{
                ".",
                ". 0",
                ".\u00A01",
                ".\"a\"",
                ".'a",
                ".;1",
                ".(1)",
                ".[1]",
                ".#1=",
                ".?1",
                ".`a",
                ".,1",
        }) {
            assertEquals(new Dot(), lex(s).getFirst());
        }
    }

    private static final Object[] STRING_TESTS = {
            "\"test\"", "test",
            "\"\\n\\ \\\n\"", "\n",
            "\"\\M-1\"", MuleString.fromRaw(new byte[]{(byte) 0xb1}),
            "\"\\70@\\70[\\70`\\70{\"", "8@8[8`8{",
            "\"\\C-[\"", "\u001b",
            "\"\\s\"", " ",
            "\"\\\\`\\376\\377\"", MuleString.fromRaw(new byte[]{'\\', '`', (byte) 0xfe, (byte) 0xff}),
    };

    @Test
    public void testStr() throws IOException {
        for (int i = 0; i < STRING_TESTS.length; i += 2) {
            String input = (String) STRING_TESTS[i];
            Object expected = STRING_TESTS[i + 1];
            MuleString expectedStr = expected instanceof MuleString s ? s : MuleString.fromString(expected.toString());
            assertEquals(Arrays.asList(
                    new Str(expectedStr),
                    new EOF()
            ), lex(input));
        }
    }

    private static final String[] SYMBOL_TESTS = new String[]{
            "a\\ b", "a b",
            "a\u00A0b", "a",
            "中日韩", "中日韩",
            "a\"\"", "a",
            "a'a", "a",
            "a;a", "a",
            "a#1#", "a",
            "a(1)", "a",
            "a[1]", "a",
            "a)1", "a",
            "a]1", "a",
            "a`1", "a",
            "a,1", "a",
            " \r\n\ta", "a",
    };

    @Test
    public void testSymbol() throws IOException {
        for (int i = 0; i < SYMBOL_TESTS.length; i += 2) {
            String expected = SYMBOL_TESTS[i + 1];
            String src = SYMBOL_TESTS[i];
            assertEquals(new Symbol(expected, true, true), lex(src).getFirst(), src);
        }
    }

    @Test
    public void testUnicode() throws IOException {
        assertEquals(Arrays.asList(
                new Symbol("🀄", true, true),
                new EOF()
        ), lex("🀄"));
        assertEquals(Arrays.asList(
                new Char(126980),
                new EOF()
        ), lex("?🀄"));
    }

    @Test
    public void testByteReader() throws IOException {
        ELispLexer lexer = new ELispLexer(
                Source.newBuilder(
                        "elisp",
                        ByteSequence.create(
                                "#@9🀄🀄\03742".getBytes(StandardCharsets.UTF_8)
                        ),
                        "<input>"
                ).build()
        );
        Token data = lexer.next().token();
        assertEquals(42L, ((Token.FixNum) data).value());
    }

    private void assertError(String input, @Nullable String expected) {
        ELispSignals.ELispSignalException err = assertThrows(ELispSignals.ELispSignalException.class, () -> lex(input), input);
        assertEquals(expected, ((ELispCons) err.getData()).car().toString());
    }

    @Test
    public void testUntimelyEOF() {
        for (String input : new String[]{
                "\"",
                "?\\u12",
                "?",
                "?\\",
                "#s",
        }) {
            Throwable err = assertThrows(IOException.class, () -> lex(input), input);
            assertInstanceOf(EOFException.class, err);
        }
    }

    @Test
    public void testErrors() throws IOException {
        assertError("\ud83c\u0000", "Invalid Unicode surrogate pair");
        assertError("\"\\A-1\"", "Invalid modifier in string");

        assertError("'(?\\u00)", "Invalid number");
        assertError("'?\\u00z", "Invalid number");
        assertError("?\\U0FFFFFFF", "Not a valid Unicode code point");
        assertError("?\\U-0FFFFFFF", "Invalid number");
        assertError("?\\C", "Invalid modifier");
        assertError("?\\H", "Invalid modifier");
        assertError("?\\M", "Invalid modifier");
        assertError("?\\S", "Invalid modifier");
        assertError("?\\\n", "Unexpected newline");
        assertError("?aa", "Invalid char");
        assertError("?\\H-aa", "Invalid char");
        assertError("?\\M-aa", "Invalid char");
        assertError("?\\C-aa", "Invalid char");
        assertError("?\\^aa", "Invalid char");
        assertError("?\\nn", "Invalid char");

        assertError("#xfrf", "Invalid number");
        assertError("#40rz", "Invalid base");
        lex("#x1@");
        lex("#x1:");
        lex("#x1[");
        lex("#x1{");
        assertError("#xffz", "Invalid number");
        assertError("#xFFZ", "Invalid number");
        assertError("#o779", "Invalid number");
        assertError("#16rffz", "Invalid number");
        assertError("#z", "Expected a number base indicator");

        assertError("#&0", "Expected '\"'");
        assertError("#s[", "Expected '('");
        assertError("#^__", "Expected '^' or '['");
        assertError("#^^^[]", "Expected '^' or '['");

    }

}
