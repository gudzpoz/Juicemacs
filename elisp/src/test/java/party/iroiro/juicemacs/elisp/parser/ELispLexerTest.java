package party.iroiro.juicemacs.elisp.parser;

import java.io.IOException;
import java.io.StringReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.oracle.truffle.api.source.Source;

import party.iroiro.juicemacs.elisp.parser.ELispLexer.NumberVariant;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.BackQuote;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.BoolVec;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.ByteCodeOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Char;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.CharTableOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.CircularDef;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.CircularRef;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Dot;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.EOF;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Function;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Num;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.ParenClose;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.ParenOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Quote;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.RecordOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.SetLexicalBindingMode;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.SkipToEnd;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.SquareClose;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.SquareOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Str;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.StrWithPropsOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.SubCharTableOpen;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Symbol;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.Unquote;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.UnquoteSplicing;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import static org.junit.jupiter.api.Assertions.*;

public class ELispLexerTest {

    private ELispLexer lexer(String s) throws IOException {
        return new ELispLexer(Source.newBuilder("elisp", new StringReader(s), "test").build());
    }

    private List<TokenData> lex(String s) throws IOException {
        ELispLexer lexer = lexer(s);
        List<TokenData> tokens = new ArrayList<>();
        while (lexer.hasNext()) {
            tokens.add(lexer.next().data());
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
            "#o54", 44,
            "#x2c", 44,
            "#24r1k", 44,
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
                NumberVariant expectedNum;
                if (expected instanceof BigInteger big) {
                    expectedNum = new NumberVariant.BigNum(big);
                } else {
                    expectedNum = new NumberVariant.FixNum((Integer) expected);
                }
                assertEquals(
                        new Num(expectedNum),
                        lexer.next().data()
                );
                assertTrue(lexer.hasNext());
                if (suffix.isEmpty() || suffix.equals(" ")) {
                    assertEquals(new EOF(), lexer.next().data());
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
            Num actual = (Num) lexer.next().data();
            if (Double.isNaN(expected)) {
                assertTrue(
                        Double.isNaN(((NumberVariant.Float) actual.value()).value()),
                        (String) FLOAT_TESTS[i]
                );
            } else {
                assertEquals(
                        new Num(new NumberVariant.Float(expected)),
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
            "?\\^", -1, // Emacs behavior
            "?\\^中", 67128877,
            "?\\^I", 9,
            "?\\^i", 9,
            "?\\C-J", 10,
            "?\\C-j", 10,
            "?\\C", (int) 'C',
            "?\\^@", 0,
            "?\\^1", 67108913,
            "?\\^?", 127,
            "?\\C-?", 127,
            "?\\M-\\C-b", 134217730,
            "?\\M-\\002", 134217730,
            "?\\S-\\002", 33554434,
            "?\\H-\\M-\\A-x", 155189368,
    };

    @Test
    public void testCharToken() throws IOException {
        for (int i = 0; i < CHAR_TESTS.length; i += 2) {
            String s = (String) CHAR_TESTS[i];
            int c = (int) CHAR_TESTS[i + 1];
            ELispLexer lexer = lexer(s);
            Char actual = (Char) lexer.next().data();
            assertEquals(c, actual.value(), s);
        }
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
        assertEquals(new EOF(), lexer.next().data());
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
                new ByteCodeOpen(),
                new Symbol("a", true, true),
                new SquareClose(),
                new EOF()
        ), lex("#[a]"));
        assertEquals(Arrays.asList(
                new BoolVec(10, ELispString.from("test")),
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
                ".#1",
                ".?1",
                ".`a",
                ".,1",
        }) {
            assertEquals(new Dot(), lex(s).getFirst());
        }
    }

    @Test
    public void testStr() throws IOException {
        assertEquals(Arrays.asList(
                new Str(ELispString.from("test")),
                new EOF()
        ), lex("\"test\""));
        assertEquals(Arrays.asList(
                new Str(ELispString.from("\n")),
                new EOF()
        ), lex("\"\\n\\ \\\n\""));
        assertEquals(Arrays.asList(
                new Str(ELispString.from("±")),
                new EOF()
        ), lex("\"\\M-1\""));
        assertEquals(Arrays.asList(
                new Str(ELispString.from("8@8[8`8{")),
                new EOF()
        ), lex("\"\\70@\\70[\\70`\\70{\""));
    }

    private static final String[] SYMBOL_TESTS = new String[]{
            "a\\ b", "a b",
            "a\u00A0b", "a",
            "中日韩", "中日韩",
            "a\"\"", "a",
            "a'a", "a",
            "a;a", "a",
            "a#1", "a",
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
            assertEquals(new Symbol(expected, true, true), lex(SYMBOL_TESTS[i]).getFirst());
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

    private void assertError(String input, String expected) {
        Throwable err = assertThrows(IOException.class, () -> lex(input), input);
        assertEquals(expected, err.getMessage());
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
            assertError(input, "Unexpected EOF");
        }
    }

    @Test
    public void testErrors() throws IOException {
        assertError("\ud83c\u0000", "Invalid Unicode surrogate pair");
        assertError("\"\\A-1\"", "Invalid modifier in string");

        assertError("'(?\\u00)", "Expecting fixed number of digits");
        assertError("'?\\u00z", "Expecting fixed number of digits");
        assertError("?\\U0FFFFFFF", "Not a valid Unicode code point");

        assertError("#xfrf", "Invalid base");
        assertError("#40rz", "Invalid base");
        lex("#x1@");
        lex("#x1:");
        lex("#x1[");
        lex("#x1{");
        assertError("#xffz", "Invalid character");
        assertError("#xFFZ", "Invalid character");
        assertError("#o779", "Invalid character");
        assertError("#16rffz", "Invalid character");
        assertError("#z", "Expected a number base indicator");

        assertError("#&0", "Expected '\"'");
        assertError("#s[", "Expected '('");
        assertError("#^__", "Expected '^' or '['");
        assertError("#^^^[]", "Expected '^' or '['");

    }

}
