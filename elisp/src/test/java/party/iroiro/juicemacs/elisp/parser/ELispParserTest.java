package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.source.Source;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.io.IOException;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public class ELispParserTest {

    private final static ELispContext context = new ELispContext();

    @BeforeAll
    public static void setup() {
        context.initGlobal(null);
    }

    private Object read(String content) throws IOException {
        context.initGlobal(null);
        return ELispParser.read(
                Source.newBuilder("elisp", content, null).build()
        );
    }

    private static final Object[] ATOM_TOKEN_TESTS = new Object[] {
            "1", 1L,
            "1.5", 1.5,
            "#xFFFFFFFFFFFFFFFFFFFF", ELispBigNum.wrap(BigInteger.ONE.shiftLeft(80).subtract(BigInteger.ONE)),
            "t", T,
            "nil", NIL,
            "#@00", NIL,
            "?a", (long) 'a',
    };

    @Test
    public void testPrimitives() throws IOException {
        for (int i = 0; i < ATOM_TOKEN_TESTS.length; i += 2) {
            String expr = (String) ATOM_TOKEN_TESTS[i];
            Object expected = ATOM_TOKEN_TESTS[i + 1];
            assertEquals(expected, read(expr));
        }
    }

    @Test
    public void testWrapperTypes() throws IOException {
        ELispString s = assertInstanceOf(ELispString.class, read("\"string abcdefg\""));
        assertEquals("string abcdefg", s.toString());

        ELispSymbol interned = assertInstanceOf(ELispSymbol.class, read("float"));
        assertSame(FLOAT, interned);
        ELispSymbol uninterned = assertInstanceOf(ELispSymbol.class, read("#:float"));
        assertNotSame(FLOAT, uninterned);
        assertEquals(FLOAT.name(), uninterned.name());
        ELispSymbol noShorthand = assertInstanceOf(ELispSymbol.class, read("#_float"));
        assertSame(FLOAT, noShorthand);

        ELispSymbol dot = assertInstanceOf(ELispSymbol.class, read("."));
        assertEquals(".", dot.name());

        ELispBoolVector bVec = assertInstanceOf(ELispBoolVector.class, read("#&2\"\\1\""));
        assertEquals(2, bVec.size());
        assertTrue(ELispSymbol.isT(bVec.get(0)));
        assertTrue(ELispSymbol.isNil(bVec.get(1)));
    }

    private void assertCons(String expr, String[] symbols) throws IOException {
        ELispCons cons = assertInstanceOf(ELispCons.class, read(expr));
        Object[] array = cons.toArray();
        for (int i = 0; i < array.length; i++) {
            ELispSymbol symbol = assertInstanceOf(ELispSymbol.class, array[i]);
            assertEquals(symbols[i], symbol.name());
        }
    }

    @Test
    public void testCons() throws IOException {
        assertSame(NIL, read("()"));
        ELispCons cons = assertInstanceOf(ELispCons.class, read("(1024 . 4321)"));
        assertEquals(1024L, cons.car());
        assertEquals(4321L, cons.cdr());
        assertCons("'quoted-symbol", new String[]{"quote", "quoted-symbol"});
        assertCons("#'quoted-function", new String[]{"function", "quoted-function"});
        assertCons("`backquoted-expr", new String[]{"`", "backquoted-expr"});
        assertCons(",unquoted-symbol", new String[]{",", "unquoted-symbol"});
        assertCons(",@unquoted-symbol", new String[]{",@", "unquoted-symbol"});
    }

    @Test
    public void testRecord() throws IOException {
        ELispRecord rec = assertInstanceOf(ELispRecord.class, read("#s(1 321)"));
        assertEquals(1L, assertInstanceOf(Long.class, rec.get(0)));
        assertEquals(321L, assertInstanceOf(Long.class, rec.get(1)));
    }

    @Test
    public void testByteCode() throws IOException {
        ELispByteCode bc = assertInstanceOf(ELispByteCode.class, read("#[() \"\" [] 0]"));
        assertSame(NIL, bc.getFirst());
    }

    @Test
    public void testLexicalBindingDetect() throws IOException {
        ELispParser parser = new ELispParser(
                Source.newBuilder(
                        "elisp",
                        ";; -*- lexical-binding: t -*-\nnil",
                        null
                ).build()
        );
        assertSame(NIL, parser.nextLisp());
        assertTrue(parser.getLexicalBinding());
    }

    @Test
    public void testCyclicRefs() throws IOException {
        ELispCons cons = assertInstanceOf(ELispCons.class, read("#1=(#1# 4321 321)"));
        assertSame(cons, cons.car());
        Object[] array = cons.toArray();
        assertEquals(3, array.length);
        assertEquals(4321L, array[1]);
        assertEquals(321L, array[2]);

        ELispVector vec = assertInstanceOf(ELispVector.class, read("#1=[#1# 4321 321]"));
        assertEquals(3, vec.size());
        assertSame(vec, vec.getFirst());
        assertEquals(4321L, vec.get(1));
        assertEquals(321L, vec.get(2));

        ELispRecord rec = assertInstanceOf(ELispRecord.class, read("#1=#s(rec #1#)"));
        assertEquals(2, rec.size());
        assertEquals("rec", assertInstanceOf(ELispSymbol.class, rec.getFirst()).name());
        assertSame(rec, rec.get(1));

        // Emacs does not handle recursive references inside hash tables
        // #1=#s(hash-table data (a #1#)) -> #s(hash-table data (a (nil)))
        ELispHashtable table = assertInstanceOf(
                ELispHashtable.class,
                read("#2=#s(hash-table not-a-field #1=(k1 v1 k2 v2 k3 #2#) data #1#)")
        );
        assertEquals(3, table.size());
        assertEquals(
                "v1",
                assertInstanceOf(ELispSymbol.class, table.get(ELispContext.intern("k1"))).name()
        );
        assertEquals(
                "v2",
                assertInstanceOf(ELispSymbol.class, table.get(ELispContext.intern("k2"))).name()
        );
        ELispCons placeholder = assertInstanceOf(ELispCons.class, table.get(ELispContext.intern("k3")));
         assertSame(NIL, placeholder.car());

        ELispString str = assertInstanceOf(ELispString.class, read("#1=#(\"text here\" 0 1 (key #1#))"));
        assertEquals(1, str.intervals());
        AtomicInteger propCount = new AtomicInteger();
        str.forRangeProperties(0, (props) -> {
            propCount.incrementAndGet();
            ELispCons properties = assertInstanceOf(ELispCons.class, props);
            assertSame(ELispContext.KEY, properties.getFirst());
            assertSame(str, properties.get(1));
        });
        assertEquals(1, propCount.get());

        assertEquals(
                "Unexpected self reference",
                assertThrows(IOException.class, () -> read("#1=#1#")).getMessage()
        );
    }

    @Test
    public void testCharTables() throws IOException {
        String charTableString = "#^[" + "t ".repeat(ELispCharTable.CHARTAB_STANDARD_SLOTS) + "]";
        ELispCharTable table = assertInstanceOf(ELispCharTable.class, read(charTableString));
        assertEquals(ELispCharTable.CHARTAB_STANDARD_SLOTS, table.size());
        table.forEach((ele) -> assertSame(T, ele));
        String subTableString = "#^^[1 1024 " + "t ".repeat(1 << ELispCharTable.CHARTAB_SIZE_BITS_1) + "]";
        ELispCharTable.SubTable sub = assertInstanceOf(ELispCharTable.SubTable.class, read(subTableString));
        assertEquals(2 + (1 << ELispCharTable.CHARTAB_SIZE_BITS_1), sub.size());
        assertEquals(1, sub.getDepth());
        assertEquals(1024, sub.getMinChar());
        sub.forEach((ele) -> {
            if (!(ele instanceof Long)) {
                assertSame(T, ele);
            }
        });
    }

    @Test
    public void testErrors() {
        assertError("", "Unexpected EOF");
        assertError("(a . a a)", "Expected ')'");
        assertError(")", "Expected start of expression");
        assertError("]", "Expected start of expression");
        assertError("#&1\"中\"", "Expected raw byte string");
        assertError("#&16\"a\"", "Unmatched bit vector length");
        assertError("#&16\"aaa\"", "Unmatched bit vector length");
    }

    private void assertError(String expr, String message) {
        assertEquals(
                message,
                assertThrows(IOException.class, () -> read(expr)).getMessage()
        );
    }

}
