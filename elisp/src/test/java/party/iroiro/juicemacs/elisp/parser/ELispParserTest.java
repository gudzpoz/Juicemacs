package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.source.Source;
import org.junit.jupiter.api.Test;
import org.mozilla.universalchardet.UniversalDetector;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;

import java.io.*;
import java.math.BigInteger;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CHARTAB_SIZE_BITS_1;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CHAR_TABLE_STANDARD_SLOTS;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isT;

public class ELispParserTest {
    private static final ELispParser.InternContext context = new ELispParser.InternContext() {
        final Map<MuleString, ELispSymbol> symbolMap = new HashMap<>(Map.of(
                MuleString.fromString("t"), T,
                MuleString.fromString("nil"), NIL,
                MuleString.fromString("float"), FLOAT,
                MuleString.fromString("hash-table"), HASH_TABLE,
                MuleString.fromString("data"), DATA,
                MuleString.fromString("key"), KEY
        ));

        @Override
        public ELispSymbol intern(String name) {
            return intern(MuleString.fromString(name));
        }

        @Override
        public ELispSymbol intern(MuleString name) {
            ELispSymbol symbol = symbolMap.get(name);
            if (symbol == null) {
                symbol = new ELispSymbol(name);
                symbolMap.put(name, symbol);
            }
            return symbol;
        }

        @Override
        public MuleString applyShorthands(MuleString symbol) {
            return symbol;
        }
    };

    private Object read(String content) throws IOException {
        return ELispParser.read(
                context,
                Source.newBuilder("elisp", content, null).build()
        );
    }

    private static final Object[] ATOM_TOKEN_TESTS = new Object[] {
            "1", 1L,
            "1.5", 1.5,
            "#xFFFFFFFFFFFFFFFFFFFF", ELispBigNum.wrap(BigInteger.ONE.shiftLeft(80).subtract(BigInteger.ONE)),
            "t", T,
            "nil", NIL,
            "#@00", false,
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
        assertEquals(".", dot.name().toString());

        ELispBoolVector bVec = assertInstanceOf(ELispBoolVector.class, read("#&2\"\\1\""));
        assertEquals(2, bVec.size());
        assertTrue(isT(bVec.get(0)));
        assertTrue(isNil(bVec.get(1)));
    }

    private void assertCons(String expr, String[] symbols) throws IOException {
        ELispCons cons = assertInstanceOf(ELispCons.class, read(expr));
        Object[] array = cons.toArray();
        for (int i = 0; i < array.length; i++) {
            ELispSymbol symbol = assertInstanceOf(ELispSymbol.class, array[i]);
            assertEquals(symbols[i], symbol.name().toString());
        }
    }

    @Test
    public void testCons() throws IOException {
        assertSame(false, read("()"));
        ELispCons cons = assertInstanceOf(ELispCons.class, read("(1024 . 4321)"));
        assertEquals(1024L, cons.car());
        assertEquals(4321L, cons.cdr());
        assertCons("'quoted-symbol", new String[]{"quote", "quoted-symbol"});
        assertCons("#'quoted-function", new String[]{"function", "quoted-function"});
        assertCons("`backquoted-expr", new String[]{"`", "backquoted-expr"});
        assertCons(",unquoted-symbol", new String[]{",", "unquoted-symbol"});
        assertCons(",@unquoted-symbol", new String[]{",@", "unquoted-symbol"});

        assertEquals("`(match ,val \\, upat)", read("`(match ,val . ,upat)").toString());
    }

    @Test
    public void testRecord() throws IOException {
        ELispRecord rec = assertInstanceOf(ELispRecord.class, read("#s(1 321)"));
        assertEquals(1L, assertInstanceOf(Long.class, rec.get(0)));
        assertEquals(321L, assertInstanceOf(Long.class, rec.get(1)));
    }

    @Test
    public void testByteCode() throws IOException {
        ELispBytecode bc = assertInstanceOf(ELispBytecode.class, read("#[() \"\" [] 0]"));
        assertSame(false, bc.getFirst());
    }

    @Test
    public void testLexicalBindingDetect() throws IOException {
        ELispParser parser = new ELispParser(
                context,
                Source.newBuilder(
                        "elisp",
                        ";; -*- lexical-binding: t -*-\nnil",
                        null
                ).build()
        );
        assertSame(false, parser.nextLisp());
        assertTrue(parser.isLexicallyBound());
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
        assertEquals("rec", assertInstanceOf(ELispSymbol.class, rec.getFirst()).name().toString());
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
                assertInstanceOf(ELispSymbol.class, table.get(context.intern("k1"))).name().toString()
        );
        assertEquals(
                "v2",
                assertInstanceOf(ELispSymbol.class, table.get(context.intern("k2"))).name().toString()
        );
        ELispCons placeholder = assertInstanceOf(ELispCons.class, table.get(context.intern("k3")));
         assertSame(NIL, placeholder.car());

        ELispString str = assertInstanceOf(ELispString.class, read("#1=#(\"text here\" 0 1 (key #1#))"));
        assertTrue(str.hasIntervals());
        AtomicInteger propCount = new AtomicInteger();
        str.forRangeProperties(0, (props, _, _) -> {
            propCount.incrementAndGet();
            ELispCons properties = assertInstanceOf(ELispCons.class, props);
            assertSame(KEY, properties.getFirst());
            assertSame(str, properties.get(1));
            return null;
        });
        assertEquals(1, propCount.get());

        assertEquals(
                "(invalid-read-syntax \"Unexpected self reference\")",
                assertThrows(ELispSignals.ELispSignalException.class, () -> read("#1=#1#")).getMessage()
        );
    }

    @Test
    public void testCharTables() throws IOException {
        String charTableString = "#^[" + "t ".repeat(CHAR_TABLE_STANDARD_SLOTS) + "]";
        ELispCharTable table = assertInstanceOf(ELispCharTable.class, read(charTableString));
        assertEquals(CHAR_TABLE_STANDARD_SLOTS, table.slots());
        assertEquals(ELispCharTable.MAX_CHAR_INDEX + 1, table.size());
        table.forEach((ele) -> assertSame(true, ele));
        String subTableString = "#^^[1 1024 " + "t ".repeat(1 << CHARTAB_SIZE_BITS_1) + "]";
        ELispCharTable.SubTable sub = assertInstanceOf(ELispCharTable.SubTable.class, read(subTableString));
        assertEquals(2 + (1 << CHARTAB_SIZE_BITS_1), sub.size());
        assertEquals(1, sub.getDepth());
        assertEquals(1024, sub.getMinChar());
        sub.forEach((ele) -> {
            if (!(ele instanceof Long)) {
                assertSame(true, ele);
            }
        });
    }

    @Test
    public void testEthiopic() throws IOException {
        Path target = Path.of("emacs", "lisp", "language", "ethiopic.el");
        Source ethiopic = Source.newBuilder(
                "elisp",
                new FileReader(target.toFile()),
                target.toFile().getName()
        ).build();
        ELispParser parser = new ELispParser(context, ethiopic);
        ELispSignals.ELispSignalException e = assertThrows(ELispSignals.ELispSignalException.class, () -> {
            while (parser.hasNext()) {
                parser.nextLisp();
            }
        });
        assertEquals("(invalid-read-syntax \"Invalid char\")", e.getMessage());
    }

    @Test
    public void testEncodingDetect() throws IOException {
        for (String file : new String[]{"ethiopic.el", "tibetan.el"}) {
            Path target = Path.of("emacs", "lisp", "language", file);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            try (FileChannel channel = FileChannel.open(target)) {
                channel.transferTo(0, channel.size(), Channels.newChannel(out));
            }
            byte[] bytes = out.toByteArray();
            UniversalDetector detector = new UniversalDetector();
            detector.handleData(bytes);
            detector.dataEnd();
            assertEquals("UTF-8", detector.getDetectedCharset());
        }
    }

    @Test
    public void testLReadTest() throws IOException {
        Path target = Path.of("emacs", "test", "src", "lread-tests.el");
        Source ethiopic = Source.newBuilder(
                "elisp",
                new FileReader(target.toFile()),
                target.toFile().getName()
        ).build();
        ELispParser parser = new ELispParser(context, ethiopic);
        assertDoesNotThrow(() -> {
            while (parser.hasNext()) {
                parser.nextLisp();
            }
        });
    }

    @Test
    public void testErrors() {
        assertNull(assertThrows(EOFException.class, () -> read("")).getMessage());
        assertError("(a . a a)", "Expected ')'");
        assertError(")", "Expected start of expression");
        assertError("]", "Expected start of expression");
        assertError("#&1\"中\"", "Expected raw byte string");
        assertError("#&16\"a\"", "Unmatched bit vector length");
        assertError("#&16\"aaa\"", "Unmatched bit vector length");
    }

    private void assertError(String expr, String message) {
        ELispSignals.ELispSignalException err = assertThrows(ELispSignals.ELispSignalException.class, () -> read(expr));
        assertEquals(message, ((ELispCons) err.getData()).car().toString());
    }

}
