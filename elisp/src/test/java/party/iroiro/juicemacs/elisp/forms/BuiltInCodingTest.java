package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.forms.BuiltInSearch.FRegexpQuote;
import party.iroiro.juicemacs.elisp.runtime.string.CharIterator;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.TestingUtils.getContextBuilder;
import static party.iroiro.juicemacs.mule.CodingUtils.writeCodepoint;

public class BuiltInCodingTest {
    @Test
    public void testDecodeCoding() {
        try (Context context = getContextBuilder(System.out).option("elisp.dumpFile", "emacs.pdmp").build()) {
            Value s = context.eval("elisp", """
                    (decode-coding-string "\\344\\270\\255" 'utf-8-emacs-unix)
                    """);
            String unicode = "中";
            assertEquals(unicode, s.asString());
        }
    }

    @Test
    public void testStringInternal() {
        ByteArrayOutputStream allCodepoints = new ByteArrayOutputStream();
        for (int c = 0; c <= 0x3FFF7F; c++) {
            byte[] bytes = new byte[5];
            int len = writeCodepoint(c, bytes, 0);
            if (c <= Character.MAX_VALUE && Character.isSurrogate((char) c)) {
                continue;
            }
            if (c <= Character.MAX_CODE_POINT) {
                byte[] expected = new StringBuilder().appendCodePoint(c).toString().getBytes(StandardCharsets.UTF_8);
                assertArrayEquals(expected, Arrays.copyOfRange(bytes, 0, len));
            }
            allCodepoints.write(bytes, 0, len);
        }
        ELispString s = ELispString.ofUtf8(allCodepoints.toByteArray());
        CharIterator iterator = s.iterator(0);
        for (int c = 0; c <= 0x3FFF7F; c++) {
            if (c <= Character.MAX_VALUE && Character.isSurrogate((char) c)) {
                continue;
            }
            assertTrue(iterator.hasNext());
            assertEquals(c, iterator.nextInt());
        }
    }

    @Test
    public void test() {
        String s = "‍↔️";
        assertEquals(3, s.codePoints().count());
        ELispString elisp = ELispString.ofJava(s);
        assertEquals(s, elisp.toString());
        assertEquals(3, elisp.length());
        ELispString quote = FRegexpQuote.regexpQuote(elisp);
        assertTrue(quote.lispEquals(elisp));
    }
}
