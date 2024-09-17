package party.iroiro.juicemacs.elisp.parser;

import org.graalvm.polyglot.io.ByteSequence;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

public class ByteSequenceReaderTest {

    private final static Object[] UTF_TESTS = {
            "", -1,
            "😼", 0x1F63C,
            "a", (int) 'a',
            "😼abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz".repeat(10), 0x1F63C,
            "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz".repeat(10), (int) 'a',
    };
    private static final String[] CHARSETS = {
            "UTF-8",
            "UTF-16",
            "UTF-16LE",
            "UTF-16BE",
            "UTF-32",
            "UTF-32LE",
            "UTF-32BE",
    };
    private static final int[] CHARSET_ASCII_SIZES = {
            1,
            2,
            2,
            2,
            4,
            4,
            4,
    };

    @Test
    public void testUTF() throws IOException {
        for (String charset : CHARSETS) {
            for (int i = 0; i < UTF_TESTS.length; i += 2) {
                String s = (String) UTF_TESTS[i];
                ByteSequence sequence = ByteSequence.create(s.getBytes(charset));
                ByteSequenceReader reader = new ByteSequenceReader(sequence, Charset.forName(charset));
                int expected = (int) UTF_TESTS[i + 1];
                assertEquals(expected, reader.read(), "Failed to read " + s + " with " + charset);
            }
        }
    }

    private void assertException(byte[] bytes, String charset, String message) {
        ByteSequence sequence = ByteSequence.create(bytes);
        ByteSequenceReader reader = new ByteSequenceReader(sequence, Charset.forName(charset));
        assertEquals(message, assertThrows(IOException.class, reader::read).getMessage());
    }

    @Test
    public void testExceptions() {
        for (String charset : CHARSETS) {
            assertException(new byte[]{-1}, charset, "Input length = 1");
        }
        byte[] bytes = "😼".getBytes(StandardCharsets.UTF_16);
        assertEquals(6, bytes.length);
        // A single UTF-16 surrogate is not a valid character
        assertException(Arrays.copyOfRange(bytes, 0, 4), StandardCharsets.UTF_16.name(), "Input length = 2");
    }

    @Test
    public void testSkipBytes() throws IOException {
        for (int i = 0; i < CHARSETS.length; i++) {
            String charset = CHARSETS[i];
            int skip = CHARSET_ASCII_SIZES[i];
            for (int j = 0; j < UTF_TESTS.length; j += 2) {
                String str = (String) UTF_TESTS[j];
                int expected = (int) UTF_TESTS[j + 1];
                ByteSequence sequence = ByteSequence.create(
                        ("a".repeat(10) + str).getBytes(Charset.forName(charset))
                );
                ByteSequenceReader reader = new ByteSequenceReader(sequence, Charset.forName(charset));
                assertEquals('a', reader.read());
                reader.skipBytes(9 * skip, false);
                assertEquals(expected, reader.read());
            }
        }
    }

}
