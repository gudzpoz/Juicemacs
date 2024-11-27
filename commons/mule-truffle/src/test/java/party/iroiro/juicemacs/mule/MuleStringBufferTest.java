package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.PrimitiveIterator;

import static org.junit.jupiter.api.Assertions.*;

public class MuleStringBufferTest {
    @Test
    public void appendCodePointTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        buffer
                .appendCodePoint('a')
                .appendCodePoint('b')
                .appendCodePoint('c');
        assertEquals(3, buffer.length());
        assertEquals('c', buffer.codePointAt(2));
        buffer.appendCodePoint('文');
        assertEquals(4, buffer.length());
        buffer.appendCodePoint(0x1f984);
        assertEquals(5, buffer.length());
        String expected = "abc文🦄";
        assertEquals(expected, buffer.toString());

        PrimitiveIterator.OfInt iterator = expected.codePoints().iterator();
        int i = 0;
        while (iterator.hasNext()) {
            assertEquals(iterator.nextInt(), buffer.codePointAt(i++));
        }
        assertThrows(IndexOutOfBoundsException.class, () -> buffer.codePointAt(-1));
        assertThrows(IndexOutOfBoundsException.class, () -> buffer.codePointAt(5));

        buffer.appendCodePoint(Character.MAX_CODE_POINT + 1).appendCodePoint(Character.MAX_CODE_POINT + 2);
        assertEquals(7, buffer.length());
        assertEquals(Character.MAX_CODE_POINT + 1, buffer.codePointAt(5));
        assertEquals(Character.MAX_CODE_POINT + 2, buffer.codePointAt(6));
        assertEquals("abc文🦄\\U00110000\\U00110001", buffer.toString());
    }

    private MuleByteArrayString fromAsciiString(String string) {
        return MuleString.fromLatin1(string.getBytes(StandardCharsets.UTF_8));
    }

    private MuleTruffleString fromJavaString(String string) {
        return new MuleTruffleString(TruffleString.fromJavaStringUncached(string, TruffleString.Encoding.UTF_32));
    }

    @Test
    public void appendStringTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        buffer.appendCodePoint('文');
        buffer.appendMuleString(fromJavaString("文"), 0, 1);
        buffer.appendCodePoint(Character.MAX_CODE_POINT + 1);
        buffer.appendMuleString(fromAsciiString("abc"), 0, 3);
        buffer.appendCodePoint('d')
                .appendMuleString(fromAsciiString("abcdefg"), 0, 1);
        assertEquals("文文\\U00110000abcda", buffer.toString());

        buffer.appendMuleString(buffer.subSequence(1, 3), 0, 2);
        assertEquals("文文\\U00110000abcda文\\U00110000", buffer.toString());

        buffer.appendMuleString(buffer.subSequence(0, 3), 0, 2);
        buffer.appendMuleString(buffer.subSequence(0, 3), 1, 3);
        buffer.appendMuleString(buffer.subSequence(0, 3), 1, 2);
        assertEquals("文文\\U00110000abcda文\\U00110000文文文\\U00110000文", buffer.toString());

        MuleByteArrayString s = fromAsciiString("012345");
        MuleStringBuffer substringAppend = new MuleStringBuffer();
        substringAppend
                .appendMuleString(s, 0, 6)
                .appendCodePoint('文')
                .appendMuleString(s, 1, 5)
                .appendCodePoint('文')
                .appendMuleString(s, 2, 4)
                .appendCodePoint('文')
                .appendMuleString(s, 3, 3)
                .appendCodePoint('文')
                .appendMuleString(s, 0, 3)
                .appendCodePoint('文')
                .appendMuleString(s, 3, 6);
        assertEquals("012345文1234文23文文012文345", substringAppend.toString());
    }

    @Test
    public void appendBufferTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        buffer.appendMuleString(fromAsciiString("abc"), 0, 3);
        buffer.appendCodePoint('文');
        buffer.appendMuleString(buffer, 0, 4);
        assertEquals("abc文abc文", buffer.toString());

        buffer.appendMuleString(fromAsciiString("def"), 0, 3);
        buffer.appendCodePoint('g');
        assertEquals(12, buffer.length());
        buffer.appendMuleString(buffer, 0, 12);
        assertEquals("abc文abc文defgabc文abc文defg", buffer.toString());
        buffer.appendMuleString(buffer, 23, 24);
        assertEquals("abc文abc文defgabc文abc文defgg", buffer.toString());
    }

    @Test
    public void buildTest() {
        assertEquals("abc", new MuleStringBuffer()
                .appendCodePoint('a').appendCodePoint('b').appendCodePoint('c')
                .build().toString());
        assertEquals("abc文", new MuleStringBuffer()
                .appendCodePoint('a').appendCodePoint('b').appendCodePoint('c')
                .appendCodePoint('文')
                .build().toString());

        String s = "abc".repeat(4096);
        assertEquals(s + "abc", new MuleStringBuffer()
                .appendMuleString(fromAsciiString(s), 0, s.length())
                .appendMuleString(fromAsciiString("abc"), 0, 3)
                .build().toString());

        assertEquals("abc🦄", new MuleStringBuffer()
                .appendMuleString(fromAsciiString("abc"), 0, 3)
                .appendMuleString(fromJavaString("🦄"), 0, 1)
                .build().toString());

        assertEquals("abc🦄abc", new MuleStringBuffer()
                .appendMuleString(fromAsciiString("abc"), 0, 3)
                .appendMuleString(fromJavaString("🦄"), 0, 1)
                .appendMuleString(fromAsciiString("abc"), 0, 3)
                .build().toString());
    }

    @Test
    public void codePointsTest() {
        assertArrayEquals(
                new int[]{'a', 'b', 'c', '文'},
                new MuleStringBuffer()
                        .appendCodePoint('a')
                        .appendCodePoint('b')
                        .appendCodePoint('c')
                        .appendCodePoint('文')
                        .codePoints(0).toArray()
        );
        assertArrayEquals(
                new int[]{'文', '文'},
                new MuleStringBuffer()
                        .appendCodePoint('文')
                        .appendCodePoint('文')
                        .codePoints(0).toArray()
        );
    }

    @Test
    public void muleStringTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        buffer.appendCodePoint(Character.MAX_CODE_POINT + 1)
                .appendMuleString(new MuleIntArrayString(new int[]{Character.MAX_CODE_POINT + 2}), 0, 1);
        assertEquals(2, buffer.length());
        assertEquals(Character.MAX_CODE_POINT + 1, buffer.codePointAt(0));
        assertEquals(Character.MAX_CODE_POINT + 2, buffer.codePointAt(1));
        assertEquals("\\U00110000\\U00110001", buffer.toString());
    }

    @Test
    public void avoidHugeCopyTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        String repeat = "abcdefg".repeat(4096);
        buffer.appendMuleString(fromAsciiString(repeat), 0, 7 * 4096);
        buffer.appendMuleString(buffer, 0, 7 * 4096);
        assertEquals(2 * 7 * 4096, buffer.length());
        assertEquals(repeat.repeat(2), buffer.toString());

        assertEquals(
                repeat,
                new MuleStringBuffer()
                        .appendMuleString(fromAsciiString(repeat), 0, repeat.length())
                        .build().toString()
        );
    }
}
