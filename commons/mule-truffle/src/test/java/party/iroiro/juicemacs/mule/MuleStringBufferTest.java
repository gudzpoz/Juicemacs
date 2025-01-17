package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;
import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.PrimitiveIterator;

import static org.junit.jupiter.api.Assertions.*;

public class MuleStringBufferTest {
    @Test
    public void directCommitTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        buffer.appendCodePoint('a');
        MuleByteArrayString s = MuleString.fromRaw(ByteBuffer.allocate(10000));
        buffer.append(s);
        assertEquals(10001, buffer.length());
        assertEquals('a', buffer.codePointAt(0));
        for (int i = 0; i < 10000; i++) {
            assertEquals(0, buffer.codePointAt(i + 1));
        }
    }

    @Test
    public void hugeStringTest() {
        int singleStringLength = 1 << 16;
        int repeat = 1 << 16;
        long length = repeat * (long) singleStringLength;
        //noinspection ConstantValue
        assertTrue(length > Integer.MAX_VALUE);

        byte[] array = new byte[singleStringLength];
        Arrays.fill(array, (byte) 'a');

        MuleByteArrayString template = MuleString.fromLatin1(array);
        MuleStringBuffer buffer = new MuleStringBuffer();
        for (int i = 0; i < repeat; i++) {
            buffer.appendMuleString(template, 0, template.length());
        }
        assertEquals(length, buffer.length());
    }

    @Test
    public void appendRawByteTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        buffer.appendRawByte((byte) 'a')
                .appendRawByte((byte) 0xFF);
        MuleByteArrayString build = assertInstanceOf(MuleByteArrayString.class, buffer.build());
        assertEquals("a\\377", build.toString());

        assertEquals(0x3FFFFF, build.codePointAt(1));
        assertEquals(0x3FFFFF, buffer.codePointAt(1));

        buffer.appendCodePoint(0xFF);
        assertEquals("a\\377\377", buffer.toString());
        assertEquals("a\\377\377", buffer.build().toString());

        assertEquals(5, buffer.indexToByteOffset(3));
        assertEquals(5, buffer.build().indexToByteOffset(3));
    }

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

    @Test
    public void substringTest() {
        MuleStringBuffer buffer = new MuleStringBuffer();
        for (int i = 0; i < 100; i++) {
            buffer.appendCodePoint('a');
        }
        assertEquals("aaa", buffer.substring(3, 6).toString());
    }
}
