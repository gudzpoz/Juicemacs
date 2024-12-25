package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

import static com.oracle.truffle.api.strings.TruffleString.Encoding.UTF_32;
import static org.junit.jupiter.api.Assertions.*;

public class MuleStringTest {
    @Test
    public void latin1SimpleTest() {
        MuleByteArrayString string = MuleString.fromLatin1(new byte[] { 'a', (byte) 128, 'c' });
        assertEquals(3, string.length());
        assertEquals("a\u0080c", string.toString());
        assertEquals('a', string.codePointAt(0));
        assertEquals(128, string.codePointAt(1));
        assertEquals('c', string.codePointAt(2));
        assertThrows(IndexOutOfBoundsException.class, () -> string.codePointAt(-1));
        assertThrows(IndexOutOfBoundsException.class, () -> string.codePointAt(3));
        assertEquals("a", string.subSequence(0, 1).toString());
        assertEquals("c", string.subSequence(2, 3).toString());
        assertEquals("\u0080", string.subSequence(1, 2).toString());
        assertEquals("a\u0080c", string.subSequence(0, 3).toString());
        assertThrows(IndexOutOfBoundsException.class, () -> string.subSequence(0, 4).toString());
        assertThrows(NegativeArraySizeException.class, () -> string.subSequence(2, 1).toString());
        assertArrayEquals(new byte[]{ 'a', (byte) 128, 'c' }, string.bytes());
    }

    @Test
    public void truffleSimpleTest() {
        String s = "1文🦄";
        MuleTruffleString string = new MuleTruffleString(TruffleString.fromJavaStringUncached(s, UTF_32));
        assertEquals(3, string.length());
        assertEquals(s, string.toString());
        assertEquals('1', string.codePointAt(0));
        assertEquals('文', string.codePointAt(1));
        assertEquals(0x0001F984, string.codePointAt(2));
        assertThrows(IndexOutOfBoundsException.class, () -> string.codePointAt(-1));
        assertThrows(IndexOutOfBoundsException.class, () -> string.codePointAt(3));
        assertEquals("1", string.subSequence(0, 1).toString());
        assertEquals("🦄", string.subSequence(2, 3).toString());
        assertEquals("文", string.subSequence(1, 2).toString());
        assertEquals(s, string.subSequence(0, 3).toString());
        assertThrows(IndexOutOfBoundsException.class, () -> string.subSequence(0, 4).toString());
        assertThrows(IndexOutOfBoundsException.class, () -> string.subSequence(2, 1).toString());
        assertEquals(s, string.truffleString().toString());
    }

    @Test
    public void intArraySimpleTest() {
        int[] array = {'a', '文', Character.MAX_CODE_POINT + 1};
        MuleIntArrayString string = new MuleIntArrayString(array);
        assertEquals(3, string.length());
        assertEquals("a文\\U00110000", string.toString());
        assertEquals('a', string.codePointAt(0));
        assertEquals('文', string.codePointAt(1));
        assertEquals(Character.MAX_CODE_POINT + 1, string.codePointAt(2));
        assertThrows(IndexOutOfBoundsException.class, () -> string.codePointAt(-1));
        assertThrows(IndexOutOfBoundsException.class, () -> string.codePointAt(3));
        assertEquals("a", string.subSequence(0, 1).toString());
        assertEquals("文", string.subSequence(1, 2).toString());
        assertThrows(IndexOutOfBoundsException.class, () -> string.subSequence(0, 4).toString());
        assertThrows(NegativeArraySizeException.class, () -> string.subSequence(2, 1).toString());
        assertSame(array, string.intArray());
    }

    @Test
    public void codePointsTest() {
        MuleByteArrayString string1 = MuleString.fromLatin1(new byte[] { 'a', 'b', 'c' });
        MuleTruffleString string2 = new MuleTruffleString(TruffleString.fromJavaStringUncached("abc", UTF_32));
        MuleIntArrayString string3 = new MuleIntArrayString(new int[]{ 'a', 'b', 'c' });
        for (MuleString s : new MuleString[] {string1, string2, string3}) {
            assertArrayEquals(new int[]{ 'a', 'b', 'c' }, s.codePoints(0).toArray());
            assertArrayEquals(new int[]{ 'c' }, s.codePoints(2).toArray());
            assertThrows(NoSuchElementException.class, () -> s.iterator(3).nextInt());
        }
    }

    @Test
    public void byteLengthTest() {
        for (int i = 'b'; i <= Character.MAX_CODE_POINT; i += 256) {
            int length = MuleString.codePointByteLength(i);
            if (Character.isHighSurrogate((char) i) || Character.isLowSurrogate((char) i)) {
                continue;
            }
            assertEquals(
                    new StringBuilder().appendCodePoint(i).toString().getBytes(StandardCharsets.UTF_8).length,
                    length,
                    "Failed for " + Integer.toHexString(i)
            );
        }
    }
}
