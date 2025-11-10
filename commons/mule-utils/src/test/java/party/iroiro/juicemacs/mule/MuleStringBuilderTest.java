package party.iroiro.juicemacs.mule;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.mule.CodingUtils.*;

@SuppressWarnings("NonAsciiCharacters")
public class MuleStringBuilderTest {
    @Test
    public void testStateTransition() {
        MuleStringBuilder builder = new MuleStringBuilder();
        // a
        builder.appendRawByte((byte) 'a');
        assertEquals(STATE_ASCII, builder.state);
        // a\200
        builder.appendRawByte((byte) 0x80);
        assertEquals(STATE_BYTES, builder.state);
        // a\200\200
        builder.appendRawByte((byte) 0x80);
        assertEquals(STATE_BYTES, builder.state);
        // a\200\200語
        builder.appendCodePoint('語');
        assertEquals(STATE_EMACS, builder.state);
        // a\200\200語\200
        builder.appendRawByte((byte) 0x80);
        assertEquals(STATE_EMACS, builder.state);

        // a\200\200語\200a
        builder.appendRawByte((byte) 'a');
        assertEquals(STATE_EMACS, builder.state);
        // a\200\200語\200a\x3FFF7F
        builder.appendCodePointOrRaw(0x3FFF7F);
        assertEquals(STATE_EMACS, builder.state);
        // a\200\200語\200a\x3FFF7F\200
        builder.appendCodePointOrRaw(0x3FFF80);
        assertEquals(STATE_EMACS, builder.state);

        byte[] 語 = "語".getBytes(StandardCharsets.UTF_8);
        assertEquals(3, 語.length);
        byte[] FF7F = new byte[5];
        assertEquals(5, writeCodepoint(0x3FFF7F, FF7F, 0));
        byte[] FF80 = new byte[2];
        assertEquals(2, writeCodepoint(0x3FFF80, FF80, 0));

        assertArrayEquals(new byte[]{
                'a',
                FF80[0], FF80[1],
                FF80[0], FF80[1],
                語[0],
                語[1],
                語[2],
                FF80[0], FF80[1],
                'a',
                FF7F[0],
                FF7F[1],
                FF7F[2],
                FF7F[3],
                FF7F[4],
                FF80[0], FF80[1],
        }, builder.toByteArray());
    }

    @Test
    public void testCodePointOrRaw() {
        assertArrayEquals(
                new byte[]{'a', 'b', 'c'},
                new MuleStringBuilder()
                        .appendCodePointOrRaw('a')
                        .appendCodePointOrRaw('b')
                        .appendCodePointOrRaw('c')
                        .toByteArray()
        );
        assertArrayEquals(
                new byte[]{'a', 'b', 'c', (byte) 0x80},
                new MuleStringBuilder()
                        .appendCodePointOrRaw('a')
                        .appendCodePointOrRaw('b')
                        .appendCodePointOrRaw('c')
                        .appendCodePointOrRaw(0x3FFF80)
                        .toByteArray()
        );

        byte[] FF7F = new byte[5];
        assertEquals(5, writeCodepoint(0x3FFF7F, FF7F, 0));
        byte[] FF80 = new byte[2];
        assertEquals(2, writeCodepoint(0x3FFF80, FF80, 0));
        assertArrayEquals(
                new byte[]{
                        'a',
                        FF80[0], FF80[1],
                        FF7F[0], FF7F[1], FF7F[2], FF7F[3], FF7F[4],
                },
                new MuleStringBuilder()
                        .appendCodePointOrRaw('a')
                        .appendCodePointOrRaw(0x3FFF80)
                        .appendCodePointOrRaw(0x3FFF7F)
                        .toByteArray()
        );
    }
}
