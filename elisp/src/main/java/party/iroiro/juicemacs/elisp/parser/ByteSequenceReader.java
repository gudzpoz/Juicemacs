package party.iroiro.juicemacs.elisp.parser;

import org.graalvm.polyglot.io.ByteSequence;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;

/**
 * A reader that reads from a {@link ByteSequence}
 *
 * <p>
 * The API is tailored for Emacs to handle its {@code #@} syntax that skips the coming bytes.
 * </p>
 */
public final class ByteSequenceReader {
    private final ByteSequence bytes;
    private final CharsetDecoder decoder;

    private final static int MIN_BUFFER_SIZE = 32;
    private final ByteBuffer bb = ByteBuffer.allocate(MIN_BUFFER_SIZE);
    private final CharBuffer cb = CharBuffer.allocate(2);

    private int lastActualPosition = 0;
    private int position = 0;
    private boolean eof = false;

    public ByteSequenceReader(ByteSequence bytes, Charset charset) {
        this.bytes = bytes;
        this.decoder = charset.newDecoder();
        bb.limit(0);
    }

    /**
     * Skips the next n <i>bytes</i>
     *
     * @param n the number of bytes to skip
     */
    public void skipBytes(int n, boolean peeked) {
        if (peeked) {
            position = lastActualPosition + n;
        } else {
            position -= bb.remaining();
            position += n;
        }
        bb.limit(0);
    }

    private void copy() {
        bb.compact();
        int end = Math.min(bytes.length(), position + bb.remaining());
        if (end == bytes.length()) {
            eof = true;
        }
        for (; position < end; position++) {
            bb.put(bytes.byteAt(position));
        }
        bb.flip();
    }

    /**
     * Reads a single Unicode codepoint from the input byte sequence
     *
     * @return a Unicode codepoint
     * @throws IOException if unable to read as a Unicode codepoint
     */
    public int read() throws IOException {
        lastActualPosition = position - bb.remaining();
        readChar();
        if (cb.remaining() == 0) {
            return -1;
        }
        if (cb.remaining() == 2) {
            char c1 = cb.get();
            char c2 = cb.get();
            return Character.toCodePoint(c1, c2);
        }
        // Surrogate pairs are handled by CharsetDecoder (cb.remaining() == 2)
        return cb.get();
    }

    /**
     * Reads a single char into the internal buffer
     *
     * @throws IOException if unable to read as a Unicode codepoint
     */
    private void readChar() throws IOException {
        cb.position(0).limit(1);
        do {
            CoderResult cr = decoder.decode(bb, cb, eof);
            if (cr.isUnderflow()) {
                copy();
                if (eof && bb.remaining() == 0) {
                    break;
                }
            } else if (cr.isOverflow()) {
                if (cb.position() == 0) {
                    // Surrogate pairs
                    cb.limit(2);
                }
            } else {
                cr.throwException();
            }
        } while (cb.position() == 0);
        cb.flip();
    }
}
