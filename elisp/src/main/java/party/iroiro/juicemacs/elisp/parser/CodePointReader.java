package party.iroiro.juicemacs.elisp.parser;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;

import java.io.EOFException;
import java.io.IOException;
import java.io.Reader;

public sealed abstract class CodePointReader implements AutoCloseable {
    private int line = 1;
    private int column = 1;
    private int codePointOffset = 0;
    private boolean eof = false;
    private int peekedCodepoint = -1;

    public final int read() throws IOException {
        int c = peek();
        peekedCodepoint = -1;
        if (c == -1) {
            eof = true;
            return -1;
        }
        if (c == '\n') {
            line++;
            column = 1;
        }
        column++;
        codePointOffset++;
        return c;
    }

    public final int peek() throws IOException {
        if (peekedCodepoint != -1) {
            return peekedCodepoint;
        }

        peekedCodepoint = readInternal();
        return peekedCodepoint;
    }

    public final boolean isEof() {
        return eof;
    }

    public final void setEof(boolean eof) {
        this.eof = eof;
    }

    public final int getLine() {
        return line;
    }

    public final int getColumn() {
        return column;
    }

    public final int getCodePointOffset() {
        return codePointOffset;
    }

    public static CodePointReader from(Reader reader) {
        return new ReaderReader(reader);
    }

    public static CodePointReader from(ByteSequenceReader reader) {
        return new ByteReader(reader);
    }

    public static CodePointReader from(ELispBuffer buffer) {
        return new BufferReader(buffer);
    }

    protected static int noEOF(int c) throws IOException {
        if (c == -1) {
            throw new EOFException();
        }
        return c;
    }

    protected abstract int readInternal() throws IOException;

    private static final class ReaderReader extends CodePointReader {
        private final Reader reader;

        public ReaderReader(Reader reader) {
            this.reader = reader;
        }

        @Override
        public int readInternal() throws IOException {
            int c1 = reader.read();
            if (c1 == -1) {
                return -1;
            }
            if (!Character.isHighSurrogate((char) c1)) {
                return c1;
            }
            int c2 = noEOF(reader.read());
            if (!Character.isLowSurrogate((char) c2)) {
                // Emacs seems to read text on a best-effort basis...
                // But we choose to differ here.
                throw ELispSignals.error("Invalid Unicode surrogate pair");
            }
            return Character.toCodePoint((char) c1, (char) c2);
        }

        @Override
        public void close() throws IOException {
            reader.close();
        }
    }

    private static final class ByteReader extends CodePointReader {
        private final ByteSequenceReader reader;

        private ByteReader(ByteSequenceReader reader) {
            this.reader = reader;
        }

        @Override
        protected int readInternal() throws IOException {
            return reader.read();
        }

        @Override
        public void close() {
        }
    }

    private static final class BufferReader extends CodePointReader {
        private final ELispBuffer buffer;
        private long offset;

        private BufferReader(ELispBuffer buffer) {
            this.buffer = buffer;
            this.offset = buffer.pointMin();
        }

        @Override
        protected int readInternal() {
            if (offset >= buffer.pointMax()) {
                return -1;
            }
            return buffer.getChar(offset++);
        }

        @Override
        public void close() {
        }
    }
}
