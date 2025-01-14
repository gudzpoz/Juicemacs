package party.iroiro.juicemacs.elisp.forms.coding;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.util.NoSuchElementException;

class ByteIterator {
    private final ByteBuffer buffer;
    final SeekableByteChannel input;
    final long start;
    final long end;
    private long position;

    public ByteIterator(SeekableByteChannel input, long start, long end) {
        this.input = input;
        this.start = start;
        this.end = end;
        position = start;
        buffer = ByteBuffer.allocate(4096).limit(0);
    }

    public long inputBytes() {
        return end - start;
    }

    public void skip(long n) {
        position += n - buffer.remaining();
        buffer.position(0).limit(0);
    }

    public void skipToEnd() throws IOException {
        position = end;
        buffer.position(0).limit(0);
    }

    public byte next() throws IOException {
        if (hasNext()) {
            return buffer.get();
        }
        throw new NoSuchElementException();
    }

    public boolean hasNext() throws IOException {
        if (buffer.hasRemaining()) {
            return true;
        }
        buffer.clear();
        buffer.limit(Math.clamp(end - position, 0, buffer.capacity()));
        int inc = input.position(position).read(buffer.clear());
        buffer.flip();
        if (inc > 0) {
            position += inc;
        }
        return buffer.hasRemaining();
    }

    public byte peek() throws IOException {
        if (hasNext()) {
            return buffer.get(buffer.position());
        }
        throw new NoSuchElementException();
    }

    public void reset(long i) {
        position = start + i;
        buffer.position(0).limit(0);
    }
}
