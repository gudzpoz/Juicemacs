package party.iroiro.juicemacs.elisp.forms.coding;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.parser.ByteSequenceReader;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.PrimitiveIterator;

public abstract class CodingSystemCategory {
    @Nullable
    public CodingSystem system;
    public int commonFlags;
    public int maxCharsetId;
    public byte[] safeCharsets;
    public int defaultChar;

    protected CodingSystemCategory() {
        system = null;
        commonFlags = 0;
        maxCharsetId = -1;
        safeCharsets = new byte[0];
        defaultChar = 0;
    }

    public CodingSystemCategory copy(CodingSystemCategory copy) {
        this.system = copy.system;
        this.commonFlags = copy.commonFlags;
        this.maxCharsetId = copy.maxCharsetId;
        this.safeCharsets = copy.safeCharsets;
        this.defaultChar = copy.defaultChar;
        return this;
    }

    protected int fillTilLimit(ByteBuffer buffer, ReadableByteChannel input, long limit) throws IOException {
        buffer.limit(buffer.position() + (int) Math.min(buffer.remaining(), limit));
        return input.read(buffer);
    }

    public abstract boolean detectCoding(ByteSequenceReader source);
    public abstract MuleStringBuffer decode(ReadableByteChannel inputStream, long limit) throws IOException;
    public abstract void encode(WritableByteChannel outputStream, PrimitiveIterator.OfInt input) throws IOException;

    public record CodingSystem(ELispSymbol name, ELispVector attrs, ELispCons aliases, Object eolType) {
    }

    public static final class RawText extends CodingSystemCategory {

        @Override
        public boolean detectCoding(ByteSequenceReader source) {
            return true;
        }

        @Override
        public MuleStringBuffer decode(ReadableByteChannel inputStream, long limit) throws IOException {
            MuleStringBuffer output = new MuleStringBuffer();
            ByteBuffer buffer = ByteBuffer.allocate(4 * 0x400);
            while (limit > 0) {
                buffer.clear();
                int c = fillTilLimit(buffer, inputStream, limit);
                buffer.flip();
                output.append(MuleString.fromRaw(buffer));
                if (c <= 0) {
                    break;
                }
                limit -= c;
            }
            return output;
        }

        private void fillBuffer(ByteBuffer buffer, PrimitiveIterator.OfInt input) {
            while (buffer.hasRemaining() && input.hasNext()) {
                buffer.put((byte) input.nextInt());
                // TODO: Use Emacs internal encoding
            }
        }

        @Override
        public void encode(WritableByteChannel outputStream, PrimitiveIterator.OfInt input) throws IOException {
            ByteBuffer buffer = ByteBuffer.allocate(4 * 1024);
            while (input.hasNext()) {
                fillBuffer(buffer, input);
                buffer.flip();
                outputStream.write(buffer);
                buffer.clear();
            }
        }
    }
}
