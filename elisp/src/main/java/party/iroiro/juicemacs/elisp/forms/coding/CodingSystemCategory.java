package party.iroiro.juicemacs.elisp.forms.coding;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.parser.ByteSequenceReader;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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

    public abstract boolean detectCoding(ByteSequenceReader source);
    public abstract MuleStringBuffer decode(InputStream inputStream) throws IOException;
    public abstract void encode(OutputStream outputStream, PrimitiveIterator.OfInt input) throws IOException;

    public record CodingSystem(ELispSymbol name, ELispVector attrs, ELispCons aliases, Object eolType) {
    }

    public static final class RawText extends CodingSystemCategory {

        @Override
        public boolean detectCoding(ByteSequenceReader source) {
            return true;
        }

        @Override
        public MuleStringBuffer decode(InputStream inputStream) throws IOException {
            MuleStringBuffer output = new MuleStringBuffer();
            while (true) {
                int c = inputStream.read();
                if (c == -1) {
                    break;
                }
                output.append((char) inputStream.read());
            }
            return output;
        }

        @Override
        public void encode(OutputStream outputStream, PrimitiveIterator.OfInt input) throws IOException {
            while (input.hasNext()) {
                int c = input.nextInt();
                if (c <= 0xFF) {
                    outputStream.write(c);
                } else {
                    // TODO: Use Emacs internal encoding
                    outputStream.write(c & 0xFF);
                }
            }
        }
    }
}
