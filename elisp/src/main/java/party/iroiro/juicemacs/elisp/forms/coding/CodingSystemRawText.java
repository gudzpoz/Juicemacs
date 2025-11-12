package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.TruffleUtils;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString.Builder;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_ATTR_ASCII_COMPAT;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_RAW_TEXT;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.RAW_TEXT;

public final class CodingSystemRawText implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return RAW_TEXT;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        return CODING_CATEGORY_RAW_TEXT;
    }

    @Override
    public ELispCodingSystem create(ELispCodingSystem.Spec spec, EolAwareStringBuilder.EndOfLine eol) {
        return new RawCoding(this, spec, eol);
    }

    public static final class RawCoding extends ELispCodingSystem {
        private RawCoding(CodingSystemRawText system, Spec spec, EolAwareStringBuilder.EndOfLine eol) {
            super(system, spec, eol);
        }

        private int fillTilLimit(ByteBuffer buffer, ReadableByteChannel input, long limit) throws IOException {
            TruffleUtils.bufLimit(buffer, buffer.position() + (int) Math.min(buffer.remaining(), limit));
            return input.read(buffer);
        }

        @Override
        Builder decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException {
            ELispString.Builder output = new ELispString.Builder();
            ByteBuffer buffer = ByteBuffer.allocate(4096);
            long limit = input.end - input.start;
            input.input.position(input.start);
            while (limit > 0) {
                buffer.clear();
                int c = fillTilLimit(buffer, input.input, limit);
                buffer.flip();
                output.append(StringSupport.fromRaw(buffer));
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
