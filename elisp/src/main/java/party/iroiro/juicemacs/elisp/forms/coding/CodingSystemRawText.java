package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_ATTR_ASCII_COMPAT;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CODING_CATEGORY_RAW_TEXT;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.RAW_TEXT;

final class CodingSystemRawText implements ELispCodingSystemType {
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
        return new RawCoding(spec, eol);
    }

    private final class RawCoding extends ELispCodingSystem {
        RawCoding(Spec spec, EolAwareStringBuilder.EndOfLine eol) {
            super(CodingSystemRawText.this, spec, eol);
        }

        private int fillTilLimit(ByteBuffer buffer, ReadableByteChannel input, long limit) throws IOException {
            buffer.limit(buffer.position() + (int) Math.min(buffer.remaining(), limit));
            return input.read(buffer);
        }

        @Override
        MuleStringBuffer decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException {
            MuleStringBuffer output = new MuleStringBuffer();
            ByteBuffer buffer = ByteBuffer.allocate(4 * 0x400);
            long limit = input.end - input.start;
            input.input.position(input.start);
            while (limit > 0) {
                buffer.clear();
                int c = fillTilLimit(buffer, input.input, limit);
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
