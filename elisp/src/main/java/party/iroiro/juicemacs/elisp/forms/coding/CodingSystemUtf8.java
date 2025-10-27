package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.elisp.forms.coding.EolAwareStringBuilder.EndOfLine;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;

import java.io.IOException;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.UTF_8;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asSym;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

final class CodingSystemUtf8 implements ELispCodingSystemType {
    @Override
    public ELispSymbol codingType() {
        return UTF_8;
    }

    @Override
    public int initExtraAttrs(ELispVector attrs, Object[] args, Object charsetListObject) {
        if (args.length < CODING_ARG_UTF8_MAX) {
            throw ELispCodingSystemType.shortArgs(args.length);
        }
        Object bom = args[CODING_ARG_UTF8_BOM];
        ELispCodingSystemType.checkBom(bom);
        attrs.set(CODING_ATTR_UTF_BOM, bom);
        if (isNil(bom)) {
            attrs.set(CODING_ATTR_ASCII_COMPAT, true);
        }
        if (bom instanceof ELispCons) {
            return CODING_CATEGORY_UTF_8_AUTO;
        } else if (isNil(bom)) {
            return CODING_CATEGORY_UTF_8_NOSIG;
        } else {
            return CODING_CATEGORY_UTF_8_SIG;
        }
    }

    @Override
    public ELispCodingSystem create(ELispCodingSystem.Spec spec, EolAwareStringBuilder.EndOfLine eol) {
        Object bom = spec.utfBom();
        if (bom instanceof ELispCons cons) {
            return new BomDetectingCodingSystem(this, spec, cons);
        }
        return new Utf8Codec(this, spec, eol);
    }

    public static final class BomDetectingCodingSystem extends ELispCodingSystem {
        private final ELispSymbol withSig;
        private final ELispSymbol noSig;

        BomDetectingCodingSystem(CodingSystemUtf8 system, Spec spec, ELispCons bom) {
            super(system, spec, EolAwareStringBuilder.EndOfLine.LF);
            withSig = asSym(bom.car());
            noSig = asSym(bom.cdr());
        }

        @Override
        MuleStringBuilder decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException {
            if (input.hasNext() && input.next() == (byte) UTF_8_BOM_1) {
                if (input.hasNext() && input.next() == (byte) UTF_8_BOM_2) {
                    if (input.hasNext() && input.next() == (byte) UTF_8_BOM_3) {
                        throw new OtherCodingDetectedException(withSig);
                    }
                }
            }
            throw new OtherCodingDetectedException(noSig);
        }
    }

    public static final class Utf8Codec extends ELispCodingSystem {
        Utf8Codec(CodingSystemUtf8 system, Spec spec, EndOfLine eol) {
            super(system, spec, eol);
        }

        @Override
        MuleStringBuilder decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException {
            // TODO: BOM, etc.
            EolAwareStringBuilder output = newStringBuilder();
            byte[] nonAsciiSequence = new byte[5];
            next:
            while (input.hasNext()) {
                byte c = input.next();
                if (c >= 0) {
                    output.appendCodePoint(c);
                    continue;
                }

                final int trailing = Integer.numberOfLeadingZeros(~c) - 24 - 1;
                if (1 <= trailing && trailing <= 4) {
                    nonAsciiSequence[0] = c;
                    for (int i = 0; i < trailing; i++) {
                        int octet = input.hasNext() ? Byte.toUnsignedInt(input.next()) : -1;
                        if (octet == -1 || invalidExtraOctet((byte) octet)) {
                            output.appendRawByte(c);
                            for (int j = 0; j < i; j++) {
                                output.appendRawByte(nonAsciiSequence[j]);
                            }
                            if (octet != -1) { output.appendRawByte((byte) octet);
                            }
                            continue next;
                        }
                        nonAsciiSequence[i + 1] = (byte) octet;
                    }
                    appendCodePoint(output, nonAsciiSequence, trailing);
                } else {
                    output.appendRawByte(c);
                }
            }
            return output.build();
        }

        private boolean invalidExtraOctet(byte octet) {
            return (octet & (byte) 0b11000000) != (byte) 0b10000000;
        }

        private void appendCodePoint(EolAwareStringBuilder output, byte[] bytes, int trailing) {
            switch (trailing) {
                case 1 -> {
                    int codePoint = ((bytes[0] & 0b00011111) << 6) | (bytes[1] & 0b111111);
                    if (codePoint < 0x80) {
                        output.appendRawByte((byte) (codePoint | 0x80));
                    } else {
                        output.appendCodePoint(codePoint);
                    }
                }
                case 2 -> output.appendCodePoint(
                        ((bytes[0] & 0b00001111) << 12)
                                | ((bytes[1] & 0b00111111) << 6)
                                | (bytes[2] & 0b00111111)
                );
                case 3 -> output.appendCodePoint(
                        ((bytes[0] & 0b00000111) << 18)
                                | ((bytes[1] & 0b00111111) << 12)
                                | ((bytes[2] & 0b00111111) << 6)
                                | (bytes[3] & 0b00111111)
                );
                case 4 -> {
                    int codePoint = ((bytes[0] & 0b00000011) << 24)
                            | ((bytes[1] & 0b00111111) << 18)
                            | ((bytes[2] & 0b00111111) << 12)
                            | ((bytes[3] & 0b00111111) << 6)
                            | (bytes[4] & 0b00111111);
                    if (codePoint > MAX_CHAR) {
                        for (byte b : bytes) {
                            output.appendRawByte(b);
                        }
                    } else {
                        output.appendCodePoint(codePoint);
                    }
                }
            }
        }
    }
}
