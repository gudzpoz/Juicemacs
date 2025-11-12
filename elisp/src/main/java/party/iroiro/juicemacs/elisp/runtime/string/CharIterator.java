package party.iroiro.juicemacs.elisp.runtime.string;

import party.iroiro.juicemacs.mule.CodingUtils;
import party.iroiro.juicemacs.mule.Utf8Utils;

import java.util.NoSuchElementException;
import java.util.PrimitiveIterator;

public final class CharIterator implements PrimitiveIterator.OfInt {
    private final byte[] bytes;
    private final boolean unibyte;
    private int codepoints = 0;
    private int index = 0;

    public CharIterator(byte[] bytes, boolean unibyte) {
        this.bytes = bytes;
        this.unibyte = unibyte;
    }

    public CharIterator(byte[] bytes, boolean unibyte, int codepoints, int index) {
        this.bytes = bytes;
        this.unibyte = unibyte;
        this.codepoints = codepoints;
        this.index = index;
    }

    public int codepointOffset() {
        return codepoints;
    }

    public int byteOffset() {
        return index;
    }

    public void skipChars(int skip) {
        index = Utf8Utils.codepointIndexToByteIndex(bytes, unibyte, skip, index);
        codepoints += skip;
    }

    @Override
    public int nextInt() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        codepoints++;
        if (unibyte) {
            return bytes[index++] & 0xFF;
        }
        long mixed = CodingUtils.readCodepointAndByteLength(bytes, index);
        int codepoint = (int) mixed;
        int step = (int) (mixed >> 32);
        index += step;
        return codepoint;
    }

    @Override
    public boolean hasNext() {
        return index < bytes.length;
    }

    public boolean hasPrevious() {
        return index > 0;
    }

    public int previousInt() {
        if (!hasPrevious()) {
            throw new NoSuchElementException();
        }
        codepoints--;
        if (unibyte) {
            return bytes[--index] & 0xFF;
        }
        int b = bytes[--index] & 0xFF;
        if (b < 0x80) {
            return b;
        }
        while (index > 0) {
            // skip continuation bytes
            if ((bytes[index] & 0b1100_0000) != 0b1000_0000) {
                break;
            }
        }
        return (int) CodingUtils.readCodepointAndByteLength(bytes, index);
    }
}
