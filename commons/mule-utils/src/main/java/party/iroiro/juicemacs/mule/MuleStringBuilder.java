package party.iroiro.juicemacs.mule;

import static party.iroiro.juicemacs.mule.CodingUtils.*;

/// A string builder for utf-8-emacs
public class MuleStringBuilder extends ByteArrayBuilder {
    protected int state = STATE_ASCII;
    protected int codepoints = 0;

    /// Appends a raw byte character
    ///
    /// State transitions:
    /// ```
    /// ASCII → BYTES
    ///
    /// UTF-8 -> EMACS
    /// ```
    public MuleStringBuilder appendRawByte(byte b) {
        codepoints++;
        if (state == STATE_ASCII) {
            state = b >= 0 ? STATE_ASCII : STATE_BYTES;
            writeByte(b);
        } else if (state == STATE_BYTES) {
            writeByte(b);
        } else {
            state = b >= 0 ? state : STATE_EMACS;
            writeRawByte(b, this);
        }
        return this;
    }

    /// Appends a character like [#appendCodePoint(int)], but use [#appendRawByte(byte)]
    /// if possible
    ///
    /// This function is for lisp readers. For example, `"\x3FFFFF"` actually yields an unibyte
    /// string, while `(string #x3FFFFF)` yields a multibyte string. So, lisp readers should
    /// use this method instead.
    public MuleStringBuilder appendCodePointOrRaw(int codePoint) {
        if (codePoint >= 0x3FFF80) {
            return appendRawByte((byte) codePoint);
        }
        return appendCodePoint(codePoint);
    }

    /// Appends a char
    ///
    /// State transitions:
    /// ```
    /// ASCII → UTF-8 → EMACS ← BYTES
    /// ```
    public MuleStringBuilder appendCodePoint(int codePoint) {
        codepoints++;
        if (codePoint < 0x80) {
            writeByte((byte) codePoint);
            return this;
        }
        if (state == STATE_BYTES) {
            convertToMultibyte();
        }
        state = state | (codePoint > Character.MAX_CODE_POINT ? STATE_EMACS : STATE_UTF_8);
        writeCodepoint(codePoint, this);
        return this;
    }

    protected void convertToMultibyte() {
        assert state == STATE_BYTES;

        byte[] original = this.bytes;
        int len = this.usedBytes;
        this.bytes = new byte[usedBytes << 1];
        this.usedBytes = 0;
        for (int i = 0; i < len; i++) {
            writeRawByte(original[i], this);
        }
        state = STATE_EMACS;
    }

    public int getState() {
        return state;
    }

    public int getCodepoints() {
        return codepoints;
    }
}
