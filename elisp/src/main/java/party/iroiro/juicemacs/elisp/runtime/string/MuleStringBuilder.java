package party.iroiro.juicemacs.elisp.runtime.string;

import com.oracle.truffle.api.strings.*;

/// A wrapper around [TruffleStringBuilderUTF32] to support emacs codepoints
///
/// It keeps track of whether raw bytes have been inserted and converts
/// the string to [TruffleString.Encoding#BYTES] encoding if its contents
/// fit.
///
/// The string builder uses uncached nodes and is mainly intended for
/// usage in lexers to produce [TruffleString] instances. If you have
/// already [TruffleString] objects, you should prefer using concat nodes
/// or [TruffleStringBuilderUTF32] directly.
public final class MuleStringBuilder {
    private TruffleStringBuilderUTF32 sb = TruffleStringBuilder.createUTF32();
    private int state = StringSupport.STATE_ASCII;

    public MuleStringBuilder appendRawByte(byte b) {
        if (state == StringSupport.STATE_ASCII) {
            state = b >= 0 ? StringSupport.STATE_ASCII : StringSupport.STATE_BYTES;
            sb.appendCodePointUncached(Byte.toUnsignedInt(b));
        } else if (state == StringSupport.STATE_BYTES) {
            sb.appendCodePointUncached(Byte.toUnsignedInt(b));
        } else {
            if (b >= 0) {
                sb.appendCodePointUncached(b);
            } else {
                state = StringSupport.STATE_EMACS;
                sb.appendStringUncached(TruffleString.fromIntArrayUTF32Uncached(new int[]{0x400000 + b}));
            }
        }
        return this;
    }

    public MuleStringBuilder appendCodePoint(int codePoint) {
        if (codePoint < 0x80) {
            return appendRawByte((byte) codePoint);
        }
        if (0x3FFF80 <= codePoint && codePoint <= 0x3FFFFF) {
            return appendRawByte((byte) codePoint);
        }
        if (state == StringSupport.STATE_ASCII) {
            state = StringSupport.STATE_UTF32;
        } else if (state == StringSupport.STATE_BYTES) {
            convertToMultibyte();
        }
        if (codePoint <= Character.MAX_CODE_POINT) {
            sb.appendCodePointUncached(codePoint);
        } else {
            sb.appendStringUncached(TruffleString.fromIntArrayUTF32Uncached(new int[]{codePoint}));
            state = StringSupport.STATE_EMACS;
        }
        return this;
    }

    private void convertToMultibyte() {
        assert state == StringSupport.STATE_BYTES;
        TruffleString s = sb.toStringUncached();
        sb = TruffleStringBuilder.createUTF32(s.byteLength(TruffleString.Encoding.UTF_32));
        sb.appendStringUncached(StringSupport.toMultibyte(s));
        state = StringSupport.STATE_EMACS;
    }

    public MuleStringBuilder append(TruffleString s, int state) {
        if ((state | this.state) != StringSupport.STATE_EMACS) {
            sb.appendStringUncached(s);
        } else {
            if (this.state == StringSupport.STATE_BYTES) {
                convertToMultibyte();
            } else {
                this.state = StringSupport.STATE_EMACS;
            }
            if (state == StringSupport.STATE_BYTES) {
                s = StringSupport.toMultibyte(s);
            }
            sb.appendStringUncached(s);
        }
        return this;
    }

    public MuleStringBuilder appendString(ELispString base) {
        return append(base.asTruffleStringUncached(), base.state());
    }

    /// Returns the built string
    ///
    /// It is the caller's responsibility to store/check [#getState()],
    /// which will be needed for string operations.
    public TruffleString build() {
        return sb.toStringUncached();
    }

    public int getState() {
        return state;
    }

    public ELispString buildString() {
        return new ELispString(sb.toStringUncached(), state);
    }
}
