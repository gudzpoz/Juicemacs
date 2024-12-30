package party.iroiro.juicemacs.elisp.forms.coding;

import party.iroiro.juicemacs.mule.MuleStringBuffer;

public final class EolAwareStringBuilder {
    private final MuleStringBuffer buffer = new MuleStringBuffer();
    private final int eol;
    private boolean crExpectLf = false;

    public EolAwareStringBuilder(EndOfLine eol) {
        this.eol = eol.c;
    }

    public void appendRawByte(byte rawByte) {
        buffer.appendRawByte(rawByte);
    }

    public EolAwareStringBuilder appendCodePoint(int codePoint) {
        if (crExpectLf) {
            crExpectLf = false;
            if (codePoint == '\n') {
                buffer.appendCodePoint('\n');
                return this;
            }
            buffer.appendRawByte((byte) '\r');
        }

        switch (codePoint) {
            case '\r' -> {
                switch (eol) {
                    case '\n' -> buffer.appendRawByte((byte) '\r');
                    case -1 -> crExpectLf = true;
                    default -> buffer.appendCodePoint('\n');
                }
            }
            case '\n' -> {
                switch (eol) {
                    case '\r', -1 -> buffer.appendRawByte((byte) '\n');
                    default -> buffer.appendCodePoint('\n');
                }
            }
            default -> buffer.appendCodePoint(codePoint);
        }
        return this;
    }

    public MuleStringBuffer build() {
        if (crExpectLf) {
            buffer.appendRawByte((byte) '\r');
            crExpectLf = false;
        }
        return buffer;
    }

    public enum EndOfLine {
        LF('\n'),
        CR('\r'),
        CR_LF(-1);

        private final int c;
        EndOfLine(int c) {
            this.c = c;
        }
    }
}
