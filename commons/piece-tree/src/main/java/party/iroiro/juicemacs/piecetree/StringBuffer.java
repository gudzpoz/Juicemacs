package party.iroiro.juicemacs.piecetree;

import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.PrimitiveIterator;

public final class StringBuffer {
    public final MuleStringBuffer buffer;
    public final LineStartList lineStarts;

    public StringBuffer(MuleString buffer, boolean readonly) {
        this(buffer, PieceTreeBase.createLineStartsFast(buffer, readonly));
    }

    public StringBuffer(MuleString buffer, LineStartList lineStarts) {
        this.buffer = new MuleStringBuffer().append(buffer);
        this.lineStarts = lineStarts;
    }

    public static MuleString trimEol(MuleString string) {
        long length = string.length();
        long end = length;
        if (0 < end && string.charAt(end - 1) == '\n') {
            end--;
        }
        if (0 < end && string.charAt(end - 1) == '\r') {
            end--;
        }
        return end == length ? string : string.substring(0, end);
    }

    public static void appendSubstringTrimEol(MuleStringBuffer sb, MuleStringBuffer buffer,
                                              long start, long end) {
        if (start < end && buffer.charAt(end - 1) == '\n') {
            end--;
        }
        if (start < end && buffer.charAt(end - 1) == '\r') {
            end--;
        }
        sb.append(buffer, start, end);
    }

    public static MuleString replaceAllEol(MuleString string, MuleString newEol) {
        MuleStringBuffer sb = new MuleStringBuffer();
        PrimitiveIterator.OfInt i = string.iterator(0);
        while (i.hasNext()) {
            int c = i.nextInt();
            do {
                if (c == '\r') {
                    sb.append(newEol);
                    if (i.hasNext()) {
                        c = i.nextInt();
                        if (c != '\n') {
                            continue;
                        }
                    }
                } else if (c == '\n') {
                    sb.append(newEol);
                } else {
                    sb.appendCodePoint(c);
                }
                break;
            } while (true);
        }
        return sb;
    }
}
