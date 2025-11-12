package party.iroiro.juicemacs.elisp.runtime.string;

import party.iroiro.juicemacs.mule.CodingUtils;
import party.iroiro.juicemacs.mule.Utf8Utils;

import java.nio.ByteBuffer;
import java.util.Arrays;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_CHAR;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asRanged;
import static party.iroiro.juicemacs.mule.CodingUtils.*;

public abstract class StringSupport {
    private StringSupport() {
    }

    public static boolean isValidChar(long c) {
        return 0 <= c && c <= MAX_CHAR;
    }

    public static ELispString appendAscii(ELispString base, String... strings) {
        byte[] bytes = base.bytes();
        int extra = 0;
        for (String s : strings) {
            extra += s.length();
        }
        byte[] concat = new byte[bytes.length + extra];
        System.arraycopy(bytes, 0, concat, 0, bytes.length);
        int offset = bytes.length;
        for (String s : strings) {
            for (int i = 0; i < s.length(); i++) {
                concat[offset++] = (byte) s.charAt(i);
            }
        }
        return ELispString.ofKnown(concat, base.length() + extra, base.state());
    }

    public static ELispString prependAscii(ELispString base, String... strings) {
        byte[] bytes = base.bytes();
        int extra = 0;
        for (String s : strings) {
            extra += s.length();
        }
        byte[] concat = new byte[bytes.length + extra];
        int offset = 0;
        for (String s : strings) {
            for (int i = 0; i < s.length(); i++) {
                concat[offset++] = (byte) s.charAt(i);
            }
        }
        System.arraycopy(bytes, 0, concat, offset, bytes.length);
        return ELispString.ofKnown(concat, base.length() + extra, base.state());
    }

    public static int codepointIndexToByteIndex(ELispString base, int index, int byteOffset) {
        return Utf8Utils.codepointIndexToByteIndex(base.bytes(), base.isUnibyte(), index, byteOffset);
    }

    public static int countCodepoints(ELispString base, int byteOffset, int byteEnd) {
        return Utf8Utils.countCodepoints(base.bytes(), base.isUnibyte(), byteOffset, byteEnd);
    }

    public static ELispString substring(ELispString base, long start, long end) {
        int startI = asRanged(start, 0, Integer.MAX_VALUE);
        int endI = asRanged(end, 0, Integer.MAX_VALUE);
        if (base.isUnibyte()) {
            return ELispString.ofKnown(
                    Arrays.copyOfRange(base.bytes(), startI, endI),
                    endI - startI,
                    base.state()
            );
        }
        int startByte = codepointIndexToByteIndex(base, startI, 0);
        int endByte = codepointIndexToByteIndex(base, endI - startI, startByte);
        return ELispString.ofKnown(
                Arrays.copyOfRange(base.bytes(), startByte, endByte),
                endI - startI,
                base.state()
        );
    }

    public static ELispString reverse(ELispString seq) {
        byte[] bytes = seq.bytes();
        byte[] revCopy = new byte[bytes.length];
        if (seq.isUnibyte()) {
            for (int i = 0; i < bytes.length; i++) {
                revCopy[i] = bytes[bytes.length - i - 1];
            }
            return ELispString.ofKnown(revCopy, revCopy.length, seq.state());
        }
        int copyFrom = 0;
        int copyTo = bytes.length;
        while (copyFrom < bytes.length) {
            int step = (int) (readCodepointAndByteLength(bytes, copyFrom) >> 32);
            copyTo -= step;
            System.arraycopy(bytes, copyFrom, revCopy, copyTo, step);
            copyFrom += step;
        }
        return ELispString.ofKnown(revCopy, seq.length(), seq.state());
    }

    public static boolean isCompatible(ELispString needle, ELispString haystack) {
        int state1 = needle.state();
        int state2 = haystack.state();
        boolean raw1 = (state1 & STATE_BYTES) != 0;
        boolean raw2 = (state2 & STATE_BYTES) != 0;
        boolean uni1 = CodingUtils.isUnibyteState(state1);
        boolean uni2 = CodingUtils.isUnibyteState(state2);
        return raw1 ? uni2 : (raw2 ? uni1 : true);
    }

    public static ELispString fromRaw(ByteBuffer buffer) {
        byte[] bytes = new byte[buffer.remaining()];
        buffer.get(bytes);
        return ELispString.ofKnown(bytes, bytes.length, STATE_BYTES);
    }
}
