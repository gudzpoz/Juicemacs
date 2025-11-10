package party.iroiro.juicemacs.mule;

/// Utility functions for iterating utf-8-emacs coding bytes
///
/// This assumes the bytes to be valid.
public abstract class Utf8Utils {
    private Utf8Utils() {
    }

    public static int codepointIndexToByteIndex(byte[] bytes, boolean unibyte, int index, int byteOffset) {
        if (unibyte) {
            return index + byteOffset;
        }
        int i = byteOffset;
        while (true) {
            do {
                if (index <= 0 || i >= bytes.length) {
                    return i;
                }
                index--;
            } while (bytes[i++] >= 0);

            while (i < bytes.length) {
                // skip continuation bytes
                if ((bytes[i] & 0b1100_0000) != 0b1000_0000) {
                    break;
                }
                i++;
            }
        }
    }

    public static int countCodepoints(byte[] bytes, boolean unibyte, int byteOffset, int byteEnd) {
        if (unibyte) {
            return byteEnd - byteOffset;
        }
        int count = 0;
        int start = Math.max(0, byteOffset);
        int end = Math.min(byteEnd, bytes.length);
        for (int i = start; i < end; i++) {
            count += (bytes[i] & 0b1100_0000) == 0b1000_0000 ? 0 : 1;
        }
        return count;
    }

    public static int nearestValidByteOffset(byte[] bytes, boolean unibyte, int byteOffset) {
        if (unibyte) {
            return byteOffset;
        }
        if (byteOffset >= bytes.length) {
            return bytes.length;
        }
        while ((bytes[byteOffset] & 0b1100_0000) == 0b1000_0000) {
            byteOffset--;
        }
        return byteOffset;
    }
}
