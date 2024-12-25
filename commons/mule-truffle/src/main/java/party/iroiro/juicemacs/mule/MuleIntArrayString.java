package party.iroiro.juicemacs.mule;

import java.util.Arrays;

public final class MuleIntArrayString implements MuleString {
    private final int[] array;

    MuleIntArrayString(int[] array) {
        this.array = array;
    }

    @Override
    public long length() {
        return array.length;
    }

    @Override
    public int codePointAt(long index) {
        return array[Math.toIntExact(index)];
    }

    @Override
    public MuleString subSequence(long start, long end) {
        int startI = Math.toIntExact(start);
        int endI = Math.toIntExact(end);
        int[] newArray = new int[endI - startI];
        System.arraycopy(array, startI, newArray, 0, newArray.length);
        return new MuleIntArrayString(newArray);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int c : array) {
            if (c <= Character.MAX_CODE_POINT) {
                builder.appendCodePoint(c);
            } else {
                String hexString = Integer.toHexString(c);
                hexString = "0".repeat(8 - hexString.length()) + hexString;
                builder.append("\\U").append(hexString);
            }
        }
        return builder.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof MuleString s)) {
            return false;
        }
        if (s instanceof MuleIntArrayString other) {
            return Arrays.equals(other.array, array);
        }
        return MuleString.equals(this, s);
    }

    @Override
    public int hashCode() {
        return MuleString.hashCode(this);
    }

    int[] intArray() {
        return array;
    }
}
