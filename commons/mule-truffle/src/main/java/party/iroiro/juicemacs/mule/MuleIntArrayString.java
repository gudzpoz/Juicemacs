package party.iroiro.juicemacs.mule;

import java.util.Arrays;

public final class MuleIntArrayString implements MuleString {
    private final int[] array;

    MuleIntArrayString(int[] array) {
        this.array = array;
    }

    @Override
    public int length() {
        return array.length;
    }

    @Override
    public int codePointAt(int index) {
        return array[index];
    }

    @Override
    public MuleString subSequence(int start, int end) {
        int[] newArray = new int[end - start];
        System.arraycopy(array, start, newArray, 0, newArray.length);
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
