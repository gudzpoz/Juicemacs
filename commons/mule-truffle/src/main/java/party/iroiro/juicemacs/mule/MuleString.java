package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;

import java.util.NoSuchElementException;
import java.util.PrimitiveIterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

public sealed interface MuleString
        permits MuleByteArrayString, MuleIntArrayString, MuleStringBuffer, MuleTruffleString {
    int length();

    int codePointAt(int index);

    MuleString subSequence(int start, int end);

    @Override
    String toString();

    @Override
    boolean equals(Object o);

    @Override
    int hashCode();

    default int charAt(int index) {
        return codePointAt(index);
    }

    default MuleString substring(int start, int end) {
        return subSequence(start, end);
    }

    default IntStream codePoints(int start) {
        return StreamSupport.intStream(
                () -> Spliterators.spliterator(iterator(start), length() - start, Spliterator.ORDERED),
                Spliterator.SUBSIZED | Spliterator.SIZED | Spliterator.ORDERED,
                false
        );
    }

    default PrimitiveIterator.OfInt iterator(int start) {
        final class CodePointIterator implements PrimitiveIterator.OfInt {
            int index = start;

            @Override
            public int nextInt() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                return codePointAt(index++);
            }

            @Override
            public boolean hasNext() {
                return index < length();
            }
        }
        return new CodePointIterator();
    }

    static MuleString fromString(String string) {
        TruffleString truffleString = MuleTruffleString.fromJavaString(string);
        int bytes = truffleString.byteLength(TruffleString.Encoding.UTF_32);
        if (bytes == string.length()) {
            // Latin1
            return new MuleByteArrayString(MuleTruffleString.toLatin1(truffleString));
        }
        return new MuleTruffleString(truffleString);
    }

    static boolean equals(MuleString a, MuleString b) {
        if (a.length() != b.length()) {
            return false;
        }
        PrimitiveIterator.OfInt i1 = a.iterator(0);
        PrimitiveIterator.OfInt i2 = b.iterator(0);
        while (i1.hasNext()) {
            if (i1.nextInt() != i2.nextInt()) {
                return false;
            }
        }
        return true;
    }
    static int hashCode(MuleString s) {
        int h = 1;
        PrimitiveIterator.OfInt i = s.iterator(0);
        while (i.hasNext()) {
            h = 31 * h + i.nextInt();
        }
        return h;
    }
}
