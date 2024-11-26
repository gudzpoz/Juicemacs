package party.iroiro.juicemacs.mule;

import com.oracle.truffle.api.strings.TruffleString;

import java.util.NoSuchElementException;
import java.util.PrimitiveIterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

/// A string-like object
///
/// ## Implementation
///
/// Basically, it implements [the Emacs string representation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Representations.html)
/// that extends the range of code points to `#x3FFFFF`. (Actually, with [MuleString],
/// we can now go up to [Integer#MAX_VALUE].)
///
/// Instead of encoding code points in UTF-8-ish fashion, we follow the string compression
/// method used by HotSpot [String] and [TruffleString]: we transparently switch between the
/// following three representations:
/// - Latin-1: 1-byte per code point, only for code points in the range `#x0000` to `#x00FF`.
/// - 16-bit: 2-bytes per code point, only for code points in the range `#x0000` to `#xFFFF`.
/// - 32-bit: 4-bytes per code point, for all other code points.
///
/// We also offer a [StringBuilder]-like API to build strings - [MuleStringBuffer], which is
/// itself a [MuleString].
///
/// ## Naming
///
/// No, it actually has very little to do with MULE (MUlti-Lingual Emacs) or
/// the MULE encoding.
///
/// ## Performance
///
/// The performance is considered bad since it was written with no optimization in mind.
/// So this is a TODO: optimize [#equals(Object)], [#hashCode()] and [#compareTo(MuleString)].
public sealed interface MuleString
        extends Comparable<MuleString>
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

    default IntStream codePoints() {
        return codePoints(0);
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

    static MuleString fromLatin1(byte[] bytes) {
        return new MuleByteArrayString(bytes);
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

    default boolean startsWith(String s) {
        PrimitiveIterator.OfInt expected = s.codePoints().iterator();
        PrimitiveIterator.OfInt actual = iterator(0);
        while (expected.hasNext()) {
            if (!actual.hasNext() || expected.nextInt() != actual.nextInt()) {
                return false;
            }
        }
        return true;
    }

    @Override
    default int compareTo(MuleString other) {
        PrimitiveIterator.OfInt i = iterator(0);
        PrimitiveIterator.OfInt j = other.iterator(0);
        while (i.hasNext() && j.hasNext()) {
            int cmp = Integer.compare(i.nextInt(), j.nextInt());
            if (cmp != 0) {
                return cmp;
            }
        }
        return i.hasNext() ? 1 : (j.hasNext() ? -1 : 0);
    }
}