package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import party.iroiro.juicemacs.elisp.ELispLanguage;

import java.math.BigInteger;

/// @see <a href="https://github.com/graalvm/simplelanguage/blob/master/language/src/main/java/com/oracle/truffle/sl/runtime/SLBigInteger.java">
/// SLBigInteger.java</a>
@SuppressWarnings("PMD.ShortMethodName")
@ExportLibrary(InteropLibrary.class)
public final class ELispBigNum extends Number implements TruffleObject, Comparable<ELispBigNum>, ELispValue {
    private final BigInteger value;

    private ELispBigNum(BigInteger value) {
        this.value = value;
    }

    /// Wrap a BigInteger into an ELispBigNum or a long if it fits.
    ///
    /// @param value the BigInteger to wrap
    /// @return the wrapped BigInteger or a long if it fits
    public static Number wrap(BigInteger value) {
        if (value.bitLength() < 64) {
            return value.longValue();
        }
        return new ELispBigNum(value);
    }

    /// Wrap a BigDecimal into an ELispBigNum (use [#wrap(BigInteger)] if possible)
    ///
    /// This is only intended to be used in [party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem].
    ///
    /// @param value the long value
    /// @return the wrapped BigDecimal
    public static ELispBigNum forceWrap(long value) {
        return new ELispBigNum(BigInteger.valueOf(value));
    }

    //#region Lisp API
    @TruffleBoundary
    public Object add1() {
        return wrap(value.add(BigInteger.ONE));
    }
    @TruffleBoundary
    public Object sub1() {
        return wrap(value.subtract(BigInteger.ONE));
    }
    @TruffleBoundary
    public Object log2() {
        if (value.compareTo(BigInteger.ZERO) <= 0) {
            return Double.NaN;
        }
        return (long) value.bitLength() - 1;
    }
    //#endregion Lisp API

    //#region BigInteger
    @TruffleBoundary
    public Number add(ELispBigNum implicitELispBigNum) {
        return wrap(value.add(implicitELispBigNum.value));
    }

    @TruffleBoundary
    public Number subtract(ELispBigNum implicitELispBigNum) {
        return wrap(value.subtract(implicitELispBigNum.value));
    }

    @TruffleBoundary
    public Number multiply(ELispBigNum implicitELispBigNum) {
        return wrap(value.multiply(implicitELispBigNum.value));
    }

    @TruffleBoundary
    public Number divide(ELispBigNum implicitELispBigNum) {
        return wrap(value.divide(implicitELispBigNum.value));
    }

    public Number xor(ELispBigNum other) {
        return wrap(value.xor(other.value));
    }

    public Number and(ELispBigNum other) {
        return wrap(value.and(other.value));
    }

    public Number or(ELispBigNum other) {
        return wrap(value.or(other.value));
    }

    public Number reciprocal() {
        return BigInteger.ONE.divide(value);
    }

    public Number not() {
        return wrap(value.not());
    }

    public Number negate() {
        return wrap(value.negate());
    }

    @TruffleBoundary
    public Number remainder(ELispBigNum other) {
        return wrap(value.remainder(other.value));
    }

    @TruffleBoundary
    public Number mod(ELispBigNum other) {
        return wrap(value.mod(other.value));
    }

    public Number shiftLeft(long count) {
        return wrap(value.shiftLeft((int) count));
    }

    public int bitCount() {
        return value.bitCount();
    }

    public int signum() {
        return value.signum();
    }

    @Override
    @TruffleBoundary
    public int compareTo(ELispBigNum o) {
        return value.compareTo(o.value);
    }

    @Override
    @TruffleBoundary
    public String toString() {
        return value.toString();
    }
    //#endregion BigInteger

    @Override
    public boolean lispEquals(Object other) {
        return (other instanceof Long l && value.equals(BigInteger.valueOf(l)))
                || ((other instanceof ELispBigNum n) && value.equals(n.value));
    }

    @SuppressWarnings("EqualsWhichDoesntCheckParameterClass")
    @Override
    public boolean equals(Object obj) {
        // TODO: Document incompatibilities
        // In Emacs, two bignums of identical values are not necessarily equal (by #'eq).
        // We choose to differ from Emacs here.
        return lispEquals(obj);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    //#region extends Number
    @Override
    public int intValue() {
        return value.intValue();
    }

    @Override
    public long longValue() {
        return value.longValue();
    }

    @TruffleBoundary
    @Override
    public float floatValue() {
        return value.floatValue();
    }

    @TruffleBoundary
    @Override
    public double doubleValue() {
        return value.doubleValue();
    }
    //#endregion extends Number

    //#region InteropLibrary exports
    @ExportMessage
    public boolean isNumber() {
        return true;
    }

    @ExportMessage
    public boolean fitsInBigInteger() {
        return true;
    }

    @ExportMessage
    public BigInteger asBigInteger() {
        return value;
    }

    @ExportMessage
    public boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    public Class<? extends TruffleLanguage<?>> getLanguage() {
        return ELispLanguage.class;
    }

    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return toString();
    }

    @ExportMessage boolean fitsInByte() { return false; }
    @ExportMessage boolean fitsInShort() { return false; }
    @ExportMessage boolean fitsInInt() { return false; }
    @ExportMessage boolean fitsInLong() { return false; }
    @ExportMessage boolean fitsInFloat() { return false; }
    @ExportMessage boolean fitsInDouble() { return false; }
    @ExportMessage byte asByte() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage short asShort() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage int asInt() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage long asLong() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage float asFloat() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage double asDouble() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    //#endregion InteropLibrary exports

}
