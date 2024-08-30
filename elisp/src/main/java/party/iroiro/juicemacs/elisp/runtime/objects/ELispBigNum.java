package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import party.iroiro.juicemacs.elisp.ELispLanguage;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * @see <a href="https://github.com/graalvm/simplelanguage/blob/master/language/src/main/java/com/oracle/truffle/sl/runtime/SLBigInteger.java">
 * SLBigInteger.java</a>
 */
@ExportLibrary(InteropLibrary.class)
public record ELispBigNum(BigInteger value) implements TruffleObject, Comparable<ELispBigNum>, ELispValue {

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

    @ExportMessage
    public boolean isNumber() {
        return true;
    }

    @ExportMessage
    @TruffleBoundary
    public boolean fitsInByte() {
        return value.bitLength() < 8;
    }

    @ExportMessage
    @TruffleBoundary
    public boolean fitsInShort() {
        return value.bitLength() < 16;
    }

    @ExportMessage
    @TruffleBoundary
    public boolean fitsInInt() {
        return value.bitLength() < 32;
    }

    @ExportMessage
    @TruffleBoundary
    public boolean fitsInLong() {
        return value.bitLength() < 64;
    }

    @ExportMessage
    @TruffleBoundary
    public boolean fitsInFloat() {
        if (value.bitLength() <= 24) {
            return true;
        }
        float floatValue = value.floatValue();
        if (!Float.isFinite(floatValue)) {
            return false;
        }
        try {
            return new BigDecimal(floatValue).toBigIntegerExact().equals(value);
        } catch (ArithmeticException e) {
            throw CompilerDirectives.shouldNotReachHere(e);
        }
    }

    @ExportMessage
    @TruffleBoundary
    public boolean fitsInDouble() {
        if (value.bitLength() <= 53) {
            return true;
        }
        double doubleValue = value.doubleValue();
        if (!Double.isFinite(doubleValue)) {
            return false;
        }
        try {
            return new BigDecimal(doubleValue).toBigIntegerExact().equals(value);
        } catch (ArithmeticException e) {
            throw CompilerDirectives.shouldNotReachHere(e);
        }
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
    @TruffleBoundary
    public double asDouble() throws UnsupportedMessageException {
        if (fitsInDouble()) {
            return value.doubleValue();
        }
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    @TruffleBoundary
    public float asFloat() throws UnsupportedMessageException {
        if (fitsInFloat()) {
            return value.floatValue();
        }
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    @TruffleBoundary
    public long asLong() throws UnsupportedMessageException {
        if (fitsInLong()) {
            return value.longValue();
        }
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    @TruffleBoundary
    public int asInt() throws UnsupportedMessageException {
        if (fitsInInt()) {
            return value.intValue();
        }
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    @TruffleBoundary
    public short asShort() throws UnsupportedMessageException {
        if (fitsInShort()) {
            return value.shortValue();
        }
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    @TruffleBoundary
    public byte asByte() throws UnsupportedMessageException {
        if (fitsInByte()) {
            return value.byteValue();
        }
        throw UnsupportedMessageException.create();
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
    public boolean hasMetaObject() {
        return false;
    }

    @ExportMessage
    public Object getMetaObject() {
        throw CompilerDirectives.shouldNotReachHere();
    }

    @ExportMessage
    @TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return value.toString();
    }

    @Override
    public boolean lispEquals(Object other) {
        return (other instanceof Long l && value.equals(BigInteger.valueOf(l)))
                || (other instanceof ELispBigNum(BigInteger i) && value.equals(i));
    }
}
