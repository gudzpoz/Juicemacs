package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asBuffer;

@ExportLibrary(InteropLibrary.class)
public final class ELispMarker extends Number implements ELispValue {
    private final MarkerPieceTree.Marker inner;

    public ELispMarker() {
        this(null, 1);
    }

    public ELispMarker(@Nullable ELispBuffer buffer, long point) {
        this.inner = new MarkerPieceTree.Marker(MarkerPieceTree.Affinity.LEFT);
        setBuffer(buffer, point);
    }

    public @Nullable ELispBuffer getBuffer() {
        MarkerPieceTree<?> tree = inner.tree();
        if (tree == null) {
            return null;
        }
        Object buffer = tree.getBuffer();
        return asBuffer(buffer);
    }

    public void setBuffer(@Nullable ELispBuffer buffer, long point) {
        if (buffer == null) {
            inner.detach();
        } else {
            buffer.setMarkerPoint(inner, point);
        }
    }

    public long point() {
        if (inner.isDetached()) {
            throw ELispSignals.error("marker does not point anywhere");
        }
        return inner.position() + 1;
    }

    public void setAffinity(MarkerPieceTree.Affinity affinity) {
        inner.setAffinity(affinity);
    }

    @Override
    public boolean lispEquals(Object other) {
        if (other instanceof ELispMarker marker) {
            @Nullable ELispBuffer otherBuffer = marker.getBuffer();
            @Nullable ELispBuffer buffer = getBuffer();
            if (otherBuffer == buffer) {
                return point() == marker.point();
            }
        }
        return false;
    }
    @Override
    public int lispHashCode() {
        return Objects.hash(getBuffer(), point());
    }

    @Override
    public void display(ELispPrint print) {
        print.print(toString());
    }

    @Override
    public String toString() {
        return inner.isDetached()
                ? "#<marker in no buffer>"
                : "#<marker@" + getBuffer()+ ":" + point() + ">";
    }

    //#region Number
    @Override
    public int intValue() {
        return (int) point();
    }
    @Override
    public long longValue() {
        return point();
    }
    @Override
    public float floatValue() {
        return point();
    }
    @Override
    public double doubleValue() {
        return point();
    }
    //#endregion Number

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
        return BigInteger.valueOf(point());
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

    @ExportMessage boolean fitsInByte() { return point() <= Byte.MAX_VALUE; }
    @ExportMessage boolean fitsInShort() { return point() <= Short.MAX_VALUE; }
    @ExportMessage boolean fitsInInt() { return point() <= Integer.MAX_VALUE; }
    @ExportMessage boolean fitsInLong() { return true; }
    @ExportMessage boolean fitsInFloat() {
        long point = point();
        if (Long.highestOneBit(point) <= 24) {
            return true;
        }
        //noinspection UnpredictableBigDecimalConstructorCall
        return new BigDecimal((float) point).toBigIntegerExact().longValue() == point;
    }
    @ExportMessage boolean fitsInDouble() {
        long point = point();
        if (Long.highestOneBit(point) <= 53) {
            return true;
        }
        //noinspection UnpredictableBigDecimalConstructorCall
        return new BigDecimal((float) point).toBigIntegerExact().longValue() == point;
    }
    @ExportMessage byte asByte() throws UnsupportedMessageException { return (byte) point(); }
    @ExportMessage short asShort() throws UnsupportedMessageException { return (short) point(); }
    @ExportMessage int asInt() throws UnsupportedMessageException { return (int) point(); }
    @ExportMessage long asLong() throws UnsupportedMessageException { return point(); }
    @ExportMessage float asFloat() throws UnsupportedMessageException { return point(); }
    @ExportMessage double asDouble() throws UnsupportedMessageException { return point(); }
    //#endregion InteropLibrary exports
}
