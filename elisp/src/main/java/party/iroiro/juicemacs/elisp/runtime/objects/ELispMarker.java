package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asBuffer;

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
        return inner.position() + 1;
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
        return "#<marker@" + getBuffer()+ ":" + point() + ">";
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
}
