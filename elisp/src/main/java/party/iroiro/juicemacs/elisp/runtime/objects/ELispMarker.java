package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.Objects;

public final class ELispMarker extends Number implements ELispValue {
    @Nullable
    private ELispBuffer buffer;
    private long position;

    public ELispMarker() {
        this(null, 1);
    }

    public ELispMarker(@Nullable ELispBuffer buffer, long position) {
        this.buffer = buffer;
        this.position = position;
    }

    public @Nullable ELispBuffer getBuffer() {
        return buffer;
    }

    public void setBuffer(@Nullable ELispBuffer buffer) {
        this.buffer = buffer;
    }

    public long getPosition() {
        return position;
    }

    public void setPosition(long position) {
        this.position = position;
    }

    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispMarker marker && marker.buffer == buffer && marker.position == position;
    }
    @Override
    public int lispHashCode() {
        return Objects.hash(buffer, position);
    }

    @Override
    public void display(ELispPrint print) {
        print.print(toString());
    }

    @Override
    public String toString() {
        return "#<marker@" + buffer + ":" + position + ">";
    }

    //#region Number
    @Override
    public int intValue() {
        return (int) position;
    }
    @Override
    public long longValue() {
        return position;
    }
    @Override
    public float floatValue() {
        return position;
    }
    @Override
    public double doubleValue() {
        return position;
    }
    //#endregion Number
}
