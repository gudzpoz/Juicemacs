package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;

import java.util.Objects;

public final class ELispMarker extends Number implements ELispValue {
    @Nullable
    private ELispBuffer buffer;
    private int position;

    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispMarker marker && marker.buffer == buffer && marker.position == position;
    }
    @Override
    public int lispHashCode() {
        return Objects.hash(buffer, position);
    }

    //#region Number
    @Override
    public int intValue() {
        return position;
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
