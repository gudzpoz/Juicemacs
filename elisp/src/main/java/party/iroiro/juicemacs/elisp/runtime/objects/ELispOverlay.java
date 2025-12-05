package party.iroiro.juicemacs.elisp.runtime.objects;

import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.piecetree.interval.Interval;

public final class ELispOverlay extends AbstractELispIdentityObject {
    @Nullable
    private ELispBuffer buffer;
    private final Interval inner;

    public ELispOverlay(@Nullable ELispBuffer buffer, Interval inner) {
        this.buffer = buffer;
        this.inner = inner;
        inner.setUserData(this);
    }

    public long start() {
        if (buffer == null) {
            return -1;
        }
        // TODO: make use of inner.cachedVersionId to avoid resolving
        buffer.resolveOverlay(inner);
        return inner.cachedAbsoluteStart + 1;
    }

    public long end() {
        if (buffer == null) {
            return -1;
        }
        buffer.resolveOverlay(inner);
        return inner.cachedAbsoluteEnd + 1;
    }

    @Nullable
    public ELispBuffer buffer() {
        return buffer;
    }

    public Interval inner() {
        return inner;
    }

    public void moveOverlay(long start, long end, @Nullable ELispBuffer other) {
        if (buffer == null && other == null) {
            return;
        }
        if (buffer != null) {
            buffer.deleteOverlay(inner);
        }
        inner.reset(0, start, end);
        (other == null ? buffer : other).insertOverlay(inner);
    }

    public void detach() {
        if (buffer == null) {
            return;
        }
        buffer.deleteOverlay(inner);
        buffer = null;
    }
}
