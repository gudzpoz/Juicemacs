package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispMarker;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInMarker extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInMarkerFactory.getFactories();
    }

    /**
     * <pre>
     * Return the buffer that MARKER points into, or nil if none.
     * Returns nil if MARKER points into a dead buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "marker-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMarkerBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object markerBuffer(ELispMarker marker) {
            @Nullable ELispBuffer buffer = marker.getBuffer();
            return buffer == null ? false : buffer;
        }
    }

    /**
     * <pre>
     * Return the position of MARKER, or nil if it points nowhere.
     * </pre>
     */
    @ELispBuiltIn(name = "marker-position", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMarkerPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void markerPosition(Object marker) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return last position of MARKER in its buffer.
     * This is like `marker-position' with one exception:  If the buffer of
     * MARKER is dead, it returns the last position of MARKER in that buffer
     * before it was killed.
     * </pre>
     */
    @ELispBuiltIn(name = "marker-last-position", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMarkerLastPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void markerLastPosition(Object marker) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Position MARKER before character number POSITION in BUFFER.
     * If BUFFER is omitted or nil, it defaults to the current buffer.  If
     * POSITION is nil, makes marker point nowhere so it no longer slows down
     * editing in any buffer.  Returns MARKER.
     * </pre>
     */
    @ELispBuiltIn(name = "set-marker", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetMarker extends ELispBuiltInBaseNode {
        @Specialization
        public ELispMarker setMarker(ELispMarker marker, Object position, Object buffer) {
            if (isNil(position)) {
                marker.setBuffer(null, -1);
            } else {
                marker.setBuffer(isNil(buffer) ? getContext().currentBuffer() : asBuffer(buffer), asLong(position));
            }
            return marker;
        }
    }

    /**
     * <pre>
     * Return a new marker pointing at the same place as MARKER.
     * If argument is a number, makes a new marker pointing
     * at that position in the current buffer.
     * If MARKER is not specified, the new marker does not point anywhere.
     * The optional argument TYPE specifies the insertion type of the new marker;
     * see `marker-insertion-type'.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-marker", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCopyMarker extends ELispBuiltInBaseNode {
        @Specialization
        public ELispMarker copyMarker(Object marker, Object type) {
            ELispMarker newMarker = new ELispMarker();
            if (!isNil(type)) {
                newMarker.setAffinity(MarkerPieceTree.Affinity.RIGHT);
            }
            if (isNil(marker)) {
                return newMarker;
            }
            if (marker instanceof Long l) {
                ELispBuffer buffer = getContext().currentBuffer();
                newMarker.setBuffer(buffer, l);
            } else {
                ELispMarker m = asMarker(marker);
                @Nullable ELispBuffer buffer = m.getBuffer();
                if (buffer != null) {
                    newMarker.setBuffer(buffer, m.point());
                }
            }
            return newMarker;
        }
    }

    /**
     * <pre>
     * Return insertion type of MARKER: t if it stays after inserted text.
     * The value nil means the marker stays before text inserted there.
     * </pre>
     */
    @ELispBuiltIn(name = "marker-insertion-type", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMarkerInsertionType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void markerInsertionType(Object marker) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the insertion-type of MARKER to TYPE.
     * If TYPE is t, it means the marker advances when you insert text at it.
     * If TYPE is nil, it means the marker stays behind when you insert text at it.
     * </pre>
     */
    @ELispBuiltIn(name = "set-marker-insertion-type", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetMarkerInsertionType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setMarkerInsertionType(Object marker, Object type) {
            throw new UnsupportedOperationException();
        }
    }
}
