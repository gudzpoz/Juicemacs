package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

import java.util.HashSet;

public final class PieceTreeSerializer extends Serializer<PieceTreeBase> {
    public PieceTreeSerializer(Fory fory) {
        super(fory, PieceTreeBase.class);
    }

    @Override
    public void write(MemoryBuffer buffer, PieceTreeBase value) {
        buffer.writeBoolean(value.isRaw());
        byte[] bytes = value.getLinesRawContent();
        buffer.writeInt32(bytes.length);
        buffer.writeBytes(bytes);
    }

    @Override
    public PieceTreeBase read(MemoryBuffer buffer) {
        fory.getRefResolver().reference(null);
        boolean raw = buffer.readBoolean();
        int length = buffer.readInt32();
        byte[] content = buffer.readBytes(length);
        return new PieceTreeBase(raw, content);
    }

    @SuppressWarnings("rawtypes")
    public static final class Intervals extends Serializer<IntervalPieceTree> {
        public Intervals(Fory fory) {
            super(fory, IntervalPieceTree.class);
        }

        @SuppressWarnings("unchecked")
        @Override
        @TruffleBoundary
        public void write(MemoryBuffer buffer, IntervalPieceTree value) {
            try (DumpUtils.CounterSlot slot = DumpUtils.CounterSlot.record(buffer)) {
                value.forPropertiesIn(0, Long.MAX_VALUE, true, new IntervalPieceTree.IntervalConsumer() {
                    long offset = 0;

                    @Override
                    public @Nullable Object accept(@Nullable Object a, long offset, long length) {
                        fory.writeRef(buffer, a);
                        buffer.writeVarUint64(length);
                        assert offset == this.offset;
                        this.offset += length;
                        slot.inc();
                        return null;
                    }
                });
            }
        }

        @Override
        public IntervalPieceTree read(MemoryBuffer buffer) {
            IntervalPieceTree<Object> tree = new IntervalPieceTree<>();
            fory.getRefResolver().reference(tree);
            int count = buffer.readInt32();
            long offset = 0;
            for (int i = 0; i < count; i++) {
                Object value = fory.readRef(buffer);
                long length = buffer.readVarUint64();
                tree.insert(offset, length, value);
            }
            return tree;
        }
    }

    @SuppressWarnings("rawtypes")
    public static final class Marks extends Serializer<MarkerPieceTree> {
        public Marks(Fory fory) {
            super(fory, MarkerPieceTree.class);
        }

        @Override
        public void write(MemoryBuffer buffer, MarkerPieceTree value) {
            HashSet markers = value.getMarkers();
            buffer.writeVarUint64(value.getLength());
            buffer.writeVarUint32(markers.size());
            MarkerPieceTree.Marker[] markerArray = new MarkerPieceTree.Marker[markers.size()];
            int i = 0;
            for (Object o : markers) {
                MarkerPieceTree.Marker marker = (MarkerPieceTree.Marker) o;
                markerArray[i++] = marker;
                long position = marker.position();
                buffer.writeVarUint64(position);
                buffer.writeBoolean(marker.affinity() == MarkerPieceTree.Affinity.RIGHT);
            }
            DumpUtils.writeAnchors(fory, buffer, markerArray);
            fory.writeRef(buffer, value.getBuffer());
            fory.writeRef(buffer, value.getListener());
        }

        @SuppressWarnings({"unchecked", "PMD.UseDiamondOperator"})
        @Override
        public MarkerPieceTree read(MemoryBuffer buffer) {
            MarkerPieceTree tree = new MarkerPieceTree((_, _) -> {
                throw CompilerDirectives.shouldNotReachHere();
            }, 0xDEADBEEFL);
            fory.getRefResolver().reference(tree);

            long length = buffer.readVarUint64();
            tree.insertString(0, length);

            int markerCount = buffer.readVarUint32();
            MarkerPieceTree.Marker[] markerArray = new MarkerPieceTree.Marker[markerCount];
            for (int i = 0; i < markerCount; i++) {
                long position = buffer.readVarUint64();
                MarkerPieceTree.Affinity affinity = buffer.readBoolean() ? MarkerPieceTree.Affinity.RIGHT : MarkerPieceTree.Affinity.LEFT;
                MarkerPieceTree.Marker marker = new MarkerPieceTree.Marker(affinity);
                markerArray[i] = marker;
                tree.insertMarker(position, marker);
            }
            DumpUtils.readAnchors(fory, buffer, markerArray);
            Object treeBuffer = fory.readRef(buffer);
            MarkerPieceTree.MarkerMovedListener listener = (MarkerPieceTree.MarkerMovedListener) fory.readRef(buffer);
            tree.setBuffer(treeBuffer);
            tree.setListener(listener);
            return tree;
        }
    }
}
