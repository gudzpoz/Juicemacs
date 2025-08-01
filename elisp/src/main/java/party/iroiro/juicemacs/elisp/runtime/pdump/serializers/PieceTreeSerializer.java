package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;
import party.iroiro.juicemacs.piecetree.StringBuffer;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

import java.util.HashSet;
import java.util.List;

public final class PieceTreeSerializer extends Serializer<PieceTreeBase> {
    public PieceTreeSerializer(Fury fury) {
        super(fury, PieceTreeBase.class);
    }

    @Override
    public void write(MemoryBuffer buffer, PieceTreeBase value) {
        fury.writeRef(buffer, value.getLinesRawContent());
    }

    @Override
    public PieceTreeBase read(MemoryBuffer buffer) {
        fury.getRefResolver().reference(null);
        MuleString content = (MuleString) fury.readRef(buffer);
        return new PieceTreeBase(
                List.of(new StringBuffer(content, true)),
                PieceTreeBase.EndOfLine.LF,
                false
        );
    }

    @SuppressWarnings("rawtypes")
    public static final class Intervals extends Serializer<IntervalPieceTree> {
        public Intervals(Fury fury) {
            super(fury, IntervalPieceTree.class);
        }

        @SuppressWarnings("unchecked")
        @Override
        public void write(MemoryBuffer buffer, IntervalPieceTree value) {
            try (DumpUtils.CounterSlot slot = DumpUtils.CounterSlot.record(buffer)) {
                value.forPropertiesIn(0, Long.MAX_VALUE, true, new IntervalPieceTree.IntervalConsumer() {
                    long offset = 0;

                    @Override
                    public @Nullable Object accept(@Nullable Object a, long offset, long length) {
                        fury.writeRef(buffer, a);
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
            fury.getRefResolver().reference(tree);
            int count = buffer.readInt32();
            long offset = 0;
            for (int i = 0; i < count; i++) {
                Object value = fury.readRef(buffer);
                long length = buffer.readVarUint64();
                tree.insert(offset, length, value);
            }
            return tree;
        }
    }

    @SuppressWarnings("rawtypes")
    public static final class Marks extends Serializer<MarkerPieceTree> {
        public Marks(Fury fury) {
            super(fury, MarkerPieceTree.class);
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
            DumpUtils.writeAnchors(fury, buffer, markerArray);
            fury.writeRef(buffer, value.getBuffer());
            fury.writeRef(buffer, value.getListener());
        }

        @SuppressWarnings({"unchecked", "PMD.UseDiamondOperator"})
        @Override
        public MarkerPieceTree read(MemoryBuffer buffer) {
            @SuppressWarnings("DataFlowIssue")
            MarkerPieceTree tree = new MarkerPieceTree(null, null);
            fury.getRefResolver().reference(tree);

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
            DumpUtils.readAnchors(fury, buffer, markerArray);
            Object treeBuffer = fury.readRef(buffer);
            MarkerPieceTree.MarkerMovedListener listener = (MarkerPieceTree.MarkerMovedListener) fury.readRef(buffer);
            tree.setBuffer(treeBuffer);
            tree.setListener(listener);
            return tree;
        }
    }
}
