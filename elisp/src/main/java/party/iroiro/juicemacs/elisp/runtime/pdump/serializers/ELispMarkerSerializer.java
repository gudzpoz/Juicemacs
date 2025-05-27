package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispMarker;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

public final class ELispMarkerSerializer extends Serializer<ELispMarker> {
    public ELispMarkerSerializer(Fury fury) {
        super(fury, ELispMarker.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispMarker value) {
        MarkerPieceTree.Marker inner = value.getInner();
        fury.writeRef(buffer, inner.tree());
        if (inner.tree() == null) {
            buffer.writeBoolean(inner.affinity() == MarkerPieceTree.Affinity.RIGHT);
            DumpUtils.writeAnchor(fury, buffer, inner);
        } else {
            fury.writeRef(buffer, inner);
        }
    }

    @Override
    public ELispMarker read(MemoryBuffer buffer) {
//        @SuppressWarnings("DataFlowIssue") ELispMarker marker = new ELispMarker(null);
        ELispMarker marker = new ELispMarker();
        fury.getRefResolver().reference(marker);

        Object o = fury.readRef(buffer);
        if (o == null) {
            MarkerPieceTree.Marker inner = new MarkerPieceTree.Marker(buffer.readBoolean()
                    ? MarkerPieceTree.Affinity.RIGHT
                    : MarkerPieceTree.Affinity.LEFT);
            DumpUtils.readAnchor(fury, buffer, inner);
            return new ELispMarker(inner);
        }
        marker.setInner((MarkerPieceTree.Marker) fury.readRef(buffer));
        return marker;
    }
}
