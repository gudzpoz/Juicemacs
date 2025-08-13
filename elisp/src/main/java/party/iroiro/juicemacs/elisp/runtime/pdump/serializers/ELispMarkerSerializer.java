package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispMarker;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

public final class ELispMarkerSerializer extends Serializer<ELispMarker> {
    public ELispMarkerSerializer(Fory fory) {
        super(fory, ELispMarker.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispMarker value) {
        MarkerPieceTree.Marker inner = value.getInner();
        fory.writeRef(buffer, inner.tree());
        if (inner.tree() == null) {
            buffer.writeBoolean(inner.affinity() == MarkerPieceTree.Affinity.RIGHT);
            DumpUtils.writeAnchor(fory, buffer, inner);
        } else {
            fory.writeRef(buffer, inner);
        }
    }

    @Override
    public ELispMarker read(MemoryBuffer buffer) {
        ELispMarker marker = new ELispMarker();
        fory.getRefResolver().reference(marker);

        Object o = fory.readRef(buffer);
        if (o == null) {
            MarkerPieceTree.Marker inner = new MarkerPieceTree.Marker(buffer.readBoolean()
                    ? MarkerPieceTree.Affinity.RIGHT
                    : MarkerPieceTree.Affinity.LEFT);
            DumpUtils.readAnchor(fory, buffer, inner);
            return new ELispMarker(inner);
        }
        marker.setInner((MarkerPieceTree.Marker) fory.readRef(buffer));
        return marker;
    }
}
