package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispOverlay;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;
import party.iroiro.juicemacs.piecetree.interval.Interval;

public final class ELispOverlaySerializer extends Serializer<ELispOverlay> {
    public ELispOverlaySerializer(Fory fory) {
        super(fory, ELispOverlay.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispOverlay value) {
        ELispBuffer b = value.buffer();
        fory.writeRef(buffer, b);
        if (b == null) {
            DumpUtils.writeAnchor(fory, buffer, value.inner());
        } else {
            fory.writeRef(buffer, value.inner());
        }
    }

    @Override
    public ELispOverlay read(MemoryBuffer buffer) {
        ELispBuffer b = (ELispBuffer) fory.readRef(buffer);
        Interval interval;
        if (b == null) {
            interval = new Interval(1, 1);
            DumpUtils.readAnchor(fory, buffer, interval);
        } else {
            interval = (Interval) fory.readRef(buffer);
        }
        return new ELispOverlay(b, interval);
    }
}
