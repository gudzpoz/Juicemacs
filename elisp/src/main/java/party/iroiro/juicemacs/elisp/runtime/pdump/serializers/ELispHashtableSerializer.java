package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.resolver.RefResolver;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispHashtable;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;

public final class ELispHashtableSerializer extends Serializer<ELispHashtable> {
    public ELispHashtableSerializer(Fory fory) {
        super(fory, ELispHashtable.class);
    }

    @Override
    @TruffleBoundary
    public void write(MemoryBuffer buffer, ELispHashtable value) {
        RefResolver resolver = fory.getRefResolver();
        if (!resolver.writeRefOrNull(buffer, value.getTest())) {
            throw ELispSignals.fatal("unsupported hash table test type");
        }
        if (!resolver.writeRefOrNull(buffer, value.getWeakness())) {
            throw ELispSignals.fatal("unsupported hash table weakness type");
        }
        // value.size() is not reliable since it might be weak.
        try (DumpUtils.CounterSlot slot = DumpUtils.CounterSlot.record(buffer)) {
            value.forEach((k, v) -> {
                fory.writeRef(buffer, k);
                fory.writeRef(buffer, v);
                slot.inc();
            });
        }
    }

    @Override
    public ELispHashtable read(MemoryBuffer buffer) {
        RefResolver resolver = fory.getRefResolver();
        int tableId = resolver.lastPreservedRefId();
        // Serialized by #write, the test/weakness field should only be
        // references and should not contain nested references to this
        // table. But just in case, let's set it to NIL so that no null
        // reference is leaked.
        resolver.reference(false);

        Object test = fory.readRef(buffer);
        Object weakness = fory.readRef(buffer);
        ELispHashtable hashtable = new ELispHashtable(test, weakness);
        resolver.setReadObject(tableId, hashtable);

        int size = buffer.readInt32();
        for (int i = 0; i < size; i++) {
            Object key = fory.readRef(buffer);
            Object value = fory.readRef(buffer);
            hashtable.put(key, value);
        }
        return hashtable;
    }
}
