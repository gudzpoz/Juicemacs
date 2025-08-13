package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.resolver.RefResolver;
import org.apache.fory.serializer.collection.CollectionSerializer;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;

import java.util.Collection;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public class ELispConsSerializer extends CollectionSerializer<ELispCons> {

    public ELispConsSerializer(Fory fory) {
        super(fory, ELispCons.class, false);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispCons value) {
        if (value.cdr() instanceof ELispCons) {
            writeList(buffer, value);
        } else {
            writeCons(buffer, value);
        }
    }

    @Override
    public ELispCons read(MemoryBuffer buffer) {
        byte state = buffer.readByte();
        if ((state & 0xF0) == 0xF0) {
            return readList(buffer);
        }
        return readCons(buffer, state);
    }

    private void writeCons(MemoryBuffer buffer, ELispCons value) {
        byte state = 0;
        boolean hasDebug = value.hasLocation();
        if (hasDebug) {
            state |= 0x01;
        }
        boolean cdrNil = isNil(value.cdr());
        if (cdrNil) {
            state |= 0x04;
        }
        boolean carNil = isNil(value.car());
        if (carNil) {
            state |= 0x08;
        }
        buffer.writeByte(state);
        if (!carNil) {
            fory.writeRef(buffer, value.car());
        }
        if (!cdrNil) {
            fory.writeRef(buffer, value.cdr());
        }
        if (hasDebug) {
            buffer.writeInt64(value.encodedLocation);
        }
    }

    private ELispCons readCons(MemoryBuffer buffer, byte state) {
        boolean hasDebug = (state & 0x01) != 0;
        boolean cdrNil = (state & 0x04) != 0;
        boolean carNil = (state & 0x08) != 0;
        ELispCons cons = ELispCons.cons(false, false);
        fory.getRefResolver().reference(cons);
        if (!carNil) {
            cons.setCar(fory.readRef(buffer));
        }
        if (!cdrNil) {
            cons.setCdr(fory.readRef(buffer));
        }
        if (hasDebug) {
            cons.encodedLocation = buffer.readInt64();
        }
        return cons;
    }

    @TruffleBoundary
    private void writeList(MemoryBuffer buffer, ELispCons value) {
        RefResolver resolver = fory.getRefResolver();
        buffer.writeByte(0xFF);
        try (DumpUtils.CounterSlot slot = DumpUtils.CounterSlot.record(buffer)) {
            // list: (obj1 obj2 obj3 . tail)
            // serialized:
            // - counter (3)
            // - writeRef(obj1) + location + ref(con2)
            // - writeRef(obj2) + location + ref(con3)
            // - writeRef(obj3) + location
            // - if writeRefValueFlag(tail):  ||
            //   - // ref written             || this part is essentially
            // - else:                        || what writeRef() does
            //   - writeNonRef(tail)          ||
            ELispCons current = value;
            while (true) {
                fory.writeRef(buffer, current.car());
                buffer.writeInt64(current.encodedLocation);
                slot.inc();
                if (!resolver.writeRefValueFlag(buffer, current.cdr())) {
                    return;
                }
                if (!(current.cdr() instanceof ELispCons next)) {
                    fory.writeNonRef(buffer, current.cdr());
                    return;
                }
                buffer.writerIndex(buffer.writerIndex() - 1);
                current = next;
            }
        }
    }

    private ELispCons readList(MemoryBuffer buffer) {
        RefResolver resolver = fory.getRefResolver();
        // list: (obj1 obj2 obj3 . tail)
        // - count = 3
        // - obj1 = readRef() + location + ref(cons2)
        // - obj2 = readRef() + location + ref(cons3)
        // - obj3 = readRef() + location
        // - tail = readRef()
        int count = buffer.readInt32();
        ELispCons head = ELispCons.cons(false, false);
        resolver.reference(head);
        ELispCons current = head;
        for (int i = 0; ; i++) {
            Object car = fory.readRef(buffer);
            current.setCar(car);
            current.encodedLocation = buffer.readInt64();
            if (i == count - 1) {
                current.setCdr(fory.readRef(buffer));
                break;
            }
            ELispCons next = ELispCons.cons(false, false);
            resolver.preserveRefId();
            resolver.reference(next);
            current.setCdr(next);
            current = next;
        }
        return head;
    }

    @Override
    public Collection<?> onCollectionWrite(MemoryBuffer buffer, ELispCons value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ELispCons onCollectionRead(Collection collection) {
        throw new UnsupportedOperationException();
    }
}
