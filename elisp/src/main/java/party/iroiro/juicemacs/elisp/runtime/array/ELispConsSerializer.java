package party.iroiro.juicemacs.elisp.runtime.array;

import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.resolver.RefResolver;
import org.apache.fury.serializer.collection.AbstractCollectionSerializer;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;

import java.util.Collection;

import static party.iroiro.juicemacs.elisp.runtime.array.ELispConsAccess.IsNilNode.isNil;

public class ELispConsSerializer extends AbstractCollectionSerializer<ELispCons> {

    public ELispConsSerializer(Fury fury) {
        super(fury, ELispCons.class, false);
        fury.registerSerializer(ELispConsArray.class, DumpUtils.never(fury, ELispConsArray.class));
    }

    private void writeArray(MemoryBuffer buffer, Object[] array, int limit) {
        for (int i = 0; i < limit; i++) {
            fury.writeRef(buffer, array[i]);
        }
    }
    private Object[] readArray(MemoryBuffer buffer, int limit) {
        Object[] array = new Object[limit];
        for (int i = 0; i < limit; i++) {
            array[i] = fury.readRef(buffer);
        }
        return array;
    }

    /// Writes an [ELispCons] and its inner [ELispConsArray]
    ///
    /// If the inner [ELispConsArray] has a [ELispCons] `cdr` to be written
    /// anew, a flag is written and the `cdr` field is returned to the caller.
    /// The caller should then write the `cdr` field.
    @Nullable
    private ELispCons writeStep(MemoryBuffer buffer, ELispCons value) {
        value = value.forwarded();

        buffer.writeVarUint32(value.index);
        RefResolver resolver = fury.getRefResolver();
        ELispConsArray array = value.array;
        if (resolver.writeRefValueFlag(buffer, array)) {
            buffer.writeVarUint32(array.size);
            writeArray(buffer, (Object[]) array.array, array.size);

            byte extra = 0;
            if (!isNil(array.cdr)) {
                extra |= (byte) (array.cdr instanceof ELispCons ? 0x2 : 0x1);
            }
            if (array.getStartLine() != 0) {
                extra |= 0x4;
                if (array.getStartLine() == array.getEndLine()) {
                    extra |= 0x8;
                }
            }
            buffer.writeByte(extra);

            if ((extra & 0x1) != 0) {
                fury.writeRef(buffer, array.cdr);
            }
            if ((extra & 0x4) != 0) {
                buffer.writeVarUint32(array.getStartLine());
                buffer.writeVarUint32(array.getStartColumn());
                if ((extra & 0x8) == 0) {
                    buffer.writeVarUint32(array.getEndLine());
                }
                buffer.writeVarUint32(array.getEndColumn());
            }
            if ((extra & 0x2) != 0) {
                if (resolver.writeRefValueFlag(buffer, array.cdr)) {
                    return (ELispCons) array.cdr;
                }
            }
        }
        return null;
    }

    @Override
    public void write(MemoryBuffer buffer, ELispCons value) {
        ELispCons current = value;
        while (current != null) {
            current = writeStep(buffer, current);
        }
    }

    public boolean readStep(MemoryBuffer buffer, ELispCons[] output) {
        RefResolver resolver = fury.getRefResolver();
        int consRef = resolver.lastPreservedRefId();

        int index = buffer.readVarUint32();
        if (resolver.readRefOrNull(buffer) == Fury.REF_FLAG) {
            ELispCons cons = new ELispCons((ELispConsArray) resolver.getReadObject(), index);
            resolver.setReadObject(consRef, cons);
            output[0] = cons;
            return false;
        }

        int size = buffer.readVarUint32();
        ELispConsArray array = new ELispConsArray(false, size, SingleArrayStrategy.INSTANCE);
        ELispCons cons = new ELispCons(array, index);
        resolver.setReadObject(consRef, cons);
        resolver.preserveRefId();
        resolver.reference(array);
        array.array = readArray(buffer, size);
        output[0] = cons;

        int extra = buffer.readByte();
        if ((extra & 0x1) != 0) {
            array.cdr = fury.readRef(buffer);
            array.strategy = WithCdrStrategy.INSTANCE;
        }
        if ((extra & 0x4) != 0) {
            int startLine = buffer.readVarUint32();
            int startColumn = buffer.readVarUint32();
            int endLine = (extra & 0x8) == 0 ? buffer.readVarUint32() : startLine;
            int endColumn = buffer.readVarUint32();
            array.setSourceLocation(startLine, startColumn, endLine, endColumn);
        }
        if ((extra & 0x2) != 0) {
            array.strategy = WithCdrStrategy.INSTANCE;
            if (resolver.readRefOrNull(buffer) != Fury.REF_FLAG) {
                resolver.preserveRefId();
                return true;
            }
            array.cdr = resolver.getReadObject();
            return false;
        }
        return false;
    }

    @Override
    public ELispCons read(MemoryBuffer buffer) {
        @Nullable ELispCons head = null, prev = null;
        ELispCons[] output = new ELispCons[1];
        while (true) {
            boolean hasNext = readStep(buffer, output);
            if (head == null) {
                head = output[0];
            }
            if (prev != null) {
                prev.array.cdr = output[0];
            }
            if (!hasNext) {
                break;
            }
            prev = output[0];
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
