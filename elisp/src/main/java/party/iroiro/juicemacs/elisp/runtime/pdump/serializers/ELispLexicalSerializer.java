package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.frame.MaterializedFrame;
import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;

public class ELispLexicalSerializer extends Serializer<ELispLexical> {
    public ELispLexicalSerializer(Fury fury) {
        super(fury, ELispLexical.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispLexical value) {
        fury.writeRef(buffer, value.parentScope());
        fury.writeRef(buffer, value.materializedParent());
        int size = value.symbols().size();
        buffer.writeVarUint32(size);
        for (ELispSymbol symbol : value.symbols()) {
            fury.writeRef(buffer, symbol);
        }
        IntArrayList slots = value.slots();
        for (int i = 0; i < size; i++) {
            buffer.writeVarUint32(slots.get(i) + 1);
        }
    }

    @Override
    public ELispLexical read(MemoryBuffer buffer) {
        // TODO: null? maybe just remove this serializer and let fury handle records
        fury.getRefResolver().reference(null);

        ELispLexical parentScope = (ELispLexical) fury.readRef(buffer);
        MaterializedFrame materializedParent = (MaterializedFrame) fury.readRef(buffer);
        int size = buffer.readVarUint32();
        ArrayList<ELispSymbol> symbols = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            symbols.add((ELispSymbol) fury.readRef(buffer));
        }
        IntArrayList slots = new IntArrayList(size);
        for (int i = 0; i < size; i++) {
            slots.add(buffer.readVarUint32() - 1);
        }
        return new ELispLexical(parentScope, materializedParent, symbols, slots);
    }
}
