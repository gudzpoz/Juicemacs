package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispObarray;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.HashMap;

public final class ELispObarraySerializer extends Serializer<ELispObarray> {
    public ELispObarraySerializer(Fury fury) {
        super(fury, ELispObarray.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispObarray value) {
        HashMap<MuleString, ELispSymbol> symbols = value.symbols();
        buffer.writeVarUint32(symbols.size());
        symbols.forEach((_, symbol) -> fury.writeRef(buffer, symbol));
    }

    @Override
    public ELispObarray read(MemoryBuffer buffer) {
        fury.getRefResolver().reference(null);
        int size = buffer.readVarUint32();
        HashMap<MuleString, ELispSymbol> symbols = new HashMap<>(size);
        for (int i = 0; i < size; i++) {
            ELispSymbol symbol = (ELispSymbol) fury.readRef(buffer);
            symbols.put(symbol.name(), symbol);
        }
        return new ELispObarray(symbols);
    }
}
