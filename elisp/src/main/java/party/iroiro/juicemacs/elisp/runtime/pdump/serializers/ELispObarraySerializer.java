package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispObarray;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.HashMap;

public final class ELispObarraySerializer extends Serializer<ELispObarray> {
    public ELispObarraySerializer(Fory fory) {
        super(fory, ELispObarray.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispObarray value) {
        HashMap<String, ELispSymbol> symbols = value.symbols();
        buffer.writeVarUint32(symbols.size());
        symbols.forEach((_, symbol) -> fory.writeRef(buffer, symbol));
    }

    @Override
    public ELispObarray read(MemoryBuffer buffer) {
        fory.getRefResolver().reference(null);
        int size = buffer.readVarUint32();
        HashMap<String, ELispSymbol> symbols = new HashMap<>(size);
        for (int i = 0; i < size; i++) {
            ELispSymbol symbol = (ELispSymbol) fory.readRef(buffer);
            symbols.put(symbol.name(), symbol);
        }
        return new ELispObarray(symbols);
    }
}
