package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

public final class ELispSymbolSerializer extends Serializer<ELispSymbol> {
    public ELispSymbolSerializer(Fory fory) {
        super(fory, ELispSymbol.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispSymbol value) {
        fory.writeRef(buffer, value.name());
    }

    @Override
    public ELispSymbol read(MemoryBuffer buffer) {
        fory.getRefResolver().reference(null);
        return new ELispSymbol((String) fory.readRef(buffer));
    }
}
