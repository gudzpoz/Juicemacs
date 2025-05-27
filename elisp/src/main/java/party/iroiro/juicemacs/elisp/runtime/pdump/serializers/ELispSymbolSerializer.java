package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.mule.MuleString;

public final class ELispSymbolSerializer extends Serializer<ELispSymbol> {
    public ELispSymbolSerializer(Fury fury) {
        super(fury, ELispSymbol.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispSymbol value) {
        fury.writeRef(buffer, value.name());
    }

    @Override
    public ELispSymbol read(MemoryBuffer buffer) {
        fury.getRefResolver().reference(null);
        return new ELispSymbol((MuleString) fury.readRef(buffer));
    }
}
