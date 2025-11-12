package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.resolver.RefResolver;
import org.apache.fory.serializer.Serializer;
import org.graalvm.collections.MapCursor;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispObarray;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispObarray.HashStringMap;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

/// Specialized version of [HashSerializer] because [ELispSymbol] contains a copy of the symbol name
public final class ELispObarraySerializer extends Serializer<ELispObarray> {
    public ELispObarraySerializer(Fory fory) {
        super(fory, ELispObarray.class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispObarray value) {
        HashStringMap<ELispSymbol> symbols = value.symbols();
        buffer.writeVarUint32(symbols.size());
        MapCursor<ELispString, ELispSymbol> entries = symbols.map().getEntries();
        while (entries.advance()) {
            fory.writeRef(buffer, entries.getValue());
        }
    }

    @Override
    public ELispObarray read(MemoryBuffer buffer) {
        RefResolver resolver = fory.getRefResolver();
        int id = resolver.lastPreservedRefId();
        int size = buffer.readVarUint32();
        HashStringMap<ELispSymbol> symbols = new HashStringMap<>(size);
        for (int i = 0; i < size; i++) {
            ELispSymbol symbol = (ELispSymbol) fory.readRef(buffer);
            symbols.put(symbol.name(), symbol);
        }
        resolver.setReadObject(id, symbols);
        return new ELispObarray(symbols);
    }

    @SuppressWarnings("rawtypes")
    public static final class HashSerializer extends Serializer<HashStringMap> {
        public HashSerializer(Fory fory) {
            super(fory, HashStringMap.class);
        }

        @Override
        public void write(MemoryBuffer buffer, HashStringMap value) {
            buffer.writeVarUint32(value.size());
            MapCursor entries = value.map().getEntries();
            while (entries.advance()) {
                fory.writeRef(buffer, entries.getKey());
                fory.writeRef(buffer, entries.getValue());
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        public HashStringMap read(MemoryBuffer buffer) {
            int size = buffer.readVarUint32();
            HashStringMap map = new HashStringMap(size);
            fory.getRefResolver().reference(map);
            for (int i = 0; i < size; i++) {
                ELispString key = (ELispString) fory.readRef(buffer);
                Object value = fory.readRef(buffer);
                map.put(key, value);
            }
            return map;
        }
    }
}
