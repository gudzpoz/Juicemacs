package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;

public final class CyclicAssumptionSerializer extends Serializer<CyclicAssumption> {
    public CyclicAssumptionSerializer(Fury fury) {
        super(fury, CyclicAssumption.class);
    }

    @Override
    public void write(MemoryBuffer buffer, CyclicAssumption value) {
        fury.writeJavaStringRef(buffer, value.getAssumption().getName());
    }

    @Override
    public CyclicAssumption read(MemoryBuffer buffer) {
        return new CyclicAssumption(fury.readJavaStringRef(buffer));
    }
}
