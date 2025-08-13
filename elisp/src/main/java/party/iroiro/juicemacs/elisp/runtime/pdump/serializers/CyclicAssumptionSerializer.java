package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;

public final class CyclicAssumptionSerializer extends Serializer<CyclicAssumption> {
    public CyclicAssumptionSerializer(Fory fory) {
        super(fory, CyclicAssumption.class);
    }

    @Override
    public void write(MemoryBuffer buffer, CyclicAssumption value) {
        fory.writeJavaStringRef(buffer, value.getAssumption().getName());
    }

    @Override
    public CyclicAssumption read(MemoryBuffer buffer) {
        return new CyclicAssumption(fory.readJavaStringRef(buffer));
    }
}
