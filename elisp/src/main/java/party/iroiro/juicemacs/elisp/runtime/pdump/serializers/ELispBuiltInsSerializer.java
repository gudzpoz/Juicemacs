package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIns;

public class ELispBuiltInsSerializer extends Serializer<ELispBuiltIns[]> {
    public ELispBuiltInsSerializer(Fory fory) {
        super(fory, ELispBuiltIns[].class);
    }

    @Override
    public void write(MemoryBuffer buffer, ELispBuiltIns[] value) {
        buffer.writeInt32(value.length);

        for (ELispBuiltIns builtIn : value) {
            fory.writeRef(buffer, builtIn);
        }
    }

    @Override
    public ELispBuiltIns[] read(MemoryBuffer buffer) {
        int length = buffer.readInt32();
        ELispBuiltIns[] builtIns = new ELispBuiltIns[length];
        fory.getRefResolver().reference(builtIns);
        for (int i = 0; i < length; i++) {
            builtIns[i] = (ELispBuiltIns) fory.readRef(buffer);
        }
        return builtIns;
    }
}
