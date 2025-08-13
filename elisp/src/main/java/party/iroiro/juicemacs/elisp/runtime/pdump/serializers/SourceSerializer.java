package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.source.Source;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;

import java.net.URI;

public final class SourceSerializer extends Serializer<Source> {
    public SourceSerializer(Fory fory) {
        super(fory, Source.class);
    }

    @Override
    public void write(MemoryBuffer buffer, Source value) {
        String language = value.getLanguage();
        if (value.isInternal()) {
            assert language.equals("java");
        } else {
            assert language.equals("elisp");
        }
        buffer.writeBoolean(value.isInternal());
        fory.writeJavaString(buffer, value.getName());
        fory.writeJavaString(buffer, value.getURI().toString());
    }

    @Override
    @TruffleBoundary
    public Source read(MemoryBuffer buffer) {
        boolean internal = buffer.readBoolean();
        String name = fory.readJavaString(buffer);
        String uri = fory.readJavaString(buffer);
        Source source = Source.newBuilder(internal ? "java" : "elisp", "", name)
                .content(Source.CONTENT_NONE)
                .internal(internal)
                .uri(URI.create(uri))
                .build();
        fory.getRefResolver().reference(source);
        return source;
    }
}
