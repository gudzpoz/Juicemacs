package party.iroiro.juicemacs.elisp.runtime.pdump.serializers;

import com.oracle.truffle.api.source.Source;
import org.apache.fury.Fury;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.Serializer;

import java.net.URI;

public final class SourceSerializer extends Serializer<Source> {
    public SourceSerializer(Fury fury) {
        super(fury, Source.class);
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
        fury.writeJavaString(buffer, value.getName());
        fury.writeJavaString(buffer, value.getURI().toString());
    }

    @Override
    public Source read(MemoryBuffer buffer) {
        boolean internal = buffer.readBoolean();
        String name = fury.readJavaString(buffer);
        String uri = fury.readJavaString(buffer);
        Source source = Source.newBuilder(internal ? "java" : "elisp", "", name)
                .content(Source.CONTENT_NONE)
                .internal(internal)
                .uri(URI.create(uri))
                .build();
        fury.getRefResolver().reference(source);
        return source;
    }
}
