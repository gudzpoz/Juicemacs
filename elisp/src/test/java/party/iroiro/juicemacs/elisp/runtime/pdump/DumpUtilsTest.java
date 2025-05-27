package party.iroiro.juicemacs.elisp.runtime.pdump;

import org.apache.fury.Fury;
import org.apache.fury.config.Language;
import org.apache.fury.memory.MemoryBuffer;
import org.apache.fury.serializer.collection.AbstractCollectionSerializer;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispRecord;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.utils.IntArrayList;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;

public class DumpUtilsTest {
    private static Fury getFury() {
        return Fury.builder()
                .withLanguage(Language.JAVA)
                .withRefTracking(true)
                .build();
    }

    @Test
    public void testDump() {
        Object[] array = new Object[]{
                new Object(),
                new Object[0],
                ELispCons.listOf(1L, 2L),
        };
        Fury fury = getFury();
        fury.register(ELispCons.class);
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(10);
        DumpUtils.writeAnchors(fury, buffer, array);
        for (Object o : array) {
            fury.writeRef(buffer, o);
        }
        fury.reset();
        DumpUtils.readAnchors(fury, buffer, array);
        for (Object o : array) {
            assertSame(o, fury.readRef(buffer));
        }
    }

    @Test
    public void testContextArray() {
        Fury fury = Fury.builder()
                .withLanguage(Language.JAVA)
                .requireClassRegistration(false)
                .withRefTracking(true)
                .build();

        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(10);

        {
            SharedIndicesMap sharedIndicesMap = new SharedIndicesMap();
            SharedIndicesMap.ContextArray<ValueStorage> array = new SharedIndicesMap.ContextArray<>(
                    sharedIndicesMap,
                    ValueStorage[]::new,
                    ValueStorage::new
            );
            int index = sharedIndicesMap.lookup(NIL);
            array.getDynamic(index);

            DumpUtils.writeContextArray(fury, buffer, sharedIndicesMap, array);
        }

        {
            SharedIndicesMap sharedIndicesMap = new SharedIndicesMap();
            SharedIndicesMap.ContextArray<ValueStorage> array = new SharedIndicesMap.ContextArray<>(
                    sharedIndicesMap,
                    ValueStorage[]::new,
                    ValueStorage::new
            );
            DumpUtils.readContextArray(fury, buffer, sharedIndicesMap, array);
        }
    }

    @Test
    public void testCopyTo() {
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(10);
        for (int i = 0; i < 10; i++) {
            buffer.writeByte(16 * i + i);
        }
        buffer.grow(1);
        buffer.copyTo(0, buffer, 1, 10);
        assertEquals(0, buffer.readByte());
        for (int i = 0; i < 10; i++) {
            assertEquals((byte) (i * 16 + i), buffer.readByte());
        }
    }

    @Test
    public void testFinalFields() {
        Fury fury = getFury();
        fury.register(FinalObject.class);
        FinalObject original = new FinalObject(100);
        byte[] bytes = fury.serialize(original);
        Object output = fury.deserialize(bytes);
        assertInstanceOf(FinalObject.class, output);
        assertEquals(original, output);
    }

    @Test
    public void testVectors() {
        Fury fury = getFury();
        fury.register(ELispVector.class);
        fury.register(ELispRecord.class);

        ELispVector v1 = new ELispVector(List.of(1L, 2L, 3L, 4L, 5L));
        ELispRecord v2 = new ELispRecord(List.of(1L, 2L, 3L, 4L, 5L));
        byte[] bytes1 = fury.serialize(v1);
        byte[] bytes2 = fury.serialize(v2);
        Object output = fury.deserialize(bytes1);
        Object output2 = fury.deserialize(bytes2);
        assertInstanceOf(ELispVector.class, output);
        assertTrue(v1.lispEquals(output));
        assertInstanceOf(ELispRecord.class, output2);
        assertTrue(v2.lispEquals(output2));
    }

    @Test
    public void testNoConstructor() {
        Fury fury = getFury();
        fury.register(FinalInt.class);
        byte[] bytes = fury.serialize(new FinalInt(new AtomicInteger(1)));
        Object output = fury.deserialize(bytes);
        FinalInt finalInt = assertInstanceOf(FinalInt.class, output);
        assertEquals(1, finalInt.getValue());
    }

    @Test
    public void testCollection() {
        Fury fury = Fury.builder()
                .withLanguage(Language.JAVA)
                .build();
        fury.register(EmptyList.class);
        fury.registerSerializer(EmptyList.class, EmptyList.EmptySerializer.class);
        fury.register(SomeRecord.class);
        SomeRecord someRecord = new SomeRecord(new EmptyList(), List.of());
        byte[] bytes = fury.serialize(someRecord);
        Object output = fury.deserialize(bytes);
        SomeRecord someRecord2 = assertInstanceOf(SomeRecord.class, output);
        assertEquals(someRecord, someRecord2);
    }

    @Disabled // TODO: upstream issue #2257
    @Test
    public void testNumberListRecord() {
        Fury fury = Fury.builder()
                .withLanguage(Language.JAVA)
                .withAsyncCompilation(true)
                .build();
        fury.register(NumberListRecord.class);
        fury.register(IntArrayList.class);
        byte[] bytes = fury.serialize(new NumberListRecord(null, new ArrayList<>()));
        fury.deserialize(bytes);
    }

    record NumberListRecord(AutoCloseable n, List<Object> list) {
    }

    record FinalObject(int value) {
    }

    private static final class FinalInt {
        private final int value;

        public FinalInt(AtomicInteger value) {
            this.value = value.get();
        }

        public int getValue() {
            return value;
        }
    }

    public static final class EmptyList extends AbstractList<Object> {
        @Override
        public Object get(int index) {
            throw new IndexOutOfBoundsException();
        }
        @Override
        public int size() {
            return 0;
        }

        public static final class EmptySerializer extends AbstractCollectionSerializer<EmptyList> {
            public EmptySerializer(Fury fury) {
                super(fury, EmptyList.class, false);
            }
            @Override
            public void write(MemoryBuffer buffer, EmptyList value) {
                // no-op
            }
            @Override
            public EmptyList read(MemoryBuffer buffer) {
                return new EmptyList();
            }
            @Override
            public Collection<?> onCollectionWrite(MemoryBuffer buffer, EmptyList value) {
                throw new UnsupportedOperationException();
            }
            @Override
            public EmptyList onCollectionRead(Collection collection) {
                throw new UnsupportedOperationException();
            }
        }
    }
    public record SomeRecord(EmptyList specialList, List<Object> normalList) {
    }
}
