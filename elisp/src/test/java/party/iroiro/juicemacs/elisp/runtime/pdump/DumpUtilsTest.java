package party.iroiro.juicemacs.elisp.runtime.pdump;

import org.apache.fory.Fory;
import org.apache.fory.config.Language;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.collection.CollectionSerializer;
import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispRecord;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;

public class DumpUtilsTest {
    private static Fory getFory() {
        return Fory.builder()
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
        Fory fory = getFory();
        fory.register(ELispCons.class);
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(10);
        DumpUtils.writeAnchors(fory, buffer, array);
        for (Object o : array) {
            fory.writeRef(buffer, o);
        }
        fory.reset();
        DumpUtils.readAnchors(fory, buffer, array);
        for (Object o : array) {
            assertSame(o, fory.readRef(buffer));
        }
    }

    @Test
    public void testContextArray() {
        Fory fory = Fory.builder()
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

            DumpUtils.writeContextArray(fory, buffer, sharedIndicesMap, array);
        }

        {
            SharedIndicesMap sharedIndicesMap = new SharedIndicesMap();
            SharedIndicesMap.ContextArray<ValueStorage> array = new SharedIndicesMap.ContextArray<>(
                    sharedIndicesMap,
                    ValueStorage[]::new,
                    ValueStorage::new
            );
            DumpUtils.readContextArray(fory, buffer, sharedIndicesMap, array);
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
        Fory fory = getFory();
        fory.register(FinalObject.class);
        FinalObject original = new FinalObject(100);
        byte[] bytes = fory.serialize(original);
        Object output = fory.deserialize(bytes);
        assertInstanceOf(FinalObject.class, output);
        assertEquals(original, output);
    }

    @Test
    public void testVectors() {
        Fory fory = getFory();
        fory.register(ELispVector.class);
        fory.register(ELispRecord.class);

        ELispVector v1 = new ELispVector(List.of(1L, 2L, 3L, 4L, 5L));
        ELispRecord v2 = new ELispRecord(List.of(1L, 2L, 3L, 4L, 5L));
        byte[] bytes1 = fory.serialize(v1);
        byte[] bytes2 = fory.serialize(v2);
        Object output = fory.deserialize(bytes1);
        Object output2 = fory.deserialize(bytes2);
        assertInstanceOf(ELispVector.class, output);
        assertTrue(v1.lispEquals(output));
        assertInstanceOf(ELispRecord.class, output2);
        assertTrue(v2.lispEquals(output2));
    }

    @Test
    public void testNoConstructor() {
        Fory fory = getFory();
        fory.register(FinalInt.class);
        byte[] bytes = fory.serialize(new FinalInt(new AtomicInteger(1)));
        Object output = fory.deserialize(bytes);
        FinalInt finalInt = assertInstanceOf(FinalInt.class, output);
        assertEquals(1, finalInt.getValue());
    }

    @Test
    public void testCollection() {
        Fory fory = Fory.builder()
                .withLanguage(Language.JAVA)
                .build();
        fory.register(EmptyList.class);
        fory.registerSerializer(EmptyList.class, EmptyList.EmptySerializer.class);
        fory.register(SomeRecord.class);
        SomeRecord someRecord = new SomeRecord(new EmptyList(), List.of());
        byte[] bytes = fory.serialize(someRecord);
        Object output = fory.deserialize(bytes);
        SomeRecord someRecord2 = assertInstanceOf(SomeRecord.class, output);
        assertEquals(someRecord, someRecord2);
    }

    @Disabled // TODO: upstream issue #2257
    @Test
    public void testNumberListRecord() {
        Fory fory = Fory.builder()
                .withLanguage(Language.JAVA)
                .withAsyncCompilation(true)
                .build();
        fory.register(NumberListRecord.class);
        byte[] bytes = fory.serialize(new NumberListRecord(null, new ArrayList<>()));
        fory.deserialize(bytes);
    }

    record NumberListRecord(@Nullable AutoCloseable n, List<Object> list) {
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

        public static final class EmptySerializer extends CollectionSerializer<EmptyList> {
            public EmptySerializer(Fory fory) {
                super(fory, EmptyList.class, false);
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
