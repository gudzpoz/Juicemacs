package party.iroiro.juicemacs.elisp.runtime.pdump;

import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fury.Fury;
import org.apache.fury.config.Language;
import org.apache.fury.memory.MemoryBuffer;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.HashMap;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class SerializersTest {
    private Fury getFury() {
        Fury fury = Fury.builder()
                .withLanguage(Language.JAVA)
                .requireClassRegistration(false)
                .withRefTracking(true)
                .build();
        ELispPortableDumper.registerSerializers(fury);
        return fury;
    }

    @SuppressWarnings("unchecked")
    private <T> T roundTrip(T object) {
        Class<? extends T> clazz = (Class<? extends T>) object.getClass();
        Fury fury = getFury();
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(4096);
        fury.serialize(buffer, object);
        int expectedSize = buffer.writerIndex();
        Object deserializedObject = fury.deserialize(buffer);
        assertEquals(expectedSize, buffer.readerIndex());
        return clazz.cast(deserializedObject);
    }

    @Test
    public void testAssumption() {
        CyclicAssumption assumption = new CyclicAssumption("test");
        CyclicAssumption restored = roundTrip(assumption);
        assertEquals(assumption.getAssumption().getName(), restored.getAssumption().getName());
    }

    @SuppressWarnings("CollectionAddedToSelf")
    @Test
    public void testELispCons() {
        ELispCons simple = (ELispCons) new ELispCons.ListBuilder()
                .add(1L)
                .add(2L)
                .add(3L)
                .buildWithCdr(1.0);
        ELispCons restored = roundTrip(simple);
        assertTrue(simple.lispEquals(restored));

        ELispCons cyclic = new ELispCons(false, false);
        cyclic.setCar(cyclic);
        cyclic.setCdr(cyclic);
        restored = roundTrip(cyclic);
        assertSame(restored, restored.car());
        assertSame(restored, restored.cdr());

        ELispCons referred = new ELispCons(1.0, 2.0);
        ELispCons tailRef = (ELispCons) new ELispCons.ListBuilder()
                .add(referred)
                .add(referred)
                .add(3L)
                .buildWithCdr(referred);
        restored = roundTrip(tailRef);
        assertTrue(tailRef.lispEquals(restored));
        assertSame(restored.car(), restored.get(1));
        assertSame(restored.car(), restored.getCons(2).cdr());
    }

    @Test
    public void testHashtable() {
        ELispHashtable hashtable = new ELispHashtable();
        hashtable.put(1L, 2L);
        hashtable.put(3L, 4L);
        ELispHashtable restored = roundTrip(hashtable);
        assertEquals(hashtable.size(), restored.size());
        assertEquals(2L, restored.get(1L));
        assertEquals(4L, restored.get(3L));
    }

    @Test
    public void testELispLexical() {
        ELispLexical.Allocator allocator = new ELispLexical.Allocator();
        ELispLexical lexical = ELispLexical.newRoot();
        lexical.addVariable(allocator, new ELispSymbol("arg1"));
        lexical.addVariable(allocator, new ELispSymbol("arg2"));
        lexical = lexical.fork();
        lexical.addVariable(allocator, new ELispSymbol("var1"));
        lexical.addVariable(allocator, new ELispSymbol("var2"));
        MaterializedFrame frame = Truffle.getRuntime().createMaterializedFrame(new Object[0], ELispLexical.frameDescriptor(true));
        lexical = lexical.fork().withParentFrame(frame);
        lexical.addVariable(allocator, new ELispSymbol("var3"));

        ELispLexical restored = roundTrip(lexical);
        while (restored != null && lexical != null) {
            assertEquals(
                    lexical.materializedParent() == null,
                    restored.materializedParent() == null
            );
            assertEquals(
                    lexical.parentScope() == null,
                    restored.parentScope() == null
            );

            IntArrayList expectedSlots = lexical.slots();
            IntArrayList slots = restored.slots();
            assertEquals(expectedSlots, slots);

            List<ELispSymbol> expectedSymbols = lexical.symbols();
            List<ELispSymbol> symbols = restored.symbols();
            assertEquals(expectedSlots.size(), symbols.size());
            for (int i = 0; i < expectedSlots.size(); i++) {
                assertEquals(expectedSymbols.get(i).name(), symbols.get(i).name());
            }

            lexical = lexical.parentScope();
            restored = restored.parentScope();
        }
    }

    @Test
    public void testELispMarker() {
        ELispMarker marker = new ELispMarker();
        ELispMarker restored = roundTrip(marker);
        assertNull(marker.getBuffer());
        assertNull(restored.getBuffer());
        assertTrue(marker.getInner().isDetached());
        assertTrue(restored.getInner().isDetached());

        marker.setBuffer(new ELispBuffer(new Object[1024]), 1);
        restored = roundTrip(marker);
        assertNotNull(restored.getBuffer());
        assertEquals(1, restored.point());
    }

    @Test
    public void testELispObarray() {
        ELispObarray obarray = new ELispObarray(new HashMap<>());
        obarray.intern("test1");
        obarray.intern("test2");

        ELispObarray restored = roundTrip(obarray);
        assertEquals(obarray.symbols().size(), restored.symbols().size());
        assertNotNull(restored.internSoft(MuleString.fromString("test1")));
        assertNotNull(restored.internSoft(MuleString.fromString("test2")));
    }
}
