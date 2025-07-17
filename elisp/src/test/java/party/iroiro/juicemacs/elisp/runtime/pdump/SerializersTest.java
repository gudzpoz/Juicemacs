package party.iroiro.juicemacs.elisp.runtime.pdump;

import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fury.Fury;
import org.apache.fury.config.Language;
import org.apache.fury.memory.MemoryBuffer;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.array.ELispConsLocation;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.EQ;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;

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

        ELispCons cyclic = ELispCons.cons(false, false);
        cyclic.setCar(cyclic);
        cyclic.setCdr(cyclic);
        restored = roundTrip(cyclic);
        assertSame(restored, restored.car());
        assertSame(restored, restored.cdr());

        ELispCons referred = ELispCons.cons(1.0, 2.0);
        ELispCons tailRef = (ELispCons) new ELispCons.ListBuilder()
                .add(referred)
                .add(referred)
                .add(3L)
                .buildWithCdr(referred);
        restored = roundTrip(tailRef);
        assertTrue(tailRef.lispEquals(restored));
        assertSame(restored.car(), restored.get(1));
        assertSame(restored.car(), restored.listIterator(2).currentCons());
    }

    @Test
    public void testConsWithDebug() {
        ELispCons simple = ELispCons.cons(true, true);
        simple.setSourceLocation(1, 2, 3, 4);
        ELispCons restored = roundTrip(simple);
        assertTrue(restored.lispEquals(simple));
        ELispConsLocation expected = simple.getLocation();
        ELispConsLocation actual = restored.getLocation();
        assertEquals(expected, actual);

        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (int i = 0; i < 10; i++) {
            builder.add((long) i);
        }
        ELispCons list = (ELispCons) builder.build();
        ELispCons.ConsIterator i = list.listIterator(0);
        while (i.hasNextCons()) {
            i.nextCons().setSourceLocation(1, 2, 3, 4);
        }
        ELispCons listRestored = roundTrip(list);
        assertTrue(listRestored.lispEquals(list));
        i = listRestored.listIterator(0);
        while (i.hasNextCons()) {
            ELispCons cons = i.nextCons();
            assertEquals(
                    new ELispConsLocation(1, 2, 3, 4),
                    cons.getLocation()
            );
        }
    }

    @Test
    public void testHashtable() {
        ELispHashtable hashtable = new ELispHashtable();
        hashtable.put(1L, 2L);
        hashtable.put(3L, 4L);

        Fury fury = getFury();
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(4096);
        DumpUtils.writeAnchor(fury, buffer, EQ);
        DumpUtils.writeAnchor(fury, buffer, NIL);
        DumpUtils.writeAnchor(fury, buffer, Boolean.FALSE);
        fury.serialize(buffer, hashtable);
        int expectedSize = buffer.writerIndex();

        DumpUtils.readAnchor(fury, buffer, EQ);
        DumpUtils.readAnchor(fury, buffer, NIL);
        DumpUtils.readAnchor(fury, buffer, Boolean.FALSE);
        Object deserializedObject = fury.deserialize(buffer);
        assertEquals(expectedSize, buffer.readerIndex());

        ELispHashtable restored = (ELispHashtable) deserializedObject;
        assertEquals(hashtable.size(), restored.size());
        assertEquals(2L, restored.get(1L));
        assertEquals(4L, restored.get(3L));
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
