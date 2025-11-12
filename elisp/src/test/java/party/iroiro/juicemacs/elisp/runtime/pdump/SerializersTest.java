package party.iroiro.juicemacs.elisp.runtime.pdump;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fory.Fory;
import org.apache.fory.config.Language;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.ExternalizableSerializer;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical.Captured;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical.Scope;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.array.ELispConsSerializer;
import party.iroiro.juicemacs.elisp.runtime.array.SourceLocation;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.objects.AbstractELispClosure.ClosureCommons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer.MarkerMoveNotifier;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable.CompressedUnipropSubTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable.SubTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispHashtable.ELispWeakHashtable;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.*;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.PieceTreeSerializer.Intervals;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.PieceTreeSerializer.Marks;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ThreadLocalStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage.*;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.Marker;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.JAVA_SOURCE;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.EQ;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;

public class SerializersTest {
    private Fory getFory() {
        Fory fory = Fory.builder()
                .withLanguage(Language.JAVA)
                .requireClassRegistration(false)
                .withRefTracking(true)
                .build();
        // Symbols
        fory.register(TruffleString.class);
        fory.register(MutableTruffleString.class);
        fory.registerSerializer(ELispObarray.class, new ELispObarraySerializer(fory));
        fory.registerSerializer(ELispSymbol.class, new ELispSymbolSerializer(fory));
        // Symbol values
        fory.registerSerializer(ThreadLocalStorage.class,
                DumpUtils.stateless(fory, ThreadLocalStorage.class, () -> new ThreadLocalStorage(false)));
        fory.registerSerializer(ValueStorage.class, new ExternalizableSerializer<>(fory, ValueStorage.class));
        fory.register(PlainValue.class);
        fory.register(Forwarded.class);
        fory.register(ForwardedBool.class);
        fory.register(ForwardedLong.class);
        fory.register(ForwardedPerKboard.class);
        fory.register(ForwardedPerBuffer.class);
        fory.register(VarAlias.class);
        fory.register(BufferLocal.class);
        fory.register(TrappedWrite.class);
        fory.register(FunctionStorage.class);

        // Objects
        fory.registerSerializer(ELispHashtable.class, new ELispHashtableSerializer(fory));
        fory.registerSerializer(ELispWeakHashtable.class, new ELispHashtableSerializer(fory));
        fory.registerSerializer(ELispCons.class, new ELispConsSerializer(fory));
        fory.register(ELispString.class);
        fory.register(ELispCharTable.class);
        fory.register(SubTable.class);
        fory.register(CompressedUnipropSubTable.class);
        fory.register(ELispBoolVector.class);
        fory.register(ELispVector.class);
        fory.register(ELispRecord.class);
        fory.register(ELispInterpretedClosure.class);
        fory.register(ELispBytecode.class);

        // Buffers
        fory.register(ELispBuffer.class);
        fory.register(MarkerMoveNotifier.class);
        fory.registerSerializer(ELispMarker.class, new ELispMarkerSerializer(fory));
        fory.registerSerializer(Marker.class, DumpUtils.never(fory, Marker.class));
        fory.registerSerializer(PieceTreeBase.class, new PieceTreeSerializer(fory));
        fory.registerSerializer(IntervalPieceTree.class, new Intervals(fory));
        fory.registerSerializer(MarkerPieceTree.class, new Marks(fory));

        // Lexical
        fory.register(ELispLexical.class);
        fory.register(Scope.class);
        fory.register(Captured.class);
        fory.register(ClosureCommons.class);

        // Truffle internals
        fory.register(FrameDescriptor.class);
        fory.registerSerializer(CyclicAssumption.class, new CyclicAssumptionSerializer(fory));
        fory.registerSerializer(Assumption.class, DumpUtils.never(fory, Assumption.class));
        Class<? extends MaterializedFrame> frameClass = Truffle.getRuntime()
                .createMaterializedFrame(new Object[0]).getClass(); // FrameWithoutBoxing
        fory.registerSerializer(frameClass, new MaterializedFrameSerializer(fory));
        fory.registerSerializer(ELispSubroutine.class, DumpUtils.never(fory, ELispSubroutine.class));
        fory.registerSerializer(JAVA_SOURCE.getClass(), new SourceSerializer(fory)); // SourceImpl
        return fory;
    }

    @SuppressWarnings("unchecked")
    private <T> T roundTrip(T object) {
        Class<? extends T> clazz = (Class<? extends T>) object.getClass();
        Fory fory = getFory();
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(4096);
        fory.serialize(buffer, object);
        int expectedSize = buffer.writerIndex();
        Object deserializedObject = fory.deserialize(buffer);
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
        assertSame(restored.car(), restored.listIterator(3).currentCons());
    }

    @Test
    public void testConsWithDebug() {
        ELispCons simple = ELispCons.cons(true, true);
        simple.setSourceLocation(1, 2, 3, 4);
        ELispCons restored = roundTrip(simple);
        assertTrue(restored.lispEquals(simple));
        SourceLocation expected = simple.getLocation();
        SourceLocation actual = restored.getLocation();
        assertEquals(expected, actual);

        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (int i = 0; i < 10; i++) {
            builder.add((long) i);
        }
        ELispCons list = (ELispCons) builder.build();
        ConsIterator i = list.listIterator(0);
        while (i.hasNextCons()) {
            i.nextCons().setSourceLocation(1, 2, 3, 4);
        }
        ELispCons listRestored = roundTrip(list);
        assertTrue(listRestored.lispEquals(list));
        i = listRestored.listIterator(0);
        while (i.hasNextCons()) {
            ELispCons cons = i.nextCons();
            assertEquals(
                    new SourceLocation(1, 2, 3, 4),
                    cons.getLocation()
            );
        }
    }

    @Test
    public void testHashtable() {
        ELispHashtable hashtable = new ELispHashtable();
        hashtable.put(1L, 2L);
        hashtable.put(3L, 4L);

        Fory fory = getFory();
        MemoryBuffer buffer = MemoryBuffer.newHeapBuffer(4096);
        DumpUtils.writeAnchor(fory, buffer, EQ);
        DumpUtils.writeAnchor(fory, buffer, NIL);
        DumpUtils.writeAnchor(fory, buffer, Boolean.FALSE);
        fory.serialize(buffer, hashtable);
        int expectedSize = buffer.writerIndex();

        DumpUtils.readAnchor(fory, buffer, EQ);
        DumpUtils.readAnchor(fory, buffer, NIL);
        DumpUtils.readAnchor(fory, buffer, Boolean.FALSE);
        Object deserializedObject = fory.deserialize(buffer);
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
        ELispObarray obarray = new ELispObarray(new ELispObarray.HashStringMap<>(0));
        obarray.intern("test1");
        obarray.intern("test2");

        ELispObarray restored = roundTrip(obarray);
        assertEquals(obarray.symbols().size(), restored.symbols().size());
        assertNotNull(restored.internSoft(ELispString.ofJava("test1")));
        assertNotNull(restored.internSoft(ELispString.ofJava("test2")));
    }
}
