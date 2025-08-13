package party.iroiro.juicemacs.elisp.runtime.pdump;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.resolver.RefResolver;
import org.apache.fory.serializer.Serializer;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

public abstract class DumpUtils {
    private DumpUtils() {
    }

    private static final int CONTEXT_ARRAY_START = 0xCAFEFEED;
    private static final int CONTEXT_ARRAY_END = 0xCAFEDEAD;

    private static final MemoryBuffer DUMMY = MemoryBuffer.newHeapBuffer(10);

    /// Set up the writer so that some objects can map to existing ones
    ///
    /// ## Example usages
    ///
    /// In [party.iroiro.juicemacs.elisp.runtime.ELispGlobals],
    /// for example, symbols are declared as `static final`.
    /// We want to serialize the references to them and point those
    /// references "back" during deserialization.
    ///
    /// Or in [party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer],
    /// we serialize the whole marker tree into marker position to avoid
    /// exposing the internals of the marker tree (that confuses the serializer).
    /// However, elisp markers objects store those internal nodes inside
    /// them, and when we restore them, we want the tree and the elisp markers
    /// to share the same instance of nodes.
    ///
    /// @see #readAnchors(Fory, MemoryBuffer, Object[])
    public static <T> void writeAnchors(Fory fory, MemoryBuffer output, T[] array) {
        if (array.length == 0) {
            return;
        }
        RefResolver resolver = fory.getRefResolver();
        int recentId = -1;
        for (T value : array) {
            int id = getAnchor(value, resolver);
            assert recentId == -1 || recentId + 1 == id;
            recentId = id;
        }
        int startId = recentId - array.length + 1;
        output.grow(Integer.BYTES * 4);
        output.writeVarUint32(startId);
        output.writeVarUint32(array.length);
    }

    private static int getAnchor(Object value, RefResolver resolver) {
        MemoryBuffer dummy = DUMMY;
        dummy.writerIndex(0);
        boolean isNewValue = resolver.writeRefValueFlag(dummy, value);
        boolean isRefWritten = !resolver.writeRefValueFlag(dummy, value);
        assert isNewValue && isRefWritten;
        dummy.readerIndex(2);
        return dummy.readVarUint32();
    }

    /// @see #writeAnchors(Fory, MemoryBuffer, Object[])
    public static <T> void readAnchors(Fory fory, MemoryBuffer input, T[] array) {
        int startId = input.readVarUint32();
        int length = input.readVarUint32();
        RefResolver resolver = fory.getRefResolver();
        for (int i = 0; i < length; i++) {
            if (resolver.preserveRefId() != startId + i) {
                throw ELispSignals.fatal("fory internal error");
            }
            resolver.reference(array[i]);
        }
    }

    public static <T> void writeAnchor(Fory fory, MemoryBuffer output, T item) {
        RefResolver resolver = fory.getRefResolver();
        int id = getAnchor(item, resolver);
        output.writeVarUint32(id);
    }

    public static <T> void readAnchor(Fory fory, MemoryBuffer input, T item) {
        int id = input.readVarUint32();
        RefResolver resolver = fory.getRefResolver();
        if (resolver.preserveRefId() != id) {
            throw ELispSignals.fatal("fory internal error");
        }
        resolver.reference(item);
    }

    /// @see #readContextArray(Fory, MemoryBuffer, SharedIndicesMap, SharedIndicesMap.ContextArray)
    @TruffleBoundary
    public static <T> void writeContextArray(
            Fory fory, MemoryBuffer output,
            SharedIndicesMap indicesMap,
            SharedIndicesMap.ContextArray<T> array
    ) {
        ConcurrentHashMap.KeySetView<ELispSymbol, Integer> symbols = indicesMap.keySet();
        output.writeInt32(CONTEXT_ARRAY_START);
        try (CounterSlot slot = CounterSlot.record(output)) {
            for (ELispSymbol symbol : symbols) {
                int index = indicesMap.lookup(symbol);
                if (array.contains(index)) {
                    fory.writeRef(output, symbol);
                    fory.writeRef(output, array.getDynamic(index));
                    slot.inc();
                }
            }
        }
        output.writeInt32(CONTEXT_ARRAY_END);
    }

    /// @see #writeContextArray(Fory, MemoryBuffer, SharedIndicesMap, SharedIndicesMap.ContextArray)
    @SuppressWarnings("unchecked")
    public static <T> int readContextArray(
            Fory fory, MemoryBuffer input,
            SharedIndicesMap indicesMap,
            SharedIndicesMap.ContextArray<T> array
    ) {
        int magic1 = input.readInt32();
        if (magic1 == CONTEXT_ARRAY_START) {
            int size = input.readInt32();
            for (int i = 0; i < size; i++) {
                ELispSymbol symbol = (ELispSymbol) fory.readRef(input);
                T value = (T) fory.readRef(input);
                int index = indicesMap.lookup(symbol);
                array.addIfAbsent(index, value);
            }
            int magic2 = input.readInt32();
            if (magic2 == CONTEXT_ARRAY_END) {
                return size;
            }
        }
        throw ELispSignals.fatal("corrupt context array magic");
    }

    public static <T> Serializer<T> never(Fory fory, Class<T> clazz) {
        return new NeverSerializer<>(fory, clazz);
    }

    public static <T> Serializer<T> stateless(Fory fory, Class<T> clazz, Supplier<T> supplier) {
        return new Serializer<>(fory, clazz) {
            @Override
            public void write(MemoryBuffer buffer, T value) {
                // no-op
            }

            @Override
            public T read(MemoryBuffer buffer) {
                return supplier.get();
            }
        };
    }

    /// Writes the item count after going over all items
    public static final class CounterSlot implements AutoCloseable {
        private final MemoryBuffer buffer;
        private final int offset;
        private int count = 0;

        CounterSlot(MemoryBuffer buffer, int offset) {
            this.buffer = buffer;
            this.offset = offset;
        }

        public static CounterSlot record(MemoryBuffer buffer) {
            CounterSlot slot = new CounterSlot(buffer, buffer.writerIndex());
            buffer.writeInt32(-1);
            return slot;
        }

        public void inc() {
            count++;
        }

        @Override
        public void close() {
            int now = buffer.writerIndex();
            buffer.writerIndex(offset);
            buffer.writeInt32(count);
            buffer.writerIndex(now);
        }
    }

    private static final class NeverSerializer<T> extends Serializer<T> {
        public NeverSerializer(Fory fory, Class<T> type) {
            super(fory, type);
        }

        @Override
        @TruffleBoundary
        public void write(MemoryBuffer buffer, T value) {
            throw new IllegalStateException("references to " + getType() + " leaked through");
        }

        @Override
        @TruffleBoundary
        public T read(MemoryBuffer buffer) {
            throw new IllegalStateException("references to " + getType() + " leaked through");
        }
    }
}
