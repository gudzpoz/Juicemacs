package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.Equivalence;
import org.graalvm.collections.MapCursor;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval.FFuncall;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.CustomHashTableTest;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.TruffleUtils;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.Map.Entry;
import java.util.function.BiConsumer;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public sealed class ELispHashtable extends AbstractELispIdentityObject implements Iterable<Map.Entry<Object, Object>> {

    protected final EconomicMap<Object, Object> inner;
    protected final Object weak;
    private final Object eqSymbol;

    public ELispHashtable() {
        this(EQ, false);
    }

    public ELispHashtable(Object testSym, Object weak) {
        this(getEquivalence(testSym), testSym, weak);
    }

    protected ELispHashtable(Equivalence equivalence, Object testSym, Object weak) {
        this.inner = EconomicMap.create(equivalence);
        this.eqSymbol = testSym;
        this.weak = weak;
    }

    protected static Equivalence getEquivalence(Object testSym) {
        Equivalence test;
        if (testSym == EQ) {
            test = new EqEquivalence();
        } else if (testSym == EQUAL) {
            test = new EqualEquivalence();
        } else if (isNil(testSym) || testSym == EQL) {
            // Default: eql
            test = new EqlEquivalence();
        } else {
            test = new CustomEquivalence(asSym(testSym));
        }
        return test;
    }

    @TruffleBoundary
    public void put(Object key, Object value) {
        try {
            inner.put(key, value);
        } catch (NullPointerException ignored) {
            // So Emacs allows using a custom hash function, and it chooses
            // to safeguard against the possibility that a user modifies the
            // hash table during the test.
            // Personally I don't like this safeguard-everything approach:
            // if your "hash" function is not stateless nor pure, then
            // please expect an Emacs crash.
            // So here we only do a simple, fast check.
            throw ELispSignals.error("hash table test modifies table");
        }
    }

    @TruffleBoundary
    public boolean containsKey(Object key) {
        return inner.containsKey(key);
    }

    @TruffleBoundary
    public Object get(Object key) {
        return Objects.requireNonNullElse(inner.get(key), false);
    }

    @TruffleBoundary
    public Object get(Object k, Object defaultValue) {
        return inner.get(k, defaultValue);
    }

    @TruffleBoundary
    public Object remove(Object key) {
        return Objects.requireNonNullElse(inner.removeKey(key), false);
    }

    @TruffleBoundary
    public void clear() {
        inner.clear();
    }

    public int size() {
        return inner.size();
    }

    public Object getTest() {
        return eqSymbol;
    }

    public Object getWeakness() {
        return weak;
    }

    @TruffleBoundary
    public void forEach(BiConsumer<Object, Object> action) {
        MapCursor<Object, Object> cursor = inner.getEntries();
        while (cursor.advance()) {
            action.accept(cursor.getKey(), cursor.getValue());
        }
    }

    @Override
    public Iterator<Map.Entry<Object, Object>> iterator() {
        MapCursor<Object, Object> entries = inner.getEntries();
        return new EntryIterator(entries);
    }

    @TruffleBoundary
    public static ELispHashtable hashTableFromPlist(List<Object> list, boolean readSyntax) {
        // (make-hash-table :size 65 :test eql :weakness t)
        // #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())
        int plistStart = readSyntax ? 1 : 0;
        Object testSym = getFromPseudoPlist(list, readSyntax ? TEST : CTEST, plistStart);
        testSym = testSym == null ? false : testSym;
        Object weak = getFromPseudoPlist(list, readSyntax ? WEAKNESS : CWEAKNESS, plistStart);
        weak = weak == null ? false : weak;
        ELispHashtable table;
        if (isNil(weak)) {
            table = new ELispHashtable(testSym, false);
        } else {
            table = new ELispWeakHashtable(testSym, weak);
        }
        if (readSyntax) {
            Object data = getFromPseudoPlist(list, DATA, plistStart);
            if (data != null && !isNil(data)) {
                ELispCons cons = (ELispCons) data;
                Iterator<Object> iterator = cons.iterator();
                while (iterator.hasNext()) {
                    Object key = iterator.next();
                    if (!iterator.hasNext()) {
                        throw ELispSignals.error("Hash table data is not a list of even length");
                    }
                    Object value = iterator.next();
                    table.put(key, value);
                }
            }
        }
        return table;
    }

    @Nullable
    @TruffleBoundary
    private static Object getFromPseudoPlist(List<Object> list, Object key, int start) {
        for (int i = start; i < list.size() - 1; i += 2) {
            if (list.get(i) == key) {
                return list.get(i + 1);
            }
        }
        return null;
    }

    @Override
    public void display(ELispPrint print) {
        print.print("#s(hash-table size ").printInt(size());
        if (!isNil(weak)) {
            print.sep().print("weakness ").print(weak);
        }
        print.sep().print("test ").print(eqSymbol).sep().print("data (").start(this);
        forEach((k, v) -> print.sep().print(k).sep().print(v));
        print.print("))").end();
    }

    public static final class ELispWeakHashtable extends ELispHashtable {
        private final boolean weakKey;
        private final boolean weakValue;
        private final ReferenceQueue<Object> queue = new ReferenceQueue<>();

        public ELispWeakHashtable(Object testSym, Object weak) {
            super(wrapEquivalence(getEquivalence(testSym)), testSym, isT(weak) ? KEY_AND_VALUE : weak);
            weak = super.weak;
            // Treat KEY_AND_VALUE like KEY
            weakKey = weak == KEY_AND_VALUE || weak == KEY_OR_VALUE || weak == KEY;
            weakValue = weak == KEY_OR_VALUE || weak == VALUE;
            if (!weakKey && !weakValue) {
                throw ELispSignals.error(TruffleUtils.concat("Invalid weakness argument: ", weak));
            }
        }

        private static Equivalence wrapEquivalence(Equivalence inner) {
            return new Equivalence() {
                @Override
                public boolean equals(Object a, Object b) {
                    if (a == b) {
                        return true;
                    }
                    if (a instanceof WeakKey key) {
                        a = key.get();
                    }
                    if (b instanceof WeakKey key) {
                        b = key.get();
                    }
                    return a != null && b != null && inner.equals(a, b);
                }

                @Override
                public int hashCode(Object o) {
                    return o instanceof WeakKey key ? key.innerHash : inner.hashCode(o);
                }
            };
        }

        @Override
        public Object get(Object key) {
            purge();
            Object value = super.get(key);
            if (value instanceof WeakReference<?> reference) {
                value = reference.get();
                if (value == null) {
                    purge();
                    return false;
                }
                return value;
            }
            return value;
        }

        @Override
        public Object get(Object k, Object defaultValue) {
            Object o = super.get(k, defaultValue);
            if (o instanceof WeakValue v) {
                o = v.get();
            }
            return Objects.requireNonNullElse(o, defaultValue);
        }

        @Override
        @TruffleBoundary
        public void put(Object key, Object value) {
            purge();
            if (weakKey) {
                key = new WeakKey(key, queue, inner.getEquivalenceStrategy().hashCode(key));
            }
            if (weakValue) {
                value = new WeakValue(value, key, queue);
            }
            super.put(key, value);
        }

        @Override
        public int size() {
            purge();
            return super.size();
        }

        @TruffleBoundary
        @Override
        public void forEach(BiConsumer<Object, Object> action) {
            MapCursor<Object, Object> cursor = inner.getEntries();
            while (cursor.advance()) {
                Object key = cursor.getKey();
                Object value = cursor.getValue();
                if (key instanceof WeakKey reference) {
                    key = reference.get();
                }
                if (value instanceof WeakValue reference) {
                    value = reference.get();
                }
                if (key != null && value != null) {
                    action.accept(key, value);
                } else {
                    cursor.remove();
                }
            }
        }

        @TruffleBoundary
        private void purge() {
            Reference<?> poll = queue.poll();
            while (poll != null) {
                if (poll instanceof WeakKey key) {
                    super.remove(key);
                } else {
                    super.remove(((WeakValue) poll).key);
                }
                poll = queue.poll();
            }
        }

        @Nullable
        static Object pruneWeakWrapper(Object weak) {
            return switch (weak) {
                case WeakKey key -> key.get();
                case WeakValue value -> value.get();
                default -> weak;
            };
        }

        private static final class WeakKey extends WeakReference<Object> {
            private final int innerHash;

            WeakKey(Object referent, ReferenceQueue<Object> queue, int innerHash) {
                super(referent, queue);
                this.innerHash = innerHash;
            }

            @Override
            public int hashCode() {
                return innerHash;
            }
        }

        private static final class WeakValue extends WeakReference<Object> {
            private final Object key;

            WeakValue(Object referent, Object key, ReferenceQueue<Object> queue) {
                super(referent, queue);
                this.key = key;
            }
        }
    }

    private static class EqlEquivalence extends Equivalence {
        @Override
        public boolean equals(Object a, Object b) {
            return BuiltInFns.FEql.eql(a, b);
        }

        @Override
        @TruffleBoundary
        public int hashCode(Object o) {
            if (o instanceof Double d) {
                return Long.hashCode(Double.doubleToRawLongBits(d));
            }
            return o.hashCode();
        }
    }

    private static class EqualEquivalence extends Equivalence {
        @Override
        public boolean equals(Object a, Object b) {
            return BuiltInFns.FEqual.equal(a, b);
        }

        @Override
        public int hashCode(Object o) {
            return ELispValue.lispHashCode(o, 0);
        }
    }

    private static class EqEquivalence extends Equivalence {
        @Override
        public boolean equals(Object a, Object b) {
            return BuiltInData.FEq.eq(a, b);
        }

        @Override
        @TruffleBoundary
        public int hashCode(Object o) {
            return o.hashCode();
        }
    }

    private static class CustomEquivalence extends Equivalence {
        final CustomHashTableTest test;

        // TODO: disallow editing the hashtable from eq/hash functions

        CustomEquivalence(ELispSymbol testSym) {
            BuiltInFns fns = ELispContext.get(null).globals().builtInFns;
            CustomHashTableTest test = fns.getHashTableTest(testSym);
            if (test == null) {
                throw ELispSignals.error("Invalid hash table test");
            }
            this.test = test;
        }

        @Override
        public boolean equals(Object a, Object b) {
            return !isNil(FFuncall.funcall(null, test.eq(), a, b));
        }

        @Override
        public int hashCode(Object o) {
            return (int) asLong(FFuncall.funcall(null, test.hash(), o));
        }
    }

    private static class EntryIterator implements Iterator<Entry<Object, Object>> {
        private final MapCursor<Object, Object> entries;
        boolean hasNext = false;
        Object key = false;
        Object value = false;

        public EntryIterator(MapCursor<Object, Object> entries) {
            this.entries = entries;
            advance();
        }

        @Override
        public boolean hasNext() {
            return hasNext;
        }

        @TruffleBoundary
        @Override
        public Entry<Object, Object> next() {
            if (!hasNext) {
                throw new NoSuchElementException();
            }
            SimpleEntry<Object, Object> entry = new SimpleEntry<>(key, value);
            advance();
            return entry;
        }

        @TruffleBoundary
        private void advance() {
            Object key = null;
            Object value = null;
            while ((key == null || value == null) && (hasNext = entries.advance())) {
                key = ELispWeakHashtable.pruneWeakWrapper(entries.getKey());
                value = ELispWeakHashtable.pruneWeakWrapper(entries.getValue());
            }
            if (hasNext) {
                this.key = assertNotNull(key);
                this.value = assertNotNull(value);
            }
        }
    }
}
