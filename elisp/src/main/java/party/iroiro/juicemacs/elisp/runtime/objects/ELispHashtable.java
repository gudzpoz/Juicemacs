package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.Equivalence;
import org.graalvm.collections.MapCursor;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.*;
import java.util.function.BiConsumer;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isT;

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
            test = new Equivalence() {
                @Override
                public boolean equals(Object a, Object b) {
                    return BuiltInData.FEq.eq(a, b);
                }
                @Override
                public int hashCode(Object o) {
                    return o.hashCode();
                }
            };
        } else if (testSym == EQUAL) {
            test = new Equivalence() {
                @Override
                public boolean equals(Object a, Object b) {
                    return BuiltInFns.FEqual.equal(a, b);
                }
                @Override
                public int hashCode(Object o) {
                    return ELispValue.lispHashCode(o);
                }
            };
        } else {
            // Default: eql
            test = new Equivalence() {
                @Override
                public boolean equals(Object a, Object b) {
                    return BuiltInFns.FEql.eql(a, b);
                }
                @Override
                public int hashCode(Object o) {
                    if (o instanceof Double d) {
                        return Long.hashCode(Double.doubleToRawLongBits(d));
                    }
                    return o.hashCode();
                }
            };
        }
        return test;
    }

    @CompilerDirectives.TruffleBoundary
    public void put(Object key, Object value) {
        inner.put(key, value);
    }

    @CompilerDirectives.TruffleBoundary
    public boolean containsKey(Object key) {
        return inner.containsKey(key);
    }

    @CompilerDirectives.TruffleBoundary
    public Object get(Object key) {
        return Objects.requireNonNullElse(inner.get(key), false);
    }

    @CompilerDirectives.TruffleBoundary
    public Object get(Object k, Object defaultValue) {
        return inner.get(k, defaultValue);
    }

    @CompilerDirectives.TruffleBoundary
    public Object remove(Object key) {
        return Objects.requireNonNullElse(inner.removeKey(key), false);
    }

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

    @CompilerDirectives.TruffleBoundary
    public void forEach(BiConsumer<Object, Object> action) {
        MapCursor<Object, Object> cursor = inner.getEntries();
        while (cursor.advance()) {
            action.accept(cursor.getKey(), cursor.getValue());
        }
    }

    @Override
    public Iterator<Map.Entry<Object, Object>> iterator() {
        MapCursor<Object, Object> entries = inner.getEntries();
        return new Iterator<>() {
            boolean hasNext = advance();
            Object key = false;
            Object value = false;

            @Override
            public boolean hasNext() {
                return hasNext;
            }

            @Override
            public Map.Entry<Object, Object> next() {
                if (!hasNext) {
                    throw new NoSuchElementException();
                }
                AbstractMap.SimpleEntry<Object, Object> entry = new AbstractMap.SimpleEntry<>(key, value);
                hasNext = advance();
                return entry;
            }

            private boolean advance() {
                boolean hasNext = false;
                @Nullable Object key = null;
                @Nullable Object value = null;
                while ((key == null || value == null) && (hasNext = entries.advance())) {
                    key = ELispWeakHashtable.pruneWeakWrapper(entries.getKey());
                    value = ELispWeakHashtable.pruneWeakWrapper(entries.getValue());
                }
                if (hasNext) {
                    this.key = key;
                    this.value = value;
                }
                return hasNext;
            }
        };
    }


    @CompilerDirectives.TruffleBoundary
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

    private static final class ELispWeakHashtable extends ELispHashtable {
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
                throw ELispSignals.error("Invalid weakness argument: " + weak);
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

        @CompilerDirectives.TruffleBoundary
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

        @CompilerDirectives.TruffleBoundary
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

            public WeakKey(Object referent, ReferenceQueue<Object> queue, int innerHash) {
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

            public WeakValue(Object referent, Object key, ReferenceQueue<Object> queue) {
                super(referent, queue);
                this.key = key;
            }
        }
    }
}
