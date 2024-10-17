package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.EconomicMapUtil;
import org.graalvm.collections.Equivalence;
import org.graalvm.collections.MapCursor;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.*;
import java.util.function.BiConsumer;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public final class ELispHashtable implements ELispValue {

    private final EconomicMap<Object, Object> inner;
    private final Object eqSymbol;

    public ELispHashtable() {
        this(EQ);
    }

    public ELispHashtable(Object testSym) {
        this.eqSymbol = testSym;
        Equivalence test;
        if (testSym == EQL) {
            test = new Equivalence() {
                @Override
                public boolean equals(Object a, Object b) {
                    return BuiltInFns.FEql.eql(a, b);
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
                    return o.hashCode();
                }
            };
        } else {
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
        }
        this.inner = EconomicMap.create(test);
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
    public void remove(Object key) {
        inner.removeKey(key);
    }

    public int size() {
        return inner.size();
    }

    @CompilerDirectives.TruffleBoundary
    public void forEach(BiConsumer<Object, Object> action) {
        MapCursor<Object, Object> cursor = inner.getEntries();
        while (cursor.advance()) {
            action.accept(cursor.getKey(), cursor.getValue());
        }
    }

    @CompilerDirectives.TruffleBoundary
    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispHashtable t && eqSymbol == t.eqSymbol && EconomicMapUtil.equals(inner, t.inner);
    }

    @CompilerDirectives.TruffleBoundary
    public static ELispHashtable hashTableFromPlist(List<Object> list) {
        // #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())
        Object testSym = getFromPseudoPlist(list, TEST);
        ELispHashtable table = new ELispHashtable(Objects.requireNonNullElse(testSym, false));
        Object data = getFromPseudoPlist(list, DATA);
        if (data != null && !ELispSymbol.isNil(data)) {
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
        return table;
    }

    @Nullable
    private static Object getFromPseudoPlist(List<Object> list, Object key) {
        for (int i = 1; i < list.size() - 1; i += 2) {
            if (list.get(i) == key) {
                return list.get(i + 1);
            }
        }
        return null;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder("#s(hash-table");
        builder.append(" size ").append(size());
        builder.append(" test ").append(eqSymbol);
        builder.append(" data (");
        forEach((k, v) -> builder.append(ELispValue.display(k)).append(' ').append(ELispValue.display(v)).append(' '));
        if (size() != 0) {
            builder.deleteCharAt(builder.length() - 1);
        }
        builder.append("))");
        return builder.toString();
    }
}
