package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.BiPredicate;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public final class ELispHashtable implements ELispValue {

    private final HashMap<ELispHashtable.ELispHashtableKey, Object> inner;
    private final Object eqSymbol;

    public ELispHashtable() {
        this(EQ);
    }

    public ELispHashtable(Object testSym) {
        this.eqSymbol = testSym;
        BiPredicate<Object, Object> test;
        if (testSym == EQ) {
            test = BuiltInData.FEq::eq;
        } else if (testSym == EQL) {
            test = BuiltInFns.FEql::eql;
        } else if (testSym == EQUAL) {
            test = BuiltInFns.FEqual::equal;
        } else {
            test = BuiltInData.FEq::eq;
        }
        this.eq = test;
        this.inner = new HashMap<>();
    }

    @CompilerDirectives.TruffleBoundary
    public void put(Object key, Object value) {
        inner.put(new ELispHashtableKey(key), value);
    }

    @CompilerDirectives.TruffleBoundary
    public boolean containsKey(Object key) {
        return inner.containsKey(new ELispHashtableKey(key));
    }

    @CompilerDirectives.TruffleBoundary
    public Object get(Object key) {
        Object o = inner.get(new ELispHashtableKey(key));
        return o == null ? false : o;
    }

    @CompilerDirectives.TruffleBoundary
    public Object get(Object k, Object defaultValue) {
        return inner.getOrDefault(new ELispHashtableKey(k), defaultValue);
    }

    @CompilerDirectives.TruffleBoundary
    public void remove(Object key) {
        inner.remove(new ELispHashtableKey(key));
    }

    public int size() {
        return inner.size();
    }

    @CompilerDirectives.TruffleBoundary
    public void forEach(BiConsumer<Object, Object> action) {
        inner.forEach((k, v) -> action.accept(k.key, v));
    }

    @CompilerDirectives.TruffleBoundary
    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispHashtable t && eq == t.eq && inner.equals(t.inner);
    }

    protected final class ELispHashtableKey {
        private final Object key;

        private ELispHashtableKey(Object key) {
            this.key = key;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof ELispHashtableKey other && eq.test(key, other.key);
        }

        @Override
        public int hashCode() {
            return key.hashCode();
        }
    }

    public final BiPredicate<Object, Object> eq;

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
                    throw new IllegalArgumentException();
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
