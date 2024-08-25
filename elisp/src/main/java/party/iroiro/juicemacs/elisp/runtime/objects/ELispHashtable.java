package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

import java.util.*;
import java.util.function.BiPredicate;

public final class ELispHashtable implements ELispValue {

    private final HashMap<ELispHashtable.ELispHashtableKey, Object> inner;

    public ELispHashtable() {
        this(Objects::equals);
    }

    public ELispHashtable(BiPredicate<Object, Object> eq) {
        this.eq = eq;
        this.inner = new HashMap<>();
    }

    public void put(Object key, Object value) {
        inner.put(new ELispHashtableKey(key), value);
    }

    public boolean containsKey(Object key) {
        return inner.containsKey(new ELispHashtableKey(key));
    }

    public Object get(Object key) {
        return inner.get(new ELispHashtableKey(key));
    }

    public void remove(Object key) {
        inner.remove(new ELispHashtableKey(key));
    }

    public int size() {
        return inner.size();
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

    public static ELispHashtable hashTableFromPlist(ELispContext context, List<Object> list) {
        // #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())
        Object testSym = getFromPseudoPlist(list, context.TEST);
        BiPredicate<Object, Object> test = null;
        if (testSym == context.EQ) {
            test = Objects::equals;
        } else if (testSym == context.EQL) {
            // TODO: Implement eql
        } else if (testSym == context.EQUAL) {
            // TODO: Implement equal
        }
        ELispHashtable table = test == null ? new ELispHashtable() : new ELispHashtable(test);
        Object data = getFromPseudoPlist(list, context.DATA);
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
    public String type() {
        return "hash-table";
    }
}
