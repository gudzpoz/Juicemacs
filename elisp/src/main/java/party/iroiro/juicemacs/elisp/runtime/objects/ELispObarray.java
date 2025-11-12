package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.MapCursor;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.Map;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.EQUAL;

public final class ELispObarray extends AbstractELispIdentityObject implements ELispValue {
    private final HashStringMap<ELispSymbol> symbols;

    public ELispObarray() {
        this(new HashStringMap<>());
    }

    public ELispObarray(HashStringMap<ELispSymbol> symbols) {
        this.symbols = symbols;
    }

    public HashStringMap<ELispSymbol> symbols() {
        return symbols;
    }

    @TruffleBoundary
    public ELispSymbol intern(ELispString name) {
        ELispSymbol symbol = internSoft(name);
        if (symbol == null) {
            symbol = new ELispSymbol(name);
            symbols.put(name, symbol);
        }
        return symbol;
    }

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    public ELispSymbol intern(String name) {
        return intern(ELispString.ofJava(name));
    }

    @TruffleBoundary
    @Nullable
    public ELispSymbol internSoft(ELispString value) {
        return symbols.get(value);
    }

    @TruffleBoundary
    @Nullable
    public ELispSymbol unintern(ELispString name) {
        return symbols.removeKey(name);
    }

    @Override
    public void display(ELispPrint print) {
        print.print(toString());
    }

    @Override
    public String toString() {
        return "#<obarray@" + Integer.toHexString(System.identityHashCode(this)) + ">";
    }

    /// A utility hash map, using [ELispString#lispEquals(Object)] instead of [Object#equals(Object)]
    public record HashStringMap<T>(EconomicMap<ELispString, T> map) {
        public HashStringMap() {
            this(16);
        }
        @TruffleBoundary
        public HashStringMap(int capacity) {
            this(EconomicMap.create(ELispHashtable.getEquivalence(EQUAL), capacity));
        }
        @TruffleBoundary
        public HashStringMap(Map<ELispString, T> map) {
            this(EconomicMap.create(ELispHashtable.getEquivalence(EQUAL), map.size()));
            map.forEach(this::put);
        }
        @Nullable
        @SuppressWarnings("DataFlowIssue") // this is definitely nullable
        @TruffleBoundary
        public T get(ELispString key) {
            return map.get(key);
        }
        @TruffleBoundary
        public void put(ELispString key, T value) {
            map.put(key, value);
        }
        @Nullable
        @SuppressWarnings("DataFlowIssue") // this is definitely nullable
        public T removeKey(ELispString key) {
            return map.removeKey(key);
        }
        @TruffleBoundary
        public boolean removeValue(T value) {
            MapCursor<ELispString, T> cursor = map.getEntries();
            while (cursor.advance()) {
                if (cursor.getValue().equals(value)) {
                    cursor.remove();
                    return true;
                }
            }
            return false;
        }
        @TruffleBoundary
        public int size() {
            return map.size();
        }
        @TruffleBoundary
        public void clear() {
            map.clear();
        }
        @TruffleBoundary
        public Object valueList() {
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            MapCursor<ELispString, T> cursor = map.getEntries();
            while (cursor.advance()) {
                builder.add(cursor.getValue());
            }
            return builder.build();
        }
    }
}
