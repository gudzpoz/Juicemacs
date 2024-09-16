package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.CHAR_TABLE_EXTRA_SLOTS;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

public class BuiltInCharTab extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharTabFactory.getFactories();
    }

    @ELispBuiltIn(name = "make-char-table", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeCharTable(ELispSymbol purpose, Object init) {
            Object slotProp = BuiltInFns.FGet.get(purpose, CHAR_TABLE_EXTRA_SLOTS);
            int extraSlots = ELispSymbol.isNil(slotProp) ? 0 : (int) (long) slotProp;
            if (extraSlots < 0 || 10 < extraSlots) {
                throw new IllegalArgumentException();
            }
            ELispCharTable table = new ELispCharTable(init, extraSlots);
            table.setParent(NIL);
            table.setPurpose(purpose);
            return table;
        }
    }

    @ELispBuiltIn(name = "char-table-subtype", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharTableSubtype extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol charTableSubtype(ELispCharTable table) {
            return table.getPurpose();
        }
    }

    @ELispBuiltIn(name = "char-table-parent", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharTableParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableParent(ELispCharTable table) {
            return table.getParent();
        }
    }

    @ELispBuiltIn(name = "set-char-table-parent", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetCharTableParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableParent(ELispCharTable table, Object parent) {
            table.setParent(parent);
            return parent;
        }
    }

    @ELispBuiltIn(name = "char-table-extra-slot", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharTableExtraSlot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableExtraSlot(ELispCharTable table, long n) {
            return table.getExtra((int) n);
        }
    }

    @ELispBuiltIn(name = "set-char-table-extra-slot", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetCharTableExtraSlot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableExtraSlot(ELispCharTable table, long n, Object value) {
            table.setExtra((int) n, value);
            return value;
        }
    }

    @ELispBuiltIn(name = "char-table-range", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharTableRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableRange(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-char-table-range", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetCharTableRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableRange(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "optimize-char-table", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOptimizeCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object optimizeCharTable(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "map-char-table", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapCharTable(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unicode-property-table-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUnicodePropertyTableInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unicodePropertyTableInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-unicode-property-internal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetUnicodePropertyInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getUnicodePropertyInternal(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "put-unicode-property-internal", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPutUnicodePropertyInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object putUnicodePropertyInternal(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }
}
