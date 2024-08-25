package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.List;

public class BuiltInCharTab extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharTabFactory.getFactories();
    }

    @ELispBuiltIn(name = "make-char-table", minArgs = 1, maxArgs = 2, doc = "Return a newly created char-table, with purpose PURPOSE.\nEach element is initialized to INIT, which defaults to nil.\n\nPURPOSE should be a symbol.  If it has a `char-table-extra-slots'\nproperty, the property's value should be an integer between 0 and 10\nthat specifies how many extra slots the char-table has.  Otherwise,\nthe char-table has no extra slot.")
    @GenerateNodeFactory
    public abstract static class FMakeCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public Object makeCharTable(Object purpose, Object init) {
            if (!ELispSymbol.isSymbol(purpose)) {
                throw new IllegalArgumentException();
            }
            int extraSlots;
            if (purpose instanceof ELispSymbol symbol) {
                extraSlots = (int) (long) (Long) BuiltInFns.FGet.get(symbol, ctx().CHAR_TABLE_EXTRA_SLOTS);
                if (extraSlots < 0 || 10 < extraSlots) {
                    throw new IllegalArgumentException();
                }
            } else {
                extraSlots = 0;
            }
            ELispCharTable table = new ELispCharTable(init, extraSlots);
            table.setParent(false);
            table.setPurpose(purpose);
            return table;
        }
    }

    @ELispBuiltIn(name = "char-table-subtype", minArgs = 1, maxArgs = 1, doc = "Return the subtype of char-table CHAR-TABLE.  The value is a symbol.")
    @GenerateNodeFactory
    public abstract static class FCharTableSubtype extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableSubtype(ELispCharTable table) {
            return table.getPurpose();
        }
    }

    @ELispBuiltIn(name = "char-table-parent", minArgs = 1, maxArgs = 1, doc = "Return the parent char-table of CHAR-TABLE.\nThe value is either nil or another char-table.\nIf CHAR-TABLE holds nil for a given character,\nthen the actual applicable value is inherited from the parent char-table\n\\(or from its parents, if necessary).")
    @GenerateNodeFactory
    public abstract static class FCharTableParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableParent(ELispCharTable table) {
            return table.getParent();
        }
    }

    @ELispBuiltIn(name = "set-char-table-parent", minArgs = 2, maxArgs = 2, doc = "Set the parent char-table of CHAR-TABLE to PARENT.\nReturn PARENT.  PARENT must be either nil or another char-table.")
    @GenerateNodeFactory
    public abstract static class FSetCharTableParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableParent(ELispCharTable table, Object parent) {
            table.setParent(parent);
            return parent;
        }
    }

    @ELispBuiltIn(name = "char-table-extra-slot", minArgs = 2, maxArgs = 2, doc = "Return the value of CHAR-TABLE's extra-slot number N.")
    @GenerateNodeFactory
    public abstract static class FCharTableExtraSlot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableExtraSlot(ELispCharTable table, Long n) {
            return table.getExtra((int) (long) n);
        }
    }

    @ELispBuiltIn(name = "set-char-table-extra-slot", minArgs = 3, maxArgs = 3, doc = "Set CHAR-TABLE's extra-slot number N to VALUE.")
    @GenerateNodeFactory
    public abstract static class FSetCharTableExtraSlot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableExtraSlot(ELispCharTable table, Long n, Object value) {
            table.setExtra((int) (long) n, value);
            return value;
        }
    }

    @ELispBuiltIn(name = "char-table-range", minArgs = 2, maxArgs = 2, doc = "Return the value in CHAR-TABLE for a range of characters RANGE.\nRANGE should be nil (for the default value),\na cons of character codes (for characters in the range), or a character code.\nIf RANGE is a cons (FROM . TO), the function returns the value for FROM.")
    @GenerateNodeFactory
    public abstract static class FCharTableRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableRange(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-char-table-range", minArgs = 3, maxArgs = 3, doc = "Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.\nRANGE should be t (for all characters), nil (for the default value),\na cons of character codes (for characters in the range),\nor a character code.  Return VALUE.")
    @GenerateNodeFactory
    public abstract static class FSetCharTableRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableRange(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "optimize-char-table", minArgs = 1, maxArgs = 2, doc = "Optimize CHAR-TABLE.\nTEST is the comparison function used to decide whether two entries are\nequivalent and can be merged.  It defaults to `equal'.")
    @GenerateNodeFactory
    public abstract static class FOptimizeCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object optimizeCharTable(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "map-char-table", minArgs = 2, maxArgs = 2, doc = "Call FUNCTION for each character in CHAR-TABLE that has non-nil value.\nFUNCTION is called with two arguments, KEY and VALUE.\nKEY is a character code or a cons of character codes specifying a\nrange of characters that have the same value.\nVALUE is what (char-table-range CHAR-TABLE KEY) returns.")
    @GenerateNodeFactory
    public abstract static class FMapCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapCharTable(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unicode-property-table-internal", minArgs = 1, maxArgs = 1, doc = "Return a char-table for Unicode character property PROP.\nUse `get-unicode-property-internal' and\n`put-unicode-property-internal' instead of `aref' and `aset' to get\nand put an element value.")
    @GenerateNodeFactory
    public abstract static class FUnicodePropertyTableInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unicodePropertyTableInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-unicode-property-internal", minArgs = 2, maxArgs = 2, doc = "Return an element of CHAR-TABLE for character CH.\nCHAR-TABLE must be what returned by `unicode-property-table-internal'.")
    @GenerateNodeFactory
    public abstract static class FGetUnicodePropertyInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getUnicodePropertyInternal(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "put-unicode-property-internal", minArgs = 3, maxArgs = 3, doc = "Set an element of CHAR-TABLE for character CH to VALUE.\nCHAR-TABLE must be what returned by `unicode-property-table-internal'.")
    @GenerateNodeFactory
    public abstract static class FPutUnicodePropertyInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object putUnicodePropertyInternal(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }
}
