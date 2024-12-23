package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.List;
import java.util.function.BiConsumer;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInCharTab extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharTabFactory.getFactories();
    }

    public static void charTableMap(ELispCharTable table, BiConsumer<Object, Object> callback) {
        table.map(new ELispCharTable.MapConsumer<Void>() {
            long prevChar = 0L;
            Object prev = false;

            @Override
            public Void accept(int codepoint, Object o) {
                if (BuiltInData.FEq.eq(prev, o)) {
                    return null; // NOPMD
                }
                if (!isNil(prev)) {
                    if (prevChar == codepoint - 1) {
                        callback.accept(prevChar, prev);
                    } else {
                        callback.accept(new ELispCons(prevChar, (long) codepoint - 1), prev);
                    }
                }
                prevChar = codepoint;
                prev = o;
                return null; // NOPMD
            }
        }, 0);
    }

    /**
     * <pre>
     * Return a newly created char-table, with purpose PURPOSE.
     * Each element is initialized to INIT, which defaults to nil.
     *
     * PURPOSE should be a symbol.  If it has a `char-table-extra-slots'
     * property, the property's value should be an integer between 0 and 10
     * that specifies how many extra slots the char-table has.  Otherwise,
     * the char-table has no extra slot.
     * </pre>
     */
    @ELispBuiltIn(name = "make-char-table", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCharTable makeCharTable(ELispSymbol purpose, Object init) {
            Object slotProp = BuiltInFns.FGet.get(purpose, CHAR_TABLE_EXTRA_SLOTS);
            int extraSlots = isNil(slotProp) ? 0 : asInt(slotProp);
            if (extraSlots < 0 || 10 < extraSlots) {
                throw ELispSignals.argsOutOfRange(extraSlots);
            }
            ELispCharTable table = ELispCharTable.create(init, purpose, extraSlots);
            table.setParent(false);
            return table;
        }
    }

    /**
     * <pre>
     * Return the subtype of char-table CHAR-TABLE.  The value is a symbol.
     * </pre>
     */
    @ELispBuiltIn(name = "char-table-subtype", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharTableSubtype extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol charTableSubtype(ELispCharTable charTable) {
            return charTable.getPurpose();
        }
    }

    /**
     * <pre>
     * Return the parent char-table of CHAR-TABLE.
     * The value is either nil or another char-table.
     * If CHAR-TABLE holds nil for a given character,
     * then the actual applicable value is inherited from the parent char-table
     * \(or from its parents, if necessary).
     * </pre>
     */
    @ELispBuiltIn(name = "char-table-parent", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharTableParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableParent(ELispCharTable charTable) {
            return charTable.getParent();
        }
    }

    /**
     * <pre>
     * Set the parent char-table of CHAR-TABLE to PARENT.
     * Return PARENT.  PARENT must be either nil or another char-table.
     * </pre>
     */
    @ELispBuiltIn(name = "set-char-table-parent", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetCharTableParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableParent(ELispCharTable charTable, Object parent) {
            charTable.setParent(parent);
            return parent;
        }
    }

    /**
     * <pre>
     * Return the value of CHAR-TABLE's extra-slot number N.
     * </pre>
     */
    @ELispBuiltIn(name = "char-table-extra-slot", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharTableExtraSlot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charTableExtraSlot(ELispCharTable charTable, long n) {
            return charTable.getExtra((int) n);
        }
    }

    /**
     * <pre>
     * Set CHAR-TABLE's extra-slot number N to VALUE.
     * </pre>
     */
    @ELispBuiltIn(name = "set-char-table-extra-slot", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetCharTableExtraSlot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableExtraSlot(ELispCharTable charTable, long n, Object value) {
            charTable.setExtra((int) n, value);
            return value;
        }
    }

    /**
     * <pre>
     * Return the value in CHAR-TABLE for a range of characters RANGE.
     * RANGE should be nil (for the default value),
     * a cons of character codes (for characters in the range), or a character code.
     * If RANGE is a cons (FROM . TO), the function returns the value for FROM.
     * </pre>
     */
    @ELispBuiltIn(name = "char-table-range", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharTableRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charTableRange(Object charTable, Object range) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.
     * RANGE should be t (for all characters), nil (for the default value),
     * a cons of character codes (for characters in the range),
     * or a character code.  Return VALUE.
     * </pre>
     */
    @ELispBuiltIn(name = "set-char-table-range", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetCharTableRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharTableRange(ELispCharTable charTable, Object range, Object value) {
            if (isT(range)) {
                charTable.setAll(value);
            } else if (isNil(range)) {
                charTable.setDefault(value);
            } else if (range instanceof Long l) {
                charTable.setChar(asInt(l), value);
            } else if (range instanceof ELispCons cons) {
                charTable.setRange(asInt(cons.car()), asInt(cons.cdr()), value);
            } else {
                throw ELispSignals.error("Invalid RANGE argument");
            }
            return value;
        }
    }

    /**
     * <pre>
     * Optimize CHAR-TABLE.
     * TEST is the comparison function used to decide whether two entries are
     * equivalent and can be merged.  It defaults to `equal'.
     * </pre>
     */
    @ELispBuiltIn(name = "optimize-char-table", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOptimizeCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean optimizeCharTable(ELispCharTable charTable, Object test) {
            charTable.optimize(
                    (isNil(test) || test == EQUAL)
                            ? BuiltInFns.FEqual::equal
                            : (test == EQ
                            ? BuiltInData.FEq::eq
                            : (a, b) -> !isNil(BuiltInEval.FFuncall.funcall(test, new Object[]{a, b})))
            );
            return false;
        }
    }

    /**
     * <pre>
     * Call FUNCTION for each character in CHAR-TABLE that has non-nil value.
     * FUNCTION is called with two arguments, KEY and VALUE.
     * KEY is a character code or a cons of character codes specifying a
     * range of characters that have the same value.
     * VALUE is what (char-table-range CHAR-TABLE KEY) returns.
     * </pre>
     */
    @ELispBuiltIn(name = "map-char-table", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapCharTable extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean mapCharTable(Object function, ELispCharTable charTable) {
            charTableMap(charTable, (key, value) -> BuiltInEval.FFuncall.funcall(function, new Object[]{key, value}));
            return false;
        }
    }

    /**
     * <pre>
     * Return a char-table for Unicode character property PROP.
     * Use `get-unicode-property-internal' and
     * `put-unicode-property-internal' instead of `aref' and `aset' to get
     * and put an element value.
     * </pre>
     */
    @ELispBuiltIn(name = "unicode-property-table-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUnicodePropertyTableInternal extends ELispBuiltInBaseNode {
        @Specialization
        public Object unicodePropertyTableInternal(Object prop) {
            ELispContext context = getContext();
            Object val = BuiltInFns.FAssq.assq(prop, CHAR_CODE_PROPERTY_ALIST.getValue());
            if (!(val instanceof ELispCons cons)) {
                return false;
            }
            if (cons.cdr() instanceof ELispString path) {
                // TODO: Uniprop decoder
                @Nullable ELispRootNode rootNode = BuiltInLRead.loadFile(
                        getLanguage(),
                        new ELispString(MuleString.concat(
                                MuleString.fromString("international/"),
                                path.value())),
                        false
                );
                if (rootNode == null) {
                    return false;
                }
                rootNode.getCallTarget().call();
            }
            if (!(cons.cdr() instanceof ELispCharTable table)
                    || table.getPurpose() != CHAR_CODE_PROPERTY_TABLE || table.extraSlots() != 5) {
                return false;
            }
            Object count = table.getExtra(1);
            if (count instanceof Long l
                    ? l < 0 || l > 1
                    : !isNil(count)) {
                return false;
            }
            return table;
        }
    }

    /**
     * <pre>
     * Return an element of CHAR-TABLE for character CH.
     * CHAR-TABLE must be what returned by `unicode-property-table-internal'.
     * </pre>
     */
    @ELispBuiltIn(name = "get-unicode-property-internal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetUnicodePropertyInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getUnicodePropertyInternal(Object charTable, Object ch) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set an element of CHAR-TABLE for character CH to VALUE.
     * CHAR-TABLE must be what returned by `unicode-property-table-internal'.
     * </pre>
     */
    @ELispBuiltIn(name = "put-unicode-property-internal", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPutUnicodePropertyInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void putUnicodePropertyInternal(Object charTable, Object ch, Object value) {
            throw new UnsupportedOperationException();
        }
    }
}
