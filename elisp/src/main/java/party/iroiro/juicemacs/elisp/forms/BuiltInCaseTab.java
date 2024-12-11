package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

import java.util.List;
import java.util.function.BiConsumer;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCharTab.charTableMap;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInCaseTab extends ELispBuiltIns {
    public static void initCasetabOnce() {
        BuiltInFns.FPut.put(CASE_TABLE, CHAR_TABLE_EXTRA_SLOTS, 3L);

        ELispCharTable asciiDownCase = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
        ELispCharTable asciiUpCase = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
        ELispCharTable asciiEqv = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
        for (int i = 0; i < 128; i++) {
            boolean up = 'A' <= i && i <= 'Z';
            boolean down = 'a' <= i && i <= 'z';
            asciiDownCase.setChar(i, (long) (up ? i + ('a' - 'A') : i));
            long toUpper = down ? i + ('A' - 'a') : i;
            asciiUpCase.setChar(i, toUpper);
            asciiEqv.setChar(i, up ? (long) (i + ('a' - 'A')) : toUpper);
        }
        asciiDownCase.setExtra(0, asciiUpCase);
        asciiDownCase.setExtra(1, BuiltInFns.FCopySequence.copySequenceCharTable(asciiDownCase));
        asciiDownCase.setExtra(2, asciiEqv);
        asciiDowncaseTable = asciiDownCase;
        asciiUpcaseTable = asciiUpCase;
        asciiEqvTable = asciiEqv;
        setCaseTable(asciiDownCase, true);
    }

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCaseTabFactory.getFactories();
    }

    public static ELispCharTable asciiDowncaseTable;
    public static ELispCharTable asciiUpcaseTable;
    public static ELispCharTable asciiCanonTable;
    public static ELispCharTable asciiEqvTable;

    /// Sets elements of `table` for `c` to `c` itself.
    private record SetIdentity(ELispCharTable table) implements BiConsumer<Object, Object> {
        @Override
        public void accept(Object range, Object value) {
            if (value instanceof Long) {
                int from, to;
                if (range instanceof ELispCons cons) {
                    from = asInt(cons.car());
                    to = asInt(cons.cdr());
                } else {
                    from = to = asInt(range);
                }
                to++;
                for (; from < to; from++) {
                    table.setChar(from, (long) from);
                }
            }
        }
    }

    /// Permutes the elements of `table` so that it has one cycle for each equivalence class
    /// induced by the translation table on which [BuiltInCharTab#charTableMap(ELispCharTable, BiConsumer)]
    /// is called.
    private record Shuffle(ELispCharTable table) implements BiConsumer<Object, Object> {
        @Override
        public void accept(Object key, Object value) {
            if (value instanceof Long elt) {
                int from, to;
                if (key instanceof ELispCons cons) {
                    from = asInt(cons.car());
                    to = asInt(cons.cdr());
                } else {
                    from = to = asInt(key);
                }
                to++;
                for (; from < to; from++) {
                    int c = elt.intValue();
                    Object tem = table.getChar(c);
                    table.setChar(c, (long) from);
                    table.setChar(from, tem);
                }
            }
        }
    }

    /// Sets elements of the canon table (of `caseTable`) in `range` to a translated `value`
    /// by `up` table, if `value` is a character.
    private record SetCanon(ELispCharTable caseTable) implements BiConsumer<Object, Object> {
        @Override
        public void accept(Object range, Object value) {
            ELispCharTable up = asCharTable(caseTable.getExtra(0));
            ELispCharTable canon = asCharTable(caseTable.getExtra(1));
            if (value instanceof Long elt) {
                BuiltInCharTab.FSetCharTableRange.setCharTableRange(
                        canon,
                        range,
                        caseTable.getChar(asInt(up.getChar(elt.intValue())))
                );
            }
        }
    }

    private static ELispCharTable setCaseTable(ELispCharTable caseTable, boolean standard) {
        if (!FCaseTableP.caseTableP(caseTable)) {
            throw ELispSignals.wrongTypeArgument(CASE_TABLE_P, caseTable);
        }
        Object up = caseTable.getExtra(0);
        Object canon = caseTable.getExtra(1);
        Object eqv = caseTable.getExtra(2);
        if (isNil(up)) {
            ELispCharTable upTable = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
            up = upTable;
            charTableMap(caseTable, new SetIdentity(upTable));
            charTableMap(caseTable, new Shuffle(upTable));
            caseTable.setExtra(0, up);
        }
        if (isNil(canon)) {
            canon = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
            caseTable.setExtra(1, canon);
            charTableMap(caseTable, new SetCanon(caseTable));
        }
        if (isNil(eqv)) {
            ELispCharTable eqvTable = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
            eqv = eqvTable;
            ELispCharTable canonTable = asCharTable(canon);
            charTableMap(canonTable, new SetIdentity(eqvTable));
            charTableMap(canonTable, new Shuffle(eqvTable));
            caseTable.setExtra(2, eqv);
        }
        asCharTable(canon).setExtra(2, eqv);
        if (standard) {
            asciiDowncaseTable = caseTable;
            asciiUpcaseTable = asCharTable(up);
            asciiEqvTable = asCharTable(eqv);
            asciiCanonTable = asCharTable(canon);
        } else {
            ELispBuffer buffer = asBuffer(CURRENT_BUFFER.getValue());
            buffer.setDowncaseTable(caseTable);
            buffer.setUpcaseTable(up);
            buffer.setCaseCanonTable(canon);
            buffer.setCaseEqvTable(eqv);
        }
        return caseTable;
    }

    /**
     * <pre>
     * Return t if OBJECT is a case table.
     * See `set-case-table' for more information on these data structures.
     * </pre>
     */
    @ELispBuiltIn(name = "case-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCaseTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean caseTableP(Object object) {
            if (!(object instanceof ELispCharTable charTable)) {
                return false;
            }
            if (charTable.getPurpose() != CASE_TABLE) {
                return false;
            }
            Object up = charTable.getExtra(0);
            Object canon = charTable.getExtra(1);
            Object eqv = charTable.getExtra(2);
            return (isNil(up) || BuiltInData.FCharTableP.charTableP(up))
            && (isNil(canon) && isNil(eqv)
                    || FCaseTableP.caseTableP(canon)
                    && (isNil(eqv) || FCaseTableP.caseTableP(eqv)));
        }
    }

    /**
     * <pre>
     * Return the case table of the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "current-case-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentCaseTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentCaseTable() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the standard case table.
     * This is the one used for new buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "standard-case-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FStandardCaseTable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCharTable standardCaseTable() {
            return asciiDowncaseTable;
        }
    }

    /**
     * <pre>
     * Select a new case table for the current buffer.
     * A case table is a char-table which maps characters
     * to their lower-case equivalents.  It also has three \"extra\" slots
     * which may be additional char-tables or nil.
     * These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
     * UPCASE maps each non-upper-case character to its upper-case equivalent.
     *  (The value in UPCASE for an upper-case character is never used.)
     *  If lower and upper case characters are in 1-1 correspondence,
     *  you may use nil and the upcase table will be deduced from DOWNCASE.
     * CANONICALIZE maps each character to a canonical equivalent;
     *  any two characters that are related by case-conversion have the same
     *  canonical equivalent character; it may be nil, in which case it is
     *  deduced from DOWNCASE and UPCASE.
     * EQUIVALENCES is a map that cyclically permutes each equivalence class
     *  (of characters with the same canonical equivalent); it may be nil,
     *  in which case it is deduced from CANONICALIZE.
     * </pre>
     */
    @ELispBuiltIn(name = "set-case-table", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetCaseTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setCaseTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Select a new standard case table for new buffers.
     * See `set-case-table' for more info on case tables.
     * </pre>
     */
    @ELispBuiltIn(name = "set-standard-case-table", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetStandardCaseTable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCharTable setStandardCaseTable(ELispCharTable table) {
            return setCaseTable(table, true);
        }
    }
}
