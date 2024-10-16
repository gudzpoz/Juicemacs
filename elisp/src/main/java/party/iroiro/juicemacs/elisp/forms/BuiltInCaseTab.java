package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public class BuiltInCaseTab extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
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

        return BuiltInCaseTabFactory.getFactories();
    }

    private static ELispCharTable asciiDowncaseTable;
    private static ELispCharTable asciiUpcaseTable;
    private static ELispCharTable asciiCanonTable;
    private static ELispCharTable asciiEqvTable;

    private static ELispCharTable asCharTable(Object object) {
        if (object instanceof ELispCharTable charTable) {
            return charTable;
        }
        throw ELispSignals.wrongTypeArgument(CHAR_TABLE_P, object);
    }

    private void setCaseTable(ELispCharTable caseTable, boolean standard) {
        if (!FCaseTableP.caseTableP(caseTable)) {
            throw ELispSignals.wrongTypeArgument(CASE_TABLE_P, caseTable);
        }
        Object up = caseTable.getExtra(0);
        Object canon = caseTable.getExtra(1);
        Object eqv = caseTable.getExtra(2);
        if (ELispSymbol.isNil(up)) {
            up = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
//            map_char_table (set_identity, Qnil, table, up);
//            map_char_table (shuffle, Qnil, table, up);
            caseTable.setExtra(0, up);
        }
        if (ELispSymbol.isNil(canon)) {
            canon = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
            caseTable.setExtra(1, canon);
//            map_char_table (set_canon, Qnil, table, table);
        }
        if (ELispSymbol.isNil(eqv)) {
            eqv = BuiltInCharTab.FMakeCharTable.makeCharTable(CASE_TABLE, false);
//            map_char_table (set_identity, Qnil, canon, eqv);
//            map_char_table (shuffle, Qnil, canon, eqv);
            caseTable.setExtra(2, eqv);
        }
        asCharTable(canon).setExtra(2, eqv);
        if (standard) {
            asciiDowncaseTable = caseTable;
            asciiUpcaseTable = asCharTable(up);
            asciiEqvTable = asCharTable(eqv);
            asciiCanonTable = asCharTable(canon);
        } else {
            // TODO: Set buffer case tables
        }
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
            return (ELispSymbol.isNil(up) || BuiltInData.FCharTableP.charTableP(up))
            && (ELispSymbol.isNil(canon) && ELispSymbol.isNil(eqv)
                    || FCaseTableP.caseTableP(canon)
                    && (ELispSymbol.isNil(eqv) || FCaseTableP.caseTableP(eqv)));
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
        public static Void setStandardCaseTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }
}
