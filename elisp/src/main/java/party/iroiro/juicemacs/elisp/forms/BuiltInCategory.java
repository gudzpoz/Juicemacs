package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInCategory extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCategoryFactory.getFactories();
    }

    private static long checkCategory(Object category) {
        return asRanged(category, ' ', '~');
    }

    private static ELispCharTable checkCategoryTable(Object table) {
        ELispContext context = ELispContext.get(null);
        if (isNil(table)) {
            return asCharTable(context.currentBuffer().getCategoryTable());
        }
        if (!FCategoryTableP.categoryTableP(table)) {
            throw ELispSignals.wrongTypeArgument(CATEGORY_TABLE_P, table);
        }
        return asCharTable(table);
    }

    private static Object getCategoryDocstring(ELispCharTable table, long category) {
        return BuiltInData.FAref.aref(table.getExtra(0), category - ' ');
    }

    private static void setCategoryDocstring(ELispCharTable table, long category, Object docstring) {
        BuiltInData.FAset.aset(asVector(table.getExtra(0)), category - ' ', docstring);
    }

    private static ELispBoolVector hashGetCategorySet(ELispCharTable table, ELispBoolVector categorySet) {
        if (isNil(table.getExtra(1))) {
            table.setExtra(1, new ELispHashtable(EQUAL, false));
        }
        ELispHashtable hashtable = asHashtable(table.getExtra(1));
        Object set = hashtable.get(categorySet);
        if (!isNil(set)) {
            return asBoolVec(set);
        }
        hashtable.put(categorySet, false);
        return categorySet;
    }

    /**
     * <pre>
     * Return a newly created category-set which contains CATEGORIES.
     * CATEGORIES is a string of category mnemonics.
     * The value is a bool-vector which has t at the indices corresponding to
     * those categories.
     * </pre>
     */
    @ELispBuiltIn(name = "make-category-set", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeCategorySet extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeCategorySet(Object categories) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Define CATEGORY as a category which is described by DOCSTRING.
     * CATEGORY should be an ASCII printing character in the range ` ' to `~'.
     * DOCSTRING is the documentation string of the category.  The first line
     * should be a terse text (preferably less than 16 characters),
     * and the rest lines should be the full description.
     * The category is defined only in category table TABLE, which defaults to
     * the current buffer's category table.
     * </pre>
     */
    @ELispBuiltIn(name = "define-category", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefineCategory extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean defineCategory(Object category, ELispString docstring, Object table) {
            long cat = checkCategory(category);
            ELispCharTable categoryTable = checkCategoryTable(table);
            if (!isNil(getCategoryDocstring(categoryTable, cat))) {
                throw ELispSignals.error("Category already defined");
            }
            setCategoryDocstring(categoryTable, cat, docstring);
            return false;
        }
    }

    /**
     * <pre>
     * Return the documentation string of CATEGORY, as defined in TABLE.
     * TABLE should be a category table and defaults to the current buffer's
     * category table.
     * </pre>
     */
    @ELispBuiltIn(name = "category-docstring", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCategoryDocstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Void categoryDocstring(Object category, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a category which is not yet defined in TABLE.
     * If no category remains available, return nil.
     * The optional argument TABLE specifies which category table to modify;
     * it defaults to the current buffer's category table.
     * </pre>
     */
    @ELispBuiltIn(name = "get-unused-category", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetUnusedCategory extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getUnusedCategory(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if ARG is a category table.
     * </pre>
     */
    @ELispBuiltIn(name = "category-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCategoryTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean categoryTableP(Object arg) {
            return arg instanceof ELispCharTable table && table.getPurpose() == CATEGORY_TABLE;
        }
    }

    /**
     * <pre>
     * Return the current category table.
     * This is the one specified by the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "category-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCategoryTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void categoryTable() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the standard category table.
     * This is the one used for new buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "standard-category-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FStandardCategoryTable extends ELispBuiltInBaseNode {
        @Specialization
        public Object standardCategoryTable() {
            return getContext().globals().getBufferDefaults().getCategoryTable();
        }
    }

    /**
     * <pre>
     * Construct a new category table and return it.
     * It is a copy of the TABLE, which defaults to the standard category table.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-category-table", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopyCategoryTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copyCategoryTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Construct a new and empty category table and return it.
     * </pre>
     */
    @ELispBuiltIn(name = "make-category-table", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMakeCategoryTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeCategoryTable() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Specify TABLE as the category table for the current buffer.
     * Return TABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "set-category-table", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetCategoryTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setCategoryTable(Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the category set of CHAR.
     * usage: (char-category-set CHAR)
     * </pre>
     */
    @ELispBuiltIn(name = "char-category-set", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharCategorySet extends ELispBuiltInBaseNode {
        @Specialization
        public Object charCategorySet(long char_) {
            ELispCharTable table = asCharTable(getContext().currentBuffer().getCategoryTable());
            return table.getChar((int) char_);
        }
    }

    /**
     * <pre>
     * Return a string containing mnemonics of the categories in CATEGORY-SET.
     * CATEGORY-SET is a bool-vector, and the categories \"in\" it are those
     * that are indexes where t occurs in the bool-vector.
     * The return value is a string containing those same categories.
     * </pre>
     */
    @ELispBuiltIn(name = "category-set-mnemonics", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCategorySetMnemonics extends ELispBuiltInBaseNode {
        @Specialization
        public static Void categorySetMnemonics(Object categorySet) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Modify the category set of CHARACTER by adding CATEGORY to it.
     * The category is changed only for table TABLE, which defaults to
     * the current buffer's category table.
     * CHARACTER can be either a single character or a cons representing the
     * lower and upper ends of an inclusive character range to modify.
     * CATEGORY must be a category name (a character between ` ' and `~').
     * Use `describe-categories' to see existing category names.
     * If optional fourth argument RESET is non-nil,
     * then delete CATEGORY from the category set instead of adding it.
     * </pre>
     */
    @ELispBuiltIn(name = "modify-category-entry", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FModifyCategoryEntry extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static boolean modifyCategoryEntry(Object character, Object category, Object table, Object reset) {
            int start, end;
            if (character instanceof ELispCons cons) {
                start = asChar(cons.car());
                end = asChar(cons.cdr());
            } else {
                start = end = asChar(character);
            }
            int cat = (int) checkCategory(category);
            ELispCharTable categoryTable = checkCategoryTable(table);
            if (isNil(getCategoryDocstring(categoryTable, cat))) {
                throw ELispSignals.error("Undefined category");
            }
            boolean setValue = isNil(reset);

            while (start <= end) {
                ELispCharTable.RefRangeResult range = categoryTable.refRange(start, start, end);
                ELispBoolVector categorySet = asBoolVec(range.value());
                if (categorySet.get(cat) != setValue) {
                    categorySet = BuiltInFns.FCopySequence.copySequenceBoolVec(categorySet);
                    categorySet.set(cat, setValue);
                    categorySet = hashGetCategorySet(categoryTable, categorySet);
                    categoryTable.setRange(range.start(), range.end(), categorySet);
                }
                start = range.end() + 1;
            }

            return false;
        }
    }
}
