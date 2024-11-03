package party.iroiro.juicemacs.elisp.runtime.objects;

/**
 * Syntax table object
 *
 * <p>
 * This is a simple wrapper for {@link ELispCharTable} as a
 * <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html">
 * syntax table</a>.
 * Theoretically, a syntax table in Emacs is a char-table with a purpose of {@code syntax-table}.
 * However, since several ELisp-y RegExp features depend on the current syntax table
 * (including the commonly used {@code \w}), we will need to do a bit of pre-processing
 * here to map those features to Java RegExp patterns.
 * </p>
 */
public final class ELispSyntaxTable extends ELispCharTable {

    public ELispSyntaxTable(Object init, int extraSlots) {
        super(init, extraSlots);
    }

}
