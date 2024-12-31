package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;

import java.util.List;

public class BuiltInIndent extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInIndentFactory.getFactories();
    }

    /**
     * <pre>
     * Return the horizontal position of point.  Beginning of line is column 0.
     * This is calculated by adding together the widths of all the displayed
     * representations of the character between the start of the previous line
     * and point (e.g., control characters will have a width of 2 or 4, tabs
     * will have a variable width).
     * Ignores finite width of frame, which means that this function may return
     * values greater than (frame-width).
     * In a buffer with very long lines, the value will be an approximation,
     * because calculating the exact number is very expensive.
     * Whether the line is visible (if `selective-display' is t) has no effect;
     * however, ^M is treated as end of line when `selective-display' is t.
     * Text that has an invisible property is considered as having width 0, unless
     * `buffer-invisibility-spec' specifies that it is replaced by an ellipsis.
     * </pre>
     */
    @ELispBuiltIn(name = "current-column", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentColumn extends ELispBuiltInBaseNode {
        @Specialization
        public long currentColumn() {
            return getContext().currentBuffer().getPosition().column();
        }
    }

    /**
     * <pre>
     * Indent from point with tabs and spaces until COLUMN is reached.
     * Optional second argument MINIMUM says always do at least MINIMUM spaces
     * even if that goes past COLUMN; by default, MINIMUM is zero.
     *
     * Whether this uses tabs or spaces depends on `indent-tabs-mode'.
     *
     * The return value is the column where the insertion ends.
     * </pre>
     */
    @ELispBuiltIn(name = "indent-to", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FIndentTo extends ELispBuiltInBaseNode {
        @Specialization
        public static Void indentTo(Object column, Object minimum) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the indentation of the current line.
     * This is the horizontal position of the character following any initial
     * whitespace.
     * Text that has an invisible property is considered as having width 0, unless
     * `buffer-invisibility-spec' specifies that it is replaced by an ellipsis.
     * </pre>
     */
    @ELispBuiltIn(name = "current-indentation", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentIndentation extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentIndentation() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point to column COLUMN in the current line.
     * Interactively, COLUMN is the value of prefix numeric argument.
     * The column of a character is calculated by adding together the widths
     * as displayed of the previous characters in the line.
     * This function ignores line-continuation;
     * there is no upper limit on the column number a character can have
     * and horizontal scrolling has no effect.
     * Text that has an invisible property is considered as having width 0,
     * unless `buffer-invisibility-spec' specifies that it is replaced by
     * an ellipsis.
     *
     * If specified column is within a character, point goes after that character.
     * If it's past end of line, point goes to end of line.
     *
     * Optional second argument FORCE non-nil means if COLUMN is in the
     * middle of a tab character, either change it to spaces (when
     * `indent-tabs-mode' is nil), or insert enough spaces before it to reach
     * COLUMN (otherwise).  In addition, if FORCE is t, and the line is too short
     * to reach COLUMN, add spaces/tabs to get there.
     *
     * The return value is the current column.
     * </pre>
     */
    @ELispBuiltIn(name = "move-to-column", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMoveToColumn extends ELispBuiltInBaseNode {
        @Specialization
        public long moveToColumn(long column, Object force) {
            ELispBuffer buffer = getContext().currentBuffer();
            PieceTreeBase.Position position = buffer.getPosition();
            buffer.setPosition(new PieceTreeBase.Position(
                    position.line(),
                    column
            ));
            return buffer.getPosition().column();
        }
    }

    /**
     * <pre>
     * Scan through the current buffer, calculating screen position.
     * Scan the current buffer forward from offset FROM,
     * assuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--
     * to position TO or position TOPOS--another cons of the form (HPOS . VPOS)--
     * and return the ending buffer position and screen location.
     *
     * If TOPOS is nil, the actual width and height of the window's
     * text area are used.
     *
     * There are three additional arguments:
     *
     * WIDTH is the number of columns available to display text;
     * this affects handling of continuation lines.  A value of nil
     * corresponds to the actual number of available text columns.
     *
     * OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).
     * HSCROLL is the number of columns not being displayed at the left
     * margin; this is usually taken from a window's hscroll member.
     * TAB-OFFSET is the number of columns of the first tab that aren't
     * being displayed, perhaps because the line was continued within it.
     * If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.
     *
     * WINDOW is the window to operate on.  It is used to choose the display table;
     * if it is showing the current buffer, it is used also for
     * deciding which overlay properties apply.
     * Note that `compute-motion' always operates on the current buffer.
     *
     * The value is a list of five elements:
     *   (POS HPOS VPOS PREVHPOS CONTIN)
     * POS is the buffer position where the scan stopped.
     * VPOS is the vertical position where the scan stopped.
     * HPOS is the horizontal position where the scan stopped.
     *
     * PREVHPOS is the horizontal position one character back from POS.
     * CONTIN is t if a line was continued after (or within) the previous character.
     *
     * For example, to find the buffer position of column COL of line LINE
     * of a certain window, pass the window's starting location as FROM
     * and the window's upper-left coordinates as FROMPOS.
     * Pass the buffer's (point-max) as TO, to limit the scan to the end of the
     * visible section of the buffer, and pass LINE and COL as TOPOS.
     * </pre>
     */
    @ELispBuiltIn(name = "compute-motion", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FComputeMotion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void computeMotion(Object from, Object frompos, Object to, Object topos, Object width, Object offsets, Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width used for displaying line numbers in the selected window.
     * If optional argument PIXELWISE is the symbol `columns', return the width
     * in units of the frame's canonical character width.  In this case, the
     * value is a float.
     * If optional argument PIXELWISE is t or any other non-nil value, return
     * the width as an integer number of pixels.
     * Otherwise return the value as an integer number of columns of the face
     * used to display line numbers, `line-number'.  Note that in the latter
     * case, the value doesn't include the 2 columns used for padding the
     * numbers on display.
     * </pre>
     */
    @ELispBuiltIn(name = "line-number-display-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLineNumberDisplayWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lineNumberDisplayWidth(Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point to start of the screen line LINES lines down.
     * If LINES is negative, this means moving up.
     *
     * This function is an ordinary cursor motion function
     * which calculates the new position based on how text would be displayed.
     * The new position may be the start of a line,
     * or the start of a continuation line,
     * or the start of the visible portion of a horizontally-scrolled line.
     *
     * The function returns number of screen lines moved over;
     * that usually equals LINES, but may be closer to zero if
     * beginning or end of buffer was reached.
     *
     * The optional second argument WINDOW specifies the window to use for
     * parameters such as width, horizontal scrolling, and so on.
     * The default is to use the selected window's parameters.
     *
     * If LINES is zero, point will move to the first visible character on
     * the current screen line.
     *
     * LINES can optionally take the form (COLS . LINES), in which case the
     * motion will stop at the COLSth column from the visual start of the
     * line (if such column exists on that line, that is).  If the line is
     * scrolled horizontally, COLS is interpreted visually, i.e., as addition
     * to the columns of text beyond the left edge of the window.
     * If LINES is a cons cell, its car COLS can be a float, which allows
     * specifying an accurate position of point on a screen line that mixes
     * fonts or uses variable-pitch font: COLS is interpreted in units of the
     * canonical character width, and is internally converted to pixel units;
     * point will then stop at the position closest to that pixel coordinate.
     * The cdr of the cons, LINES, must be an integer; if it is zero, this
     * function moves point horizontally in the current screen line, to the
     * position specified by COLS.
     *
     * The optional third argument CUR-COL specifies the horizontal
     * window-relative coordinate of point, in units of frame's canonical
     * character width, where the function is invoked.  If this argument is
     * omitted or nil, the function will determine the point coordinate by
     * going back to the beginning of the line.
     *
     * `vertical-motion' always uses the current buffer, regardless of which
     * buffer is displayed in WINDOW.  This is consistent with other cursor
     * motion functions and makes it possible to use `vertical-motion' in any
     * buffer, whether or not it is currently displayed in some window.
     * </pre>
     */
    @ELispBuiltIn(name = "vertical-motion", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FVerticalMotion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void verticalMotion(Object lines, Object window, Object curCol) {
            throw new UnsupportedOperationException();
        }
    }
}
