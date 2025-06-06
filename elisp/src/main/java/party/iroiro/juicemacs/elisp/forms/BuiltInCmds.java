package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.notNilOr;

public class BuiltInCmds extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCmdsFactory.getFactories();
    }

    /**
     * <pre>
     * Move point N characters forward (backward if N is negative).
     * On reaching end or beginning of buffer, stop and signal error.
     * Interactively, N is the numeric prefix argument.
     * If N is omitted or nil, move point 1 character forward.
     *
     * Depending on the bidirectional context, the movement may be to the
     * right or to the left on the screen.  This is in contrast with
     * \\[right-char], which see.
     * </pre>
     */
    @ELispBuiltIn(name = "forward-char", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForwardChar extends ELispBuiltInBaseNode {
        @Specialization
        public boolean forwardChar(Object n) {
            ELispBuffer buffer = getContext().currentBuffer();
            return forwardCharBuffer(n, buffer);
        }

         public static boolean forwardCharBuffer(Object n, ELispBuffer buffer) {
            long count = notNilOr(n, 1);
            buffer.setPoint(buffer.getPoint() + count);
            return false;
        }
    }

    /**
     * <pre>
     * Move point N characters backward (forward if N is negative).
     * On attempt to pass beginning or end of buffer, stop and signal error.
     * Interactively, N is the numeric prefix argument.
     * If N is omitted or nil, move point 1 character backward.
     *
     * Depending on the bidirectional context, the movement may be to the
     * right or to the left on the screen.  This is in contrast with
     * \\[left-char], which see.
     * </pre>
     */
    @ELispBuiltIn(name = "backward-char", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBackwardChar extends ELispBuiltInBaseNode {
        @Specialization
        public boolean backwardChar(Object n) {
            ELispBuffer buffer = getContext().currentBuffer();
            return backwardCharBuffer(n, buffer);
        }

        public static boolean backwardCharBuffer(Object n, ELispBuffer buffer) {
            long count = notNilOr(n, 1);
            buffer.setPoint(buffer.getPoint() - count);
            return false;
        }
    }

    /**
     * <pre>
     * Move N lines forward (backward if N is negative).
     * Precisely, if point is on line I, move to the start of line I + N
     * \("start of line" in the logical order).
     * If there isn't room, go as far as possible (no error).
     * Interactively, N is the numeric prefix argument and defaults to 1.
     *
     * Returns the count of lines left to move.  If moving forward,
     * that is N minus number of lines moved; if backward, N plus number
     * moved.
     *
     * Exception: With positive N, a non-empty line at the end of the
     * buffer, or of its accessible portion, counts as one line
     * successfully moved (for the return value).  This means that the
     * function will move point to the end of such a line and will count
     * it as a line moved across, even though there is no next line to
     * go to its beginning.
     * </pre>
     */
    @ELispBuiltIn(name = "forward-line", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForwardLine extends ELispBuiltInBaseNode {
        @Specialization
        public long forwardLine(Object n) {
            return forwardLineCtx(getContext().currentBuffer(), n);
        }

        /// See [BuiltInEditFns.FLineBeginningPosition#lineEdgePosition(Object, ELispBuffer, long, long)]
        public static long forwardLineCtx(ELispBuffer buffer, Object n) {
            long line = notNilOr(n, 1);
            PieceTreeBase.Position position = buffer.getPosition();
            int maxLine = buffer.getLineCount();

            long target = position.line() + line;
            int nextLine = Math.clamp(target, 1, maxLine);
            buffer.setPosition(new PieceTreeBase.Position(
                    nextLine,
                    nextLine == target || line < 0 ? 1 : Long.MAX_VALUE
            ));
            return (long) maxLine - nextLine;
        }
    }

    /**
     * <pre>
     * Move point to beginning of current line (in the logical order).
     * With argument N not nil or 1, move forward N - 1 lines first.
     * If point reaches the beginning or end of buffer, it stops there.
     *
     * This function constrains point to the current field unless this moves
     * point to a different line from the original, unconstrained result.
     * If N is nil or 1, and a front-sticky field starts at point, the point
     * does not move.  To ignore field boundaries bind
     * `inhibit-field-text-motion' to t, or use the `forward-line' function
     * instead.  For instance, `(forward-line 0)' does the same thing as
     * `(beginning-of-line)', except that it ignores field boundaries.
     * </pre>
     */
    @ELispBuiltIn(name = "beginning-of-line", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBeginningOfLine extends ELispBuiltInBaseNode {
        @Specialization
        public boolean beginningOfLine(Object n) {
            ELispBuffer buffer = getContext().currentBuffer();
            buffer.setPoint(BuiltInEditFns.FLineBeginningPosition.lineEdgePosition(n, buffer, true));
            return false;
        }
    }

    /**
     * <pre>
     * Move point to end of current line (in the logical order).
     * With argument N not nil or 1, move forward N - 1 lines first.
     * If point reaches the beginning or end of buffer, it stops there.
     * To ignore intangibility, bind `inhibit-point-motion-hooks' to t.
     *
     * This function constrains point to the current field unless this moves
     * point to a different line from the original, unconstrained result.  If
     * N is nil or 1, and a rear-sticky field ends at point, the point does
     * not move.  To ignore field boundaries bind `inhibit-field-text-motion'
     * to t.
     * </pre>
     */
    @ELispBuiltIn(name = "end-of-line", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FEndOfLine extends ELispBuiltInBaseNode {
        @Specialization
        public boolean endOfLine(Object n) {
            ELispBuffer buffer = getContext().currentBuffer();
            return endOfLineBuffer(n, buffer);
        }

        public static boolean endOfLineBuffer(Object n, ELispBuffer buffer) {
            buffer.setPoint(BuiltInEditFns.FLineBeginningPosition.lineEdgePosition(n, buffer, false));
            return false;
        }
    }

    /**
     * <pre>
     * Delete the following N characters (previous if N is negative).
     * Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
     * Interactively, N is the prefix arg, and KILLFLAG is set if
     * N was explicitly specified.
     *
     * The command `delete-forward-char' is preferable for interactive use, e.g.
     * because it respects values of `delete-active-region' and `overwrite-mode'.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-char", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteChar extends ELispBuiltInBaseNode {
        @Specialization
        public boolean deleteChar(long n, Object killflag) {
            ELispBuffer buffer = getContext().currentBuffer();
            long point = buffer.getPoint();
            buffer.delete(point, n);
            return false;
        }
    }

    /**
     * <pre>
     * Insert the character you type.
     * Whichever character C you type to run this command is inserted.
     * The numeric prefix argument N says how many times to repeat the insertion.
     * Before insertion, `expand-abbrev' is executed if the inserted character does
     * not have word syntax and the previous character in the buffer does.
     * After insertion, `internal-auto-fill' is called if
     * `auto-fill-function' is non-nil and if the `auto-fill-chars' table has
     * a non-nil value for the inserted character.  At the end, it runs
     * `post-self-insert-hook'.
     * </pre>
     */
    @ELispBuiltIn(name = "self-insert-command", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSelfInsertCommand extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean selfInsertCommand(Object n, Object c) {
            // TODO
            long count = BuiltInCallInt.FPrefixNumericValue.prefixNumericValue(n);
            long character = notNilOr(c, ' ');
            if (count == 1) {
                return BuiltInEditFns.FInsert.insert(new Object[]{character});
            }
            MuleStringBuffer buffer = new MuleStringBuffer();
            for (int i = 0; i < count; i++) {
                buffer.append((int) character);
            }
            return BuiltInEditFns.FInsert.insert(new Object[]{new ELispString(buffer.build())});
        }
    }
}
