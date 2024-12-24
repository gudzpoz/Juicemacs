package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInDispNew extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInDispNewFactory.getFactories();
    }

    /**
     * <pre>
     * Dump redisplay history to stderr.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-redisplay-history", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDumpRedisplayHistory extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpRedisplayHistory() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Clear frame FRAME and output again what is supposed to appear on it.
     * If FRAME is omitted or nil, the selected frame is used.
     * </pre>
     */
    @ELispBuiltIn(name = "redraw-frame", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRedrawFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void redrawFrame(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Clear and redisplay all visible frames.
     * </pre>
     */
    @ELispBuiltIn(name = "redraw-display", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRedrawDisplay extends ELispBuiltInBaseNode {
        @Specialization
        public static Void redrawDisplay() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Handle mouse movement detected by Lisp code.
     *
     * This function should be called when Lisp code detects the mouse has
     * moved, even if `track-mouse' is nil.  This handles updates that do not
     * rely on input events such as updating display for mouse-face
     * properties or updating the help echo text.
     * </pre>
     */
    @ELispBuiltIn(name = "display--update-for-mouse-movement", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDisplayUpdateForMouseMovement extends ELispBuiltInBaseNode {
        @Specialization
        public static Void displayUpdateForMouseMovement(Object mouseX, Object mouseY) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Start writing all terminal output to FILE as well as the terminal.
     * FILE = nil means just close any termscript file currently open.
     * </pre>
     */
    @ELispBuiltIn(name = "open-termscript", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOpenTermscript extends ELispBuiltInBaseNode {
        @Specialization
        public static Void openTermscript(Object file) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Send STRING to the terminal without alteration.
     * Control characters in STRING will have terminal-dependent effects.
     *
     * Optional parameter TERMINAL specifies the tty terminal device to use.
     * It may be a terminal object, a frame, or nil for the terminal used by
     * the currently selected frame.  In batch mode, STRING is sent to stdout
     * when TERMINAL is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "send-string-to-terminal", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSendStringToTerminal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sendStringToTerminal(Object string, Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Beep, or flash the screen.
     * Also, unless an argument is given,
     * terminate any keyboard macro currently executing.
     * </pre>
     */
    @ELispBuiltIn(name = "ding", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDing extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ding(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Pause, without updating display, for SECONDS seconds.
     * SECONDS may be a floating-point value, meaning that you can wait for a
     * fraction of a second.
     * An optional second arg MILLISECONDS can be provided but is deprecated:
     * it specifies an additional wait period, in milliseconds.
     * </pre>
     */
    @ELispBuiltIn(name = "sleep-for", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSleepFor extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sleepFor(Object seconds, Object milliseconds) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Perform redisplay.
     * Optional arg FORCE, if non-nil, prevents redisplay from being
     * preempted by arriving input, even if `redisplay-dont-pause' is nil.
     * If `redisplay-dont-pause' is non-nil (the default), redisplay is never
     * preempted by arriving input, so FORCE does nothing.
     *
     * Return t if redisplay was performed, nil if redisplay was preempted
     * immediately by pending input.
     * </pre>
     */
    @ELispBuiltIn(name = "redisplay", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRedisplay extends ELispBuiltInBaseNode {
        @Specialization
        public static Void redisplay(Object force) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if the frame and buffer state appears to have changed.
     * VARIABLE is a variable name whose value is either nil or a state vector
     * that will be updated to contain all frames and buffers,
     * aside from buffers whose names start with space,
     * along with the buffers' read-only and modified flags.  This allows a fast
     * check to see whether buffer menus might need to be recomputed.
     * If this function returns non-nil, it updates the internal vector to reflect
     * the current state.
     *
     * If VARIABLE is nil, an internal variable is used.  Users should not
     * pass nil for VARIABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-or-buffer-changed-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameOrBufferChangedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameOrBufferChangedP(Object variable) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the cursor-visibility flag of WINDOW to SHOW.
     * WINDOW nil means use the selected window.  SHOW non-nil means
     * show a cursor in WINDOW in the next redisplay.  SHOW nil means
     * don't show a cursor.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-show-cursor", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalShowCursor extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalShowCursor(Object window, Object show) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Value is non-nil if next redisplay will display a cursor in WINDOW.
     * WINDOW nil or omitted means report on the selected window.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-show-cursor-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalShowCursorP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalShowCursorP(Object window) {
            throw new UnsupportedOperationException();
        }
    }
}
