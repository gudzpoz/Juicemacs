package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInTerm extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInTermFactory.getFactories();
    }

    /**
     * <pre>
     * Return non-nil if the tty device TERMINAL can display colors.
     *
     * TERMINAL can be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).  This function always returns nil if
     * TERMINAL does not refer to a text terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "tty-display-color-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtyDisplayColorP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttyDisplayColorP(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of colors supported by the tty device TERMINAL.
     *
     * TERMINAL can be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).  This function always returns 0 if
     * TERMINAL does not refer to a text terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "tty-display-color-cells", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtyDisplayColorCells extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttyDisplayColorCells(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the type of the tty device that TERMINAL uses.
     * Returns nil if TERMINAL is not on a tty device.
     *
     * TERMINAL can be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).
     * </pre>
     */
    @ELispBuiltIn(name = "tty-type", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtyType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttyType(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if TERMINAL is the controlling tty of the Emacs process.
     *
     * TERMINAL can be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).  This function always returns nil if
     * TERMINAL is not on a tty device.
     * </pre>
     */
    @ELispBuiltIn(name = "controlling-tty-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FControllingTtyP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void controllingTtyP(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Declare that the tty used by TERMINAL does not handle underlining.
     * This is used to override the terminfo data, for certain terminals that
     * do not really do underlining, but say that they do.  This function has
     * no effect if used on a non-tty terminal.
     *
     * TERMINAL can be a terminal object, a frame or nil (meaning the
     * selected frame's terminal).  This function always returns nil if
     * TERMINAL does not refer to a text terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "tty-no-underline", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtyNoUnderline extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttyNoUnderline(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the topmost terminal frame on TERMINAL.
     * TERMINAL can be a terminal object, a frame or nil (meaning the
     * selected frame's terminal).  This function returns nil if TERMINAL
     * does not refer to a text terminal.  Otherwise, it returns the
     * top-most frame on the text terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "tty-top-frame", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtyTopFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttyTopFrame(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Suspend the terminal device TTY.
     *
     * The device is restored to its default state, and Emacs ceases all
     * access to the tty device.  Frames that use the device are not deleted,
     * but input is not read from them and if they change, their display is
     * not updated.
     *
     * TTY may be a terminal object, a frame, or nil for the terminal device
     * of the currently selected frame.
     *
     * This function runs `suspend-tty-functions' after suspending the
     * device.  The functions are run with one arg, the id of the suspended
     * terminal device.
     *
     * `suspend-tty' does nothing if it is called on a device that is already
     * suspended.
     *
     * A suspended tty may be resumed by calling `resume-tty' on it.
     * </pre>
     */
    @ELispBuiltIn(name = "suspend-tty", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSuspendTty extends ELispBuiltInBaseNode {
        @Specialization
        public static Void suspendTty(Object tty) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Resume the previously suspended terminal device TTY.
     * The terminal is opened and reinitialized.  Frames that are on the
     * suspended terminal are revived.
     *
     * It is an error to resume a terminal while another terminal is active
     * on the same device.
     *
     * This function runs `resume-tty-functions' after resuming the terminal.
     * The functions are run with one arg, the id of the resumed terminal
     * device.
     *
     * `resume-tty' does nothing if it is called on a device that is not
     * suspended.
     *
     * TTY may be a terminal object, a frame, or nil (meaning the selected
     * frame's terminal).
     * </pre>
     */
    @ELispBuiltIn(name = "resume-tty", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FResumeTty extends ELispBuiltInBaseNode {
        @Specialization
        public static Void resumeTty(Object tty) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the output buffer size for a TTY.
     *
     * SIZE zero means use the system's default value.  If SIZE is
     * non-zero, this also avoids flushing the output stream.
     *
     * TTY may be a terminal object, a frame, or nil (meaning the selected
     * frame's terminal).
     *
     * This function temporarily suspends and resumes the terminal
     * device.
     * </pre>
     */
    @ELispBuiltIn(name = "tty--set-output-buffer-size", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTtySetOutputBufferSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttySetOutputBufferSize(Object size, Object tty) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the output buffer size of TTY.
     *
     * TTY may be a terminal object, a frame, or nil (meaning the selected
     * frame's terminal).
     *
     * A value of zero means TTY uses the system's default value.
     * </pre>
     */
    @ELispBuiltIn(name = "tty--output-buffer-size", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtyOutputBufferSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttyOutputBufferSize(Object tty) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Open a connection to Gpm.
     * Gpm-mouse can only be activated for one tty at a time.
     * </pre>
     */
    @ELispBuiltIn(name = "gpm-mouse-start", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGpmMouseStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Void gpmMouseStart() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Close a connection to Gpm.
     * </pre>
     */
    @ELispBuiltIn(name = "gpm-mouse-stop", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGpmMouseStop extends ELispBuiltInBaseNode {
        @Specialization
        public static Void gpmMouseStop() {
            throw new UnsupportedOperationException();
        }
    }
}
