package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInTerminal extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInTerminalFactory.getFactories();
    }

    /**
     * <pre>
     * Delete TERMINAL by deleting all frames on it and closing the terminal.
     * TERMINAL may be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).
     *
     * Normally, you may not delete a display if all other displays are suspended,
     * but if the second argument FORCE is non-nil, you may do so.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-terminal", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteTerminal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteTerminal(Object terminal, Object force) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the terminal that FRAME is displayed on.
     * If FRAME is nil, use the selected frame.
     *
     * The terminal device is represented by its integer identifier.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-terminal", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTerminal extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean frameTerminal(Object frame) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return non-nil if OBJECT is a terminal which has not been deleted.
     * Return nil if OBJECT is not a live display terminal.
     * OBJECT may be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).
     * If OBJECT is a live display terminal, return what sort of output
     * terminal it uses.  See the documentation of `framep' for possible
     * return values.
     * </pre>
     */
    @ELispBuiltIn(name = "terminal-live-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTerminalLiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean terminalLiveP(Object object) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return a list of all terminal devices.
     * </pre>
     */
    @ELispBuiltIn(name = "terminal-list", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FTerminalList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void terminalList() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the name of the terminal device TERMINAL.
     * It is not guaranteed that the returned value is unique among opened devices.
     *
     * TERMINAL may be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).
     * </pre>
     */
    @ELispBuiltIn(name = "terminal-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTerminalName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void terminalName(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parameter-alist of terminal TERMINAL.
     * The value is a list of elements of the form (PARM . VALUE), where PARM
     * is a symbol.
     *
     * TERMINAL can be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).
     * </pre>
     */
    @ELispBuiltIn(name = "terminal-parameters", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTerminalParameters extends ELispBuiltInBaseNode {
        @Specialization
        public static Void terminalParameters(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return TERMINAL's value for parameter PARAMETER.
     * TERMINAL can be a terminal object, a frame, or nil (meaning the
     * selected frame's terminal).
     * </pre>
     */
    @ELispBuiltIn(name = "terminal-parameter", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTerminalParameter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void terminalParameter(Object terminal, Object parameter) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set TERMINAL's value for parameter PARAMETER to VALUE.
     * Return the previous value of PARAMETER.
     *
     * TERMINAL can be a terminal object, a frame or nil (meaning the
     * selected frame's terminal).
     * </pre>
     */
    @ELispBuiltIn(name = "set-terminal-parameter", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetTerminalParameter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setTerminalParameter(Object terminal, Object parameter, Object value) {
            throw new UnsupportedOperationException();
        }
    }
}
