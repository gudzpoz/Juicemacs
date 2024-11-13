package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInMacros extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInMacrosFactory.getFactories();
    }

    /**
     * <pre>
     * Record subsequent keyboard input, defining a keyboard macro.
     * The commands are recorded even as they are executed.
     * Use \\[end-kbd-macro] to finish recording and make the macro available.
     * Use \\[name-last-kbd-macro] to give it a permanent name.
     * Non-nil arg (prefix arg) means append to last macro defined;
     * this begins by re-executing that macro as if you typed it again.
     * If optional second arg, NO-EXEC, is non-nil, do not re-execute last
     * macro before appending to it.
     * </pre>
     */
    @ELispBuiltIn(name = "start-kbd-macro", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStartKbdMacro extends ELispBuiltInBaseNode {
        @Specialization
        public static Void startKbdMacro(Object append, Object noExec) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Finish defining a keyboard macro.
     * The definition was started by \\[start-kbd-macro].
     * The macro is now available for use via \\[call-last-kbd-macro],
     * or it can be given a name with \\[name-last-kbd-macro] and then invoked
     * under that name.
     *
     * With numeric arg, repeat macro now that many times,
     * counting the definition just completed as the first repetition.
     * An argument of zero means repeat until error.
     *
     * In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
     * each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "end-kbd-macro", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEndKbdMacro extends ELispBuiltInBaseNode {
        @Specialization
        public static Void endKbdMacro(Object repeat, Object loopfunc) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Cancel the events added to a keyboard macro for this command.
     * </pre>
     */
    @ELispBuiltIn(name = "cancel-kbd-macro-events", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCancelKbdMacroEvents extends ELispBuiltInBaseNode {
        @Specialization
        public static Void cancelKbdMacroEvents() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Store EVENT into the keyboard macro being defined.
     * </pre>
     */
    @ELispBuiltIn(name = "store-kbd-macro-event", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStoreKbdMacroEvent extends ELispBuiltInBaseNode {
        @Specialization
        public static Void storeKbdMacroEvent(Object event) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Call the last keyboard macro that you defined with \\[start-kbd-macro].
     *
     * A prefix argument serves as a repeat count.  Zero means repeat until error.
     *
     * To make a macro permanent so you can call it even after
     * defining others, use \\[name-last-kbd-macro].
     *
     * In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
     * each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "call-last-kbd-macro", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCallLastKbdMacro extends ELispBuiltInBaseNode {
        @Specialization
        public static Void callLastKbdMacro(Object prefix, Object loopfunc) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Execute MACRO as a sequence of events.
     * If MACRO is a string or vector, then the events in it are executed
     * exactly as if they had been input by the user.
     *
     * If MACRO is a symbol, its function definition is used.  If that is
     * another symbol, this process repeats.  Eventually the result should be
     * a string or vector.  If the result is not a symbol, string, or vector,
     * an error is signaled.
     *
     * COUNT is a repeat count, or nil for once, or 0 for infinite loop.
     *
     * Optional third arg LOOPFUNC may be a function that is called prior to
     * each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.
     *
     * The buffer shown in the currently selected window will be made the current
     * buffer before the macro is executed.
     * </pre>
     */
    @ELispBuiltIn(name = "execute-kbd-macro", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FExecuteKbdMacro extends ELispBuiltInBaseNode {
        @Specialization
        public static Void executeKbdMacro(Object macro, Object count, Object loopfunc) {
            throw new UnsupportedOperationException();
        }
    }
}
