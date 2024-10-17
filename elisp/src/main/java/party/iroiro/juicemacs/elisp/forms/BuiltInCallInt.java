package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInCallInt extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCallIntFactory.getFactories();
    }

    /**
     * <pre>
     * Specify a way of parsing arguments for interactive use of a function.
     * For example, write
     *  (defun foo (arg buf) "Doc string" (interactive "P\\nbbuffer: ") .... )
     *  to make ARG be the raw prefix argument, and set BUF to an existing buffer,
     *  when `foo' is called as a command.
     *
     * The "call" to `interactive' is actually a declaration rather than a
     *  function; it tells `call-interactively' how to read arguments to pass
     *  to the function.  When actually called, `interactive' just returns
     *  nil.
     *
     * Usually the argument of `interactive' is a string containing a code
     *  letter followed optionally by a prompt.  (Some code letters do not
     *  use I/O to get the argument and do not use prompts.)  To pass several
     *  arguments to the command, concatenate the individual strings,
     *  separating them by newline characters.
     *
     * Prompts are passed to `format', and may use %s escapes to print the
     *  arguments that have already been read.
     *
     * If the argument is not a string, it is evaluated to get a list of
     *  arguments to pass to the command.
     *
     * Just `(interactive)' means pass no arguments to the command when
     *  calling interactively.
     *
     * Code letters available are:
     * a -- Function name: symbol with a function definition.
     * b -- Name of existing buffer.
     * B -- Name of buffer, possibly nonexistent.
     * c -- Character (no input method is used).
     * C -- Command name: symbol with interactive function definition.
     * d -- Value of point as number.  Does not do I/O.
     * D -- Directory name.
     * e -- Parameterized event (i.e., one that's a list) that invoked this command.
     *      If used more than once, the Nth `e' returns the Nth parameterized event.
     *      This skips events that are integers or symbols.
     * f -- Existing file name.
     * F -- Possibly nonexistent file name.
     * G -- Possibly nonexistent file name, defaulting to just directory name.
     * i -- Ignored, i.e. always nil.  Does not do I/O.
     * k -- Key sequence (downcase the last event if needed to get a definition).
     * K -- Key sequence to be redefined (do not downcase the last event).
     * m -- Value of mark as number.  Does not do I/O.
     * M -- Any string.  Inherits the current input method.
     * n -- Number read using minibuffer.
     * N -- Numeric prefix arg, or if none, do like code `n'.
     * p -- Prefix arg converted to number.  Does not do I/O.
     * P -- Prefix arg in raw form.  Does not do I/O.
     * r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
     * s -- Any string.  Does not inherit the current input method.
     * S -- Any symbol.
     * U -- Mouse up event discarded by a previous k or K argument.
     * v -- Variable name: symbol that is `custom-variable-p'.
     * x -- Lisp expression read but not evaluated.
     * X -- Lisp expression read and evaluated.
     * z -- Coding system.
     * Z -- Coding system, nil if no prefix arg.
     *
     * In addition, if the string begins with `*', an error is signaled if
     *   the buffer is read-only.
     * If `@' appears at the beginning of the string, and if the key sequence
     *  used to invoke the command includes any mouse events, then the window
     *  associated with the first of those events is selected before the
     *  command is run.
     * If the string begins with `^' and `shift-select-mode' is non-nil,
     *  Emacs first calls the function `handle-shift-selection'.
     * You may use `@', `*', and `^' together.  They are processed in the
     *  order that they appear, before reading any arguments.
     *
     * If MODES is present, it should be one or more mode names (symbols)
     * for which this command is applicable.  This is so that `M-x TAB'
     * will be able to exclude this command from the list of completion
     * candidates if the current buffer's mode doesn't match the list.
     * Which commands are excluded from the list of completion
     * candidates based on this information is controlled by the value
     * of `read-extended-command-predicate', which see.
     *
     * usage: (interactive &amp;optional ARG-DESCRIPTOR &amp;rest MODES)
     * </pre>
     */
    @ELispBuiltIn(name = "interactive", minArgs = 0, maxArgs = 1, varArgs = false, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FInteractive extends ELispBuiltInBaseNode {
        @Specialization
        public static Void interactive(Object argDescriptor) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Like `funcall' but marks the call as interactive.
     * I.e. arrange that within the called function `called-interactively-p' will
     * return non-nil.
     * usage: (funcall-interactively FUNCTION &amp;rest ARGUMENTS)
     * </pre>
     */
    @ELispBuiltIn(name = "funcall-interactively", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFuncallInteractively extends ELispBuiltInBaseNode {
        @Specialization
        public static Void funcallInteractively(Object function, Object[] arguments) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Call FUNCTION, providing args according to its interactive calling specs.
     * Return the value FUNCTION returns.
     * The function contains a specification of how to do the argument reading.
     * In the case of user-defined functions, this is specified by placing a call
     * to the function `interactive' at the top level of the function body.
     * See `interactive'.
     *
     * Optional second arg RECORD-FLAG non-nil
     * means unconditionally put this command in the variable `command-history'.
     * Otherwise, this is done only if an arg is read using the minibuffer.
     *
     * Optional third arg KEYS, if given, specifies the sequence of events to
     * supply, as a vector, if FUNCTION inquires which events were used to
     * invoke it (via an `interactive' spec that contains, for instance, an
     * \"e\" code letter).  If KEYS is omitted or nil, the return value of
     * `this-command-keys-vector' is used.
     * </pre>
     */
    @ELispBuiltIn(name = "call-interactively", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FCallInteractively extends ELispBuiltInBaseNode {
        @Specialization
        public static Void callInteractively(Object function, Object recordFlag, Object keys) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return numeric meaning of raw prefix argument RAW.
     * A raw prefix argument is what you get from `(interactive "P")'.
     * Its numeric meaning is what you would get from `(interactive "p")'.
     * </pre>
     */
    @ELispBuiltIn(name = "prefix-numeric-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPrefixNumericValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Void prefixNumericValue(Object raw) {
            throw new UnsupportedOperationException();
        }
    }
}
