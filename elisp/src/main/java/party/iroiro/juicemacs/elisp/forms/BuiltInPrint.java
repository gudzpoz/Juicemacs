package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.ERROR;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.ERROR_MESSAGE;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInPrint extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInPrintFactory.getFactories();
    }

    /**
     * <pre>
     * Output character CHARACTER to stream PRINTCHARFUN.
     * PRINTCHARFUN defaults to the value of `standard-output' (which see).
     * </pre>
     */
    @ELispBuiltIn(name = "write-char", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWriteChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void writeChar(Object character, Object printcharfun) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Output a newline to stream PRINTCHARFUN.
     * If ENSURE is non-nil only output a newline if not already at the
     * beginning of a line.  Value is non-nil if a newline is printed.
     * If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used.
     * </pre>
     */
    @ELispBuiltIn(name = "terpri", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTerpri extends ELispBuiltInBaseNode {
        @Specialization
        public static Void terpri(Object printcharfun, Object ensure) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Output the printed representation of OBJECT, any Lisp object.
     * Quoting characters are printed when needed to make output that `read'
     * can handle, whenever this is possible.  For complex objects, the behavior
     * is controlled by `print-level' and `print-length', which see.
     *
     * OBJECT is any of the Lisp data types: a number, a string, a symbol,
     * a list, a buffer, a window, a frame, etc.
     *
     * A printed representation of an object is text which describes that object.
     *
     * Optional argument PRINTCHARFUN is the output stream, which can be one
     * of these:
     *
     *    - a buffer, in which case output is inserted into that buffer at point;
     *    - a marker, in which case output is inserted at marker's position;
     *    - a function, in which case that function is called once for each
     *      character of OBJECT's printed representation;
     *    - a symbol, in which case that symbol's function definition is called; or
     *    - t, in which case the output is displayed in the echo area.
     *
     * If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
     * is used instead.
     *
     * Optional argument OVERRIDES should be a list of settings for print-related
     * variables.  An element in this list can be the symbol t, which means "reset
     * all the values to their defaults".  Otherwise, an element should be a pair,
     * where the `car' or the pair is the setting symbol, and the `cdr' is the
     * value of the setting to use for this `prin1' call.
     *
     * For instance:
     *
     *   (prin1 object nil \\='((length . 100) (circle . t))).
     *
     * See Info node `(elisp)Output Overrides' for a list of possible values.
     *
     * As a special case, OVERRIDES can also simply be the symbol t, which
     * means "use default values for all the print-related settings".
     * </pre>
     */
    @ELispBuiltIn(name = "prin1", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPrin1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object prin1(Object object, Object printcharfun, Object overrides) {
            ELispPrint print = getPrint(printcharfun);
            print.print(object).flush();
            return object;
        }

        public static ELispPrint getPrint(Object printcharfun) {
            if (isNil(printcharfun)) {
                printcharfun = ELispContext.get(null).currentBuffer();
            }
            // TODO
            MuleStringBuilder output = isT(printcharfun) ? new MuleStringBuilder() : null;
            return switch (printcharfun) {
                case ELispBuffer buffer -> ELispPrint.fromBuffer(buffer);
                case ELispMarker marker -> ELispPrint.fromMarker(marker);
                default -> output == null ? ELispPrint.fromFunc(printcharfun) : ELispPrint.fromBuilder(output);
            };
        }
    }

    /**
     * <pre>
     * Return a string containing the printed representation of OBJECT.
     * OBJECT can be any Lisp object.  This function outputs quoting characters
     * when necessary to make output that `read' can handle, whenever possible,
     * unless the optional second argument NOESCAPE is non-nil.  For complex objects,
     * the behavior is controlled by `print-level' and `print-length', which see.
     *
     * OBJECT is any of the Lisp data types: a number, a string, a symbol,
     * a list, a buffer, a window, a frame, etc.
     *
     * See `prin1' for the meaning of OVERRIDES.
     *
     * A printed representation of an object is text which describes that object.
     * </pre>
     */
    @ELispBuiltIn(name = "prin1-to-string", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPrin1ToString extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString prin1ToString(Object object, Object noescape, Object overrides) {
            return new ELispString(ELispPrint.toString(object));
        }
    }

    /**
     * <pre>
     * Output the printed representation of OBJECT, any Lisp object.
     * No quoting characters are used; no delimiters are printed around
     * the contents of strings.
     *
     * OBJECT is any of the Lisp data types: a number, a string, a symbol,
     * a list, a buffer, a window, a frame, etc.
     *
     * A printed representation of an object is text which describes that object.
     *
     * Optional argument PRINTCHARFUN is the output stream, which can be one
     * of these:
     *
     *    - a buffer, in which case output is inserted into that buffer at point;
     *    - a marker, in which case output is inserted at marker's position;
     *    - a function, in which case that function is called once for each
     *      character of OBJECT's printed representation;
     *    - a symbol, in which case that symbol's function definition is called; or
     *    - t, in which case the output is displayed in the echo area.
     *
     * If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
     * is used instead.
     * </pre>
     */
    @ELispBuiltIn(name = "princ", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPrinc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object princ(Object object, Object printcharfun) {
            // TODO
            switch (toSym(object)) {
                case ELispString s -> FPrin1.getPrint(printcharfun).print(s.value()).flush();
                case ELispSymbol s -> FPrin1.getPrint(printcharfun).print(s.name()).flush();
                default -> FPrin1.prin1(object, printcharfun, false);
            }
            return object;
        }
    }

    /**
     * <pre>
     * Output the printed representation of OBJECT, with newlines around it.
     * Quoting characters are printed when needed to make output that `read'
     * can handle, whenever this is possible.  For complex objects, the behavior
     * is controlled by `print-level' and `print-length', which see.
     *
     * OBJECT is any of the Lisp data types: a number, a string, a symbol,
     * a list, a buffer, a window, a frame, etc.
     *
     * A printed representation of an object is text which describes that object.
     *
     * Optional argument PRINTCHARFUN is the output stream, which can be one
     * of these:
     *
     *    - a buffer, in which case output is inserted into that buffer at point;
     *    - a marker, in which case output is inserted at marker's position;
     *    - a function, in which case that function is called once for each
     *      character of OBJECT's printed representation;
     *    - a symbol, in which case that symbol's function definition is called; or
     *    - t, in which case the output is displayed in the echo area.
     *
     * If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
     * is used instead.
     * </pre>
     */
    @ELispBuiltIn(name = "print", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPrint extends ELispBuiltInBaseNode {
        @Specialization
        public static Void print(Object object, Object printcharfun) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Flush standard-output.
     * This can be useful after using `princ' and the like in scripts.
     * </pre>
     */
    @ELispBuiltIn(name = "flush-standard-output", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FFlushStandardOutput extends ELispBuiltInBaseNode {
        @Specialization
        public static Void flushStandardOutput() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Write CHARACTER to stderr.
     * You can call `print' while debugging emacs, and pass it this function
     * to make it write to the debugging output.
     * </pre>
     */
    @ELispBuiltIn(name = "external-debugging-output", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FExternalDebuggingOutput extends ELispBuiltInBaseNode {
        @Specialization
        public static Void externalDebuggingOutput(Object character) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Redirect debugging output (stderr stream) to file FILE.
     * If FILE is nil, reset target to the initial stderr stream.
     * Optional arg APPEND non-nil (interactively, with prefix arg) means
     * append to existing target file.
     * </pre>
     */
    @ELispBuiltIn(name = "redirect-debugging-output", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRedirectDebuggingOutput extends ELispBuiltInBaseNode {
        @Specialization
        public static Void redirectDebuggingOutput(Object file, Object append) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert an error value (ERROR-SYMBOL . DATA) to an error message.
     * See Info anchor `(elisp)Definition of signal' for some details on how this
     * error message is constructed.
     * </pre>
     */
    @ELispBuiltIn(name = "error-message-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FErrorMessageString extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString errorMessageString(ELispCons obj) {
            ELispSymbol error = asSym(obj.car());
            Object message = BuiltInFns.FGet.get(error, ERROR_MESSAGE);
            if (isNil(message) && error == ERROR) {
                message = asCons(obj.cdr()).car();
            }
            MuleStringBuilder buffer = new MuleStringBuilder();
            ELispPrint print = ELispPrint.fromBuilder(buffer);
            if (isNil(message)) {
                print.print("peculiar error");
            } else {
                print.print(asStr(message));
            }
            print.print(':').print(' ');
            boolean start = true;
            for (Object o : ELispCons.iterate(obj.cdr())) {
                if (start) {
                    start = false;
                } else {
                    print.print(',').print(' ');
                }
                print.print(o);
            }
            print.flush();
            return new ELispString(buffer.build());
        }
    }

    /**
     * <pre>
     * Extract sharing info from OBJECT needed to print it.
     * Fills `print-number-table' if `print-circle' is non-nil.  Does nothing
     * if `print-circle' is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "print--preprocess", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPrintPreprocess extends ELispBuiltInBaseNode {
        @Specialization
        public static Void printPreprocess(Object object) {
            throw new UnsupportedOperationException();
        }
    }
}
