package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInCallProc extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCallProcFactory.getFactories();
    }

    /**
     * <pre>
     * Call PROGRAM synchronously in separate process.
     * The remaining arguments are optional.
     *
     * The program's input comes from file INFILE (nil means `null-device').
     * If INFILE is a relative path, it will be looked for relative to the
     * directory where the process is run (see below).  If you want to make the
     * input come from an Emacs buffer, use `call-process-region' instead.
     *
     * Third argument DESTINATION specifies how to handle program's output.
     * (\"Output\" here means both standard output and standard error
     * output.)
     * If DESTINATION is a buffer or the name of a buffer, or t (which stands for
     * the current buffer), it means insert output in that buffer before point.
     * If DESTINATION is nil, it means discard output; 0 means discard
     *  and don't wait for the program to terminate.
     * If DESTINATION is `(:file FILE)', where FILE is a file name string,
     *  it means that output should be written to that file (if the file
     *  already exists it is overwritten).
     * DESTINATION can also have the form (REAL-BUFFER STDERR-FILE); in that case,
     *  REAL-BUFFER says what to do with standard output, as above,
     *  while STDERR-FILE says what to do with standard error in the child.
     *  STDERR-FILE may be nil (discard standard error output),
     *  t (mix it with ordinary output), or a file name string.
     *
     * Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
     * Remaining arguments ARGS are strings passed as command arguments to PROGRAM.
     *
     * If PROGRAM is not an absolute file name, `call-process' will look for
     * PROGRAM in `exec-path' (which is a list of directories).
     *
     * If executable PROGRAM can't be found as an executable, `call-process'
     * signals a Lisp error.  `call-process' reports errors in execution of
     * the program only through its return and output.
     *
     * If DESTINATION is 0, `call-process' returns immediately with value nil.
     * Otherwise it waits for PROGRAM to terminate
     * and returns a numeric exit status or a signal description string.
     * If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.
     *
     * The process runs in `default-directory' if that is local (as
     * determined by `unhandled-file-name-directory'), or "~" otherwise.  If
     * you want to run a process in a remote directory use `process-file'.
     *
     * usage: (call-process PROGRAM &amp;optional INFILE DESTINATION DISPLAY &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "call-process", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FCallProcess extends ELispBuiltInBaseNode {
        @Specialization
        @CompilerDirectives.TruffleBoundary
        public Object callProcess(ELispString program, Object[] args) {
            Object input = args.length > 0 ? args[0] : false;
            if (!isNil(input)) {
                throw new UnsupportedOperationException();
            }
            @Nullable ELispBuffer buffer = null;
            if (args.length > 1 && !isNil(args[1])) {
                Object outSpec = args[1];
                if (outSpec instanceof ELispCons cons) {
                    if (!isNil(cons.get(1))) {
                        // TODO: error stream
                        throw new UnsupportedOperationException();
                    }
                    outSpec = cons.get(0);
                }
                buffer = isT(outSpec)
                        ? getContext().currentBuffer()
                        : asBuffer(BuiltInBuffer.FGetBuffer.getBuffer(outSpec));
            }
            boolean redisplay = args.length > 2 && !isNil(args[2]);
            // TODO: redisplay
            String[] commands = new String[1 + Math.max(args.length - 3, 0)];
            commands[0] = program.toString();
            for (int i = 1; i < commands.length; i++) {
                commands[i] = args[i + 2].toString();
            }
            try {
                // TODO: encoding
                Process process = Runtime.getRuntime().exec(commands);
                long ret = process.waitFor();
                if (buffer == null) {
                    return ret;
                }
                try (BufferedReader reader = process.inputReader()) {
                    String line;
                    // TODO: encoding & CR/LF
                    ELispString newline = new ELispString("\n");
                    while ((line = reader.readLine()) != null) {
                        buffer.insert(new ELispString(line));
                        buffer.insert(newline);
                    }
                }
                return ret;
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, program);
            } catch (InterruptedException e) {
                throw ELispSignals.kill(e.getMessage());
            }
        }
    }

    /**
     * <pre>
     * Send text from START to END to a synchronous process running PROGRAM.
     *
     * START and END are normally buffer positions specifying the part of the
     * buffer to send to the process.
     * If START is nil, that means to use the entire buffer contents; END is
     * ignored.
     * If START is a string, then send that string to the process
     * instead of any buffer contents; END is ignored.
     * The remaining arguments are optional.
     * Delete the text if fourth arg DELETE is non-nil.
     *
     * Insert output in BUFFER before point; t means current buffer; nil for
     *  BUFFER means discard it; 0 means discard and don't wait; and `(:file
     *  FILE)', where FILE is a file name string, means that it should be
     *  written to that file (if the file already exists it is overwritten).
     * BUFFER can be a string which is the name of a buffer.
     * BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
     * REAL-BUFFER says what to do with standard output, as above,
     * while STDERR-FILE says what to do with standard error in the child.
     * STDERR-FILE may be nil (discard standard error output),
     * t (mix it with ordinary output), or a file name string.
     *
     * Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
     * Remaining arguments ARGS are passed to PROGRAM at startup as command-line
     * arguments.
     *
     * If PROGRAM is not an absolute file name, `call-process-region' will
     * look for PROGRAM in `exec-path' (which is a list of directories).
     *
     * If BUFFER is 0, `call-process-region' returns immediately with value nil.
     * Otherwise it waits for PROGRAM to terminate
     * and returns a numeric exit status or a signal description string.
     * If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.
     *
     * usage: (call-process-region START END PROGRAM &amp;optional DELETE BUFFER DISPLAY &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "call-process-region", minArgs = 3, maxArgs = 3, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FCallProcessRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void callProcessRegion(Object start, Object end, Object program, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Get the value of environment variable VARIABLE.
     * VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
     * the environment.  Otherwise, value is a string.
     *
     * This function searches `process-environment' for VARIABLE.
     *
     * If optional parameter ENV is a list, then search this list instead of
     * `process-environment', and return t when encountering a negative entry
     * \(an entry for a variable with no value).
     * </pre>
     */
    @ELispBuiltIn(name = "getenv-internal", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetenvInternal extends ELispBuiltInBaseNode {
        @Specialization
        public Object getenvInternal(ELispString variable, Object env) {
            if (!isNil(env)) {
                // TODO: process-environment?
                throw new UnsupportedOperationException();
            }
            String value = getContext().getEnv(variable.asString());
            return value == null ? false : new ELispString(value);
        }
    }
}
