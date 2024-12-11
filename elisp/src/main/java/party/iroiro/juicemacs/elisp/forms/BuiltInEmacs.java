package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.File;
import java.util.List;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.SAFE_MAGIC;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public class BuiltInEmacs extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEmacsFactory.getFactories();
    }

    /// Reads an env variable that is list of paths separated by the platform's path separator,
    /// and converts it into a list of strings
    ///
    /// @param envVarName the name of the environment variable to decode
    /// @param defaultValue default value to use if the environment variable is not set
    /// @param empty when `true`, returns `nil` for empty paths; ".", otherwise
    /// @return a list of strings
    public static ELispCons decodeEnvPath(@Nullable String envVarName, String defaultValue, boolean empty) {
        @Nullable ELispString emptyElement = empty ? null : new ELispString(".");
        String path = Objects.requireNonNullElse(envVarName == null ? null : ELispLanguage.getEnv().get(envVarName), defaultValue);
        ELispCons.ListBuilder paths = new ELispCons.ListBuilder();
        for (String element : path.split(File.pathSeparator)) {
            ELispString current = element.isEmpty() ? emptyElement : new ELispString(element);
            if (current != null) {
                Object handler = BuiltInFileIO.FFindFileNameHandler.findFileNameHandler(current, true);
                if (handler instanceof ELispSymbol symbol) {
                    if (!isNil(symbol.getProperty(SAFE_MAGIC))) {
                        handler = false;
                    }
                }
                if (!isNil(handler)) {
                    current = BuiltInFns.FConcat.concat(new Object[]{new ELispString("/:"), current});
                }
            }
            paths.add(Objects.requireNonNullElse(current, false));
        }
        return asCons(paths.build());
    }

    /**
     * <pre>
     * Return the program name that was used to run Emacs.
     * Any directory names are omitted.
     * </pre>
     */
    @ELispBuiltIn(name = "invocation-name", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FInvocationName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void invocationName() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the directory name in which the Emacs executable was located.
     * </pre>
     */
    @ELispBuiltIn(name = "invocation-directory", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FInvocationDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Void invocationDirectory() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Exit the Emacs job and kill it.
     * If ARG is an integer, return ARG as the exit program code.
     * If ARG is a string, stuff it as keyboard input.
     * Any other value of ARG, or ARG omitted, means return an
     * exit code that indicates successful program termination.
     *
     * If RESTART is non-nil, instead of just exiting at the end, start a new
     * Emacs process, using the same command line arguments as the currently
     * running Emacs process.
     *
     * This function is called upon receipt of the signals SIGTERM
     * or SIGHUP, and upon SIGINT in batch mode.  (Other fatal signals
     * shut down Emacs without calling this function.)
     *
     * The value of `kill-emacs-hook', if not void, is a list of functions
     * (of no args), all of which are called before Emacs is actually
     * killed.
     * </pre>
     */
    @ELispBuiltIn(name = "kill-emacs", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FKillEmacs extends ELispBuiltInBaseNode {
        @Specialization
        public static Void killEmacs(Object arg, Object restart) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Dump current state of Emacs into executable file FILENAME.
     * Take symbols from SYMFILE (presumably the file you executed to run Emacs).
     * This is used in the file `loadup.el' when building Emacs.
     *
     * You must run Emacs in batch mode in order to dump it.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-emacs", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpEmacs extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpEmacs(Object filename, Object symfile) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if the current emacs process is a daemon.
     * If the daemon was given a name argument, return that name.
     * </pre>
     */
    @ELispBuiltIn(name = "daemonp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDaemonp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void daemonp() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Mark the Emacs daemon as being initialized.
     * This finishes the daemonization process by doing the other half of detaching
     * from the parent process and its tty file descriptors.
     * </pre>
     */
    @ELispBuiltIn(name = "daemon-initialized", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDaemonInitialized extends ELispBuiltInBaseNode {
        @Specialization
        public static Void daemonInitialized() {
            throw new UnsupportedOperationException();
        }
    }
}
