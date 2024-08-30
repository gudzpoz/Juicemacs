package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInEmacs extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEmacsFactory.getFactories();
    }

    @ELispBuiltIn(name = "invocation-name", minArgs = 0, maxArgs = 0, doc = "Return the program name that was used to run Emacs.\nAny directory names are omitted.")
    @GenerateNodeFactory
    public abstract static class FInvocationName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object invocationName() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "invocation-directory", minArgs = 0, maxArgs = 0, doc = "Return the directory name in which the Emacs executable was located.")
    @GenerateNodeFactory
    public abstract static class FInvocationDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object invocationDirectory() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-emacs", minArgs = 0, maxArgs = 2, doc = "Exit the Emacs job and kill it.\nIf ARG is an integer, return ARG as the exit program code.\nIf ARG is a string, stuff it as keyboard input.\nAny other value of ARG, or ARG omitted, means return an\nexit code that indicates successful program termination.\n\nIf RESTART is non-nil, instead of just exiting at the end, start a new\nEmacs process, using the same command line arguments as the currently\nrunning Emacs process.\n\nThis function is called upon receipt of the signals SIGTERM\nor SIGHUP, and upon SIGINT in batch mode.  (Other fatal signals\nshut down Emacs without calling this function.)\n\nThe value of `kill-emacs-hook', if not void, is a list of functions\n(of no args), all of which are called before Emacs is actually\nkilled.")
    @GenerateNodeFactory
    public abstract static class FKillEmacs extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killEmacs(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "dump-emacs", minArgs = 2, maxArgs = 2, doc = "Dump current state of Emacs into executable file FILENAME.\nTake symbols from SYMFILE (presumably the file you executed to run Emacs).\nThis is used in the file `loadup.el' when building Emacs.\n\nYou must run Emacs in batch mode in order to dump it.")
    @GenerateNodeFactory
    public abstract static class FDumpEmacs extends ELispBuiltInBaseNode {
        @Specialization
        public static Object dumpEmacs(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "daemonp", minArgs = 0, maxArgs = 0, doc = "Return non-nil if the current emacs process is a daemon.\nIf the daemon was given a name argument, return that name.")
    @GenerateNodeFactory
    public abstract static class FDaemonp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object daemonp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "daemon-initialized", minArgs = 0, maxArgs = 0, doc = "Mark the Emacs daemon as being initialized.\nThis finishes the daemonization process by doing the other half of detaching\nfrom the parent process and its tty file descriptors.")
    @GenerateNodeFactory
    public abstract static class FDaemonInitialized extends ELispBuiltInBaseNode {
        @Specialization
        public static Object daemonInitialized() {
            throw new UnsupportedOperationException();
        }
    }
}
