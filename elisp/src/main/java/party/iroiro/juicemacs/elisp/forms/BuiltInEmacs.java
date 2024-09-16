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

    @ELispBuiltIn(name = "invocation-name", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FInvocationName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object invocationName() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "invocation-directory", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FInvocationDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object invocationDirectory() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-emacs", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FKillEmacs extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killEmacs(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "dump-emacs", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpEmacs extends ELispBuiltInBaseNode {
        @Specialization
        public static Object dumpEmacs(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "daemonp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDaemonp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object daemonp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "daemon-initialized", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDaemonInitialized extends ELispBuiltInBaseNode {
        @Specialization
        public static Object daemonInitialized() {
            throw new UnsupportedOperationException();
        }
    }
}
