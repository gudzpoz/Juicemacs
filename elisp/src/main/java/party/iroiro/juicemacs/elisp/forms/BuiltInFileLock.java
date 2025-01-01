package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInFileLock extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFileLockFactory.getFactories();
    }

    /**
     * <pre>
     * Check whether FILE was modified since it was visited, and lock it.
     * If user option `create-lockfiles' is nil, this does not create
     * a lock file for FILE, but it still checks whether FILE was modified
     * outside of the current Emacs session, and if so, asks the user
     * whether to modify FILE.
     * </pre>
     */
    @ELispBuiltIn(name = "lock-file", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLockFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lockFile(Object file) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Unlock FILE.
     * </pre>
     */
    @ELispBuiltIn(name = "unlock-file", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUnlockFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unlockFile(Object file) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Lock FILE, if current buffer is modified.
     * FILE defaults to current buffer's visited file,
     * or else nothing is done if current buffer isn't visiting a file.
     *
     * If the option `create-lockfiles' is nil, this does nothing.
     * </pre>
     */
    @ELispBuiltIn(name = "lock-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLockBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lockBuffer(Object file) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Unlock the file visited in the current buffer.
     * If the buffer is not modified, this does nothing because the file
     * should not be locked in that case.  It also does nothing if the
     * current buffer is not visiting a file, or is not locked.  Handles file
     * system errors by calling `display-warning' and continuing as if the
     * error did not occur.
     * </pre>
     */
    @ELispBuiltIn(name = "unlock-buffer", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUnlockBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unlockBuffer() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a value indicating whether FILENAME is locked.
     * The value is nil if the FILENAME is not locked,
     * t if it is locked by you, else a string saying which user has locked it.
     * </pre>
     */
    @ELispBuiltIn(name = "file-locked-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileLockedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileLockedP(Object filename) {
            throw new UnsupportedOperationException();
        }
    }
}
