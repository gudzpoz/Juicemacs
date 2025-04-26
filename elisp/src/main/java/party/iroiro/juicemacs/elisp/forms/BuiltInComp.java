package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

/**
 * Built-in functions from {@code src/comp.c}
 */
public class BuiltInComp extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCompFactory.getFactories();
    }

    /**
     * <pre>
     * Support function to hash_native_abi.
     * For internal use.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--subr-signature", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompSubrSignature extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compSubrSignature(Object subr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the relative name of the .eln file for FILENAME.
     * FILENAME must exist, and if it's a symlink, the target must exist.
     * If FILENAME is compressed, it must have the \".gz\" extension,
     * and Emacs must have been compiled with zlib; the file will be
     * uncompressed on the fly to hash its contents.
     * Value includes the original base name, followed by 2 hash values,
     * one for the file name and another for its contents, followed by .eln.
     * </pre>
     */
    @ELispBuiltIn(name = "comp-el-to-eln-rel-filename", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompElToElnRelFilename extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compElToElnRelFilename(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the absolute .eln file name for source FILENAME.
     * The resulting .eln file name is intended to be used for natively
     * compiling FILENAME.  FILENAME must exist and be readable, but other
     * than that, its leading directories are ignored when constructing
     * the name of the .eln file.
     * If BASE-DIR is non-nil, use it as the directory for the .eln file;
     * non-absolute BASE-DIR is interpreted as relative to `invocation-directory'.
     * If BASE-DIR is omitted or nil, look for the first writable directory
     * in `native-comp-eln-load-path', and use as BASE-DIR its subdirectory
     * whose name is given by `comp-native-version-dir'.
     * If FILENAME specifies a preloaded file, the directory for the .eln
     * file is the \"preloaded/\" subdirectory of the directory determined
     * as described above.  FILENAME is considered to be a preloaded file if
     * the value of `comp-file-preloaded-p' is non-nil, or if FILENAME
     * appears in the value of the environment variable LISP_PRELOADED;
     * the latter is supposed to be used by the Emacs build procedure.
     * </pre>
     */
    @ELispBuiltIn(name = "comp-el-to-eln-filename", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCompElToElnFilename extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compElToElnFilename(Object filename, Object baseDir) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Install a TRAMPOLINE for primitive SUBR-NAME.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--install-trampoline", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCompInstallTrampoline extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compInstallTrampoline(Object subrName, Object trampoline) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Initialize the native compiler context.
     * Return t on success.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--init-ctxt", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompInitCtxt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compInitCtxt() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Release the native compiler context.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--release-ctxt", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompReleaseCtxt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compReleaseCtxt() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if `comp-native-driver-options' is effective.
     * </pre>
     */
    @ELispBuiltIn(name = "comp-native-driver-options-effective-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompNativeDriverOptionsEffectiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compNativeDriverOptionsEffectiveP() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if `comp-native-compiler-options' is effective.
     * </pre>
     */
    @ELispBuiltIn(name = "comp-native-compiler-options-effective-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompNativeCompilerOptionsEffectiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compNativeCompilerOptionsEffectiveP() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Compile the current context as native code to file FILENAME.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--compile-ctxt-to-file0", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompCompileCtxtToFile0 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compCompileCtxtToFile0(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return libgccjit version in use.
     *
     * The return value has the form (MAJOR MINOR PATCHLEVEL) or nil if
     * unknown (before GCC version 10).
     * </pre>
     */
    @ELispBuiltIn(name = "comp-libgccjit-version", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompLibgccjitVersion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compLibgccjitVersion() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Register anonymous lambda.
     * This gets called by top_level_run during the load phase.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--register-lambda", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompRegisterLambda extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compRegisterLambda(Object relocIdx, Object cName, Object minarg, Object maxarg, Object type, Object rest, Object compU) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Register exported subr.
     * This gets called by top_level_run during the load phase.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--register-subr", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompRegisterSubr extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compRegisterSubr(Object name, Object cName, Object minarg, Object maxarg, Object type, Object rest, Object compU) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Register exported subr.
     * This gets called by late_top_level_run during the load phase.
     * </pre>
     */
    @ELispBuiltIn(name = "comp--late-register-subr", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompLateRegisterSubr extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compLateRegisterSubr(Object name, Object cName, Object minarg, Object maxarg, Object type, Object rest, Object compU) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Load native elisp code FILENAME.
     * LATE-LOAD has to be non-nil when loading for deferred compilation.
     * </pre>
     */
    @ELispBuiltIn(name = "native-elisp-load", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNativeElispLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nativeElispLoad(Object filename, Object lateLoad) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if native compilation support is built-in.
     * </pre>
     */
    @ELispBuiltIn(name = "native-comp-available-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FNativeCompAvailableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nativeCompAvailableP() {
            return false;
        }
    }
}
