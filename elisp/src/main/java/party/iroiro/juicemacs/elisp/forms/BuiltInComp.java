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

    @ELispBuiltIn(name = "comp--subr-signature", minArgs = 1, maxArgs = 1, doc = "Support function to hash_native_abi.\nFor internal use.")
    @GenerateNodeFactory
    public abstract static class FCompSubrSignature extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compSubrSignature(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-el-to-eln-rel-filename", minArgs = 1, maxArgs = 1, doc = "Return the relative name of the .eln file for FILENAME.\nFILENAME must exist, and if it's a symlink, the target must exist.\nIf FILENAME is compressed, it must have the \\\".gz\\\" extension,\nand Emacs must have been compiled with zlib; the file will be\nuncompressed on the fly to hash its contents.\nValue includes the original base name, followed by 2 hash values,\none for the file name and another for its contents, followed by .eln.")
    @GenerateNodeFactory
    public abstract static class FCompElToElnRelFilename extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compElToElnRelFilename(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-el-to-eln-filename", minArgs = 1, maxArgs = 2, doc = "Return the absolute .eln file name for source FILENAME.\nThe resulting .eln file name is intended to be used for natively\ncompiling FILENAME.  FILENAME must exist and be readable, but other\nthan that, its leading directories are ignored when constructing\nthe name of the .eln file.\nIf BASE-DIR is non-nil, use it as the directory for the .eln file;\nnon-absolute BASE-DIR is interpreted as relative to `invocation-directory'.\nIf BASE-DIR is omitted or nil, look for the first writable directory\nin `native-comp-eln-load-path', and use as BASE-DIR its subdirectory\nwhose name is given by `comp-native-version-dir'.\nIf FILENAME specifies a preloaded file, the directory for the .eln\nfile is the \\\"preloaded/\\\" subdirectory of the directory determined\nas described above.  FILENAME is considered to be a preloaded file if\nthe value of `comp-file-preloaded-p' is non-nil, or if FILENAME\nappears in the value of the environment variable LISP_PRELOADED;\nthe latter is supposed to be used by the Emacs build procedure.")
    @GenerateNodeFactory
    public abstract static class FCompElToElnFilename extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compElToElnFilename(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--install-trampoline", minArgs = 2, maxArgs = 2, doc = "Install a TRAMPOLINE for primitive SUBR-NAME.")
    @GenerateNodeFactory
    public abstract static class FCompInstallTrampoline extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compInstallTrampoline(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--init-ctxt", minArgs = 0, maxArgs = 0, doc = "Initialize the native compiler context.\nReturn t on success.")
    @GenerateNodeFactory
    public abstract static class FCompInitCtxt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compInitCtxt() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--release-ctxt", minArgs = 0, maxArgs = 0, doc = "Release the native compiler context.")
    @GenerateNodeFactory
    public abstract static class FCompReleaseCtxt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compReleaseCtxt() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-native-driver-options-effective-p", minArgs = 0, maxArgs = 0, doc = "Return t if `comp-native-driver-options' is effective.")
    @GenerateNodeFactory
    public abstract static class FCompNativeDriverOptionsEffectiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compNativeDriverOptionsEffectiveP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-native-compiler-options-effective-p", minArgs = 0, maxArgs = 0, doc = "Return t if `comp-native-compiler-options' is effective.")
    @GenerateNodeFactory
    public abstract static class FCompNativeCompilerOptionsEffectiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compNativeCompilerOptionsEffectiveP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--compile-ctxt-to-file0", minArgs = 1, maxArgs = 1, doc = "Compile the current context as native code to file FILENAME.")
    @GenerateNodeFactory
    public abstract static class FCompCompileCtxtToFile0 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compCompileCtxtToFile0(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-libgccjit-version", minArgs = 0, maxArgs = 0, doc = "Return libgccjit version in use.\n\nThe return value has the form (MAJOR MINOR PATCHLEVEL) or nil if\nunknown (before GCC version 10).")
    @GenerateNodeFactory
    public abstract static class FCompLibgccjitVersion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compLibgccjitVersion() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--register-lambda", minArgs = 7, maxArgs = 7, doc = "Register anonymous lambda.\nThis gets called by top_level_run during the load phase.")
    @GenerateNodeFactory
    public abstract static class FCompRegisterLambda extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compRegisterLambda(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--register-subr", minArgs = 7, maxArgs = 7, doc = "Register exported subr.\nThis gets called by top_level_run during the load phase.")
    @GenerateNodeFactory
    public abstract static class FCompRegisterSubr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compRegisterSubr(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--late-register-subr", minArgs = 7, maxArgs = 7, doc = "Register exported subr.\nThis gets called by late_top_level_run during the load phase.")
    @GenerateNodeFactory
    public abstract static class FCompLateRegisterSubr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compLateRegisterSubr(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-elisp-load", minArgs = 1, maxArgs = 2, doc = "Load native elisp code FILENAME.\nLATE-LOAD has to be non-nil when loading for deferred compilation.")
    @GenerateNodeFactory
    public abstract static class FNativeElispLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeElispLoad(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-available-p", minArgs = 0, maxArgs = 0, doc = "Return non-nil if native compilation support is built-in.")
    @GenerateNodeFactory
    public abstract static class FNativeCompAvailableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompAvailableP() {
            throw new UnsupportedOperationException();
        }
    }
}
