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

    @ELispBuiltIn(name = "comp--subr-signature", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompSubrSignature extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compSubrSignature(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-el-to-eln-rel-filename", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompElToElnRelFilename extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compElToElnRelFilename(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-el-to-eln-filename", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCompElToElnFilename extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compElToElnFilename(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--install-trampoline", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCompInstallTrampoline extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compInstallTrampoline(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--init-ctxt", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompInitCtxt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compInitCtxt() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--release-ctxt", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompReleaseCtxt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compReleaseCtxt() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-native-driver-options-effective-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompNativeDriverOptionsEffectiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compNativeDriverOptionsEffectiveP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-native-compiler-options-effective-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompNativeCompilerOptionsEffectiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compNativeCompilerOptionsEffectiveP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--compile-ctxt-to-file0", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompCompileCtxtToFile0 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compCompileCtxtToFile0(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp-libgccjit-version", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCompLibgccjitVersion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compLibgccjitVersion() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--register-lambda", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompRegisterLambda extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compRegisterLambda(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--register-subr", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompRegisterSubr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compRegisterSubr(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "comp--late-register-subr", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompLateRegisterSubr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compLateRegisterSubr(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-elisp-load", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNativeElispLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeElispLoad(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-available-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FNativeCompAvailableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompAvailableP() {
            throw new UnsupportedOperationException();
        }
    }
}
