package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNode;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBytecode;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInData.isMultibyte;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;

public class BuiltInBytecode extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInBytecodeFactory.getFactories();
    }

    /**
     * <pre>
     * Function used internally in byte-compiled code.
     * The first argument, BYTESTR, is a string of byte code;
     * the second, VECTOR, a vector of constants;
     * the third, MAXDEPTH, the maximum stack depth used in this function.
     * If the third argument is incorrect, Emacs may crash.
     * </pre>
     */
    @ELispBuiltIn(name = "byte-code", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FByteCode extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @TruffleBoundary
        @Specialization
        public Object byteCode(ELispString bytestr, ELispVector vector, long maxdepth) {
            if (isMultibyte(bytestr)) {
                throw ELispSignals.wrongTypeArgument(BYTE_CODE_FUNCTION_P, bytestr);
            }
            ELispBytecode f = BuiltInAlloc.FMakeByteCode.makeByteCode(
                    false, bytestr, vector, maxdepth, new Object[0], this
            );
            f.setName("byte-code@" + System.identityHashCode(f));
            return FuncallDispatchNode.dispatchArgsUncached(this, f);
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return BuiltInBytecodeFactory.FByteCodeFactory.create(arguments);
        }
    }

    /**
     * <pre>
     * internal
     * </pre>
     */
    @ELispBuiltIn(name = "internal-stack-stats", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FInternalStackStats extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalStackStats() {
            throw new UnsupportedOperationException();
        }
    }
}
