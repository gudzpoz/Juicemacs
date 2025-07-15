package party.iroiro.juicemacs.elisp.nodes.funcall;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;

import static party.iroiro.juicemacs.elisp.runtime.ELispSignals.remapException;

public class ReadFunctionArgNode extends ELispExpressionNode {

    protected final int index;

    public ReadFunctionArgNode(int argIndex) {
        // arg[0] stores the function object and real args start from index 1
        this.index = argIndex + 1;
    }

    @Override
    public void executeVoid(VirtualFrame frame) {
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        Object[] arguments = frame.getArguments();
        return index < arguments.length ? arguments[this.index] : false;
    }

    public static class ArgCountVerificationNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode function;
        private final int minArgs;
        private final int maxArgs;

        public ArgCountVerificationNode(ELispExpressionNode function, int minArgs, int maxArgs) {
            this.function = function;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        @CompilerDirectives.TruffleBoundary
        private ELispSignals.ELispSignalException wrongNumberOfArguments(int actual) {
            Object functionInfo = false;
            if (function instanceof ELispBuiltInBaseNode) {
                ELispBuiltIn annotation = function.getClass().getSuperclass().getAnnotation(ELispBuiltIn.class);
                if (annotation != null) {
                    functionInfo = getContext().intern(annotation.name());
                }
            } else if (function instanceof ELispInterpretedClosure.ELispClosureCallNode closure) {
                functionInfo = closure.getClosure().getName();
            }
            return ELispSignals.wrongNumberOfArguments(functionInfo, actual);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            super.executeVoid(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            int length = frame.getArguments().length - 1;
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.SLOWPATH_PROBABILITY,
                    length < minArgs || (0 <= maxArgs && maxArgs < length)
            )) {
                throw wrongNumberOfArguments(length);
            }
            try {
                return function.executeGeneric(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw remapException(e, this.function);
            }
        }

        @Override
        public SourceSection getSourceSection() {
            return function.getSourceSection();
        }
    }

    @SuppressWarnings("PMD.TruffleNodeMissingExecuteVoid")
    public static sealed class ReadFunctionRestArgsNode extends ReadFunctionArgNode {
        public ReadFunctionRestArgsNode(int index) {
            super(index);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Object[] arguments = frame.getArguments();
            if (index >= arguments.length) {
                return new Object[0];
            }
            Object[] varArgs = new Object[arguments.length - index];
            System.arraycopy(arguments, index, varArgs, 0, arguments.length - index);
            return varArgs;
        }
    }

    @SuppressWarnings("PMD.TruffleNodeMissingExecuteVoid")
    public static final class ReadFunctionRestArgsAsConsNode extends ReadFunctionRestArgsNode {
        public ReadFunctionRestArgsAsConsNode(int index) {
            super(index);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Object[] arguments = (Object[]) super.executeGeneric(frame);
            if (arguments.length == 0) {
                return false;
            }
            return ELispCons.listOf(arguments);
        }
    }
}
