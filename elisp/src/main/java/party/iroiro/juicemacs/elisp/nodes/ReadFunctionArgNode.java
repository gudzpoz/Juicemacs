package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;

public class ReadFunctionArgNode extends ELispExpressionNode {

    protected final int index;

    public ReadFunctionArgNode(int index) {
        this.index = index;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        Object[] arguments = frame.getArguments();
        return index < arguments.length ? arguments[this.index] : false;
    }

    public static sealed class ArgCountVerificationNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode function;
        private final int minArgs;
        private final int maxArgs;

        public ArgCountVerificationNode(ELispExpressionNode function, int minArgs, int maxArgs) {
            this.function = function;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
            adoptChildren();
        }

        @CompilerDirectives.TruffleBoundary
        private ELispSignals.ELispSignalException wrongNumberOfArguments(int actual) {
            Object functionInfo = false;
            if (function instanceof ELispBuiltInBaseNode) {
                ELispBuiltIn annotation = function.getClass().getSuperclass().getAnnotation(ELispBuiltIn.class);
                if (annotation != null) {
                    functionInfo = ELispContext.intern(annotation.name());
                }
            } else if (function instanceof ELispInterpretedClosure.ELispClosureCallNode closure) {
                functionInfo = closure.getClosure().getName();
            }
            return ELispSignals.wrongNumberOfArguments(functionInfo, actual);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            int length = frame.getArguments().length;
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.SLOWPATH_PROBABILITY,
                    length < minArgs || (0 <= maxArgs && maxArgs < length)
            )) {
                throw wrongNumberOfArguments(length);
            }
            return function.executeGeneric(frame);
        }

        @Override
        public SourceSection getSourceSection() {
            return function.getSourceSection();
        }
    }

    public static final class DslExceptionRemapNode extends ArgCountVerificationNode {
        public DslExceptionRemapNode(ELispExpressionNode function, int minArgs, int maxArgs) {
            super(function, minArgs, maxArgs);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try {
                return super.executeGeneric(frame);
            } catch (RuntimeException e) {
                CompilerDirectives.transferToInterpreter();
                throw remapException(e);
            }
        }
    }

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
            ELispCons cons = new ELispCons(arguments[0]);
            ELispCons tail = cons;
            for (int i = 1; i < arguments.length; i++) {
                tail.setCdr(new ELispCons(arguments[i]));
                tail = (ELispCons) tail.cdr();
            }
            return cons;
        }
    }
}
