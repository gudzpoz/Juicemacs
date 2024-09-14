package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

public class ReadFunctionArgNode extends ELispExpressionNode {

    protected final int index;
    private final boolean required;

    public ReadFunctionArgNode(int index, boolean required) {
        this.index = index;
        this.required = required;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        Object[] arguments = frame.getArguments();
        if (required && arguments.length <= index) {
            throw new IllegalArgumentException();
        }
        return index < arguments.length ? arguments[this.index] : false;
    }

    public static class ReadFunctionRestArgsNode extends ReadFunctionArgNode {
        public ReadFunctionRestArgsNode(int index) {
            super(index, false);
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

    public static class ReadFunctionRestArgsAsConsNode extends ReadFunctionRestArgsNode {
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
