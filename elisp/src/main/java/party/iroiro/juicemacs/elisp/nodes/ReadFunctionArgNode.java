package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;

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
}
