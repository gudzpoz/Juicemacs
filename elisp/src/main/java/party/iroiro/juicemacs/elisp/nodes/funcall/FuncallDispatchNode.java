package party.iroiro.juicemacs.elisp.nodes.funcall;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;

@GenerateInline
@GenerateUncached
public abstract class FuncallDispatchNode extends Node {
    public abstract Object executeDispatch(@Nullable Node node, Object[] args);
    public Object dispatch(@Nullable Node node, Object... args) {
        return executeDispatch(node, args);
    }
    public Object dispatchArgs(@Nullable Node node, Object function, Object[] args) {
        Object[] objects = new Object[args.length + 1];
        objects[0] = function;
        System.arraycopy(args, 0, objects, 1, args.length);
        return executeDispatch(node, objects);
    }

    @Specialization
    public static Object doDispatch(
            Node node,
            Object[] args,
            @Cached ReadFunctionObjectNodes.GetDynamicSymbolFunctionNode symbolGet,
            @Cached ReadFunctionObjectNodes.ConvertFunctionNode functionConvert,
            @Cached FunctionObjectCallNode callNode
    ) {
        Object car = args[0];
        args[0] = functionConvert.executeConvert(node, symbolGet.execute(node, car));
        return callNode.executeCall(node, car, args);
    }

    public static Object dispatchArgsUncached(@Nullable Node node, Object function, Object... arguments) {
        Object[] args = new Object[arguments.length + 1];
        args[0] = function;
        System.arraycopy(arguments, 0, args, 1, arguments.length);
        return FuncallDispatchNodeGen.getUncached().executeDispatch(node, args);
    }

    public static ELispExpressionNode createSpecializedCallNode(ELispExpressionNode[] args) {
        return switch (args.length - 1) {
            case 0 -> FuncallDispatchNodeGen.Call0NodeGen.create(args);
            case 1 -> FuncallDispatchNodeGen.Call1NodeGen.create(args);
            default -> FuncallDispatchNodeGen.CallNNodeGen.create(new ToArrayNode(args));
        };
    }

    public static final class ToArrayNode extends ELispExpressionNode {
        @Children
        final ELispExpressionNode[] nodes;

        public ToArrayNode(ELispExpressionNode[] nodes) {
            this.nodes = nodes;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            super.executeVoid(frame);
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Object[] arguments = new Object[nodes.length];
            for (int i = 0; i < nodes.length; i++) {
                arguments[i] = nodes[i].executeGeneric(frame);
            }
            return arguments;
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call0Node extends ELispExpressionNode {
        @Specialization
        protected Object dispatch(
                Object function,
                @Cached(inline = true) FuncallDispatchNode callNode
        ) {
            return callNode.executeDispatch(this, new Object[]{function});
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call1Node extends ELispExpressionNode {
        @Specialization
        protected Object dispatch(
                Object function,
                Object arg,
                @Cached(inline = true) FuncallDispatchNode callNode
        ) {
            return callNode.executeDispatch(this, new Object[]{function, arg});
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode.class)
    public abstract static class CallNNode extends ELispExpressionNode {
        @Specialization
        protected Object dispatch(
                Object[] args,
                @Cached(inline = true) FuncallDispatchNode callNode
        ) {
            return callNode.executeDispatch(this, args);
        }
    }
}
