package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

@GenerateInline
@GenerateCached(value = false)
public abstract class FunctionDispatchNode extends Node {

    public abstract Object executeDispatch(Node node, Object function, Object[] arguments);

    @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
    protected static Object dispatchDirectly(
            ELispFunctionObject function,
            Object[] arguments,
            @Cached(value = "create(function.callTarget())", inline = false) DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    @Specialization(replaces = "dispatchDirectly")
    protected static Object dispatchIndirectly(
            ELispFunctionObject function,
            Object[] arguments,
            @Cached(inline = false) IndirectCallNode indirectCallNode) {
        return indirectCallNode.call(function.callTarget(), arguments);
    }

    @Fallback
    protected static Object targetIsNotAFunction(Object nonFunction, Object[] arguments) {
        throw ELispSignals.invalidFunction("Not a function");
    }

    public static ELispExpressionNode createSpecializedCallNode(ELispExpressionNode[] args) {
        return switch (args.length - 1) {
            case 0 -> FunctionDispatchNodeGen.Call0NodeGen.create(args);
            case 1 -> FunctionDispatchNodeGen.Call1NodeGen.create(args);
            case 2 -> FunctionDispatchNodeGen.Call2NodeGen.create(args);
            case 3 -> FunctionDispatchNodeGen.Call3NodeGen.create(args);
            case 4 -> FunctionDispatchNodeGen.Call4NodeGen.create(args);
            case 5 -> FunctionDispatchNodeGen.Call5NodeGen.create(args);
            default -> FunctionDispatchNodeGen.CallNNodeGen.create(args);
        };
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class Call0Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call();
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget());
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class Call1Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class Call2Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class Call3Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2, arg3);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2, arg3);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class Call4Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2, arg3, arg4);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2, arg3, arg4);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class Call5Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                Object arg5,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2, arg3, arg4, arg5);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                Object arg5,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2, arg3, arg4, arg5);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    public abstract static class CallNNode extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object[] args,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(args);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object[] args,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), args);
        }
    }

}
