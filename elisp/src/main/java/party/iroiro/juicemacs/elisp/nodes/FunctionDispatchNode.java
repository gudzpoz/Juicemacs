package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

public abstract class FunctionDispatchNode extends Node {

    public abstract Object executeDispatch(Object function, Object[] arguments);

    @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
    protected static Object dispatchDirectly(
            ELispFunctionObject function,
            Object[] arguments,
            @Cached("create(function.callTarget())") DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    @Specialization(replaces = "dispatchDirectly")
    protected static Object dispatchIndirectly(
            ELispFunctionObject function,
            Object[] arguments,
            @Cached IndirectCallNode indirectCallNode) {
        return indirectCallNode.call(function.callTarget(), arguments);
    }

    @Fallback
    protected static Object targetIsNotAFunction(Object nonFunction, Object[] arguments) {
        throw new IllegalArgumentException("Not a function");
    }

}
