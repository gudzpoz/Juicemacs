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

}
