package party.iroiro.juicemacs.elisp.nodes.funcall;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.AbstractELispClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;

/// A call node that calls function objects like [ELispSubroutine] or [AbstractELispClosure]
///
/// One should probably use this node along with [ReadFunctionObjectNodes].
@GenerateInline
@GenerateUncached
public abstract class FunctionObjectCallNode extends Node {
    public abstract Object executeCall(Node node, Object symbol, Object[] args);

    public static Object getFunctionSlot(Object[] args) {
        return args[0];
    }

    @Specialization(guards = "lastFunction == getFunctionSlot(args)", limit = "1")
    static Object callCached(
            Node node,
            Object symbol,
            Object[] args,
            @Cached(value = "getFunctionSlot(args)", allowUncached = true) Object lastFunction,
            @Cached @Cached.Shared ToFunctionObjectNode toFunction,
            @Cached(value = "toFunction.execute($node, symbol, lastFunction)", allowUncached = true)
            ELispFunctionObject object,
            @Cached @Cached.Shared FunctionDispatchNode dispatchNode
    ) {
        return dispatchNode.executeDispatch(node, object, args);
    }

    @Specialization
    static Object callUncached(
            Node node,
            Object symbol,
            Object[] args,
            @Cached @Cached.Shared ToFunctionObjectNode toFunction,
            @Cached @Cached.Shared FunctionDispatchNode dispatchNode
    ) {
        ELispFunctionObject object = toFunction.execute(node, symbol, args[0]);
        return dispatchNode.executeDispatch(node, object, args);
    }

    @GenerateInline
    @GenerateUncached
    public abstract static class ToFunctionObjectNode extends Node {
        public abstract ELispFunctionObject execute(Node node, Object errorSymbol, Object function);

        @Specialization(guards = "cached == closure", limit = "1")
        public static ELispFunctionObject getClosure(
                Object symbol,
                AbstractELispClosure closure,
                @Cached(value = "closure", allowUncached = true) AbstractELispClosure cached,
                @Cached(value = "cached.getFunction()", allowUncached = true) ELispFunctionObject object
        ) {
            return object;
        }

        @Specialization(guards = "cached == subroutine", limit = "1")
        public static ELispFunctionObject getSubroutine(
                Object symbol,
                ELispSubroutine subroutine,
                @Cached(value = "subroutine", allowUncached = true) ELispSubroutine cached,
                @Cached(value = "cached.body()", allowUncached = true) ELispFunctionObject object
        ) {
            return object;
        }

        @Specialization(replaces = {"getClosure", "getSubroutine"})
        public static ELispFunctionObject getFunction(Object symbol, Object function) {
            return switch (function) {
                case AbstractELispClosure closure -> closure.getFunction();
                case ELispSubroutine subroutine -> subroutine.body();
                default -> throw ELispSignals.invalidFunction(function, symbol);
            };
        }
    }
}
