package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.objects.AbstractELispClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.AUTOLOAD;

@GenerateInline
public abstract class FuncallDispatchNode extends Node {

    /// Dispatch a function call
    ///
    /// The `function` argument will be passed to
    /// [party.iroiro.juicemacs.elisp.forms.BuiltInEval.FFuncall#getFunctionObject(Object)].
    ///
    /// @see FunctionDispatchNode
    public abstract Object executeDispatch(Node node, Object function, Object[] arguments);

    protected ELispFunctionObject getFunctionObject(ELispSymbol symbol, Object o) {
        if (o instanceof ELispCons cons && cons.car() == AUTOLOAD) {
            o = symbol;
        }
        return BuiltInEval.FFuncall.getFunctionObject(o);
    }

    protected ELispContext getContext() {
        return ELispContext.get(this);
    }

    /// Dispatch a function call to a symbol
    ///
    /// @see ToFunctionObjectNode
    @Specialization(
            guards = {"symbol == lastSymbol", "o.callTarget() == directCallNode.getCallTarget()"},
            assumptions = "storage.getStableAssumption()",
            limit = "2"
    )
    protected static Object dispatchSymbolDirectly(
            ELispSymbol symbol,
            Object[] arguments,
            @Cached("symbol") ELispSymbol lastSymbol,
            @Cached("getContext().getFunctionStorage(lastSymbol)") FunctionStorage storage,
            @Cached("getFunctionObject(lastSymbol, storage.get())") ELispFunctionObject o,
            @Cached(value = "create(o.callTarget())", inline = false) DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    protected ELispFunctionObject getFunctionObject(ELispSymbol symbol) {
        ELispContext context = ELispContext.get(this);
        return getFunctionObject(symbol, context.getFunctionStorage(symbol).get()); // NOPMD: not recursion
    }

    @Specialization(guards = "o.callTarget() == directCallNode.getCallTarget()")
    protected static Object dispatchSymbol(
            ELispSymbol symbol,
            Object[] arguments,
            @Cached(value = "getFunctionObject(symbol)", neverDefault = true) ELispFunctionObject o,
            @Cached(value = "create(o.callTarget())", inline = false) DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    @Specialization(guards = "closure == oldClosure", limit = "1")
    protected static Object dispatchClosureDirectly(
            AbstractELispClosure closure,
            Object[] arguments,
            @Cached("closure") AbstractELispClosure oldClosure,
            @Cached(value = "create(oldClosure.getFunction().callTarget())", inline = false) DirectCallNode directCallNode
    ) {
        return directCallNode.call(arguments);
    }

    @Specialization(replaces = {"dispatchSymbolDirectly", "dispatchSymbol", "dispatchClosureDirectly"})
    protected static Object dispatchIndirectly(Object function, Object[] arguments, @Cached(inline = false) IndirectCallNode callNode) {
        return callNode.call(BuiltInEval.FFuncall.getFunctionObject(function).callTarget(), arguments);
    }

    @NodeChild(value = "function", type = ELispExpressionNode.class)
    public abstract static class ToFunctionObjectNode extends ELispExpressionNode {
        ELispFunctionObject getFunctionObject(ELispSymbol symbol, Object o) {
            if (o instanceof ELispCons cons && cons.car() == AUTOLOAD) {
                o = symbol;
            }
            return BuiltInEval.FFuncall.getFunctionObject(o);
        }

        @Specialization(assumptions = "storage.getStableAssumption()", guards = "symbol == lastSymbol", limit = "2")
        public ELispFunctionObject stableSymbolToObject(
                ELispSymbol symbol,
                @Cached("symbol") ELispSymbol lastSymbol,
                @Cached("getContext().getFunctionStorage(lastSymbol)") FunctionStorage storage,
                @Cached("getFunctionObject(lastSymbol, storage.get())") ELispFunctionObject o
        ) {
            return o;
        }

        @Specialization
        public ELispFunctionObject symbolToObject(ELispSymbol symbol) {
            return getFunctionObject(symbol, getContext().getFunctionStorage(symbol).get());
        }

        @Specialization
        public ELispFunctionObject closureToFunction(AbstractELispClosure closure) {
            return closure.getFunction();
        }

        @Specialization
        public ELispFunctionObject toFunction(Object function) {
            return BuiltInEval.FFuncall.getFunctionObject(function);
        }
    }
}
