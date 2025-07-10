package party.iroiro.juicemacs.elisp.nodes.funcall;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInLRead;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// Provide function values for all kinds of function calls
///
/// ## What kinds of function calls?
///
/// So in ELisp there are a lot of ways to call a function:
///
/// - Lisp forms, which include the following:
///   - A form like `(function-symbol args)` (the most common one)
///   - A form like `((lambda (args) ...) args)`
///   - A form like `(#[lisp-closure] args)`
///   - A form like `(#[bytecode-closure] args)`
/// - `CALL` bytecodes, which also accept both symbols and function objects
///   as arguments
/// - Indirect calls like `apply` and `funcall`
/// - Utility functions like `mapc`, `mapcar`, etc.
///
/// We need different handling for different callee:
/// - A symbol:
///   - Aliases: a symbol can point to another symbol.
///   - Autoload: the function is like `(autoload FILE)`, meaning that we
///     should load it from `FILE` before execution.
/// - A cons like `(lambda () ...)`: it is a cons, so we need to convert it
///   to an actual interpreted function before executing, which we should cache.
///
/// ## How?
///
/// We separate the implementation into two nodes.
///
/// 1. Symbol indirection node: cache symbol function values and handle autoload.
///    See [GetSymbolFunctionNode].
/// 2. Closure cache node: handle `(lambda ())` conses and cache the value.
///    See [ConvertFunctionNode].
public abstract class ReadFunctionObjectNodes {
    private ReadFunctionObjectNodes() {}

    public static Object getFunctionUncached(@Nullable Node node, Object function) {
        GetSymbolFunctionNode symbolGet = ReadFunctionObjectNodesFactory.GetDynamicSymbolFunctionNodeGen.getUncached();
        ConvertFunctionNode convertFunction = ReadFunctionObjectNodesFactory.ConvertFunctionNodeGen.getUncached();
        //noinspection DataFlowIssue
        return convertFunction.executeConvert(node, symbolGet.execute(node, function));
    }

    /// Creates a node to read function objects for lisp forms like `(func args)`
    ///
    /// It is special because if `func` is `(lambda () body)`, it will capture the
    /// current lexical environment. This is different from when a symbol contains
    /// a `(lambda () body)` definition.
    public static ELispExpressionNode createFormCardinal(Object car) {
        if (car instanceof ELispCons cons) {
            if (cons.car() == LAMBDA) {
                return BuiltInEval.FFunction.function(cons);
            }
        } else if (toSym(car) instanceof ELispSymbol symbol) {
            return ReadFunctionObjectNodesFactory.ReadFormSymbolCardinalFunctionNodeGen.create(symbol);
        }
        return ReadFunctionObjectNodesFactory.ReadDynamicSymbolNodeGen.create(ELispInterpretedNode.literal(car));
    }

    public abstract static class GetSymbolFunctionNode extends Node {
        public abstract Object execute(Node node, Object symbol);

        public static FunctionStorage getStorage(Node node, ELispSymbol symbol) {
            return ELispContext.get(node).getFunctionStorage(symbol);
        }
        public static Object getFunction(Node node, ELispSymbol symbol) {
            return ELispContext.get(node).getFunctionStorage(symbol).get();
        }
        public static Object doAutoload(Node node, ELispCons cons, ELispSymbol symbol) {
            CompilerDirectives.transferToInterpreter();
            ELispString file = asStr(asCons(cons.cdr()).car());
            BuiltInLRead.loadFile(ELispLanguage.get(node), node, file, true);
            return symbol.getIndirectFunction();
        }
        @Idempotent
        public static boolean noRedirect(Object function) {
            return !(function instanceof ELispSymbol) && !(function instanceof ELispCons);
        }
    }

    @GenerateInline
    public abstract static class GetStaticSymbolFunctionNode extends GetSymbolFunctionNode {
        @Specialization(assumptions = "storage.getStableAssumption()", guards = "noRedirect(function)")
        public static Object getStableSymbol(
                Node node,
                ELispSymbol symbol,
                @Cached(value = "getStorage(node, symbol)", neverDefault = true) FunctionStorage storage,
                @Cached(value = "storage.get()", neverDefault = true) Object function
        ) {
            return function;
        }

        @Specialization(replaces = "getStableSymbol")
        public static Object getRedirected(Node node, ELispSymbol symbol) {
            Object function = symbol;
            while (function instanceof ELispSymbol sym) {
                function = getFunction(node, sym);
            }
            if (function instanceof ELispCons cons && cons.car() == AUTOLOAD) {
                return doAutoload(node, cons, symbol);
            }
            return function;
        }

    }

    @GenerateInline
    @GenerateUncached
    public abstract static class GetDynamicSymbolFunctionNode extends GetSymbolFunctionNode {
        @Specialization(guards = "symbol == lastSym", limit = "3")
        public static Object getStaticSymbol(
                Node node,
                ELispSymbol symbol,
                @Cached("symbol") ELispSymbol lastSym,
                @Cached("create()") GetStaticSymbolFunctionNode getSymbolFunctionNode
        ) {
            return getSymbolFunctionNode.execute(node, lastSym);
        }

        @Specialization(replaces = "getStaticSymbol", guards = "noRedirect(function)")
        public static Object getDynamicSymbol(
                Node node,
                ELispSymbol symbol,
                @Bind("getFunction(node, symbol)") Object function
        ) {
            return function;
        }

        @Specialization(replaces = "getDynamicSymbol")
        public static Object getDynamicRedirectedSymbol(Node node, ELispSymbol symbol) {
            return GetStaticSymbolFunctionNode.getRedirected(node, symbol);
        }

        @Fallback
        public Object nonSymbolAsIs(Object car) {
            return car;
        }
    }

    @GenerateInline
    @GenerateUncached
    public abstract static class ConvertFunctionNode extends Node {
        public abstract Object executeConvert(Node node, Object function);

        public boolean isLambdaCons(ELispCons cons) {
            return cons.car() == LAMBDA;
        }
        public ELispExpressionNode createFunctionQuote(ELispCons cons) {
            return BuiltInEval.FFunction.function(cons);
        }

        /// Fast path: most functions are closures.
        ///
        /// This is to make sure we don't [Fallback] on these common functions ([#nonConsAsIs(Object)]).
        @Specialization
        public Object closureAsIs(AbstractELispClosure closure) {
            return closure;
        }

        @Specialization(guards = {"cons == oldCons", "isLambdaCons(cons)"}, limit = "1")
        public Object checkCons(
                ELispCons cons,
                @Cached("cons") ELispCons oldCons,
                @Cached("createFunctionQuote(oldCons)") ELispExpressionNode functionNode
        ) {
            return functionNode.executeGeneric(null);
        }

        @Specialization(replaces = "checkCons")
        public Object checkConsUncached(ELispCons cons) {
            if (cons.car() == LAMBDA) {
                return BuiltInEval.FFunction.getFunction(cons);
            }
            // cons.car() == MACRO and others
            return cons;
        }

        @Fallback
        public Object nonConsAsIs(Object car) {
            return car;
        }
    }

    @NodeField(name = "symbol", type = ELispSymbol.class)
    public abstract static class ReadFormSymbolCardinalFunctionNode extends ELispExpressionNode {
        public abstract ELispSymbol getSymbol();

        @Specialization
        public Object readFormCardinal(
                @Cached(inline = true, neverDefault = true)
                GetStaticSymbolFunctionNode symbolRead,
                @Cached(inline = true)
                ConvertFunctionNode convertFunction
        ) {
            return convertFunction.executeConvert(this, symbolRead.execute(this, getSymbol()));
        }
    }

    @NodeChild(value = "symbol", type = ELispExpressionNode.class)
    public abstract static class ReadDynamicSymbolNode extends ELispExpressionNode {
        @Specialization
        public Object read(
                Object symbol,
                @Cached(inline = true, neverDefault = true)
                GetDynamicSymbolFunctionNode symbolRead,
                @Cached(inline = true)
                ConvertFunctionNode convertFunction
        ) {
            return convertFunction.executeConvert(this, symbolRead.execute(this, symbol));
        }
    }
}
