package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispBindingScope;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

/**
 * Built-in functions from {@code src/eval.c}
 */
public class BuiltInEval extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEvalFactory.getFactories();
    }

    public static Object evalSub(Object form) {
        return switch (form) {
            case ELispSymbol symbol when symbol == T -> true;
            case ELispSymbol symbol when symbol == NIL -> false;
            case ELispSymbol symbol -> symbol.getValue();
            case ELispCons cons -> evalCons(cons);
            default -> form;
        };
    }

    private static Object evalCons(ELispCons cons) {
        Object function = cons.car();
        Object[] args = cons.cdr() instanceof ELispCons rest ? rest.toArray() : new Object[0];
        if (function instanceof ELispSymbol symbol) {
            function = symbol.getFunction();
        } else {
            function = FFunction.function(function);
        }
        if (function instanceof ELispSubroutine(CallTarget body, boolean specialForm)) {
            if (!specialForm) {
                for (int i = 0; i < args.length; i++) {
                    args[i] = evalSub(args[i]);
                }
            }
            return body.call(args);
        }
        if (function instanceof ELispInterpretedClosure) {
            for (int i = 0; i < args.length; i++) {
                args[i] = evalSub(args[i]);
            }
            return ((ELispInterpretedClosure) function).getFunction().callTarget().call(args);
        }
        if (function instanceof ELispCons fCons && fCons.car() == MACRO) {
            try (var _ = ELispBindingScope.withLexicalBinding(true)) {
                Object result = FApply.apply(fCons.cdr(), new Object[]{cons.cdr()});
                return evalSub(result);
            }
        }
        throw new UnsupportedOperationException(function.toString());
    }

    @ELispBuiltIn(name = "or", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FOr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object or(Object[] args) {
            for (Object arg : args) {
                Object result = evalSub(arg);
                if (!ELispSymbol.isNil(result)) {
                    return result;
                }
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "and", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FAnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object and(Object[] args) {
            Object lastResult = true;
            for (Object arg : args) {
                lastResult = evalSub(arg);
                if (ELispSymbol.isNil(lastResult)) {
                    return false;
                }
            }
            return lastResult;
        }
    }

    @ELispBuiltIn(name = "if", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FIf extends ELispBuiltInBaseNode {
        @Specialization
        public static Object if_(Object cond, Object then, Object[] args) {
            if (ELispSymbol.isNil(evalSub(cond))) {
                Object lastResult = NIL;
                for (Object arg : args) {
                    lastResult = evalSub(arg);
                }
                return lastResult;
            } else {
                return evalSub(then);
            }
        }
    }

    @ELispBuiltIn(name = "cond", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FCond extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cond(Object[] args) {
            for (Object arg : args) {
                ELispCons cons = (ELispCons) arg;
                Object result = evalSub(cons.car());
                if (!ELispSymbol.isNil(result)) {
                    ELispCons.BrentTortoiseHareIterator iterator = cons.listIterator(1);
                    while (iterator.hasNext()) {
                        result = evalSub(iterator.next());
                    }
                    return result;
                }
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "progn", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FProgn extends ELispBuiltInBaseNode {
        @Specialization
        public static Object progn(Object[] args) {
            Object lastResult = NIL;
            for (Object arg : args) {
                lastResult = evalSub(arg);
            }
            return lastResult;
        }
    }

    @ELispBuiltIn(name = "prog1", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FProg1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object prog1(Object a, Object[] args) {
            Object result = evalSub(a);
            for (Object arg : args) {
                evalSub(arg);
            }
            return result;
        }
    }

    @ELispBuiltIn(name = "setq", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSetq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setq(Object[] args) {
            if (args.length % 2 != 0) {
                throw new IllegalArgumentException();
            }
            Object last = false;
            for (int i = 0; i < args.length; i += 2) {
                ELispSymbol symbol = (ELispSymbol) args[i];
                last = evalSub(args[i + 1]);
                symbol.setValue(last);
            }
            return last;
        }
    }

    @ELispBuiltIn(name = "quote", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FQuote extends ELispBuiltInBaseNode {
        @Specialization
        public static Object quote(Object a) {
            return a;
        }
    }

    @ELispBuiltIn(name = "make-interpreted-closure", minArgs = 3, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMakeInterpretedClosure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeInterpretedClosure(Object args, ELispCons body, Object env, Object doc, Object iForm) {
            return new ELispInterpretedClosure(
                    args,
                    body,
                    env,
                    doc,
                    iForm
            );
        }
    }

    @ELispBuiltIn(name = "function", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object function(Object quoted) {
            if (quoted instanceof ELispCons def && def.car() == LAMBDA) {
                ELispCons.BrentTortoiseHareIterator iterator = def.listIterator(1);
                Object args = iterator.next();
                Object docString = null;
                if (iterator.hasNext()) {
                    docString = switch (iterator.currentCons().car()) {
                        case ELispString s -> s;
                        case ELispCons cons when cons.car() == CDOCUMENTATION && cons.cdr() instanceof ELispCons doc ->
                                evalSub(doc.car());
                        default -> null;
                    };
                }
                if (docString == null) {
                    docString = NIL;
                } else {
                    iterator.next();
                }
                Object interactive = NIL;
                if (iterator.hasNext()) {
                    if (iterator.currentCons().car() instanceof ELispCons cons && cons.car() == INTERACTIVE) {
                        interactive = cons;
                        iterator.next();
                    }
                }
                ELispCons body = iterator.hasNext() ? iterator.currentCons() : new ELispCons(NIL);
                return FMakeInterpretedClosure.makeInterpretedClosure(
                        args,
                        body,
                        ELispSymbol.isNil(LEXICAL_BINDING.getValue()) ? false
                                : Objects.requireNonNullElse(ELispBindingScope.getCurrentLexical(), true),
                        docString,
                        interactive
                );
            }
            return quoted;
        }
    }

    @ELispBuiltIn(name = "defvaralias", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefvaralias extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defvaralias(ELispSymbol newAlias, ELispSymbol baseVar, Object doc) {
            if (newAlias.isConstant()) {
                throw new IllegalArgumentException();
            }
            newAlias.aliasSymbol(baseVar);
            return baseVar;
        }
    }

    @ELispBuiltIn(name = "default-toplevel-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDefaultToplevelValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultToplevelValue(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-default-toplevel-value", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetDefaultToplevelValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefaultToplevelValue(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--define-uninitialized-variable", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalDefineUninitializedVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefineUninitializedVariable(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defvar", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FDefvar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defvar(ELispSymbol symbol, Object[] args) {
            symbol.setSpecial(true);
            if (args.length > 0) {
                symbol.setValue(evalSub(args[0]));
            }
            // TODO: If SYMBOL is let-bound, then this form does not affect the local let\nbinding but the toplevel default binding instead
            // TODO: If INITVALUE is missing, the form marks the\nvariable \\\"special\\\" locally (i.e., within the current\nlexical scope, or the current file, if the form is at top-level)
            return symbol;
        }
    }

    @ELispBuiltIn(name = "defvar-1", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefvar1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defvar1(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defconst", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FDefconst extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defconst(ELispSymbol symbol, Object init, Object[] args) {
            symbol.setValue(evalSub(init));
            symbol.setSpecial(true);
            symbol.setConstant(true);
            return symbol;
        }
    }

    @ELispBuiltIn(name = "defconst-1", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefconst1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defconst1(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-make-var-non-special", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeVarNonSpecial extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeVarNonSpecial(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "let*", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FLetx extends ELispBuiltInBaseNode {
        @Specialization
        public static Object letx(Object a, Object[] args) {
            try (ELispBindingScope.@Nullable ClosableScope _ = FLet.makeScope(a, true)) {
                return FProgn.progn(args);
            }
        }
    }

    @ELispBuiltIn(name = "let", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FLet extends ELispBuiltInBaseNode {
        public static ELispBindingScope.@Nullable ClosableScope makeScope(Object a, boolean progressive) {
            if (ELispSymbol.isNil(a)) {
                return null;
            }
            // TODO: What about dynamic binding?
            HashMap<ELispSymbol, ELispSymbol.Value.Forwarded> lexicalBindings = new HashMap<>();
            ELispBindingScope.ClosableScope handle = null;
            if (progressive) {
                handle = ELispBindingScope.pushLexical(lexicalBindings);
            }
            try {
                for (Object assignment : (ELispCons) a) {
                    // TODO: Handle "special == true" symbols
                    if (assignment instanceof ELispSymbol symbol) {
                        lexicalBindings.put(symbol, new ELispSymbol.Value.Forwarded(false));
                    } else {
                        ELispCons cons = (ELispCons) assignment;
                        ELispSymbol symbol = (ELispSymbol) cons.car();
                        ELispCons cdr = (ELispCons) cons.cdr();
                        if (!ELispSymbol.isNil(cdr.cdr())) {
                            throw new IllegalArgumentException();
                        }
                        lexicalBindings.put(symbol, new ELispSymbol.Value.Forwarded(evalSub(cdr.car())));
                    }
                }
            } catch (Throwable e) {
                if (progressive) {
                    handle.close();
                }
                throw e;
            }
            if (!progressive) {
                handle = ELispBindingScope.pushLexical(lexicalBindings);
            }
            return handle;
        }

        @Specialization
        public static Object let(Object a, Object[] args) {
            try (ELispBindingScope.@Nullable ClosableScope _ = makeScope(a, false)) {
                return FProgn.progn(args);
            }
        }
    }

    @ELispBuiltIn(name = "while", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FWhile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object while_(Object a, Object[] args) {
            while (!ELispSymbol.isNil(evalSub(a))) {
                FProgn.progn(args);
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "funcall-with-delayed-message", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFuncallWithDelayedMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcallWithDelayedMessage(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "macroexpand", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMacroexpand extends ELispBuiltInBaseNode {
        @Specialization
        public static Object macroexpand(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "catch", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FCatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object catch_(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "throw", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FThrow extends ELispBuiltInBaseNode {
        @Specialization
        public static Object throw_(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unwind-protect", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FUnwindProtect extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unwindProtect(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "condition-case", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FConditionCase extends ELispBuiltInBaseNode {
        @Specialization
        public static Object conditionCase(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "handler-bind-1", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FHandlerBind1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object handlerBind1(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "signal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSignal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object signal(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "commandp", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCommandp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object commandp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "autoload", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FAutoload extends ELispBuiltInBaseNode {
        @Specialization
        public static Object autoload(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "autoload-do-load", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAutoloadDoLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static Object autoloadDoLoad(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eval", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEval extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eval(Object form, boolean lexical) {
            try (var _ = ELispBindingScope.withLexicalBinding(lexical)) {
                return evalSub(form);
            }
        }
    }

    @ELispBuiltIn(name = "apply", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Object apply(Object f, Object[] args) {
            List<Object> objects = new ArrayList<>(Arrays.asList(args).subList(0, args.length - 1));
            Object last = args[args.length - 1];
            if (!ELispSymbol.isNil(last)) {
                objects.addAll((ELispCons) last);
            }
            args = objects.toArray();
            return FFuncall.funcall(f, args);
        }
    }

    @ELispBuiltIn(name = "run-hooks", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHooks extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHooks(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-with-args", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgs extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWithArgs(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-with-args-until-success", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgsUntilSuccess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWithArgsUntilSuccess(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-with-args-until-failure", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgsUntilFailure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWithArgsUntilFailure(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-wrapped", minArgs = 2, maxArgs = 2, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWrapped extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWrapped(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "functionp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFunctionp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object functionp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "funcall", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFuncall extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcall(Object f, Object[] args) {
            if (f instanceof ELispSymbol symbol) {
                f = symbol.getFunction();
            }
            if (f instanceof ELispSubroutine subroutine) {
                return subroutine.body().call(args);
            }
            if (f instanceof ELispInterpretedClosure closure) {
                return closure.getCallTarget().call(args);
            }
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "func-arity", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFuncArity extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcArity(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "special-variable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSpecialVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object specialVariableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace-debug", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBacktraceDebug extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceDebug(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapbacktrace", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapbacktrace extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapbacktrace(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace-frame--internal", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBacktraceFrameInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceFrameInternal(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace--frames-from-thread", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBacktraceFramesFromThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceFramesFromThread(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace-eval", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBacktraceEval extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceEval(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace--locals", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBacktraceLocals extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceLocals(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }
}
