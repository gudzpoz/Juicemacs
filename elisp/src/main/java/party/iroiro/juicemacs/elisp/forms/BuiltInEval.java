package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.List;

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
            case ELispSymbol symbol when symbol == T -> false;
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
        throw new UnsupportedOperationException(function.toString());
    }

    @ELispBuiltIn(name = "or", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Eval args until one of them yields non-nil, then return that value.\nThe remaining args are not evalled at all.\nIf all args return nil, return nil.\nusage: (or CONDITIONS...)")
    @GenerateNodeFactory
    public abstract static class FOr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object or(Object[] args) {
            for (Object arg : args) {
                Object result = evalSub(arg);
                if (ELispSymbol.isNil(result)) {
                    return result;
                }
            }
            return NIL;
        }
    }

    @ELispBuiltIn(name = "and", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Eval args until one of them yields nil, then return nil.\nThe remaining args are not evalled at all.\nIf no arg yields nil, return the last arg's value.\nusage: (and CONDITIONS...)")
    @GenerateNodeFactory
    public abstract static class FAnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object and(Object[] args) {
            Object lastResult = NIL;
            for (Object arg : args) {
                lastResult = evalSub(arg);
                if (ELispSymbol.isNil(lastResult)) {
                    return NIL;
                }
            }
            return lastResult;
        }
    }

    @ELispBuiltIn(name = "if", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true, doc = "If COND yields non-nil, do THEN, else do ELSE...\nReturns the value of THEN or the value of the last of the ELSE's.\nTHEN must be one expression, but ELSE... can be zero or more expressions.\nIf COND yields nil, and there are no ELSE's, the value is nil.\nusage: (if COND THEN ELSE...)")
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

    @ELispBuiltIn(name = "cond", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Try each clause until one succeeds.\nEach clause looks like (CONDITION BODY...).  CONDITION is evaluated\nand, if the value is non-nil, this clause succeeds:\nthen the expressions in BODY are evaluated and the last one's\nvalue is the value of the cond-form.\nIf a clause has one element, as in (CONDITION), then the cond-form\nreturns CONDITION's value, if that is non-nil.\nIf no clause succeeds, cond returns nil.\nusage: (cond CLAUSES...)")
    @GenerateNodeFactory
    public abstract static class FCond extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cond(Object[] args) {
            for (Object arg : args) {
                ELispCons cons = (ELispCons) arg;
                Object result = evalSub(cons.car());
                if (result != NIL) {
                    ELispCons.BrentTortoiseHareIterator iterator = cons.listIterator(1);
                    while (iterator.hasNext()) {
                        result = evalSub(iterator.next());
                    }
                    return result;
                }
            }
            return NIL;
        }
    }

    @ELispBuiltIn(name = "progn", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Eval BODY forms sequentially and return value of last one.\nusage: (progn BODY...)")
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

    @ELispBuiltIn(name = "prog1", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Eval FIRST and BODY sequentially; return value from FIRST.\nThe value of FIRST is saved during the evaluation of the remaining args,\nwhose values are discarded.\nusage: (prog1 FIRST BODY...)")
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

    @ELispBuiltIn(name = "setq", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Set each SYM to the value of its VAL.\nThe symbols SYM are variables; they are literal (not evaluated).\nThe values VAL are expressions; they are evaluated.\nThus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.\nThe second VAL is not computed until after the first SYM is set, and so on;\neach VAL can use the new value of variables set earlier in the `setq'.\nThe return value of the `setq' form is the value of the last VAL.\nusage: (setq [SYM VAL]...)")
    @GenerateNodeFactory
    public abstract static class FSetq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setq(Object[] args) {
            if (args.length % 2 != 0) {
                throw new IllegalArgumentException();
            }
            Object last = NIL;
            for (int i = 0; i < args.length; i += 2) {
                ELispSymbol symbol = (ELispSymbol) args[i];
                last = evalSub(args[i + 1]);
                symbol.setValue(last);
            }
            return last;
        }
    }

    @ELispBuiltIn(name = "quote", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Return the argument, without evaluating it.  `(quote x)' yields `x'.\nWarning: `quote' does not construct its return value, but just returns\nthe value that was pre-constructed by the Lisp reader (see info node\n`(elisp)Printed Representation').\nThis means that \\\\='(a . b) is not identical to (cons \\\\='a \\\\='b): the former\ndoes not cons.  Quoting should be reserved for constants that will\nnever be modified by side-effects, unless you like self-modifying code.\nSee the common pitfall in info node `(elisp)Rearrangement' for an example\nof unexpected results when a quoted object is modified.\nusage: (quote ARG)")
    @GenerateNodeFactory
    public abstract static class FQuote extends ELispBuiltInBaseNode {
        @Specialization
        public static Object quote(Object a) {
            return a;
        }
    }

    @ELispBuiltIn(name = "make-interpreted-closure", minArgs = 3, maxArgs = 5, doc = "Make an interpreted closure.\nARGS should be the list of formal arguments.\nBODY should be a non-empty list of forms.\nENV should be a lexical environment, like the second argument of `eval'.\nIFORM if non-nil should be of the form (interactive ...).")
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

    @ELispBuiltIn(name = "function", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Like `quote', but preferred for objects which are functions.\nIn byte compilation, `function' causes its argument to be handled by\nthe byte compiler.  Similarly, when expanding macros and expressions,\nARG can be examined and possibly expanded.  If `quote' is used\ninstead, this doesn't happen.\n\nusage: (function ARG)")
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
                        INTERNAL_INTERPRETER_ENVIRONMENT.getValue(),
                        docString,
                        interactive
                );
            }
            return quoted;
        }
    }

    @ELispBuiltIn(name = "defvaralias", minArgs = 2, maxArgs = 3, doc = "Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.\nAliased variables always have the same value; setting one sets the other.\nThird arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is\nomitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,\nor of the variable at the end of the chain of aliases, if BASE-VARIABLE is\nitself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,\nthen the value of BASE-VARIABLE is set to that of NEW-ALIAS.\nThe return value is BASE-VARIABLE.\n\nIf the resulting chain of variable definitions would contain a loop,\nsignal a `cyclic-variable-indirection' error.")
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

    @ELispBuiltIn(name = "default-toplevel-value", minArgs = 1, maxArgs = 1, doc = "Return SYMBOL's toplevel default value.\n\"Toplevel\" means outside of any let binding.")
    @GenerateNodeFactory
    public abstract static class FDefaultToplevelValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultToplevelValue(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-default-toplevel-value", minArgs = 2, maxArgs = 2, doc = "Set SYMBOL's toplevel default value to VALUE.\n\"Toplevel\" means outside of any let binding.")
    @GenerateNodeFactory
    public abstract static class FSetDefaultToplevelValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefaultToplevelValue(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--define-uninitialized-variable", minArgs = 1, maxArgs = 2, doc = "Define SYMBOL as a variable, with DOC as its docstring.\nThis is like `defvar' and `defconst' but without affecting the variable's\nvalue.")
    @GenerateNodeFactory
    public abstract static class FInternalDefineUninitializedVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefineUninitializedVariable(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defvar", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Define SYMBOL as a variable, and return SYMBOL.\nYou are not required to define a variable in order to use it, but\ndefining it lets you supply an initial value and documentation, which\ncan be referred to by the Emacs help facilities and other programming\ntools.\n\nIf SYMBOL's value is void and the optional argument INITVALUE is\nprovided, INITVALUE is evaluated and the result used to set SYMBOL's\nvalue.  If SYMBOL is buffer-local, its default value is what is set;\nbuffer-local values are not affected.  If INITVALUE is missing,\nSYMBOL's value is not set.\n\nIf INITVALUE is provided, the `defvar' form also declares the variable\nas \\\"special\\\", so that it is always dynamically bound even if\n`lexical-binding' is t.  If INITVALUE is missing, the form marks the\nvariable \\\"special\\\" locally (i.e., within the current\nlexical scope, or the current file, if the form is at top-level),\nand does nothing if `lexical-binding' is nil.\n\nIf SYMBOL is let-bound, then this form does not affect the local let\nbinding but the toplevel default binding instead, like\n`set-toplevel-default-binding`.\n(`defcustom' behaves similarly in this respect.)\n\nThe optional argument DOCSTRING is a documentation string for the\nvariable.\n\nTo define a user option, use `defcustom' instead of `defvar'.\n\nTo define a buffer-local variable, use `defvar-local'.\nusage: (defvar SYMBOL &optional INITVALUE DOCSTRING)")
    @GenerateNodeFactory
    public abstract static class FDefvar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defvar(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defvar-1", minArgs = 2, maxArgs = 3, doc = "Like `defvar' but as a function.\nMore specifically behaves like (defvar SYM 'INITVALUE DOCSTRING).")
    @GenerateNodeFactory
    public abstract static class FDefvar1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defvar1(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defconst", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true, doc = "Define SYMBOL as a constant variable.\nThis declares that neither programs nor users should ever change the\nvalue.  This constancy is not actually enforced by Emacs Lisp, but\nSYMBOL is marked as a special variable so that it is never lexically\nbound.\n\nThe `defconst' form always sets the value of SYMBOL to the result of\nevalling INITVALUE.  If SYMBOL is buffer-local, its default value is\nwhat is set; buffer-local values are not affected.  If SYMBOL has a\nlocal binding, then this form sets the local binding's value.\nHowever, you should normally not make local bindings for variables\ndefined with this form.\n\nThe optional DOCSTRING specifies the variable's documentation string.\nusage: (defconst SYMBOL INITVALUE [DOCSTRING])")
    @GenerateNodeFactory
    public abstract static class FDefconst extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defconst(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defconst-1", minArgs = 2, maxArgs = 3, doc = "Like `defconst' but as a function.\nMore specifically, behaves like (defconst SYM 'INITVALUE DOCSTRING).")
    @GenerateNodeFactory
    public abstract static class FDefconst1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defconst1(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-make-var-non-special", minArgs = 1, maxArgs = 1, doc = "Internal function.")
    @GenerateNodeFactory
    public abstract static class FMakeVarNonSpecial extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeVarNonSpecial(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "let*", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Bind variables according to VARLIST then eval BODY.\nThe value of the last form in BODY is returned.\nEach element of VARLIST is a symbol (which is bound to nil)\nor a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).\nEach VALUEFORM can refer to the symbols already bound by this VARLIST.\nusage: (let* VARLIST BODY...)")
    @GenerateNodeFactory
    public abstract static class FLetx extends ELispBuiltInBaseNode {
        @Specialization
        public static Object letx(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "let", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Bind variables according to VARLIST then eval BODY.\nThe value of the last form in BODY is returned.\nEach element of VARLIST is a symbol (which is bound to nil)\nor a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).\nAll the VALUEFORMs are evalled before any symbols are bound.\nusage: (let VARLIST BODY...)")
    @GenerateNodeFactory
    public abstract static class FLet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object let(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "while", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "If TEST yields non-nil, eval BODY... and repeat.\nThe order of execution is thus TEST, BODY, TEST, BODY and so on\nuntil TEST returns nil.\n\nThe value of a `while' form is always nil.\n\nusage: (while TEST BODY...)")
    @GenerateNodeFactory
    public abstract static class FWhile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object while_(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "funcall-with-delayed-message", minArgs = 3, maxArgs = 3, doc = "Like `funcall', but display MESSAGE if FUNCTION takes longer than TIMEOUT.\nTIMEOUT is a number of seconds, and can be an integer or a floating\npoint number.\n\nIf FUNCTION takes less time to execute than TIMEOUT seconds, MESSAGE\nis not displayed.")
    @GenerateNodeFactory
    public abstract static class FFuncallWithDelayedMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcallWithDelayedMessage(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "macroexpand", minArgs = 1, maxArgs = 2, doc = "Return result of expanding macros at top level of FORM.\nIf FORM is not a macro call, it is returned unchanged.\nOtherwise, the macro is expanded and the expansion is considered\nin place of FORM.  When a non-macro-call results, it is returned.\n\nThe second optional arg ENVIRONMENT specifies an environment of macro\ndefinitions to shadow the loaded ones for use in file byte-compilation.")
    @GenerateNodeFactory
    public abstract static class FMacroexpand extends ELispBuiltInBaseNode {
        @Specialization
        public static Object macroexpand(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "catch", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Eval BODY allowing nonlocal exits using `throw'.\nTAG is evalled to get the tag to use; it must not be nil.\n\nThen the BODY is executed.\nWithin BODY, a call to `throw' with the same TAG exits BODY and this `catch'.\nIf no throw happens, `catch' returns the value of the last BODY form.\nIf a throw happens, it specifies the value to return from `catch'.\nusage: (catch TAG BODY...)")
    @GenerateNodeFactory
    public abstract static class FCatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object catch_(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "throw", minArgs = 2, maxArgs = 2, doc = "Throw to the catch for TAG and return VALUE from it.\nBoth TAG and VALUE are evalled.")
    @GenerateNodeFactory
    public abstract static class FThrow extends ELispBuiltInBaseNode {
        @Specialization
        public static Object throw_(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unwind-protect", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true, doc = "Do BODYFORM, protecting with UNWINDFORMS.\nIf BODYFORM completes normally, its value is returned\nafter executing the UNWINDFORMS.\nIf BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.\nusage: (unwind-protect BODYFORM UNWINDFORMS...)")
    @GenerateNodeFactory
    public abstract static class FUnwindProtect extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unwindProtect(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "condition-case", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true, doc = "Regain control when an error is signaled.\nExecutes BODYFORM and returns its value if no error happens.\nEach element of HANDLERS looks like (CONDITION-NAME BODY...)\nor (:success BODY...), where the BODY is made of Lisp expressions.\n\nA handler is applicable to an error if CONDITION-NAME is one of the\nerror's condition names.  Handlers may also apply when non-error\nsymbols are signaled (e.g., `quit').  A CONDITION-NAME of t applies to\nany symbol, including non-error symbols.  If multiple handlers are\napplicable, only the first one runs.\n\nThe car of a handler may be a list of condition names instead of a\nsingle condition name; then it handles all of them.  If the special\ncondition name `debug' is present in this list, it allows another\ncondition in the list to run the debugger if `debug-on-error' and the\nother usual mechanisms say it should (otherwise, `condition-case'\nsuppresses the debugger).\n\nWhen a handler handles an error, control returns to the `condition-case'\nand it executes the handler's BODY...\nwith VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.\n\\(If VAR is nil, the handler can't access that information.)\nThen the value of the last BODY form is returned from the `condition-case'\nexpression.\n\nThe special handler (:success BODY...) is invoked if BODYFORM terminated\nwithout signaling an error.  BODY is then evaluated with VAR bound to\nthe value returned by BODYFORM.\n\nSee also the function `signal' for more info.\nusage: (condition-case VAR BODYFORM &rest HANDLERS)")
    @GenerateNodeFactory
    public abstract static class FConditionCase extends ELispBuiltInBaseNode {
        @Specialization
        public static Object conditionCase(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "handler-bind-1", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Set up error handlers around execution of BODYFUN.\nBODYFUN should be a function and it is called with no arguments.\nCONDITIONS should be a list of condition names (symbols).\nWhen an error is signaled during execution of BODYFUN, if that\nerror matches one of CONDITIONS, then the associated HANDLER is\ncalled with the error as argument.\nHANDLER should either transfer the control via a non-local exit,\nor return normally.\nIf it returns normally, the search for an error handler continues\nfrom where it left off.\n\nusage: (handler-bind BODYFUN [CONDITIONS HANDLER]...)")
    @GenerateNodeFactory
    public abstract static class FHandlerBind1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object handlerBind1(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "signal", minArgs = 2, maxArgs = 2, doc = "Signal an error.  Args are ERROR-SYMBOL and associated DATA.\nThis function does not return.\n\nWhen `noninteractive' is non-nil (in particular, in batch mode), an\nunhandled error calls `kill-emacs', which terminates the Emacs\nsession with a non-zero exit code.\n\nAn error symbol is a symbol with an `error-conditions' property\nthat is a list of condition names.  The symbol should be non-nil.\nA handler for any of those names will get to handle this signal.\nThe symbol `error' should normally be one of them.\n\nDATA should be a list.  Its elements are printed as part of the error message.\nSee Info anchor `(elisp)Definition of signal' for some details on how this\nerror message is constructed.\nIf the signal is handled, DATA is made available to the handler.\nSee also the function `condition-case'.")
    @GenerateNodeFactory
    public abstract static class FSignal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object signal(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "commandp", minArgs = 1, maxArgs = 2, doc = "Non-nil if FUNCTION makes provisions for interactive calling.\nThis means it contains a description for how to read arguments to give it.\nThe value is nil for an invalid function or a symbol with no function\ndefinition.\n\nInteractively callable functions include strings and vectors (treated\nas keyboard macros), lambda-expressions that contain a top-level call\nto `interactive', autoload definitions made by `autoload' with non-nil\nfourth argument, and some of the built-in functions of Lisp.\n\nAlso, a symbol satisfies `commandp' if its function definition does so.\n\nIf the optional argument FOR-CALL-INTERACTIVELY is non-nil,\nthen strings and vectors are not accepted.")
    @GenerateNodeFactory
    public abstract static class FCommandp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object commandp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "autoload", minArgs = 2, maxArgs = 5, doc = "Define FUNCTION to autoload from FILE.\nFUNCTION is a symbol; FILE is a file name string to pass to `load'.\n\nThird arg DOCSTRING is documentation for the function.\n\nFourth arg INTERACTIVE if non-nil says function can be called\ninteractively.  If INTERACTIVE is a list, it is interpreted as a list\nof modes the function is applicable for.\n\nFifth arg TYPE indicates the type of the object:\n   nil or omitted says FUNCTION is a function,\n   `keymap' says FUNCTION is really a keymap, and\n   `macro' or t says FUNCTION is really a macro.\n\nThird through fifth args give info about the real definition.\nThey default to nil.\n\nIf FUNCTION is already defined other than as an autoload,\nthis does nothing and returns nil.")
    @GenerateNodeFactory
    public abstract static class FAutoload extends ELispBuiltInBaseNode {
        @Specialization
        public static Object autoload(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "autoload-do-load", minArgs = 1, maxArgs = 3, doc = "Load FUNDEF which should be an autoload.\nIf non-nil, FUNNAME should be the symbol whose function value is FUNDEF,\nin which case the function returns the new autoloaded function value.\nIf equal to `macro', MACRO-ONLY specifies that FUNDEF should only be loaded if\nit defines a macro.")
    @GenerateNodeFactory
    public abstract static class FAutoloadDoLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static Object autoloadDoLoad(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eval", minArgs = 1, maxArgs = 2, doc = "Evaluate FORM and return its value.\nIf LEXICAL is `t', evaluate using lexical binding by default.\nThis is the recommended value.\n\nIf absent or `nil', use dynamic scoping only.\n\nLEXICAL can also represent an actual lexical environment; see the Info\nnode `(elisp)Eval' for details.")
    @GenerateNodeFactory
    public abstract static class FEval extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eval(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "apply", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Call FUNCTION with our remaining args, using our last arg as list of args.\nThen return the value FUNCTION returns.\nWith a single argument, call the argument's first element using the\nother elements as args.\nThus, (apply \\\\='+ 1 2 \\\\='(3 4)) returns 10.\nusage: (apply FUNCTION &rest ARGUMENTS)")
    @GenerateNodeFactory
    public abstract static class FApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Object apply(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hooks", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Run each hook in HOOKS.\nEach argument should be a symbol, a hook variable.\nThese symbols are processed in the order specified.\nIf a hook symbol has a non-nil value, that value may be a function\nor a list of functions to be called to run the hook.\nIf the value is a function, it is called with no arguments.\nIf it is a list, the elements are called, in order, with no arguments.\n\nMajor modes should not use this function directly to run their mode\nhook; they should use `run-mode-hooks' instead.\n\nDo not use `make-local-variable' to make a hook variable buffer-local.\nInstead, use `add-hook' and specify t for the LOCAL argument.\nusage: (run-hooks &rest HOOKS)")
    @GenerateNodeFactory
    public abstract static class FRunHooks extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHooks(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-with-args", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Run HOOK with the specified arguments ARGS.\nHOOK should be a symbol, a hook variable.  The value of HOOK\nmay be nil, a function, or a list of functions.  Call each\nfunction in order with arguments ARGS.  The final return value\nis unspecified.\n\nDo not use `make-local-variable' to make a hook variable buffer-local.\nInstead, use `add-hook' and specify t for the LOCAL argument.\nusage: (run-hook-with-args HOOK &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgs extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWithArgs(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-with-args-until-success", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Run HOOK with the specified arguments ARGS.\nHOOK should be a symbol, a hook variable.  The value of HOOK\nmay be nil, a function, or a list of functions.  Call each\nfunction in order with arguments ARGS, stopping at the first\none that returns non-nil, and return that value.  Otherwise (if\nall functions return nil, or if there are no functions to call),\nreturn nil.\n\nDo not use `make-local-variable' to make a hook variable buffer-local.\nInstead, use `add-hook' and specify t for the LOCAL argument.\nusage: (run-hook-with-args-until-success HOOK &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgsUntilSuccess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWithArgsUntilSuccess(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-with-args-until-failure", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Run HOOK with the specified arguments ARGS.\nHOOK should be a symbol, a hook variable.  The value of HOOK\nmay be nil, a function, or a list of functions.  Call each\nfunction in order with arguments ARGS, stopping at the first\none that returns nil, and return nil.  Otherwise (if all functions\nreturn non-nil, or if there are no functions to call), return non-nil\n\\(do not rely on the precise return value in this case).\n\nDo not use `make-local-variable' to make a hook variable buffer-local.\nInstead, use `add-hook' and specify t for the LOCAL argument.\nusage: (run-hook-with-args-until-failure HOOK &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgsUntilFailure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWithArgsUntilFailure(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "run-hook-wrapped", minArgs = 2, maxArgs = 2, varArgs = true, doc = "Run HOOK, passing each function through WRAP-FUNCTION.\nI.e. instead of calling each function FUN directly with arguments ARGS,\nit calls WRAP-FUNCTION with arguments FUN and ARGS.\nAs soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'\naborts and returns that value.\nusage: (run-hook-wrapped HOOK WRAP-FUNCTION &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FRunHookWrapped extends ELispBuiltInBaseNode {
        @Specialization
        public static Object runHookWrapped(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "functionp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a function.\n\nAn object is a function if it is callable via `funcall'; this includes\nsymbols with function bindings, but excludes macros and special forms.\n\nOrdinarily return nil if OBJECT is not a function, although t might be\nreturned in rare cases.")
    @GenerateNodeFactory
    public abstract static class FFunctionp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object functionp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "funcall", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Call first argument as a function, passing remaining arguments to it.\nReturn the value that function returns.\nThus, (funcall \\\\='cons \\\\='x \\\\='y) returns (x . y).\nusage: (funcall FUNCTION &rest ARGUMENTS)")
    @GenerateNodeFactory
    public abstract static class FFuncall extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcall(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "func-arity", minArgs = 1, maxArgs = 1, doc = "Return minimum and maximum number of args allowed for FUNCTION.\nFUNCTION must be a function of some kind.\nThe returned value is a cons cell (MIN . MAX).  MIN is the minimum number\nof args.  MAX is the maximum number, or the symbol `many', for a\nfunction with `&rest' args, or `unevalled' for a special form.")
    @GenerateNodeFactory
    public abstract static class FFuncArity extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcArity(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "special-variable-p", minArgs = 1, maxArgs = 1, doc = "Return non-nil if SYMBOL's global binding has been declared special.\nA special variable is one that will be bound dynamically, even in a\ncontext where binding is lexical by default.")
    @GenerateNodeFactory
    public abstract static class FSpecialVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object specialVariableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace-debug", minArgs = 2, maxArgs = 3, doc = "Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.\nLEVEL and BASE specify the activation frame to use, as in `backtrace-frame'.\nThe debugger is entered when that frame exits, if the flag is non-nil.")
    @GenerateNodeFactory
    public abstract static class FBacktraceDebug extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceDebug(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapbacktrace", minArgs = 1, maxArgs = 2, doc = "Call FUNCTION for each frame in backtrace.\nIf BASE is non-nil, it should be a function and iteration will start\nfrom its nearest activation frame.\nFUNCTION is called with 4 arguments: EVALD, FUNC, ARGS, and FLAGS.  If\na frame has not evaluated its arguments yet or is a special form,\nEVALD is nil and ARGS is a list of forms.  If a frame has evaluated\nits arguments and called its function already, EVALD is t and ARGS is\na list of values.\nFLAGS is a plist of properties of the current frame: currently, the\nonly supported property is :debug-on-exit.  `mapbacktrace' always\nreturns nil.")
    @GenerateNodeFactory
    public abstract static class FMapbacktrace extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapbacktrace(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace-frame--internal", minArgs = 3, maxArgs = 3, doc = "Call FUNCTION on stack frame NFRAMES away from BASE.\nReturn the result of FUNCTION, or nil if no matching frame could be found.")
    @GenerateNodeFactory
    public abstract static class FBacktraceFrameInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceFrameInternal(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace--frames-from-thread", minArgs = 1, maxArgs = 1, doc = "Return the list of backtrace frames from current execution point in THREAD.\nIf a frame has not evaluated the arguments yet (or is a special form),\nthe value of the list element is (nil FUNCTION ARG-FORMS...).\nIf a frame has evaluated its arguments and called its function already,\nthe value of the list element is (t FUNCTION ARG-VALUES...).\nA &rest arg is represented as the tail of the list ARG-VALUES.\nFUNCTION is whatever was supplied as car of evaluated list,\nor a lambda expression for macro calls.")
    @GenerateNodeFactory
    public abstract static class FBacktraceFramesFromThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceFramesFromThread(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace-eval", minArgs = 2, maxArgs = 3, doc = "Evaluate EXP in the context of some activation frame.\nNFRAMES and BASE specify the activation frame to use, as in `backtrace-frame'.")
    @GenerateNodeFactory
    public abstract static class FBacktraceEval extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceEval(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "backtrace--locals", minArgs = 1, maxArgs = 2, doc = "Return names and values of local variables of a stack frame.\nNFRAMES and BASE specify the activation frame to use, as in `backtrace-frame'.")
    @GenerateNodeFactory
    public abstract static class FBacktraceLocals extends ELispBuiltInBaseNode {
        @Specialization
        public static Object backtraceLocals(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }
}
