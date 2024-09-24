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
@SuppressWarnings("DefaultAnnotationParam")
public class BuiltInEval extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEvalFactory.getFactories();
    }

    public static Object evalSub(Object form) {
        return switch (form) {
            case ELispSymbol symbol when symbol == T -> true;
            case ELispSymbol symbol when symbol == NIL -> false;
            case ELispSymbol symbol -> {
                Object lexical = ELispBindingScope.getLexical(symbol);
                yield lexical == null ? symbol.getValue() : lexical;
            }
            case ELispCons cons -> evalCons(cons);
            default -> form;
        };
    }

    private static Object evalCons(ELispCons cons) {
        Object function = cons.car();
        Object[] args = cons.cdr() instanceof ELispCons rest ? rest.toArray() : new Object[0];
        if (function instanceof ELispSymbol symbol) {
            function = symbol.getIndirectFunction();
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
        throw new UnsupportedOperationException(cons.car().toString());
    }

    /**
     * <pre>
     * Eval args until one of them yields non-nil, then return that value.
     * The remaining args are not evalled at all.
     * If all args return nil, return nil.
     * usage: (or CONDITIONS...)
     * </pre>
     */
    @ELispBuiltIn(name = "or", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FOr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object or(Object[] conditions) {
            for (Object arg : conditions) {
                Object result = evalSub(arg);
                if (!ELispSymbol.isNil(result)) {
                    return result;
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Eval args until one of them yields nil, then return nil.
     * The remaining args are not evalled at all.
     * If no arg yields nil, return the last arg's value.
     * usage: (and CONDITIONS...)
     * </pre>
     */
    @ELispBuiltIn(name = "and", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FAnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object and(Object[] conditions) {
            Object lastResult = true;
            for (Object arg : conditions) {
                lastResult = evalSub(arg);
                if (ELispSymbol.isNil(lastResult)) {
                    return false;
                }
            }
            return lastResult;
        }
    }

    /**
     * <pre>
     * If COND yields non-nil, do THEN, else do ELSE...
     * Returns the value of THEN or the value of the last of the ELSE's.
     * THEN must be one expression, but ELSE... can be zero or more expressions.
     * If COND yields nil, and there are no ELSE's, the value is nil.
     * usage: (if COND THEN ELSE...)
     * </pre>
     */
    @ELispBuiltIn(name = "if", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FIf extends ELispBuiltInBaseNode {
        @Specialization
        public static Object if_(Object cond, Object then, Object[] else_) {
            if (ELispSymbol.isNil(evalSub(cond))) {
                Object lastResult = NIL;
                for (Object arg : else_) {
                    lastResult = evalSub(arg);
                }
                return lastResult;
            } else {
                return evalSub(then);
            }
        }
    }

    /**
     * <pre>
     * Try each clause until one succeeds.
     * Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
     * and, if the value is non-nil, this clause succeeds:
     * then the expressions in BODY are evaluated and the last one's
     * value is the value of the cond-form.
     * If a clause has one element, as in (CONDITION), then the cond-form
     * returns CONDITION's value, if that is non-nil.
     * If no clause succeeds, cond returns nil.
     * usage: (cond CLAUSES...)
     * </pre>
     */
    @ELispBuiltIn(name = "cond", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FCond extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cond(Object[] clauses) {
            for (Object arg : clauses) {
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

    /**
     * <pre>
     * Eval BODY forms sequentially and return value of last one.
     * usage: (progn BODY...)
     * </pre>
     */
    @ELispBuiltIn(name = "progn", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FProgn extends ELispBuiltInBaseNode {
        @Specialization
        public static Object progn(Object[] body) {
            Object lastResult = NIL;
            for (Object arg : body) {
                lastResult = evalSub(arg);
            }
            return lastResult;
        }
    }

    /**
     * <pre>
     * Eval FIRST and BODY sequentially; return value from FIRST.
     * The value of FIRST is saved during the evaluation of the remaining args,
     * whose values are discarded.
     * usage: (prog1 FIRST BODY...)
     * </pre>
     */
    @ELispBuiltIn(name = "prog1", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FProg1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object prog1(Object first, Object[] body) {
            Object result = evalSub(first);
            for (Object arg : body) {
                evalSub(arg);
            }
            return result;
        }
    }

    /**
     * <pre>
     * Set each SYM to the value of its VAL.
     * The symbols SYM are variables; they are literal (not evaluated).
     * The values VAL are expressions; they are evaluated.
     * Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
     * The second VAL is not computed until after the first SYM is set, and so on;
     * each VAL can use the new value of variables set earlier in the `setq'.
     * The return value of the `setq' form is the value of the last VAL.
     * usage: (setq [SYM VAL]...)
     * </pre>
     */
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
                if (!ELispBindingScope.setLexical(symbol, last)) {
                    symbol.setValue(last);
                }
            }
            return last;
        }
    }

    /**
     * <pre>
     * Return the argument, without evaluating it.  `(quote x)' yields `x'.
     * Warning: `quote' does not construct its return value, but just returns
     * the value that was pre-constructed by the Lisp reader (see info node
     * `(elisp)Printed Representation').
     * This means that \\='(a . b) is not identical to (cons \\='a \\='b): the former
     * does not cons.  Quoting should be reserved for constants that will
     * never be modified by side-effects, unless you like self-modifying code.
     * See the common pitfall in info node `(elisp)Rearrangement' for an example
     * of unexpected results when a quoted object is modified.
     * usage: (quote ARG)
     * </pre>
     */
    @ELispBuiltIn(name = "quote", minArgs = 1, maxArgs = 1, varArgs = false, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FQuote extends ELispBuiltInBaseNode {
        @Specialization
        public static Object quote(Object arg) {
            return arg;
        }
    }

    /**
     * <pre>
     * Make an interpreted closure.
     * ARGS should be the list of formal arguments.
     * BODY should be a non-empty list of forms.
     * ENV should be a lexical environment, like the second argument of `eval'.
     * IFORM if non-nil should be of the form (interactive ...).
     * </pre>
     */
    @ELispBuiltIn(name = "make-interpreted-closure", minArgs = 3, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMakeInterpretedClosure extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispInterpretedClosure makeInterpretedClosure(Object args, ELispCons body, Object env, Object docstring, Object iform) {
            return new ELispInterpretedClosure(
                    args,
                    body,
                    env,
                    docstring,
                    iform
            );
        }
    }

    /**
     * <pre>
     * Like `quote', but preferred for objects which are functions.
     * In byte compilation, `function' causes its argument to be handled by
     * the byte compiler.  Similarly, when expanding macros and expressions,
     * ARG can be examined and possibly expanded.  If `quote' is used
     * instead, this doesn't happen.
     *
     * usage: (function ARG)
     * </pre>
     */
    @ELispBuiltIn(name = "function", minArgs = 1, maxArgs = 1, varArgs = false, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object function(Object arg) {
            if (arg instanceof ELispCons def && def.car() == LAMBDA) {
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
                        ELispSymbol.isNil(LEXICAL_BINDING.getValueOr(false)) ? false
                                : Objects.requireNonNullElse(ELispBindingScope.getCurrentLexical(), true),
                        docString,
                        interactive
                );
            }
            return arg;
        }
    }

    /**
     * <pre>
     * Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.
     * Aliased variables always have the same value; setting one sets the other.
     * Third arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is
     * omitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,
     * or of the variable at the end of the chain of aliases, if BASE-VARIABLE is
     * itself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,
     * then the value of BASE-VARIABLE is set to that of NEW-ALIAS.
     * The return value is BASE-VARIABLE.
     *
     * If the resulting chain of variable definitions would contain a loop,
     * signal a `cyclic-variable-indirection' error.
     * </pre>
     */
    @ELispBuiltIn(name = "defvaralias", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefvaralias extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol defvaralias(ELispSymbol newAlias, ELispSymbol baseVariable, Object docstring) {
            if (newAlias.isConstant()) {
                throw new IllegalArgumentException();
            }
            newAlias.aliasSymbol(baseVariable);
            return baseVariable;
        }
    }

    /**
     * <pre>
     * Return SYMBOL's toplevel default value.
     * "Toplevel" means outside of any let binding.
     * </pre>
     */
    @ELispBuiltIn(name = "default-toplevel-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDefaultToplevelValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Void defaultToplevelValue(Object symbol) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set SYMBOL's toplevel default value to VALUE.
     * "Toplevel" means outside of any let binding.
     * </pre>
     */
    @ELispBuiltIn(name = "set-default-toplevel-value", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetDefaultToplevelValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setDefaultToplevelValue(Object symbol, Object value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Define SYMBOL as a variable, with DOC as its docstring.
     * This is like `defvar' and `defconst' but without affecting the variable's
     * value.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--define-uninitialized-variable", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalDefineUninitializedVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol internalDefineUninitializedVariable(ELispSymbol symbol, Object doc) {
            // TODO: Chech Emacs implementation
            return symbol;
        }
    }

    /**
     * <pre>
     * Define SYMBOL as a variable, and return SYMBOL.
     * You are not required to define a variable in order to use it, but
     * defining it lets you supply an initial value and documentation, which
     * can be referred to by the Emacs help facilities and other programming
     * tools.
     *
     * If SYMBOL's value is void and the optional argument INITVALUE is
     * provided, INITVALUE is evaluated and the result used to set SYMBOL's
     * value.  If SYMBOL is buffer-local, its default value is what is set;
     * buffer-local values are not affected.  If INITVALUE is missing,
     * SYMBOL's value is not set.
     *
     * If INITVALUE is provided, the `defvar' form also declares the variable
     * as \"special\", so that it is always dynamically bound even if
     * `lexical-binding' is t.  If INITVALUE is missing, the form marks the
     * variable \"special\" locally (i.e., within the current
     * lexical scope, or the current file, if the form is at top-level),
     * and does nothing if `lexical-binding' is nil.
     *
     * If SYMBOL is let-bound, then this form does not affect the local let
     * binding but the toplevel default binding instead, like
     * `set-toplevel-default-binding`.
     * (`defcustom' behaves similarly in this respect.)
     *
     * The optional argument DOCSTRING is a documentation string for the
     * variable.
     *
     * To define a user option, use `defcustom' instead of `defvar'.
     *
     * To define a buffer-local variable, use `defvar-local'.
     * usage: (defvar SYMBOL &amp;optional INITVALUE DOCSTRING)
     * </pre>
     */
    @ELispBuiltIn(name = "defvar", minArgs = 1, maxArgs = 3, varArgs = false, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FDefvar extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol defvar(ELispSymbol symbol, Object initvalue, Object docstring) {
            if (ELispSymbol.isNil(initvalue)) {
                ELispBindingScope.markDynamic(symbol);
            } else {
                symbol.setSpecial(true);
                symbol.setDefaultValue(evalSub(initvalue));
            }
            return symbol;
        }
    }

    /**
     * <pre>
     * Like `defvar' but as a function.
     * More specifically behaves like (defvar SYM 'INITVALUE DOCSTRING).
     * </pre>
     */
    @ELispBuiltIn(name = "defvar-1", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefvar1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void defvar1(Object sym, Object initvalue, Object docstring) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Define SYMBOL as a constant variable.
     * This declares that neither programs nor users should ever change the
     * value.  This constancy is not actually enforced by Emacs Lisp, but
     * SYMBOL is marked as a special variable so that it is never lexically
     * bound.
     *
     * The `defconst' form always sets the value of SYMBOL to the result of
     * evalling INITVALUE.  If SYMBOL is buffer-local, its default value is
     * what is set; buffer-local values are not affected.  If SYMBOL has a
     * local binding, then this form sets the local binding's value.
     * However, you should normally not make local bindings for variables
     * defined with this form.
     *
     * The optional DOCSTRING specifies the variable's documentation string.
     * usage: (defconst SYMBOL INITVALUE [DOCSTRING])
     * </pre>
     */
    @ELispBuiltIn(name = "defconst", minArgs = 2, maxArgs = 3, varArgs = false, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FDefconst extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol defconst(ELispSymbol symbol, Object initvalue, Object docstring) {
            symbol.setValue(evalSub(initvalue));
            symbol.setSpecial(true);
            symbol.setConstant(true);
            return symbol;
        }
    }

    /**
     * <pre>
     * Like `defconst' but as a function.
     * More specifically, behaves like (defconst SYM 'INITVALUE DOCSTRING).
     * </pre>
     */
    @ELispBuiltIn(name = "defconst-1", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefconst1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void defconst1(Object sym, Object initvalue, Object docstring) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal function.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-make-var-non-special", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeVarNonSpecial extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeVarNonSpecial(Object symbol) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Bind variables according to VARLIST then eval BODY.
     * The value of the last form in BODY is returned.
     * Each element of VARLIST is a symbol (which is bound to nil)
     * or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
     * Each VALUEFORM can refer to the symbols already bound by this VARLIST.
     * usage: (let* VARLIST BODY...)
     * </pre>
     */
    @ELispBuiltIn(name = "let*", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FLetx extends ELispBuiltInBaseNode {
        @Specialization
        public static Object letx(Object varlist, Object[] body) {
            try (ELispBindingScope.@Nullable ClosableScope _ = FLet.makeScope(varlist, true)) {
                return FProgn.progn(body);
            }
        }
    }

    /**
     * <pre>
     * Bind variables according to VARLIST then eval BODY.
     * The value of the last form in BODY is returned.
     * Each element of VARLIST is a symbol (which is bound to nil)
     * or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
     * All the VALUEFORMs are evalled before any symbols are bound.
     * usage: (let VARLIST BODY...)
     * </pre>
     */
    @ELispBuiltIn(name = "let", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FLet extends ELispBuiltInBaseNode {
        public static ELispBindingScope.@Nullable ClosableScope makeScope(Object varlist, boolean progressive) {
            if (ELispSymbol.isNil(varlist)) {
                return null;
            }
            boolean dynamicBinding = ELispSymbol.isNil(LEXICAL_BINDING.getValue());
            HashMap<ELispSymbol, ELispSymbol.Value.Forwarded> lexicalBindings = new HashMap<>();
            ArrayList<ELispSymbol> specialBindings = new ArrayList<>();
            ArrayList<Object> specialValues = new ArrayList<>();
            ELispBindingScope.ClosableScope handle = null;
            if (progressive) {
                handle = ELispBindingScope.pushLexical(lexicalBindings);
            }
            try {
                for (Object assignment : (ELispCons) varlist) {
                    ELispSymbol symbol;
                    Object value;
                    if (assignment instanceof ELispSymbol sym) {
                        symbol = sym;
                        value = false;
                    } else {
                        ELispCons cons = (ELispCons) assignment;
                        symbol = (ELispSymbol) cons.car();
                        ELispCons cdr = (ELispCons) cons.cdr();
                        if (!ELispSymbol.isNil(cdr.cdr())) {
                            throw new IllegalArgumentException();
                        }
                        value = evalSub(cdr.car());
                    }
                    if (dynamicBinding || ELispBindingScope.isDynamic(symbol)) {
                        specialBindings.add(symbol);
                        specialValues.add(progressive ? symbol.swapThreadLocalValue(value) : value);
                    } else {
                        lexicalBindings.put(symbol, new ELispSymbol.Value.Forwarded(value));
                    }
                }
            } catch (Throwable e) {
                if (progressive) {
                    handle.close();
                    ELispBindingScope.pushDynamic(
                            specialBindings.toArray(new ELispSymbol[0]),
                            specialValues.toArray(new Object[0])
                    ).close();
                }
                throw e;
            }
            if (!progressive) {
                handle = ELispBindingScope.pushLexical(lexicalBindings);
                if (!specialBindings.isEmpty()) {
                    handle = ELispBindingScope.pushComposite(
                            handle,
                            ELispBindingScope.pushDynamic(
                                    specialBindings.toArray(new ELispSymbol[0]),
                                    specialValues.toArray(new Object[0])
                            )
                    );
                }
            } else {
                if (!specialBindings.isEmpty()) {
                    handle = ELispBindingScope.pushComposite(
                            handle,
                            new ELispBindingScope.ClosableScope.Dynamic(
                                    specialBindings.toArray(new ELispSymbol[0]),
                                    specialValues.toArray(new Object[0])
                            )
                    );
                }
            }
            return handle;
        }

        @Specialization
        public static Object let(Object varlist, Object[] body) {
            try (ELispBindingScope.@Nullable ClosableScope _ = makeScope(varlist, false)) {
                return FProgn.progn(body);
            }
        }
    }

    /**
     * <pre>
     * If TEST yields non-nil, eval BODY... and repeat.
     * The order of execution is thus TEST, BODY, TEST, BODY and so on
     * until TEST returns nil.
     *
     * The value of a `while' form is always nil.
     *
     * usage: (while TEST BODY...)
     * </pre>
     */
    @ELispBuiltIn(name = "while", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FWhile extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean while_(Object test, Object[] body) {
            while (!ELispSymbol.isNil(evalSub(test))) {
                FProgn.progn(body);
            }
            return false;
        }
    }

    /**
     * <pre>
     * Like `funcall', but display MESSAGE if FUNCTION takes longer than TIMEOUT.
     * TIMEOUT is a number of seconds, and can be an integer or a floating
     * point number.
     *
     * If FUNCTION takes less time to execute than TIMEOUT seconds, MESSAGE
     * is not displayed.
     * </pre>
     */
    @ELispBuiltIn(name = "funcall-with-delayed-message", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFuncallWithDelayedMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Void funcallWithDelayedMessage(Object timeout, Object message, Object function) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return result of expanding macros at top level of FORM.
     * If FORM is not a macro call, it is returned unchanged.
     * Otherwise, the macro is expanded and the expansion is considered
     * in place of FORM.  When a non-macro-call results, it is returned.
     *
     * The second optional arg ENVIRONMENT specifies an environment of macro
     * definitions to shadow the loaded ones for use in file byte-compilation.
     * </pre>
     */
    @ELispBuiltIn(name = "macroexpand", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMacroexpand extends ELispBuiltInBaseNode {
        @Specialization
        public static Void macroexpand(Object form, Object environment) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Eval BODY allowing nonlocal exits using `throw'.
     * TAG is evalled to get the tag to use; it must not be nil.
     *
     * Then the BODY is executed.
     * Within BODY, a call to `throw' with the same TAG exits BODY and this `catch'.
     * If no throw happens, `catch' returns the value of the last BODY form.
     * If a throw happens, it specifies the value to return from `catch'.
     * usage: (catch TAG BODY...)
     * </pre>
     */
    @ELispBuiltIn(name = "catch", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FCatch extends ELispBuiltInBaseNode {
        @Specialization
        public static Void catch_(Object tag, Object[] body) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Throw to the catch for TAG and return VALUE from it.
     * Both TAG and VALUE are evalled.
     * </pre>
     */
    @ELispBuiltIn(name = "throw", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FThrow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void throw_(Object tag, Object value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Do BODYFORM, protecting with UNWINDFORMS.
     * If BODYFORM completes normally, its value is returned
     * after executing the UNWINDFORMS.
     * If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.
     * usage: (unwind-protect BODYFORM UNWINDFORMS...)
     * </pre>
     */
    @ELispBuiltIn(name = "unwind-protect", minArgs = 1, maxArgs = 1, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FUnwindProtect extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unwindProtect(Object bodyform, Object[] unwindforms) {
            // TODO: Add test once we have signal support
            try {
                return evalSub(bodyform);
            } catch (Exception e) {
                for (Object unwindform : unwindforms) {
                    evalSub(unwindform);
                }
                throw e;
            }
        }
    }

    /**
     * <pre>
     * Regain control when an error is signaled.
     * Executes BODYFORM and returns its value if no error happens.
     * Each element of HANDLERS looks like (CONDITION-NAME BODY...)
     * or (:success BODY...), where the BODY is made of Lisp expressions.
     *
     * A handler is applicable to an error if CONDITION-NAME is one of the
     * error's condition names.  Handlers may also apply when non-error
     * symbols are signaled (e.g., `quit').  A CONDITION-NAME of t applies to
     * any symbol, including non-error symbols.  If multiple handlers are
     * applicable, only the first one runs.
     *
     * The car of a handler may be a list of condition names instead of a
     * single condition name; then it handles all of them.  If the special
     * condition name `debug' is present in this list, it allows another
     * condition in the list to run the debugger if `debug-on-error' and the
     * other usual mechanisms say it should (otherwise, `condition-case'
     * suppresses the debugger).
     *
     * When a handler handles an error, control returns to the `condition-case'
     * and it executes the handler's BODY...
     * with VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.
     * \(If VAR is nil, the handler can't access that information.)
     * Then the value of the last BODY form is returned from the `condition-case'
     * expression.
     *
     * The special handler (:success BODY...) is invoked if BODYFORM terminated
     * without signaling an error.  BODY is then evaluated with VAR bound to
     * the value returned by BODYFORM.
     *
     * See also the function `signal' for more info.
     * usage: (condition-case VAR BODYFORM &amp;rest HANDLERS)
     * </pre>
     */
    @ELispBuiltIn(name = "condition-case", minArgs = 2, maxArgs = 2, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FConditionCase extends ELispBuiltInBaseNode {
        @Specialization
        public static Void conditionCase(Object var, Object bodyform, Object[] handlers) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set up error handlers around execution of BODYFUN.
     * BODYFUN should be a function and it is called with no arguments.
     * CONDITIONS should be a list of condition names (symbols).
     * When an error is signaled during execution of BODYFUN, if that
     * error matches one of CONDITIONS, then the associated HANDLER is
     * called with the error as argument.
     * HANDLER should either transfer the control via a non-local exit,
     * or return normally.
     * If it returns normally, the search for an error handler continues
     * from where it left off.
     *
     * usage: (handler-bind BODYFUN [CONDITIONS HANDLER]...)
     * </pre>
     */
    @ELispBuiltIn(name = "handler-bind-1", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FHandlerBind1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void handlerBind1(Object bodyfun, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Signal an error.  Args are ERROR-SYMBOL and associated DATA.
     * This function does not return.
     *
     * When `noninteractive' is non-nil (in particular, in batch mode), an
     * unhandled error calls `kill-emacs', which terminates the Emacs
     * session with a non-zero exit code.
     *
     * An error symbol is a symbol with an `error-conditions' property
     * that is a list of condition names.  The symbol should be non-nil.
     * A handler for any of those names will get to handle this signal.
     * The symbol `error' should normally be one of them.
     *
     * DATA should be a list.  Its elements are printed as part of the error message.
     * See Info anchor `(elisp)Definition of signal' for some details on how this
     * error message is constructed.
     * If the signal is handled, DATA is made available to the handler.
     * See also the function `condition-case'.
     * </pre>
     */
    @ELispBuiltIn(name = "signal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSignal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void signal(Object errorSymbol, Object data) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Non-nil if FUNCTION makes provisions for interactive calling.
     * This means it contains a description for how to read arguments to give it.
     * The value is nil for an invalid function or a symbol with no function
     * definition.
     *
     * Interactively callable functions include strings and vectors (treated
     * as keyboard macros), lambda-expressions that contain a top-level call
     * to `interactive', autoload definitions made by `autoload' with non-nil
     * fourth argument, and some of the built-in functions of Lisp.
     *
     * Also, a symbol satisfies `commandp' if its function definition does so.
     *
     * If the optional argument FOR-CALL-INTERACTIVELY is non-nil,
     * then strings and vectors are not accepted.
     * </pre>
     */
    @ELispBuiltIn(name = "commandp", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCommandp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void commandp(Object function, Object forCallInteractively) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Define FUNCTION to autoload from FILE.
     * FUNCTION is a symbol; FILE is a file name string to pass to `load'.
     *
     * Third arg DOCSTRING is documentation for the function.
     *
     * Fourth arg INTERACTIVE if non-nil says function can be called
     * interactively.  If INTERACTIVE is a list, it is interpreted as a list
     * of modes the function is applicable for.
     *
     * Fifth arg TYPE indicates the type of the object:
     *    nil or omitted says FUNCTION is a function,
     *    `keymap' says FUNCTION is really a keymap, and
     *    `macro' or t says FUNCTION is really a macro.
     *
     * Third through fifth args give info about the real definition.
     * They default to nil.
     *
     * If FUNCTION is already defined other than as an autoload,
     * this does nothing and returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "autoload", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FAutoload extends ELispBuiltInBaseNode {
        @Specialization
        public static Object autoload(ELispSymbol function, ELispString file, Object docstring, Object interactive, Object type) {
            // TODO: Add support in evalSub
            if (!ELispSymbol.isNil(function.getFunction())) {
                return false;
            }
            function.setFunction(new ELispCons.ListBuilder()
                    .add(AUTOLOAD)
                    .add(file)
                    .add(docstring)
                    .add(interactive)
                    .add(type)
                    .build());
            return function;
        }
    }

    /**
     * <pre>
     * Load FUNDEF which should be an autoload.
     * If non-nil, FUNNAME should be the symbol whose function value is FUNDEF,
     * in which case the function returns the new autoloaded function value.
     * If equal to `macro', MACRO-ONLY specifies that FUNDEF should only be loaded if
     * it defines a macro.
     * </pre>
     */
    @ELispBuiltIn(name = "autoload-do-load", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAutoloadDoLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static Void autoloadDoLoad(Object fundef, Object funname, Object macroOnly) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Evaluate FORM and return its value.
     * If LEXICAL is `t', evaluate using lexical binding by default.
     * This is the recommended value.
     *
     * If absent or `nil', use dynamic scoping only.
     *
     * LEXICAL can also represent an actual lexical environment; see the Info
     * node `(elisp)Eval' for details.
     * </pre>
     */
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

    /**
     * <pre>
     * Call FUNCTION with our remaining args, using our last arg as list of args.
     * Then return the value FUNCTION returns.
     * With a single argument, call the argument's first element using the
     * other elements as args.
     * Thus, (apply \\='+ 1 2 \\='(3 4)) returns 10.
     * usage: (apply FUNCTION &amp;rest ARGUMENTS)
     * </pre>
     */
    @ELispBuiltIn(name = "apply", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Object apply(Object function, Object[] arguments) {
            List<Object> objects = new ArrayList<>(Arrays.asList(arguments).subList(0, arguments.length - 1));
            Object last = arguments[arguments.length - 1];
            if (!ELispSymbol.isNil(last)) {
                objects.addAll((ELispCons) last);
            }
            arguments = objects.toArray();
            return FFuncall.funcall(function, arguments);
        }
    }

    /**
     * <pre>
     * Run each hook in HOOKS.
     * Each argument should be a symbol, a hook variable.
     * These symbols are processed in the order specified.
     * If a hook symbol has a non-nil value, that value may be a function
     * or a list of functions to be called to run the hook.
     * If the value is a function, it is called with no arguments.
     * If it is a list, the elements are called, in order, with no arguments.
     *
     * Major modes should not use this function directly to run their mode
     * hook; they should use `run-mode-hooks' instead.
     *
     * Do not use `make-local-variable' to make a hook variable buffer-local.
     * Instead, use `add-hook' and specify t for the LOCAL argument.
     * usage: (run-hooks &amp;rest HOOKS)
     * </pre>
     */
    @ELispBuiltIn(name = "run-hooks", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHooks extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean runHooks(Object[] hooks) {
            for (Object hook : hooks) {
                Object value = ((ELispSymbol) hook).getValue();
                if (FFunctionp.functionp(value)) {
                    FFuncall.funcall(value, new Object[0]);
                } else if (value instanceof ELispCons cons) {
                    for (Object element : cons) {
                        FFuncall.funcall(element, new Object[0]);
                    }
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Run HOOK with the specified arguments ARGS.
     * HOOK should be a symbol, a hook variable.  The value of HOOK
     * may be nil, a function, or a list of functions.  Call each
     * function in order with arguments ARGS.  The final return value
     * is unspecified.
     *
     * Do not use `make-local-variable' to make a hook variable buffer-local.
     * Instead, use `add-hook' and specify t for the LOCAL argument.
     * usage: (run-hook-with-args HOOK &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "run-hook-with-args", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgs extends ELispBuiltInBaseNode {
        @Specialization
        public static Void runHookWithArgs(Object hook, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Run HOOK with the specified arguments ARGS.
     * HOOK should be a symbol, a hook variable.  The value of HOOK
     * may be nil, a function, or a list of functions.  Call each
     * function in order with arguments ARGS, stopping at the first
     * one that returns non-nil, and return that value.  Otherwise (if
     * all functions return nil, or if there are no functions to call),
     * return nil.
     *
     * Do not use `make-local-variable' to make a hook variable buffer-local.
     * Instead, use `add-hook' and specify t for the LOCAL argument.
     * usage: (run-hook-with-args-until-success HOOK &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "run-hook-with-args-until-success", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgsUntilSuccess extends ELispBuiltInBaseNode {
        @Specialization
        public static Void runHookWithArgsUntilSuccess(Object hook, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Run HOOK with the specified arguments ARGS.
     * HOOK should be a symbol, a hook variable.  The value of HOOK
     * may be nil, a function, or a list of functions.  Call each
     * function in order with arguments ARGS, stopping at the first
     * one that returns nil, and return nil.  Otherwise (if all functions
     * return non-nil, or if there are no functions to call), return non-nil
     * \(do not rely on the precise return value in this case).
     *
     * Do not use `make-local-variable' to make a hook variable buffer-local.
     * Instead, use `add-hook' and specify t for the LOCAL argument.
     * usage: (run-hook-with-args-until-failure HOOK &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "run-hook-with-args-until-failure", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWithArgsUntilFailure extends ELispBuiltInBaseNode {
        @Specialization
        public static Void runHookWithArgsUntilFailure(Object hook, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Run HOOK, passing each function through WRAP-FUNCTION.
     * I.e. instead of calling each function FUN directly with arguments ARGS,
     * it calls WRAP-FUNCTION with arguments FUN and ARGS.
     * As soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'
     * aborts and returns that value.
     * usage: (run-hook-wrapped HOOK WRAP-FUNCTION &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "run-hook-wrapped", minArgs = 2, maxArgs = 2, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRunHookWrapped extends ELispBuiltInBaseNode {
        @Specialization
        public static Void runHookWrapped(Object hook, Object wrapFunction, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a function.
     *
     * An object is a function if it is callable via `funcall'; this includes
     * symbols with function bindings, but excludes macros and special forms.
     *
     * Ordinarily return nil if OBJECT is not a function, although t might be
     * returned in rare cases.
     * </pre>
     */
    @ELispBuiltIn(name = "functionp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFunctionp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean functionp(Object object) {
            return (object instanceof ELispSubroutine(_, boolean special) && !special)
                    || object instanceof ELispInterpretedClosure
                    || (object instanceof ELispSymbol symbol && functionp(symbol.getIndirectFunction()));
        }
    }

    /**
     * <pre>
     * Call first argument as a function, passing remaining arguments to it.
     * Return the value that function returns.
     * Thus, (funcall \\='cons \\='x \\='y) returns (x . y).
     * usage: (funcall FUNCTION &amp;rest ARGUMENTS)
     * </pre>
     */
    @ELispBuiltIn(name = "funcall", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFuncall extends ELispBuiltInBaseNode {
        @Specialization
        public static Object funcall(Object function, Object[] arguments) {
            if (function instanceof ELispSymbol symbol) {
                function = symbol.getIndirectFunction();
            }
            if (function instanceof ELispSubroutine subroutine) {
                return subroutine.body().call(arguments);
            }
            if (function instanceof ELispInterpretedClosure closure) {
                return closure.getCallTarget().call(arguments);
            }
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return minimum and maximum number of args allowed for FUNCTION.
     * FUNCTION must be a function of some kind.
     * The returned value is a cons cell (MIN . MAX).  MIN is the minimum number
     * of args.  MAX is the maximum number, or the symbol `many', for a
     * function with `&amp;rest' args, or `unevalled' for a special form.
     * </pre>
     */
    @ELispBuiltIn(name = "func-arity", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFuncArity extends ELispBuiltInBaseNode {
        @Specialization
        public static Void funcArity(Object function) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if SYMBOL's global binding has been declared special.
     * A special variable is one that will be bound dynamically, even in a
     * context where binding is lexical by default.
     * </pre>
     */
    @ELispBuiltIn(name = "special-variable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSpecialVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void specialVariableP(Object symbol) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
     * LEVEL and BASE specify the activation frame to use, as in `backtrace-frame'.
     * The debugger is entered when that frame exits, if the flag is non-nil.
     * </pre>
     */
    @ELispBuiltIn(name = "backtrace-debug", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBacktraceDebug extends ELispBuiltInBaseNode {
        @Specialization
        public static Void backtraceDebug(Object level, Object flag, Object base) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Call FUNCTION for each frame in backtrace.
     * If BASE is non-nil, it should be a function and iteration will start
     * from its nearest activation frame.
     * FUNCTION is called with 4 arguments: EVALD, FUNC, ARGS, and FLAGS.  If
     * a frame has not evaluated its arguments yet or is a special form,
     * EVALD is nil and ARGS is a list of forms.  If a frame has evaluated
     * its arguments and called its function already, EVALD is t and ARGS is
     * a list of values.
     * FLAGS is a plist of properties of the current frame: currently, the
     * only supported property is :debug-on-exit.  `mapbacktrace' always
     * returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "mapbacktrace", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapbacktrace extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mapbacktrace(Object function, Object base) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Call FUNCTION on stack frame NFRAMES away from BASE.
     * Return the result of FUNCTION, or nil if no matching frame could be found.
     * </pre>
     */
    @ELispBuiltIn(name = "backtrace-frame--internal", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBacktraceFrameInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void backtraceFrameInternal(Object function, Object nframes, Object base) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the list of backtrace frames from current execution point in THREAD.
     * If a frame has not evaluated the arguments yet (or is a special form),
     * the value of the list element is (nil FUNCTION ARG-FORMS...).
     * If a frame has evaluated its arguments and called its function already,
     * the value of the list element is (t FUNCTION ARG-VALUES...).
     * A &amp;rest arg is represented as the tail of the list ARG-VALUES.
     * FUNCTION is whatever was supplied as car of evaluated list,
     * or a lambda expression for macro calls.
     * </pre>
     */
    @ELispBuiltIn(name = "backtrace--frames-from-thread", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBacktraceFramesFromThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Void backtraceFramesFromThread(Object thread) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Evaluate EXP in the context of some activation frame.
     * NFRAMES and BASE specify the activation frame to use, as in `backtrace-frame'.
     * </pre>
     */
    @ELispBuiltIn(name = "backtrace-eval", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBacktraceEval extends ELispBuiltInBaseNode {
        @Specialization
        public static Void backtraceEval(Object exp, Object nframes, Object base) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return names and values of local variables of a stack frame.
     * NFRAMES and BASE specify the activation frame to use, as in `backtrace-frame'.
     * </pre>
     */
    @ELispBuiltIn(name = "backtrace--locals", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBacktraceLocals extends ELispBuiltInBaseNode {
        @Specialization
        public static Void backtraceLocals(Object nframes, Object base) {
            throw new UnsupportedOperationException();
        }
    }
}
