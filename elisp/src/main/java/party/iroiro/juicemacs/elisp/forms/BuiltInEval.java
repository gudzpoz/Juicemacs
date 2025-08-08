package party.iroiro.juicemacs.elisp.forms;

import java.util.*;
import java.util.function.BiFunction;

import com.oracle.truffle.api.*;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.nodes.*;

import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.profiles.LoopConditionProfile;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;

import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.nodes.ast.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.nodes.ast.ELispLiteralNodes;
import party.iroiro.juicemacs.elisp.nodes.ast.ELispRootNodes;
import party.iroiro.juicemacs.elisp.nodes.ast.LazyConsExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.*;
import party.iroiro.juicemacs.elisp.nodes.local.*;
import party.iroiro.juicemacs.elisp.runtime.*;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import static party.iroiro.juicemacs.elisp.forms.BuiltInLRead.loadFile;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

/**
 * Built-in functions from {@code src/eval.c}
 */
@SuppressWarnings({"DefaultAnnotationParam", "ForLoopReplaceableByForEach"})
public class BuiltInEval extends ELispBuiltIns {

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEvalFactory.getFactories();
    }

    static void checkSoftExit(ELispContext context, RuntimeException e) {
        boolean hardExit = context.options().hardExit();
        CompilerAsserts.partialEvaluationConstant(hardExit);
        if (!hardExit && e instanceof ELispSignals.ELispSignalException ex
                && ex.getExceptionType() == ExceptionType.EXIT) {
            throw e;
        }
    }

    public static final class ScopeWrapperNode extends ELispExpressionNode implements ELispLexical.ScopeProvider {
        @Child
        ELispExpressionNode inner;

        private final ELispLexical.Scope scope;

        public ScopeWrapperNode(ELispExpressionNode inner, ELispLexical.Scope scope) {
            this.inner = inner;
            this.scope = scope;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            inner.executeVoid(frame);
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            return inner.executeLong(frame);
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            return inner.executeDouble(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return inner.executeGeneric(frame);
        }

        @Override
        public ELispLexical.Scope getScope() {
            return scope;
        }
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
    public abstract static class FOr extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void orBailout(Object[] conditions) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new OrNode(arguments);
        }

        private static class OrNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            private ELispExpressionNode[] nodes;

            public OrNode(Object[] conditions) {
                nodes = ELispInterpretedNode.create(conditions);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                executeGeneric(frame);
            }

            @ExplodeLoop
            @Override
            public Object executeGeneric(VirtualFrame frame) {
                int length = nodes.length;
                for (int i = 0; i < length; i++) {
                    Object result = nodes[i].executeGeneric(frame);
                    if (!isNil(result)) {
                        return result;
                    }
                }
                return false;
            }
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
    public abstract static class FAnd extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void andBailout(Object[] conditions) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new AndNode(arguments);
        }

        private static class AndNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            private ELispExpressionNode[] nodes;

            public AndNode(Object[] conditions) {
                nodes = ELispInterpretedNode.create(conditions);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                executeGeneric(frame);
            }

            @ExplodeLoop
            @Override
            public Object executeGeneric(VirtualFrame frame) {
                Object lastResult = true;
                int length = nodes.length;
                for (int i = 0; i < length; i++) {
                    ELispExpressionNode node = nodes[i];
                    lastResult = node.executeGeneric(frame);
                    if (isNil(lastResult)) {
                        return false;
                    }
                }
                return lastResult;
            }
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
    public abstract static class FIf extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void ifBailout(Object cond, Object then, Object[] else_) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new IfNode(arguments[0], arguments[1], Arrays.copyOfRange(arguments, 2, arguments.length));
        }

        private static class IfNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode condition;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode thenBranch;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode elseBranch;

            final ConditionProfile conditionProfile;

            public IfNode(Object cond, Object then, Object[] else_) {
                condition = ELispInterpretedNode.create(cond);
                thenBranch = ELispInterpretedNode.create(then);
                elseBranch = FProgn.progn(else_);
                conditionProfile = ConditionProfile.create();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                if (conditionProfile.profile(isNil(condition.executeGeneric(frame)))) {
                    elseBranch.executeVoid(frame);
                    return;
                }
                thenBranch.executeVoid(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                if (isNil(condition.executeGeneric(frame))) {
                    return elseBranch.executeGeneric(frame);
                }
                return thenBranch.executeGeneric(frame);
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
    public abstract static class FCond extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void condBailout(Object[] clauses) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new CondNode(arguments);
        }

        private static class CondNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            ELispExpressionNode[] conditions;
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            @Nullable ELispExpressionNode[] thenCases;

            public CondNode(Object[] clauses) {
                List<ELispExpressionNode> conditionNodes = new ArrayList<>(clauses.length);
                List<@Nullable ELispExpressionNode> cases = new ArrayList<>(clauses.length);
                for (Object clause : clauses) {
                    ELispCons cons = asCons(clause);
                    conditionNodes.add(ELispInterpretedNode.create(cons.car()));
                    List<Object> body = new ArrayList<>();
                    ELispCons.ConsIterator iterator = cons.listIterator(1);
                    while (iterator.hasNext()) {
                        body.add(iterator.next());
                    }
                    cases.add(body.isEmpty() ? null : FProgn.progn(body.toArray(new Object[0])));
                }
                conditions = conditionNodes.toArray(new ELispExpressionNode[0]);
                thenCases = cases.toArray(new ELispExpressionNode[0]);
            }

            @ExplodeLoop
            @Override
            public void executeVoid(VirtualFrame frame) {
                int length = conditions.length;
                for (int i = 0; i < length; i++) {
                    Object condition = conditions[i].executeGeneric(frame);
                    if (!isNil(condition)) {
                        @Nullable ELispExpressionNode then = thenCases[i];
                        if (then != null) {
                            then.executeVoid(frame);
                        }
                        return;
                    }
                }
            }

            @ExplodeLoop
            @Override
            public Object executeGeneric(VirtualFrame frame) {
                int length = conditions.length;
                for (int i = 0; i < length; i++) {
                    Object condition = conditions[i].executeGeneric(frame);
                    if (!isNil(condition)) {
                        @Nullable ELispExpressionNode then = thenCases[i];
                        return then == null ? condition : then.executeGeneric(frame);
                    }
                }
                return false;
            }
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
    public abstract static class FProgn extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void prognBailout(Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return progn(arguments);
        }

        public static ELispExpressionNode progn(Object[] body) {
            return prognNode(ELispInterpretedNode.create(body));
        }

        public static ELispExpressionNode prognNode(ELispExpressionNode[] nodes) {
            if (nodes.length == 0) {
                return ELispLiteralNodes.of(false);
            }
            if (nodes.length == 1) {
                return nodes[0];
            }
            return new PrognBlockNode(nodes);
        }

        public static final class PrognBlockNode extends ELispExpressionNode
                implements BlockNode.ElementExecutor<ELispExpressionNode> {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            BlockNode<ELispExpressionNode> block;

            public PrognBlockNode(ELispExpressionNode[] body) {
                block = BlockNode.create(body, this);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                block.executeVoid(frame, BlockNode.NO_ARGUMENT);
            }

            @Override
            public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
                return block.executeLong(frame, BlockNode.NO_ARGUMENT);
            }

            @Override
            public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
                return block.executeDouble(frame, BlockNode.NO_ARGUMENT);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                return block.executeGeneric(frame, BlockNode.NO_ARGUMENT);
            }

            @Override
            public void executeVoid(VirtualFrame frame, ELispExpressionNode node, int index, int argument) {
                node.executeVoid(frame);
            }

            @Override
            public long executeLong(VirtualFrame frame, ELispExpressionNode node, int index, int argument) throws UnexpectedResultException {
                return node.executeLong(frame);
            }

            @Override
            public double executeDouble(VirtualFrame frame, ELispExpressionNode node, int index, int argument) throws UnexpectedResultException {
                return node.executeDouble(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame, ELispExpressionNode node, int index, int argument) {
                return node.executeGeneric(frame);
            }
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
    public abstract static class FProg1 extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void prog1Bailout(Object first, Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new Prog1Node(arguments[0], Arrays.copyOfRange(arguments, 1, arguments.length));
        }

        private static class Prog1Node extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode firstNode;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode others;

            public Prog1Node(Object first, Object[] body) {
                firstNode = ELispInterpretedNode.create(first);
                others = FProgn.progn(body);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                firstNode.executeVoid(frame);
                others.executeVoid(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                Object result = firstNode.executeGeneric(frame);
                others.executeVoid(frame);
                return result;
            }
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
    public abstract static class FSetq extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        static final class FSetqItem extends ELispExpressionNode {
            private final ELispSymbol symbol;

            @Child
            @Nullable
            ELispExpressionNode writeNode;

            private final ELispExpressionNode inner;
            private final Object value;

            FSetqItem(ELispSymbol symbol, Object value) {
                this.symbol = symbol;
                this.inner = ELispInterpretedNode.create(value);
                this.value = value;
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                updateSlots().executeVoid(frame);
            }

            @Override
            public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
                return updateSlots().executeLong(frame);
            }

            @Override
            public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
                return updateSlots().executeDouble(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                return updateSlots().executeGeneric(frame);
            }

            private ELispExpressionNode updateSlots() {
                ELispExpressionNode write = writeNode;
                if (write != null) {
                    return write;
                }

                CompilerDirectives.transferToInterpreterAndInvalidate();
                ELispLexical.@Nullable LexicalReference reference = ELispLexical.getLexicalReference(this, symbol);
                ELispExpressionNode replace = reference == null
                        ? GlobalVariableWriteNodeGen.create(symbol, inner)
                        : ELispFrameSlotWriteNode.createWrite(reference.level(), reference.index(), inner);
                if (value instanceof ELispCons cons) {
                    replace = new ELispInterpretedNode.SourceSectionWrapper(cons, replace); // NOPMD: replace called later
                }
                return replace(replace);
            }
        }

        @Specialization
        public static Void setqBailout(Object[] args) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            if (arguments.length % 2 != 0) {
                return new ELispExpressionNode() {
                    @Override
                    public void executeVoid(VirtualFrame frame) {
                        super.executeVoid(frame);
                    }

                    @Override
                    public Object executeGeneric(VirtualFrame frame) {
                        throw ELispSignals.wrongNumberOfArguments(SETQ, arguments.length);
                    }
                };
            }
            if (arguments.length == 0) {
                return ELispLiteralNodes.of(false);
            }
            int half = arguments.length / 2;
            ELispExpressionNode[] assignments = new ELispExpressionNode[half];
            for (int i = 0; i < half; i++) {
                assignments[i] = new FSetqItem(asSym(arguments[i * 2]), arguments[i * 2 + 1]);
            }
            return FProgn.prognNode(assignments);
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
    public abstract static class FQuote extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void quoteBailout(Object arg) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return ELispLiteralNodes.of(arguments[0]);
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
    public abstract static class FMakeInterpretedClosure extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static ELispInterpretedClosure makeInterpretedClosure(
                Object args, ELispCons body, Object env, Object docstring, Object iform
        ) {
            AbstractELispClosure.ClosureCommons commons;
            if (args instanceof ELispInterpretedClosure closure) {
                // This branch is only executed when created via createNode, which see.
                commons = closure.getCommons();
                args = closure.get(CLOSURE_ARGLIST);
            } else {
                commons = new AbstractELispClosure.ClosureCommons();
            }
            return makeClosure(
                    args, body, env, docstring, iform,
                    commons
            );
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments[0] instanceof LazyConsExpressionNode cons) {
                // `oclosure--copy` cannot preserve lambda root nodes, because it "destructures"
                // the closure, when root nodes are stored within closure objects.
                // This is a hack to get the real closure object.
                if (cons.cons.car() == AREF && cons.cons.cdr() instanceof ELispCons cdr
                        && cons.cons.cdr() instanceof ELispCons cddr
                        && cddr.car() instanceof Long l && l == CLOSURE_ARGLIST) {
                    arguments[0] = ELispInterpretedNode.create(cddr.car());
                }
            }
            return BuiltInEvalFactory.FMakeInterpretedClosureFactory.create(arguments);
        }

        public static ELispInterpretedClosure makeClosure(
                Object args, ELispCons body, Object env, Object docstring, Object iform,
                AbstractELispClosure.ClosureCommons commons
        ) {
            List<Object> inner = new ArrayList<>(CLOSURE_INTERACTIVE + 1);
            inner.add(args);
            inner.add(body);
            inner.add(env);
            boolean hasDoc = !isNil(docstring);
            boolean hasIForm = !isNil(iform);
            if (hasDoc || hasIForm) {
                inner.add(false); // CLOSURE_STACK_DEPTH
                inner.add(docstring);
                if (hasIForm) {
                    ELispCons cons = asCons(iform);
                    if (isNil(cons.cdr())) {
                        iform = false;
                    } else {
                        ELispCons iArgs = asCons(cons.cdr());
                        if (isNil(iArgs.cdr())) {
                            iform = iArgs.car();
                        } else {
                            iform = new ELispVector(new Object[]{iArgs.car(), iArgs.cdr()});
                        }
                    }
                    inner.add(iform);
                }
            }
            return (ELispInterpretedClosure) AbstractELispClosure.create(inner, commons);
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
    public abstract static class FFunction extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void functionBailout(Object arg) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return function(arguments[0]);
        }

        public static ELispExpressionNode function(Object arg) {
            if (arg instanceof ELispCons def && def.car() == LAMBDA) {
                return getDefinition(def);
            }
            return ELispLiteralNodes.of(arg);
        }

        public static Object getFunction(ELispCons def, @Nullable Node node) {
            ELispExpressionNode definition = getDefinition(def);
            if (node != null) {
                node.insert(definition);
            }
            return definition.executeGeneric(null);
        }

        private static ELispExpressionNode getDefinition(ELispCons def) {
            CompilerDirectives.transferToInterpreter();
            ELispCons.ConsIterator iterator = def.listIterator(1);
            Object args = iterator.next();
            ELispExpressionNode docString = null;
            if (iterator.hasNext()) {
                docString = switch (iterator.currentCons().car()) {
                    case ELispString s -> ELispInterpretedNode.create(s);
                    case ELispCons cons when cons.car() == CDOCUMENTATION && cons.cdr() instanceof ELispCons doc ->
                            ELispInterpretedNode.create(doc.car());
                    default -> null;
                };
            }
            if (docString == null) {
                docString = ELispInterpretedNode.create(false);
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
            ELispCons body = iterator.hasNext() ? iterator.currentCons() : ELispCons.listOf(false);
            body.fillDebugInfo(def);
            ELispExpressionNode finalDocString = docString;
            Object finalInteractive = interactive;
            return new ELispExpressionNode() {
                @SuppressWarnings("FieldMayBeFinal")
                @Child
                ELispExpressionNode doc = finalDocString;

                private AbstractELispClosure.@Nullable ClosureCommons commons = null;

                @Override
                public void executeVoid(VirtualFrame frame) {
                    executeGeneric(frame);
                }

                @Override
                public Object executeGeneric(@Nullable VirtualFrame frame) {
                    ELispLexical.@Nullable Scope scope = ELispLexical.getScope(this);
                    Object env = scope == null || frame == null
                            ? false
                            : new ELispLexical.Captured(scope, frame.materialize());
                    Object doc = this.doc.executeGeneric(frame);
                    return createClosure(scope, doc, env);
                }

                private AbstractELispClosure createClosure(ELispLexical.@Nullable Scope scope, Object doc, Object env) {
                    body.fillDebugInfo(getParent());
                    if (commons == null) {
                        commons = new AbstractELispClosure.ClosureCommons(getRootNode());
                    }
                    // TODO: make use of INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION to clean up frames
                    ELispInterpretedClosure scopeHolder = FMakeInterpretedClosure.makeClosure(
                            args,
                            body,
                            scope == null ? false : ELispCons.listOf(true),
                            doc,
                            finalInteractive,
                            commons
                    );
                    scopeHolder.set(CLOSURE_CONSTANTS, env);
                    return scopeHolder;
                }
            };
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
                throw ELispSignals.error("Cannot make a constant an alias");
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
        public Object defaultToplevelValue(ELispSymbol symbol) {
            Optional<ValueStorage> storage = getContext().getStorageLazy(symbol);
            if (storage.isPresent()) {
                Object value = storage.get().getDefaultValue();
                if (value != ValueStorage.UNBOUND) {
                    return value;
                }
            }
            throw ELispSignals.voidVariable(symbol);
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
        public static boolean setDefaultToplevelValue(ELispSymbol symbol, Object value) {
            symbol.setDefaultValue(value);
            return false;
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
    public abstract static class FDefvar extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void defvarBailout(Object symbol, Object initvalue, Object docstring) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            boolean initValueMissing = arguments.length < 2;
            return new DefVarNode(arguments[0], initValueMissing ? false : arguments[1], initValueMissing);
        }

        private static class DefVarNode extends ELispExpressionNode {
            private final Object symbol;
            private final boolean initValueMissing;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode init;

            public DefVarNode(Object symbol, Object initvalue, boolean initValueMissing) {
                this.symbol = symbol;
                this.initValueMissing = initValueMissing;
                init = ELispInterpretedNode.create(initvalue);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                executeGeneric(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ELispSymbol sym = asSym(symbol);
                if (initValueMissing && !ELispLexical.isRootScope(this)) {
                    ELispLexical.markAsDynamic(this, sym);
                } else {
                    sym.setSpecial(true);
                    if (!initValueMissing && sym.getAnyDefaultValue() == ValueStorage.UNBOUND) {
                        sym.setDefaultValue(init.executeGeneric(frame));
                    }
                }
                return replace(ELispLiteralNodes.of(symbol)).executeGeneric(frame);
            }
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
        public static Object defvar1(ELispSymbol sym, Object initvalue, Object docstring) {
            sym.setSpecial(true);
            sym.setDefaultValue(initvalue);
            return sym;
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
    public abstract static class FDefconst extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void defconstBailout(Object symbol, Object initvalue, Object docstring) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new DefConstNode(arguments[0], arguments[1]);
        }

        private static class DefConstNode extends ELispExpressionNode {
            private final Object symbol;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode init;

            public DefConstNode(Object symbol, Object initvalue) {
                this.symbol = symbol;
                init = ELispInterpretedNode.create(initvalue);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                executeGeneric(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ELispSymbol sym = asSym(symbol);
                sym.setValue(init.executeGeneric(frame));
                sym.setSpecial(true);
                // Emacs actually allows modifying defconst constants...
                // So we are not calling sym.setConstant(true) here.
                return symbol;
            }
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
        public static ELispSymbol defconst1(ELispSymbol sym, Object initvalue, Object docstring) {
            sym.setValue(initvalue);
            sym.setSpecial(true);
            return sym;
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
        public boolean makeVarNonSpecial(ELispSymbol symbol) {
            ELispContext context = getContext();
            context.getStorage(symbol).setSpecial(context, false);
            return false;
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
    public abstract static class FLetx extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void letxBailout(Object varlist, Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            Object[] body = Arrays.copyOfRange(arguments, 1, arguments.length);
            if (isNil(arguments[0])) {
                return FProgn.progn(body);
            }
            return new LetNode(arguments[0], body, true);
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
    public abstract static class FLet extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void letBailout(Object varlist, Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return let(arguments[0], Arrays.copyOfRange(arguments, 1, arguments.length));
        }

        public static ELispExpressionNode let(Object varlist, Object[] body) {
            if (isNil(varlist)) {
                return FProgn.progn(body);
            }
            return new LetNode(varlist, body, false);
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
    public abstract static class FWhile extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void whileBailout(Object test, Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new WhileNode(arguments[0], Arrays.copyOfRange(arguments, 1, arguments.length));
        }

        public static final class RepeatingBodyNode extends Node implements RepeatingNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode condition;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode bodyNode;

            final LoopConditionProfile loopConditionProfile;

            public RepeatingBodyNode(Object test, Object[] body) {
                condition = ELispInterpretedNode.create(test);
                bodyNode = FProgn.progn(body);
                loopConditionProfile = LoopConditionProfile.create();
            }

            @Override
            public boolean executeRepeating(VirtualFrame frame) {
                Object cond = condition.executeGeneric(frame);
                if (loopConditionProfile.profile(!isNil(cond))) {
                    bodyNode.executeVoid(frame);
                    return true;
                } else {
                    return false;
                }
            }
        }

        public static class WhileNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            LoopNode loopNode;

            public WhileNode(Object test, Object[] body) {
                RepeatingBodyNode bodyNode = new RepeatingBodyNode(test, body);
                loopNode = Truffle.getRuntime().createLoopNode(bodyNode);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                executeGeneric(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                loopNode.execute(frame);
                return false;
            }
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
        public Object macroexpand(Object form, Object environment) {
            Object original = form;
            while (true) {
                if (!(form instanceof ELispCons cons)) {
                    return form;
                }
                Object function = cons.car();
                if (toSym(function) instanceof ELispSymbol symbol) {
                    while (toSym(function) instanceof ELispSymbol sym && sym != NIL) {
                        symbol = sym;
                        Object envLookup = BuiltInFns.FAssq.assq(sym, environment);
                        if (!isNil(envLookup)) {
                            function = envLookup;
                            break;
                        }
                        function = symbol.getFunction();
                    }
                    function = FAutoloadDoLoad.autoloadDoLoad(function, symbol, MACRO);
                }
                if (!(function instanceof ELispCons macro) || macro.car() != MACRO) {
                    return copySourceLocation(form, original);
                }
                Object newForm = FFuncall.funcall(
                        this,
                        macro.cdr(),
                        isNil(cons.cdr())
                                ? new Object[0]
                                : asCons(cons.cdr()).toArray()
                );
                if (BuiltInData.FEq.eq(newForm, form)) {
                    return copySourceLocation(form, original);
                }
                form = newForm;
            }
        }

        public static Object copySourceLocation(Object form, Object oldForm) {
            if (oldForm instanceof ELispCons original && original.hasLocation()) {
                if (form instanceof ELispCons expanded && !original.hasLocation()) {
                    expanded.fillDebugInfo(original);
                }
            }
            return form;
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
    public abstract static class FCatch extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void catchBailout(Object tag, Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new CatchNode(arguments[0], Arrays.copyOfRange(arguments, 1, arguments.length));
        }

        private static class CatchNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode tagNode;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode bodyNodes;

            public CatchNode(Object tag, Object[] body) {
                tagNode = ELispInterpretedNode.create(tag);
                bodyNodes = FProgn.progn(body);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                // Most usages of throw/catch are to change the return value.
                // So executeVoid probably will not be called.
                executeGeneric(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                Object tag = tagNode.executeGeneric(frame);
                try {
                    return bodyNodes.executeGeneric(frame);
                } catch (ELispSignals.ELispCatchException e) {
                    if (!isNil(tag) && e.getTag() == tag) {
                        return e.getData();
                    }
                    throw e;
                }
            }
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
            throw new ELispSignals.ELispCatchException(tag, value);
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
    public abstract static class FUnwindProtect extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void unwindProtectBailout(Object bodyform, Object[] unwindforms) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new UnwindProtectNode(arguments[0], Arrays.copyOfRange(arguments, 1, arguments.length));
        }

        private static class UnwindProtectNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode body;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode unwind;

            public UnwindProtectNode(Object bodyform, Object[] unwindforms) {
                body = ELispInterpretedNode.create(bodyform);
                unwind = FProgn.progn(unwindforms);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                RuntimeException rethrow = null;
                try {
                    body.executeVoid(frame);
                } catch (AbstractTruffleException e) {
                    rethrow = e;
                    checkSoftExit(getContext(), rethrow);
                }
                runUnwind(frame, rethrow);
            }

            private void runUnwind(VirtualFrame frame, @Nullable RuntimeException rethrow) {
                // TODO: decide whether to keep compatibility
                // Emacs throws unwindEx instead of rethrow...
                // But personally, esp. when debugging, I would prefer having the original exception.
                try {
                    unwind.executeVoid(frame);
                } catch (AbstractTruffleException unwindEx) {
                    if (rethrow == null) {
                        rethrow = unwindEx;
                    } else {
                        TruffleUtils.addSuppressed(rethrow, unwindEx);
                    }
                }
                if (rethrow != null) {
                    throw rethrow;
                }
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                RuntimeException rethrow = null;
                Object result = null;
                try {
                    result = body.executeGeneric(frame);
                } catch (AbstractTruffleException e) {
                    rethrow = e;
                    checkSoftExit(getContext(), rethrow);
                }
                runUnwind(frame, rethrow);
                return Objects.requireNonNull(result);
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
    public abstract static class FConditionCase extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void conditionCaseBailout(Object var, Object bodyform, Object[] handlers) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return getConditionCaseNode(arguments[0], arguments[1], Arrays.copyOfRange(arguments, 2, arguments.length));
        }

        private static ConditionCaseNode getConditionCaseNode(Object var, Object bodyform, Object[] handlers) {
            int length = handlers.length;
            int successIndex = -1;
            Object[] conditionNames = new Object[length];
            @Nullable Object[] bodies = new Object[length];
            for (int i = 0; i < length; i++) {
                Object handler = handlers[i];
                if (handler instanceof ELispCons cons) {
                    Object conditionName = cons.car();
                    conditionNames[i] = conditionName;
                    if (conditionName == CSUCCESS) {
                        successIndex = i;
                    }
                    Object body = cons.cdr();
                    if (body instanceof ELispCons bodyCons) {
                        bodies[i] = bodyCons;
                    } else {
                        bodies[i] = null;
                    }
                } else {
                    throw ELispSignals.error("Invalid condition handler: " + handler);
                }
            }
            int finalSuccessIndex = successIndex;
            return new ConditionCaseNode(bodyform, finalSuccessIndex, bodies, conditionNames, var);
        }

        private static boolean matches(ELispSymbol conditionName, Object tag) {
            Object property = asSym(tag).getProperty(ERROR_CONDITIONS);
            if (property instanceof ELispCons list) {
                return list.contains(conditionName);
            }
            return false;
        }

        public static boolean shouldHandle(Object tag, Object conditionName) {
            boolean shouldHandle = false;
            if (isT(conditionName)) {
                shouldHandle = true;
            } else if (conditionName instanceof ELispCons list) {
                for (Object sym : list) {
                    if (toSym(sym) instanceof ELispSymbol symbol) {
                        if (matches(symbol, tag)) {
                            shouldHandle = true;
                        }
                    }
                }
            } else {
                if (matches(asSym(conditionName), tag)) {
                    shouldHandle = true;
                }
            }
            return shouldHandle;
        }

        private static class ConditionCaseNode extends ELispExpressionNode {
            private static final int ERROR_SLOT_UNINITIALIZED = -1;
            private static final int ERROR_SLOT_DYNAMIC = -2;
            private static final int ERROR_SLOT_NIL = -3;
            private final int finalSuccessIndex;
            private final Object[] conditionNames;
            private final Object var;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode body;

            private final @Nullable Object[] handlerBodies;
            @Children
            @Nullable ELispExpressionNode[] handlers;

            @CompilerDirectives.CompilationFinal
            private int slot = ERROR_SLOT_UNINITIALIZED;
            @CompilerDirectives.CompilationFinal
            @Nullable
            private ELispLexical lexicalBlock = null;

            public ConditionCaseNode(Object bodyform, int finalSuccessIndex, @Nullable Object[] handlerBodies, Object[] conditionNames, Object var) {
                this.finalSuccessIndex = finalSuccessIndex;
                this.handlerBodies = handlerBodies;
                this.conditionNames = conditionNames;
                this.var = var;
                body = ELispInterpretedNode.create(bodyform);

                int length = handlerBodies.length;
                handlers = new ELispExpressionNode[length];
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                // TODO
                super.executeVoid(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                try {
                    Object o = body.executeGeneric(frame);
                    if (finalSuccessIndex != -1) {
                        ELispExpressionNode handler = handlers[finalSuccessIndex];
                        if (handler != null) {
                            return handle(frame, null, o, handler, finalSuccessIndex);
                        }
                    }
                    return o;
                } catch (ELispSignals.ELispSignalException e) {
                    checkSoftExit(getContext(), e);
                    int i;
                    Object tag = e.getTag();
                    for (i = 0; i < conditionNames.length; i++) {
                        Object conditionName = conditionNames[i];
                        boolean shouldHandle = shouldHandle(tag, conditionName);
                        if (shouldHandle) {
                            ELispExpressionNode handler = handlers[i];
                            ELispCons error = ELispCons.cons(tag, e.getData());
                            return handle(frame, e, error, handler, i);
                        }
                    }
                    throw e;
                }
            }

            private Object handle(
                    VirtualFrame frame,
                    ELispSignals.@Nullable ELispSignalException e, Object data,
                    @Nullable ELispExpressionNode handler, int handlerIndex
            ) {
                ELispSymbol symbol = asSym(var);
                if (slot == ERROR_SLOT_UNINITIALIZED) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    for (int i = 0; i < handlerBodies.length; i++) {
                        @Nullable Object body = handlerBodies[i];
                        handlers[i] = body == null ? null : FProgn.progn(asCons(body).toArray());
                    }
                    if (isNil(symbol)) {
                        slot = ERROR_SLOT_NIL;
                    } else {
                        @Nullable ELispLexical block = ELispLexical.newBlock(this, new ELispSymbol[]{symbol});
                        this.lexicalBlock = block;
                        if (block == null) {
                            slot = ERROR_SLOT_DYNAMIC;
                        } else {
                            slot = block.slots()[0];
                            block.descriptor().setSlotKind(slot, FrameSlotKind.Object);
                            ELispLexical.Scope scope = block.newScope(1);
                            for (int i = 0; i < handlers.length; i++) {
                                @Nullable ELispExpressionNode inner = handlers[i];
                                if (inner != null) {
                                    handlers[i] = new ScopeWrapperNode(inner, scope);
                                }
                            }
                        }
                    }
                    for (ELispExpressionNode node : handlers) {
                        if (node != null) {
                            insert(node);
                            notifyInserted(node);
                        }
                    }
                    handler = handlers[handlerIndex];
                }
                if (handler == null) {
                    return false;
                }
                try {
                    if (slot == ERROR_SLOT_NIL) {
                        return handler.executeGeneric(frame);
                    }
                    if (slot == ERROR_SLOT_DYNAMIC) {
                        try (Dynamic _ = Dynamic.pushDynamic(symbol, data)) {
                            return handler.executeGeneric(frame);
                        }
                    }
                    frame = Objects.requireNonNull(lexicalBlock).newFrame(frame);
                    frame.setObject(slot, data);
                    return handler.executeGeneric(frame);
                } catch (ELispSignals.ELispSignalException newException) {
                    if (e != null) {
                        TruffleUtils.addSuppressed(newException, e);
                    }
                    throw newException;
                }
            }
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
        public Object handlerBind1(Object bodyfun, Object[] args) {
            try {
                return FFuncall.funcall(this, bodyfun);
            } catch (ELispSignals.ELispSignalException signal) {
                checkSoftExit(getContext(), signal);
                Object tag = signal.getTag();
                for (int i = 0; i < args.length; i += 2) {
                    if (FConditionCase.shouldHandle(tag, args[i])) {
                        FFuncall.funcall(this, args[i + 1], ELispCons.cons(tag, signal.getData()));
                        // search continues
                    }
                }
                throw signal;
            }
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
        public static RuntimeException signal(ELispSymbol errorSymbol, Object data) {
            throw new ELispSignals.ELispSignalException(errorSymbol, data);
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
            if (!isNil(function.getFunction())) {
                return false;
            }
            function.setFunction(ELispCons.listOf(AUTOLOAD, file, docstring, interactive, type));
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
        public static Object autoloadDoLoad(Object fundef, ELispSymbol funname, Object macroOnly) {
            if (!(fundef instanceof ELispCons def) || def.car() != AUTOLOAD) {
                return fundef;
            }
            Object kind = def.get(4);
            boolean isMacro = isT(kind) || kind == MACRO;
            if (macroOnly == MACRO && !isMacro) {
                return fundef;
            }
            boolean ignoreErrors = !isMacro && !isNil(macroOnly);
            // TODO: load_with_autoload_queue
            loadFile(ELispLanguage.get(null), null, asCons(def.cdr()).car(), !ignoreErrors);

            if (funname == NIL || ignoreErrors) {
                return false;
            } else {
                Object fun = BuiltInData.FIndirectFunction.indirectFunction(funname, false);
                if (BuiltInFns.FEqual.equal(fun, fundef)) {
                    throw ELispSignals.error("Autoload failed");
                }
                return fun;
            }
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
    public abstract static class FEval extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        private static final Source EVAL_SOURCE = Source.newBuilder(
                "elisp",
                "",
                "<eval>"
        ).content(Source.CONTENT_NONE).build();

        @Specialization
        public Object eval(Object form, boolean lexical) {
            return evalForm(form, lexical);
        }

        @CompilerDirectives.TruffleBoundary(transferToInterpreterOnException = false)
        private Object evalForm(Object form, boolean lexical) {
            ELispRootNode root = getEvalRoot(this, form, lexical);
            return root.getCallTarget().call();
        }

        public static ELispRootNode getEvalRoot(
                @Nullable Node node,
                Object form,
                boolean lexical
        ) {
            ELispExpressionNode expr = ELispRootNodes.createRoot(new Object[]{form}, lexical);
            return new ELispRootNode(
                    ELispLanguage.get(node),
                    expr,
                    EVAL_SOURCE.createSection(0, 0)
            );
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            ELispExpressionNode form = arguments[0];
            ELispExpressionNode lexical = arguments.length == 1 ? ELispInterpretedNode.create(false) : arguments[1];
            return BuiltInEvalFactory.FEvalFactory.InlinedEvalNodeGen.create(form, lexical);
        }

        @NodeChild(value = "form", type = ELispExpressionNode.class)
        @NodeChild(value = "lexical", type = ELispExpressionNode.class)
        public abstract static class InlinedEval extends ELispExpressionNode {
            boolean sameForm(Object form, Object oldForm) {
                return form == oldForm;
            }

            @Specialization(guards = "sameForm(form, oldForm)", limit = "3")
            public Object eval(
                    Object form, Object lexical,
                    @Cached("form") Object oldForm,
                    @Cached("getRootCallTarget(oldForm, lexical)") ELispFunctionObject function,
                    @Cached("create(function.callTarget())") DirectCallNode dispatchNode
            ) {
                return dispatchNode.call();
            }

            @CompilerDirectives.TruffleBoundary(transferToInterpreterOnException = false)
            public ELispFunctionObject getRootCallTarget(Object form, Object lexical) {
                ELispExpressionNode expr = ELispRootNodes.createRoot(new Object[]{form}, !isNil(lexical));
                ELispRootNode root = new ELispRootNode(ELispLanguage.get(this), expr, getEvalSourceSection(form));
                return new ELispFunctionObject(root.getCallTarget());
            }

            public SourceSection getEvalSourceSection(Object form) {
                SourceSection evalSection = getParent().getSourceSection();
                SourceSection formSection = null;
                if (form instanceof ELispCons cons) {
                    if (!cons.hasLocation()) {
                        cons.fillDebugInfo(getParent());
                    }
                    if (cons.hasLocation() && evalSection != null) {
                        formSection = cons.getSourceSection(evalSection.getSource());
                    }
                }
                if (formSection == null) {
                    formSection = Objects.requireNonNullElseGet(evalSection, EVAL_SOURCE::createUnavailableSection);
                }
                return formSection;
            }

            @Specialization(replaces = "eval")
            public Object evalPolymorphic(
                    Object form, Object lexical,
                    @Cached(inline = false) FuncallDispatchNode dispatchNode
            ) {
                return dispatchNode.dispatch(this, EVAL, form, lexical);
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
    public abstract static class FApply extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public Object apply(Object function, Object[] arguments, @Cached(inline = true) FuncallDispatchNode dispatchNode) {
            List<Object> objects = new ArrayList<>(arguments.length);
            objects.add(function);
            objects.addAll(Arrays.asList(arguments).subList(0, arguments.length - 1));
            Object last = arguments[arguments.length - 1];
            if (!isNil(last)) {
                objects.addAll(asCons(last));
            }
            arguments = objects.toArray();
            return dispatchNode.executeDispatch(this, arguments);
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 0) {
                throw ELispSignals.wrongNumberOfArguments(APPLY, 0);
            }
            return FuncallDispatchNodeGen.CallNNodeGen.create(new ApplyArgsToArrayNode(arguments));
        }

        public static final class ApplyArgsToArrayNode extends ELispExpressionNode {
            @Children
            final ELispExpressionNode[] nodes;

            public ApplyArgsToArrayNode(ELispExpressionNode[] arguments) {
                this.nodes = arguments;
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                super.executeVoid(frame);
            }

            @ExplodeLoop
            @Override
            public Object[] executeGeneric(VirtualFrame frame) {
                if (nodes.length == 1) {
                    Object o = nodes[0].executeGeneric(frame);
                    return new Object[]{o};
                }
                ArrayList<Object> objects = new ArrayList<>(nodes.length);
                for (int i = 0; i < nodes.length - 1; i++) {
                    objects.add(nodes[i].executeGeneric(frame));
                }
                Object o = nodes[nodes.length - 1].executeGeneric(frame);
                if (!isNil(o)) {
                    objects.addAll(asCons(o));
                }
                return objects.toArray();
            }
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
        public boolean runHooks(Object[] hooks) {
            for (Object hook : hooks) {
                ELispSymbol symbol = asSym(hook);
                if (!symbol.isBound()) {
                    continue;
                }
                Object value = symbol.getValue();
                if (FFunctionp.functionp(value)) {
                    FFuncall.funcall(this, value);
                } else if (value instanceof ELispCons cons) {
                    for (Object element : cons) {
                        FFuncall.funcall(this, element);
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
        public boolean runHookWithArgs(ELispSymbol hook, Object[] args, @Cached(inline = true) FuncallDispatchNode dispatchNode) {
            if (!hook.isBound()) {
                return false;
            }
            Object value = hook.getValue();
            if (!isNil(value)) {
                if (FFunctionp.functionp(value)) {
                    dispatchNode.dispatchArgs(this, value, args);
                } else {
                    // TODO: Handle buffer-local hooks
                    for (Object function : asCons(value)) {
                        dispatchNode.dispatchArgs(this, function, args);
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
        public Object runHookWithArgsUntilSuccess(ELispSymbol hook, Object[] args) {
            if (!hook.isBound()) {
                return false;
            }
            Object value = hook.getValue();
            if (isNil(value)) {
                return false;
            }
            ELispCons hooks;
            if (value instanceof ELispCons cons) {
                hooks = cons;
            } else {
                hooks = ELispCons.listOf(value);
            }
            for (Object callable : hooks) {
                // TODO: Handle buffer-local hooks
                Object result = FFuncall.funcall(this, callable, args);
                if (!isNil(result)) {
                    return result;
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
            if (toSym(object) instanceof ELispSymbol symbol) {
                object = symbol.getIndirectFunction();
                if (object instanceof ELispCons cons && cons.car() == AUTOLOAD) {
                    return true;
                }
            }
            return (object instanceof ELispSubroutine sub && !sub.specialForm())
                    || object instanceof AbstractELispClosure
                    || object instanceof ELispCons cons && cons.car() == LAMBDA;
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
    public abstract static class FFuncall extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public Object funcallThis(Object function, Object[] arguments) {
            return funcall(this, function, arguments);
        }

        public static Object funcall(@Nullable Node node, Object function, Object... arguments) {
            return FuncallDispatchNode.dispatchArgsUncached(node, function, arguments);
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 0) {
                throw ELispSignals.wrongNumberOfArguments(FUNCALL, 0);
            }
            return FuncallDispatchNode.createSpecializedCallNode(arguments);
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
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static ELispCons funcArity(Object function) {
            Object object = ELispInterpretedNode.getIndirectFunction(function);
            if (object instanceof ELispCons cons && cons.car() == AUTOLOAD) {
                FAutoloadDoLoad.autoloadDoLoad(object, NIL, false);
                object = ELispInterpretedNode.getIndirectFunction(function);
            }
            if (object instanceof ELispCons cons && cons.car() == MACRO) {
                object = cons.cdr();
            }
            Object argList;
            switch (object) {
                case ELispSubroutine s -> {
                    ELispBuiltIn info = s.info();
                    if (s.specialForm()) {
                        return ELispCons.cons((long) info.minArgs(), UNEVALLED);
                    }
                    if (info.varArgs()) {
                        return ELispCons.cons((long) info.minArgs(), MANY);
                    }
                    return ELispCons.cons((long) info.minArgs(), (long) info.maxArgs());
                }
                case ELispCons cons when cons.car() == LAMBDA ->
                        argList = cons.get(1); // fallthrough
                case ELispInterpretedClosure closure -> //noinspection SequencedCollectionMethodCanBeUsed
                        argList = closure.get(CLOSURE_ARGLIST); // fallthrough
                case ELispBytecode byteCode -> //noinspection SequencedCollectionMethodCanBeUsed
                        argList = byteCode.get(CLOSURE_ARGLIST);
                default -> throw ELispSignals.invalidFunction(function);
            }
            ELispInterpretedClosure.ClosureArgs args = ELispInterpretedClosure.ClosureArgs.parse(argList);
            return ELispCons.cons(
                    (long) args.requiredArgCount(),
                    args.rest() == null ? (long) args.maxArgCount() : MANY
            );
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
        public boolean specialVariableP(
                ELispSymbol symbol,
                @Cached GlobalIndirectLookupNode lookup
        ) {
            Optional<ValueStorage> storage = lookup.execute(this, symbol);
            return storage.isPresent() && storage.get().isSpecial();
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
        public boolean mapbacktrace(Object function, Object base) {
            FBacktraceFrameInternal.backtraceFrames((args, _) -> {
                FFuncall.funcall(this, function, args);
                return null;
            }, 0, base);
            return false;
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
        public Object backtraceFrameInternal(Object function, long nframes, Object base) {
            return backtraceFrames(
                    (args, _) -> FFuncall.funcall(this, function, args),
                    nframes,
                    base
            );
        }

        @CompilerDirectives.TruffleBoundary
        public static Object backtraceFrames(
                BiFunction<Object[], FrameInstance, @Nullable Object> function,
                long nframes,
                Object base
        ) {
            // TODO: get stack function symbols
            @Nullable
            Object result = Truffle.getRuntime().iterateFrames(new FrameInstanceVisitor<>() {
                int i = isNil(base) ? 0 : -1;

                @Override
                @Nullable
                public Object visitFrame(FrameInstance frameInstance) {
                    CallTarget frameTarget = frameInstance.getCallTarget();
                    if (frameTarget == null) {
                        return null;
                    }
                    Object f = getFunctionObject(frameTarget);
                    if (i == -1 && base == f) {
                        i = 0;
                    }
                    if (i == nframes) {
                        Frame frame = frameInstance.getFrame(FrameInstance.FrameAccess.READ_ONLY);
                        boolean evaluated = true;
                        ELispCons.ListBuilder args = new ELispCons.ListBuilder();
                        for (Object argument : frame.getArguments()) {
                            args.add(argument);
                        }
                        Object flags = false; // TODO: backtrace_debug_on_exit?
                        return function.apply(new Object[]{evaluated, f, args.build(), flags}, frameInstance);
                    }
                    if (i != -1) {
                        i++;
                    }
                    return null;
                }

                private static Object getFunctionObject(CallTarget frameTarget) {
                    if (!(frameTarget instanceof RootCallTarget rootCallTarget)) {
                        throw new UnsupportedOperationException();
                    }
                    RootNode rootNode = rootCallTarget.getRootNode();
                    return switch (rootNode) {
                        case FunctionRootNode functionRootNode -> functionRootNode.getLispFunction();
                        case ELispRootNode _ -> false;
                        default -> throw new UnsupportedOperationException();
                    };
                }
            });
            return result == null ? false : result;
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
        public static Object backtraceLocals(long nframes, Object base) {
            return FBacktraceFrameInternal.backtraceFrames((_, frame) -> {
                Node node = frame.getCallNode();
                Frame f = frame.getFrame(FrameInstance.FrameAccess.READ_ONLY);
                ELispLexical.@Nullable Scope lexical = null;
                if (node != null) {
                    lexical = ELispLexical.getScope(node);
                }
                if (lexical == null) {
                    // TODO: dynamic
                    return false;
                }
                return lexical.getRootScope().toAssocList(f);
            }, nframes, base);
        }
    }
}
