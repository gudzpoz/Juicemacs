package party.iroiro.juicemacs.elisp.forms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.*;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.*;

import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import static party.iroiro.juicemacs.elisp.forms.BuiltInLRead.loadFile;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/**
 * Built-in functions from {@code src/eval.c}
 */
@SuppressWarnings({"DefaultAnnotationParam", "ForLoopReplaceableByForEach"})
public class BuiltInEval extends ELispBuiltIns {

    public static final String ELISP_SPECIAL_FORM = "elisp special form";

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEvalFactory.getFactories();
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
        public static ELispExpressionNode orBailout(Object[] conditions) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new OrNode(conditions);
        }

        private static class OrNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            private ELispExpressionNode[] nodes;

            public OrNode(Object[] conditions) {
                nodes = ELispInterpretedNode.create(conditions);
                adoptChildren();
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
    public abstract static class FAnd extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode andBailout(Object[] conditions) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new AndNode(conditions);
        }

        private static class AndNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            private ELispExpressionNode[] nodes;

            public AndNode(Object[] conditions) {
                nodes = ELispInterpretedNode.create(conditions);
                adoptChildren();
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
    public abstract static class FIf extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode ifBailout(Object cond, Object then, Object[] else_) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new IfNode(cond, then, else_);
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

            public IfNode(Object cond, Object then, Object[] else_) {
                condition = ELispInterpretedNode.create(cond);
                thenBranch = ELispInterpretedNode.create(then);
                elseBranch = FProgn.progn(else_);
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                if (isNil(condition.executeGeneric(frame))) {
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
    public abstract static class FCond extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode condBailout(Object[] clauses) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new CondNode(clauses);
        }

        private static class CondNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            ELispExpressionNode[] conditions;
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            ELispExpressionNode[] thenCases;

            public CondNode(Object[] clauses) {
                List<ELispExpressionNode> conditionNodes = new ArrayList<>();
                List<ELispExpressionNode> cases = new ArrayList<>();
                for (Object clause : clauses) {
                    ELispCons cons = asCons(clause);
                    conditionNodes.add(ELispInterpretedNode.create(cons.car()));
                    List<Object> body = new ArrayList<>();
                    ELispCons.BrentTortoiseHareIterator iterator = cons.listIterator(1);
                    while (iterator.hasNext()) {
                        body.add(iterator.next());
                    }
                    cases.add(FProgn.progn(body.toArray(new Object[0])));
                }
                conditions = conditionNodes.toArray(new ELispExpressionNode[0]);
                thenCases = cases.toArray(new ELispExpressionNode[0]);
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                int length = conditions.length;
                for (int i = 0; i < length; i++) {
                    if (!isNil(conditions[i].executeGeneric(frame))) {
                        thenCases[i].executeVoid(frame);
                        return;
                    }
                }
            }

            @ExplodeLoop
            @Override
            public Object executeGeneric(VirtualFrame frame) {
                int length = conditions.length;
                for (int i = 0; i < length; i++) {
                    if (!isNil(conditions[i].executeGeneric(frame))) {
                        return thenCases[i].executeGeneric(frame);
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
    public abstract static class FProgn extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode prognBailout(Object[] body) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return progn(body);
        }

        public static ELispExpressionNode progn(Object[] body) {
            if (body.length == 0) {
                return ELispInterpretedNode.literal(false);
            }
            return new PrognBlockNode(ELispInterpretedNode.create(body));
        }

        public static final class PrognBlockNode extends ELispExpressionNode
                implements BlockNode.ElementExecutor<ELispExpressionNode> {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            BlockNode<ELispExpressionNode> block;

            public PrognBlockNode(ELispExpressionNode[] body) {
                block = BlockNode.create(body, this);
                adoptChildren();
                block.adoptChildren();
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
    public abstract static class FProg1 extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode prog1Bailout(Object first, Object[] body) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new Prog1Node(first, body);
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
                adoptChildren();
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
    public abstract static class FSetq extends ELispBuiltInBaseNode {
        static final class FSetqItem extends ELispExpressionNode {
            public static final int INVALID = ELispLexical.NON_VAR_SLOT0;
            public static final int DYNAMIC = ELispLexical.NON_VAR_SLOT1;

            final Object symbol;

            @CompilerDirectives.CompilationFinal
            int top = INVALID;

            @Child
            ELispFrameSlotNode.@Nullable ELispFrameSlotWriteNode writeNode;

            private final ELispExpressionNode inner;

            FSetqItem(Object symbol, Object value) {
                this.symbol = symbol;
                this.inner = ELispInterpretedNode.create(value);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                if (updateSlots(frame) != null) {
                    return;
                }
                Objects.requireNonNull(writeNode).executeVoid(frame);
            }

            @Override
            public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
                Object dyn = updateSlots(frame);
                if (dyn != null) {
                    return ELispTypeSystemGen.expectLong(dyn);
                }
                return Objects.requireNonNull(writeNode).executeLong(frame);
            }

            @Override
            public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
                Object dyn = updateSlots(frame);
                if (dyn != null) {
                    return ELispTypeSystemGen.expectDouble(dyn);
                }
                return Objects.requireNonNull(writeNode).executeDouble(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                Object dyn = updateSlots(frame);
                if (dyn != null) {
                    return dyn;
                }
                return Objects.requireNonNull(writeNode).executeGeneric(frame);
            }

            /// Returns the value assigned to a dynamic variable or `null` if lexical
            private @Nullable Object updateSlots(VirtualFrame frame) {
                if (top == DYNAMIC) {
                    return setDynamic(frame);
                }
                ELispFrameSlotNode.ELispFrameSlotWriteNode write = writeNode;
                if (CompilerDirectives.injectBranchProbability(
                        CompilerDirectives.FASTPATH_PROBABILITY,
                        write != null && write.isFrameTopValid(frame, top)
                )) {
                    return null;
                }
                CompilerDirectives.transferToInterpreterAndInvalidate();
                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                ELispLexical.@Nullable LexicalReference lexical =
                        lexicalFrame == null ? null : lexicalFrame.getLexicalReference(frame, asSym(symbol));
                int newIndex;
                if (lexical == null) {
                    newIndex = ELispFrameSlotNode.BYPASS;
                } else {
                    newIndex = lexical.index();
                }
                if (write == null || write.getSlot() != newIndex) {
                    if (lexical == null) {
                        top = DYNAMIC;
                        write = ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(
                                newIndex,
                                null, // PMD: Null
                                inner
                        );
                        writeNode = write;
                        adoptChildren();
                        return setDynamic(frame);
                    } else {
                        top = ELispLexical.getMaterializedTop(frame);
                        write = ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(
                                newIndex, lexical.frame(), inner
                        );
                        writeNode = write;
                        adoptChildren();
                    }
                } else {
                    top = ELispLexical.getMaterializedTop(frame);
                }
                return null;
            }

            private Object setDynamic(VirtualFrame frame) {
                Object o = Objects.requireNonNull(writeNode).executeGeneric(frame);
                asSym(symbol).setValue(o);
                return o;
            }
        }

        @Specialization
        public static ELispExpressionNode setqBailout(Object[] args) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            if (args.length % 2 != 0) {
                return new ELispExpressionNode() {
                    @Override
                    public void executeVoid(VirtualFrame frame) {
                        super.executeVoid(frame);
                    }

                    @Override
                    public Object executeGeneric(VirtualFrame frame) {
                        throw ELispSignals.wrongNumberOfArguments(SETQ, args.length);
                    }
                };
            }
            if (args.length == 0) {
                return ELispInterpretedNode.literal(false);
            }
            int half = args.length / 2;
            ELispExpressionNode[] assignments = new ELispExpressionNode[half];
            for (int i = 0; i < half; i++) {
                assignments[i] = new FSetqItem(args[i * 2], args[i * 2 + 1]);
            }
            return new FProgn.PrognBlockNode(assignments);
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
        public static ELispExpressionNode quoteBailout(Object arg) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return ELispInterpretedNode.literal(arg);
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
        public ELispInterpretedClosure makeInterpretedClosure(Object args, ELispCons body, Object env, Object docstring, Object iform) {
            return new ELispInterpretedClosure(
                    args,
                    body,
                    env,
                    docstring,
                    iform,
                    getRootNode()
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
        public static ELispExpressionNode functionBailout(Object arg) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return function(arg);
        }

        public static ELispExpressionNode function(Object arg) {
            if (arg instanceof ELispCons def && def.car() == LAMBDA) {
                return getDefinition(def);
            }
            return ELispInterpretedNode.literal(arg);
        }

        private static ELispExpressionNode getDefinition(ELispCons def) {
            CompilerDirectives.transferToInterpreter();
            ELispCons.BrentTortoiseHareIterator iterator = def.listIterator(1);
            Object args = iterator.next();
            ELispInterpretedNode docString = null;
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
            ELispCons body = iterator.hasNext() ? iterator.currentCons() : new ELispCons(false);
            ELispInterpretedNode finalDocString = docString;
            Object finalInteractive = interactive;
            return new ELispExpressionNode() {
                @SuppressWarnings("FieldMayBeFinal")
                @Child
                ELispExpressionNode doc = finalDocString;

                {
                    adoptChildren();
                }

                @Override
                public void executeVoid(VirtualFrame frame) {
                    executeGeneric(frame);
                }

                @Override
                public Object executeGeneric(VirtualFrame frame) {
                    @Nullable ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                    return new ELispInterpretedClosure(
                            args,
                            body,
                            lexicalFrame == null ? false : lexicalFrame.getEnv(frame),
                            doc.executeGeneric(frame),
                            finalInteractive,
                            getRootNode()
                    );
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
        public static Object defaultToplevelValue(ELispSymbol symbol) {
            return symbol.getDefaultValue();
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
    public abstract static class FDefvar extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode defvarBailout(VirtualFrame frame, Object symbol, Object initvalue, Object docstring) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            boolean initValueMissing = frame.getArguments().length < 2;
            return new DefVarNode(symbol, initvalue, initValueMissing);
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
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                executeGeneric(frame);
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ELispSymbol sym = asSym(symbol);
                if (initValueMissing) {
                    ELispLexical.markDynamic(frame, sym);
                } else {
                    sym.setSpecial(true);
                    sym.setDefaultValue(init.executeGeneric(frame));
                }
                return symbol;
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
        public static ELispExpressionNode defconstBailout(Object symbol, Object initvalue, Object docstring) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new DefConstNode(symbol, initvalue);
        }

        private static class DefConstNode extends ELispExpressionNode {
            private final Object symbol;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode init;

            public DefConstNode(Object symbol, Object initvalue) {
                this.symbol = symbol;
                init = ELispInterpretedNode.create(initvalue);
                adoptChildren();
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
        public static ELispExpressionNode letxBailout(Object varlist, Object[] body) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new LetxNode(varlist, body);
        }

        private static class LetxNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            FLet.MakeScopeNode scopeNode;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode bodyNode;

            public LetxNode(Object varlist, Object[] body) {
                scopeNode = FLet.makeScope(varlist, true);
                bodyNode = new FProgn.PrognBlockNode(ELispInterpretedNode.create(body));
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                try (ELispLexical.Dynamic _ = scopeNode.executeGeneric(frame)) {
                    bodyNode.executeVoid(frame);
                } finally {
                    if (lexicalFrame != null) {
                        lexicalFrame.restore(frame);
                    }
                }
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                try (ELispLexical.Dynamic _ = scopeNode.executeGeneric(frame)) {
                    return bodyNode.executeGeneric(frame);
                } finally {
                    if (lexicalFrame != null) {
                        lexicalFrame.restore(frame);
                    }
                }
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
        static class MakeScopeNode extends ELispExpressionNode {
            private final boolean progressive;
            private final Object[] symbols;
            @SuppressWarnings("FieldMayBeFinal")
            @Children
            ELispInterpretedNode[] valueNodes;

            public MakeScopeNode(List<ELispInterpretedNode> values, boolean progressive, Object[] symbols) {
                this.progressive = progressive;
                this.symbols = symbols;
                valueNodes = values.toArray(ELispInterpretedNode[]::new);
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                throw CompilerDirectives.shouldNotReachHere();
            }

            @ExplodeLoop
            @Override
            public ELispLexical.@Nullable Dynamic executeGeneric(VirtualFrame frame) {
                ArrayList<ELispSymbol> specialBindings = new ArrayList<>();
                ArrayList<Object> specialValues = new ArrayList<>();

                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                boolean dynamicBinding = lexicalFrame == null;
                Object[] lexicalValues = (dynamicBinding && !progressive) ? new Object[0] : new Object[symbols.length];
                int length = symbols.length;
                try {
                    for (int i = 0; i < length; i++) {
                        ELispSymbol symbol = asSym(symbols[i]);
                        Object value = valueNodes[i].executeGeneric(frame);
                        if (dynamicBinding || ELispLexical.isDynamic(frame, symbol)) {
                            specialBindings.add(symbol);
                            specialValues.add(progressive ? symbol.swapThreadLocalValue(value) : value);
                        } else {
                            // TODO: Check if Truffle bail out on this.
                            if (progressive) {
                                lexicalFrame = lexicalFrame.fork(frame);
                                lexicalFrame.addVariable(frame, symbol, value);
                            } else {
                                lexicalValues[i] = value;
                            }
                        }
                    }
                } catch (Throwable e) {
                    if (progressive) {
                        // Restore dynamic scopes.
                        ELispLexical.pushDynamic(
                                specialBindings.toArray(new ELispSymbol[0]),
                                specialValues.toArray(new Object[0])
                        ).close();
                        // The caller is responsible for restoring lexical scopes.
                    }
                    throw e;
                }
                ELispLexical.Dynamic handle = null;
                if (!specialBindings.isEmpty()) {
                    ELispSymbol[] specialSymbols = specialBindings.toArray(new ELispSymbol[0]);
                    Object[] values = specialValues.toArray(new Object[0]);
                    handle = progressive ? new ELispLexical.Dynamic(
                            specialSymbols,
                            values
                    ) : ELispLexical.pushDynamic(
                            specialSymbols,
                            values
                    );
                }
                if (!progressive) {
                    if (!dynamicBinding) {
                        lexicalFrame = lexicalFrame.fork(frame);
                        for (int i = 0; i < length; i++) {
                            Object value = lexicalValues[i];
                            if (value != null) {
                                lexicalFrame.addVariable(frame, asSym(symbols[i]), value);
                            }
                        }
                    }
                }
                return handle;
            }
        }
        static MakeScopeNode makeScope(Object varlist, boolean progressive) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            List<Object> symbolList = new ArrayList<>();
            List<ELispInterpretedNode> values = new ArrayList<>();
            for (Object assignment : ELispCons.iterate(varlist)) {
                Object symbol;
                Object value;
                if (assignment instanceof ELispSymbol sym) {
                    symbol = sym;
                    value = false;
                } else {
                    ELispCons cons = asCons(assignment);
                    symbol = cons.car();
                    ELispCons cdr = asCons(cons.cdr());
                    if (!isNil(cdr.cdr())) {
                        throw ELispSignals.error("`let' bindings can have only one value-form");
                    }
                    value = cdr.car();
                }
                symbolList.add(symbol);
                values.add(ELispInterpretedNode.create(value));
            }
            Object[] symbols = symbolList.toArray();
            return new MakeScopeNode(values, progressive, symbols);
        }

        @Specialization
        public static ELispExpressionNode letBailout(Object varlist, Object[] body) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return let(varlist, body);
        }

        public static ELispExpressionNode let(Object varlist, Object[] body) {
            return new LetNode(varlist, body);
        }

        private static class LetNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            MakeScopeNode scopeNode;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode bodyNode;

            public LetNode(Object varlist, Object[] body) {
                scopeNode = FLet.makeScope(varlist, false);
                bodyNode = FProgn.progn(body);
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                try (ELispLexical.Dynamic _ = scopeNode.executeGeneric(frame)) {
                    bodyNode.executeVoid(frame);
                } finally {
                    if (lexicalFrame != null) {
                        lexicalFrame.restore(frame);
                    }
                }
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                try (ELispLexical.Dynamic _ = scopeNode.executeGeneric(frame)) {
                    return bodyNode.executeGeneric(frame);
                } finally {
                    if (lexicalFrame != null) {
                        lexicalFrame.restore(frame);
                    }
                }
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
        public static ELispExpressionNode whileBailout(Object test, Object[] body) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new WhileNode(test, body);
        }

        private final static class RepeatingBodyNode extends Node implements RepeatingNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode condition;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode bodyNode;

            public RepeatingBodyNode(Object test, Object[] body) {
                condition = ELispInterpretedNode.create(test);
                bodyNode = FProgn.progn(body);
                adoptChildren();
            }

            @Override
            public boolean executeRepeating(VirtualFrame frame) {
                if (isNil(condition.executeGeneric(frame))) {
                    return false;
                } else {
                    bodyNode.executeVoid(frame);
                    return true;
                }
            }
        }

        private static class WhileNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            LoopNode loopNode;

            public WhileNode(Object test, Object[] body) {
                loopNode = Truffle.getRuntime().createLoopNode(new RepeatingBodyNode(test, body));
                adoptChildren();
                loopNode.adoptChildren();
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
        public static ELispExpressionNode catchBailout(Object tag, Object[] body) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new CatchNode(tag, body);
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
                adoptChildren();
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
    public abstract static class FUnwindProtect extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispExpressionNode unwindProtectBailout(Object bodyform, Object[] unwindforms) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return new UnwindProtectNode(bodyform, unwindforms);
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
                adoptChildren();
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                try {
                    body.executeVoid(frame);
                } catch (ELispSignals.ELispCatchException | ELispSignals.ELispSignalException e) {
                    unwind.executeVoid(frame);
                    throw e;
                }
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                // TODO: Add test once we have signal support
                try {
                    return body.executeGeneric(frame);
                } catch (ELispSignals.ELispCatchException | ELispSignals.ELispSignalException e) {
                    unwind.executeVoid(frame);
                    throw e;
                }
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
        public static ELispExpressionNode conditionCaseBailout(Object var, Object bodyform, Object[] handlers) {
            CompilerDirectives.bailout(ELISP_SPECIAL_FORM);
            return getConditionCaseNode(var, bodyform, handlers);
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
                    if (conditionName == ELispContext.CSUCCESS) {
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
            return new ConditionCaseNode(bodyform, finalSuccessIndex, bodies, length, conditionNames, var);
        }

        private static class ConditionCaseNode extends ELispExpressionNode {
            private final int finalSuccessIndex;
            private final @Nullable Object[] bodies;
            private final int length;
            private final Object[] conditionNames;
            private final Object var;
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            ELispExpressionNode body;

            public ConditionCaseNode(Object bodyform, int finalSuccessIndex, @Nullable Object[] bodies, int length, Object[] conditionNames, Object var) {
                this.finalSuccessIndex = finalSuccessIndex;
                this.bodies = bodies;
                this.length = length;
                this.conditionNames = conditionNames;
                this.var = var;
                body = ELispInterpretedNode.create(bodyform);
                adoptChildren();
            }

            private static boolean matches(ELispSymbol conditionName, Object tag) {
                Object property = asSym(tag).getProperty(ELispContext.ERROR_CONDITIONS);
                if (property instanceof ELispCons list) {
                    return list.contains(conditionName);
                }
                return false;
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
                        Object handler = bodies[finalSuccessIndex];
                        if (handler != null) {
                            return handle(frame, o, handler);
                        }
                    }
                    return o;
                } catch (ELispSignals.ELispSignalException e) {
                    int i;
                    for (i = 0; i < length; i++) {
                        Object conditionName = conditionNames[i];
                        boolean shouldHandle = false;
                        if (isT(conditionName)) {
                            shouldHandle = true;
                        } else if (conditionName instanceof ELispCons list) {
                            for (Object sym : list) {
                                if (sym instanceof ELispSymbol symbol) {
                                    if (matches(symbol, e.getTag())) {
                                        shouldHandle = true;
                                    }
                                }
                            }
                        } else {
                            if (matches(asSym(conditionName), e.getTag())) {
                                shouldHandle = true;
                            }
                        }
                        if (shouldHandle) {
                            Object handler = bodies[i];
                            if (handler == null) {
                                return false;
                            }
                            ELispCons error = new ELispCons(e.getTag(), e.getData());
                            return handle(frame, error, handler);
                        }
                    }
                    throw e;
                }
            }

            private Object handle(VirtualFrame frame, Object data, Object handler) {
                CompilerDirectives.transferToInterpreter();
                return FLet.let(
                        new ELispCons(ELispCons.listOf(var, ELispCons.listOf(ELispContext.QUOTE, data))),
                        asCons(handler).toArray()
                ).executeGeneric(frame);
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
        public static Void signal(ELispSymbol errorSymbol, Object data) {
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
            // TODO: Add support in evalSub
            if (!isNil(function.getFunction())) {
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
        public Object autoloadDoLoad(Object fundef, ELispSymbol funname, Object macroOnly) {
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
            loadFile(ELispLanguage.get(this), asCons(def.cdr()).car(), !ignoreErrors);

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
    public abstract static class FEval extends ELispBuiltInBaseNode {
        @Specialization
        public Object eval(Object form, boolean lexical) {
            try {
                return CompilerDirectives.interpreterOnly(() -> evalForm(form, lexical));
            } catch (Exception e) {
                throw e instanceof RuntimeException re ? re : CompilerDirectives.shouldNotReachHere();
            }
        }

        private Object evalForm(Object form, boolean lexical) {
            ELispExpressionNode expr = ELispInterpretedNode.create(new Object[]{form}, lexical);
            Source source = this.getRootNode().getSourceSection().getSource();
            SourceSection sourceSection =
                    form instanceof ELispCons cons ? cons.getSourceSection(source) : source.createUnavailableSection();
            ELispRootNode root = new ELispRootNode(ELispLanguage.get(this), expr, sourceSection);
            return root.getCallTarget().call();
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
            if (!isNil(last)) {
                objects.addAll(asCons(last));
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
                Object value = asSym(hook).getValue();
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
        public static boolean runHookWithArgs(ELispSymbol hook, Object[] args) {
            Object value = hook.getValue();
            if (ELispSymbol.isNil(value)) {
                return false;
            }
            if (FFunctionp.functionp(value)) {
                FFuncall.funcall(value, args);
                return false;
            }
            for (Object function : asCons(value)) {
                FFuncall.funcall(function, args);
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
            if (object instanceof ELispSymbol symbol) {
                object = symbol.getIndirectFunction();
            }
            return (object instanceof ELispSubroutine(_, boolean special, _) && !special)
                    || object instanceof ELispInterpretedClosure;
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
            if (function instanceof ELispSubroutine subroutine && !subroutine.specialForm()) {
                return subroutine.body().callTarget().call(arguments);
            }
            if (function instanceof ELispInterpretedClosure closure) {
                return closure.getFunction().callTarget().call(arguments);
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
        public static Object backtraceFrameInternal(Object function, long nframes, Object base) {
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
                        ELispCons argInfo = ELispCons.listOf((long) frame.getArguments().length, args);
                        Object flags = false; // TODO: backtrace_debug_on_exit?
                        return FFuncall.funcall(function, new Object[]{evaluated, f, argInfo, flags});
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
        public static Void backtraceLocals(Object nframes, Object base) {
            throw new UnsupportedOperationException();
        }
    }
}
