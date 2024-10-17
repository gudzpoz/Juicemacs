package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

/**
 * Special nodes for interpreted ELisp
 *
 * <p>
 * Interpreted ELisp is too dynamic to compile to a constant AST. This class (and its subclasses)
 * are used to represent interpreted ELisp expressions, somehow dynamically.
 * </p>
 *
 * <h2>Dynamically Constant AST</h2>
 * <p>
 * Basically, what it does is to try to "predict" the AST of the expression at compile time, and
 * dynamically "replaces" the nodes that we have had a wrong prediction.
 * </p>
 */
public abstract class ELispInterpretedNode extends ELispExpressionNode {

    public static ELispInterpretedNode create(Object expression) {
        return switch (expression) {
            case ELispSymbol symbol -> new ELispSymbolDereferenceNode(symbol);
            case ELispCons cons -> new ELispConsExpressionNode(cons);
            default -> literal(expression);
        };
    }

    public static ELispInterpretedNode literal(Object expression) {
        return switch (expression) {
            case Long l -> new ELispLongLiteralNode(l);
            case Double d -> new ELispDoubleLiteralNode(d);
            default -> new ELispObjectLiteralNode(expression);
        };
    }

    public static ELispInterpretedNode[] create(Object[] expressions) {
        ELispInterpretedNode[] nodes = new ELispInterpretedNode[expressions.length];
        for (int i = 0; i < expressions.length; i++) {
            nodes[i] = ELispInterpretedNode.create(expressions[i]);
        }
        return nodes;
    }

    public static ELispInterpretedNode create(Object[] expressions, boolean lexical) {
        return new ELispRootExpressions(expressions, lexical);
    }

    private static Object getIndirectFunction(Object function) {
        if (function instanceof ELispSymbol symbol) {
            function = symbol.getIndirectFunction();
        } else if (function instanceof ELispCons cons && cons.car() == LAMBDA) {
            function = BuiltInEval.FFunction.function(function);
        }
        return function;
    }

    private final static class ELispRootExpressions extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode node;

        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private boolean lexical;

        public ELispRootExpressions(Object[] expressions, boolean lexical) {
            this.node = BuiltInEval.FProgn.progn(expressions);
            this.lexical = lexical;
            adoptChildren();
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            if (lexical) {
                new ELispLexical(frame, null, null, List.of());
            }
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(lexical)) {
                node.executeVoid(frame);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            if (lexical) {
                new ELispLexical(frame, null, null, List.of());
            }
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(lexical)) {
                return node.executeGeneric(frame);
            }
        }
    }

    /// A node that represents a literal
    ///
    /// This node stores the literal as is, without copying. This means
    /// any modification to the literal will be reflected in the node,
    /// and that the evaluation of it might not be idempotent.
    ///
    /// This is the expected behavior for literals in Emacs:
    /// ```elisp
    /// (setq ast '(let* ((x '(a b))
    ///                   (a (car x)))
    ///              (setcar x 'b)
    ///              a))
    /// (eval ast) ; => 'a
    /// (eval ast) ; => 'b
    /// ```
    ///
    /// If one wish to have an idempotent evaluation, one should use
    /// always re-read the source string, and remember to suffix the source
    /// with a random comment to avoid Truffle AST caching.
    ///
    private final static class ELispObjectLiteralNode extends ELispInterpretedNode {
        private final Object literal;

        public ELispObjectLiteralNode(Object literal) {
            this.literal = literal;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return literal;
        }
    }

    private final static class ELispLongLiteralNode extends ELispInterpretedNode {
        private final long literal;

        public ELispLongLiteralNode(long literal) {
            this.literal = literal;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
        }

        @Override
        public long executeLong(VirtualFrame frame) {
            return literal;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return literal;
        }
    }

    private final static class ELispDoubleLiteralNode extends ELispInterpretedNode {
        private final double literal;

        public ELispDoubleLiteralNode(double literal) {
            this.literal = literal;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
        }

        @Override
        public double executeDouble(VirtualFrame frame) {
            return literal;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return literal;
        }
    }

    private final static class ELispSymbolDereferenceNode extends ELispInterpretedNode {
        public static final int INVALID = ELispLexical.NON_VAR_SLOT0;
        public static final int DYNAMIC = ELispLexical.NON_VAR_SLOT1;
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private ELispSymbol symbol;

        @CompilerDirectives.CompilationFinal
        private int top = INVALID;

        @Child
        private ELispFrameSlotNode.@Nullable ELispFrameSlotReadNode readNode;

        public ELispSymbolDereferenceNode(ELispSymbol symbol) {
            this.symbol = symbol;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            if (symbol == NIL || symbol == T) {
                throw new UnexpectedResultException(symbol);
            }
            ELispFrameSlotNode.ELispFrameSlotReadNode read = updateSlotInfo(frame);
            if (read == null) {
                return ELispTypeSystemGen.expectLong(symbol.getValue());
            } else {
                return read.executeLong(frame);
            }
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            if (symbol == NIL || symbol == T) {
                throw new UnexpectedResultException(symbol);
            }
            ELispFrameSlotNode.ELispFrameSlotReadNode read = updateSlotInfo(frame);
            if (read == null) {
                return ELispTypeSystemGen.expectDouble(symbol.getValue());
            } else {
                return read.executeDouble(frame);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            if (symbol == NIL) {
                return false;
            }
            if (symbol == T) {
                return true;
            }
            ELispFrameSlotNode.ELispFrameSlotReadNode read = updateSlotInfo(frame);
            if (read == null) {
                return symbol.getValue();
            } else {
                return read.executeGeneric(frame);
            }
        }

        private ELispFrameSlotNode.@Nullable ELispFrameSlotReadNode updateSlotInfo(VirtualFrame currentFrame) {
            if (top == DYNAMIC) {
                return null;
            }
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.FASTPATH_PROBABILITY,
                    readNode != null && readNode.isFrameTopValid(currentFrame, top)
            )) {
                return readNode;
            }
            CompilerDirectives.transferToInterpreterAndInvalidate();
            ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(currentFrame);
            ELispLexical.LexicalReference lexical = lexicalFrame == null
                    ? null : lexicalFrame.getLexicalReference(currentFrame, symbol);
            if (lexical == null) {
                top = DYNAMIC;
                return null;
            } else {
                ELispFrameSlotNode.ELispFrameSlotReadNode reader =
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(lexical.index(), lexical.frame());
                readNode = reader;
                adoptChildren();
                readNode.adoptChildren();
                top = reader.getValidFrameTop(currentFrame);
            }
            return readNode;
        }
    }

    private abstract static class ConsCallNode extends ELispExpressionNode {
        protected final Object function;

        @Children
        protected ELispExpressionNode @Nullable[] args;

        protected ConsCallNode(Object function, @Nullable ELispCons cons, boolean special) {
            this.function = function;
            this.args = cons == null ? null : initChildren(cons, special);
            adoptChildren();
        }

        @ExplodeLoop
        protected Object[] evalArgs(VirtualFrame frame) {
            int length = Objects.requireNonNull(this.args).length;
            Object[] args = new Object[length];
            for (int i = 0; i < length; i++) {
                args[i] = this.args[i].executeGeneric(frame);
            }
            return args;
        }

        protected static ELispExpressionNode[] initChildren(ELispCons cons, boolean special) {
            List<ELispExpressionNode> childrenList = new ArrayList<>();
            ELispCons.BrentTortoiseHareIterator argIterator = cons.listIterator(1);
            while (argIterator.hasNext()) {
                if (special) {
                    childrenList.add(literal(argIterator.next()));
                } else {
                    childrenList.add(ELispInterpretedNode.create(argIterator.next()));
                }
            }
            return childrenList.toArray(new ELispExpressionNode[0]);
        }

        public Object getFunction() {
            return function;
        }

        protected ELispFunctionObject getFunctionObject(Object function) {
            return switch (function) {
                case ELispSubroutine(ELispFunctionObject body, _, _) -> body;
                case ELispInterpretedClosure closure -> closure.getFunction();
                default -> throw new UnsupportedOperationException();
            };
        }
    }

    abstract static class ConsFunctionCallNode extends ConsCallNode {
        @Child
        @Nullable
        private ELispExpressionNode inlineLambdaNode = null;

        ConsFunctionCallNode(Object function, ELispCons cons) {
            super(function, cons, false);
            if (function instanceof ELispExpressionNode node) {
                inlineLambdaNode = node;
                inlineLambdaNode.adoptChildren();
            }
            adoptChildren();
        }

        @Specialization
        public Object call(VirtualFrame frame, @Cached FunctionDispatchNode dispatchNode) {
            Object function = this.function;
            if (inlineLambdaNode != null) {
                function = inlineLambdaNode.executeGeneric(frame);
            }
            return dispatchNode.executeDispatch(this, getFunctionObject(function), evalArgs(frame));
        }
    }

    private static final class ConsInlinedAstNode extends ConsCallNode {
        @Child
        ELispExpressionNode inlinedNode;

        private ConsInlinedAstNode(Object function, ELispCons cons) {
            super(function, cons, false);
            inlinedNode = generateInlineNode(cons, Objects.requireNonNull(((ELispSubroutine) function).inline()));
            inlinedNode.adoptChildren();
        }

        private static ELispExpressionNode generateInlineNode(ELispCons cons, ELispSubroutine.InlineInfo inline) {
            ELispBuiltIn info = inline.info();
            int args = cons.size() - 1;
            if (args < info.minArgs() || (!info.varArgs() && info.maxArgs() < args)) {
                throw ELispSignals.wrongNumberOfArguments(cons.car(), args);
            }
            if (inline.isTailored()) {
                return inline.createNode(initChildren(cons, false));
            }
            List<ELispExpressionNode> nodes = new ArrayList<>();
            List<ELispExpressionNode> restNodes = new ArrayList<>();
            ELispCons.BrentTortoiseHareIterator argIterator = cons.listIterator(1);
            while (argIterator.hasNext()) {
                if (nodes.size() < info.maxArgs()) {
                    nodes.add(ELispInterpretedNode.create(argIterator.next()));
                } else {
                    restNodes.add(ELispInterpretedNode.create(argIterator.next()));
                }
            }
            while (nodes.size() < info.maxArgs()) {
                nodes.add(literal(false));
            }
            if (info.varArgs()) {
                nodes.add(new ELispExpressionNode() {
                    @SuppressWarnings("FieldMayBeFinal")
                    @Children
                    private ELispExpressionNode[] restArgs = restNodes.toArray(ELispExpressionNode[]::new);

                    {
                        adoptChildren();
                    }

                    @Override
                    public void executeVoid(VirtualFrame frame) {
                        super.executeVoid(frame);
                    }

                    @ExplodeLoop
                    @Override
                    public Object executeGeneric(VirtualFrame frame) {
                        Object[] args = new Object[restArgs.length];
                        for (int i = 0; i < restArgs.length; i++) {
                            args[i] = restArgs[i].executeGeneric(frame);
                        }
                        return args;
                    }
                });
            }
            ELispExpressionNode[] arguments = nodes.toArray(ELispExpressionNode[]::new);
            return inline.createNode(arguments);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            try {
                Objects.requireNonNull(inlinedNode).executeVoid(frame);
            } catch (RuntimeException e) {
                throw remapException(e);
            }
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return Objects.requireNonNull(inlinedNode).executeLong(frame);
            } catch (RuntimeException e) {
                throw remapException(e);
            }
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return Objects.requireNonNull(inlinedNode).executeDouble(frame);
            } catch (RuntimeException e) {
                throw remapException(e);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try {
                return Objects.requireNonNull(inlinedNode).executeGeneric(frame);
            } catch (RuntimeException e) {
                throw remapException(e);
            }
        }
    }

    private final static class ConsSpecialCallNode extends ConsCallNode {
        @Nullable
        @Child
        private ELispExpressionNode generated = null;

        ConsSpecialCallNode(Object function, ELispCons cons) {
            super(function, cons, true);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            ELispExpressionNode form = updateNode(frame);
            form.executeVoid(frame);
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            ELispExpressionNode form = updateNode(frame);
            return form.executeLong(frame);
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            ELispExpressionNode form = updateNode(frame);
            return form.executeDouble(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            ELispExpressionNode form = updateNode(frame);
            return form.executeGeneric(frame);
        }

        private ELispExpressionNode updateNode(VirtualFrame frame) {
            ELispExpressionNode form = generated;
            if (form == null) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                form = (ELispExpressionNode) getFunctionObject(function).callTarget().call(this, evalArgs(frame));
                generated = form;
                adoptChildren();
            }
            return form;
        }
    }

    abstract static class ConsMacroCallNode extends ConsCallNode {
        @Nullable
        private Object generated = null;

        @Child
        @Nullable
        private ELispExpressionNode inlineLambdaNode = null;

        @Child
        @Nullable
        private ELispExpressionNode generatedNode = null;

        ConsMacroCallNode(Object function, ELispCons cons) {
            super(getIndirectFunction(((ELispCons) function).cdr()), cons, true);
            if (this.function instanceof ELispExpressionNode node) {
                inlineLambdaNode = node;
            }
            adoptChildren();
        }

        @Specialization
        public Object call(VirtualFrame frame, @Cached FunctionDispatchNode dispatchNode) {
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(true)) {
                Object function = this.function;
                if (inlineLambdaNode != null) {
                    function = inlineLambdaNode.executeGeneric(frame);
                }
                Object o = dispatchNode.executeDispatch(this, getFunctionObject(function), evalArgs(frame));
                ELispExpressionNode macro = generatedNode;
                Object generated = this.generated;
                if (macro == null || generated == null || !BuiltInFns.FEqual.equal(generated, o)) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    this.generated = o;
                    macro = ELispInterpretedNode.create(o);
                    this.generatedNode = macro;
                    adoptChildren();
                }
                return macro.executeGeneric(frame);
            }
        }
    }

    public final static class ELispConsExpressionNode extends ELispInterpretedNode {
        public final static int FORM_FUNCTION = 0;
        public final static int FORM_SPECIAL = 1;
        public final static int FORM_MACRO = 2;
        public final static int FORM_INLINED = 3;

        @CompilerDirectives.CompilationFinal
        private final ELispCons cons;

        @CompilerDirectives.CompilationFinal
        private volatile int type = -1;

        private final Assumption stable;

        @Child
        @Nullable
        private volatile ConsCallNode callNode;

        public ELispConsExpressionNode(ELispCons cons) {
            this.cons = cons;
            this.callNode = null;
            this.stable = getAssumption(cons);
        }

        private static Assumption getAssumption(ELispCons cons) {
            if (
                    cons.car() instanceof ELispSymbol symbol
                    && symbol.getFunction() instanceof ELispSubroutine(_, _, ELispSubroutine.InlineInfo inline)
                    && inline != null
            ) {
                return inline.stable();
            }
            return Assumption.NEVER_VALID;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            ConsCallNode node = updateInnerNode();
            node.executeVoid(frame);
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
           ConsCallNode node = updateInnerNode();
           return node.executeLong(frame);
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
          ConsCallNode node = updateInnerNode();
          return node.executeDouble(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            ConsCallNode node = updateInnerNode();
            return node.executeGeneric(frame);
        }

        private ConsCallNode updateInnerNode() {
            ConsCallNode node = callNode;
            if (stable.isValid() && node != null) {
                return node;
            }
            Object function = getIndirectFunction(cons.car());
            if (ELispSymbol.isNil(function)) {
                throw ELispSignals.voidFunction(cons.car());
            }
            int newType = switch (function) {
                case ELispSubroutine(_, boolean specialForm, _) when specialForm -> FORM_SPECIAL;
                case ELispSubroutine(_, _, ELispSubroutine.InlineInfo factory) when factory != null -> FORM_INLINED;
                case ELispCons c when c.car() == MACRO -> FORM_MACRO;
                default -> FORM_FUNCTION;
            };
            if (node == null || type != newType || node.getFunction() != function) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                node = switch (newType) {
                    case FORM_SPECIAL -> new ConsSpecialCallNode(function, cons);
                    case FORM_INLINED -> new ConsInlinedAstNode(function, cons);
                    case FORM_FUNCTION -> ELispInterpretedNodeFactory.ConsFunctionCallNodeGen.create(function, cons);
                    case FORM_MACRO -> ELispInterpretedNodeFactory.ConsMacroCallNodeGen.create(function, cons);
                    default -> throw CompilerDirectives.shouldNotReachHere();
                };
                type = newType;
                callNode = node;
                adoptChildren();
            }
            return node;
        }

        @Override
        public SourceSection getSourceSection() {
            RootNode rootNode = getRootNode();
            if (rootNode == null) {
                return null;
            }
            SourceSection section = rootNode.getSourceSection();
            if (section == null) {
                return null;
            }
            return cons.getSourceSection(section.getSource());
        }
    }

}
