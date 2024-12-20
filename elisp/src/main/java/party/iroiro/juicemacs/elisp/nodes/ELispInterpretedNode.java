package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.*;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// Special nodes for interpreted ELisp
///
/// Interpreted ELisp is too dynamic to compile to a constant AST. This class (and its subclasses)
/// are used to represent interpreted ELisp expressions, somehow dynamically.
///
/// ## Dynamically Constant AST
///
/// Basically, what it does is to try to "predict" the AST of the expression at compile time, and
/// dynamically "replaces" the nodes that we have had a wrong prediction.
///
/// @see ELispConsExpressionNode
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
        private final Long literal;

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
        private final Double literal;

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
            ELispFrameSlotNode.ELispFrameSlotReadNode readNode = this.readNode;
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.FASTPATH_PROBABILITY,
                    readNode != null && readNode.isFrameTopValid(currentFrame, top)
            )) {
                return readNode;
            }
            ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(currentFrame);
            ELispLexical.LexicalReference lexical = lexicalFrame == null
                    ? null : lexicalFrame.getLexicalReference(currentFrame, symbol);
            CompilerDirectives.transferToInterpreterAndInvalidate();
            if (lexical == null) {
                top = DYNAMIC;
                return null;
            } else {
                ELispFrameSlotNode.ELispFrameSlotReadNode reader =
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(lexical.index(), lexical.frame());
                this.readNode = insertOrReplace(reader, readNode);
                top = reader.getValidFrameTop(currentFrame);
                return reader;
            }
        }
    }

    private abstract static class ConsCallNode extends ELispExpressionNode {
        protected final Object function;

        @Children
        protected ELispExpressionNode @Nullable[] args;

        protected ConsCallNode(Object function, @Nullable ELispCons cons, boolean special) {
            this.function = function;
            this.args = cons == null ? null : initChildren(cons, special);
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
            if (function instanceof ELispCons lambda && lambda.car() == LAMBDA) {
                inlineLambdaNode = BuiltInEval.FFunction.function(lambda);
            }
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
            ELispSubroutine subroutine = (ELispSubroutine) function;
            inlinedNode = generateInlineNode(cons, Objects.requireNonNull(subroutine.inline()));
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
                ELispInterpretedNode node = ELispInterpretedNode.create(argIterator.next());
                if (nodes.size() < info.maxArgs()) {
                    nodes.add(node);
                } else {
                    restNodes.add(node);
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
                inlinedNode.executeVoid(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return inlinedNode.executeLong(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return inlinedNode.executeDouble(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try {
                return inlinedNode.executeGeneric(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @CompilerDirectives.TruffleBoundary
        public RuntimeException rewriteException(RuntimeException e) {
            return ELispSignals.remapException(e, getParent() == null ? this : getParent());
        }
    }

    private static final class ConsSpecialCallNode extends ConsCallNode {
        @Child
        private ELispExpressionNode generated = null;

        ConsSpecialCallNode(Object function, ELispCons cons) {
            super(function, cons, true);
            generated = (ELispExpressionNode) getFunctionObject(function)
                    .callTarget()
                    .call(this, evalArgs(null));
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            generated.executeVoid(frame);
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            return generated.executeLong(frame);
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            return generated.executeDouble(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return generated.executeGeneric(frame);
        }
    }

    private static final class ConsMacroCallNode extends ConsCallNode {
        private final ELispCons cons;

        @Child
        @Nullable
        private ELispExpressionNode inlineLambdaNode = null;

        @Child
        @Nullable
        private ELispExpressionNode generatedNode = null;

        ConsMacroCallNode(Object function, ELispCons cons) {
            super(getIndirectFunction(((ELispCons) function).cdr()), cons, true);
            this.cons = cons;
            if (this.function instanceof ELispCons lambda && lambda.car() == LAMBDA) {
                inlineLambdaNode = BuiltInEval.FFunction.function(lambda);
            }
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            try {
                updateGenerated(frame).executeVoid(frame);
            } catch (ELispSignals.ELispSignalException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try {
                return updateGenerated(frame).executeGeneric(frame);
            } catch (ELispSignals.ELispSignalException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        public ELispExpressionNode updateGenerated(VirtualFrame frame) {
            ELispExpressionNode inner = generatedNode;
            if (inner != null) {
                return inner;
            }

            CompilerDirectives.transferToInterpreterAndInvalidate();
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(true)) {
                Object function = this.function;
                if (inlineLambdaNode != null) {
                    function = inlineLambdaNode.executeGeneric(frame);
                }
                Object o = getFunctionObject(function).callTarget().call(evalArgs(frame));
                if (o instanceof ELispCons debuggable) {
                    debuggable.setSourceLocation(
                            cons.getStartLine(),
                            cons.getStartColumn(),
                            cons.getEndLine(),
                            cons.getEndColumn()
                    );
                }
                return generatedNode = insert(ELispInterpretedNode.create(o));
            }
        }

        @Override
        public SourceSection getSourceSection() {
            return generatedNode == null ? null : generatedNode.getSourceSection();
        }
    }

    public static final class ELispConsExpressionNode extends ELispInterpretedNode {
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

        public ELispCons getCons() {
            return cons;
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
            if (isNil(function)) {
                throw ELispSignals.voidFunction(cons.car());
            }
            if (function instanceof ELispCons fCons && fCons.car() == AUTOLOAD) {
                function = autoload(fCons);
            }
            int newType = switch (function) {
                case ELispSubroutine(_, boolean specialForm, _) when specialForm -> FORM_SPECIAL;
                case ELispSubroutine(_, _, ELispSubroutine.InlineInfo factory) when factory != null -> FORM_INLINED;
                case ELispCons c when c.car() == MACRO -> FORM_MACRO;
                case ELispCons c when c.car() == LAMBDA -> FORM_FUNCTION;
                case ELispSubroutine _, ELispInterpretedClosure _, ELispExpressionNode _ -> FORM_FUNCTION;
                default -> throw ELispSignals.invalidFunction(cons.car());
            };
            if (node == null || type != newType || node.getFunction() != function) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                ConsCallNode created = switch (newType) {
                    case FORM_SPECIAL -> new ConsSpecialCallNode(function, cons);
                    case FORM_INLINED -> new ConsInlinedAstNode(function, cons);
                    case FORM_FUNCTION -> ELispInterpretedNodeFactory.ConsFunctionCallNodeGen.create(function, cons);
                    case FORM_MACRO -> new ConsMacroCallNode(function, cons);
                    default -> throw CompilerDirectives.shouldNotReachHere();
                };
                type = newType;
                callNode = insertOrReplace(created, node);
                cons.fillDebugInfo(getParent());
                return created;
            }
            return node;
        }

        private Object autoload(ELispCons function) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            IndirectCallNode indirectCallNode = Truffle.getRuntime().createIndirectCallNode();
            ELispString file = asStr(asCons(function.cdr()).car());
            ELispRootNode root = BuiltInLRead.loadFile(ELispLanguage.get(this), file, true);
            indirectCallNode.call(Objects.requireNonNull(root).getCallTarget());
            return getIndirectFunction(cons.car());
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
