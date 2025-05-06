package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.interop.NodeLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.*;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.runtime.*;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.DebuggerScopeObject;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
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

    public static ELispExpressionNode create(Object expression) {
        return switch (expression) {
            case ELispSymbol symbol when symbol == NIL -> literal(false);
            case ELispSymbol symbol when symbol == T -> literal(true);
            case ELispSymbol symbol when symbol.isKeyword() -> literal(symbol);
            case ELispSymbol symbol -> new ELispSymbolDereferenceNode(symbol);
            case ELispCons cons -> new ELispConsExpressionNode(cons);
            default -> literal(expression);
        };
    }

    public static ELispExpressionNode literal(Object expression) {
        return switch (expression) {
            case Long l -> new ELispLongLiteralNode(l);
            case Double d -> new ELispDoubleLiteralNode(d);
            default -> new ELispObjectLiteralNode(expression);
        };
    }

    public static ELispExpressionNode[] create(Object[] expressions) {
        ELispExpressionNode[] nodes = new ELispExpressionNode[expressions.length];
        for (int i = 0; i < expressions.length; i++) {
            nodes[i] = ELispInterpretedNode.create(expressions[i]);
        }
        return nodes;
    }

    public static ELispExpressionNode createMacroexpand(Object[] expressions, boolean lexical) {
        @Nullable ELispContext context = ELispContext.get(null);
        //noinspection ConstantValue
        if (context != null) {
            Object macroexpand = context.getFunctionStorage(INTERNAL_MACROEXPAND_FOR_LOAD).get();
            if (!isNil(macroexpand)) {
                return new ELispRootExpressions(
                        new ELispMacroexpandExpressions(expressions, macroexpand),
                        lexical
                );
            }
        }
        return new ELispRootExpressions(expressions, lexical);
    }

    public static ELispExpressionNode create(Object[] expressions, boolean lexical) {
        return new ELispRootExpressions(expressions, lexical);
    }

    public static Object getIndirectFunction(Object function) {
        if (toSym(function) instanceof ELispSymbol symbol) {
            function = symbol.getIndirectFunction();
        }
        return function;
    }

    private final static class ELispRootExpressions extends ELispInterpretedNode implements ELispLexical.ScopeProvider {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode node;

        private final ELispLexical.@Nullable Allocator rootLexical;
        private final @Nullable ELispLexical rootScope;

        public ELispRootExpressions(ELispExpressionNode node, boolean lexical) {
            this.node = node;
            this.rootLexical = lexical ? new ELispLexical.Allocator() : null;
            this.rootScope = lexical ? ELispLexical.newRoot() : null;
        }

        public ELispRootExpressions(Object[] expressions, boolean lexical) {
            this(BuiltInEval.FProgn.progn(expressions), lexical);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(rootLexical != null)) {
                node.executeVoid(frame);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(rootLexical != null)) {
                return node.executeGeneric(frame);
            }
        }

        @Override
        public @Nullable ELispLexical lexicalScope() {
            return rootScope;
        }

        @Override
        public ELispLexical.@Nullable Allocator rootScope() {
            return rootLexical;
        }
    }

    private final static class ELispMacroexpandExpressions extends ELispInterpretedNode {
        @Children
        private ELispExpressionNode[] nodes;

        @CompilerDirectives.CompilationFinal
        private int nodeCount = 0;

        @CompilerDirectives.CompilationFinal
        private int currentExpression = 0;

        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private final Object[] expressions;
        private final Object macroexpand;

        @CompilerDirectives.CompilationFinal
        private boolean macroExpanded = false;

        private ELispMacroexpandExpressions(Object[] expressions, Object macroexpand) {
            this.nodes = new ELispExpressionNode[expressions.length];
            this.expressions = expressions;
            this.macroexpand = macroexpand;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            executeGeneric(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            if (macroExpanded) {
                return nodes[0].executeGeneric(frame);
            }
            return evalAndExpand(frame);
        }

        private Object evalAndExpand(VirtualFrame frame) {
            int nodeCount;
            int currentExpression;
            ELispExpressionNode[] nodes = this.nodes;
            synchronized (this) {
                nodeCount = this.nodeCount;
                currentExpression = this.currentExpression;
            }
            @Nullable Object result = null;
            for (int i = 0; i < nodeCount; i++) {
                ELispExpressionNode node = nodes[i];
                if (i == nodeCount - 1) {
                    result = node.executeGeneric(frame);
                } else {
                    node.executeVoid(frame);
                }
            }
            if (currentExpression != expressions.length) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                for (int i = currentExpression; i < expressions.length; i++) {
                    Object expression = expressions[i];
                    Object expanded = BuiltInEval.FFuncall.funcall(this, macroexpand, expression, false);
                    Object[] newNodes = expandNodes(expanded);
                    nodes = addChildren(nodes, newNodes);
                    for (int j = 0; j < newNodes.length; j++) {
                        ELispExpressionNode node = nodes[nodeCount + j];
                        if (i == expressions.length - 1 && j == newNodes.length - 1) {
                            result = node.executeGeneric(frame);
                        } else {
                            node.executeVoid(frame);
                        }
                    }
                    nodeCount += newNodes.length;
                }
            }

            final ELispExpressionNode[] finalNodes = nodes;
            final int finalNodeCount = nodeCount;
            atomic (() -> {
                synchronized (this) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    this.macroExpanded = true;
                    ELispExpressionNode newChild = BuiltInEval.FProgn.prognNode(Arrays.copyOf(finalNodes, finalNodeCount));
                    this.nodes = new ELispExpressionNode[]{insert(newChild)};
                    newChild.adoptChildren(); // NOPMD
                }
            });
            return result == null ? false : result;
        }

        private static Object[] expandNodes(Object expanded) {
            Object @Nullable [] newNodes = null;
            if (expanded instanceof ELispCons cons && cons.car() == PROGN) {
                if (isNil(cons.cdr())) {
                    newNodes = new Object[0];
                } else if (cons.cdr() instanceof ELispCons body) {
                    newNodes = body.toArray();
                }
            }
            if (newNodes == null) {
                newNodes = new Object[]{expanded};
            }
            return newNodes;
        }

        private synchronized ELispExpressionNode[] addChildren(ELispExpressionNode[] original, Object... array) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            int added = array.length;
            int start = nodeCount;
            ELispExpressionNode[] nodes = original;
            if (added + start > nodes.length) {
                nodes = Arrays.copyOf(nodes, (int) ((added + start) * 1.5));
                this.nodes = nodes;
            }
            for (int i = 0; i < added; i++) {
                nodes[start + i] = insert(create(array[i]));
            }
            nodeCount = start + added;
            currentExpression++;
            return nodes;
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
        private final ELispSymbol symbol;

        public ELispSymbolDereferenceNode(ELispSymbol symbol) {
            this.symbol = symbol;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return updateSlotInfo().executeGeneric(frame);
        }

        private ELispExpressionNode updateSlotInfo() {
            ELispLexical.LexicalReference lexical = ELispLexical.getLexicalReference(this, symbol);
            CompilerDirectives.transferToInterpreterAndInvalidate();

            if (lexical == null) {
                return replace(GlobalVariableReadNodeGen.create(symbol));
            } else {
                ELispExpressionNode reader = ELispFrameSlotNode.createRead(lexical.index(), lexical.frame());
                return replace(reader);
            }
        }
    }

    @GenerateWrapper
    @ExportLibrary(NodeLibrary.class)
    abstract static class ConsCallNode extends ELispExpressionNode implements InstrumentableNode {
        protected final Object function;

        @Children
        protected ELispExpressionNode @Nullable[] args;

        protected ConsCallNode(Object function, @Nullable ELispCons cons, boolean special) {
            this.function = function;
            this.args = cons == null ? null : initChildren(cons, special);
        }

        /// Constructor only used by instrumentation wrapper nodes
        ConsCallNode() {
            this(NIL, null, false);
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
            // Trade time for allocation amount. Hopefully most conses are short.
            ELispExpressionNode[] children = new ELispExpressionNode[cons.size() - 1];
            ELispCons.BrentTortoiseHareIterator argIterator = cons.listIterator(1);
            for (int i = 0; argIterator.hasNext(); i++) {
                Object arg = argIterator.next();
                children[i] = special
                        ? literal(arg)
                        : ELispInterpretedNode.create(arg);
            }
            return children;
        }

        public Object getFunction() {
            return function;
        }

        protected static ELispFunctionObject getFunctionObject(Object function) {
            return switch (function) {
                case ELispSubroutine sub -> sub.body();
                case ELispInterpretedClosure closure -> closure.getFunction();
                case ELispBytecode bytecode -> bytecode.getFunction();
                default -> throw new UnsupportedOperationException();
            };
        }

        @Override
        public boolean isInstrumentable() {
            SourceSection source = getSourceSection();
            return source != null && source.isAvailable();
        }

        @Override
        public WrapperNode createWrapper(ProbeNode probe) {
            return new ConsCallNodeWrapper(this, probe);
        }

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return tag == StandardTags.StatementTag.class
                    || tag == StandardTags.CallTag.class
                    || tag == StandardTags.ExpressionTag.class;
        }

        @Override
        public SourceSection getSourceSection() {
            return getParent().getSourceSection();
        }

        //#region NodeLibrary
        @ExportMessage
        public boolean hasScope(Frame frame) {
            @Nullable ELispLexical lexical = ELispLexical.getScope(this);
            return lexical != null;
        }

        @ExportMessage
        public Object getScope(Frame frame, boolean nodeEnter) throws UnsupportedMessageException {
            @Nullable ELispLexical lexical = ELispLexical.getScope(this);
            if (lexical == null) {
                throw UnsupportedMessageException.create();
            }
            return new DebuggerScopeObject(getContext(), lexical, frame);
        }
        //#endregion NodeLibrary
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

        public ELispFunctionObject getFunction(VirtualFrame frame) {
            Object function = this.function;
            if (inlineLambdaNode != null) {
                function = inlineLambdaNode.executeGeneric(frame);
            }
            return getFunctionObject(function);
        }

        @Specialization
        public Object call(
                VirtualFrame frame,
                @Cached(value = "getFunction(frame)", neverDefault = true) ELispFunctionObject function,
                @Cached FunctionDispatchNode dispatchNode
        ) {
            return dispatchNode.executeDispatch(this, function, evalArgs(frame));
        }
    }

    private static final class ConsInlinedAstNode extends ConsCallNode {
        @Child
        ELispExpressionNode inlinedNode;

        private ConsInlinedAstNode(Object function, ELispSubroutine.InlineInfo inline, ELispCons cons) {
            super(function, cons, false);
            inlinedNode = generateInlineNode(cons, Objects.requireNonNull(inline));
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
                ELispExpressionNode node = ELispInterpretedNode.create(argIterator.next());
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
                nodes.add(new VarargToArrayNode(restNodes));
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

    public static class VarargToArrayNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ELispExpressionNode[] restArgs;

        public VarargToArrayNode(List<ELispExpressionNode> restNodes) {
            restArgs = restNodes.toArray(ELispExpressionNode[]::new);
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
    }

    private static final class ConsSpecialCallNode extends ConsCallNode {
        @Child
        private ELispExpressionNode generated = null;

        ConsSpecialCallNode(Object function, ELispFunctionObject target, ELispCons cons) {
            super(function, cons, true);
            generated = (ELispExpressionNode) target.callTarget().call(this, evalArgs(null));
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

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return tag == StandardTags.StatementTag.class || tag == StandardTags.ExpressionTag.class;
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

        ConsMacroCallNode(ELispCons function, ELispCons cons) {
            super(getIndirectFunction(function.cdr()), cons, true);
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
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(
                    ELispLexical.getScope(this) != null
            )) {
                Object function = this.function;
                if (inlineLambdaNode != null) {
                    function = inlineLambdaNode.executeGeneric(frame);
                }
                Object o = getFunctionObject(function).callTarget().call(this, evalArgs(frame));
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

    public static final class ELispConsExpressionNode extends ELispInterpretedNode implements InstrumentableNode {
        private final ELispCons cons;

        @Nullable
        private final FunctionStorage storage;

        @CompilerDirectives.CompilationFinal
        private Assumption stable;

        @Child
        @Nullable
        private ConsCallNode callNode;

        public ELispConsExpressionNode(ELispCons cons) {
            this.cons = cons;
            this.callNode = null;
            if (toSym(cons.car()) instanceof ELispSymbol symbol) {
                storage = getContext().getFunctionStorage(symbol);
                stable = storage.getStableAssumption();
            } else {
                storage = null;
                stable = Assumption.NEVER_VALID;
            }
        }

        public ELispCons getCons() {
            return cons;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            updateInnerNode().executeVoid(frame);
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
           return updateInnerNode().executeLong(frame);
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
          return updateInnerNode().executeDouble(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return updateInnerNode().executeGeneric(frame);
        }

        private ConsCallNode updateInnerNode() {
            ConsCallNode node = callNode;
            if (stable.isValid() && node != null) {
                return node;
            }

            Object function;
            if (storage != null) {
                function = storage.get();
                if (toSym(function) instanceof ELispSymbol symbol) {
                    function = getIndirectFunction(symbol);
                } else {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    stable = storage.getStableAssumption();
                }
            } else {
                function = getIndirectFunction(cons.car());
            }
            if (isNil(function)) {
                throw ELispSignals.voidFunction(cons.car());
            }
            if (function instanceof ELispCons fCons && fCons.car() == AUTOLOAD) {
                function = autoload(fCons);
            }
            if (node == null || node.getFunction() != function) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                ConsCallNode created = switch (function) {
                    case ELispSubroutine sub when sub.specialForm() ->
                            new ConsSpecialCallNode(function, sub.body(), cons);
                    case ELispSubroutine sub when sub.inline() != null ->
                            new ConsInlinedAstNode(function, sub.inline(), cons);
                    case ELispCons c when c.car() == MACRO ->
                            new ConsMacroCallNode(c, cons);
                    case ELispCons c when c.car() == LAMBDA ->
                            ELispInterpretedNodeFactory.ConsFunctionCallNodeGen.create(function, cons);
                    case ELispSubroutine _, ELispInterpretedClosure _, ELispBytecode _, ELispExpressionNode _ ->
                            ELispInterpretedNodeFactory.ConsFunctionCallNodeGen.create(function, cons);
                    default -> throw ELispSignals.invalidFunction(cons.car());
                };
                callNode = insertOrReplace(created, node);
                cons.fillDebugInfo(getParent());
                return created;
            }
            return node;
        }

        private Object autoload(ELispCons function) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            ELispString file = asStr(asCons(function.cdr()).car());
            BuiltInLRead.loadFile(getLanguage(), this, file, true);
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

        @Override
        public boolean isInstrumentable() {
            SourceSection source = getSourceSection();
            return source != null && source.isAvailable();
        }

        @Override
        public WrapperNode createWrapper(ProbeNode probe) {
            throw new UnsupportedOperationException();
        }

        @Override
        public InstrumentableNode materializeInstrumentableNodes(Set<Class<? extends Tag>> materializedTags) {
            expandAllChildren();
            return this;
        }

        private void expandAllChildren() {
            if (callNode != null) {
                return;
            }
            ConsCallNode consCallNode;
            try {
                Object function = getIndirectFunction(storage == null ? cons.car() : storage.get());
                if (function instanceof ELispCons fCons && fCons.car() == AUTOLOAD) {
                    return;
                }
                consCallNode = updateInnerNode(); // NOPMD
            } catch (Throwable ignored) {
                return;
            }
            if (consCallNode.args == null) {
                return;
            }
            for (ELispExpressionNode arg : consCallNode.args) {
                if (arg instanceof ELispConsExpressionNode child) {
                    child.expandAllChildren();
                }
            }
        }
    }

}
