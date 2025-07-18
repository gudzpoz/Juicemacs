package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerAsserts;
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
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNodeGen;
import party.iroiro.juicemacs.elisp.nodes.funcall.FunctionObjectCallNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionObjectNodes;
import party.iroiro.juicemacs.elisp.nodes.local.ELispFrameSlotReadNode;
import party.iroiro.juicemacs.elisp.runtime.*;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.DebuggerScopeObject;

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
/// @see LazyConsExpressionNode
public abstract class ELispInterpretedNode extends ELispExpressionNode {

    public static ELispExpressionNode create(Object expression) {
        return switch (expression) {
            case ELispSymbol symbol when symbol == NIL -> literal(false);
            case ELispSymbol symbol when symbol == T -> literal(true);
            case ELispSymbol symbol when symbol.isKeyword() -> literal(symbol);
            case ELispSymbol symbol -> new ELispSymbolDereferenceNode(symbol);
            case ELispCons cons -> new LazyConsExpressionNode(cons);
            default -> literal(expression);
        };
    }

    public static ELispExpressionNode createWithLocation(Object expression, ELispCons original) {
        ELispExpressionNode node = create(expression);
        if (original.hasLocation()) {
            if (expression instanceof ELispCons cons) {
                cons.fillDebugInfo(original);
            } else {
                node = new SourceSectionWrapper(original, node);
            }
        }
        return node;
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

    public static ELispExpressionNode createRoot(Object[] expressions, boolean lexical) {
        return new ELispRootExpressions(expressions, lexical);
    }

    public static ELispExpressionNode createRoot(Object[] expressions, ELispLexical.@Nullable Scope debugScope) {
        return new ELispRootExpressions(expressions, debugScope);
    }

    public static Object getIndirectFunction(Object function) {
        if (toSym(function) instanceof ELispSymbol symbol) {
            function = symbol.getIndirectFunction();
        }
        return function;
    }

    public static SourceSection getConsSourceSection(Node node, ELispCons cons) {
        RootNode rootNode = node.getRootNode();
        if (rootNode == null) {
            return null;
        }
        SourceSection section = rootNode.getSourceSection();
        if (section == null) {
            return null;
        }
        return cons.getSourceSection(section.getSource());
    }

    private final static class ELispRootExpressions extends ELispInterpretedNode implements ELispLexical.ScopeProvider {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode node;

        private final ELispLexical.@Nullable Scope rootScope;

        public ELispRootExpressions(ELispExpressionNode node, boolean lexical) {
            this.node = node;
            this.rootScope = lexical ? ELispLexical.newRoot().newScope(0) : null;
        }

        public ELispRootExpressions(Object[] body, ELispLexical.@Nullable Scope debugScope) {
            this.node = BuiltInEval.FProgn.progn(body);
            this.rootScope = debugScope;
        }

        public ELispRootExpressions(Object[] expressions, boolean lexical) {
            this(BuiltInEval.FProgn.progn(expressions), lexical);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            try (Dynamic _ = Dynamic.withLexicalBinding(rootScope != null)) {
                node.executeVoid(frame);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try (Dynamic _ = Dynamic.withLexicalBinding(rootScope != null)) {
                return node.executeGeneric(frame);
            }
        }

        @Override
        public ELispLexical.@Nullable Scope getScope() {
            return rootScope;
        }

        @Override
        public SourceSection getSourceSection() {
            RootNode rootNode = getRootNode();
            if (rootNode == null) {
                return null;
            }
            return rootNode.getSourceSection();
        }

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return tag == StandardTags.RootTag.class;
        }
    }

    private final static class ELispMacroexpandExpressions extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private FuncallDispatchNode callNode;
        @Children
        private ELispExpressionNode[] nodes;

        @CompilerDirectives.CompilationFinal
        private boolean macroExpanded = false;

        private final Object macroexpand;
        private final Object[] expressions;

        private ELispMacroexpandExpressions(Object[] expressions, Object macroexpand) {
            this.nodes = new ELispExpressionNode[expressions.length];
            this.expressions = expressions;
            this.macroexpand = macroexpand;
            this.callNode = FuncallDispatchNodeGen.create();
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
            CompilerDirectives.transferToInterpreterAndInvalidate();
            return evalAndExpand(frame);
        }

        private synchronized Object evalAndExpand(VirtualFrame frame) {
            CompilerAsserts.neverPartOfCompilation();
            ArrayList<Object> expressions = new ArrayList<>(List.of(this.expressions).reversed());
            ELispExpressionNode[] nodes = this.nodes;
            int nodeCount = 0;
            @Nullable Object result = null;
            while (!expressions.isEmpty()) {
                Object expression = expressions.removeLast();
                Object expanded = callNode.dispatch(this, macroexpand, expression, false);
                BuiltInEval.FMacroexpand.copySourceLocation(expanded, expression);
                if (expanded instanceof ELispCons form && form.car() == PROGN
                        && form.cdr() instanceof ELispCons body) {
                    expressions.addAll(List.of(body.toArray()).reversed());
                } else {
                    expanded = callNode.dispatch(this, macroexpand, expression, true);
                    BuiltInEval.FMacroexpand.copySourceLocation(expanded, expression);
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    this.nodes = nodes = addChild(nodes, expanded, nodeCount);
                    result = nodes[nodeCount].executeGeneric(frame); // NOPMD: never part of compilation
                    nodeCount++;
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
                    notifyInserted(newChild);
                }
            });
            expressions.clear();
            return result == null ? false : result;
        }

        private synchronized ELispExpressionNode[] addChild(ELispExpressionNode[] original, Object form, int i) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            ELispExpressionNode[] nodes = original;
            if (i + 1 > nodes.length) {
                nodes = Arrays.copyOf(nodes, (int) ((i + 1) * 1.5));
                this.nodes = nodes;
            }
            ELispExpressionNode newChild = create(form);
            nodes[i] = insert(newChild);
            notifyInserted(newChild);
            return nodes;
        }
    }

    public static final class SourceSectionWrapper extends ELispExpressionNode {
        private final ELispCons cons;
        @Child
        ELispExpressionNode inner;

        public SourceSectionWrapper(ELispCons cons, ELispExpressionNode inner) {
            this.cons = cons;
            this.inner = inner;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            try {
                inner.executeVoid(frame);
            } catch (RuntimeException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return inner.executeLong(frame);
            } catch (RuntimeException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return inner.executeDouble(frame);
            } catch (RuntimeException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try {
                return inner.executeGeneric(frame);
            } catch (RuntimeException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        @Nullable
        public SourceSection getSourceSection() {
            return getConsSourceSection(this, cons);
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
                ELispExpressionNode reader = ELispFrameSlotReadNode.createRead(lexical);
                return replace(reader);
            }
        }
    }

    @GenerateWrapper
    @ExportLibrary(NodeLibrary.class)
    public abstract static class ConsCallNode extends ELispExpressionNode implements InstrumentableNode {
        protected final ELispCons cons;

        public ConsCallNode(ELispCons cons) {
            this.cons = cons;
        }

        public ConsCallNode(ConsCallNode copy) {
            this(copy.cons);
        }

        protected static ELispExpressionNode[] initChildren(ELispCons cons, int reserved) {
            // Trade time for allocation amount. Hopefully most conses are short.
            ELispExpressionNode[] children = new ELispExpressionNode[cons.size() - 1 + reserved];
            ELispCons.ConsIterator argIterator = cons.listIterator(1);
            for (int i = reserved; argIterator.hasNext(); i++) {
                Object arg = argIterator.next();
                children[i] = ELispInterpretedNode.create(arg);
            }
            return children;
        }

        protected static Object[] argsArray(ELispCons cons) {
            Object cdr = cons.cdr();
            if (isNil(cdr)) {
                return new Object[0];
            } else {
                return asCons(cdr).toArray();
            }
        }
        protected static Object[] argsArrayWithFunc(Object function, ELispCons cons) {
            Object cdr = cons.cdr();
            if (isNil(cdr)) {
                return new Object[]{function};
            } else {
                Object[] array = cons.toArray();
                array[0] = function;
                return array;
            }
        }

        public ELispCons getCons() {
            return cons;
        }

        @Override
        public boolean isInstrumentable() {
            SourceSection source = getSourceSection();
            return source != null && source.isAvailable();
        }

        @Override
        public WrapperNode createWrapper(ProbeNode probe) {
            return new ConsCallNodeWrapper(this, this, probe);
        }

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return tag == StandardTags.StatementTag.class
                    || tag == StandardTags.CallTag.class
                    || tag == StandardTags.ExpressionTag.class;
        }

        @Override
        public SourceSection getSourceSection() {
            return getConsSourceSection(this, cons);
        }

        //#region NodeLibrary
        @ExportMessage
        public boolean hasScope(Frame frame) {
            ELispLexical.@Nullable Scope lexical = ELispLexical.getScope(this);
            return frame != null && lexical != null;
        }

        @ExportMessage
        public Object getScope(Frame frame, boolean nodeEnter) throws UnsupportedMessageException {
            ELispLexical.@Nullable Scope lexical = ELispLexical.getScope(this);
            if (frame == null || lexical == null) {
                throw UnsupportedMessageException.create();
            }
            return new DebuggerScopeObject(getContext(), lexical, frame.materialize());
        }
        //#endregion NodeLibrary
    }

    abstract static class ConsFunctionCallNode extends ConsCallNode {
        @Children
        protected ELispExpressionNode[] args;

        ConsFunctionCallNode(ELispCons cons) {
            super(cons);
            this.args = initChildren(cons, 1);
            this.args[0] = ReadFunctionObjectNodes.createFormCardinal(cons.car());
        }

        @ExplodeLoop
        private Object[] evalArgs(VirtualFrame frame) {
            int length = Objects.requireNonNull(this.args).length;
            Object[] args = new Object[length];
            for (int i = 0; i < length; i++) {
                args[i] = this.args[i].executeGeneric(frame);
            }
            return args;
        }

        @Specialization
        public Object call(VirtualFrame frame, @Cached(inline = true) FunctionObjectCallNode dispatchNode) {
            return dispatchNode.executeCall(this, cons.car(), evalArgs(frame));
        }
    }

    private static final class ConsInlinedAstNode extends ConsCallNode {
        @Child
        ELispExpressionNode inlinedNode;

        private final Assumption stable;

        private ConsInlinedAstNode(Assumption stable, ELispSubroutine inline, ELispCons cons) {
            super(cons);
            this.inlinedNode = generateInlineNode(cons, Objects.requireNonNull(inline));
            this.stable = stable;
        }

        private static ELispExpressionNode generateInlineNode(ELispCons cons, ELispSubroutine inline) {
            ELispBuiltIn info = inline.info();
            int args = cons.size() - 1;
            if (args < info.minArgs() || (!info.varArgs() && info.maxArgs() < args)) {
                throw ELispSignals.wrongNumberOfArguments(cons.car(), args);
            }
            if (inline.specialForm()) {
                return inline.createNode(argsArray(cons));
            } else if (inline.isTailored()) {
                return inline.createNode(initChildren(cons, 0));
            }
            List<ELispExpressionNode> nodes = new ArrayList<>();
            List<ELispExpressionNode> restNodes = new ArrayList<>();
            ELispCons.ConsIterator argIterator = cons.listIterator(1);
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
            if (!stable.isValid()) {
                rewriteNode().executeVoid(frame);
                return;
            }
            try {
                inlinedNode.executeVoid(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            if (!stable.isValid()) {
                return rewriteNode().executeLong(frame);
            }
            try {
                return inlinedNode.executeLong(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            if (!stable.isValid()) {
                return rewriteNode().executeDouble(frame);
            }
            try {
                return inlinedNode.executeDouble(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw rewriteException(e);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            if (!stable.isValid()) {
                return rewriteNode().executeGeneric(frame);
            }
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

        public ELispExpressionNode rewriteNode() {
            return replace(new LazyConsExpressionNode(cons));
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

    public static final class LazyConsExpressionNode extends ELispInterpretedNode implements InstrumentableNode {
        public final ELispCons cons;

        public LazyConsExpressionNode(ELispCons cons) {
            this.cons = cons;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            updateInnerNode().executeVoid(frame);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return updateInnerNode().executeGeneric(frame);
        }

        private ELispExpressionNode updateInnerNode() {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            Object cardinal = cons.car();
            @Nullable Assumption stable = toSym(cardinal) instanceof ELispSymbol sym
                    ? getContext().getFunctionStorage(sym).getStableAssumption()
                    : null;
            Object function = ReadFunctionObjectNodes.getFunctionUncached(this, cardinal);
            if (function instanceof ELispSubroutine sub) {
                if (sub.specialForm()) {
                    return replace(sub.createNode(ConsCallNode.argsArray(cons)));
                }
            }
            ELispExpressionNode created = switch (function) {
                case ELispCons c when c.car() == MACRO -> {
                    Object callable = ReadFunctionObjectNodes.getFunctionUncached(this, c.cdr());
                    try (Dynamic _ = Dynamic.withLexicalBinding(
                            ELispLexical.getScope(this) != null
                    )) {
                        Object[] args = ConsCallNode.argsArrayWithFunc(callable, cons);
                        Object o = FuncallDispatchNodeGen.getUncached().executeDispatch(this, args);
                        yield ELispInterpretedNode.createWithLocation(o, cons);
                    }
                }
                case ELispSubroutine sub when sub.inlinable() && stable != null && stable != Assumption.NEVER_VALID ->
                        new ConsInlinedAstNode(stable, sub, cons);
                case ELispSubroutine _, ELispInterpretedClosure _, ELispBytecode _ ->
                        ELispInterpretedNodeFactory.ConsFunctionCallNodeGen.create(cons);
                default -> throw ELispSignals.invalidFunction(cons.car());
            };
            return replace(created);
        }

        @Override
        public SourceSection getSourceSection() {
            return getConsSourceSection(this, cons);
        }

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return false;
        }

        @Override
        public InstrumentableNode materializeInstrumentableNodes(Set<Class<? extends Tag>> materializedTags) {
            expandAllChildren();
            return this;
        }

        private void expandAllChildren() {
            try {
                updateInnerNode(); // NOPMD
            } catch (Throwable ignored) {
            }
        }
    }
}
