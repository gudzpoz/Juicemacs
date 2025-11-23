package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNodeGen;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.AbstractELispClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.AbstractELispClosure.ClosureCommons;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.INTERNAL_MACROEXPAND_FOR_LOAD;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.PROGN;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public abstract class ELispRootNodes {
    private ELispRootNodes() {
    }

    public static ELispExpressionNode createMacroexpand(Object[] expressions, boolean lexical) {
        ELispContext context = ELispContext.get(null);
        //noinspection ConstantValue: nullable when creating AST during debug sessions
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

    public static ELispExpressionNode createRoot(Object[] expressions, ELispCons lexical) {
        ELispCons env = asCons(lexical);
        AbstractELispClosure closure = AbstractELispClosure.create(
                List.of(false, ELispCons.listOf(expressions), env),
                new ClosureCommons()
        );
        ELispExpressionNode[] funcallArgs = { ELispLiteralNodes.of(closure) };
        ELispExpressionNode root = FuncallDispatchNode.createSpecializedCallNode(funcallArgs);
        return new ELispRootExpressions(root, true);
    }

    public static ELispExpressionNode createRoot(Object[] expressions, boolean lexical) {
        return new ELispRootExpressions(expressions, lexical);
    }

    public static ELispExpressionNode createRoot(Object[] expressions, ELispLexical.@Nullable Scope debugScope) {
        return new ELispRootExpressions(expressions, debugScope);
    }

    private final static class ELispRootExpressions extends ELispExpressionNode implements ELispLexical.ScopeProvider {
        @Child
        ELispExpressionNode node;

        private final ELispLexical.@Nullable Scope rootScope;

        ELispRootExpressions(ELispExpressionNode node, boolean lexical) {
            this.node = node;
            this.rootScope = lexical ? ELispLexical.newRoot().newScope(0) : null;
        }

        ELispRootExpressions(Object[] body, ELispLexical.@Nullable Scope debugScope) {
            this.node = BuiltInEval.FProgn.progn(body);
            this.rootScope = debugScope;
        }

        ELispRootExpressions(Object[] expressions, boolean lexical) {
            this(BuiltInEval.FProgn.progn(expressions), lexical);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            Dynamic scope = Dynamic.withLexicalBinding(rootScope != null);
            try {
                node.executeVoid(frame);
            } finally {
                scope.close();
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Dynamic scope = Dynamic.withLexicalBinding(rootScope != null);
            try {
                return node.executeGeneric(frame);
            } finally {
                scope.close();
            }
        }

        @Override
        public ELispLexical.@Nullable Scope getScope() {
            return rootScope;
        }

        @Nullable
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

    /// Body node for root nodes with runtime macro-expansion
    ///
    /// This node implements functionality done by `readevalloop_eager_expand_eval`
    /// in GNU Emacs.
    private final static class ELispMacroexpandExpressions extends ELispExpressionNode {
        @Child
        FuncallDispatchNode callNode;
        @Children
        ELispExpressionNode[] nodes;

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
                return replace(nodes[0]).executeGeneric(frame);
            }
            CompilerDirectives.transferToInterpreterAndInvalidate();
            return evalAndExpand(frame);
        }

        private synchronized Object evalAndExpand(VirtualFrame frame) {
            ArrayList<Object> expressions = new ArrayList<>(Arrays.asList(this.expressions).reversed());
            ELispExpressionNode[] nodes = this.nodes;
            int nodeCount = 0;
            Object result = null;
            while (!expressions.isEmpty()) {
                Object expression = expressions.removeLast();
                Object expanded = callNode.dispatch(this, macroexpand, expression, false);
                BuiltInEval.FMacroexpand.copySourceLocation(expanded, expression);
                if (expanded instanceof ELispCons form && form.car() == PROGN
                    && form.cdr() instanceof ELispCons body) {
                    expressions.addAll(Arrays.asList(body.toArray()).reversed());
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
            ELispExpressionNode newChild = ELispInterpretedNode.create(form);
            nodes[i] = insert(newChild);
            notifyInserted(newChild);
            return nodes;
        }
    }
}
