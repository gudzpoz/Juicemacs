package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.*;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.*;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

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
public abstract class ELispInterpretedNode {

    public static ELispExpressionNode create(Object expression) {
        return switch (expression) {
            case ELispSymbol symbol when symbol == NIL -> ELispLiteralNodes.of(false);
            case ELispSymbol symbol when symbol == T -> ELispLiteralNodes.of(true);
            case ELispSymbol symbol when symbol.isKeyword() -> ELispLiteralNodes.of(symbol);
            case ELispSymbol symbol -> new ELispSymbolDereferenceNode(symbol);
            case ELispCons cons -> new LazyConsExpressionNode(cons);
            default -> ELispLiteralNodes.of(expression);
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

    public static ELispExpressionNode[] create(Object[] expressions) {
        ELispExpressionNode[] nodes = new ELispExpressionNode[expressions.length];
        for (int i = 0; i < expressions.length; i++) {
            nodes[i] = ELispInterpretedNode.create(expressions[i]);
        }
        return nodes;
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
}
