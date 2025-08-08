package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;

public abstract class ELispLiteralNodes {
    private ELispLiteralNodes() {
    }

    @SuppressWarnings("PMD.ShortMethodName")
    public static ELispExpressionNode of(Object expression) {
        return switch (expression) {
            case Long l -> new LongLiteralNode(l);
            case Double d -> new DoubleLiteralNode(d);
            default -> new ObjectLiteralNode(expression);
        };
    }

    private static final class LongLiteralNode extends ELispExpressionNode {
        private final Long literal;

        public LongLiteralNode(long literal) {
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

    private static final class DoubleLiteralNode extends ELispExpressionNode {
        private final Double literal;

        public DoubleLiteralNode(double literal) {
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
    /// If one wishes to have an idempotent evaluation, one should
    /// always re-read the source string, and remember to suffix the source
    /// with a random comment to avoid Truffle AST caching.
    ///
    private static final class ObjectLiteralNode extends ELispExpressionNode {
        private final Object literal;

        public ObjectLiteralNode(Object literal) {
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
}
