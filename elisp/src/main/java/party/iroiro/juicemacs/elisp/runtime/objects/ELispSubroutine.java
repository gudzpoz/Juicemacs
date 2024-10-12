package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.dsl.NodeFactory;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

public record ELispSubroutine(
        ELispFunctionObject body,
        boolean specialForm,
        @Nullable InlineInfo inline
) implements ELispValue {
    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }

    @Override
    public String toString() {
        return "#<subr " + body.callTarget().toString() +  ">";
    }

    /// @see party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode
    public record InlineInfo(ELispBuiltIn info, Object factory) {
        public ELispExpressionNode createNode(ELispExpressionNode[] args) {
            if (factory instanceof NodeFactory<?> nodeFactory) {
                return (ELispExpressionNode) nodeFactory.createNode((Object) args);
            }
            return ((ELispBuiltInBaseNode.InlineFactory) factory).createNode(args);
        }

        public boolean isTailored() {
            return factory instanceof ELispBuiltInBaseNode.InlineFactory;
        }
    }
}
