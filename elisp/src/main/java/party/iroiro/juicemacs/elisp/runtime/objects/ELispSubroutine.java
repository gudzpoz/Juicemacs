package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.dsl.NodeFactory;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.Objects;

/// Container of built-in functions define in subclasses of
/// [party.iroiro.juicemacs.elisp.forms.ELispBuiltIns]
///
/// Several functions can be inlined into the AST (when [#inline()] is not null).
///
/// @see party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.InlineFactory
public record ELispSubroutine(
        ELispFunctionObject body,
        ELispBuiltIn info,
        @Nullable Object inlineFactory
) implements ELispValue {
    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }
    @Override
    public int lispHashCode(int depth) {
        return hashCode();
    }

    @Override
    public void display(ELispPrint print) {
        print.print(toString());
    }

    @Override
    public String toString() {
        return "#<subr " + body.callTarget() +  ">";
    }

    public boolean specialForm() {
        return info.rawArg();
    }

    public boolean inlinable() {
        return inlineFactory != null;
    }

    /// Returns `true` if the inline factory is hand-rolled
    ///
    /// This means one may apply the child nodes as is into [#createNode(ELispExpressionNode\[\])].
    /// Otherwise, the caller will need to handle required args, optional args and varargs before
    /// passing them to the factory.
    public boolean isTailored() {
        return inlineFactory instanceof ELispBuiltInBaseNode.InlineFactory;
    }

    public ELispExpressionNode createNode(ELispExpressionNode[] args) {
        if (inlineFactory instanceof NodeFactory<?> nodeFactory) {
            return (ELispExpressionNode) nodeFactory.createNode((Object) args);
        }
        return ((ELispBuiltInBaseNode.InlineFactory) Objects.requireNonNull(inlineFactory)).createNode(args);
    }

    public ELispExpressionNode createNode(Object[] args) {
        return ((ELispBuiltInBaseNode.SpecialFactory) Objects.requireNonNull(inlineFactory)).createNode(args);
    }
}
