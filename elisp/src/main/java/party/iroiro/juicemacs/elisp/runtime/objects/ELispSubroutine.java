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
/// @see InlineInfo
public record ELispSubroutine(
        ELispFunctionObject body,
        boolean specialForm,
        @Nullable InlineInfo inline,
        ELispBuiltIn info
) implements ELispValue {
    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }
    @Override
    public int lispHashCode() {
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

    /// Information about an inline-able built-in function
    ///
    /// For example, `(+ 1 1.0 1)` can get inlined into `(long+double 1 (double+long 1.0 1))`,
    /// so that each node gets its own specialized implementation to speed things up.
    ///
    /// Also, [special forms](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Forms.html)
    /// are inlined by default and do not need a factory (and [#factory()] is null).
    ///
    /// The execution of inlined nodes and special forms are defined in
    /// [party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode].
    ///
    /// @see party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode
    public record InlineInfo(ELispBuiltIn info, @Nullable Object factory) {
        public ELispExpressionNode createNode(ELispExpressionNode[] args) {
            if (factory instanceof NodeFactory<?> nodeFactory) {
                return (ELispExpressionNode) nodeFactory.createNode((Object) args);
            }
            return ((ELispBuiltInBaseNode.InlineFactory) Objects.requireNonNull(factory)).createNode(args);
        }

        /// Returns `true` if the inline factory is hand-rolled
        ///
        /// This means one may apply the child nodes as is into [#createNode(ELispExpressionNode\[\])].
        /// Otherwise, the caller will need to handle required args, optional args and varargs before
        /// passing them to the factory.
        public boolean isTailored() {
            return factory instanceof ELispBuiltInBaseNode.InlineFactory;
        }
    }
}
