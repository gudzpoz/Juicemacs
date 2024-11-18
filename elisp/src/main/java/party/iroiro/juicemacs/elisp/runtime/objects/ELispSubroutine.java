package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.dsl.NodeFactory;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

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
        @Nullable InlineInfo inline
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
    public String toString() {
        return "#<subr " + body.callTarget().toString() +  ">";
    }

    /// Information about an inline-able built-in function
    ///
    /// For example, `(+ 1 1.0 1)` can get inlined into `(long+double 1 (double+long 1.0 1))`,
    /// so that each node gets its own specialized implementation to speed things up.
    ///
    /// Also, [special forms](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Forms.html)
    /// are inlined by default and do not need a factory (and [#factory()] is null).
    ///
    /// [#stable()] is used to detect if any *bad* users rewrites these special functions...
    /// (Emacs allows overriding `+`, `-` and even special forms like `if`.)
    /// It boosts the performance if the user does not do that, when the interpreter can slack and
    /// does not need to check if the function is still the same.
    /// It is only invalidated by [ELispSymbol], when the user assigns a new function to those special symbols
    /// like `+`, `-`, `if`, etc.
    ///
    /// The execution of inlined nodes and special forms are defined in
    /// [party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode].
    ///
    /// @see party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode
    public record InlineInfo(ELispBuiltIn info, @Nullable Object factory, Assumption stable) {
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
