package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ast.ELispInterpretedNode;

@NodeChild(value = "arguments", type = ELispExpressionNode[].class)
public abstract class ELispBuiltInBaseNode extends ELispExpressionNode {
    public static final Source JAVA_SOURCE = Source.newBuilder("java", "", "<built-in>")
            .content(Source.CONTENT_NONE)
            .internal(true)
            .build();

    @CompilerDirectives.TruffleBoundary
    @Override
    public SourceSection getSourceSection() {
        Class<?> enclosingClass = this.getClass();
        while (enclosingClass.getEnclosingClass() != null) {
            enclosingClass = enclosingClass.getEnclosingClass();
        }
        String simpleName = enclosingClass.getSimpleName();
        if (simpleName.endsWith("Factory")) {
            simpleName = simpleName.substring(0, simpleName.length() - "Factory".length());
        }
        Source source = ELispBuiltIns.BUILT_IN_SOURCES.getOrDefault(simpleName, JAVA_SOURCE);
        return source.createUnavailableSection();
    }

    /// A factory interface for hand-rolled built-in function inlining
    ///
    /// For example, as a function, `+` needs to handle all numeric types and is relatively
    /// slow in cases like `(+ 1 2.0 some-big-num)`. Instead, we can inline the function
    /// into `(ast_+ (ast_+ 1 2.0) some-big-num)` and avoid the costs of function calls and
    /// have each AST node get their own specialized implementation.
    ///
    /// For example, `(+ 1 1.0 1)` can get inlined into `(long+double 1 (double+long 1.0 1))`,
    /// so that each node gets its own specialized implementation to speed things up.
    ///
    /// The execution of inlined nodes and special forms are defined in
    /// [ELispInterpretedNode].
    ///
    /// @see BuiltInData.FPlusBinary
    /// @see BuiltInData.FPlus
    /// @see ELispInterpretedNode
    public interface InlineFactory {
        ELispExpressionNode createNode(ELispExpressionNode[] arguments);
    }

    /// Similar to [InlineFactory], but with raw arguments
    public interface SpecialFactory {
        ELispExpressionNode createNode(Object[] arguments);
    }
}
