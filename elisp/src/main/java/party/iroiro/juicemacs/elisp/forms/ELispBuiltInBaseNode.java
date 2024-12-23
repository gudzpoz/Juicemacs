package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asBuffer;

@NodeChild(value = "arguments", type = ELispExpressionNode[].class)
public abstract class ELispBuiltInBaseNode extends ELispExpressionNode {
    public static final Source JAVA_SOURCE = Source.newBuilder("java", "", "<built-in>")
            .content(Source.CONTENT_NONE)
            .internal(true)
            .build();

    public static ELispBuffer currentBuffer() {
        return asBuffer(ELispLanguage.get(null).currentBuffer().getValue());
    }

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
    /// @see BuiltInData.FPlusBinary
    /// @see BuiltInData.FPlus
    public interface InlineFactory {
        ELispExpressionNode createNode(ELispExpressionNode[] arguments);
    }
}
