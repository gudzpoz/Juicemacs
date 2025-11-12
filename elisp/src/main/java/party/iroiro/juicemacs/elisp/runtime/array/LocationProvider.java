package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ast.ConsCallNode;
import party.iroiro.juicemacs.elisp.nodes.ast.LazyConsExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.TruffleUtils;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;

/// Interface implemented by some [ELispValue] for debug location info
public interface LocationProvider {
    long getEncodedLocation();
    void setEncodedLocation(long encodedLocation);

    default boolean hasLocation() {
        return getEncodedLocation() != 0;
    }
    default SourceLocation getLocation() {
        return SourceLocation.decodeDebugInfo(getEncodedLocation());
    }
    default void setSourceLocation(int startLine, int startColumn, int endLine, int endColumn) {
        setEncodedLocation(SourceLocation.encodeDebugInfo(startLine, startColumn, endLine, endColumn));
    }
    default void fillDebugInfo(LocationProvider original) {
        if (getEncodedLocation() == 0 && original.getEncodedLocation() != 0) {
            setEncodedLocation(original.getEncodedLocation());
        }
    }
    @TruffleBoundary
    @SuppressWarnings("PMD.TruffleNodeUseInsertToAdoptDynamicChildren")
    default void fillDebugInfo(@Nullable Node parent) {
        while (parent != null) {
            // Some parent nodes, like function root nodes, will call the children's
            // getSourceSection method, so we need to make sure *not* to call those
            // parent nodes' getSourceSection method, which might lead to stack overflow.
            //
            // Also, cons-related nodes are a good filter: most inlined nodes just has no
            // source info.
            if (parent instanceof LazyConsExpressionNode || parent instanceof ConsCallNode) {
                SourceSection source = parent.getSourceSection();
                if (source != null && source.isAvailable()) {
                    setSourceLocation(
                            source.getStartLine(),
                            source.getStartColumn(),
                            source.getEndLine(),
                            source.getEndColumn()
                    );
                    return;
                }
            }
            parent = parent.getParent();
        }
    }
    @Nullable
    default SourceSection getSourceSection(Source source) {
        if (!hasLocation()) {
            return null;
        }
        SourceLocation location = getLocation();
        return TruffleUtils.createSection(source,
                location.startLine(), location.startColumn(),
                location.endLine(), location.endColumn()
        );
    }
}
