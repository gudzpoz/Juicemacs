package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;

public abstract class TruffleUtils {
    private TruffleUtils() {
    }

    /// Creates a [SourceSection] retaining all provide information.
    ///
    /// This is a workaround utility method because [Source#createSection(int, int, int, int)]
    /// will try to detect whether the provided line numbers are valid. For non-Unicode texts
    /// in some of the Emacs files (or those with non-`\n` line breaks), this almost always fails.
    ///
    /// Unlike [Source#createSection(int, int, int, int)], this method should *never*
    /// throw exceptions with the following workaround:
    ///
    /// 1. [Source#createSection(int, int, int, int)] as is.
    /// 2. [Source#createSection(int)] with only the starting line number.
    /// 3. Use [Source#newBuilder(Source)] to remove file contents so that Truffle
    ///    has no access to line number info. (Note that this might cause problems
    ///    for Truffle debug utilities.)
    /// 4. `null` (because [Source#createUnavailableSection()] causes *a lot* of problems).
    @Nullable
    public static SourceSection createSection(
            Source source,
            int startLine, int startColumn,
            int endLine, int endColumn
    ) {
        try {
            return source.createSection(startLine, startColumn, endLine, endColumn);
        } catch (IllegalArgumentException ignored1) {
            try {
                return source.createSection(startLine);
            } catch (IllegalArgumentException ignored2) {
                Source newSource = Source.newBuilder(source).content(Source.CONTENT_NONE).build();
                try {
                    return newSource.createSection(startLine, startColumn, endLine, endColumn);
                } catch (IllegalArgumentException ignored3) {
                    return null;
                }
            }
        }
    }
}
