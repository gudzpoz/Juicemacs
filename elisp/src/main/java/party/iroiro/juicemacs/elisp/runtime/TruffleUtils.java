package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;

import java.util.Iterator;

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
    @TruffleBoundary
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

    /// Workaround for [Throwable#addSuppressed(Throwable)] during native image generation
    ///
    /// [Throwable#addSuppressed(Throwable)] is blocklisted and should not be reachable during
    /// runtime compilation.
    @TruffleBoundary
    public static void addSuppressed(Exception e, Exception suppressed) {
        e.addSuppressed(suppressed);
    }

    /// [com.oracle.truffle.api.CompilerDirectives.TruffleBoundary] wrapper around [Throwable#getMessage()]
    @TruffleBoundary
    public static String eMessage(Throwable e) {
        String message = e.getMessage();
        return message == null ? e.getClass().getName() : message;
    }

    /// [com.oracle.truffle.api.CompilerDirectives.TruffleBoundary] wrapper around [Object#toString()]
    @TruffleBoundary
    public static String toString(Object o) {
        return o.toString();
    }

    /// A [com.oracle.truffle.api.CompilerDirectives.TruffleBoundary] wrapper around [Iterator]
    ///
    /// The iterator interface is highly polymorphic and might cause Truffle native images
    /// to compile *all* iterator implementations it can find.
    @CompilerDirectives.ValueType
    public record Iter<T>(Iterator<T> inner) {
        @TruffleBoundary
        public boolean hasNext() {
            return inner.hasNext();
        }
        @TruffleBoundary
        public T next() {
            return inner.next();
        }
        @SuppressWarnings("PMD.ShortMethodName")
        public static <T> Iter<T> of(Iterator<T> inner) {
            return new Iter<>(inner);
        }
    }
}
