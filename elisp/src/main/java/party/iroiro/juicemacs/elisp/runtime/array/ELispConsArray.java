package party.iroiro.juicemacs.elisp.runtime.array;

import org.eclipse.jdt.annotation.Nullable;

final class ELispConsArray {
    int size;
    Object array;
    ArrayStrategy strategy;
    Object cdr = false;

    /// Metadata (source location and hash code)
    ///
    /// Used by [WithCdrStrategy].
    int @Nullable [] metadata = null;
    static final int METADATA_HAS_SOURCE_LOCATION = 0x10;

    ELispConsArray(Object array, int size, ArrayStrategy strategy) {
        this.array = array;
        this.size = size;
        this.strategy = strategy;
    }

    public int size() {
        return size;
    }

    private int getDebugMetadata(int i) {
        if (metadata == null) {
            return 0;
        }
        if ((metadata[0] & METADATA_HAS_SOURCE_LOCATION) == 0) {
            return 0;
        }
        return metadata[i];
    }
    public int getStartLine() {
        return getDebugMetadata(1);
    }
    public int getStartColumn() {
        return getDebugMetadata(2);
    }
    public int getEndLine() {
        return getDebugMetadata(3);
    }
    public int getEndColumn() {
        return getDebugMetadata(4);
    }
    public void setSourceLocation(int startLine, int startColumn, int endLine, int endColumn) {
        if (metadata == null) {
            metadata = new int []{
                    METADATA_HAS_SOURCE_LOCATION,
                    startLine, startColumn, endLine, endColumn
            };
        }
    }
}
