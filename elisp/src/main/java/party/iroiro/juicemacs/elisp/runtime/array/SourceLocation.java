package party.iroiro.juicemacs.elisp.runtime.array;

public record SourceLocation(int startLine, int startColumn, int endLine, int endColumn) {
    static final int MAX_LINE_NUMBER = (1 << 18) - 1;
    static final int MAX_LINE_DELTA = (1 << 14) - 1;
    static final int MAX_COLUMN_NUMBER = (1 << 14) - 1;

    /// Encodes source location info into 64 bits
    ///
    /// ## Encoding Patterns
    ///
    /// Patterns are denoted by MSBs.
    ///
    /// - `0b1xxx`: only line numbers are encoded (`(endline << 32) | startLine`)
    /// - `0b00xx`: reserved
    /// - `0b0100`: `endCol (14b) | startCol (14b) | deltaLine (14b) | startLine (18b)`
    static long encodeDebugInfo(int startLine, int startColumn, int endLine, int endColumn) {
        int delta = endLine - startLine;
        if (startLine <= MAX_LINE_NUMBER && delta <= MAX_LINE_DELTA) {
            return (
                    (0b0100L << 60)
                            | ((long) (endColumn & MAX_COLUMN_NUMBER) << 46)
                            | ((long) (startColumn & MAX_COLUMN_NUMBER) << 32)
                            | ((long) delta << 18)
                            | startLine
            );
        }
        return startLine | ((long) endLine << 32) | (0b1000L << 60);
    }

    static SourceLocation decodeDebugInfo(long encoded) {
        if ((encoded >> 63) == 0) {
            int startLine = (int) (encoded & MAX_LINE_NUMBER);
            return new SourceLocation(
                    startLine,
                    (int) ((encoded >> 32) & MAX_COLUMN_NUMBER),
                    (int) (startLine + ((encoded >> 18) & MAX_LINE_DELTA)),
                    (int) ((encoded >> 46) & MAX_COLUMN_NUMBER)
            );
        }
        return new SourceLocation(
                (int) encoded, 1,
                (int) (encoded >> 32) & Integer.MAX_VALUE, 1
        );
    }
}
