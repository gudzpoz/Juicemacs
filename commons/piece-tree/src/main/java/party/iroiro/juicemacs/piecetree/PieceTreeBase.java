/*---------------------------------------------------------------------------------------------
 * Copyright (c) 2024 gudzpoz
 * Copyright (c) 2015 - present Microsoft Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * See LICENSE.piece-tree for license information.
 *--------------------------------------------------------------------------------------------*/

package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.mule.Utf8Utils;

import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

import static party.iroiro.juicemacs.piecetree.TreeNode.*;

/// Transliterated from <a href="https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeBase.ts">
/// pieceTreeBase.ts @ vscode</a>
///
/// Those lowercase `to do` comments are from the original code.
public final class PieceTreeBase {
    public static final int AVERAGE_BUFFER_SIZE = 0x10000;

    /**
     * @param node            Piece Index
     * @param nodeStartOffset node start offset in document.
     * @param remainder       remainder in current piece.
     */
    public record NodePosition(
            TreeNode node,
            long nodeStartOffset,
            int remainder
    ) {
    }

    /**
     * @param line   Line number in current buffer (0-based)
     * @param column Column number in current buffer (0-based)
     */
    public record BufferCursor(int line, int column) {
    }

    public record Piece(
            int bufferIndex,
            BufferCursor start,
            BufferCursor end,
            int lineFeedCnt,
            int length
    ) {
    }

    /* Skipped: class PieceTreeSnapshot */

    /* Moved: class PieceTreeSearchCache -> PieceTreeSearchCache.java */

    TreeNode root = SENTINEL;
    private final ArrayList<StringBuffer> buffers;
    private int lineCnt;
    private long length;
    private final BufferCursor[] lastChangeBufferPos = new BufferCursor[2];
    private final PieceTreeSearchCache searchCache;
    private int lastVisitedLine;
    private byte[] lastVisitedLineValue = StringBuffer.EMPTY_STRING;

    public PieceTreeBase(boolean raw) {
        this(raw, new StringBuffer[0]);
    }

    public PieceTreeBase(boolean raw, byte[] string) {
        this(raw, StringBuffer.mutable(string, raw));
    }

    PieceTreeBase(boolean raw, StringBuffer... chunks) {
        buffers = new ArrayList<>();
        searchCache = new PieceTreeSearchCache();
        create(raw, chunks);
    }

    @TruffleBoundary
    private void create(boolean raw, StringBuffer[] chunks) {
        buffers.clear();
        buffers.add(StringBuffer.mutable(1024, raw));
        buffers.add(StringBuffer.mutable(1024, true));
        lastChangeBufferPos[0] = new BufferCursor(0, 0);
        lastChangeBufferPos[1] = new BufferCursor(0, 0);
        root = SENTINEL;
        lineCnt = 1;
        length = 0;

        TreeNode lastNode = null;
        for (StringBuffer chunk : chunks) {
            if (!chunk.isEmpty()) {
                IndexList lineStarts = chunk.lineStarts();
                Piece piece = new Piece(
                        buffers.size(),
                        new BufferCursor(0, 0),
                        new BufferCursor(
                                lineStarts.size() - 1,
                                chunk.length() - lineStarts.getLast()
                        ),
                        lineStarts.size() - 1,
                        chunk.length()
                );
                buffers.add(chunk);
                lastNode = rbInsertRight(lastNode, piece);
            }
        }

        searchCache.clear();
        lastVisitedLine = 0;
        lastVisitedLineValue = StringBuffer.EMPTY_STRING;
        computeBufferMetadata();
    }

    public void compact() {
        boolean raw = isRaw();
        int averageBufferSize = AVERAGE_BUFFER_SIZE;
        int min = averageBufferSize - averageBufferSize / 3;
        int max = min * 2;
        ByteArrayOutputStream tempChunk = new ByteArrayOutputStream();
        ArrayList<StringBuffer> chunks = new ArrayList<>();
        iterate(root, (node) -> {
            long len = node.piece.length; // estimate only (should be bytes)
            int tempChunkLen = tempChunk.size();
            if (min < tempChunkLen && max <= tempChunkLen + len) {
                // flush anyway
                chunks.add(StringBuffer.immutable(tempChunk.toByteArray(), raw));
                tempChunk.reset();
            }
            getNodeContent(node, tempChunk);
            return true;
        });
        if (tempChunk.size() != 0) {
            chunks.add(StringBuffer.immutable(tempChunk.toByteArray(), raw));
        }
        create(raw, chunks.toArray(new StringBuffer[0]));
    }

    public boolean isRaw() {
        return buffers.getFirst().isRaw();
    }

    //#region Buffer API
    /* Skipped: createSnapshot() */
    /* Skipped: equal() */

    public long getOffsetAt(int lineNumber, long column) {
        long leftLen = 0; // inorder
        TreeNode x = root;
        while (x != SENTINEL) {
            if (x.left != SENTINEL && x.lf_left + 1 >= lineNumber) {
                x = x.left;
            } else if (x.lf_left + x.piece.lineFeedCnt() + 1 >= lineNumber) {
                leftLen += x.size_left;
                // lineNumber >= 2
                long accumulatedValueInCurrentIndex = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                return leftLen + accumulatedValueInCurrentIndex + column - 1;
            } else {
                lineNumber -= x.lf_left + x.piece.lineFeedCnt();
                leftLen += x.size_left + x.piece.length();
                x = x.right;
            }
        }
        return leftLen;
    }

    public Position getPositionAt(long offset) {
        offset = Math.max(0, offset);
        TreeNode x = root;
        int lfCnt = 0;
        final long originalOffset = offset;
        while (x != SENTINEL) {
            if (x.size_left != 0 && x.size_left >= offset) {
                x = x.left;
            } else if (x.size_left + x.piece.length() >= offset) {
                Index out = getIndexOf(x, (int) (offset - x.size_left));
                lfCnt += x.lf_left + out.index;
                if (out.index == 0) {
                    long lineStartOffset = getOffsetAt(lfCnt + 1, 1);
                    int column = Math.toIntExact(originalOffset - lineStartOffset);
                    return new Position(lfCnt + 1, column + 1);
                }
                return new Position(lfCnt + 1, out.remainder + 1);
            } else {
                offset -= x.size_left + x.piece.length();
                lfCnt += x.lf_left + x.piece.lineFeedCnt();
                if (x.right == SENTINEL) {
                    // last node
                    long lineStartOffset = getOffsetAt(lfCnt + 1, 1);
                    int column = Math.toIntExact(originalOffset - offset - lineStartOffset);
                    return new Position(lfCnt + 1, column + 1);
                } else {
                    x = x.right;
                }
            }
        }
        return new Position(1, 1);
    }

    public byte[] getValueInRange(Range range) {
        if (range.startLineNumber == range.endLineNumber && range.startColumn == range.endColumn) {
            return StringBuffer.EMPTY_STRING;
        }
        NodePosition startPosition = nodeAt2(range.startLineNumber, range.startColumn);
        NodePosition endPosition = nodeAt2(range.endLineNumber, range.endColumn);
        return getValueInRange2(startPosition, endPosition);
    }

    public byte[] getValueInRange2(NodePosition startPosition, NodePosition endPosition) {
        TreeNode x = startPosition.node;
        StringBuffer buffer = buffers.get(x.piece.bufferIndex);
        int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
        if (startPosition.node == endPosition.node) {
            return buffer.substring(startOffset + startPosition.remainder, endPosition.remainder - startPosition.remainder);
        }
        ByteArrayOutputStream ret = new ByteArrayOutputStream();
        buffer.substring(startOffset + startPosition.remainder, x.piece.length - startPosition.remainder, ret);
        x = x.next();
        while (x != SENTINEL) {
            buffer = buffers.get(x.piece.bufferIndex);
            startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
            if (x == endPosition.node) {
                buffer.substring(startOffset, endPosition.remainder, ret);
                break;
            } else {
                buffer.substring(startOffset, x.piece.length, ret);
            }
            x = x.next();
        }
        return ret.toByteArray();
    }

    public ArrayList<byte[]> getLinesContent() {
        ArrayList<byte[]> lines = new ArrayList<>();
        ByteArrayOutputStream currentLine = new ByteArrayOutputStream();

        iterate(root, (node) -> {
            if (node == SENTINEL) {
                return true;
            }
            final Piece piece = node.piece;
            int pieceLength = piece.length;
            if (pieceLength == 0) {
                return true;
            }

            final StringBuffer buffer = buffers.get(piece.bufferIndex);
            final IndexList lineStarts = buffer.lineStarts();
            final int pieceStartLine = piece.start().line();
            final int pieceEndLine = piece.end().line();
            int pieceStartOffset = Math.toIntExact(lineStarts.get(pieceStartLine) + piece.start().column());

            if (pieceStartLine == pieceEndLine) {
                // this piece has no new lines
                buffer.substring(pieceStartOffset, pieceLength, currentLine);
                return true;
            }

            // add the text before the first line start in this piece
            buffer.substring(
                    pieceStartOffset,
                    Math.max(0, lineStarts.get(pieceStartLine + 1) - 1 - pieceStartOffset),
                    currentLine
            );
            lines.add(currentLine.toByteArray());

            for (int line = pieceStartLine + 1; line < pieceEndLine; line++) {
                currentLine.reset();
                int start = lineStarts.get(line);
                buffer.substring(start, lineStarts.get(line + 1) - 1 - start, currentLine);
                lines.add(currentLine.toByteArray());
            }
            currentLine.reset();
            buffer.substring(lineStarts.get(pieceEndLine), piece.end().column(), currentLine);
            return true;
        });
        lines.add(currentLine.toByteArray());
        return lines;
    }

    public long getLength() {
        return length;
    }

    public int getLineCount() {
        return lineCnt;
    }

    public byte[] getLineContent(int lineNumber) {
        if (lastVisitedLine == lineNumber) {
            return lastVisitedLineValue;
        }
        lastVisitedLine = lineNumber;
        lastVisitedLineValue = getLineRawContent(lineNumber, lineNumber == lineCnt ? 0 : 1);
        return lastVisitedLineValue;
    }

    private int getCharCode(NodePosition nodePos) {
        if (nodePos.remainder == nodePos.node.piece.length) {
            // the char we want to fetch is at the head of next node.
            TreeNode matchingNode = nodePos.node.next();
            if (matchingNode == SENTINEL) {
                return 0;
            }
            StringBuffer buffer = buffers.get(matchingNode.piece.bufferIndex);
            int startOffset = offsetInBuffer(matchingNode.piece.bufferIndex, matchingNode.piece.start);
            return buffer.charAtSlow(startOffset);
        } else {
            StringBuffer buffer = buffers.get(nodePos.node.piece.bufferIndex);
            int startOffset = offsetInBuffer(nodePos.node.piece.bufferIndex, nodePos.node.piece.start);
            int targetOffset = startOffset + nodePos.remainder;
            return buffer.charAtSlow(targetOffset);
        }
    }

    public int getLineCharCode(int lineNumber, int index) {
        NodePosition nodePos = nodeAt2(lineNumber, index + 1);
        return getCharCode(nodePos);
    }

    public long getLineLength(int lineNumber) {
        if (lineNumber == getLineCount()) {
            long startOffset = getOffsetAt(lineNumber, 1);
            return getLength() - startOffset;
        }
        return getOffsetAt(lineNumber + 1, 1) - getOffsetAt(lineNumber, 1) - 1;
    }

    public int getCharCode(long offset) {
        NodePosition nodePos = nodeAt(offset);
        return getCharCode(nodePos);
    }

    public byte[] getNearestChunk(long offset) {
        NodePosition nodePos = nodeAt(offset);
        if (nodePos.remainder == nodePos.node.piece.length) {
            // the offset is at the head of next node.
            TreeNode matchingNode = nodePos.node.next();
            if (matchingNode == SENTINEL) {
                return StringBuffer.EMPTY_STRING;
            }
            StringBuffer buffer = buffers.get(matchingNode.piece.bufferIndex);
            int startOffset = offsetInBuffer(matchingNode.piece.bufferIndex, matchingNode.piece.start);
            return buffer.substring(startOffset, matchingNode.piece.length);
        } else {
            StringBuffer buffer = buffers.get(nodePos.node.piece.bufferIndex);
            int startOffset = offsetInBuffer(nodePos.node.piece.bufferIndex, nodePos.node.piece.start);
            int targetOffset = startOffset + nodePos.remainder;
            int targetLength = nodePos.node.piece.length - nodePos.remainder;
            return buffer.substring(targetOffset, targetLength);
        }
    }

    /* Skipped: findMatchesInNode */
    /* Skipped: findMatchesLineByLine */
    /* Skipped: _findMatchesInLine */
    //#endregion

    //#region Piece Table
    public void insert(long offset, byte[] value) {
        insert(offset, value, false);
    }
    @TruffleBoundary
    public void insert(long offset, byte[] value, boolean ascii) {
        lastVisitedLine = 0;
        lastVisitedLineValue = StringBuffer.EMPTY_STRING;
        if (root != SENTINEL) {
            NodePosition nodePosition = nodeAt(offset);
            TreeNode node = nodePosition.node;
            int remainder = nodePosition.remainder;
            long nodeStartOffset = nodePosition.nodeStartOffset;
            Piece piece = node.piece;
            int bufferIndex = piece.bufferIndex;
            BufferCursor insertPosInBuffer = positionInBuffer(node, remainder);
            int level = ascii ? 1 : 0;
            if (node.piece.bufferIndex == level &&
                    piece.end.line == lastChangeBufferPos[level].line &&
                    piece.end.column == lastChangeBufferPos[level].column &&
                    (nodeStartOffset + piece.length == offset) &&
                    value.length < AVERAGE_BUFFER_SIZE
            ) {
                // changed buffer
                appendToNode(node, value, ascii);
                computeBufferMetadata();
                return;
            }
            if (nodeStartOffset == offset) {
                insertContentToNodeLeft(value, ascii, node);
                searchCache.validate(offset);
            } else if (nodeStartOffset + node.piece.length > offset) {
                // we are inserting into the middle of a node.
                Piece newRightPiece = new Piece(
                        bufferIndex,
                        insertPosInBuffer,
                        piece.end,
                        getLineFeedCnt(insertPosInBuffer, piece.end),
                        offsetInBuffer(bufferIndex, piece.end) - offsetInBuffer(bufferIndex, insertPosInBuffer)
                );
                // reuse node for content before insertion point.
                deleteNodeTail(node, insertPosInBuffer);
                ArrayList<Piece> newPieces = createNewPieces(value, ascii);
                if (newRightPiece.length > 0) {
                    rbInsertRight(node, newRightPiece);
                }
                TreeNode tmpNode = node;
                for (Piece newPiece : newPieces) {
                    tmpNode = rbInsertRight(tmpNode, newPiece);
                }
            } else {
                insertContentToNodeRight(value, ascii, node);
            }
        } else {
            // insert new node
            ArrayList<Piece> pieces = createNewPieces(value, ascii);
            TreeNode node = rbInsertLeft(null, pieces.getFirst());
            for (int k = 1; k < pieces.size(); k++) {
                node = rbInsertRight(node, pieces.get(k));
            }
        }
        // todo, this is too brutal. Total line feed count should be updated the same way as lf_left.
        computeBufferMetadata();
    }

    @TruffleBoundary
    public void delete(long offset, long cnt) {
        lastVisitedLine = 0;
        lastVisitedLineValue = StringBuffer.EMPTY_STRING;
        if (cnt <= 0 || root == SENTINEL) {
            return;
        }
        NodePosition startPosition = nodeAt(offset);
        NodePosition endPosition = nodeAt(offset + cnt);
        TreeNode startNode = startPosition.node;
        TreeNode endNode = endPosition.node;
        if (startNode == endNode) {
            BufferCursor startSplitPosInBuffer = positionInBuffer(startNode, startPosition.remainder);
            BufferCursor endSplitPosInBuffer = positionInBuffer(startNode, endPosition.remainder);
            if (startPosition.nodeStartOffset == offset) {
                if (cnt == startNode.piece.length) { // delete node
                    rbDelete(this, startNode);
                    computeBufferMetadata();
                    return;
                }
                deleteNodeHead(startNode, endSplitPosInBuffer);
                searchCache.validate(offset);
                computeBufferMetadata();
                return;
            }
            if (startPosition.nodeStartOffset + startNode.piece.length == offset + cnt) {
                deleteNodeTail(startNode, startSplitPosInBuffer);
                computeBufferMetadata();
                return;
            }
            // delete content in the middle, this node will be splitted to nodes
            shrinkNode(startNode, startSplitPosInBuffer, endSplitPosInBuffer);
            computeBufferMetadata();
            return;
        }
        ArrayList<TreeNode> nodesToDel = new ArrayList<>();
        BufferCursor startSplitPosInBuffer = positionInBuffer(startNode, startPosition.remainder);
        deleteNodeTail(startNode, startSplitPosInBuffer);
        searchCache.validate(offset);
        if (startNode.piece.length == 0) {
            nodesToDel.add(startNode);
        }
        // update last touched node
        BufferCursor endSplitPosInBuffer = positionInBuffer(endNode, endPosition.remainder);
        deleteNodeHead(endNode, endSplitPosInBuffer);
        if (endNode.piece.length == 0) {
            nodesToDel.add(endNode);
        }
        // delete nodes in between
        TreeNode secondNode = startNode.next();
        for (TreeNode node = secondNode; node != endNode && node != SENTINEL; node = node.next()) {
            nodesToDel.add(node);
        }
        deleteNodes(nodesToDel);
        computeBufferMetadata();
    }

    private void insertContentToNodeLeft(byte[] value, boolean ascii, TreeNode node) {
        // we are inserting content to the beginning of node
        ArrayList<TreeNode> nodesToDel = new ArrayList<>();
        ArrayList<Piece> newPieces = createNewPieces(value, ascii);
        TreeNode newNode = rbInsertLeft(node, newPieces.getLast());
        for (int k = newPieces.size() - 2; k >= 0; k--) {
            newNode = rbInsertLeft(newNode, newPieces.get(k));
        }
        deleteNodes(nodesToDel);
    }

    private void insertContentToNodeRight(byte[] value, boolean ascii, TreeNode node) {
        // we are inserting to the right of this node.
        ArrayList<Piece> newPieces = createNewPieces(value, ascii);
        node = rbInsertRight(node, newPieces.getFirst());
        for (int k = 1; k < newPieces.size(); k++) {
            node = rbInsertRight(node, newPieces.get(k));
        }
    }

    private BufferCursor positionInBuffer(TreeNode node, int remainder) {
        final Piece piece = node.piece;
        final int bufferIndex = node.piece.bufferIndex;
        final IndexList lineStarts = buffers.get(bufferIndex).lineStarts();
        final int startOffset = lineStarts.get(piece.start.line) + piece.start.column;
        final int offset = startOffset + remainder;
        // binary search offset between startOffset and endOffset
        int low = piece.start.line;
        int high = piece.end.line;
        int mid = 0;
        int midStop;
        int midStart = 0;
        while (low <= high) {
            mid = low + (high - low) / 2;
            midStart = lineStarts.get(mid);
            if (mid == high) {
                break;
            }
            midStop = lineStarts.get(mid + 1);
            if (offset < midStart) {
                high = mid - 1;
            } else if (offset >= midStop) {
                low = mid + 1;
            } else {
                break;
            }
        }
        return new BufferCursor(mid, offset - midStart);
    }

    private int getLineFeedCnt(BufferCursor start, BufferCursor end) {
        // vscode uses \r|\n|\r\n as line separator, while we use \n only
        return end.line - start.line;
    }

    private int offsetInBuffer(int bufferIndex, BufferCursor cursor) {
        IndexList lineStarts = buffers.get(bufferIndex).lineStarts();
        return lineStarts.get(cursor.line) + cursor.column;
    }

    private void deleteNodes(List<TreeNode> nodes) {
        for (TreeNode node : nodes) {
            rbDelete(this, node);
        }
    }

    private ArrayList<Piece> createNewPieces(byte[] text, boolean ascii) {
        int textLength = text.length;
        if (textLength > AVERAGE_BUFFER_SIZE) {
            boolean raw = buffers.getFirst().isRaw();
            // the content is large, operations like substring, charCode becomes slow
            // so here we split it into smaller chunks, just like what we did for CR/LF normalization
            ArrayList<Piece> newPieces = new ArrayList<>(textLength / AVERAGE_BUFFER_SIZE + 1);
            int start = 0;
            while (start < textLength) {
                int remaining = textLength - start;
                int byteLength = Utf8Utils.nearestValidByteOffset(
                        text,
                        raw,
                        start + Math.min(AVERAGE_BUFFER_SIZE, remaining)
                ) - start;
                int charLength = Utf8Utils.countCodepoints(text, raw, start, start + byteLength);
                byte[] splitText = Arrays.copyOfRange(text, start, start + byteLength);
                start += byteLength;
                StringBuffer piece = StringBuffer.immutable(splitText, raw || byteLength == charLength);
                IndexList lineStarts = piece.lineStarts();
                newPieces.add(new Piece(
                        buffers.size(),
                        new BufferCursor(0, 0),
                        new BufferCursor(lineStarts.size() - 1, charLength - lineStarts.getLast()),
                        lineStarts.size() - 1,
                        charLength
                ));
                buffers.add(piece);
            }
            return newPieces;
        }
        Piece newPiece = appendBuffer(text, ascii, lastChangeBufferPos[ascii ? 1 : 0], 0);
        ArrayList<Piece> newPieces = new ArrayList<>(1);
        newPieces.add(newPiece);
        return newPieces;
    }

    @TruffleBoundary // TODO: why native-image reports parsing error?
    private Piece appendBuffer(byte[] text, boolean ascii, BufferCursor start, int extraLength) {
        int level = ascii ? 1 : 0;
        StringBuffer buffer = buffers.get(level);
        int startOffset = buffer.length();
        buffer.append(text);
        int endIndex = buffer.lineStarts().size() - 1;
        int endColumn = buffer.length() - buffer.lineStarts().get(endIndex);
        BufferCursor endPos = new BufferCursor(endIndex, endColumn);
        lastChangeBufferPos[level] = endPos;
        return new Piece(
                level,
                start,
                endPos,
                getLineFeedCnt(start, endPos),
                buffer.length() - startOffset + extraLength
        );
    }

    public byte[] getLinesRawContent() {
        return getContentOfSubTree(root);
    }

    public byte[] getLineRawContent(int lineNumber, int endOffset) {
        TreeNode x = root;
        ByteArrayOutputStream ret = new ByteArrayOutputStream();
        PieceTreeSearchCache.@Nullable CacheEntry cache = searchCache.get2(lineNumber);
        if (cache != null) {
            x = cache.node();
            int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - cache.nodeStartLineNumber() - 1);
            StringBuffer buffer = buffers.get(x.piece.bufferIndex);
            int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
            if (cache.nodeStartLineNumber() + x.piece.lineFeedCnt == lineNumber) {
                buffer.substring(startOffset + prevAccumulatedValue, x.piece.length - prevAccumulatedValue, ret);
            } else {
                int accumulatedValue = getAccumulatedValue(x, lineNumber - cache.nodeStartLineNumber());
                return buffer.substring(startOffset + prevAccumulatedValue, accumulatedValue - endOffset - prevAccumulatedValue);
            }
        } else {
            long nodeStartOffset = 0;
            int originalLineNumber = lineNumber;
            while (x != SENTINEL) {
                if (x.left != SENTINEL && x.lf_left >= lineNumber - 1) {
                    x = x.left;
                } else if (x.lf_left + x.piece.lineFeedCnt > lineNumber - 1) {
                    int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                    int accumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 1);
                    StringBuffer buffer = buffers.get(x.piece.bufferIndex);
                    int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                    nodeStartOffset += x.size_left;
                    searchCache.add(new PieceTreeSearchCache.CacheEntry(x, nodeStartOffset, originalLineNumber - (lineNumber - 1 - x.lf_left)));
                    return buffer.substring(startOffset + prevAccumulatedValue, accumulatedValue - endOffset - prevAccumulatedValue);
                } else if (x.lf_left + x.piece.lineFeedCnt == lineNumber - 1) {
                    int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                    StringBuffer buffer = buffers.get(x.piece.bufferIndex);
                    int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                    ret.reset();
                    buffer.substring(startOffset + prevAccumulatedValue, x.piece.length - prevAccumulatedValue, ret);
                    break;
                } else {
                    lineNumber -= x.lf_left + x.piece.lineFeedCnt;
                    nodeStartOffset += x.size_left + x.piece.length;
                    x = x.right;
                }
            }
        }
        // search in order, to find the node contains end column
        x = x.next();
        while (x != SENTINEL) {
            StringBuffer buffer = buffers.get(x.piece.bufferIndex);
            if (x.piece.lineFeedCnt > 0) {
                int accumulatedValue = getAccumulatedValue(x, 0);
                int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                buffer.substring(startOffset, accumulatedValue - endOffset, ret);
                break;
            } else {
                int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                buffer.substring(startOffset, x.piece.length, ret);
            }
            x = x.next();
        }
        return ret.toByteArray();
    }

    private void computeBufferMetadata() {
        TreeNode x = root;
        int lfCnt = 1;
        long len = 0;
        while (x != SENTINEL) {
            lfCnt += x.lf_left + x.piece.lineFeedCnt;
            len += x.size_left + x.piece.length;
            x = x.right;
        }
        this.lineCnt = lfCnt;
        this.length = len;
        searchCache.validate(len);
    }
    //#endregion

    //#region Node operations
    private Index getIndexOf(TreeNode node, int accumulatedValue) {
        Piece piece = node.piece;
        BufferCursor pos = positionInBuffer(node, accumulatedValue);
        int lineCnt = pos.line - piece.start.line;
        if (offsetInBuffer(piece.bufferIndex, piece.end) - offsetInBuffer(piece.bufferIndex, piece.start) == accumulatedValue) {
            // we are checking the end of this node, so a CRLF check is necessary.
            int realLineCnt = getLineFeedCnt(piece.start, pos);
            if (realLineCnt != lineCnt) {
                return new Index(realLineCnt, 0);
            }
        }
        return new Index(lineCnt, pos.column);
    }

    private int getAccumulatedValue(TreeNode node, int index) {
        if (index < 0) {
            return 0;
        }
        Piece piece = node.piece;
        IndexList lineStarts = buffers.get(piece.bufferIndex()).lineStarts();
        int expectedLineStartIndex = piece.start().line() + index + 1;
        if (expectedLineStartIndex > piece.end().line()) {
            return lineStarts.get(piece.end().line()) + piece.end().column()
                    - lineStarts.get(piece.start().line()) - piece.start().column();
        } else {
            return lineStarts.get(expectedLineStartIndex)
                    - lineStarts.get(piece.start().line()) - piece.start().column();
        }
    }

    private void deleteNodeTail(TreeNode node, BufferCursor pos) {
        Piece piece = node.piece;
        int originalLFCnt = piece.lineFeedCnt;
        long originalEndOffset = offsetInBuffer(piece.bufferIndex, piece.end);
        //noinspection UnnecessaryLocalVariable
        BufferCursor newEnd = pos;
        long newEndOffset = offsetInBuffer(piece.bufferIndex, newEnd);
        int newLineFeedCnt = getLineFeedCnt(piece.start, newEnd);
        int lf_delta = newLineFeedCnt - originalLFCnt;
        long size_delta = newEndOffset - originalEndOffset;
        int newLength = Math.toIntExact(piece.length + size_delta);
        node.piece = new Piece(
                piece.bufferIndex,
                piece.start,
                newEnd,
                newLineFeedCnt,
                newLength
        );
        updateTreeMetadata(this, node, size_delta, lf_delta);
    }

    private void deleteNodeHead(TreeNode node, BufferCursor pos) {
        Piece piece = node.piece;
        int originalLFCnt = piece.lineFeedCnt;
        long originalStartOffset = offsetInBuffer(piece.bufferIndex, piece.start);
        //noinspection UnnecessaryLocalVariable
        BufferCursor newStart = pos;
        int newLineFeedCnt = getLineFeedCnt(newStart, piece.end);
        long newStartOffset = offsetInBuffer(piece.bufferIndex, newStart);
        int lf_delta = newLineFeedCnt - originalLFCnt;
        long size_delta = originalStartOffset - newStartOffset;
        int newLength = Math.toIntExact(piece.length + size_delta);
        node.piece = new Piece(
                piece.bufferIndex,
                newStart,
                piece.end,
                newLineFeedCnt,
                newLength
        );
        updateTreeMetadata(this, node, size_delta, lf_delta);
    }

    private void shrinkNode(TreeNode node, BufferCursor start, BufferCursor end) {
        Piece piece = node.piece;
        BufferCursor originalStartPos = piece.start;
        BufferCursor originalEndPos = piece.end;
        // old piece, originalStartPos, start
        long oldLength = piece.length;
        int oldLFCnt = piece.lineFeedCnt;
        //noinspection UnnecessaryLocalVariable
        BufferCursor newEnd = start;
        int newLineFeedCnt = getLineFeedCnt(piece.start, newEnd);
        int newLength = offsetInBuffer(piece.bufferIndex, start) - offsetInBuffer(piece.bufferIndex, originalStartPos);
        node.piece = new Piece(
                piece.bufferIndex,
                piece.start,
                newEnd,
                newLineFeedCnt,
                newLength
        );
        updateTreeMetadata(this, node, newLength - oldLength, newLineFeedCnt - oldLFCnt);
        // new right piece, end, originalEndPos
        Piece newPiece = new Piece(
                piece.bufferIndex,
                end,
                originalEndPos,
                getLineFeedCnt(end, originalEndPos),
                offsetInBuffer(piece.bufferIndex, originalEndPos) - offsetInBuffer(piece.bufferIndex, end)
        );
        rbInsertRight(node, newPiece);
    }

    private void appendToNode(TreeNode node, byte[] text, boolean ascii) {
        Piece piece = appendBuffer(text, ascii, node.piece.start, node.piece.length);
        int lf_delta = piece.lineFeedCnt - node.piece.lineFeedCnt;
        int size_delta = piece.length - node.piece.length;
        node.piece = piece;
        updateTreeMetadata(this, node, size_delta, lf_delta);
    }

    private NodePosition nodeAt(long offset) {
        TreeNode x = root;
        PieceTreeSearchCache.@Nullable CacheEntry cache = searchCache.get(offset);
        if (cache != null) {
            return new NodePosition(cache.node(), cache.nodeStartOffset(), Math.toIntExact(offset - cache.nodeStartOffset()));
        }
        long nodeStartOffset = 0;
        while (x != SENTINEL) {
            if (x.size_left > offset) {
                x = x.left;
            } else if (x.size_left + x.piece.length >= offset) {
                nodeStartOffset += x.size_left;
                NodePosition ret = new NodePosition(x, nodeStartOffset, Math.toIntExact(offset - x.size_left));
                searchCache.add(new PieceTreeSearchCache.CacheEntry(x, nodeStartOffset, -1));
                return ret;
            } else {
                offset -= x.size_left + x.piece.length;
                nodeStartOffset += x.size_left + x.piece.length;
                x = x.right;
            }
        }
        throw new IllegalArgumentException();
    }

    private NodePosition nodeAt2(int lineNumber, int column) {
        TreeNode x = root;
        long nodeStartOffset = 0;
        while (x != SENTINEL) {
            if (x.left != SENTINEL && x.lf_left >= lineNumber - 1) {
                x = x.left;
            } else if (x.lf_left + x.piece.lineFeedCnt > lineNumber - 1) {
                int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                int accumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 1);
                nodeStartOffset += x.size_left;

                return new NodePosition(x, nodeStartOffset, Math.min(prevAccumulatedValue + column - 1, accumulatedValue));
            } else if (x.lf_left + x.piece.lineFeedCnt == lineNumber - 1) {
                int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                if (prevAccumulatedValue + column - 1 <= x.piece.length) {
                    return new NodePosition(x, nodeStartOffset, prevAccumulatedValue + column - 1);
                } else {
                    column -= x.piece.length - prevAccumulatedValue;
                    break;
                }
            } else {
                lineNumber -= x.lf_left + x.piece.lineFeedCnt;
                nodeStartOffset += x.size_left + x.piece.length;
                x = x.right;
            }
        }
        // search in order, to find the node contains position.column
        x = x.next();
        while (x != SENTINEL) {
            if (x.piece.lineFeedCnt > 0) {
                int accumulatedValue = getAccumulatedValue(x, 0);
                nodeStartOffset = offsetOfNode(x);
                return new NodePosition(x, nodeStartOffset, Math.min(column - 1, accumulatedValue));
            } else {
                if (x.piece.length >= column - 1) {
                    nodeStartOffset = offsetOfNode(x);
                    return new NodePosition(x, nodeStartOffset, column - 1);
                } else {
                    column -= x.piece.length;
                }
            }
            x = x.next();
        }
        throw new IllegalArgumentException();
    }

    private long offsetOfNode(TreeNode node) {
        long pos = node.size_left;
        while (node != root) {
            if (node.parent.right == node) {
                pos += node.parent.size_left + node.parent.piece.length;
            }
            node = node.parent;
        }
        return pos;
    }
    //#endregion

    //#region Tree operations
    @TruffleBoundary
    private boolean iterate(TreeNode node, Predicate<TreeNode> callback) {
        if (node == SENTINEL) {
            return true;
        }
        boolean leftRet = iterate(node.left, callback);
        if (!leftRet) {
            return false;
        }
        return callback.test(node) && iterate(node.right, callback);
    }

    private void getNodeContent(TreeNode node, ByteArrayOutputStream output) {
        if (node == SENTINEL) {
            return;
        }
        StringBuffer buffer = buffers.get(node.piece.bufferIndex());
        Piece piece = node.piece;
        int startOffset = offsetInBuffer(piece.bufferIndex(), piece.start);
        int endOffset = offsetInBuffer(piece.bufferIndex(), piece.end);
        buffer.substring(startOffset, endOffset - startOffset, output);
    }

    private TreeNode rbInsertRight(@Nullable TreeNode node, Piece piece) {
        TreeNode z = new TreeNode(piece, TreeNode.RED);
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.parent = SENTINEL;
        z.size_left = 0;
        z.lf_left = 0;

        TreeNode x = root;
        assert root == SENTINEL || node != null;
        if (x == SENTINEL) {
            root = z;
            z.color = TreeNode.BLACK;
        } else if (node.right == SENTINEL) {
            node.right = z;
            z.parent = node;
        } else {
            TreeNode nextNode = node.right.leftest();
            nextNode.left = z;
            z.parent = nextNode;
        }

        fixInsert(this, z);
        return z;
    }

    private TreeNode rbInsertLeft(@Nullable TreeNode node, Piece piece) {
        TreeNode z = new TreeNode(piece, TreeNode.RED);
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.parent = SENTINEL;
        z.size_left = 0;
        z.lf_left = 0;

        assert root == SENTINEL || node != null;
        if (root == SENTINEL) {
            root = z;
            z.color = TreeNode.BLACK;
        } else if (node.left == SENTINEL) {
            node.left = z;
            z.parent = node;
        } else {
            TreeNode prevNode = node.left.rightest();
            prevNode.right = z;
            z.parent = prevNode;
        }

        fixInsert(this, z);
        return z;
    }

    private byte[] getContentOfSubTree(TreeNode subTree) {
        ByteArrayOutputStream str = new ByteArrayOutputStream();
        iterate(subTree, (node) -> {
            getNodeContent(node, str);
            return true;
        });
        return str.toByteArray();
    }
    //#endregion

    public record Range(int startLineNumber, int startColumn, int endLineNumber, int endColumn) {
    }

    /**
     * @param line   1-based
     * @param column 1-based
     */
    public record Position(int line, int column) {
    }

    private record Index(int index, int remainder) {
    }

    public final class NodeIterator implements PrimitiveIterator.OfInt {
        private TreeNode currentNode;
        private int cachedStartOffset;
        private int current;
        private long remainder;

        public NodeIterator(long start, long end) {
            NodePosition startPosition = nodeAt(start);
            currentNode = startPosition.node;
            cachedStartOffset = -1;
            current = startPosition.remainder;
            remainder = end - start;
        }

        @Override
        public int nextInt() {
            Piece piece = checkPieceHasNext();
            StringBuffer buffer = buffers.get(piece.bufferIndex);
            remainder--;
            return buffer.charAtSlow(cachedStartOffset + current++);
        }

        @Override
        public boolean hasNext() {
            if (remainder <= 0) {
                return false;
            }
            while (currentNode != SENTINEL) {
                Piece piece = currentNode.piece;
                if (current < piece.length()) {
                    return true;
                }
                current = 0;
                cachedStartOffset = -1;
                currentNode = currentNode.next();
            }
            return false;
        }

        public NodePiece nextNode() {
            Piece piece = checkPieceHasNext();
            StringBuffer buffer = buffers.get(piece.bufferIndex);
            int startByte = buffer.charIndexToByteIndex(cachedStartOffset + current);
            int currentRemaining = (int) Math.min(piece.length - current, remainder);
            int endByte = buffer.charIndexToByteIndex(cachedStartOffset + currentRemaining);
            int codePoints = currentRemaining - current;
            current += currentRemaining;
            remainder -= currentRemaining;
            return new NodePiece(buffer.inner(), startByte, endByte, codePoints);
        }

        private Piece checkPieceHasNext() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            // currentNode must have extra chars after calling hasNext
            Piece piece = currentNode.piece;
            if (cachedStartOffset == -1) {
                cachedStartOffset = offsetInBuffer(piece.bufferIndex, piece.start);
            }
            return piece;
        }

        public record NodePiece(byte[] bytes, int startByte, int endByte, int codePoints) {
            public int byteLength() {
                return endByte - startByte;
            }
        }
    }

    public NodeIterator iterator(long start, long end) {
        return new NodeIterator(start, end);
    }

    public IntStream chars() {
        return StreamSupport.intStream(() ->
                        Spliterators.spliterator(
                                new NodeIterator(0, getLength()),
                                getLength(),
                                Spliterator.ORDERED),
                Spliterator.SUBSIZED | Spliterator.SIZED | Spliterator.ORDERED,
                false);
    }
}
