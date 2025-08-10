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
import com.oracle.truffle.api.strings.*;
import org.eclipse.jdt.annotation.Nullable;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

import static party.iroiro.juicemacs.piecetree.StringNodes.*;
import static party.iroiro.juicemacs.piecetree.TreeNode.*;

/// Transliterated from <a href="https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeBase.ts">
/// pieceTreeBase.ts @ vscode</a>
///
/// Those lowercase `to do` comments are from the original code.
@SuppressWarnings({"UnnecessaryLocalVariable"})
public final class PieceTreeBase {
    public static final int AVERAGE_BUFFER_SIZE = 0x10000;
    private static final TruffleString EMPTY_STRING =
            TruffleString.fromJavaStringUncached("", TruffleString.Encoding.UTF_32);

    /* Skipped: class LineStarts */

    @TruffleBoundary // TODO: why native-image reports parsing error?
    static LineStartList createLineStartsFast(AbstractTruffleString str) {
        LineStartList r = new LineStartList();
        r.add(0);
        r.appendLineStarts(str, 0);
        r.trimToSize();
        return r;
    }

    /* Skipped: createLineStarts */

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
    private final BufferCursor[] lastChangeBufferPos = new BufferCursor[3];
    private final PieceTreeSearchCache searchCache;
    private int lastVisitedLine;
    private TruffleString lastVisitedLineValue = EMPTY_STRING;

    public PieceTreeBase() {
        this(new StringBuffer[0]);
    }

    public PieceTreeBase(TruffleString string) {
        this(new StringBuffer(string));
    }

    PieceTreeBase(StringBuffer... chunks) {
        buffers = new ArrayList<>();
        searchCache = new PieceTreeSearchCache();
        create(chunks);
    }

    @TruffleBoundary
    private void create(StringBuffer[] chunks) {
        buffers.clear();
        buffers.add(new StringBuffer(TruffleString.CompactionLevel.S1));
        buffers.add(new StringBuffer(TruffleString.CompactionLevel.S2));
        buffers.add(new StringBuffer(TruffleString.CompactionLevel.S4));
        lastChangeBufferPos[0] = new BufferCursor(0, 1);
        lastChangeBufferPos[1] = new BufferCursor(0, 1);
        lastChangeBufferPos[2] = new BufferCursor(0, 1);
        root = SENTINEL;
        lineCnt = 1;
        length = 0;

        TreeNode lastNode = null;
        for (int i = 0, len = chunks.length; i < len; i++) {
            if (!chunks[i].isEmpty()) {
                LineStartList lineStarts = chunks[i].lineStarts();
                Piece piece = new Piece(
                        i + 3,
                        new BufferCursor(0, 0),
                        new BufferCursor(
                                lineStarts.size() - 1,
                                chunks[i].length() - lineStarts.getLast()
                        ),
                        lineStarts.size() - 1,
                        chunks[i].length()
                );
                buffers.add(chunks[i]);
                //noinspection DataFlowIssue: When root is SENTINEL, lastNode being null is fine.
                lastNode = rbInsertRight(lastNode, piece);
            }
        }

        searchCache.clear();
        lastVisitedLine = 0;
        lastVisitedLineValue = EMPTY_STRING;
        computeBufferMetadata();
    }

    public void compact() {
        int averageBufferSize = AVERAGE_BUFFER_SIZE;
        int min = averageBufferSize - averageBufferSize / 3;
        int max = min * 2;
        TruffleStringBuilderUTF32[] tempChunk = {TruffleStringBuilder.createUTF32(max)};
        ArrayList<StringBuffer> chunks = new ArrayList<>();
        iterate(root, (node) -> {
            TruffleStringBuilderUTF32 sb = TruffleStringBuilder.createUTF32();
            getNodeContent(node, sb);
            TruffleString str = buildString(sb);
            long len = length(str);
            int tempChunkLen = tempChunk[0].byteLength() >> 2;
            if (min < tempChunkLen && max <= tempChunkLen + len) {
                // flush anyway
                chunks.add(new StringBuffer(buildString(tempChunk[0])));
                tempChunk[0] = TruffleStringBuilder.createUTF32(max);
            }
            append(tempChunk[0], str);
            return true;
        });
        if (!tempChunk[0].isEmpty()) {
            chunks.add(new StringBuffer(buildString(tempChunk[0])));
        }
        create(chunks.toArray(new StringBuffer[0]));
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

    public TruffleString getValueInRange(Range range) {
        if (range.startLineNumber == range.endLineNumber && range.startColumn == range.endColumn) {
            return EMPTY_STRING;
        }
        NodePosition startPosition = nodeAt2(range.startLineNumber, range.startColumn);
        NodePosition endPosition = nodeAt2(range.endLineNumber, range.endColumn);
        return getValueInRange2(startPosition, endPosition);
    }

    public TruffleString getValueInRange2(NodePosition startPosition, NodePosition endPosition) {
        TreeNode x = startPosition.node;
        StringBuffer buffer = buffers.get(x.piece.bufferIndex);
        int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
        if (startPosition.node == endPosition.node) {
            return buffer.substring(startOffset + startPosition.remainder, endPosition.remainder - startPosition.remainder);
        }
        TruffleStringBuilderUTF32 ret = TruffleStringBuilder.createUTF32();
        append(ret, buffer.substring(startOffset + startPosition.remainder, x.piece.length() - startPosition.remainder));
        x = x.next();
        while (x != SENTINEL) {
            buffer = buffers.get(x.piece.bufferIndex);
            startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
            if (x == endPosition.node) {
                append(ret, buffer.substring(startOffset, endPosition.remainder));
                break;
            } else {
                append(ret, buffer.substring(startOffset, x.piece.length()));
            }
            x = x.next();
        }
        return buildString(ret);
    }

    public ArrayList<TruffleString> getLinesContent() {
        ArrayList<TruffleString> lines = new ArrayList<>();
        TruffleStringBuilderUTF32[] currentLine = {TruffleStringBuilder.createUTF32()};

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
            final LineStartList lineStarts = buffer.lineStarts();
            final int pieceStartLine = piece.start().line();
            final int pieceEndLine = piece.end().line();
            int pieceStartOffset = Math.toIntExact(lineStarts.get(pieceStartLine) + piece.start().column());

            if (pieceStartLine == pieceEndLine) {
                // this piece has no new lines
                append(currentLine[0], buffer.substring(pieceStartOffset, pieceLength));
                return true;
            }

            // add the text before the first line start in this piece
            append(currentLine[0], buffer.substring(
                    pieceStartOffset,
                    Math.max(0, lineStarts.get(pieceStartLine + 1) - 1 - pieceStartOffset)
            ));
            lines.add(buildString(currentLine[0]));

            for (int line = pieceStartLine + 1; line < pieceEndLine; line++) {
                currentLine[0] = TruffleStringBuilder.createUTF32();
                int start = lineStarts.get(line);
                append(currentLine[0], buffer.substring(start, lineStarts.get(line + 1) - 1 - start));
                lines.add(buildString(currentLine[0]));
            }
            currentLine[0] = TruffleStringBuilder.createUTF32();
            append(currentLine[0], buffer.substring(lineStarts.get(pieceEndLine), piece.end().column()));
            return true;
        });
        lines.add(buildString(currentLine[0]));
        return lines;
    }

    public long getLength() {
        return length;
    }

    public int getLineCount() {
        return lineCnt;
    }

    public TruffleString getLineContent(int lineNumber) {
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
            return buffer.charAt(startOffset);
        } else {
            StringBuffer buffer = buffers.get(nodePos.node.piece.bufferIndex);
            int startOffset = offsetInBuffer(nodePos.node.piece.bufferIndex, nodePos.node.piece.start);
            int targetOffset = startOffset + nodePos.remainder;
            return buffer.charAt(targetOffset);
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

    public TruffleString getNearestChunk(long offset) {
        NodePosition nodePos = nodeAt(offset);
        if (nodePos.remainder == nodePos.node.piece.length) {
            // the offset is at the head of next node.
            TreeNode matchingNode = nodePos.node.next();
            if (matchingNode == SENTINEL) {
                return EMPTY_STRING;
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
    public void insert(long offset, AbstractTruffleString value) {
        lastVisitedLine = 0;
        lastVisitedLineValue = EMPTY_STRING;
        int level = value.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32).getLog2();
        if (root != SENTINEL) {
            NodePosition nodePosition = nodeAt(offset);
            TreeNode node = nodePosition.node;
            int remainder = nodePosition.remainder;
            long nodeStartOffset = nodePosition.nodeStartOffset;
            Piece piece = node.piece;
            int bufferIndex = piece.bufferIndex;
            BufferCursor insertPosInBuffer = positionInBuffer(node, remainder);
            if (node.piece.bufferIndex == level &&
                    piece.end.line == lastChangeBufferPos[level].line &&
                    piece.end.column == lastChangeBufferPos[level].column &&
                    (nodeStartOffset + piece.length == offset) &&
                    length(value) < AVERAGE_BUFFER_SIZE
            ) {
                // changed buffer
                appendToNode(node, value);
                computeBufferMetadata();
                return;
            }
            if (nodeStartOffset == offset) {
                insertContentToNodeLeft(value, node);
                searchCache.validate(offset);
            } else if (nodeStartOffset + node.piece.length > offset) {
                // we are inserting into the middle of a node.
                Piece newRightPiece = new Piece(
                        bufferIndex,
                        insertPosInBuffer,
                        piece.end,
                        getLineFeedCnt(piece.bufferIndex, insertPosInBuffer, piece.end),
                        offsetInBuffer(bufferIndex, piece.end) - offsetInBuffer(bufferIndex, insertPosInBuffer)
                );
                // reuse node for content before insertion point.
                deleteNodeTail(node, insertPosInBuffer);
                ArrayList<Piece> newPieces = createNewPieces(value);
                if (newRightPiece.length > 0) {
                    rbInsertRight(node, newRightPiece);
                }
                TreeNode tmpNode = node;
                for (Piece newPiece : newPieces) {
                    tmpNode = rbInsertRight(tmpNode, newPiece);
                }
            } else {
                insertContentToNodeRight(value, node);
            }
        } else {
            // insert new node
            ArrayList<Piece> pieces = createNewPieces(value);
            //noinspection DataFlowIssue: OK when root == SENTINEL
            TreeNode node = rbInsertLeft(null, pieces.getFirst());
            for (int k = 1; k < pieces.size(); k++) {
                node = rbInsertRight(node, pieces.get(k));
            }
        }
        // todo, this is too brutal. Total line feed count should be updated the same way as lf_left.
        computeBufferMetadata();
    }

    public void delete(long offset, long cnt) {
        lastVisitedLine = 0;
        lastVisitedLineValue = EMPTY_STRING;
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

    private void insertContentToNodeLeft(AbstractTruffleString value, TreeNode node) {
        // we are inserting content to the beginning of node
        ArrayList<TreeNode> nodesToDel = new ArrayList<>();
        ArrayList<Piece> newPieces = createNewPieces(value);
        TreeNode newNode = rbInsertLeft(node, newPieces.getLast());
        for (int k = newPieces.size() - 2; k >= 0; k--) {
            newNode = rbInsertLeft(newNode, newPieces.get(k));
        }
        deleteNodes(nodesToDel);
    }

    private void insertContentToNodeRight(AbstractTruffleString value, TreeNode node) {
        // we are inserting to the right of this node.
        ArrayList<Piece> newPieces = createNewPieces(value);
        TreeNode newNode = rbInsertRight(node, newPieces.getFirst());
        TreeNode tmpNode = newNode;
        for (int k = 1; k < newPieces.size(); k++) {
            tmpNode = rbInsertRight(tmpNode, newPieces.get(k));
        }
    }

    private BufferCursor positionInBuffer(TreeNode node, int remainder) {
        final Piece piece = node.piece;
        final int bufferIndex = node.piece.bufferIndex;
        final LineStartList lineStarts = buffers.get(bufferIndex).lineStarts();
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

    private int getLineFeedCnt(int bufferIndex, BufferCursor start, BufferCursor end) {
        // we don't need to worry about start: abc\r|\n, or abc|\r, or abc|\n, or abc|\r\n doesn't change the fact that, there is one line break after start.
        // now let's take care of end: abc\r|\n, if end is in between \r and \n, we need to add line feed count by 1
        if (end.column == 0) {
            return end.line - start.line;
        }
        LineStartList lineStarts = buffers.get(bufferIndex).lineStarts();
        if (end.line == lineStarts.size() - 1) { // it means, there is no \n after end, otherwise, there will be one more lineStart.
            return end.line - start.line;
        }
        long nextLineStartOffset = lineStarts.get(end.line + 1);
        long endOffset = lineStarts.get(end.line) + end.column;
        if (nextLineStartOffset > endOffset + 1) { // there are more than 1 character after end, which means it can't be \n
            return end.line - start.line;
        }
        // endOffset + 1 == nextLineStartOffset
        // character at endOffset is \n, so we check the character before first
        // if character at endOffset is \r, end.column is 0 and we can't get here.
        return end.line - start.line;
    }

    private int offsetInBuffer(int bufferIndex, BufferCursor cursor) {
        LineStartList lineStarts = buffers.get(bufferIndex).lineStarts();
        return lineStarts.get(cursor.line) + cursor.column;
    }

    private void deleteNodes(List<TreeNode> nodes) {
        for (TreeNode node : nodes) {
            rbDelete(this, node);
        }
    }

    private ArrayList<Piece> createNewPieces(AbstractTruffleString text) {
        int textLength = length(text);
        if (textLength > AVERAGE_BUFFER_SIZE) {
            // the content is large, operations like substring, charCode becomes slow
            // so here we split it into smaller chunks, just like what we did for CR/LF normalization
            ArrayList<Piece> newPieces = new ArrayList<>(textLength / AVERAGE_BUFFER_SIZE + 1);
            int start = 0;
            while (start < textLength) {
                int remaining = textLength - start;
                int length = Math.min(AVERAGE_BUFFER_SIZE, remaining);
                TruffleString splitText = substring(text, start, length);
                start += length;
                LineStartList lineStarts = createLineStartsFast(splitText);
                newPieces.add(new Piece(
                        buffers.size(),
                        new BufferCursor(0, 0),
                        new BufferCursor(lineStarts.size() - 1, length - lineStarts.getLast()),
                        lineStarts.size() - 1,
                        length
                ));
                buffers.add(new StringBuffer(splitText, lineStarts));
            }
            return newPieces;
        }
        int level = text.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32).getLog2();
        Piece newPiece = appendBuffer(text, lastChangeBufferPos[level], level, 0);
        ArrayList<Piece> newPieces = new ArrayList<>(1);
        newPieces.add(newPiece);
        return newPieces;
    }

    @TruffleBoundary // TODO: why native-image reports parsing error?
    private Piece appendBuffer(AbstractTruffleString text, BufferCursor start, int level, int extraLength) {
        StringBuffer buffer = buffers.get(level);
        int startOffset = buffer.length();
        LineStartList appendLines = new LineStartList();
        appendLines.appendLineStarts(text, startOffset);
        buffer.lineStarts().addAll(appendLines);
        buffer = buffer.append(text, level);
        buffers.set(level, buffer);
        int endIndex = buffer.lineStarts().size() - 1;
        int endColumn = buffer.length() - buffer.lineStarts().get(endIndex);
        BufferCursor endPos = new BufferCursor(endIndex, endColumn);
        lastChangeBufferPos[level] = endPos;
        return new Piece(
                level,
                start,
                endPos,
                getLineFeedCnt(level, start, endPos),
                buffer.length() - startOffset + extraLength
        );
    }

    public TruffleString getLinesRawContent() {
        return getContentOfSubTree(root);
    }

    public TruffleString getLineRawContent(int lineNumber, int endOffset) {
        TreeNode x = root;
        TruffleStringBuilderUTF32 ret = TruffleStringBuilder.createUTF32();
        PieceTreeSearchCache.@Nullable CacheEntry cache = searchCache.get2(lineNumber);
        if (cache != null) {
            x = cache.node();
            int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - cache.nodeStartLineNumber() - 1);
            StringBuffer buffer = buffers.get(x.piece.bufferIndex);
            int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
            if (cache.nodeStartLineNumber() + x.piece.lineFeedCnt == lineNumber) {
                append(ret, buffer.substring(startOffset + prevAccumulatedValue, x.piece.length - prevAccumulatedValue));
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
                    ret = TruffleStringBuilder.createUTF32();
                    append(ret, buffer.substring(startOffset + prevAccumulatedValue, x.piece.length - prevAccumulatedValue));
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
                append(ret, buffer.substring(startOffset, accumulatedValue - endOffset));
                break;
            } else {
                int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                append(ret, buffer.substring(startOffset, x.piece.length));
            }
            x = x.next();
        }
        return buildString(ret);
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
            int realLineCnt = getLineFeedCnt(node.piece.bufferIndex, piece.start, pos);
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
        LineStartList lineStarts = buffers.get(piece.bufferIndex()).lineStarts();
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
        int newLineFeedCnt = getLineFeedCnt(piece.bufferIndex, piece.start, newEnd);
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
        int newLineFeedCnt = getLineFeedCnt(piece.bufferIndex, newStart, piece.end);
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
        int newLineFeedCnt = getLineFeedCnt(piece.bufferIndex, piece.start, newEnd);
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
                getLineFeedCnt(piece.bufferIndex, end, originalEndPos),
                offsetInBuffer(piece.bufferIndex, originalEndPos) - offsetInBuffer(piece.bufferIndex, end)
        );
        rbInsertRight(node, newPiece);
    }

    private void appendToNode(TreeNode node, AbstractTruffleString text) {
        assert text.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32).getLog2() == node.piece.bufferIndex;
        Piece piece = appendBuffer(text, node.piece.start, node.piece.bufferIndex, node.piece.length);
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
            return callback.test(SENTINEL);
        }
        boolean leftRet = iterate(node.left, callback);
        if (!leftRet) {
            return false;
        }
        return callback.test(node) && iterate(node.right, callback);
    }

    private void getNodeContent(TreeNode node, TruffleStringBuilderUTF32 output) {
        if (node == SENTINEL) {
            return;
        }
        StringBuffer buffer = buffers.get(node.piece.bufferIndex());
        Piece piece = node.piece;
        int startOffset = offsetInBuffer(piece.bufferIndex(), piece.start);
        int endOffset = offsetInBuffer(piece.bufferIndex(), piece.end);
        append(output, buffer.substring(startOffset, endOffset - startOffset));
    }

    private TreeNode rbInsertRight(TreeNode node, Piece piece) {
        TreeNode z = new TreeNode(piece, TreeNode.RED);
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.parent = SENTINEL;
        z.size_left = 0;
        z.lf_left = 0;

        TreeNode x = root;
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

    private TreeNode rbInsertLeft(@SuppressWarnings("NullableProblems") TreeNode node, Piece piece) {
        TreeNode z = new TreeNode(piece, TreeNode.RED);
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.parent = SENTINEL;
        z.size_left = 0;
        z.lf_left = 0;

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

    private TruffleString getContentOfSubTree(TreeNode subTree) {
        TruffleStringBuilderUTF32 str = TruffleStringBuilder.createUTF32();
        iterate(subTree, (node) -> {
            getNodeContent(node, str);
            return true;
        });
        return buildString(str);
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

    private class CharIterator implements PrimitiveIterator.OfInt {
        private TreeNode currentNode;
        private int cachedStartOffset;
        private int current;
        private long remainder;

        public CharIterator(long start, long end) {
            NodePosition startPosition = nodeAt(start);
            currentNode = startPosition.node;
            cachedStartOffset = -1;
            current = startPosition.remainder;
            remainder = end - start;
        }

        @Override
        public int nextInt() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            // currentNode must have extra chars after calling hasNext
            Piece piece = currentNode.piece;
            if (cachedStartOffset == -1) {
                cachedStartOffset = offsetInBuffer(piece.bufferIndex, piece.start);
            }
            StringBuffer buffer = buffers.get(piece.bufferIndex);
            remainder--;
            return buffer.charAt(cachedStartOffset + current++);
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
    }

    public PrimitiveIterator.OfInt iterator(long start, long end) {
        return new CharIterator(start, end);
    }

    public IntStream chars() {
        return StreamSupport.intStream(() ->
                        Spliterators.spliterator(
                                new CharIterator(0, getLength()),
                                getLength(),
                                Spliterator.ORDERED),
                Spliterator.SUBSIZED | Spliterator.SIZED | Spliterator.ORDERED,
                false);
    }
}
