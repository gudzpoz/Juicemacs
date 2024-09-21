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

import org.eclipse.jdt.annotation.Nullable;

import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

import static party.iroiro.juicemacs.piecetree.TreeNode.*;

/**
 * Transliterated from <a href="https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeBase.ts">
 * pieceTreeBase.ts @ vscode</a>
 */
@SuppressWarnings({"UnnecessaryLocalVariable", "ExtractMethodRecommender"})
public final class PieceTreeBase implements CharSequence {
    public static final int AVERAGE_BUFFER_SIZE = 65535;

    /* Skipped: class LineStarts */

    static OrderedIntArrayList createLineStartsFast(String str, boolean readonly) {
        OrderedIntArrayList.LazyArrayList r = OrderedIntArrayList.ofLazy(str.length() / 80, str.length());
        r.set(0, 0);
        int rLength = 1;
        for (int i = 0, len = str.length(); i < len; i++) {
            char chr = str.charAt(i);
            if (chr == '\r') {
                if (i + 1 < len && str.charAt(i + 1) == '\n') {
                    // \r\n... case
                    r.set(rLength++, i + 2);
                    i++; // skip \n
                } else {
                    // \r... case
                    r.set(rLength++, i + 1);
                }
            } else if (chr == '\n') {
                r.set(rLength++, i + 1);
            }
        }
        return readonly ? OrderedIntArrayList.ofConstant(r) : r;
    }

    /* Skipped: createLineStarts */

    /**
     * @param node            Piece Index
     * @param remainder       remainder in current piece.
     * @param nodeStartOffset node start offset in document.
     */
    public record NodePosition(
            TreeNode node,
            int nodeStartOffset,
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

    public static final class StringBuffer {
        public StringBuilder buffer;
        public OrderedIntArrayList lineStarts;

        public StringBuffer(String buffer, boolean readonly) {
            this(buffer, createLineStartsFast(buffer, readonly));
        }

        public StringBuffer(String buffer, OrderedIntArrayList lineStarts) {
            this.buffer = new StringBuilder(buffer);
            this.lineStarts = lineStarts;
        }
    }

    /* Skipped: class PieceTreeSnapshot */

    /* Moved: class PieceTreeSearchCache -> PieceTreeSearchCache.java */

    TreeNode root = SENTINEL;
    private final List<StringBuffer> buffers;
    private int lineCnt;
    private int length;
    private EndOfLine EOL = EndOfLine.LF;
    private boolean EOLNormalized;
    private BufferCursor lastChangeBufferPos = new BufferCursor(0, 0);
    private final PieceTreeSearchCache searchCache;
    private int lastVisitedLine;
    private String lastVisitedLineValue = "";

    public PieceTreeBase(List<StringBuffer> chunks, EndOfLine EOL, boolean EOLNormalized) {
        buffers = new ArrayList<>();
        searchCache = new PieceTreeSearchCache(1);
        create(chunks, EOL, EOLNormalized);
    }

    private void create(List<StringBuffer> chunks, EndOfLine eol, boolean eolNormalized) {
        buffers.clear();
        buffers.add(new StringBuffer("", false));
        lastChangeBufferPos = new BufferCursor(0, 0);
        root = SENTINEL;
        lineCnt = 1;
        length = 0;
        this.EOL = eol;
        this.EOLNormalized = eolNormalized;

        TreeNode lastNode = null;
        for (int i = 0, len = chunks.size(); i < len; i++) {
            if (!chunks.get(i).buffer.isEmpty()) {
                Piece piece = new Piece(
                        i + 1,
                        new BufferCursor(0, 0),
                        new BufferCursor(
                                chunks.get(i).lineStarts.size() - 1,
                                chunks.get(i).buffer.length() - chunks.get(i).lineStarts.getLast()
                        ),
                        chunks.get(i).lineStarts.size() - 1,
                        chunks.get(i).buffer.length()
                );
                buffers.add(chunks.get(i));
                //noinspection DataFlowIssue: When root is SENTINEL, lastNode being null is fine.
                lastNode = rbInsertRight(lastNode, piece);
            }
        }

        searchCache.clear();
        lastVisitedLine = 0;
        lastVisitedLineValue = "";
        computeBufferMetadata();
    }

    private void normalizeEOL(EndOfLine eol) {
        int averageBufferSize = AVERAGE_BUFFER_SIZE;
        int min = averageBufferSize - averageBufferSize / 3;
        int max = min * 2;
        StringBuilder tempChunk = new StringBuilder();
        List<StringBuffer> chunks = new ArrayList<>();
        iterate(root, (node) -> {
            StringBuilder sb = new StringBuilder();
            getNodeContent(node, sb);
            String str = sb.toString();
            int len = str.length();
            if (min < tempChunk.length() && max <= tempChunk.length() + len) {
                // flush anyways
                chunks.add(new StringBuffer(tempChunk.toString(), true));
                tempChunk.setLength(0);
            }
            PrimitiveIterator.OfInt i = str.chars().iterator();
            while (i.hasNext()) {
                char c = (char) i.nextInt();
                if (c == '\r') {
                    tempChunk.append(eol);
                    if (i.hasNext()) {
                        c = (char) i.nextInt();
                        if (c != '\n') {
                            tempChunk.append(c);
                        }
                    }
                } else if (c == '\n') {
                    tempChunk.append(eol);
                } else {
                    tempChunk.append(c);
                }
            }
            return true;
        });
        if (!tempChunk.isEmpty()) {
            chunks.add(new StringBuffer(tempChunk.toString(), true));
        }
        create(chunks, eol, true);
    }

    //#region Buffer API
    public EndOfLine getEOL() {
        return EOL;
    }

    public void setEOL(EndOfLine EOL) {
        this.EOL = EOL;
        normalizeEOL(EOL);
    }
    /* Skipped: createSnapshot() */
    /* Skipped: equal() */

    public int getOffsetAt(int lineNumber, int column) {
        int leftLen = 0; // inorder
        TreeNode x = root;
        while (x != SENTINEL) {
            if (x.left != SENTINEL && x.lf_left + 1 >= lineNumber) {
                x = x.left;
            } else if (x.lf_left + x.piece.lineFeedCnt() + 1 >= lineNumber) {
                leftLen += x.size_left;
                // lineNumber >= 2
                int accumulatedValueInCurrentIndex = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                return leftLen + accumulatedValueInCurrentIndex + column - 1;
            } else {
                lineNumber -= x.lf_left + x.piece.lineFeedCnt();
                leftLen += x.size_left + x.piece.length();
                x = x.right;
            }
        }
        return leftLen;
    }

    public Position getPositionAt(int offset) {
        offset = Math.max(0, offset);
        TreeNode x = root;
        int lfCnt = 0;
        final int originalOffset = offset;
        while (x != SENTINEL) {
            if (x.size_left != 0 && x.size_left >= offset) {
                x = x.left;
            } else if (x.size_left + x.piece.length() >= offset) {
                Index out = getIndexOf(x, offset - x.size_left);
                lfCnt += x.lf_left + out.index;
                if (out.index == 0) {
                    int lineStartOffset = getOffsetAt(lfCnt + 1, 1);
                    int column = originalOffset - lineStartOffset;
                    return new Position(lfCnt + 1, column + 1);
                }
                return new Position(lfCnt + 1, out.remainder + 1);
            } else {
                offset -= x.size_left + x.piece.length();
                lfCnt += x.lf_left + x.piece.lineFeedCnt();
                if (x.right == SENTINEL) {
                    // last node
                    int lineStartOffset = getOffsetAt(lfCnt + 1, 1);
                    int column = originalOffset - offset - lineStartOffset;
                    return new Position(lfCnt + 1, column + 1);
                } else {
                    x = x.right;
                }
            }
        }
        return new Position(1, 1);
    }

    private final static Pattern EOL_PATTERN = Pattern.compile("\r\n|\r|\n");

    public String getValueInRange(Range range, @Nullable EndOfLine eol) {
        if (range.startLineNumber == range.endLineNumber && range.startColumn == range.endColumn) {
            return "";
        }
        NodePosition startPosition = nodeAt2(range.startLineNumber, range.startColumn);
        NodePosition endPosition = nodeAt2(range.endLineNumber, range.endColumn);
        String value = getValueInRange2(startPosition, endPosition);
        if (eol != null) {
            if (eol != this.EOL || !EOLNormalized) {
                return EOL_PATTERN.matcher(value).replaceAll(eol.eol());
            }
        }
        return value;
    }

    public String getValueInRange2(NodePosition startPosition, NodePosition endPosition) {
        if (startPosition.node == endPosition.node) {
            TreeNode node = startPosition.node;
            StringBuilder buffer = buffers.get(node.piece.bufferIndex).buffer;
            int startOffset = offsetInBuffer(node.piece.bufferIndex, node.piece.start);
            return buffer.substring(startOffset + startPosition.remainder, startOffset + endPosition.remainder);
        }
        TreeNode x = startPosition.node;
        StringBuilder buffer = buffers.get(x.piece.bufferIndex).buffer;
        int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
        StringBuilder ret = new StringBuilder();
        ret.append(buffer, startOffset + startPosition.remainder, startOffset + x.piece.length());
        x = x.next();
        while (x != SENTINEL) {
            buffer = buffers.get(x.piece.bufferIndex).buffer;
            startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
            if (x == endPosition.node) {
                ret.append(buffer, startOffset, startOffset + endPosition.remainder);
                break;
            } else {
                ret.append(buffer, startOffset, startOffset + x.piece.length());
            }
            x = x.next();
        }
        return ret.toString();
    }

    private static void trimEol(StringBuilder builder) {
        if (!builder.isEmpty() && builder.charAt(builder.length() - 1) == '\n') {
            builder.setLength(builder.length() - 1);
        }
        if (!builder.isEmpty() && builder.charAt(builder.length() - 1) == '\r') {
            builder.setLength(builder.length() - 1);
        }
    }

    public List<String> getLinesContent() {
        List<String> lines = new ArrayList<>();
        final StringBuilder currentLine = new StringBuilder();
        final boolean[] danglingCR = {false};

        iterate(root, (node) -> {
            if (node == SENTINEL) {
                return true;
            }
            final Piece piece = node.piece;
            int pieceLength = piece.length;
            if (pieceLength == 0) {
                return true;
            }

            final StringBuilder buffer = buffers.get(piece.bufferIndex).buffer;
            final OrderedIntArrayList lineStarts = buffers.get(piece.bufferIndex).lineStarts;
            final int pieceStartLine = piece.start().line();
            final int pieceEndLine = piece.end().line();
            int pieceStartOffset = lineStarts.get(pieceStartLine) + piece.start().column();
            if (danglingCR[0]) {
                if (buffer.charAt(pieceStartOffset) == '\n') {
                    pieceStartOffset++;
                    pieceLength--;
                }
                lines.add(currentLine.toString());
                currentLine.setLength(0);
                danglingCR[0] = false;
                if (pieceLength == 0) {
                    return true;
                }
            }

            if (pieceStartLine == pieceEndLine) {
                // this piece has no new lines
                if (!EOLNormalized && buffer.charAt(pieceStartOffset + pieceLength - 1) == '\r') {
                    danglingCR[0] = true;
                    currentLine.append(buffer, pieceStartOffset, pieceStartOffset + pieceLength - 1);
                } else {
                    currentLine.append(buffer, pieceStartOffset, pieceStartOffset + pieceLength);
                }
                return true;
            }

            // add the text before the first line start in this piece
            if (EOLNormalized) {
                currentLine.append(
                        buffer,
                        pieceStartOffset,
                        Math.max(pieceStartOffset, lineStarts.get(pieceStartLine + 1) - EOL.length())
                );
            } else {
                trimEol(currentLine.append(buffer, pieceStartOffset, lineStarts.get(pieceStartLine + 1)));
            }
            lines.add(currentLine.toString());

            for (int line = pieceStartLine + 1; line < pieceEndLine; line++) {
                currentLine.setLength(0);
                if (EOLNormalized) {
                    currentLine.append(buffer, lineStarts.get(line), lineStarts.get(line + 1) - EOL.length());
                } else {
                    currentLine.append(buffer, lineStarts.get(line), lineStarts.get(line + 1));
                    trimEol(currentLine);
                }
                lines.add(currentLine.toString());
            }
            if (!EOLNormalized && buffer.charAt(lineStarts.get(pieceEndLine) + piece.end().column() - 1) == '\r') {
                danglingCR[0] = true;
                if (piece.end().column() == 0) {
                    // The last line ended with a \r, let's undo the push, it will be pushed by next iteration
                    lines.removeLast();
                } else {
                    currentLine.setLength(0);
                    currentLine.append(buffer, lineStarts.get(pieceEndLine), lineStarts.get(pieceEndLine) + piece.end().column() - 1);
                }
            } else {
                currentLine.setLength(0);
                currentLine.append(buffer, lineStarts.get(pieceEndLine), lineStarts.get(pieceEndLine) + piece.end().column());
            }
            return true;
        });
        if (danglingCR[0]) {
            lines.add(currentLine.toString());
            currentLine.setLength(0);
        }
        lines.add(currentLine.toString());
        return lines;
    }

    public int getLength() {
        return length;
    }

    public int getLineCount() {
        return lineCnt;
    }

    public String getLineContent(int lineNumber) {
        if (lastVisitedLine == lineNumber) {
            return lastVisitedLineValue;
        }
        lastVisitedLine = lineNumber;
        if (lineNumber == lineCnt) {
            lastVisitedLineValue = getLineRawContent(lineNumber, 0);
        } else if (EOLNormalized) {
            lastVisitedLineValue = getLineRawContent(lineNumber, EOL.length());
        } else {
            StringBuilder builder = new StringBuilder(getLineRawContent(lineNumber, 0));
            trimEol(builder);
            lastVisitedLineValue = builder.toString();
        }
        return lastVisitedLineValue;
    }

    private char getCharCode(NodePosition nodePos) {
        if (nodePos.remainder == nodePos.node.piece.length) {
            // the char we want to fetch is at the head of next node.
            TreeNode matchingNode = nodePos.node.next();
            if (matchingNode == SENTINEL) {
                return 0;
            }
            StringBuffer buffer = buffers.get(matchingNode.piece.bufferIndex);
            int startOffset = offsetInBuffer(matchingNode.piece.bufferIndex, matchingNode.piece.start);
            return buffer.buffer.charAt(startOffset);
        } else {
            StringBuffer buffer = buffers.get(nodePos.node.piece.bufferIndex);
            int startOffset = offsetInBuffer(nodePos.node.piece.bufferIndex, nodePos.node.piece.start);
            int targetOffset = startOffset + nodePos.remainder;
            return buffer.buffer.charAt(targetOffset);
        }
    }

    public char getLineCharCode(int lineNumber, int index) {
        NodePosition nodePos = nodeAt2(lineNumber, index + 1);
        return getCharCode(nodePos);
    }

    public int getLineLength(int lineNumber) {
        if (lineNumber == getLineCount()) {
            int startOffset = getOffsetAt(lineNumber, 1);
            return getLength() - startOffset;
        }
        return getOffsetAt(lineNumber + 1, 1) - getOffsetAt(lineNumber, 1) - EOL.length();
    }

    public int getCharCode(int offset) {
        NodePosition nodePos = nodeAt(offset);
        return getCharCode(nodePos);
    }

    public String getNearestChunk(int offset) {
        NodePosition nodePos = nodeAt(offset);
        if (nodePos.remainder == nodePos.node.piece.length) {
            // the offset is at the head of next node.
            TreeNode matchingNode = nodePos.node.next();
            if (matchingNode == SENTINEL) {
                return "";
            }
            StringBuffer buffer = buffers.get(matchingNode.piece.bufferIndex);
            int startOffset = offsetInBuffer(matchingNode.piece.bufferIndex, matchingNode.piece.start);
            return buffer.buffer.substring(startOffset, startOffset + matchingNode.piece.length);
        } else {
            StringBuffer buffer = buffers.get(nodePos.node.piece.bufferIndex);
            int startOffset = offsetInBuffer(nodePos.node.piece.bufferIndex, nodePos.node.piece.start);
            int targetOffset = startOffset + nodePos.remainder;
            int targetEnd = startOffset + nodePos.node.piece.length;
            return buffer.buffer.substring(targetOffset, targetEnd);
        }
    }

    /* Skipped: findMatchesInNode */
    /* Skipped: findMatchesLineByLine */
    /* Skipped: _findMatchesInLine */
    //#endregion

    //#region Piece Table
    public void insert(int offset, String value, boolean eolNormalized) {
        this.EOLNormalized = this.EOLNormalized && eolNormalized;
        lastVisitedLine = 0;
        lastVisitedLineValue = "";
        if (root != SENTINEL) {
            NodePosition nodePosition = nodeAt(offset);
            TreeNode node = nodePosition.node;
            int remainder = nodePosition.remainder;
            int nodeStartOffset = nodePosition.nodeStartOffset;
            Piece piece = node.piece;
            int bufferIndex = piece.bufferIndex;
            BufferCursor insertPosInBuffer = positionInBuffer(node, remainder);
            if (node.piece.bufferIndex == 0 &&
                    piece.end.line == lastChangeBufferPos.line &&
                    piece.end.column == lastChangeBufferPos.column &&
                    (nodeStartOffset + piece.length == offset) &&
                    value.length() < AVERAGE_BUFFER_SIZE
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
                List<TreeNode> nodesToDel = new ArrayList<>();
                Piece newRightPiece = new Piece(
                        bufferIndex,
                        insertPosInBuffer,
                        piece.end,
                        getLineFeedCnt(piece.bufferIndex, insertPosInBuffer, piece.end),
                        offsetInBuffer(bufferIndex, piece.end) - offsetInBuffer(bufferIndex, insertPosInBuffer)
                );
                if (shouldCheckCRLF() && endWithCR(value)) {
                    int headOfRight = nodeCharCodeAt(node, remainder);
                    if (headOfRight == '\n') {
                        BufferCursor newStart = new BufferCursor(newRightPiece.start.line + 1, 0);
                        newRightPiece = new Piece(
                                newRightPiece.bufferIndex,
                                newStart,
                                newRightPiece.end,
                                getLineFeedCnt(newRightPiece.bufferIndex, newStart, newRightPiece.end),
                                newRightPiece.length - 1
                        );
                        value += '\n';
                    }
                }
                // reuse node for content before insertion point.
                if (shouldCheckCRLF() && startWithLF(value)) {
                    int tailOfLeft = nodeCharCodeAt(node, remainder - 1);
                    if (tailOfLeft == '\r') {
                        BufferCursor previousPos = positionInBuffer(node, remainder - 1);
                        deleteNodeTail(node, previousPos);
                        value = '\r' + value;
                        if (node.piece.length == 0) {
                            nodesToDel.add(node);
                        }
                    } else {
                        deleteNodeTail(node, insertPosInBuffer);
                    }
                } else {
                    deleteNodeTail(node, insertPosInBuffer);
                }
                List<Piece> newPieces = createNewPieces(value);
                if (newRightPiece.length > 0) {
                    rbInsertRight(node, newRightPiece);
                }
                TreeNode tmpNode = node;
                for (Piece newPiece : newPieces) {
                    tmpNode = rbInsertRight(tmpNode, newPiece);
                }
                deleteNodes(nodesToDel);
            } else {
                insertContentToNodeRight(value, node);
            }
        } else {
            // insert new node
            List<Piece> pieces = createNewPieces(value);
            //noinspection DataFlowIssue: OK when root == SENTINEL
            TreeNode node = rbInsertLeft(null, pieces.getFirst());
            for (int k = 1; k < pieces.size(); k++) {
                node = rbInsertRight(node, pieces.get(k));
            }
        }
        // todo, this is too brutal. Total line feed count should be updated the same way as lf_left.
        computeBufferMetadata();
    }

    public void delete(int offset, int cnt) {
        lastVisitedLine = 0;
        lastVisitedLineValue = "";
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
                    TreeNode next = startNode.next();
                    rbDelete(this, startNode);
                    validateCRLFWithPrevNode(next);
                    computeBufferMetadata();
                    return;
                }
                deleteNodeHead(startNode, endSplitPosInBuffer);
                searchCache.validate(offset);
                validateCRLFWithPrevNode(startNode);
                computeBufferMetadata();
                return;
            }
            if (startPosition.nodeStartOffset + startNode.piece.length == offset + cnt) {
                deleteNodeTail(startNode, startSplitPosInBuffer);
                validateCRLFWithNextNode(startNode);
                computeBufferMetadata();
                return;
            }
            // delete content in the middle, this node will be splitted to nodes
            shrinkNode(startNode, startSplitPosInBuffer, endSplitPosInBuffer);
            computeBufferMetadata();
            return;
        }
        List<TreeNode> nodesToDel = new ArrayList<>();
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
        TreeNode prev = startNode.piece.length == 0 ? startNode.prev() : startNode;
        deleteNodes(nodesToDel);
        validateCRLFWithNextNode(prev);
        computeBufferMetadata();
    }

    private void insertContentToNodeLeft(String value, TreeNode node) {
        // we are inserting content to the beginning of node
        List<TreeNode> nodesToDel = new ArrayList<>();
        if (shouldCheckCRLF() && endWithCR(value) && startWithLF(node)) {
            // move `\n` to new node.
            Piece piece = node.piece;
            BufferCursor newStart = new BufferCursor(piece.start.line + 1, 0);
            Piece nPiece = new Piece(
                    piece.bufferIndex,
                    newStart,
                    piece.end,
                    getLineFeedCnt(piece.bufferIndex, newStart, piece.end),
                    piece.length - 1
            );
            node.piece = nPiece;
            value += '\n';
            updateTreeMetadata(this, node, -1, -1);
            if (node.piece.length == 0) {
                nodesToDel.add(node);
            }
        }
        List<Piece> newPieces = createNewPieces(value);
        TreeNode newNode = rbInsertLeft(node, newPieces.getLast());
        for (int k = newPieces.size() - 2; k >= 0; k--) {
            newNode = rbInsertLeft(newNode, newPieces.get(k));
        }
        validateCRLFWithPrevNode(newNode);
        deleteNodes(nodesToDel);
    }

    private void insertContentToNodeRight(String value, TreeNode node) {
        // we are inserting to the right of this node.
        if (adjustCarriageReturnFromNext(value, node)) {
            // move \n to the new node.
            value += '\n';
        }
        List<Piece> newPieces = createNewPieces(value);
        TreeNode newNode = rbInsertRight(node, newPieces.getFirst());
        TreeNode tmpNode = newNode;
        for (int k = 1; k < newPieces.size(); k++) {
            tmpNode = rbInsertRight(tmpNode, newPieces.get(k));
        }
        validateCRLFWithPrevNode(newNode);
    }

    private BufferCursor positionInBuffer(TreeNode node, int remainder) {
        final Piece piece = node.piece;
        final int bufferIndex = node.piece.bufferIndex;
        final OrderedIntArrayList lineStarts = buffers.get(bufferIndex).lineStarts;
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
        OrderedIntArrayList lineStarts = buffers.get(bufferIndex).lineStarts;
        if (end.line == lineStarts.size() - 1) { // it means, there is no \n after end, otherwise, there will be one more lineStart.
            return end.line - start.line;
        }
        int nextLineStartOffset = lineStarts.get(end.line + 1);
        int endOffset = lineStarts.get(end.line) + end.column;
        if (nextLineStartOffset > endOffset + 1) { // there are more than 1 character after end, which means it can't be \n
            return end.line - start.line;
        }
        // endOffset + 1 == nextLineStartOffset
        // character at endOffset is \n, so we check the character before first
        // if character at endOffset is \r, end.column is 0 and we can't get here.
        int previousCharOffset = endOffset - 1; // end.column > 0 so it's okay.
        StringBuilder buffer = buffers.get(bufferIndex).buffer;
        if (buffer.charAt(previousCharOffset) == '\r') {
            return end.line - start.line + 1;
        } else {
            return end.line - start.line;
        }
    }

    private int offsetInBuffer(int bufferIndex, BufferCursor cursor) {
        OrderedIntArrayList lineStarts = buffers.get(bufferIndex).lineStarts;
        return lineStarts.get(cursor.line) + cursor.column;
    }

    private void deleteNodes(List<TreeNode> nodes) {
        for (TreeNode node : nodes) {
            rbDelete(this, node);
        }
    }

    private List<Piece> createNewPieces(String text) {
        if (text.length() > AVERAGE_BUFFER_SIZE) {
            // the content is large, operations like substring, charCode becomes slow
            // so here we split it into smaller chunks, just like what we did for CR/LF normalization
            List<Piece> newPieces = new ArrayList<>(text.length() / AVERAGE_BUFFER_SIZE);
            int start = 0;
            while (text.length() - start > AVERAGE_BUFFER_SIZE) {
                char lastChar = text.charAt(start + AVERAGE_BUFFER_SIZE - 1);
                String splitText;
                if (lastChar == '\r' || Character.isHighSurrogate(lastChar)) {
                    // last character is \r or a high surrogate => keep it back
                    splitText = text.substring(start, start + AVERAGE_BUFFER_SIZE - 1);
                    start += AVERAGE_BUFFER_SIZE - 1;
                } else {
                    splitText = text.substring(start, start + AVERAGE_BUFFER_SIZE);
                    start += AVERAGE_BUFFER_SIZE;
                }
                OrderedIntArrayList lineStarts = createLineStartsFast(splitText, true);
                newPieces.add(new Piece(
                        buffers.size(),
                        new BufferCursor(0, 0),
                        new BufferCursor(lineStarts.size() - 1, splitText.length() - lineStarts.getLast()),
                        lineStarts.size() - 1,
                        splitText.length()
                ));
                buffers.add(new StringBuffer(splitText, lineStarts));
            }
            text = text.substring(start);
            OrderedIntArrayList lineStarts = createLineStartsFast(text, true);
            newPieces.add(new Piece(
                    buffers.size(),
                    new BufferCursor(0, 0),
                    new BufferCursor(lineStarts.size() - 1, text.length() - lineStarts.getLast()),
                    lineStarts.size() - 1,
                    text.length()
            ));
            buffers.add(new StringBuffer(text, lineStarts));
            return newPieces;
        }
        int startOffset = buffers.getFirst().buffer.length();
        OrderedIntArrayList lineStarts = createLineStartsFast(text, false);
        BufferCursor start = lastChangeBufferPos;
        if (buffers.getFirst().lineStarts.getLast() == startOffset
                && startOffset != 0
                && startWithLF(text)
                && endWithCR(buffers.getFirst().buffer) // todo, we can check lastChangeBufferPos's column as it's the last one
        ) {
            lastChangeBufferPos = new BufferCursor(lastChangeBufferPos.line, lastChangeBufferPos.column + 1);
            start = lastChangeBufferPos;
            for (int i = 0; i < lineStarts.size(); i++) {
                lineStarts.set(i, lineStarts.get(i) + 1);
            }
            buffers.getFirst().lineStarts.addAll(lineStarts, 1);
            buffers.getFirst().buffer.append('_').append(text);
            startOffset += 1;
        } else {
            if (startOffset != 0) {
                for (int i = 0; i < lineStarts.size(); i++) {
                    lineStarts.set(i, lineStarts.get(i) + startOffset);
                }
            }
            buffers.getFirst().lineStarts.addAll(lineStarts, 1);
            buffers.getFirst().buffer.append(text);
        }
        int endOffset = buffers.getFirst().buffer.length();
        int endIndex = buffers.getFirst().lineStarts.size() - 1;
        int endColumn = endOffset - buffers.getFirst().lineStarts.get(endIndex);
        BufferCursor endPos = new BufferCursor(endIndex, endColumn);
        Piece newPiece = new Piece(
                0,
                start,
                endPos,
                getLineFeedCnt(0, start, endPos),
                endOffset - startOffset
        );
        lastChangeBufferPos = endPos;
        return List.of(newPiece);
    }

    public String getLinesRawContent() {
        return getContentOfSubTree(root);
    }

    public String getLineRawContent(int lineNumber, int endOffset) {
        TreeNode x = root;
        StringBuilder ret = new StringBuilder();
        PieceTreeSearchCache.@Nullable CacheEntry cache = searchCache.get2(lineNumber);
        if (cache != null) {
            x = cache.node();
            int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - cache.nodeStartLineNumber() - 1);
            StringBuilder buffer = buffers.get(x.piece.bufferIndex).buffer;
            int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
            if (cache.nodeStartLineNumber() + x.piece.lineFeedCnt == lineNumber) {
                ret.setLength(0);
                ret.append(buffer, startOffset + prevAccumulatedValue, startOffset + x.piece.length);
            } else {
                int accumulatedValue = getAccumulatedValue(x, lineNumber - cache.nodeStartLineNumber());
                return buffer.substring(startOffset + prevAccumulatedValue, startOffset + accumulatedValue - endOffset);
            }
        } else {
            int nodeStartOffset = 0;
            int originalLineNumber = lineNumber;
            while (x != SENTINEL) {
                if (x.left != SENTINEL && x.lf_left >= lineNumber - 1) {
                    x = x.left;
                } else if (x.lf_left + x.piece.lineFeedCnt > lineNumber - 1) {
                    int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                    int accumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 1);
                    StringBuilder buffer = buffers.get(x.piece.bufferIndex).buffer;
                    int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                    nodeStartOffset += x.size_left;
                    searchCache.add(new PieceTreeSearchCache.CacheEntry(x, nodeStartOffset, originalLineNumber - (lineNumber - 1 - x.lf_left)));
                    return buffer.substring(startOffset + prevAccumulatedValue, startOffset + accumulatedValue - endOffset);
                } else if (x.lf_left + x.piece.lineFeedCnt == lineNumber - 1) {
                    int prevAccumulatedValue = getAccumulatedValue(x, lineNumber - x.lf_left - 2);
                    StringBuilder buffer = buffers.get(x.piece.bufferIndex).buffer;
                    int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                    ret.setLength(0);
                    ret.append(buffer, startOffset + prevAccumulatedValue, startOffset + x.piece.length);
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
            StringBuilder buffer = buffers.get(x.piece.bufferIndex).buffer;
            if (x.piece.lineFeedCnt > 0) {
                int accumulatedValue = getAccumulatedValue(x, 0);
                int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                ret.append(buffer, startOffset, startOffset + accumulatedValue - endOffset);
                return ret.toString();
            } else {
                int startOffset = offsetInBuffer(x.piece.bufferIndex, x.piece.start);
                ret.append(buffer, startOffset, startOffset + x.piece.length);
            }
            x = x.next();
        }
        return ret.toString();
    }

    private void computeBufferMetadata() {
        TreeNode x = root;
        int lfCnt = 1;
        int len = 0;
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
        OrderedIntArrayList lineStarts = buffers.get(piece.bufferIndex()).lineStarts;
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
        int originalEndOffset = offsetInBuffer(piece.bufferIndex, piece.end);
        //noinspection UnnecessaryLocalVariable
        BufferCursor newEnd = pos;
        int newEndOffset = offsetInBuffer(piece.bufferIndex, newEnd);
        int newLineFeedCnt = getLineFeedCnt(piece.bufferIndex, piece.start, newEnd);
        int lf_delta = newLineFeedCnt - originalLFCnt;
        int size_delta = newEndOffset - originalEndOffset;
        int newLength = piece.length + size_delta;
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
        int originalStartOffset = offsetInBuffer(piece.bufferIndex, piece.start);
        //noinspection UnnecessaryLocalVariable
        BufferCursor newStart = pos;
        int newLineFeedCnt = getLineFeedCnt(piece.bufferIndex, newStart, piece.end);
        int newStartOffset = offsetInBuffer(piece.bufferIndex, newStart);
        int lf_delta = newLineFeedCnt - originalLFCnt;
        int size_delta = originalStartOffset - newStartOffset;
        int newLength = piece.length + size_delta;
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
        int oldLength = piece.length;
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
        TreeNode newNode = rbInsertRight(node, newPiece);
        validateCRLFWithPrevNode(newNode);
    }

    private void appendToNode(TreeNode node, String value) {
        if (adjustCarriageReturnFromNext(value, node)) {
            value += '\n';
        }
        boolean hitCRLF = shouldCheckCRLF() && startWithLF(value) && endWithCR(node);
        int startOffset = buffers.getFirst().buffer.length();
        buffers.getFirst().buffer.append(value);
        OrderedIntArrayList lineStarts = createLineStartsFast(value, false);
        for (int i = 0; i < lineStarts.size(); i++) {
            lineStarts.set(i, lineStarts.get(i) + startOffset);
        }
        if (hitCRLF) {
            OrderedIntArrayList bufferLineStarts = buffers.getFirst().lineStarts;
            int prevStartOffset = bufferLineStarts.get(bufferLineStarts.size() - 2);
            bufferLineStarts.pop();
            // _lastChangeBufferPos is already wrong
            lastChangeBufferPos = new BufferCursor(lastChangeBufferPos.line - 1, startOffset - prevStartOffset);
        }
        buffers.getFirst().lineStarts.addAll(lineStarts, 1);
        int endIndex = buffers.getFirst().lineStarts.size() - 1;
        int endColumn = buffers.getFirst().buffer.length() - buffers.getFirst().lineStarts.get(endIndex);
        BufferCursor newEnd = new BufferCursor(endIndex, endColumn);
        int newLength = node.piece.length + value.length();
        int oldLineFeedCnt = node.piece.lineFeedCnt;
        int newLineFeedCnt = getLineFeedCnt(0, node.piece.start, newEnd);
        int lf_delta = newLineFeedCnt - oldLineFeedCnt;
        node.piece = new Piece(
                node.piece.bufferIndex,
                node.piece.start,
                newEnd,
                newLineFeedCnt,
                newLength
        );
        lastChangeBufferPos = newEnd;
        updateTreeMetadata(this, node, value.length(), lf_delta);
    }

    private NodePosition nodeAt(int offset) {
        TreeNode x = root;
        PieceTreeSearchCache.@Nullable CacheEntry cache = searchCache.get(offset);
        if (cache != null) {
            return new NodePosition(cache.node(), cache.nodeStartOffset(), offset - cache.nodeStartOffset());
        }
        int nodeStartOffset = 0;
        while (x != SENTINEL) {
            if (x.size_left > offset) {
                x = x.left;
            } else if (x.size_left + x.piece.length >= offset) {
                nodeStartOffset += x.size_left;
                NodePosition ret = new NodePosition(x, nodeStartOffset, offset - x.size_left);
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
        int nodeStartOffset = 0;
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

    private int nodeCharCodeAt(TreeNode node, int offset) {
        if (node.piece.lineFeedCnt < 1) {
            return -1;
        }
        StringBuffer buffer = buffers.get(node.piece.bufferIndex);
        int newOffset = offsetInBuffer(node.piece.bufferIndex, node.piece.start) + offset;
        return buffer.buffer.charAt(newOffset);
    }

    private int offsetOfNode(TreeNode node) {
        int pos = node.size_left;
        while (node != root) {
            if (node.parent.right == node) {
                pos += node.parent.size_left + node.parent.piece.length;
            }
            node = node.parent;
        }
        return pos;
    }
    //#endregion

    //#region CR LF
    private boolean shouldCheckCRLF() {
        return !(EOLNormalized && EOL == EndOfLine.LF);
    }

    private boolean startWithLF(CharSequence val) {
        return val.charAt(0) == '\n';
    }

    private boolean startWithLF(TreeNode val) {
        if (val == SENTINEL || val.piece.lineFeedCnt == 0) {
            return false;
        }
        Piece piece = val.piece;
        OrderedIntArrayList lineStarts = buffers.get(piece.bufferIndex).lineStarts;
        int line = piece.start.line;
        int startOffset = lineStarts.get(line) + piece.start.column;
        if (line == lineStarts.size() - 1) {
            // last line, so there is no line feed at the end of this line
            return false;
        }
        int nextLineOffset = lineStarts.get(line + 1);
        if (nextLineOffset > startOffset + 1) {
            return false;
        }
        return buffers.get(piece.bufferIndex).buffer.charAt(startOffset) == '\n';
    }

    private boolean endWithCR(CharSequence val) {
        return val.charAt(val.length() - 1) == '\r';
    }

    private boolean endWithCR(TreeNode val) {
        if (val == SENTINEL || val.piece.lineFeedCnt == 0) {
            return false;
        }
        return nodeCharCodeAt(val, val.piece.length - 1) == '\r';
    }

    private void validateCRLFWithPrevNode(TreeNode nextNode) {
        if (shouldCheckCRLF() && startWithLF(nextNode)) {
            TreeNode node = nextNode.prev();
            if (endWithCR(node)) {
                fixCRLF(node, nextNode);
            }
        }
    }

    private void validateCRLFWithNextNode(TreeNode node) {
        if (shouldCheckCRLF() && endWithCR(node)) {
            TreeNode nextNode = node.next();
            if (startWithLF(nextNode)) {
                fixCRLF(node, nextNode);
            }
        }
    }

    private void fixCRLF(TreeNode prev, TreeNode next) {
        List<TreeNode> nodesToDel = new ArrayList<>();
        // update node
        OrderedIntArrayList lineStarts = buffers.get(prev.piece.bufferIndex).lineStarts;
        BufferCursor newEnd;
        if (prev.piece.end.column == 0) {
            // it means, last line ends with \r, not \r\n
            newEnd = new BufferCursor(
                    prev.piece.end.line - 1,
                    lineStarts.get(prev.piece.end.line) - lineStarts.get(prev.piece.end.line - 1) - 1
            );
        } else {
            // \r\n
            newEnd = new BufferCursor(prev.piece.end.line, prev.piece.end.column - 1);
        }
        int prevNewLength = prev.piece.length - 1;
        int prevNewLFCnt = prev.piece.lineFeedCnt - 1;
        prev.piece = new Piece(
                prev.piece.bufferIndex,
                prev.piece.start,
                newEnd,
                prevNewLFCnt,
                prevNewLength
        );
        updateTreeMetadata(this, prev, -1, -1);
        if (prev.piece.length == 0) {
            nodesToDel.add(prev);
        }
        // update nextNode
        BufferCursor newStart = new BufferCursor(next.piece.start.line + 1, 0);
        int newLength = next.piece.length - 1;
        int newLineFeedCnt = getLineFeedCnt(next.piece.bufferIndex, newStart, next.piece.end);
        next.piece = new Piece(
                next.piece.bufferIndex,
                newStart,
                next.piece.end,
                newLineFeedCnt,
                newLength
        );
        updateTreeMetadata(this, next, -1, -1);
        if (next.piece.length == 0) {
            nodesToDel.add(next);
        }
        // create new piece which contains \r\n
        List<Piece> pieces = createNewPieces("\r\n");
        rbInsertRight(prev, pieces.getFirst());
        // delete empty nodes
        for (TreeNode node : nodesToDel) {
            rbDelete(this, node);
        }
    }

    private boolean adjustCarriageReturnFromNext(String value, TreeNode node) {
        if (shouldCheckCRLF() && endWithCR(value)) {
            TreeNode nextNode = node.next();
            if (startWithLF(nextNode)) {
                // move `\n` forward
                value += '\n';
                if (nextNode.piece.length == 1) {
                    rbDelete(this, nextNode);
                } else {
                    Piece piece = nextNode.piece;
                    BufferCursor newStart = new BufferCursor(piece.start.line + 1, 0);
                    int newLength = piece.length - 1;
                    int newLineFeedCnt = getLineFeedCnt(piece.bufferIndex, newStart, piece.end);
                    nextNode.piece = new Piece(
                            piece.bufferIndex,
                            newStart,
                            piece.end,
                            newLineFeedCnt,
                            newLength
                    );
                    updateTreeMetadata(this, nextNode, -1, -1);
                }
                return true;
            }
        }
        return false;
    }
    //#endregion

    //#region Tree operations
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

    private void getNodeContent(TreeNode node, StringBuilder output) {
        if (node == SENTINEL) {
            return;
        }
        StringBuffer buffer = buffers.get(node.piece.bufferIndex());
        Piece piece = node.piece;
        int startOffset = offsetInBuffer(piece.bufferIndex(), piece.start);
        int endOffset = offsetInBuffer(piece.bufferIndex(), piece.end);
        output.append(buffer.buffer, startOffset, endOffset);
    }

    private String getPieceContent(Piece piece) {
        StringBuffer buffer = buffers.get(piece.bufferIndex());
        int startOffset = offsetInBuffer(piece.bufferIndex(), piece.start);
        int endOffset = offsetInBuffer(piece.bufferIndex(), piece.end);
        String currentContent = buffer.buffer.substring(startOffset, endOffset);
        return currentContent;
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

    private String getContentOfSubTree(TreeNode subTree) {
        StringBuilder str = new StringBuilder();
        iterate(subTree, (node) -> {
            getNodeContent(node, str);
            return true;
        });
        return str.toString();
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

    public enum EndOfLine {
        LF("\n"),
        CR_LF("\r\n");

        private final String eol;

        EndOfLine(String eol) {
            this.eol = eol;
        }

        public String eol() {
            return eol;
        }

        public int length() {
            return eol.length();
        }

        @Override
        public String toString() {
            return eol();
        }
    }

    private record Index(int index, int remainder) {
    }

    @Override
    public int length() {
        return getLength();
    }

    @Override
    public char charAt(int index) {
        return (char) getCharCode(index);
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        NodePosition startNode = nodeAt(start);
        NodePosition endNode = nodeAt(end);
        return getValueInRange2(startNode, endNode);
    }

    @Override
    public String toString() {
        return getLinesRawContent();
    }

    private class CharIterator implements PrimitiveIterator.OfInt {
        private TreeNode currentNode = root.leftest();
        private int cachedStartOffset = -1;
        private int current = 0;

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
            StringBuilder buffer = buffers.get(piece.bufferIndex).buffer;
            return buffer.charAt(cachedStartOffset + current++);
        }

        @Override
        public boolean hasNext() {
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

    @Override
    public IntStream chars() {
        return StreamSupport.intStream(() ->
                        Spliterators.spliterator(
                                new CharIterator(),
                                length(),
                                Spliterator.ORDERED),
                Spliterator.SUBSIZED | Spliterator.SIZED | Spliterator.ORDERED,
                false);
    }

    @Override
    public IntStream codePoints() {
        class CodePointIterator implements PrimitiveIterator.OfInt {
            private final CharIterator charIterator = new CharIterator();
            private int peeked = -1;

            @Override
            public int nextInt() {
                char c1;
                if (peeked == -1) {
                    c1 = (char) charIterator.nextInt();
                } else {
                    c1 = (char) peeked;
                    peeked = -1;
                }
                if (Character.isHighSurrogate(c1) && charIterator.hasNext()) {
                    char c2 = (char) charIterator.nextInt();
                    if (Character.isLowSurrogate(c2)) {
                        return Character.toCodePoint(c1, c2);
                    } else {
                        peeked = c2;
                    }
                }
                return c1;
            }

            @Override
            public boolean hasNext() {
                return peeked != -1 || charIterator.hasNext();
            }
        }
        return StreamSupport.intStream(() ->
                        Spliterators.spliteratorUnknownSize(
                                new CodePointIterator(),
                                Spliterator.ORDERED),
                Spliterator.ORDERED,
                false);
    }
}
