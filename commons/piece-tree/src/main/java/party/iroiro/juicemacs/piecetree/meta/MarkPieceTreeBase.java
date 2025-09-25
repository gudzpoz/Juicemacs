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

package party.iroiro.juicemacs.piecetree.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;

import java.util.*;

import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.*;

/// A red-black tree optimized for text range/mark bookkeeping
///
/// ## Rationale
///
/// Emacs text mark assign mark to ranges of text, which "naturally"
/// calls for an interval tree, right? Actually, no. The problem is that whenever
/// Emacs inserts text, either into a buffer or by concatenating texts, *almost all*
/// the intervals in the tree need to be updated.
///
/// Instead, we should use a rope-like data structure. In this way, ranges are updated
/// similar to how [party.iroiro.juicemacs.piecetree.PieceTreeBase] handles text insertion.
///
/// ## Code
///
/// This class is modified from [party.iroiro.juicemacs.piecetree.PieceTreeBase], which in turn is
/// transliterated from <a href="https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeBase.ts">
/// pieceTreeBase.ts @ vscode</a>
///
/// ## Marks
///
/// This implementation assumes that there are three kinds of marks:
/// - Empty interval marks: marks whose [Piece#mark] is `null`. They serve as placeholders,
///   storing relative offsets of other *real* marks. Their [Piece#length] must not be `0`.
///   - Adjacent empty intervals are automatically merged.
/// - Non-empty interval marks: marks whose [Piece#mark] is not `null`, with non-zero [Piece#length].
///   They are most likely to be used for Emacs text properties.
///   - To change the merge behavior, override [#isMergeable].
/// - Non-empty point markers: marks whose [Piece#mark] is not `null`, with zero [Piece#length].
///   These marks represent Emacs markers, that is, a single position in a buffer. They have
///   [MarkerPieceTree.Affinity]: when inserting text at the mark, markers with [MarkerPieceTree.Affinity#LEFT] will not move,
///   while [MarkerPieceTree.Affinity#RIGHT] will move along to the end of the inserted text.
///   - To assign affinity, override [#getAffinity].
@SuppressWarnings("unchecked")
public sealed abstract class MarkPieceTreeBase<T> permits IntervalPieceTree, MarkerPieceTree {
    MarkTreeNode<T> root;

    public MarkPieceTreeBase() {
        root = SENTINEL;
    }

    //#region Override methods to change behaviors
    protected boolean isMergeable(Piece<T> value, Piece<T> piece) {
        // TODO: expose
        return value.mark == piece.mark;
    }
    //#endregion Override methods to change behaviors

    public long getLength() {
        long length = 0;
        MarkTreeNode<T> node = root;
        while (node != SENTINEL) {
            length += node.size_left + node.piece.length;
            node = node.right;
        }
        return length;
    }

    @TruffleBoundary
    protected void insert(long offset, Piece<T> value) {
        if (root == SENTINEL) {
            // insert new node
            rbInsertLeft(null, value);
            return;
        }

        NodePosition<T> nodePosition = nodeAt(offset);
        MarkTreeNode<T> node = nodePosition.node;
        long remainder = nodePosition.remainder;
        long nodeStartOffset = nodePosition.nodeStartOffset;
        Piece<T> piece = node.piece;
        if (isMergeable(value, piece)) {
            // changed buffer
            updateNodeSome(node, value.length);
            return;
        }
        if (nodeStartOffset == offset) {
            MarkTreeNode<T> prev = node.prev();
            if (prev != SENTINEL && isMergeable(prev.piece, piece)) {
                updateNodeSome(prev, value.length);
                return;
            }
            rbInsertLeft(node, value);
        } else if (nodeStartOffset + node.piece.length > offset) {
            // we are inserting into the middle of a node.
            Piece<T> newRightPiece = new Piece<>(
                    piece.length - remainder,
                    piece.mark
            );
            // reuse node for content before insertion point.
            updateNodeSome(node, remainder - node.piece.length);
            if (newRightPiece.length > 0) {
                rbInsertRightNoMerge(node, newRightPiece);
            }
            rbInsertRight(node, value);
        } else {
            MarkTreeNode<T> next = node.next();
            if (next != SENTINEL && isMergeable(next.piece, piece)) {
                updateNodeSome(next, value.length);
                return;
            }
            rbInsertRight(node, value);
        }
    }

    public void delete(long offset, long cnt) {
        if (cnt <= 0 || root == SENTINEL) {
            return;
        }
        NodePosition<T> startPosition = nodeAt(offset);
        deleteStartingFrom(offset, cnt, startPosition);
    }

    @TruffleBoundary
    protected void deleteStartingFrom(long offset, long cnt, NodePosition<T> startPosition) {
        if (cnt == 0) {
            // [#delete] only deletes interval marks
            return;
        }

        NodePosition<T> endPosition = nodeAt(offset + cnt);
        MarkTreeNode<T> startNode = startPosition.node;
        MarkTreeNode<T> endNode = endPosition.node;
        if (startNode == endNode) {
            // since cnt != 0, startNode is an interval node
            if (cnt == startNode.piece.length) { // delete node
                MarkTreeNode<T> prev = startNode.prev();
                MarkTreeNode<T> next = startNode.next();
                rbDelete(this, startNode);
                if (prev != SENTINEL && next != SENTINEL) {
                    if (isMergeable(prev.piece, next.piece)) {
                        updateNodeSome(prev, next.piece.length);
                        rbDelete(this, next);
                    }
                }
                return;
            }
            updateNodeSome(startNode, -cnt);
            return;
        }

        deleteInBetween(startPosition, endPosition);
    }

    protected void deleteInBetween(NodePosition<T> startPosition, NodePosition<T> endPosition) {
        MarkTreeNode<T> startNode = startPosition.node;
        MarkTreeNode<T> endNode = endPosition.node;

        List<MarkTreeNode<T>> nodesToDel = new ArrayList<>();

        long lengthToDelete = startPosition.node.piece.length - startPosition.remainder;
        if (lengthToDelete != 0) {
            updateNodeSome(startNode, -lengthToDelete);
            if (startNode.piece.length == 0) {
                // an interval node is deleted
                nodesToDel.add(startNode);
            }
        }

        // delete interval nodes in between
        MarkTreeNode<T> secondNode = startNode.next();
        for (MarkTreeNode<T> node = secondNode; node != endNode && node != SENTINEL; node = node.next()) {
            nodesToDel.add(node);
        }

        // update last touched node
        if (endPosition.remainder != 0) {
            updateNodeSome(endNode, -endPosition.remainder);
            if (endNode.piece.length == 0) {
                // an interval node is deleted
                nodesToDel.add(endNode);
            }
        }
        deleteNodes(nodesToDel);
    }

    protected void deleteNodes(List<MarkTreeNode<T>> nodes) {
        for (MarkTreeNode<T> node : nodes) {
            rbDelete(this, node);
        }
    }

    protected void updateNodeSome(MarkTreeNode<T> node, long lengthDelta) {
        Piece<T> piece = node.piece;
        long newLength = node.piece.length + lengthDelta;
        node.piece = new Piece<>(
                newLength,
                piece.mark
        );
        updateTreeMetadata(this, node, lengthDelta);
    }

    @TruffleBoundary
    protected NodePosition<T> nodeAt(long offset) {
        MarkTreeNode<T> x = root;
        long nodeStartOffset = 0;
        while (x != SENTINEL) {
            if (x.size_left > offset) {
                x = x.left;
            } else if (x.size_left + x.piece.length >= offset) {
                nodeStartOffset += x.size_left;
                return new NodePosition<>(x, nodeStartOffset, offset - x.size_left);
            } else {
                offset -= x.size_left + x.piece.length;
                nodeStartOffset += x.size_left + x.piece.length;
                x = x.right;
            }
        }
        throw new IllegalArgumentException();
    }

    //#region Tree operations
    protected MarkTreeNode<T> rbInsertRight(MarkTreeNode<T> node, Piece<T> piece) {
        return rbInsertRightNoMerge(node, piece);
    }

    private MarkTreeNode<T> rbInsertRightNoMerge(MarkTreeNode<T> node, Piece<T> piece) {
        MarkTreeNode<T> z = new MarkTreeNode<>(piece, MarkTreeNode.RED);
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.parent = SENTINEL;
        z.size_left = 0;

        MarkTreeNode<T> x = root;
        if (x == SENTINEL) {
            root = z;
            z.color = MarkTreeNode.BLACK;
        } else if (node.right == SENTINEL) {
            node.right = z;
            z.parent = node;
        } else {
            MarkTreeNode<T> nextNode = node.right.leftest();
            nextNode.left = z;
            z.parent = nextNode;
        }

        fixInsert(this, z);
        return z;
    }

    @SuppressWarnings("DataFlowIssue")
    protected MarkTreeNode<T> rbInsertLeft(@Nullable MarkTreeNode<T> node, Piece<T> piece) {
        MarkTreeNode<T> z = new MarkTreeNode<>(piece, MarkTreeNode.RED);
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.parent = SENTINEL;
        z.size_left = 0;

        if (root == SENTINEL) {
            root = z;
            z.color = MarkTreeNode.BLACK;
        } else if (node.left == SENTINEL) {
            node.left = z;
            z.parent = node;
        } else {
            MarkTreeNode<T> prevNode = node.left.rightest();
            prevNode.right = z;
            z.parent = prevNode;
        }

        fixInsert(this, z);
        return z;
    }
    //#endregion

    @Nullable
    @TruffleBoundary
    protected <R> R forEachMarkIn(long offset, long cnt, PieceConsumer<T, R> consumer) {
        if (root == SENTINEL) {
            return null;
        }
        NodePosition<T> startPosition = nodeAt(offset);
        MarkTreeNode<T> currentNode = startPosition.node();
        long currentOffset = offset;
        long remainder = startPosition.remainder();
        long targetOffset = offset + cnt;
        if (targetOffset < cnt) {
            targetOffset = Long.MAX_VALUE;
        }
        if (remainder == 0) {
            while (true) {
                MarkTreeNode<T> prev = currentNode.prev();
                if (prev == SENTINEL || prev.piece.length() != 0) {
                    break;
                }
                currentNode = prev;
            }
        }
        while (currentOffset < targetOffset && currentNode != SENTINEL) {
            Piece<T> piece = currentNode.piece;
            long length = piece.length() - remainder;
            long newOffset = currentOffset + length;
            remainder = 0;
            if (newOffset > targetOffset) {
                length -= newOffset - targetOffset;
            }
            if (length != piece.length()) {
                piece = new Piece<>(length, piece.mark());
            }
            R accept = consumer.accept(piece, currentOffset);
            if (accept != null) {
                return accept;
            }
            currentOffset = newOffset;
            currentNode = currentNode.next();
        }
        if (currentOffset == targetOffset) {
            while (currentNode != SENTINEL && currentNode.piece.length == 0) {
                R accept = consumer.accept(currentNode.piece, currentOffset);
                if (accept != null) {
                    return accept;
                }
                currentNode = currentNode.next();
            }
        }
        return null;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        forEachMarkIn(0, Long.MAX_VALUE, (piece, offset) -> {
            if (piece.length == 0) {
                builder .append('[').append(offset).append(']');
            } else {
                builder .append('[').append(offset).append(',').append(piece.length + offset - 1).append(']');
            }
            builder
                    .append(": ")
                    .append(piece.mark)
                    .append('\n');
            return null;
        });
        return builder.toString();
    }

    protected interface PieceConsumer<T, R> {
        @Nullable R accept(Piece<T> piece, long offset);
    }

    /**
     * @param node            piece Index
     * @param remainder       remainder in the current piece
     * @param nodeStartOffset node start offset in document
     */
    protected record NodePosition<T>(MarkTreeNode<T> node, long nodeStartOffset, long remainder) {
    }

    protected record Piece<T>(long length, @Nullable T mark) {
    }

}
