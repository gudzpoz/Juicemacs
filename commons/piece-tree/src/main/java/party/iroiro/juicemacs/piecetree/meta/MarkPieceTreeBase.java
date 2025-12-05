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
/// Although both [IntervalPieceTree] and [MarkerPieceTree] builds upon this class,
/// their "marks"  are quite different. So, this class only offers the bare minimum
/// of tree editing [#rbInsertLeft(MarkTreeNode, Piece)] and [#rbInsertRight(MarkTreeNode, Piece)],
/// with main tree operations implemented in corresponding classes, with moderate
/// code repetitions.
@SuppressWarnings("unchecked")
public sealed abstract class MarkPieceTreeBase<T extends @Nullable Object> permits IntervalPieceTree, MarkerPieceTree {
    MarkTreeNode<T> root;

    public MarkPieceTreeBase() {
        root = SENTINEL;
    }

    public long getLength() {
        long length = 0;
        MarkTreeNode<T> node = root;
        while (node != SENTINEL) {
            length += node.size_left + node.piece.length;
            node = node.right;
        }
        return length;
    }

    final void deleteNodes(List<MarkTreeNode<T>> nodes) {
        for (MarkTreeNode<T> node : nodes) {
            rbDelete(this, node);
        }
    }

    final void updateNodeSome(MarkTreeNode<T> node, long lengthDelta) {
        node.piece.length += lengthDelta;
        updateTreeMetadata(this, node, lengthDelta);
    }

    @TruffleBoundary
    final NodePosition<T> nodeAt(long offset) {
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
    final MarkTreeNode<T> rbInsertRight(MarkTreeNode<T> node, Piece<T> piece) {
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
    final MarkTreeNode<T> rbInsertLeft(@Nullable MarkTreeNode<T> node, Piece<T> piece) {
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
    final <R> R forEachMarkIn(long offset, long cnt, PieceConsumer<T, R> consumer) {
        if (root == SENTINEL) {
            return null;
        }
        NodePosition<T> startPosition = nodeAt(offset);
        MarkTreeNode<T> currentNode = startPosition.node;
        long currentOffset = offset;
        long remainder = startPosition.remainder;
        long targetOffset = offset + cnt;
        if (targetOffset < cnt) {
            targetOffset = Long.MAX_VALUE;
        }
        if (remainder == 0) {
            while (true) {
                MarkTreeNode<T> prev = currentNode.prev();
                if (prev == SENTINEL || prev.piece.length != 0) {
                    break;
                }
                currentNode = prev;
            }
        }
        while (currentOffset < targetOffset && currentNode != SENTINEL) {
            Piece<T> piece = currentNode.piece;
            long length = piece.length - remainder;
            long newOffset = currentOffset + length;
            remainder = 0;
            if (newOffset > targetOffset) {
                length -= newOffset - targetOffset;
            }
            if (length != piece.length) {
                piece = new Piece<>(length, piece.mark);
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

    interface PieceConsumer<T extends @Nullable Object, R> {
        @Nullable R accept(Piece<T> piece, long offset);
    }

    static final class NodePosition<T extends @Nullable Object> {
        final MarkTreeNode<T> node;
        final long nodeStartOffset;
        final long remainder;

        /**
         * @param node            piece Index
         * @param remainder       remainder in the current piece
         * @param nodeStartOffset node start offset in document
         */
        private NodePosition(MarkTreeNode<T> node, long nodeStartOffset, long remainder) {
            this.node = node;
            this.nodeStartOffset = nodeStartOffset;
            this.remainder = remainder;
        }
    }

    static final class Piece<T extends @Nullable Object> {
        long length;
        final T mark;

        Piece(long length, T mark) {
            this.length = length;
            this.mark = mark;
        }
    }
}
