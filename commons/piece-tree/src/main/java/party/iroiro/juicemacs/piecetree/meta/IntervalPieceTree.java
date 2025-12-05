package party.iroiro.juicemacs.piecetree.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.SENTINEL;
import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.rbDelete;

/// A tree for keeping text properties.
///
/// Since it is for Emacs buffer text properties, for which "the properties
/// belong to a particular character at a particular place", it is actually
/// not an "interval tree". And that makes it rather similar to our text
/// piece tree [party.iroiro.juicemacs.piecetree.PieceTreeBase]: each segment
/// of text of the same properties is a "piece". The only difference is that
/// this class make extra efforts to merge adjacent pieces of the same properties.
public final class IntervalPieceTree<T extends @Nullable Object> extends MarkPieceTreeBase<@Nullable T> {
    //#region Properties
    @Nullable
    public T getPropertiesAt(long offset) {
        NodePosition<T> nodePosition = nodeAt(offset);
        MarkTreeNode<T> node = nodePosition.node;
        Piece<T> piece = node.piece;
        if (nodePosition.nodeStartOffset + piece.length == offset) {
            MarkTreeNode<T> next = node.next();
            if (next == SENTINEL) {
                // out of bound: nodeAt expects offsets in [0, len], but we expect [0, len) here
                throw new IllegalArgumentException();
            }
            return next.piece.mark;
        }
        return piece.mark;
    }

    @TruffleBoundary
    public void putPropertiesFor(long offset, long cnt, T properties) {
        NodePosition<T> startPosition = nodeAt(offset);
        MarkTreeNode<T> node = startPosition.node;
        if (startPosition.remainder == 0 && node.piece.length == cnt) {
            node.piece = new Piece<>(cnt, properties);
            long length = cnt;
            MarkTreeNode<T> merged = null;
            MarkTreeNode<T> prev = node.prev();
            if (prev != SENTINEL && prev.piece.mark == node.piece.mark) {
                rbDelete(this, node);
                length += prev.piece.length;
                node = merged = prev;
            }
            MarkTreeNode<T> next = node.next();
            if (next != SENTINEL && node.piece.mark == next.piece.mark) {
                rbDelete(this, node);
                length += next.piece.length;
                merged = next;
            }
            if (merged != null) {
                updateNodeSome(merged, length - merged.piece.length);
            }
            return;
        }
        deleteStartingFrom(offset, cnt, startPosition);
        insertMerging(offset, new Piece<>(cnt, properties));
    }

    @Nullable
    public <R> R forPropertiesIn(long offset, long cnt, boolean includeNull, IntervalConsumer<T, R> consumer) {
        return forEachMarkIn(offset, cnt, (piece, currentOffset) -> {
            if (includeNull || piece.mark != null) {
                return consumer.accept(piece.mark, currentOffset, piece.length);
            }
            return null;
        });
    }

    @SuppressWarnings("unchecked")
    public IntervalPieceTree<T> subTree(long offset, long cnt) {
        IntervalPieceTree<T> subTree = new IntervalPieceTree<>();
        MarkTreeNode<T>[] prev = new MarkTreeNode[1];
        forEachMarkIn(offset, cnt, (piece, _) -> {
            prev[0] = subTree.rbInsertRight(prev[0], piece);
            return null;
        });
        return subTree;
    }

    public void insert(long offset, MarkPieceTreeBase<T> other) {
        if (other.root == SENTINEL) {
            return;
        }
        other.forEachMarkIn(0, Long.MAX_VALUE, (piece, currentOffset) -> {
            insertMerging(offset + currentOffset, piece);
            return null;
        });
    }

    public void insert(long offset, long cnt, @Nullable T properties) {
        if (cnt == 0) {
            return;
        }
        insertMerging(offset, new Piece<>(cnt, properties));
    }

    void insert(long offset, Piece<T> value) {
        insertMerging(offset, value);
    }
    //#endregion Properties

    @TruffleBoundary
    private void insertMerging(long offset, Piece<T> value) {
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
        if (value.mark == piece.mark) {
            // changed buffer
            updateNodeSome(node, value.length);
            return;
        }
        if (nodeStartOffset == offset) {
            MarkTreeNode<T> prev = node.prev();
            if (prev != SENTINEL && prev.piece.mark == value.mark) {
                updateNodeSome(prev, value.length);
                return;
            }
            rbInsertLeft(node, value);
        } else if (nodeStartOffset + piece.length > offset) {
            insertMiddleOf(value, node, remainder);
        } else {
            MarkTreeNode<T> next = node.next();
            if (next != SENTINEL && next.piece.mark == value.mark) {
                updateNodeSome(next, value.length);
                return;
            }
            rbInsertRight(node, value);
        }
    }

    void insertMiddleOf(Piece<T> value, MarkTreeNode<T> node, long remainder) {
        Piece<T> surrounding = node.piece;
        // we are inserting into the middle of a node.
        Piece<T> newLeftPiece = new Piece<>(
                remainder,
                surrounding.mark
        );
        // reuse node for content before insertion point.
        updateNodeSome(node, -remainder);
        if (newLeftPiece.length > 0) {
            rbInsertLeft(node, newLeftPiece);
        }
        rbInsertLeft(node, value);
    }

    public void delete(long offset, long cnt) {
        if (cnt <= 0 || root == SENTINEL) {
            return;
        }
        NodePosition<T> startPosition = nodeAt(offset);
        deleteStartingFrom(offset, cnt, startPosition);
    }

    @TruffleBoundary
    private void deleteStartingFrom(long offset, long cnt, NodePosition<T> startPosition) {
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
                    if (prev.piece.mark == next.piece.mark) {
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

    private void deleteInBetween(NodePosition<T> startPosition, NodePosition<T> endPosition) {
        MarkTreeNode<T> startNode = startPosition.node;
        MarkTreeNode<T> endNode = endPosition.node;

        List<MarkTreeNode<T>> nodesToDel = new ArrayList<>();

        long lengthToDelete = startPosition.node.piece.length - startPosition.remainder;
        MarkTreeNode<T> mergeStart = startNode;
        if (lengthToDelete != 0) {
            if (startNode.piece.length == lengthToDelete) {
                // an interval node is deleted
                nodesToDel.add(startNode);
                mergeStart = startNode.prev();
            } else {
                updateNodeSome(startNode, -lengthToDelete);
            }
        }

        // delete interval nodes in between
        MarkTreeNode<T> secondNode = startNode.next();
        for (MarkTreeNode<T> node = secondNode; node != endNode; node = node.next()) {
            assert node != SENTINEL;
            nodesToDel.add(node);
        }

        // update last touched node
        MarkTreeNode<T> mergeEnd = endNode;
        if (endPosition.remainder != 0) {
            if (endNode.piece.length == endPosition.remainder) {
                // an interval node is deleted
                nodesToDel.add(endNode);
                mergeEnd = endNode.next();
            } else {
                updateNodeSome(endNode, -endPosition.remainder);
            }
        }
        deleteNodes(nodesToDel);

        if (mergeStart != SENTINEL && mergeEnd != SENTINEL) {
            if (mergeStart.piece.mark == mergeEnd.piece.mark) {
                updateNodeSome(mergeStart, mergeEnd.piece.length);
                rbDelete(this, mergeEnd);
            }
        }
    }

    public interface IntervalConsumer<T extends @Nullable Object, R> {
        @Nullable R accept(@Nullable T properties, long offset, long cnt);
    }
}
