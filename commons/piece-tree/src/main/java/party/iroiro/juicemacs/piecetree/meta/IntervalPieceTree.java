package party.iroiro.juicemacs.piecetree.meta;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;

import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.SENTINEL;
import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.rbDelete;

public final class IntervalPieceTree<T> extends MarkPieceTreeBase<T> {
    //#region Properties
    @Nullable
    public T getPropertiesAt(long offset) {
        NodePosition<T> nodePosition = nodeAt(offset);
        MarkTreeNode<T> node = nodePosition.node();
        Piece<T> piece = node.piece;
        if (nodePosition.nodeStartOffset() + piece.length() == offset) {
            MarkTreeNode<T> next = node.next();
            if (next == SENTINEL) {
                // out of bound: nodeAt expects offsets in [0, len], but we expect [0, len) here
                throw new IllegalArgumentException();
            }
            return next.piece.mark();
        }
        return piece.mark();
    }

    @CompilerDirectives.TruffleBoundary
    public void putPropertiesFor(long offset, long cnt, T properties) {
        NodePosition<T> startPosition = nodeAt(offset);
        MarkTreeNode<T> node = startPosition.node();
        if (startPosition.remainder() == 0 && node.piece.length() == cnt) {
            node.piece = new Piece<>(cnt, properties);
            long length = cnt;
            MarkTreeNode<T> merged = null;
            MarkTreeNode<T> prev = node.prev();
            if (prev != SENTINEL && isMergeable(prev.piece, node.piece)) {
                rbDelete(this, node);
                length += prev.piece.length();
                node = merged = prev;
            }
            MarkTreeNode<T> next = node.next();
            if (next != SENTINEL && isMergeable(node.piece, next.piece)) {
                rbDelete(this, node);
                length += next.piece.length();
                merged = next;
            }
            if (merged != null) {
                updateNodeSome(merged, length - merged.piece.length());
            }
            return;
        }
        deleteStartingFrom(offset, cnt, startPosition);
        insert(offset, new Piece<>(cnt, properties));
    }

    @Nullable
    public <R> R forPropertiesIn(long offset, long cnt, boolean includeNull, IntervalConsumer<T, R> consumer) {
        return forEachMarkIn(offset, cnt, (piece, currentOffset) -> {
            if (includeNull || piece.mark() != null) {
                return consumer.accept(piece.mark(), currentOffset, piece.length());
            }
            return null;
        });
    }

    public IntervalPieceTree<T> subTree(long offset, long cnt) {
        IntervalPieceTree<T> subTree = new IntervalPieceTree<>();
        forEachMarkIn(offset, cnt, (piece, currentOffset) -> {
            subTree.insert(currentOffset - offset, piece);
            return null;
        });
        return subTree;
    }

    public void insert(long offset, MarkPieceTreeBase<T> other) {
        if (other.root == SENTINEL) {
            return;
        }
        other.forEachMarkIn(0, Long.MAX_VALUE, (piece, currentOffset) -> {
            insert(offset + currentOffset, piece);
            return null;
        });
    }

    public void insert(long offset, long cnt, @Nullable T properties) {
        if (cnt == 0) {
            return;
        }
        insert(offset, new Piece<>(cnt, properties));
    }
    //#endregion Properties

    public interface IntervalConsumer<T, R> {
        @Nullable R accept(@Nullable T properties, long offset, long cnt);
    }
}
