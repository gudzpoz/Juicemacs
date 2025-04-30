package party.iroiro.juicemacs.piecetree.meta;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.SENTINEL;
import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.rbDelete;

public final class MarkerPieceTree<T> extends MarkPieceTreeBase<MarkerPieceTree.Marker> {
    private final MarkerMovedListener listener;
    private final HashSet<Marker> markers;
    private final T buffer;

    public MarkerPieceTree(MarkerMovedListener listener, T buffer) {
        this.listener = listener;
        this.markers = new HashSet<>();
        this.buffer = buffer;
    }

    public T getBuffer() {
        return buffer;
    }

    @CompilerDirectives.TruffleBoundary
    public void insertMarker(long offset, Marker marker) {
        marker.detach();
        insert(offset, new Piece<>(0, marker));
        markers.add(marker);
    }

    public void insertString(long offset, long cnt) {
        if (cnt == 0) {
            return;
        }
        insert(offset, new Piece<>(cnt, null));
    }

    @Override
    protected boolean isMergeable(Piece<Marker> value, Piece<Marker> piece) {
        return false;
    }

    @Nullable
    private static Affinity getAffinity(Piece<Marker> piece) {
        //noinspection DataFlowIssue
        return piece.length() == 0 ? piece.mark().affinity : null;
    }

    /// When there are point marks in between, this function rearranges the point marks
    /// so that all [Affinity#LEFT] marks are before all [Affinity#RIGHT] marks.
    @CompilerDirectives.TruffleBoundary
    private static void sortPointMarksNearby(MarkTreeNode<Marker> some) {
        MarkTreeNode<Marker> left = some;
        while (true) {
            MarkTreeNode<Marker> prev = left.prev();
            if (prev == SENTINEL || prev.piece.length() != 0) {
                break;
            }
            left = prev;
        }

        while (getAffinity(left.piece) == Affinity.LEFT) {
            MarkTreeNode<Marker> next = left.next();
            if (next == SENTINEL || next.piece.length() != 0) {
                return;
            }
            left = next;
        }

        MarkTreeNode<Marker> right = left;
        while (true) {
            MarkTreeNode<Marker> next = right.next();
            if (next == SENTINEL || next.piece.length() != 0) {
                return;
            }

            right = next;
            Piece<Marker> rightPiece = right.piece;
            if (getAffinity(rightPiece) == Affinity.LEFT) {
                // swap left and right
                right.piece = left.piece;
                left.piece = rightPiece;
                //noinspection DataFlowIssue
                left.piece.mark().node = left;
                //noinspection DataFlowIssue
                right.piece.mark().node = right;
                left = left.next();
            }
        }
    }

    @Override
    protected void deleteInBetween(NodePosition<Marker> startPosition, NodePosition<Marker> endPosition) {
        MarkTreeNode<Marker> startNode = startPosition.node();
        MarkTreeNode<Marker> endNode = endPosition.node();

        // we need to reserve point marks
        List<MarkTreeNode<Marker>> nodesToDel = new ArrayList<>();
        MarkTreeNode<Marker> pointMark = startNode.prev();
        if (pointMark == SENTINEL || pointMark.piece.length() != 0) {
            // condition 1: (point mark) [region to delete] (point marks): should resort marks
            pointMark = null;
        }

        long lengthToDelete = startPosition.node().piece.length() - startPosition.remainder();
        if (lengthToDelete != 0) {
            updateNodeSome(startNode, -lengthToDelete);
            if (startNode.piece.length() == 0) {
                // an interval node is deleted
                nodesToDel.add(startNode);
            } else {
                pointMark = null;
            }
        } else if (startNode.piece.length() == 0) {
            // condition 2: [region to delete (startNode: point mark) ...] (point marks): should resort marks
            pointMark = startNode;
        }

        // delete interval nodes in between
        long markOffset = -lengthToDelete;
        MarkTreeNode<Marker> secondNode = startNode.next();
        for (MarkTreeNode<Marker> node = secondNode; node != endNode && node != SENTINEL; node = node.next()) {
            long length = node.piece.length();
            if (length == 0) {
                // a point mark: report position change, but do not delete
                //noinspection DataFlowIssue
                listener.onMarkerMoved(node.piece.mark(), markOffset);
                pointMark = node;
            } else {
                nodesToDel.add(node);
                markOffset -= length;
            }
        }

        // update last touched node
        if (endPosition.remainder() != 0) {
            updateNodeSome(endNode, -endPosition.remainder());
            if (endNode.piece.length() == 0) {
                // an interval node is deleted
                nodesToDel.add(endNode);
            }
        }
        deleteNodes(nodesToDel);

        if (pointMark != null) {
            sortPointMarksNearby(pointMark);
        }
    }

    @Override
    protected MarkTreeNode<Marker> rbInsertRight(MarkTreeNode<Marker> node, Piece<Marker> piece) {
        node = adjustInsertionPoint(node, piece, Affinity.RIGHT);
        if (node == SENTINEL) {
            return updateMarkerNode(super.rbInsertLeft(root.leftest(), piece));
        }
        if (tryMerge(node, piece)) {
            return node;
        }
        return updateMarkerNode(super.rbInsertRight(node, piece));
    }

    @Override
    protected MarkTreeNode<Marker> rbInsertLeft(@Nullable MarkTreeNode<Marker> node, Piece<Marker> piece) {
        if (node != null) {
            node = adjustInsertionPoint(node, piece, Affinity.LEFT);
            if (node == SENTINEL) {
                return updateMarkerNode(super.rbInsertRight(root.rightest(), piece));
            }
            if (tryMerge(node, piece)) {
                return node;
            }
        }
        return updateMarkerNode(super.rbInsertLeft(node, piece));
    }

    private boolean tryMerge(MarkTreeNode<Marker> node, Piece<Marker> piece) {
        if (piece.mark() == null && node.piece.mark() == null) {
            updateNodeSome(node, piece.length());
            return true;
        }
        return false;
    }

    /// Spaghetti code to find the insertion point for point marks
    private MarkTreeNode<Marker> adjustInsertionPoint(MarkTreeNode<Marker> node, Piece<Marker> piece, Affinity where) {
        // This method should prevent point marks getting inserted in an unsorted way,
        // that is, point marks with [Affinity#LEFT] should be before point marks with [Affinity#RIGHT].
        // Also, when an interval is inserted, it should be inserted between LEFT and RIGHT point marks.

        Affinity pieceAffinity = getAffinity(piece);
        Affinity currentAffinity = getAffinity(node.piece);
        if (currentAffinity == pieceAffinity && pieceAffinity != null) {
            // The result is always [L][L] or [R][R], which are obviously sorted.
            return node;
        }

        // In the following comments, we use (L), (R) or (=) to denote the being inserted `piece`.
        if (pieceAffinity != null) {
            return findDifferingAffinity(
                    node,
                    // Differing affinity, that is:
                    // [=][L]...[R](L)[R][=] or [=][L](R)[L]...[R][=]
                    // So when pieceAffinity is (L), we search to the left [L], looking for an [L];
                    // or to the right [R] otherwise, looking for an [R]/[=].
                    // See [#findDifferingAffinity] #1 and #2.
                    pieceAffinity,
                    // When we arrive at the appropriate insertion point:
                    // ... => [=][L][L1](?)[R1][R][=]
                    // If where == [L] (insert on the left side of the returned node), we want to return [R1];
                    // if where == [R], we want to return [L1].
                    // That is, if we search to the right [R] and where == [L], we should return the next node, thus
                    // inclusive == true.
                    // To summarize, when pieceAffinity != where, return the next node (see [#findDifferingAffinity] #3);
                    // otherwise, return the current node (#4).
                    pieceAffinity != where
            );
        }
        // The inserted piece is an interval.
        Affinity direction = currentAffinity == null
                // If the current node is an interval, then the search direction depends on `where`.
                // If where == [L], then it is ...[L][R](=)[=] => ...[L](=)[R][=] (searching to the left);
                // if where == [R], then it is [=](=)[L][R]... => [=][L](=)[R]... (searching to the right).
                ? where
                // If the current node is a point mark, then:
                // If it is ...[L][R](=)[R] => ...[L](=)[R][R] (searching to the left);
                // if it is [L](=)[L][R]... => [L][L](=)[R]... (searching to the right).
                // That is, when surrounded by [R], search to the left, otherwise search to the right.
                : currentAffinity == Affinity.LEFT ? Affinity.RIGHT : Affinity.LEFT;
        return findDifferingAffinity(node, direction, direction != where);
    }

    private MarkTreeNode<Marker> findDifferingAffinity(MarkTreeNode<Marker> node, Affinity direction, boolean inclusive) {
        while (true) {
            MarkTreeNode<Marker> next = inclusive ? node : direction.next(node); // #1, #3
            if (next == SENTINEL // We treat SENTINEL as [=], which is handled in [#rbInsertLeft/Right]
                    || next.piece.length() != 0 // #2
                    || getAffinity(next.piece) == direction // #2
            ) {
                return node; // #4
            }
            node = inclusive ? direction.next(node) : next; // #3
        }
    }

    private MarkTreeNode<Marker> updateMarkerNode(MarkTreeNode<Marker> node) {
        Marker mark = node.piece.mark();
        if (mark != null) {
            mark.node = node;
            mark.tree = this;
        }
        return node;
    }

    public enum Affinity {
        LEFT {
            @Override
            public MarkTreeNode<Marker> next(MarkTreeNode<Marker> node) {
                return node.prev();
            }
        },
        RIGHT {
            @Override
            public MarkTreeNode<Marker> next(MarkTreeNode<Marker> node) {
                return node.next();
            }
        };

        public abstract MarkTreeNode<Marker> next(MarkTreeNode<Marker> node);
    }

    public interface MarkerMovedListener {
        void onMarkerMoved(Marker marker, long delta);
    }

    public static final class Marker {
        @Nullable
        private MarkTreeNode<Marker> node = null;

        @Nullable
        private MarkerPieceTree<?> tree = null;

        private Affinity affinity;

        public Marker(Affinity affinity) {
            this.affinity = affinity;
        }

        public long position() {
            MarkTreeNode<Marker> parent = node;
            if (parent == null) {
                return -1;
            }
            long offset = parent.size_left;
            while (true) {
                MarkTreeNode<Marker> child = parent;
                parent = parent.parent;
                if (parent == SENTINEL) {
                    return offset;
                }
                if (child == parent.right) {
                    offset += parent.size_left + parent.piece.length();
                }
            }
        }

        public Affinity affinity() {
            return affinity;
        }

        public void setAffinity(Affinity affinity) {
            this.affinity = affinity;
            if (node != null) {
                sortPointMarksNearby(node);
            }
        }

        @CompilerDirectives.TruffleBoundary
        public void detach() {
            if (node != null) {
                assert tree != null;
                tree.markers.remove(this);
                rbDelete(tree, node);
                tree = null;
                node = null;
            }
        }

        @Nullable
        public MarkerPieceTree<?> tree() {
            return tree;
        }

        @Override
        public String toString() {
            return "Marker{" +
                    "pos=" + position() +
                    ", affinity=" + affinity +
                    '}';
        }
    }
}
