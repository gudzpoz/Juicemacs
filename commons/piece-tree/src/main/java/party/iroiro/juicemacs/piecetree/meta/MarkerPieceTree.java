package party.iroiro.juicemacs.piecetree.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.MarkerArray;

import java.util.*;

import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.SENTINEL;
import static party.iroiro.juicemacs.piecetree.meta.MarkTreeNode.rbDelete;

/// A piece tree that supports markers
///
/// This "marker piece tree" is a bit different from both [IntervalPieceTree]
/// and [party.iroiro.juicemacs.piecetree.PieceTreeBase]: each piece now keeps
/// track of two things - the markers at the end of the piece, and the text
/// before those markers, delimited by markers.
///
/// Uhh, for example, the following buffer:
///
/// ```
/// (m1)text(m2)(m3)hello world!(m4)(m5)
/// ```
///
/// ...is represented by three pieces:
///
/// ```
/// Piece 1: {length: 0, markers: [m1]}
/// Piece 2: {length: 4, markers: [m2, m3]} // text
/// Piece 3: {length: 12, markers: [m4, m5]} // hello world!
/// ```
public final class MarkerPieceTree<T> extends MarkPieceTreeBase<MarkerArray> {
    private @Nullable MarkerMovedListener listener;
    private T buffer;
    private final HashSet<Marker> markers;
    private long length = 0;

    public MarkerPieceTree(MarkerMovedListener listener, T buffer) {
        this.listener = listener;
        this.markers = new HashSet<>();
        this.buffer = buffer;
    }

    public T getBuffer() {
        return buffer;
    }

    public void setBuffer(T buffer) {
        this.buffer = buffer;
    }

    @Nullable
    public MarkerMovedListener getListener() {
        return listener;
    }

    public void setListener(MarkerMovedListener listener) {
        this.listener = listener;
    }

    public HashSet<Marker> getMarkers() {
        return markers;
    }

    @TruffleBoundary
    public void insertMarker(long offset, Marker marker) {
        marker.detach();
        markers.add(marker);
        if (root == SENTINEL) {
            this.length = offset;
            insertLeft(null, marker, offset);
            return;
        }
        if (offset > this.length) {
            insertRight(root.rightest(), marker, offset - this.length);
            this.length = offset;
            return;
        }

        NodePosition<MarkerArray> nodePosition = nodeAt(offset);
        MarkTreeNode<MarkerArray> node = nodePosition.node;
        long remainder = nodePosition.remainder;
        long nodeStartOffset = nodePosition.nodeStartOffset;

        Piece<MarkerArray> piece = node.piece;
        if (nodeStartOffset == offset) {
            MarkTreeNode<MarkerArray> prev = node.prev();
            if (prev == SENTINEL) {
                if (piece.length != 0) {
                    insertLeft(node, marker, 0);
                    return;
                }
            } else {
                node = prev;
            }
            node.piece.mark.insert(marker);
        } else if (nodeStartOffset + piece.length > offset) {
            updateNodeSome(node, -remainder);
            insertLeft(node, marker, remainder);
        } else {
            node.piece.mark.insert(marker);
        }
    }

    private void insertLeft(@Nullable MarkTreeNode<MarkerArray> node, Marker marker, long length) {
        MarkerArray array = new MarkerArray(this, marker);
        Piece<MarkerArray> piece = new Piece<>(length, array);
        array.node = rbInsertLeft(node, piece);
    }
    private void insertRight(MarkTreeNode<MarkerArray> node, Marker marker, long length) {
        MarkerArray array = new MarkerArray(this, marker);
        Piece<MarkerArray> piece = new Piece<>(length, array);
        array.node = rbInsertRight(node, piece);
    }

    public void insertString(long offset, long cnt) {
        if (cnt == 0 || offset > this.length || root == SENTINEL) {
            return;
        }
        this.length += cnt;

        NodePosition<MarkerArray> nodePosition = nodeAt(offset);
        MarkTreeNode<MarkerArray> node = nodePosition.node;
        long nodeStartOffset = nodePosition.nodeStartOffset;

        Piece<MarkerArray> piece = node.piece;
        MarkTreeNode<MarkerArray> split;
        if (nodeStartOffset == offset) {
            MarkTreeNode<MarkerArray> prev = node.prev();
            if (prev == SENTINEL) {
                if (piece.length == 0) {
                    split = node;
                } else {
                    updateNodeSome(node, cnt);
                    return;
                }
            } else {
                split = prev;
            }
        } else if (nodeStartOffset + piece.length > offset) {
            updateNodeSome(node, cnt);
            return;
        } else {
            split = node;
        }

        MarkerArray markers = split.piece.mark;
        markers.trySplit(split, this, cnt);
    }

    public void delete(long offset, long cnt) {
        if (offset >= this.length || cnt <= 0) {
            return;
        }
        assert root != SENTINEL;
        long end = Math.min(this.length, offset + cnt);
        this.length -= end - offset;

        NodePosition<MarkerArray> startPosition = nodeAt(offset);
        MarkTreeNode<MarkerArray> startNode = startPosition.node;
        long remainder = startPosition.remainder;

        if (remainder == 0) {
            MarkTreeNode<MarkerArray> prev = startNode.prev();
            if (prev != SENTINEL) {
                startNode = prev;
                remainder = prev.piece.length;
            }
        }

        NodePosition<MarkerArray> endPosition = nodeAt(end);
        MarkTreeNode<MarkerArray> endNode = endPosition.node;

        if (startNode == endNode) {
            // remainder == 0: only if the node is the leftest, where we allow zero length;
            // remainder > 0: updated node always has length > 0.
            // either way, we don't need to merge nodes
            long delta = remainder - endPosition.remainder;
            updateNodeSome(startNode, delta);
            if (endPosition.remainder == endNode.piece.length) {
                // when deletion overlaps with markers, we need explicit entries in undo logs.
                startNode.piece.mark.notifyMovement(listener, delta);
            }
            return;
        }

        ArrayList<Marker> leftMarks = new ArrayList<>();
        ArrayList<Marker> rightMarks = new ArrayList<>();
        ArrayList<MarkTreeNode<MarkerArray>> nodesToDel = new ArrayList<>();
        long delta = 0;
        if (remainder != startNode.piece.length) {
            delta = remainder - startNode.piece.length;
            updateNodeSome(startNode, delta);
            startNode.piece.mark.notifyMovement(listener, delta);
        }
        startNode.piece.mark.splitInto(leftMarks, rightMarks);

        for (MarkTreeNode<MarkerArray> node = startNode.next(); node != endNode; node = node.next()) {
            assert node != SENTINEL;
            delta -= node.piece.length;
            MarkerArray array = node.piece.mark;
            array.notifyMovement(listener, delta);
            array.splitInto(leftMarks, rightMarks);
            nodesToDel.add(node);
        }

        // update last touched node
        if (endPosition.remainder != 0) {
            MarkerArray array = endNode.piece.mark;
            delta -= endPosition.remainder;
            array.notifyMovement(listener, delta);
            if (endPosition.remainder == endNode.piece.length) {
                array.splitInto(leftMarks, rightMarks);
                nodesToDel.add(endNode);
            } else {
                updateNodeSome(endNode, -endPosition.remainder);
            }
        }

        deleteNodes(nodesToDel);
        startNode.piece.mark.mergeMarks(leftMarks, rightMarks);
    }

    public enum Affinity {
        LEFT, RIGHT
    }

    public interface MarkerMovedListener {
        void onMarkerMoved(Marker marker, long delta);
    }

    /// Marker data directly stored by a node
    ///
    /// Note that it tries to avoid array allocation by keeping
    /// [#markers] as a single marker or an array of markers.
    static final class MarkerArray {
        /// [Marker] or an array of [Marker Markers]
        Object markers;
        MarkTreeNode<MarkerArray> node;
        final MarkerPieceTree<?> tree;

        @SuppressWarnings("DataFlowIssue")
        MarkerArray(MarkerPieceTree<?> tree, Marker init) {
            this.node = null; // initialized later
            this.tree = tree;
            this.markers = init;
            init.parent = this;
        }
        @SuppressWarnings("DataFlowIssue")
        MarkerArray(MarkerPieceTree<?> tree, Marker[] init) {
            this.node = null; // initialized later
            this.tree = tree;
            for (Marker marker : init) {
                marker.parent = this;
            }
            this.markers = init.length == 1 ? init[0] : init;
        }

        private static int indexOf(Marker[] markers, Marker marker) {
            for (int i = 0; i < markers.length; i++) {
                if (markers[i] == marker) {
                    return i;
                }
            }
            throw new IllegalArgumentException();
        }

        /// When there are point marks in between, this function rearranges the point marks
        /// so that all [Affinity#LEFT] marks are before all [Affinity#RIGHT] marks.
        @TruffleBoundary
        void sortChangedMarker(Marker marker) {
            if (this.markers == marker) {
                return;
            }
            Marker[] markers = (Marker[]) this.markers;
            final int index = indexOf(markers, marker);
            int i = index;
            // marker.affinity == LEFT:
            // - find the farthest RIGHT affinity marker to the left, and swap.
            // marker.affinity == RIGHT:
            // - find the farthest LEFT affinity marker to the right, and swap.
            if (marker.affinity == Affinity.LEFT) {
                while (i >= 0) {
                    if (i == 0 || markers[i - 1].affinity == Affinity.LEFT) {
                        markers[index] = markers[i];
                        markers[i] = marker;
                        break;
                    }
                    i--;
                }
            } else {
                while (i < markers.length) {
                    if (i == markers.length - 1 || markers[i + 1].affinity == Affinity.RIGHT) {
                        markers[index] = markers[i];
                        markers[i] = marker;
                        break;
                    }
                    i++;
                }
            }
        }

        void remove(Marker marker) {
            tree.markers.remove(marker);
            if (markers != marker) {
                Marker[] old = (Marker[]) markers;
                if (old.length != 1) {
                    int index = indexOf(old, marker);
                    Marker[] markers = new Marker[old.length - 1];
                    System.arraycopy(old, 0, markers, 0, index);
                    System.arraycopy(old, index + 1, markers, index, old.length - index - 1);
                    this.markers = markers.length == 1 ? markers[0] : markers;
                    return;
                }
            }
            MarkTreeNode<MarkerArray> next = node.next();
            if (next == SENTINEL) {
                tree.length -= node.piece.length;
            } else {
                tree.updateNodeSome(next, node.piece.length);
            }
            rbDelete(tree, node);
        }

        void insert(Marker marker) {
            marker.parent = this;
            if (markers instanceof Marker prev) {
                markers = prev.affinity == Affinity.RIGHT && marker.affinity == Affinity.LEFT
                        ? new Marker[]{marker, prev}
                        : new Marker[]{prev, marker};
            } else {
                Marker[] old = (Marker[]) this.markers;
                Marker[] markers = new Marker[old.length + 1];
                if (marker.affinity == Affinity.LEFT) {
                    markers[0] = marker;
                    System.arraycopy(old, 0, markers, 1, old.length);
                } else {
                    markers[markers.length - 1] = marker;
                    System.arraycopy(old, 0, markers, 0, old.length);
                }
                this.markers = markers;
            }
        }

        void trySplit(MarkTreeNode<MarkerArray> node, MarkerPieceTree<?> tree, long cnt) {
            Affinity affinity;
            if (markers instanceof Marker marker) {
                affinity = marker.affinity;
            } else {
                Marker[] markers = (Marker[]) this.markers;
                int firstRight = -1;
                for (int i = 0; i < markers.length; i++) {
                    if (markers[i].affinity == Affinity.RIGHT) {
                        firstRight = i;
                        break;
                    }
                }
                if (firstRight == 0) {
                    // all affinity is RIGHT
                    affinity = Affinity.RIGHT;
                } else if (firstRight == -1) {
                    // all affinity is LEFT
                    affinity = Affinity.LEFT;
                } else  {
                    // split needed
                    Marker[] rightMarkers = Arrays.copyOfRange(markers, firstRight, markers.length);
                    MarkerArray array = new MarkerArray(tree, rightMarkers);
                    Piece<MarkerArray> piece = new Piece<>(cnt, array);
                    array.node = tree.rbInsertRight(node, piece);
                    this.markers = firstRight == 1 ? markers[0] : Arrays.copyOf(markers, firstRight);
                    return;
                }
            }

            if (affinity == Affinity.RIGHT) {
                tree.updateNodeSome(node, cnt);
            } else {
                MarkTreeNode<MarkerArray> next = node.next();
                if (next == SENTINEL) {
                    tree.length -= cnt;
                } else {
                    tree.updateNodeSome(next, cnt);
                }
            }
        }

        void notifyMovement(MarkerPieceTree.@Nullable MarkerMovedListener listener, long delta) {
            if (listener == null) {
                return;
            }
            if (markers instanceof Marker marker) {
                listener.onMarkerMoved(marker, delta);
            } else {
                Marker[] markers = (Marker[]) this.markers;
                for (Marker marker : markers) {
                    listener.onMarkerMoved(marker, delta);
                }
            }
        }

        void splitInto(ArrayList<Marker> leftMarks, ArrayList<Marker> rightMarks) {
            if (markers instanceof Marker marker) {
                if (marker.affinity == Affinity.LEFT) {
                    leftMarks.add(marker);
                } else {
                    rightMarks.add(marker);
                }
            } else {
                Marker[] markers = (Marker[]) this.markers;
                for (Marker marker : markers) {
                    if (marker.affinity == Affinity.LEFT) {
                        leftMarks.add(marker);
                    } else {
                        rightMarks.add(marker);
                    }
                }
            }
        }

        public void mergeMarks(ArrayList<Marker> leftMarks, ArrayList<Marker> rightMarks) {
            if (leftMarks.size() + rightMarks.size() == 1) {
                markers = leftMarks.isEmpty() ? rightMarks.getFirst() : leftMarks.getFirst();
                return;
            }

            Marker[] markers = new Marker[leftMarks.size() + rightMarks.size()];
            int i = 0;
            for (Marker marker : leftMarks) {
                markers[i++] = marker;
            }
            for (Marker marker : rightMarks) {
                markers[i++] = marker;
            }
            this.markers = markers;
            for (Marker marker : markers) {
                marker.parent = this;
            }
        }

        @Override
        public String toString() {
            if (markers instanceof Marker marker) {
                return marker.toString();
            } else {
                return Arrays.toString((Marker[]) markers);
            }
        }
    }

    public static final class Marker {
        @Nullable
        MarkerArray parent = null;
        private Affinity affinity;

        public Marker(Affinity affinity) {
            this.affinity = affinity;
        }

        public long position() {
            if (this.parent == null) {
                return -1;
            }
            MarkTreeNode<MarkerArray> parent = this.parent.node;
            long offset = parent.size_left + parent.piece.length;
            while (true) {
                MarkTreeNode<MarkerArray> child = parent;
                parent = parent.parent;
                if (parent == SENTINEL) {
                    return offset;
                }
                if (child == parent.right) {
                    offset += parent.size_left + parent.piece.length;
                }
            }
        }

        public Affinity affinity() {
            return affinity;
        }

        public void setAffinity(Affinity affinity) {
            Affinity prev = this.affinity;
            this.affinity = affinity;
            if (prev != affinity && parent != null) {
                parent.sortChangedMarker(this);
            }
        }

        @TruffleBoundary
        public void detach() {
            if (parent != null) {
                parent.remove(this);
                parent = null;
            }
        }

        public boolean isDetached() {
            return parent == null;
        }

        @Nullable
        public MarkerPieceTree<?> tree() {
            return parent == null ? null : parent.tree;
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
