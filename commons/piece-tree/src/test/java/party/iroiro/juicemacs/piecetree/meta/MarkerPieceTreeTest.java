package party.iroiro.juicemacs.piecetree.meta;

import org.junit.jupiter.api.Test;

import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.Marker;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

class MarkerPieceTreeTest {
    @Test
    public void testAffinity() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        Marker m1 = new Marker(MarkerPieceTree.Affinity.LEFT);
        tree.insertMarker(4, m1);
        Marker m2 = new Marker(MarkerPieceTree.Affinity.RIGHT);
        tree.insertMarker(4, m2);
        tree.insertString(4, 4);
        assertEquals(4, m1.position());
        assertEquals(8, m2.position());

        tree.delete(1, 2);
        assertEquals(2, m1.position());
        assertEquals(6, m2.position());
    }

    @Test
    public void testStartEnd() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        Marker m1 = new Marker(MarkerPieceTree.Affinity.LEFT);
        Marker m2 = new Marker(MarkerPieceTree.Affinity.RIGHT);
        tree.insertMarker(0, m1);
        tree.insertMarker(0, m2);
        assertEquals("""
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=RIGHT}
                [0,7]: null
                """, tree.toString());

        tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(0, m2);
        tree.insertMarker(0, m1);
        assertEquals("""
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=RIGHT}
                [0,7]: null
                """, tree.toString());

        tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(8, m1);
        tree.insertMarker(8, m2);
        assertEquals("""
                [0,7]: null
                [8]: Marker{pos=8, affinity=LEFT}
                [8]: Marker{pos=8, affinity=RIGHT}
                """, tree.toString());

        tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(8, m2);
        tree.insertMarker(8, m1);
        assertEquals("""
                [0,7]: null
                [8]: Marker{pos=8, affinity=LEFT}
                [8]: Marker{pos=8, affinity=RIGHT}
                """, tree.toString());
    }

    private static MarkerPieceTree<?> getTree() {
        return new MarkerPieceTree<>((_, _) -> {
        }, null);
    }

    @Test
    public void testLeft() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 4);
        tree.insertMarker(4, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.insertString(4, 4);
        tree.insertMarker(4, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertString(4, 4);
        assertEquals("""
                [0,3]: null
                [4]: Marker{pos=4, affinity=LEFT}
                [4,7]: null
                [8]: Marker{pos=8, affinity=RIGHT}
                [8,11]: null
                """, tree.toString());

        tree = getTree();
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.RIGHT));
        assertEquals("""
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=RIGHT}
                [0]: Marker{pos=0, affinity=RIGHT}
                """, tree.toString());
    }

    @Test
    public void testRight() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 4);
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertString(0, 4);
        System.out.println(tree);
        tree.insertMarker(4, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.insertString(4, 4);
        assertEquals("""
                [0,3]: null
                [4]: Marker{pos=4, affinity=LEFT}
                [4,7]: null
                [8]: Marker{pos=8, affinity=RIGHT}
                [8,11]: null
                """, tree.toString());

        tree = getTree();
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.insertMarker(0, new Marker(MarkerPieceTree.Affinity.LEFT));
        assertEquals("""
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=RIGHT}
                [0]: Marker{pos=0, affinity=RIGHT}
                """, tree.toString());
    }

    @Test
    public void testDeleteMiddle() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(2, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertMarker(4, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.insertMarker(4, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertMarker(6, new Marker(MarkerPieceTree.Affinity.LEFT));
        tree.delete(2, 4);
        assertEquals("""
                [0,1]: null
                [2]: Marker{pos=2, affinity=LEFT}
                [2]: Marker{pos=2, affinity=LEFT}
                [2]: Marker{pos=2, affinity=RIGHT}
                [2]: Marker{pos=2, affinity=RIGHT}
                [2,3]: null
                """, tree.toString());
    }

    @Test
    public void testForAll() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(8, new Marker(MarkerPieceTree.Affinity.RIGHT));
        tree.insertMarker(8, new Marker(MarkerPieceTree.Affinity.LEFT));
        ArrayList<MarkPieceTreeBase.Piece<Marker>> pieces = new ArrayList<>();
        tree.forEachMarkIn(0, 8, (piece, _) -> {
            pieces.add(piece);
            return null;
        });
        assertEquals(3, pieces.size());
        assertNull(pieces.getFirst().mark());
        assertEquals(MarkerPieceTree.Affinity.LEFT, Objects.requireNonNull(pieces.get(1).mark()).affinity());
        assertEquals(MarkerPieceTree.Affinity.RIGHT, Objects.requireNonNull(pieces.get(2).mark()).affinity());
    }

    @Test
    public void testAffinityChange() {
        MarkerPieceTree<?> tree = getTree();
        Marker l = new Marker(MarkerPieceTree.Affinity.LEFT);
        tree.insertMarker(0, l);
        Marker r = new Marker(MarkerPieceTree.Affinity.RIGHT);
        tree.insertMarker(0, r);
        assertEquals("""
                [0]: Marker{pos=0, affinity=LEFT}
                [0]: Marker{pos=0, affinity=RIGHT}
                """, tree.toString());

        l.setAffinity(MarkerPieceTree.Affinity.RIGHT);
        r.setAffinity(MarkerPieceTree.Affinity.LEFT);
        ArrayList<Marker> markers = new ArrayList<>();
        tree.forEachMarkIn(0, 1, (piece, _) -> {
            markers.add(Objects.requireNonNull(piece.mark()));
            return null;
        });
        assertArrayEquals(new Marker[]{r, l}, markers.toArray());
    }

    @Test
    public void testDelete() {
        MarkerPieceTree<?> tree = getTree();
        Random random = new Random(0);
        ArrayList<Marker> markers = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            Marker m = new Marker(random.nextBoolean() ? MarkerPieceTree.Affinity.LEFT : MarkerPieceTree.Affinity.RIGHT);
            markers.add(m);
            tree.insertMarker(0, m);
        }
        for (int i = 1; i < 1000; i++) {
            markers.get(i).detach();
        }
        ArrayList<Marker> remaining = new ArrayList<>();
        tree.forEachMarkIn(0, 0, (piece, _) -> remaining.add(Objects.requireNonNull(piece.mark())));
        assertEquals(1, remaining.size());
        assertEquals(markers.getFirst(), remaining.getFirst());
    }
}
