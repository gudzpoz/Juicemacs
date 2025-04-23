package party.iroiro.juicemacs.piecetree.meta;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;

import party.iroiro.juicemacs.piecetree.meta.MarkPieceTreeBase.Piece;

import static org.junit.jupiter.api.Assertions.*;

class IntervalPieceTreeTest {
    @Test
    public void simpleRangeTest() {
        IntervalPieceTree<Integer> tree = new IntervalPieceTree<>();
        tree.insert(0, new Piece<>(16, null));
        for (int i = 1; i < 8; i++) {
            tree.putPropertiesFor(i, 4, i);
        }
        tree.insert(8, new Piece<>(1, null));
        assertNull(tree.getPropertiesAt(0));
        for (int i = 1; i < 8; i++) {
            assertEquals(i, tree.getPropertiesAt(i));
        }
        assertNull(tree.getPropertiesAt(8));
        for (int i = 9; i < 12; i++) {
            assertEquals(7, tree.getPropertiesAt(i));
        }
        for (int i = 12; i < 17; i++) {
            assertNull(tree.getPropertiesAt(i));
        }

        assertIndices(tree, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12);
    }

    @Test
    public void testMerge() {
        IntervalPieceTree<Integer> tree = new IntervalPieceTree<>();
        tree.insert(0, new Piece<>(16, null));
        tree.insert(8, new Piece<>(8, 1));
        assertIndices(tree, 0, 8, 16);
        assertNull(tree.getPropertiesAt(0));
        assertEquals(1, tree.getPropertiesAt(8));

        tree.delete(0, 4);
        tree.delete(16, 4);
        assertIndices(tree, 0, 4, 12);
        assertNull(tree.getPropertiesAt(0));
        assertEquals(1, tree.getPropertiesAt(4));
        assertNull(tree.getPropertiesAt(12));

        tree.delete(6, 4);
        assertIndices(tree, 0, 4, 8);
        assertNull(tree.getPropertiesAt(0));
        assertEquals(1, tree.getPropertiesAt(4));
        assertNull(tree.getPropertiesAt(8));

        tree.delete(4, 4);
        assertIndices(tree, 0);
        assertNull(tree.getPropertiesAt(0));
    }

    @Test
    public void testInterTree() {
        IntervalPieceTree<Integer> tree1 = new IntervalPieceTree<>();
        tree1.insert(0, new Piece<>(4, null));
        tree1.insert(2, new Piece<>(4, 1));
        tree1.insert(4, new Piece<>(2, 2));
        assertIndices(tree1, 0, 2, 4, 6, 8);
        IntervalPieceTree<Integer> tree2 = tree1.subTree(2, 6);
        assertIndices(tree2, 0, 2, 4);
        assertEquals(1, tree2.getPropertiesAt(0));
        assertEquals(2, tree2.getPropertiesAt(2));
        assertEquals(1, tree2.getPropertiesAt(4));

        IntervalPieceTree<Integer> tree3 = new IntervalPieceTree<>();
        tree3.insert(0, new Piece<>(4, null));
        tree3.insert(2, new IntervalPieceTree<>());
        tree3.insert(2, tree2);
        assertIndices(tree3, 0, 2, 4, 6, 8);
        assertEquals(1, tree3.getPropertiesAt(2));
        assertEquals(2, tree3.getPropertiesAt(4));
        assertEquals(1, tree3.getPropertiesAt(6));

        assertEquals("""
                [0,1]: null
                [2,3]: 1
                [4,5]: 2
                [6,7]: 1
                [8,9]: null
                """, tree3.toString());
    }

    @Test
    public void testPutProperties() {
        IntervalPieceTree<Integer> tree = new IntervalPieceTree<>();
        tree.insert(0, 16, null);
        tree.insert(8, 8, 1);
        tree.putPropertiesFor(8, 8, 2);
        assertEquals(2, tree.getPropertiesAt(8));
        assertIndices(tree, 0, 8, 16);

        tree.putPropertiesFor(0, 8, 3);
        tree.putPropertiesFor(16, 8, 3);
        tree.putPropertiesFor(8, 8, 3);
        assertEquals(3, tree.getPropertiesAt(0));
        assertIndices(tree, 0);

        tree.putPropertiesFor(0, 16, 4);
        assertIndices(tree, 0, 16);
        tree.putPropertiesFor(16, 8, 4);
        assertEquals(4, tree.getPropertiesAt(0));
        assertIndices(tree, 0);

        tree.putPropertiesFor(16, 8, 5);
        assertIndices(tree, 0, 16);
        tree.putPropertiesFor(0, 16, 5);
        assertEquals(5, tree.getPropertiesAt(0));
        assertIndices(tree, 0);
    }

    private void assertIndices(IntervalPieceTree<Integer> tree, long... expected) {
        ArrayList<Long> indices = new ArrayList<>();
        tree.forEachMarkIn(0, Long.MAX_VALUE, (_, offset) -> indices.add(offset));
        assertArrayEquals(expected, indices.stream().mapToLong((l) -> l).toArray());
    }

    @Test
    public void testOutOfBound() {
        IntervalPieceTree<Integer> tree = new IntervalPieceTree<>();
        tree.insert(0, new Piece<>(16, null));
        assertThrows(IllegalArgumentException.class, () -> tree.getPropertiesAt(16));
    }
}
