package party.iroiro.juicemacs.piecetree.meta;

import org.jspecify.annotations.NonNull;
import org.junit.jupiter.api.Test;

import party.iroiro.juicemacs.piecetree.meta.MarkPieceTreeBase.Piece;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.Affinity;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.Marker;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.MarkerArray;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class MarkerPieceTreeTest {
    @Test
    public void testAffinity() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        Marker m1 = new Marker(Affinity.LEFT);
        tree.insertMarker(4, m1);
        Marker m2 = new Marker(Affinity.RIGHT);
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
        Marker m1 = new Marker(Affinity.LEFT);
        Marker m2 = new Marker(Affinity.RIGHT);
        tree.insertMarker(0, m1);
        tree.insertMarker(0, m2);
        assertEquals("""
                [0]: [Marker{pos=0, affinity=LEFT}, Marker{pos=0, affinity=RIGHT}]
                """, tree.toString());

        tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(0, m2);
        tree.insertMarker(0, m1);
        assertEquals("""
                [0]: [Marker{pos=0, affinity=LEFT}, Marker{pos=0, affinity=RIGHT}]
                """, tree.toString());

        tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(8, m1);
        tree.insertMarker(8, m2);
        assertEquals("""
                [0,7]: [Marker{pos=8, affinity=LEFT}, Marker{pos=8, affinity=RIGHT}]
                """, tree.toString());

        tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(8, m2);
        tree.insertMarker(8, m1);
        assertEquals("""
                [0,7]: [Marker{pos=8, affinity=LEFT}, Marker{pos=8, affinity=RIGHT}]
                """, tree.toString());
    }

    @SuppressWarnings("DataFlowIssue")
    private static MarkerPieceTree<?> getTree() {
        return new MarkerPieceTree<>((_, _) -> {
        }, null);
    }

    @Test
    public void testLeft() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 4);
        tree.insertMarker(4, new Marker(Affinity.LEFT));
        tree.insertString(4, 4);
        tree.insertMarker(4, new Marker(Affinity.RIGHT));
        tree.insertString(4, 4);
        assertEquals("""
                [0,3]: Marker{pos=4, affinity=LEFT}
                [4,7]: Marker{pos=8, affinity=RIGHT}
                """, tree.toString());

        tree = getTree();
        tree.insertMarker(0, new Marker(Affinity.LEFT));
        tree.insertMarker(0, new Marker(Affinity.LEFT));
        tree.insertMarker(0, new Marker(Affinity.RIGHT));
        tree.insertMarker(0, new Marker(Affinity.RIGHT));
        assertEquals("""
                [0]: [Marker{pos=0, affinity=LEFT}, Marker{pos=0, affinity=LEFT}, \
                Marker{pos=0, affinity=RIGHT}, Marker{pos=0, affinity=RIGHT}]
                """, tree.toString());
    }

    @Test
    public void testRight() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 4);
        tree.insertMarker(0, new Marker(Affinity.RIGHT));
        tree.insertString(0, 4);
        assertEquals("""
                [0,3]: Marker{pos=4, affinity=RIGHT}
                """, tree.toString());
        tree.insertMarker(4, new Marker(Affinity.LEFT));
        tree.insertString(4, 4);
        assertEquals("""
                [0,3]: Marker{pos=4, affinity=LEFT}
                [4,7]: Marker{pos=8, affinity=RIGHT}
                """, tree.toString());

        tree = getTree();
        tree.insertMarker(0, new Marker(Affinity.RIGHT));
        tree.insertMarker(0, new Marker(Affinity.RIGHT));
        tree.insertMarker(0, new Marker(Affinity.LEFT));
        tree.insertMarker(0, new Marker(Affinity.LEFT));
        assertEquals("""
                [0]: [Marker{pos=0, affinity=LEFT}, Marker{pos=0, affinity=LEFT}, \
                Marker{pos=0, affinity=RIGHT}, Marker{pos=0, affinity=RIGHT}]
                """, tree.toString());
    }

    @Test
    public void testMergeNull() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        tree.insertString(4, 4);
        tree.insertString(4, 4);
        assertEquals("""
                """, tree.toString());
    }

    @Test
    public void testDeleteMiddle() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(2, new Marker(Affinity.RIGHT));
        tree.insertMarker(4, new Marker(Affinity.LEFT));
        tree.insertMarker(4, new Marker(Affinity.RIGHT));
        tree.insertMarker(6, new Marker(Affinity.LEFT));
        tree.delete(2, 4);
        assertEquals("""
                [0,1]: [Marker{pos=2, affinity=LEFT}, Marker{pos=2, affinity=LEFT}, \
                Marker{pos=2, affinity=RIGHT}, Marker{pos=2, affinity=RIGHT}]
                """, tree.toString());
    }

    @Test
    public void testForAll() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(8, new Marker(Affinity.RIGHT));
        tree.insertMarker(8, new Marker(Affinity.LEFT));
        ArrayList<Piece<MarkerArray>> pieces = new ArrayList<>();
        tree.forEachMarkIn(0, 8, (piece, _) -> {
            pieces.add(piece);
            return null;
        });
        assertEquals(1, pieces.size());
        MarkerArray array = Objects.requireNonNull(pieces.getFirst().mark);
        Marker[] markers = (Marker[]) array.markers;
        assertEquals(Affinity.LEFT, markers[0].affinity());
        assertEquals(Affinity.RIGHT, markers[1].affinity());
    }

    @Test
    public void testForAllEdges() {
        MarkerPieceTree<?> tree = getTree();
        tree.insertString(0, 8);
        tree.insertMarker(0, new Marker(Affinity.LEFT));
        tree.insertMarker(8, new Marker(Affinity.RIGHT));
        tree.insertMarker(4, new Marker(Affinity.LEFT));
        tree.insertMarker(4, new Marker(Affinity.RIGHT));
        tree.insertString(4, 4);
        assertEquals(12, tree.getLength());
        ArrayList<MarkerArray> arrays = new ArrayList<>();
        tree.forEachMarkIn(0, 12, (piece, _) -> {
            arrays.add(piece.mark);
            return null;
        });
        assertEquals(4, arrays.size());
        assertEquals("Marker{pos=0, affinity=LEFT}", arrays.get(0).toString());
        assertEquals("Marker{pos=4, affinity=LEFT}", arrays.get(1).toString());
        assertEquals("Marker{pos=8, affinity=RIGHT}", arrays.get(2).toString());
        assertEquals("Marker{pos=12, affinity=RIGHT}", arrays.get(3).toString());
    }

    @Test
    public void testAffinityChange() {
        MarkerPieceTree<?> tree = getTree();
        Marker l = new Marker(Affinity.LEFT);
        tree.insertMarker(0, l);
        Marker r = new Marker(Affinity.RIGHT);
        tree.insertMarker(0, r);
        assertEquals("""
                [0]: [Marker{pos=0, affinity=LEFT}, Marker{pos=0, affinity=RIGHT}]
                """, tree.toString());

        l.setAffinity(Affinity.RIGHT);
        r.setAffinity(Affinity.LEFT);
        ArrayList<MarkerArray> markers = new ArrayList<>();
        tree.forEachMarkIn(0, 1, (piece, _) -> {
            markers.add(Objects.requireNonNull(piece.mark));
            return null;
        });
        assertEquals(1, markers.size());
        assertArrayEquals(new Marker[]{r, l}, (Marker[]) markers.getFirst().markers);
    }

    @Test
    public void testDelete() {
        MarkerPieceTree<?> tree = getTree();
        Random random = new Random(0);
        ArrayList<Marker> markers = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            Marker m = new Marker(random.nextBoolean() ? Affinity.LEFT : Affinity.RIGHT);
            markers.add(m);
            tree.insertMarker(0, m);
        }
        for (int i = 1; i < 1000; i++) {
            markers.get(i).detach();
        }
        ArrayList<MarkerArray> remaining = new ArrayList<>();
        tree.forEachMarkIn(0, 0, (piece, _) ->
                remaining.add(Objects.requireNonNull(piece.mark)));
        assertEquals(1, remaining.size());
        assertEquals(markers.getFirst(), remaining.getFirst().markers);
    }

    @Test
    public void testRandom() {
        Random random = new Random(0);
        MarkerPieceTree<?> tree = getTree();
        String expect = ""; // " ": space, "<" or ">": markers
        int expectedLength = 0;
        int markerCount = 0;
        for (int i = 0; i < 500000; i++) {
            switch (random.nextInt(4)) {
                case 0 -> {
                    // insert string
                    int where = expectedLength == 0 ? 0 : random.nextInt(expectedLength);
                    int length = random.nextInt(10);
                    expect = markStringInsertAt(expect, where, " ".repeat(length));
                    tree.insertString(where, length);
                    expectedLength += length;
                }
                case 1 -> {
                    // insert marker
                    int where = expectedLength == 0 ? 0 : random.nextInt(expectedLength);
                    Marker m = new Marker(random.nextBoolean() ? Affinity.LEFT : Affinity.RIGHT);
                    expect = markStringInsertAt(expect, where, m.affinity() == Affinity.LEFT ? "<" : ">");
                    tree.insertMarker(where, m);
                    markerCount++;
                }
                case 2 -> {
                    // delete string
                    if (expectedLength == 0) {
                        continue;
                    }
                    int start = random.nextInt(expectedLength);
                    int end = random.nextInt(expectedLength - start) + start;
                    expect = markStringDelete(expect, start, end);
                    tree.delete(start, end - start);
                    expectedLength -= end - start;
                }
                case 3 -> {
                    // delete marker
                    if (markerCount == 0) {
                        continue;
                    }
                    int removed = random.nextInt(markerCount);
                    int index;
                    int count = -1;
                    for (index = 0; index < expect.length(); index++) {
                        char c = expect.charAt(index);
                        if (c != ' ') {
                            count++;
                        }
                        if (count == removed) {
                            break;
                        }
                    }
                    char markerC = expect.charAt(index);
                    expect = expect.substring(0, index) + expect.substring(index + 1);
                    int finalIndex = index - count;
                    Marker marker = tree.getMarkers().stream()
                            .filter((m) -> (m.affinity() == Affinity.LEFT) == (markerC == '<')
                                    && m.position() == finalIndex)
                            .findFirst().orElseThrow();
                    marker.detach();
                    markerCount--;
                }
            }


            assertEquals(expectedLength + markerCount, expect.length());
            int actualLength = Math.toIntExact(tree.getLength());
            assertTrue(expectedLength >= actualLength);
            if (expectedLength > actualLength) {
                int tail = expectedLength - actualLength;
                assertTrue(expect.substring(expect.length() - tail).isBlank());
            }
            int[] index = {0};
            String expectS = expect;
            tree.forEachMarkIn(0, actualLength, (m, _) -> {
                int length = Math.toIntExact(m.length);
                String spaces = expectS.substring(index[0], index[0] + length);
                assertEquals(" ".repeat(length), spaces);
                index[0] += length;

                assertNotNull(m.mark);
                Object markers = m.mark.markers;
                if (markers instanceof Marker marker) {
                    assertEquals(expectS.charAt(index[0]), marker.affinity() == Affinity.LEFT ? '<' : '>');
                    index[0]++;
                } else {
                    Marker[] array = (Marker[]) markers;
                    assertNotEquals(0, array.length);
                    StringBuilder brackets = new StringBuilder(array.length);
                    for (Marker marker : array) {
                        char c = marker.affinity() == Affinity.LEFT ? '<' : '>';
                        assertTrue(brackets.isEmpty() || brackets.charAt(brackets.length() - 1) <= c);
                        brackets.append(c);
                    }
                    assertEquals(expectS.substring(index[0], index[0] + brackets.length()), brackets.toString());
                    index[0] += array.length;
                }
                return null;
            });
        }
    }

    private static @NonNull String markStringDelete(String expect, int start, int end) {
        StringBuilder sb = new StringBuilder(expect.length());
        int count = 0, left = 0, right = 0;
        for (int j = 0; j < expect.length(); j++) {
            char c = expect.charAt(j);
            if (start <= count && count <= end) {
                if (c == '<') {
                    left++;
                } else if (c == '>') {
                    right++;
                } else if (j == expect.length() - 1 || (c == ' ' && count == end)) {
                    // commit
                    sb.append("<".repeat(left));
                    sb.append(">".repeat(right));
                    if (count == end) {
                        sb.append(c);
                    }
                }
            } else {
                sb.append(c);
            }
            if (c == ' ') {
                count++;
            }
        }
        expect = sb.toString();
        return expect;
    }

    private String markStringInsertAt(String s, int index, String insertion) {
        StringBuilder sb = new StringBuilder(s.length() + insertion.length());
        int count = 0, i = 0;
        while (true) {
            if (count == index && !(i < s.length() && s.charAt(i) == '<')) {
                sb.append(insertion);
                count++;
            }
            if (i >= s.length()) {
                break;
            }
            char c = s.charAt(i);
            if (c == ' ') {
                count++;
            }
            sb.append(c);
            i++;
        }
        return sb.toString();
    }
}
