package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.Arrays;
import java.util.List;
import java.util.PrimitiveIterator;
import java.util.Random;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

public class PieceTreeBaseTest {
    private static TruffleString fromString(String s) {
        return TruffleString.fromJavaStringUncached(s, TruffleString.Encoding.UTF_32);
    }

    @Test
    public void testCreateTree() {
        PieceTreeBase tree = new PieceTreeBase(
                new StringBuffer(fromString("""
                        Hello, world!
                        Line #2
                        Line #3
                        Line #4
                        Line""")),
                new StringBuffer(fromString("""
                         #5
                        Line #6
                        Line #7
                        Line""")),
                new StringBuffer(fromString(" ")),
                new StringBuffer(fromString("#8\n")),
                new StringBuffer(fromString("""
                        Line #9
                        Line #10"""))
        );
        assertLinesMatch(
                List.of(
                        "Hello, world!",
                        "Line #2",
                        "Line #3",
                        "Line #4",
                        "Line #5",
                        "Line #6",
                        "Line #7",
                        "Line #8",
                        "Line #9",
                        "Line #10"
                ),
                tree.getLinesContent().stream().map(AbstractTruffleString::toString).toList()
        );
    }

    private PieceTreeBase from(String s) {
        return new PieceTreeBase(new StringBuffer(fromString(s)));
    }

    @Test
    public void testInsert() {
        PieceTreeBase hello = from("Hello ");
        hello.insert(6, fromString("World!"));
        assertEquals("Hello World!", hello.getLinesRawContent().toString());
        hello.insert(1, fromString("ee"));
        assertEquals("Heeello World!", hello.getLinesRawContent().toString());
        hello.delete(3, 1);
        assertEquals("Heello World!", hello.getLinesRawContent().toString());
        hello.delete(2, 8);
        assertEquals("Held!", hello.getLinesRawContent().toString());
        assertEquals("H", hello.getNearestChunk(0).toString());
    }

    @Test
    public void testGetChunk() {
        PieceTreeBase hello = from("Hello ");
        hello.insert(6, fromString("World!"));
        assertEquals("Hello ", hello.getNearestChunk(0).toString());
        assertEquals(" ", hello.getNearestChunk(5).toString());
        assertEquals("World!", hello.getNearestChunk(6).toString());
        assertEquals("!", hello.getNearestChunk(11).toString());
        assertEquals("", hello.getNearestChunk(12).toString());
    }

    @Test
    public void testAppend() {
        PieceTreeBase hello = from("Hello ");
        "World!".chars().forEach(c -> hello.insert(hello.getLength(), fromString(String.valueOf((char) c))));
        assertEquals("Hello World!", hello.getLinesRawContent().toString());
        for (int i = 0; i < 7; i++) {
            hello.delete(hello.getLength() - 1, 1);
        }
        assertEquals("Hello", hello.getLinesRawContent().toString());
    }

    @Test
    public void testPrepend() {
        PieceTreeBase hello = from("World!");
        " olleH".chars().forEach(c -> hello.insert(0, fromString(String.valueOf((char) c))));
        assertEquals("Hello World!", hello.getLinesRawContent().toString());
        for (int i = 0; i < 6; i++) {
            hello.delete(0, 1);
        }
        assertEquals("World!", hello.getLinesRawContent().toString());
    }

    @Test
    public void testDelete() {
        PieceTreeBase hello = from("ABC");
        assertThrows(IllegalArgumentException.class, () -> hello.delete(1, 30));
    }

    @Test
    public void testEmpty() {
        PieceTreeBase hello = from("");
        assertEquals("", hello.getLinesRawContent().toString());
        assertEquals(0, hello.getLength());
        assertEquals(1, hello.getLineCount());
        hello.insert(0, fromString("hello"));
        assertEquals("hello", hello.getLinesRawContent().toString());
    }


    @Test
    public void testInsertEndOfLine() {
        PieceTreeBase hello = from("Hello");
        hello.insert(hello.getLength(), fromString("\r"));
        hello.insert(hello.getLength(), fromString("\n"));
        assertEquals("Hello\r\n", hello.getLinesRawContent().toString());

        hello.insert(0, fromString("\n"));
        hello.insert(0, fromString("\r"));
        assertEquals("\r\nHello\r\n", hello.getLinesRawContent().toString());
    }

    @Test
    public void testPositions() {
        PieceTreeBase hello = from("Hello");
        assertEquals(new PieceTreeBase.Position(1, 6), hello.getPositionAt(1000));
        assertEquals(new PieceTreeBase.Position(1, 1), hello.getPositionAt(-1000));

        assertEquals(new PieceTreeBase.Position(1, 1), from("").getPositionAt(0));
        assertEquals(0, from("").getOffsetAt(1, 1));
    }

    @Test
    public void testDeleteAll() {
        PieceTreeBase hello = from("Hello !");
        hello.insert(6, fromString("World"));
        hello.insert(5, fromString(","));
        assertEquals("Hello, World!", hello.getLinesRawContent().toString());
        hello.delete(0, hello.getLinesRawContent().codePointLengthUncached(TruffleString.Encoding.UTF_32));
        assertEquals("", hello.getLinesRawContent().toString());
        assertEquals(0, hello.getLength());
        assertEquals(1, hello.getLineCount());
    }

    @Test
    public void testCharSequence() {
        PieceTreeBase hello = from("H!");
        hello.insert(1, fromString("ed"));
        hello.insert(2, fromString("ll"));
        hello.insert(3, fromString("lr"));
        hello.insert(4, fromString("oo"));
        hello.insert(5, fromString(" W"));
        assertEquals("Hello World!", hello.getLinesRawContent().toString());

        IntStream chars = "Hello World!".chars();
        assertArrayEquals(chars.toArray(), hello.chars().toArray());

        String s = "🍊 🍎 🍇";
        PieceTreeBase unicode = from(s);
        assertEquals(s, unicode.getLinesRawContent().toString());
        assertEquals(s, unicode.getLineContent(1).toString());
        assertEquals(s, unicode.getLineRawContent(1, 0).toString());
        int[] codePoints = unicode.chars().toArray();
        assertEquals(5, codePoints.length);
        assertArrayEquals(s.codePoints().toArray(), codePoints);
        assertEquals("🍊".codePointAt(0), unicode.getCharCode(0));
    }

    @Test
    public void testLines() {
        PieceTreeBase hello = from("Hello\r\nWorld\n!\r");
        assertEquals(3, hello.getLineCount());
    }

    private String generateRandomString(Random random) {
        int length;
        if (random.nextInt(20) == 0) {
            length = random.nextInt(10 * (int) Short.MAX_VALUE) + 4 * (int) Short.MAX_VALUE;
        } else {
            length = random.nextInt(100) + 1;
        }
        return generateRandomString(random, length);
    }

    private String generateRandomString(Random random, int length) {
        char[] chars = new char[length];
        Arrays.fill(chars, (char) random.nextInt(Character.MIN_HIGH_SURROGATE));
        int minLineLength = random.nextInt(5);
        int maxLineLength = Math.max(40, length >> 2);
        for (int i = 0; i < length; i++) {
            i += random.nextInt(maxLineLength - minLineLength) + minLineLength;
            if (i >= length) {
                break;
            }
            switch (random.nextInt(3)) {
                case 0 -> chars[i] = '\n';
                case 1 -> chars[i] = '\r';
                case 2 -> {
                    chars[i] = '\n';
                    if (random.nextBoolean() && 0 < i) {
                        chars[i - 1] = '\r';
                    }
                }
            }
        }
        return new String(chars);
    }

    private long[] assertValidNode(TreeNode node) {
        if (node == TreeNode.SENTINEL) {
            return new long[]{ 0, 0 };
        }
        TreeNode left = node.left;
        TreeNode right = node.right;
        if (node.color == TreeNode.RED) {
            assertEquals(TreeNode.BLACK, left.color);
            assertEquals(TreeNode.BLACK, right.color);
        }
        assertTrue(node.lf_left >= 0);
        assertTrue(node.size_left >= 0);
        long[] actualLeft = assertValidNode(left);
        assertEquals(actualLeft[0], node.lf_left);
        assertEquals(actualLeft[1], node.size_left);
        long[] actualRight = assertValidNode(right);
        return new long[]{ node.lf_left + node.piece.lineFeedCnt() + actualRight[0], node.size_left + node.piece.length() + actualRight[1] };
    }

    private int assertDepth(TreeNode node) {
        if (node == TreeNode.SENTINEL) {
            return 1;
        }
        assertEquals(assertDepth(node.left), assertDepth(node.right));
        return (node.color == TreeNode.BLACK ? 1 : 0) + assertDepth(node.left);
    }

    private void assertValidTree(PieceTreeBase tree) {
        long[] stats = assertValidNode(tree.root);
        assertEquals(stats[0] + 1, tree.getLineCount());
        assertEquals(stats[1], tree.getLength());
        assertDepth(tree.root);
    }

    @Test
    public void testSimpleFuzz() {
        Random random = new Random(0);
        Random test = new Random(1);
        String content = generateRandomString(random, 10 * Short.MAX_VALUE);
        PieceTreeBase tree = from(content);
        assertEquals(content, tree.getLinesRawContent().toString());
        for (int i = 0; i < 1000; i++) {
            if (random.nextBoolean()) {
                // insert
                String s = generateRandomString(random);
                int offset = random.nextInt(content.length());
                tree.insert(offset, fromString(s));
                content = content.substring(0, offset) + s + content.substring(offset);
            } else {
                // delete
                int offset = random.nextInt(content.length());
                int length = random.nextInt(content.length() - offset);
                tree.delete(offset, length);
                content = content.substring(0, offset) + content.substring(offset + length);
            }

            assertEquals(content, tree.getLinesRawContent().toString(), "@" + i);

            String[] lines = content.split("\n", -1);
            assertEquals(lines.length, tree.getLineCount(), "@" + i);
            List<TruffleString> linesContent = tree.getLinesContent();
            assertEquals(Arrays.asList(lines), linesContent.stream().map(TruffleString::toString).toList(), "@" + i);
            for (int j = 0; j < 3; j++) {
                int line = test.nextInt(lines.length);
                TruffleString lineContent = tree.getLineContent(line + 1);
                assertEquals(lines[line], lineContent.toString());
                assertTrue(Math.abs(lines[line].length() - tree.getLineLength(line + 1)) <= 1);
            }
            for (int j = 0; j < 10; j++) {
                int line = test.nextInt(lines.length);
                if (lines[line].isEmpty()) {
                    continue;
                }
                int index = test.nextInt(lines[line].length());
                int c = tree.getLineCharCode(line + 1, index);
                assertEquals(lines[line].codePointAt(index), c);
            }

            for (int j = 0; j < 20; j++) {
                int offset = test.nextInt(content.length());
                assertEquals(content.charAt(offset), tree.getCharCode(offset));
                PieceTreeBase.Position position = tree.getPositionAt(offset);
                long offsetAt = tree.getOffsetAt(position.line(), position.column());
                assertEquals(offset, offsetAt);
            }

            int startOffset = test.nextInt(content.length());
            int endOffset = test.nextInt(content.length() - startOffset) + startOffset;
            PieceTreeBase.Position startPosition = tree.getPositionAt(startOffset);
            PieceTreeBase.Position endPosition = tree.getPositionAt(endOffset);
            PieceTreeBase.Range range = new PieceTreeBase.Range(
                    startPosition.line(), startPosition.column(),
                    endPosition.line(), endPosition.column()
            );
            TruffleString valueInRange = tree.getValueInRange(range);
            String substring = content.substring(
                    content.offsetByCodePoints(0, startOffset),
                    content.offsetByCodePoints(0, endOffset)
            );
            try {
                assertEquals(
                        substring,
                        valueInRange.toString(),
                        substring.replace("\r", "<cr>\n")
                                .replace("\n", "<lf>\n")
                );
            } catch (AssertionError e) {
                tree.getValueInRange(range);
                throw e;
            }

            assertValidTree(tree);
        }
    }

    @Test
    public void wholeFileTest() throws IOException {
        File file = new File("../../elisp/emacs/lisp/ldefs-boot.el");
        StringBuilder sb = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            sb.append(reader.readLine()).append('\n');
        }
        String content = sb.toString();
        PieceTreeBase tree = from("");
        tree.insert(0, fromString(content));
        assertEquals(content, tree.getLinesRawContent().toString());
    }

    @Test
    public void emacsCharTest() {
        PieceTreeBase tree = new PieceTreeBase();
        TruffleString s = TruffleString.fromIntArrayUTF32Uncached(new int[]{
                0x3FFF80,
                0x3FFFA0,
                0x3FFFFF,
        });
        tree.insert(0, s);
        assertEquals(3, tree.getLength());
        assertEquals(0x3FFF80, tree.getCharCode(0));
        assertEquals(0x3FFFA0, tree.getCharCode(1));
        assertEquals(0x3FFFFF, tree.getCharCode(2));
        assertTrue(s.equalsUncached(tree.getLinesRawContent(), TruffleString.Encoding.UTF_32));
        assertTrue(s.equalsUncached(tree.getLineContent(1), TruffleString.Encoding.UTF_32));
    }

    @Test
    public void iteratorTest() {
        PieceTreeBase tree = from("abc\ndef\nghi\njkl\n");
        PrimitiveIterator.OfInt i = tree.iterator(4, 8);
        assertEquals('d', i.nextInt());
        assertEquals('e', i.nextInt());
        assertEquals('f', i.nextInt());
        assertEquals('\n', i.nextInt());
        assertFalse(i.hasNext());
    }
}
