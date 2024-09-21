package party.iroiro.juicemacs.piecetree;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.piecetree.PieceTreeBase.EndOfLine.CR_LF;
import static party.iroiro.juicemacs.piecetree.PieceTreeBase.EndOfLine.LF;

public class PieceTreeBaseTest {
    @Test
    public void testCreateTree() {
        PieceTreeBase tree = new PieceTreeBase(List.of(
                new PieceTreeBase.StringBuffer("""
                        Hello, world!
                        Line #2
                        Line #3
                        Line #4
                        Line""", true),
                new PieceTreeBase.StringBuffer("""
                         #5
                        Line #6
                        Line #7
                        Line""", true),
                new PieceTreeBase.StringBuffer(" ", true),
                new PieceTreeBase.StringBuffer("#8\n", true),
                new PieceTreeBase.StringBuffer("""
                        Line #9
                        Line #10""", true)
        ), PieceTreeBase.EndOfLine.LF, true);
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
                tree.getLinesContent()
        );
    }

    private PieceTreeBase from(String s) {
        return new PieceTreeBase(
                List.of(new PieceTreeBase.StringBuffer(s, true)),
                PieceTreeBase.EndOfLine.LF,
                !s.contains("\r")
        );
    }

    @Test
    public void testInsert() {
        PieceTreeBase hello = from("Hello ");
        hello.insert(6, "World!", true);
        assertEquals("Hello World!", hello.getLinesRawContent());
        hello.insert(1, "ee", true);
        assertEquals("Heeello World!", hello.getLinesRawContent());
        hello.delete(3, 1);
        assertEquals("Heello World!", hello.getLinesRawContent());
        hello.delete(2, 8);
        assertEquals("Held!", hello.getLinesRawContent());
        assertEquals("H", hello.getNearestChunk(0));
    }

    @Test
    public void testGetChunk() {
        PieceTreeBase hello = from("Hello ");
        hello.insert(6, "World!", true);
        assertEquals("Hello ", hello.getNearestChunk(0));
        assertEquals(" ", hello.getNearestChunk(5));
        assertEquals("World!", hello.getNearestChunk(6));
        assertEquals("!", hello.getNearestChunk(11));
        assertEquals("", hello.getNearestChunk(12));
    }

    @Test
    public void testAppend() {
        PieceTreeBase hello = from("Hello ");
        "World!".chars().forEach(c -> hello.insert(hello.getLength(), String.valueOf((char) c), true));
        assertEquals("Hello World!", hello.getLinesRawContent());
        for (int i = 0; i < 7; i++) {
            hello.delete(hello.getLength() - 1, 1);
        }
        assertEquals("Hello", hello.getLinesRawContent());
    }

    @Test
    public void testPrepend() {
        PieceTreeBase hello = from("World!");
        " olleH".chars().forEach(c -> hello.insert(0, String.valueOf((char) c), true));
        assertEquals("Hello World!", hello.getLinesRawContent());
        for (int i = 0; i < 6; i++) {
            hello.delete(0, 1);
        }
        assertEquals("World!", hello.getLinesRawContent());
    }

    @Test
    public void testDelete() {
        PieceTreeBase hello = from("ABC");
        assertThrows(IllegalArgumentException.class, () -> hello.delete(1, 30));
    }

    @Test
    public void testEmpty() {
        PieceTreeBase hello = from("");
        assertEquals("", hello.getLinesRawContent());
        assertEquals(0, hello.getLength());
        assertEquals(1, hello.getLineCount());
        hello.insert(0, "hello", true);
        assertEquals("hello", hello.getLinesRawContent());
    }

    @Test
    public void testChangeEndOfLine() {
        PieceTreeBase hello = from("Hello\r\nWorld\r!\n!\r");
        assertEquals(5, hello.getLineCount());
        hello.setEOL(PieceTreeBase.EndOfLine.LF);
        assertEquals("Hello\nWorld\n!\n!\n", hello.getLinesRawContent());
    }

    @Test
    public void testCRThenLF() {
        PieceTreeBase tree = new PieceTreeBase(List.of(
                new PieceTreeBase.StringBuffer("Hello\r", true),
                new PieceTreeBase.StringBuffer("\nWorld\n", true)
        ), CR_LF, false);
        assertEquals("Hello\r\nWorld\n", tree.getLinesRawContent());
        assertEquals("Hello\r", tree.getLineRawContent(1, 0));
        assertEquals("\n", tree.getLineRawContent(2, 0));
        assertEquals("World\n", tree.getLineRawContent(3, 0));
        tree.setEOL(CR_LF);
        assertEquals(CR_LF, tree.getEOL());
        assertEquals("Hello\r\n\r\nWorld\r\n", tree.getLinesRawContent());
        assertEquals("Hello\r\n", tree.getLineRawContent(1, 0));
        assertEquals("\r\n", tree.getLineRawContent(2, 0));
        assertEquals("World\r\n", tree.getLineRawContent(3, 0));
    }

    @Test
    public void testInsertEndOfLine() {
        PieceTreeBase hello = from("Hello");
        hello.insert(hello.getLength(), "\r", false);
        hello.insert(hello.getLength(), "\n", false);
        assertEquals("Hello\r\n", hello.getLinesRawContent());

        hello.insert(0, "\n", false);
        hello.insert(0, "\r", false);
        assertEquals("\r\nHello\r\n", hello.getLinesRawContent());
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
        hello.insert(6, "World", true);
        hello.insert(5, ",", true);
        assertEquals("Hello, World!", hello.getLinesRawContent());
        hello.delete(0, hello.getLinesRawContent().length());
        assertEquals("", hello.getLinesRawContent());
        assertEquals(0, hello.getLength());
        assertEquals(1, hello.getLineCount());
    }

    @Test
    public void testCharSequence() {
        PieceTreeBase hello = from("H!");
        hello.insert(1, "ed", true);
        hello.insert(2, "ll", true);
        hello.insert(3, "lr", true);
        hello.insert(4, "oo", true);
        hello.insert(5, " W", true);
        assertEquals("Hello World!", hello.getLinesRawContent());
        Matcher matcher = Pattern.compile("\\wll\\w\\s+\\b").matcher(hello);
        assertTrue(matcher.find());
        assertEquals(1, matcher.start());
        assertEquals(6, matcher.end());

        IntStream chars = "Hello World!".chars();
        assertArrayEquals(chars.toArray(), hello.chars().toArray());

        String s = "🍊 🍎 🍇";
        PieceTreeBase unicode = from(s);
        assertEquals(s, unicode.getLinesRawContent());
        assertEquals(s, unicode.toString());
        assertEquals(s, unicode.getLineContent(1));
        assertEquals(s, unicode.getLineRawContent(1, 0));
        int[] codePoints = unicode.codePoints().toArray();
        assertEquals(5, codePoints.length);
        assertArrayEquals(s.codePoints().toArray(), codePoints);
        assertEquals("🍊", unicode.subSequence(0, 2));

        PieceTreeBase surrogate = from("");
        surrogate.insert(0, "" + s.charAt(0), true);
        surrogate.insert(1, "abc", true);
        surrogate.insert(4, "" + s.charAt(3), true);
        assertArrayEquals(new int[]{ s.charAt(0), 'a', 'b', 'c', s.charAt(3) }, surrogate.codePoints().toArray());
    }

    @Test
    public void testMergeCRLF() {
        PieceTreeBase hello = from("HelloWorld");
        hello.insert(5, "\r \n", false);
        hello.delete(6, 1);
        assertEquals("Hello\r\nWorld", hello.getLinesRawContent());
        hello.setEOL(CR_LF);
        assertEquals("Hello\r\nWorld", hello.getLinesRawContent());

        hello.insert(12, "\n-", false);
        hello.insert(12, "-\r", false);
        assertEquals("Hello\r\nWorld-\r\n-", hello.getLinesRawContent());
        hello.setEOL(CR_LF);
        assertEquals("Hello\r\nWorld-\r\n-", hello.getLinesRawContent());

        hello = from("test");
        hello.insert(4, "\n", false);
        hello.insert(4, "\r", false);
        assertEquals("test\r\n", hello.getLinesRawContent());
    }

    @Test
    public void testLines() {
        PieceTreeBase hello = from("Hello\r\nWorld\n!\r");
        assertEquals(4, hello.getLineCount());
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
        Arrays.fill(chars, (char) random.nextInt(Character.MAX_LOW_SURROGATE));
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

    private int[] assertValidNode(TreeNode node) {
        if (node == TreeNode.SENTINEL) {
            return new int[]{ 0, 0 };
        }
        TreeNode left = node.left;
        TreeNode right = node.right;
        if (node.color == TreeNode.RED) {
            assertEquals(TreeNode.BLACK, left.color);
            assertEquals(TreeNode.BLACK, right.color);
        }
        assertTrue(node.lf_left >= 0);
        assertTrue(node.size_left >= 0);
        int[] actualLeft = assertValidNode(left);
        assertEquals(actualLeft[0], node.lf_left);
        assertEquals(actualLeft[1], node.size_left);
        int[] actualRight = assertValidNode(right);
        return new int[]{ node.lf_left + node.piece.lineFeedCnt() + actualRight[0], node.size_left + node.piece.length() + actualRight[1] };
    }

    private int assertDepth(TreeNode node) {
        if (node == TreeNode.SENTINEL) {
            return 1;
        }
        assertEquals(assertDepth(node.left), assertDepth(node.right));
        return (node.color == TreeNode.BLACK ? 1 : 0) + assertDepth(node.left);
    }

    private void assertValidTree(PieceTreeBase tree) {
        int[] stats = assertValidNode(tree.root);
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
        assertEquals(content, tree.getLinesRawContent());
        for (int i = 0; i < 1000; i++) {
            if (random.nextBoolean()) {
                // insert
                String s = generateRandomString(random);
                int offset = random.nextInt(content.length());
                tree.insert(offset, s, !s.contains("\r"));
                content = content.substring(0, offset) + s + content.substring(offset);
            } else {
                // delete
                int offset = random.nextInt(content.length());
                int length = random.nextInt(content.length() - offset);
                tree.delete(offset, length);
                content = content.substring(0, offset) + content.substring(offset + length);
            }

            assertEquals(content, tree.getLinesRawContent(), "@" + i);

            String[] lines = content.split("\r\n|\r|\n", -1);
            assertEquals(lines.length, tree.getLineCount(), "@" + i);
            List<String> linesContent = tree.getLinesContent();
            assertEquals(Arrays.asList(lines), linesContent);
            for (int j = 0; j < 3; j++) {
                int line = test.nextInt(lines.length);
                String lineContent = tree.getLineContent(line + 1);
                assertEquals(lines[line], lineContent);
                assertTrue(Math.abs(lines[line].length() - tree.getLineLength(line + 1)) <= 1);
            }
            for (int j = 0; j < 10; j++) {
                int line = test.nextInt(lines.length);
                if (lines[line].isEmpty()) {
                    continue;
                }
                int index = test.nextInt(lines[line].length());
                char c = tree.getLineCharCode(line + 1, index);
                assertEquals(lines[line].charAt(index), c);
            }

            for (int j = 0; j < 20; j++) {
                int offset = test.nextInt(content.length());
                assertEquals(content.charAt(offset), tree.getCharCode(offset));
                PieceTreeBase.Position position = tree.getPositionAt(offset);
                int offsetAt = tree.getOffsetAt(position.line(), position.column());
                assertEquals(offset, offsetAt);
            }

            int startOffset = test.nextInt(content.length());
            int endOffset = test.nextInt(content.length() - startOffset) + startOffset;
            PieceTreeBase.Position startPosition = tree.getPositionAt(startOffset);
            PieceTreeBase.Position endPosition = tree.getPositionAt(endOffset);
            String valueInRange = tree.getValueInRange(new PieceTreeBase.Range(
                    startPosition.line(), startPosition.column(),
                    endPosition.line(), endPosition.column()
            ), LF);
            assertEquals(
                    content.substring(startOffset, endOffset).replaceAll("\r\n|\r|\n", "\n"),
                    valueInRange
            );

            assertValidTree(tree);
        }
    }
}
