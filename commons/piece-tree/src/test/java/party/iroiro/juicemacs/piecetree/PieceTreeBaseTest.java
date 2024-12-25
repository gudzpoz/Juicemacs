package party.iroiro.juicemacs.piecetree;

import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.mule.MuleString.fromString;
import static party.iroiro.juicemacs.piecetree.PieceTreeBase.EndOfLine.CR_LF;
import static party.iroiro.juicemacs.piecetree.PieceTreeBase.EndOfLine.LF;

public class PieceTreeBaseTest {
    @Test
    public void testCreateTree() {
        PieceTreeBase tree = new PieceTreeBase(List.of(
                new StringBuffer(fromString("""
                        Hello, world!
                        Line #2
                        Line #3
                        Line #4
                        Line"""), true),
                new StringBuffer(fromString("""
                         #5
                        Line #6
                        Line #7
                        Line"""), true),
                new StringBuffer(fromString(" "), true),
                new StringBuffer(fromString("#8\n"), true),
                new StringBuffer(fromString("""
                        Line #9
                        Line #10"""), true)
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
                tree.getLinesContent().stream().map(MuleString::toString).toList()
        );
    }

    private PieceTreeBase from(String s) {
        return new PieceTreeBase(
                List.of(new StringBuffer(fromString(s), true)),
                PieceTreeBase.EndOfLine.LF,
                !s.contains("\r")
        );
    }

    @Test
    public void testInsert() {
        PieceTreeBase hello = from("Hello ");
        hello.insert(6, fromString("World!"), true);
        assertEquals("Hello World!", hello.getLinesRawContent().toString());
        hello.insert(1, fromString("ee"), true);
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
        hello.insert(6, fromString("World!"), true);
        assertEquals("Hello ", hello.getNearestChunk(0).toString());
        assertEquals(" ", hello.getNearestChunk(5).toString());
        assertEquals("World!", hello.getNearestChunk(6).toString());
        assertEquals("!", hello.getNearestChunk(11).toString());
        assertEquals("", hello.getNearestChunk(12).toString());
    }

    @Test
    public void testAppend() {
        PieceTreeBase hello = from("Hello ");
        "World!".chars().forEach(c -> hello.insert(hello.getLength(), fromString(String.valueOf((char) c)), true));
        assertEquals("Hello World!", hello.getLinesRawContent().toString());
        for (int i = 0; i < 7; i++) {
            hello.delete(hello.getLength() - 1, 1);
        }
        assertEquals("Hello", hello.getLinesRawContent().toString());
    }

    @Test
    public void testPrepend() {
        PieceTreeBase hello = from("World!");
        " olleH".chars().forEach(c -> hello.insert(0, fromString(String.valueOf((char) c)), true));
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
        hello.insert(0, fromString("hello"), true);
        assertEquals("hello", hello.getLinesRawContent().toString());
    }

    @Test
    public void testChangeEndOfLine() {
        PieceTreeBase hello = from("Hello\r\nWorld\r!\n!\r");
        assertEquals(5, hello.getLineCount());
        hello.setEOL(PieceTreeBase.EndOfLine.LF);
        assertEquals("Hello\nWorld\n!\n!\n", hello.getLinesRawContent().toString());
    }

    @Test
    public void testCRThenLF() {
        PieceTreeBase tree = new PieceTreeBase(List.of(
                new StringBuffer(fromString("Hello\r"), true),
                new StringBuffer(fromString("\nWorld\n"), true)
        ), CR_LF, false);
        assertEquals("Hello\r\nWorld\n", tree.getLinesRawContent().toString());
        assertEquals("Hello\r", tree.getLineRawContent(1, 0).toString());
        assertEquals("\n", tree.getLineRawContent(2, 0).toString());
        assertEquals("World\n", tree.getLineRawContent(3, 0).toString());
        tree.setEOL(CR_LF);
        assertEquals(CR_LF, tree.getEOL());
        assertEquals("Hello\r\n\r\nWorld\r\n", tree.getLinesRawContent().toString());
        assertEquals("Hello\r\n", tree.getLineRawContent(1, 0).toString());
        assertEquals("\r\n", tree.getLineRawContent(2, 0).toString());
        assertEquals("World\r\n", tree.getLineRawContent(3, 0).toString());
    }

    @Test
    public void testInsertEndOfLine() {
        PieceTreeBase hello = from("Hello");
        hello.insert(hello.getLength(), fromString("\r"), false);
        hello.insert(hello.getLength(), fromString("\n"), false);
        assertEquals("Hello\r\n", hello.getLinesRawContent().toString());

        hello.insert(0, fromString("\n"), false);
        hello.insert(0, fromString("\r"), false);
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
        hello.insert(6, fromString("World"), true);
        hello.insert(5, fromString(","), true);
        assertEquals("Hello, World!", hello.getLinesRawContent().toString());
        hello.delete(0, hello.getLinesRawContent().length());
        assertEquals("", hello.getLinesRawContent().toString());
        assertEquals(0, hello.getLength());
        assertEquals(1, hello.getLineCount());
    }

    @Test
    public void testCharSequence() {
        PieceTreeBase hello = from("H!");
        hello.insert(1, fromString("ed"), true);
        hello.insert(2, fromString("ll"), true);
        hello.insert(3, fromString("lr"), true);
        hello.insert(4, fromString("oo"), true);
        hello.insert(5, fromString(" W"), true);
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
    public void testMergeCRLF() {
        PieceTreeBase hello = from("HelloWorld");
        hello.insert(5, fromString("\r \n"), false);
        hello.delete(6, 1);
        assertEquals("Hello\r\nWorld", hello.getLinesRawContent().toString());
        hello.setEOL(CR_LF);
        assertEquals("Hello\r\nWorld", hello.getLinesRawContent().toString());

        hello.insert(12, fromString("\n-"), false);
        hello.insert(12, fromString("-\r"), false);
        assertEquals("Hello\r\nWorld-\r\n-", hello.getLinesRawContent().toString());
        hello.setEOL(CR_LF);
        assertEquals("Hello\r\nWorld-\r\n-", hello.getLinesRawContent().toString());

        hello = from("test");
        hello.insert(4, fromString("\n"), false);
        hello.insert(4, fromString("\r"), false);
        assertEquals("test\r\n", hello.getLinesRawContent().toString());
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
                tree.insert(offset, fromString(s), !s.contains("\r"));
                content = content.substring(0, offset) + s + content.substring(offset);
            } else {
                // delete
                int offset = random.nextInt(content.length());
                int length = random.nextInt(content.length() - offset);
                tree.delete(offset, length);
                content = content.substring(0, offset) + content.substring(offset + length);
            }

            assertEquals(content, tree.getLinesRawContent().toString(), "@" + i);

            String[] lines = content.split("\r\n|\r|\n", -1);
            assertEquals(lines.length, tree.getLineCount(), "@" + i);
            List<MuleString> linesContent = tree.getLinesContent();
            assertEquals(Arrays.asList(lines), linesContent.stream().map(MuleString::toString).toList());
            for (int j = 0; j < 3; j++) {
                int line = test.nextInt(lines.length);
                MuleString lineContent = tree.getLineContent(line + 1);
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
            MuleString valueInRange = tree.getValueInRange(range, LF);
            String substring = content.substring(
                    content.offsetByCodePoints(0, startOffset),
                    content.offsetByCodePoints(0, endOffset)
            );
            try {
                assertEquals(
                        substring.replaceAll("\r\n|\r|\n", "\n"),
                        valueInRange.toString(),
                        substring.replace("\r", "<cr>\n")
                                .replace("\n", "<lf>\n")
                );
            } catch (AssertionError e) {
                tree.getValueInRange(range, LF);
                throw e;
            }

            assertValidTree(tree);
        }
    }
}
