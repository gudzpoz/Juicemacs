package party.iroiro.juicemacs.piecetree;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

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
    }

    @Test
    public void testDelete() {
        PieceTreeBase hello = from("ABC");
        assertThrows(IllegalArgumentException.class, () -> hello.delete(1, 30));
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

    @Test
    public void testSimpleFuzz() {
        Random random = new Random(0);
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
        }
    }
}
