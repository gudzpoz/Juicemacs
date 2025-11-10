package party.iroiro.juicemacs.piecetree;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

public class StringBufferTest {
    public final static String[] SAMPLES = {
            "abc",
            "àb",
            "ààà",
            "Παν語",
            "😄",
    };
    public final static String[] LINE_BREAKS = {"\r", "\n", "\r\n"};

    @Test
    public void testCharCounts() {
        for (String s1 : SAMPLES) {
            for (String s2 : LINE_BREAKS) {
                for (String s3 : SAMPLES) {
                    String[] test = {s1, s2, s3};
                    for (int i = 0; i < test.length; i++) {
                        String init = Arrays.stream(test).limit(i).collect(Collectors.joining());
                        StringBuffer buffer = StringBuffer.mutable(init.getBytes(StandardCharsets.UTF_8), false);
                        for (int j = i; j < test.length; j++) {
                            buffer.append(test[j].getBytes(StandardCharsets.UTF_8));
                        }
                        String expected = String.join("", test);
                        assertEquals(expected.codePoints().count(), buffer.length());
                        assertEquals(expected.getBytes(StandardCharsets.UTF_8).length, buffer.bytes());
                        assertEquals(expected, new String(buffer.substring(0, buffer.length())));
                    }
                }
            }
        }
    }

    @Test
    public void testIndices() {
        int half = 5120;
        String longText = "😄".repeat(half);
        StringBuffer buffer = StringBuffer.mutable(longText.getBytes(StandardCharsets.UTF_8), false);
        buffer.append(longText.getBytes(StandardCharsets.UTF_8));
        assertEquals(half * 2, buffer.length());
        assertEquals("😄", new String(buffer.substring(0, 1)));
        assertEquals("😄", new String(buffer.substring(0, 1)));
        for (int i = 0; i < half; i++) {
            assertEquals(longText, new String(buffer.substring(i, half)));
        }
    }
}
