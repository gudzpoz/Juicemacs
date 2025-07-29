package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.TestingUtils.getContextBuilder;

public class BuiltInCodingTest {
    @Test
    public void testDecodeCoding() {
        try (Context context = getContextBuilder(System.out).option("elisp.dumpFile", "emacs.pdmp").build()) {
            Value s = context.eval("elisp", """
                    (decode-coding-string "\344\270\255" 'utf-8-emacs-unix)
                    """);
            String unicode = "中";
            System.out.println(Arrays.toString(unicode.getBytes(StandardCharsets.UTF_8)));
            System.out.println(Arrays.toString(s.asString().getBytes(StandardCharsets.UTF_8)));
            assertEquals(unicode, s.asString());
        }
    }
}
