package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.Rule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.rules.ErrorCollector;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.mule.MuleString;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.TestingUtils.getContextBuilder;

public class ElSemiFuzzTest {
    @Rule
    public ErrorCollector collector = new ErrorCollector();

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

    @Test
    public void testElSemiFuzz() throws IOException {
        String funcall = """
                (esfuzz-run-fuzz
                 "^data.c$"
                 "^mod$"
                 t)
                """;
        String[] commands = new String[] {
                "emacs", "-Q", "-nw", "--batch",
                "-L", ".", "-l", "el-semi-fuzz",
                "--eval", funcall,
        };
        Path cwd = Path.of("scripts", "emacs-extractor");
        Process fuzzer = new ProcessBuilder(commands)
                .directory(cwd.toFile())
                .start();
        ArrayList<Executable> failures = new ArrayList<>();
        try (InputStream is = fuzzer.getInputStream();
             Context context = getContextBuilder(System.out)
                     .option("elisp.dumpFile", "emacs.pdmp")
                     .build()) {
            Value bindings = context.getBindings("elisp");
            context.eval("elisp", """
                    (defun fuzz-case (s)
                      (let* ((case (read (decode-coding-string s 'utf-8-emacs)))
                             (type (car case))
                             (expr (nth 1 case))
                             (result (cons type (nth 2 case)))
                             (actual (condition-case err
                                         (cons ?| (eval expr))
                                       (error (cons ?! (car err))))))
                        (vector (prin1-to-string result)
                                (prin1-to-string actual))))
                    """);
            while (true) {
                int len = readInt(is);
                if (len <= 0) {
                    break;
                }
                byte[] bytes = is.readNBytes(len);
                ELispString expr = new ELispString(MuleString.fromRaw(bytes));
                bindings.putMember("args", expr);
                Value extracted = context.eval("elisp", "(fuzz-case args)");
                try {
                    assertEquals(
                            extracted.getArrayElement(0).asString(),
                            extracted.getArrayElement(1).asString(),
                            () -> new String(bytes)
                    );
                } catch (AssertionError e) {
                    System.err.println(e.getMessage());
                    failures.add(() -> {
                        throw e;
                    });
                }
            }
        } catch (RuntimeException e) {
            failures.add(() -> fail(e));
        } finally {
            fuzzer.destroy();
            assertEquals(0, failures.size());
        }
    }

    private static int readInt(InputStream is) throws IOException {
        int c = ' ';
        while (c != -1 && Character.isWhitespace(c)) {
            c = is.read();
        }
        int integer = 0;
        while (c != -1 && Character.isDigit(c)) {
            integer = integer * 10 + c - '0';
            c = is.read();
        }
        return integer;
    }
}
