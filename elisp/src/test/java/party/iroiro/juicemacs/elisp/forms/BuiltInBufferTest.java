package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;

public class BuiltInBufferTest {
    @Test
    public void bufferStringTest() {
        try (Context context = getTestingContext()) {
            File file = new File("emacs/lisp/ldefs-boot.el");
            Value value = context.eval("elisp", """
                    (set-buffer (get-buffer-create " *test*"))
                    (insert-file-contents "emacs/lisp/ldefs-boot.el")
                    (length (buffer-string))
                    """);
            assertEquals(file.length(), value.asLong());
        }
    }
}
