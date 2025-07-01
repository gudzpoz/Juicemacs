package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;

public class BuiltInLReadTest {
    @Test
    void testLocateOpenPNoDirectory() {
        try (Context context = getTestingContext()) {
            Value value = context.eval(
                    "elisp",
                    "(locate-file-internal \"org\" load-path)"
            );
            assertEquals("false", value.toString());
        }
    }
}
