package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public abstract class BaseFormTest {

    /// @return test entries like `["(+ 1 1)", 2, "(expr)", expectedResult, ...]`
    protected abstract Object[] entries();

    /// Usage: Set [#WARM_UP] to `true` to warm up the test and enable
    /// `-Dpolyglot.engine.TraceCompilation=true` to see if there are any
    /// compilation errors (e.g. Truffle bailing out).
    private final static boolean WARM_UP = true;
    private final static int WARM_UP_COUNT = 10000;

    public static Context.Builder getTestingContextBuilder() {
        return Context.newBuilder("elisp")
                .environment("EMACSLOADPATH", Path.of("emacs", "lisp").toAbsolutePath().toString())
                .environment("EMACSDATA", Path.of("emacs", "etc").toAbsolutePath().toString())
                .allowIO(IOAccess.ALL);
    }

    public static Context getTestingContext() {
        return Context.newBuilder("elisp")
                .environment("EMACSLOADPATH", Path.of("emacs", "lisp").toAbsolutePath().toString())
                .environment("EMACSDATA", Path.of("emacs", "etc").toAbsolutePath().toString())
                .allowIO(IOAccess.ALL)
                .build();
    }

    @Test
    public void test() {
        try (Context context = getTestingContext()) {
            Object[] entries = entries();
            for (int i = 0; i < entries.length; i += 2) {
                String expr = (String) entries[i];
                Object expected = entries[i + 1];
                try {
                    assertEquals(expected, context.eval("elisp", expr).as(expected.getClass()), expr);
                    if (WARM_UP && !expr.contains(";; no-warm-up-test")) {
                        System.out.println(i + ": " + expr);
                        for (int j = 0; j < WARM_UP_COUNT; j++) {
                            context.eval("elisp", expr + " ;; run#" + (j + 1));
                        }
                    }
                } catch (Exception e) {
                    fail(expr, e);
                }
            }
        }
    }

}
