package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public abstract class BaseFormTest {

    /**
     * @return test entries like {@code ["(+ 1 1)", 2, "(expr)", expectedResult, ...]}
     */
    protected abstract Object[] entries();

    @Test
    public void test() {
        try (Context context = Context.newBuilder("elisp")
                .environment("ELISP_PATH", "")
                .build()
        ) {
            Object[] entries = entries();
            for (int i = 0; i < entries.length; i += 2) {
                String expr = (String) entries[i];
                Object expected = entries[i + 1];
                try {
                    assertEquals(expected, context.eval("elisp", expr).as(expected.getClass()), expr);
                } catch (Exception e) {
                    fail(expr, e);
                }
            }
        }
    }

}
