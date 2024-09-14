package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class BuiltInEvalTest {
    private static final Object[] TESTS = new Object[]{
            "((lambda () 42))", 42L,
            "((lambda () (+ 1 2 3 4 5)))", 15L,
            "((lambda (a b c) (+ a b c)) 1 2 3)", 6L,
            "((lambda (a &optional b c) (+ a (or b 0) (or c 0))) 1)", 1L,
            "((lambda (a &optional b c) (+ a (or b 0) (or c 0))) 1 2)", 3L,
            "((lambda (a &optional b c) (+ a (or b 0) (or c 0))) 1 2 3)", 6L,
            "((lambda (a &rest b) (+ a (car b))) 1 2 3 4 5)", 3L,
            "(or)", false,
            "(or 1)", 1L,
            "(or nil 1)", 1L,
            "(and)", true,
            "(and 1)", 1L,
            "(and 1 nil)", false,
            "(if nil 1 2)", 2L,
            "(if 1 2 3)", 2L,
            "(cond (nil 1) (t 2))", 2L,
            "(cond (1 1) (t 2))", 1L,
            "(cond (nil 1) ((= 1 0) 2))", false,
            "(progn 1 2 3)", 3L,
            "(setq)", false,
            "(setq aaa 1)", 1L,
    };

    @Test
    public void testBuiltInEval() {
        try (Context context = Context.newBuilder("elisp")
                .environment("ELISP_PATH", "")
                .build()
        ) {
            for (int i = 0; i < TESTS.length; i += 2) {
                String expr = (String) TESTS[i];
                Object expected = TESTS[i + 1];
                assertEquals(expected, context.eval("elisp", expr).as(expected.getClass()), expr);
            }
        }

    }
}
