package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;

public class ELispLanguageTest {

    private static final Object[] NUMERIC_VALUE_TESTS = new Object[] {
            "100", 100,
            "255.0", 255.0,
            "#xFFFFFFFFFFFFFFFFFFFF", BigInteger.ONE.shiftLeft(80).subtract(BigInteger.ONE),
    };

    @Test
    public void testNumberValue() {
        try (Context context = Context.create()) {
            for (int i = 0; i < NUMERIC_VALUE_TESTS.length; i += 2) {
                String expr = (String) NUMERIC_VALUE_TESTS[i];
                Object expected = NUMERIC_VALUE_TESTS[i + 1];
                Value result = context.eval("elisp", expr);
                assertEquals(expected, result.as(expected.getClass()));
            }
        }
    }

    private static final Object[] ARITHMETIC_TESTS = new Object[] {
            "(+ 1 1)", 2,
            "(+ 1 1 1)", 3,
            "(+ #xFFFFFFFFFFFFFFFFFFFF 1)", BigInteger.ONE.shiftLeft(80),
            "(+ #xFFFFFFFFFFFFFFFFFFFF 1 (1+ 0) (1- 0))", BigInteger.ONE.shiftLeft(80),
            "(+ 1 1 1.5)", 3.5,
    };

    @Test
    public void test() {
        try (Context context = Context.create()) {
            for (int i = 0; i < ARITHMETIC_TESTS.length; i += 2) {
                String expr = (String) ARITHMETIC_TESTS[i];
                Object expected = ARITHMETIC_TESTS[i + 1];
                Value result = context.eval("elisp", expr);
                assertEquals(expected, result.as(expected.getClass()));
            }
        }
    }

}
