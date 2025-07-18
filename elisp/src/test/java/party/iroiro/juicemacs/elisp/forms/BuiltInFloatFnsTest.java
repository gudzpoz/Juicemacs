package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BuiltInFloatFnsTest extends BaseFormTest {
    public static Object[] TESTS = {
            "(logb 1024)", 10L,
            "(logb 10000)", 13L,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }

    @Test
    public void truncateTest() {
        try (Context context = getTestingContext()) {
            Value value = context.eval("elisp", """
                    (setq elb-num 130767436800000000
                          elb-acc 1269589784327156250
                          elb-den 191898783962510625
                          nth 3)
                    (truncate (+ (* elb-num nth) elb-acc) elb-den)
                    """);
            assertEquals(8, value.asLong());
        }
    }
}
