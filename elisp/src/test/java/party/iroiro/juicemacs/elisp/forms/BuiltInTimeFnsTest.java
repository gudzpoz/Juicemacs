package party.iroiro.juicemacs.elisp.forms;

public class BuiltInTimeFnsTest extends BaseFormTest {
    private static final Object[] TESTS = {
            "(length (current-time))", 4L,
            "(let* ((current-time-list nil) (time (current-time))) (and (natnump (car time)) (natnump (cdr time))))",
            true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
