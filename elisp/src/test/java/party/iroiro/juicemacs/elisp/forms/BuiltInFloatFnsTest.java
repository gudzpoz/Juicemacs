package party.iroiro.juicemacs.elisp.forms;

class BuiltInFloatFnsTest extends BaseFormTest {
    public static Object[] TESTS = {
            "(logb 1024)", 10L,
            "(logb 10000)", 13L,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
