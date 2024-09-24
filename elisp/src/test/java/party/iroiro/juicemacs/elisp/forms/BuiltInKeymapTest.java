package party.iroiro.juicemacs.elisp.forms;

public class BuiltInKeymapTest extends BaseFormTest {
    private static final Object[] TESTS = {
            "(keymapp (make-keymap))", true,
            "(keymapp (make-keymap \"s\"))", true,
            "(keymapp (make-sparse-keymap))", true,
            "(keymapp (make-sparse-keymap \"s\"))", true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
