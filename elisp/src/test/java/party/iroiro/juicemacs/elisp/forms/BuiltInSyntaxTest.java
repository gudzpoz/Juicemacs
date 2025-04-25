package party.iroiro.juicemacs.elisp.forms;

public class BuiltInSyntaxTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            """
            (progn
              (set-buffer (get-buffer-create "test-skip-chars-backward"))
              (insert "abc")
              (skip-chars-backward "bc")
              (point))
            """, 2L,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
