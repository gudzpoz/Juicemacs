package party.iroiro.juicemacs.elisp.forms;

public class BuiltInEditFnsTest extends BaseFormTest {
    private final static Object[] TESTS = {
            "(equal (system-name) (progn (setq system-name \"blahblah\") (system-name)))", false,
            "(equal \"blahblah\" (progn (setq system-name \"blahblah\") (system-name)))", true,
            """
            (progn
             (set-buffer (get-buffer-create "test"))
             (insert "aabb")
             (goto-char 3) ; aa|bb
             (save-excursion
               (goto-char 1) ; |aa.bb
               (insert "cc") ; cc|aa.bb
               (insert "dd") ; ccdd|aa.bb
               ) ; ccddaa|bb
             (insert "ee") ; ccddaaee|bb
             (buffer-string))
            """, "ccddaaeebb",
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
