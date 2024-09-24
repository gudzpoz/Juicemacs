package party.iroiro.juicemacs.elisp.forms;

class BuiltInSearchTest extends BaseFormTest {
    private static final Object[] TESTS = {
            "(string-match \"abc\" \"abc\")", 0L,
            "(string-match \"abc\" \"---abc\")", 3L,
            "(string-match \"abc\" \"abc---\")", 0L,
            "(string-match \"abc\" \"---cba---\")", false,
            """
            (let ((start (string-match "-\\\\(abc\\\\)d" "--abcd--")))
              (and (eq start 1)
                   (eq (match-beginning 0) 1) (eq (match-end 0) 6)
                   (eq (match-beginning 1) 2) (eq (match-end 1) 5)))""", true,
            "(progn (string-match \"abc\" \"abc\") (equal (match-data) '(0 3)))", true,
            "(progn (set-match-data '(1 2 3)) (equal (match-data) '(1 2)))", true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
