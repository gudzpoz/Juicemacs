package party.iroiro.juicemacs.elisp.forms;

public class BuiltInAllocTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "(car (cons 1 2))", 1L,
            "(cdr (cons 1 2))", 2L,
            "(listp (cons 1 2))", true,
            "(list)", false,
            "(listp (list))", true,
            "(null (list))", true,
            "(nth 0 (list 1 2 3))", 1L,
            "(nth 1 (list 1 2 3))", 2L,
            "(nth 2 (list 1 2 3))", 3L,
            "(eq (purecopy 'symbol) 'symbol)", true,
            "(let* ((a \"str\") (b (purecopy a))) (and (equal a b) (null (eq a b))))", true,
            "(let* ((a [1 2 3]) (b (purecopy a))) (and (equal a b) (null (eq a b))))", true,
            "(let* ((a '(12 3)) (b (purecopy a))) (and (equal a b) (null (eq a b))))", true,
            "(garbage-collect)", false, // TODO: implement
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }

}
