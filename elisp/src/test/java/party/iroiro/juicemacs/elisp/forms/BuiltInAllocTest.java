package party.iroiro.juicemacs.elisp.forms;

public class BuiltInAllocTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "(car (cons 1 2))", 1L,
            "(cdr (cons 1 2))", 2L,
            "(listp (cons 1 2))", true,
            "(list)", false,
            "(listp (list))", true,
            "(length (make-vector 3 nil))", 3L,
            "(length (vector 1 2 3))", 3L,
            "(null (list))", true,
            "(nth 0 (list 1 2 3))", 1L,
            "(nth 1 (list 1 2 3))", 2L,
            "(nth 2 (list 1 2 3))", 3L,
            "(eq 'test (make-symbol \"test\"))", false,
            "(eq (purecopy 'symbol) 'symbol)", true,
            "(null (garbage-collect)) ;; no-warm-up-test", false, // TODO: implement
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }

}
