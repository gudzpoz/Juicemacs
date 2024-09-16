package party.iroiro.juicemacs.elisp.forms;

public class BuiltInCharTabTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "(eq (type-of (make-char-table nil)) 'char-table)", true,
            "(progn (put 'ct 'char-table-extra-slots 1) (char-table-extra-slot (make-char-table 'ct 42) 0))", 42L,
            """
            (progn (put 'ct 'char-table-extra-slots 1)
                   (let ((tb (make-char-table 'ct 42)))
                     (set-char-table-extra-slot tb 0 24)
                     (char-table-extra-slot tb 0)
                     ))
            """, 24L,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
