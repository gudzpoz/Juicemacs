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
            """
            (let ((tb (make-char-table nil 0)))
              (set-char-table-range tb t 42)
              (set-char-table-range tb '(?b . ?z) 1)
              (set-char-table-range tb '(9999 . 100000) 100)
              (set-char-table-range tb 1234 1234)
              (set-char-table-range tb nil 0)
              (set-char-table-parent tb nil)
              (and
                (null (char-table-subtype tb))
                (null (char-table-parent tb))
                (eq (elt tb ?a) 42)
                (eq (elt tb ?b) 1)
                (eq (elt tb ?z) 1)
                (eq (elt tb ?A) 42)
                (eq (elt tb 1234) 1234)
                (eq (elt tb 9998) 42)
                (eq (elt tb 9999) 100)
                (eq (elt tb 100000) 100)
                (eq (elt tb 100001) 42)
                ))""",
            true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
