package party.iroiro.juicemacs.elisp.forms;

public class BuiltInEditFnsTest extends BaseFormTest {
    private final static Object[] TESTS = {
            "(equal (system-name) (progn (setq system-name \"blahblah\") (system-name)))", false,
            "(equal \"blahblah\" (progn (setq system-name \"blahblah\") (system-name)))", true,
            """
            (progn
             (set-buffer (get-buffer-create "test-excursion"))
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
            """
            (progn
             (set-buffer (get-buffer-create "test-line-beginning-end"))
             (insert "cc")
             (goto-char 2)
             (+
               (* (line-beginning-position 0) 1)
               (* (line-beginning-position 1) 10)
               (* (line-beginning-position 2) 100)
               (* (line-end-position 0) 1000)
               (* (line-end-position 1) 10000)
               (* (line-end-position 2) 100000))
             )
            """, 331311L,
            """
            (progn
             (set-buffer (get-buffer-create "test-forward-line"))
             (insert "cc")
             (goto-char 2)
             (forward-line)
             (point))
            """, 3L,
            """
            (defalias 'test-bol-eol #'(lambda (f i expected-point)
              (goto-char 2)
              (funcall f i)
              (if (null (= (point) expected-point))
                  (signal 'error (list expected-point (point))))))
            
            (set-buffer (get-buffer-create "test-bol-eol"))
            (insert "aa")
            (test-bol-eol 'beginning-of-line 0 1)
            (test-bol-eol 'beginning-of-line 1 1)
            (test-bol-eol 'beginning-of-line 2 3)
            (test-bol-eol 'end-of-line 0 1)
            (test-bol-eol 'end-of-line 1 3)
            (test-bol-eol 'end-of-line 2 3)
            """, false,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
