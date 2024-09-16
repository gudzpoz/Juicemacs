package party.iroiro.juicemacs.elisp.forms;

public class BuiltInEvalTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "((lambda () 42))", 42L,
            "((lambda () (+ 1 2 3 4 5)))", 15L,
            "((lambda (a b c) (+ a b c)) 1 2 3)", 6L,
            "((lambda (a &optional b c) (+ a (or b 0) (or c 0))) 1)", 1L,
            "((lambda (a &optional b c) (+ a (or b 0) (or c 0))) 1 2)", 3L,
            "((lambda (a &optional b c) (+ a (or b 0) (or c 0))) 1 2 3)", 6L,
            "((lambda (a &rest b) (+ a (car b))) 1 2 3 4 5)", 3L,
            "(progn (defalias 'a (cons 'macro #'(lambda (x) (list '1+ x)))) (a 1))", 2L,
            "(or)", false,
            "(or 1)", 1L,
            "(or nil 1)", 1L,
            "(and)", true,
            "(and 1)", 1L,
            "(and 1 nil)", false,
            "(if nil 1 2)", 2L,
            "(if 1 2 3)", 2L,
            "(cond (nil 1) (t 2))", 2L,
            "(cond (1 1) (t 2))", 1L,
            "(cond (nil 1) ((= 1 0) 2))", false,
            "(progn 1 2 3)", 3L,
            "(progn (setq a 1) (setq a 2))", 2L,
            "(prog1 (setq a 1) (setq a 2))", 1L,
            "(setq)", false,
            "(setq aaa 1)", 1L,
            "(eq 'aaa #'aaa)", true,
            "(progn (setq aaa 42) (defvaralias 'aaaa 'aaa) aaaa)", 42L,
            "(progn (defvar aaa (1+ 11)) aaa)", 12L,
            "(progn (defconst aaaconst (1+ 11)) aaaconst)", 12L,
            "(let ((a 1)) (let* ((a 2) (b (+ a 1))) b))", 3L,
            "(let ((a 1)) (let  ((a 2) (b (+ a 1))) b))", 2L,
            "(let (a) a)", false,
            "(let () 1)", 1L,
            "(let ((sum 0) (n 100)) (while (> n 0) (setq sum (+ sum n) n (- n 1))) sum)", 5050L,
            "(eval '(+ 1 2 3))", 6L,
            "(apply '+ '(1 2 3))", 6L,
            "(funcall '+ 1 2 3)", 6L,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
