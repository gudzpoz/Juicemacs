package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispBindingScopeTest.LEXICAL_BINDING_TEST;
import static party.iroiro.juicemacs.elisp.runtime.ELispBindingScopeTest.SPECIAL_IN_LEXICAL_BINDING_TEST;

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
            "(progn (defconst aaaconst (1+ 11)) aaaconst) ;; no-warm-up-test", 12L,
            "(let ((a 1)) (let* ((a 2) (b (+ a 1))) b))", 3L,
            "(let ((a 1)) (let  ((a 2) (b (+ a 1))) b))", 2L,
            "(let (a) a)", false,
            "(let () 1)", 1L,
            "(let ((sum 0) (n 100)) (while (> n 0) (setq sum (+ sum n) n (- n 1))) sum)", 5050L,
            LEXICAL_BINDING_TEST, 1L,
            SPECIAL_IN_LEXICAL_BINDING_TEST, 0L,
            """
            ;;; -*- lexical-binding: t -*-
            (progn
              (setq case-fold-search nil)
              (defalias 'search #'(lambda () (string-match "A" "a")))
              (let* ((case-fold-search t)
                     (start (search)))
                start))""", 0L,
            """
            ;;; -*- lexical-binding: t -*-
            (+
             (let ((_ 1))
               (defvar y) ;; dynamic only in this let block
               (defalias 'gety #'(lambda () y))
               (setq y 1)
               (let ((y 2))
                 (gety)))
             (let ((y 10))
               (defalias 'gety #'(lambda () y))
               (let ((y 20))
                 (gety))))
            """, 12L,
            """
            ;;; -*- lexical-binding: t -*-
            (let ((y 1))
              (let* ((x #'(lambda () y))
                     (y 2))
                (funcall x)))
            """, 1L,
            "(eval '(+ 1 2 3))", 6L,
            "(apply '+ '(1 2 3))", 6L,
            "(functionp '+)", true,
            "(functionp 'quote)", false,
            "(functionp (function (lambda (x) x)))", true,
            "(funcall '+ 1 2 3)", 6L,

            "(catch 1 (throw 1 2))", 2L,
            """
            (catch 'a
              (+ 100 (catch 'b
                       (+ 1000 (catch 'c
                                 (throw 'b 10))))))
            """, 110L,
            """
            (progn
              (put 'e 'error-conditions '(a b c))
              (condition-case s (signal 'e 1)
                (a (= (cdr s) 1))))
            """, true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }

    private void assertErrorMessage(Context context, String expr, String message) {
        String actual = assertThrows(PolyglotException.class, () -> context.eval("elisp", expr)).getMessage();
        assertEquals(message, actual);
    }

    @Test
    public void testSignals() {
        try (Context context = getTestingContext()) {
            assertErrorMessage(context, "((lambda ()) 1)",
                    "(wrong-number-of-arguments #[nil (nil) nil nil nil nil] 1)");
            assertErrorMessage(context, "((lambda (x)))",
                    "(wrong-number-of-arguments #[(x) (nil) nil nil nil nil] 0)");
            assertDoesNotThrow(() -> context.eval("elisp", "((lambda (&rest x)))"));
            assertErrorMessage(context, "(1+)", "(wrong-number-of-arguments 1+ 0)");
            assertErrorMessage(context, "(1+ 1 1 1)", "(wrong-number-of-arguments 1+ 3)");
            assertErrorMessage(context, "(1- 1 1 1)", "(wrong-number-of-arguments 1- 3)");
            assertErrorMessage(context, "(garbage-collect 1)", "(wrong-number-of-arguments garbage-collect 1)");
            assertErrorMessage(context, "(lognot nil)", "(wrong-type-argument integerp nil)");
        }
    }

    @Test
    public void testEvalBufferStackTrace() {
        try (Context context = getTestingContext()) {
            PolyglotException e = assertThrows(PolyglotException.class, () -> context.eval("elisp", """
                    (set-buffer (get-buffer-create " *temp-test-eval-buffer*"))
                    (insert "(defalias 'fun #'(lambda () (signal 'error nil)))
                    (fun)")
                    (eval-buffer (current-buffer) nil "pseudo-file-name")
                    """));
            String[] expected = {
                    "signal", "BuiltInEval.java:1",
                    "fun", "pseudo-file-name:1",
                    "pseudo-file-name", "pseudo-file-name:2",
                    "eval-buffer", "BuiltInLRead.java:1",
                    "Unnamed", "Unnamed:4",
            };
            StackTraceElement[] stackTrace = e.getStackTrace();
            for (int i = 0; i < expected.length; i += 2) {
                String f = expected[i];
                String place = expected[i + 1];
                StackTraceElement element = stackTrace[i / 2];
                assertEquals("<elisp>", element.getClassName());
                assertEquals(f, element.getMethodName());
                assertEquals(place.split(":")[0], element.getFileName());
                assertEquals(Integer.parseInt(place.split(":")[1]), element.getLineNumber());
            }
        }
    }
}
