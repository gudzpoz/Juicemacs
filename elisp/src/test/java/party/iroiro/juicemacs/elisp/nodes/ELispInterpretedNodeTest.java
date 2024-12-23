package party.iroiro.juicemacs.elisp.nodes;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;

public class ELispInterpretedNodeTest {
    @Test
    public void testSimple() {
        try (Context context = getTestingContext()) {
            Value v = context.eval("elisp", """
                    ((lambda (x) (* 10 x)) 1)
                    """);
            assertEquals(10, v.asInt());
        }
    }

    @Test
    public void testFormChange() {
        try (Context context = getTestingContext()) {
            Value v = context.eval("elisp", """
                    (let ((form-change-var 1)
                          (result 0))
                     ;; function
                     (defalias 'test-changing-func #'(lambda () (x (* 10 form-change-var))))
                     (defalias 'x #'(lambda (v) (* 10 v)))
                     (setq result (+ result (test-changing-func))) ; 0 + 10 * 10 * 1 -> 100

                     ;; macro #1 (quoted with #')
                     (defalias 'x (cons 'macro #'(lambda (v) (list 'cdr (list 'quote v)))))
                     (setq the-cons (test-changing-func)) ; macro -> '(10 form-change-var)
                     (setq result (+ result (car the-cons))) ; 100 + 10 -> 110

                     ;; macro #2 (quoted with #')
                     (defalias 'x (cons 'macro #'(lambda (v) (list 'and v 1))))
                     (setq the-value (test-changing-func)) ; macro -> (and (* 10 form-change-var) 1) -> 1
                     (setq result (+ result the-value)) ; 110 + 1 -> 111

                     ;; macro #1 (quoted with ')
                     (defalias 'x (cons 'macro '(lambda (v) (list 'cdr (list 'quote v)))))
                     (setq the-cons (test-changing-func)) ; macro -> '(10 form-change-var)
                     (setq result (+ result (car the-cons))) ; 111 + 10 -> 121

                     ;; macro #2 (quoted with ')
                     (defalias 'x (cons 'macro '(lambda (v) (list 'and v 1))))
                     (setq the-value (test-changing-func)) ; macro -> (and (* 10 form-change-var) 1) -> 1
                     (setq result (+ result the-value)) ; 121 + 1 -> 122

                     (defalias 'x (symbol-function #'quote))
                     (setq the-cons (test-changing-func)) ; quote -> '(* 10 form-change-var)
                     (setq result (+ result (* 100 (elt the-cons 1)))) ; 122 + 10 * 100 -> 1122
                     result)
                    """);
            assertEquals(1122L, v.asInt());
        }
    }
}
