package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;

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
            "(progn (defvar aaa-new-var (1+ 11)) aaa-new-var)", 12L,
            """
            (progn
              (setq new-var-before-defvar 1)
              (defvar new-var-before-defvar 0)
              new-var-before-defvar)
            """, 1L,
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
            (eval '(defvar top-level-test-v))
            (defalias 'top-level-test-func #'(lambda (x) (+ x top-level-test-v)))
            (let ((top-level-test-v 1)) (top-level-test-func 2))
            """, 3L,
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
            """
            (let ((x 0))
              (unwind-protect nil (setq x 1))
              x)
            """, 1L,
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
                    "(wrong-number-of-arguments #[nil (nil) nil] 1)");
            assertErrorMessage(context, "((lambda (x)))",
                    "(wrong-number-of-arguments #[(x) (nil) nil] 0)");
            assertDoesNotThrow(() -> context.eval("elisp", "((lambda (&rest x)))"));
            assertErrorMessage(context, "(1+)", "(wrong-number-of-arguments 1+ 0)");
            assertErrorMessage(context, "(1+ 1 1 1)", "(wrong-number-of-arguments 1+ 3)");
            assertErrorMessage(context, "(1- 1 1 1)", "(wrong-number-of-arguments 1- 3)");
            assertErrorMessage(context, "(garbage-collect 1)", "(wrong-number-of-arguments garbage-collect 1)");
            assertErrorMessage(context, "(lognot nil)", "(wrong-type-argument integerp nil)");
        }
    }

    @Test
    public void testSoftExitExceptions() {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try (Context context = getTestingContextBuilder().out(out).build()) {
            context.eval("elisp", "(setq result 0)");
            Value bindings = context.getBindings("elisp");
            assertEquals(0L, bindings.getMember("result").asLong());
            assertErrorMessage(
                    context,
                    """
                            (unwind-protect
                                (signal 'error '(1))
                              (setq result 1)
                              (message "normal error"))
                            """,
                    "(error 1)"
            );
            assertEquals(1L, bindings.getMember("result").asLong());
            assertErrorMessage(
                    context,
                    """
                            (unwind-protect
                                (kill-emacs -1)
                              (setq result 2)
                              (message "soft exit"))
                            """,
                    "(fatal kill-emacs -1)"
            );
            assertEquals(1L, bindings.getMember("result").asLong());
        }
        assertEquals("normal error\n", out.toString());
        out.reset();
        try (Context context = getTestingContextBuilder().out(out)
                .option("elisp.hardExit", "true")
                .build()) {
            context.eval("elisp", "(setq result 0)");
            Value bindings = context.getBindings("elisp");
            assertEquals(0L, bindings.getMember("result").asLong());
            assertErrorMessage(
                    context,
                    """
                            (unwind-protect
                                (kill-emacs -1)
                              (setq result 2)
                              (message "hard exit"))
                            """,
                    "Exit was called with exit code -1."
            );
            assertThrows(PolyglotException.class, () -> bindings.getMember("result"));
        } catch (PolyglotException e) {
            assertEquals("Exit was called with exit code -1.", e.getMessage());
        }
        assertEquals("", out.toString());
    }

    @Test
    public void testPerIterationScope() {
        String eval = """
                ;;; -*- lexical-binding: t -*-
                (let ((i 0)
                      readers writers)
                  (while (< i 5)
                    (let ((j 0))
                      (while (<= j i)
                        (let ((k (+ (* 10 i) j)))
                          (setq readers (cons #'(lambda () (list i j k)) readers)
                                writers (cons #'(lambda () (setq i (1+ i)
                                                                 j (1+ j)
                                                                 k (1+ k)))
                                              writers)))
                        (setq j (1+ j))))
                    (setq i (1+ i)))
                  (vector
                   (mapcar #'funcall readers)
                   (mapcar #'funcall writers)
                   (mapcar #'funcall readers)))
                """;
        try (Context context = getTestingContext()) {
            Value value = context.eval("elisp", eval);
            assertEquals(3, value.getArraySize());
            Value read1 = value.getArrayElement(0);
            assertEquals(
                    // output: (i j k)
                    // i: shared by all functions
                    // j: shared by some functions (with same j)
                    // k: shared by two (reader and writer)
                    """
                            ((5 5 44) (5 5 43) (5 5 42) (5 5 41) (5 5 40) \
                            (5 4 33) (5 4 32) (5 4 31) (5 4 30) \
                            (5 3 22) (5 3 21) (5 3 20) \
                            (5 2 11) (5 2 10) \
                            (5 1 0))""",
                    read1.toString()
            );
            Value write = value.getArrayElement(1);
            assertEquals(
                    "(45 44 43 42 41 34 33 32 31 23 22 21 12 11 1)",
                    write.toString()
            );
            Value read2 = value.getArrayElement(2);
            assertEquals(
                    // output: (i j k)
                    // i: increment by 15 (shared writer count)
                    // j: increment by j (shared writer count)
                    // k: increment by 1 (shared writer count)
                    """
                            ((20 10 45) (20 10 44) (20 10 43) (20 10 42) (20 10 41) \
                            (20 8 34) (20 8 33) (20 8 32) (20 8 31) \
                            (20 6 23) (20 6 22) (20 6 21) \
                            (20 4 12) (20 4 11) \
                            (20 2 1))""",
                    read2.toString()
            );
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
