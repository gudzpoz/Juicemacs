package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.Test;

public class BuiltInFnsTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "(length nil)", 0L,
            "(length '(1 2 3))", 3L,
            "(length \"abc\")", 3L,
            "(length \"\")", 0L,
            "(length \"文\")", 1L,
            "(length [1 2 3])", 3L,
            "(length [])", 0L,
            "(string-equal \"bcd\" (substring \"abcdef\" 1 4))", true,
            "(string-equal \"二\" (substring \"一二\" 1 2))", true,
            "(string-equal \"ab\" (substring \"abcde\" nil 2))", true,
            "(string-equal \"cde\" (substring \"abcde\" 2))", true,
            "(string-equal \"d\" (substring \"abcde\" -2 -1))", true,
            "(append)", false,
            "(equal (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))", true,
            "(equal (concat \"abc\" \"def\") \"abcdef\")", true,
            "(equal (concat \"文\" \"abc\" '(?d ?e ?f)) \"文abcdef\")", true,
            "(copy-sequence (= 1 0))", false,
            "(equal (copy-sequence '(1 2 3)) '(1 2 3))", true,
            "(equal (copy-sequence \"文\") \"文\")", true,
            "(equal (copy-sequence \"abc\") \"abc\")", true,
            "(equal (copy-sequence [1 2 3]) [1 2 3])", true,
            "(equal (copy-sequence #&8\"a\") #&8\"a\")", true,
            "(nth 0 '(1 2 3))", 1L,
            "(nth 1 '(1 2 3))", 2L,
            "(nth 2 nil)", false,
            "(nth 3 '(1 2 3))", false,
            "(elt '(1 2 3) 0)", 1L,
            "(elt '(1 2 3) 3)", false,
            "(elt nil 3)", false,
            "(elt [1 2 3] 1)", 2,
            "(elt (make-char-table nil 42) ?a)", 42L,
            "(null (member \"1\" '(\"1\" 2 3)))", false,
            "(member 4 '(1 2 3))", false,
            "(member 1 nil)", false,
            "(null (member t '(t)))", false,
            "(car (memq 1 '(1 2 3)))", 1L,
            "(null (memq \"1\" '(\"1\" 2 3)))", true,
            "(memq 1 nil)", false,
            "(assq 1 '(1 2 3))", false,
            "(assq 1 nil)", false,
            "(cdr (assq 1 '(2 (2 . 1) (1 . 42) 3)))", 42L,
            "(delq 1 nil)", false,
            "(delq 1 '(1))", false,
            "(apply #'+ (delq 1 '(1 2 1 3 1)))", 5L,
            "(nreverse nil)", false,
            "(equal (nreverse '(1)) '(1))", true,
            "(equal (nreverse '(1 2 3)) '(3 2 1))", true,
            "(equal (nreverse [1 2 3]) [3 2 1])", true,
            "(equal (nreverse \"abc\") \"cba\")", true,
            "(equal (nreverse #&2\"\\1\") #&2\"\\2\")", true,
            "(plist-get '(:a 1 :b 2) :a)", 1L,
            "(plist-get '(:a 1 :b 2) :c)", false,
            "(plist-get '(:a) :a)", false,
            "(plist-get '(\"a\" 1) \"a\" 'equal)", 1L,
            "(get 'a 'bb)", false,
            "(progn (put 'a 'b 1) (get 'a 'b))", 1L,
            "(eql nil t)", false,
            "(eql nil nil)", true,
            "(eql 0.0 0.0)", true,
            "(eql 0.0 -0.0)", false,
            "(eql 0 -0)", true,
            "(eql 0.0 0)", false,
            "(eql 0.0e+NaN 0.0e+NaN)", true,
            // TODO: Distinguishing 0.0e+NaN and 100.0e+NaN does not seem possible in Java.
            //   According to Double.longBitsToDouble, the VM might silently unify NaN bits.
            "(progn (eql 0.0e+NaN 100.0e+NaN) nil)", false,
            "(eql #xFFFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFFFF)", true,
            "(eql #xFFFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFFF0)", false,
            "(eql #xFFFFFFFFFFFFFFFFFFFFFF 0)", false,
            "(equal 1 1)", true,
            "(equal 1 1.0)", true,
            "(equal 1.0 1.0)", true,
            "(equal 0.0 -0.0)", false,
            "(equal 1.0 -1.0)", false,
            "(equal 1.0 #xFFFFFFFFFFFFFFFFFFFFFFFF)", false,
            "(equal 0.0e+NaN 0.0e+NaN)", true,
            "(equal 0.0e+NaN 100.0e+NaN)", true,
            "(nconc)", false,
            "(equal (nconc '(1 2 3) nil '(4 5 6)) '(1 2 3 4 5 6))", true,
            "(equal (mapcar #'1+ '(1 2 3)) '(2 3 4))", true,
            "(featurep 'aaaa)", false,
            "(featurep 'emacs)", true,
            "(featurep 'emacs 'aaaa)", false,
            "(progn (provide 'aaaaa '(1 2 3 4)) (featurep 'aaaaa 1))", true,
            "(require 'emacs)", true,
            "(let ((ht (make-hash-table))) (puthash 1 2 ht) (gethash 1 ht))", 2L,
            "(hash-table-p (make-hash-table))", true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
