package party.iroiro.juicemacs.elisp.forms;

import java.math.BigInteger;

public class BuiltInDataTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "(eq (and) (intern \"t\"))", true,
            "(eq (or) (intern \"nil\"))", true,
            "(eq 1 1)", true,
            "(eq 1 0)", false,
            "(eq 'a 'a)", true,
            "(eq 'a 'b)", false,
            "(eq 'symbol (type-of t))", true,
            "(eq 'symbol (type-of nil))", true,
            "(eq 'symbol (type-of (intern \"t\")))", true,
            "(eq 'symbol (type-of (intern \"nil\")))", true,
            "(eq 'symbol (type-of (eq 1 1)))", true,
            "(eq 'symbol (type-of (eq 1 0)))", true,
            "(eq 'symbol (type-of 'symbol))", true,
            "(eq 'integer (type-of 1))", true,
            "(eq 'integer (type-of #xFFFFFFFFFFFFFFFFFFFF))", true,
            "(eq 'float (type-of 1.0))", true,
            "(eq 'subr (type-of (symbol-function 'eq)))", true,
            "(equal \"t\" (symbol-name t))", true,
            "(and (fset 'test-sym 1) (symbol-function 'test-sym))", 1L,
            "(+ (set 'a 1) a)", 2L,
            """
            (let ((a 1))
              (+ a
               (progn (set 'a 10) a)
               (progn (setq a 100) a)))""", 102L,
            "(eq 'string (type-of \"s\"))", true,
            "(eq 'vector (type-of [1 2 3]))", true,
            "(eq 'cons (type-of '(1)))", true,
            "(eq 'bool-vector (type-of #&1\"a\"))", true,
            "(null nil)", true,
            "(null 1)", false,
            "(consp nil)", false,
            "(consp '(a))", true,
            "(atom 1)", true,
            "(atom 'a)", true,
            "(atom '(a))", false,
            "(listp 1)", false,
            "(listp nil)", true,
            "(listp '(a))", true,
            "(nlistp nil)", false,
            "(nlistp '(a))", false,
            "(nlistp 1)", true,
            "(symbolp 1)", false,
            "(symbolp nil)", true,
            "(symbolp 'a)", true,
            "(keywordp 1)", false,
            "(keywordp nil)", false,
            "(keywordp 'a)", false,
            "(keywordp ':a)", true, // TODO: Keywords should not need quoting
            "(vectorp '(a))", false,
            "(vectorp [])", true,
            "(recordp [])", false,
            "(recordp #s(hash-table))", false,
            "(recordp #s(not-hash-table))", true,
            "(stringp 1)", false,
            "(stringp \"\")", true,
            "(char-table-p 1)", false,
            "(char-table-p (make-char-table nil))", true,
            "(vector-or-char-table-p 1)", false,
            "(vector-or-char-table-p (make-char-table nil))", true,
            "(vector-or-char-table-p [])", true,
            "(bool-vector-p 1)", false,
            "(bool-vector-p #&1\"a\")", true,
            "(arrayp 1)", false,
            "(arrayp [])", true,
            "(arrayp #&1\"a\")", true,
            "(arrayp (make-char-table nil))", true,
            "(arrayp \"string\")", true,
            "(sequencep 1)", false,
            "(sequencep \"\")", true,
            "(sequencep [])", true,
            "(sequencep '(a b c))", true,
            "(char-or-string-p nil)", false,
            "(char-or-string-p #xFFFFFFFFFFFFFFFFFFFFFFF)", false,
            "(char-or-string-p \"\")", true,
            "(char-or-string-p 1)", true,
            "(natnump nil)", false,
            "(natnump -1)", false,
            "(natnump #x-FFFFFFFFFFFFFFFFFFFFF)", false,
            "(natnump 1)", true,
            "(natnump #xFFFFFFFFFFFFFFFFFFFFF)", true,
            "(numberp nil)", false,
            "(numberp 1)", true,
            "(numberp 1.0)", true,
            "(car nil)", false,
            "(car (intern \"nil\"))", false,
            "(car '(1 2 3))", 1L,
            "(car-safe 1)", false,
            "(car-safe '(1 2 3))", 1L,
            "(cdr nil)", false,
            "(cdr (intern \"nil\"))", false,
            "(cdr '(1 . 2))", 2L,
            "(cdr-safe 1)", false,
            "(cdr-safe '(1 . 2))", 2L,
            "(let ((a '(1 . 2))) (setcar a 3) (car a))", 3L,
            "(let ((a '(1 . 2))) (setcdr a 3) (cdr a))", 3L,
            "(let ((not-globally-bound 1)) (boundp 'not-globally-bound))", false,
            """
            (and (setq not-bound-yet 1) (boundp 'not-bound-yet)
                 (makunbound 'not-bound-yet) (null (boundp 'not-bound-yet)))
            """, true,
            """
            (and (null (fboundp 'unbound-yet-f)) (defalias 'unbound-yet-f 'identity)
                 (fboundp 'unbound-yet-f)
                 (fmakunbound 'unbound-yet-f) (null (fboundp 'unbound-yet-f)))
            """, true,
            "(progn (put 'a 'b 1) (get 'a 'b))", 1L,
            "(progn (put 'aaaa 'b 1) (equal (symbol-plist 'aaaa) '(b 1)))", true,
            "(progn (setq add 100) (defalias 'add '+) (+ (add 1 10) add))", 111L,
            """
            (progn (setq built-in-data-test-mvbl-in-let 1)
              (let ((built-in-data-test-mvbl-in-let 1))
               (make-variable-buffer-local 'built-in-data-test-mvbl-in-let)
               (local-variable-p 'built-in-data-test-mvbl-in-let))
            )
            """, false,
            """
            (let ((built-in-data-test-mvbl-in-let-1 1))
              (make-variable-buffer-local 'built-in-data-test-mvbl-in-let-1)
              (local-variable-if-set-p 'built-in-data-test-mvbl-in-let-1))
            """, true,
            """
            (progn (set-default 'built-in-data-test-default t)
                   (and (null (local-variable-p 'built-in-data-test-default))
                        (default-boundp 'built-in-data-test-default)
                        built-in-data-test-default))""", true,
            "(progn (setq built-in-data-test-default-1 1) (default-value 'built-in-data-test-default-1))", 1L,
            """
            (progn (make-variable-buffer-local 'built-in-data-test-boundp)
                   (boundp 'built-in-data-test-boundp))""", true,
            """
            (progn (make-local-variable 'built-in-data-test-boundp-1)
                   (boundp 'built-in-data-test-boundp-1))""", false,
            // TODO: Add more buffer-local tests after we have multiple buffers.
            "(progn (defalias 'funca 1) (defalias 'funcb 'funca) (indirect-function 'funcb))", 1L,
            "(indirect-function 1)", 1L,
            "(let ((vec [1 2 3])) (aset vec 0 4) (equal vec [4 2 3]))", true,
            "(= 1 1 1 1 1)", true,
            "(= 1 1 1 1 0)", false,
            "(= 1.0 1.0 1 1)", true,
            "(= 1.0 2.0 1 1)", false,
            "(= 1.0 1.0 1 2)", false,
            "(= #xFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFF)", true,
            "(= #xFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFF 0)", false,
            "(< 1 2 3.0 #xFFFFFFFFFFFFFFFFFFFF)", true,
            "(< 1.0 2.0 #xFFFFFFFFFFFFFFFFFFFF 3.0)", false,
            "(> 2 1)", true,
            "(> 1 2)", false,
            "(>= 2 1)", true,
            "(>= 1 2)", false,
            "(<= 1 2)", true,
            "(<= 2 1)", false,
            "(/= 1 2)", true,
            "(/= 1 1)", false,
            "(string-to-number \"\")", 0L,
            "(string-to-number \"1\")", 1L,
            "(string-to-number \"qwerty\")", 0L,
            "(string-to-number \"011..qwerty\" 8)", 9L,
            "(string-to-number \"ff\" 16)", 255L,
            "(+ 1 1)", 2L,
            "(+ 1 1.0 2.5)", 4.5,
            "(+ " + Long.MAX_VALUE + " 1)", BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE),
            "(+ 1 1)", 2L, // rewritten
            "(+ #xFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFF)",
            new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16).multiply(BigInteger.TWO),
            "(= (+ #xFFFFFFFFFFFFFFFFFFFF 1.0) (+ 1.0 #xFFFFFFFFFFFFFFFFFFFF))", true,
            "(-)", 0L,
            "(- 1)", -1L,
            "(- 1 2)", -1L,
            "(- 1.0)", -1.0,
            "(-)", 0L, // rewritten
            "(- 1)", -1L,
            "(- 1.0)", -1.0,
            "(- " + Long.MIN_VALUE + ")", BigInteger.valueOf(Long.MIN_VALUE).negate(),
            "(- #xFFFFFFFFFFFFFFFFFFFF)", new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16).negate(),
            "(- 1 2)", -1L,
            "(- 0 " + Long.MIN_VALUE + ")", BigInteger.valueOf(Long.MIN_VALUE).negate(),
            "(- 1 2.0)", -1.0,
            "(- 0 #xFFFFFFFFFFFFFFFFFFFF #x-FFFFFFFFFFFFFFFFFFFF)", 0L,
            "(- #xFFFFFFFFFFFFFFFFFFFF)", new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16).negate(),
            "(- 1.0 2.0)", -1.0,
            "(- #xFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFF)", 0L,
            "(- (- #xFFFFFFFFFFFFFFFFFFFF 1.0) (+ #xFFFFFFFFFFFFFFFFFFFF -1.0))", 0.0,
            "(*)", 1L,
            "(* 1)", 1L,
            "(* " + Long.MAX_VALUE + " 2)", BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.TWO),
            "(* 1)", 1L, // rewritten
            "(* 1.0)", 1.0,
            "(* #xFFFFFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFFFFFF)",
            new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16).pow(2),
            "(- (* #xFFFFFFFFFFFFFFFFFFFF 2.0) (* 2.0 #xFFFFFFFFFFFFFFFFFFFF))", 0.0,
            "(/ 1 2)", 0L,
            "(/ 1 1 2 2.0)", 0.25,
            "(/ 1 2)", 0L, // rewritten
            "(/ 1.0 2)", 0.5,
            "(/ " + Long.MIN_VALUE + " -1)", BigInteger.valueOf(Long.MIN_VALUE).negate(),
            "(/ 1 (/ " + Long.MIN_VALUE + " -1))", 0L,
            "(/ (/ " + Long.MIN_VALUE + " -1) -1)", Long.MIN_VALUE,
            "(% 1 2)", 1L,
            "(% (* -1 " + Long.MIN_VALUE + ") 1)", 0L,
            "(mod 1 2)", 1L,
            "(mod (* -1 " + Long.MIN_VALUE + ") 1)", 0L,
            "(max 1 2 1)", 2L,
            "(min 2 1 2)", 1L,
            "(logand 1 3 7)", 1L,
            "(logand #xFFFFFFFFFFFFFFFFFFFF 1 3 7)", 1L,
            "(logior 1 2 4)", 7L,
            "(logior #xFFFFFFFFFFFFFFFFFFF0 1 2 4 8)", new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16),
            "(logxor 1 2 4)", 7L,
            "(logxor #xFFFFFFFFFFFFFFFFFFF0 1 2 4 8)", new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16),
            "(logcount 1)", 1L,
            "(logcount -1)", 0L,
            "(logcount #xFFFFFFFFFFFFFFFFFFFF)", 80L,
            "(ash 1 0)", 1L,
            "(ash 1 1)", 2L,
            "(ash 1 -1)", 0L,
            "(ash 1 63)", BigInteger.ONE.shiftLeft(63),
            "(ash 1 -64)", 0L,
            "(ash -1 -64)", -1L,
            "(ash #xFFFFFFFFFFFFFFFFFFFF -60)", new BigInteger("FFFFF", 16),
            "(1+ 1)", 2L,
            "(1- 1)", 0L,
            "(1+ " + Long.MAX_VALUE + ")", BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE),
            "(1- " + Long.MIN_VALUE + ")", BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.ONE),
            "(1+ 1.0)", 2.0,
            "(1- 1.0)", 0.0,
            "(1+ #xFFFFFFFFFFFFFFFFFFFF)", new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16).add(BigInteger.ONE),
            "(1- #xFFFFFFFFFFFFFFFFFFFF)", new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16).subtract(BigInteger.ONE),
            "(lognot 1)", -2L,
            "(lognot -1)", 0L,
            "(lognot #xFFFFFFFFFFFFFFFFFFFF)", new BigInteger("100000000000000000000", 16).negate(),
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
