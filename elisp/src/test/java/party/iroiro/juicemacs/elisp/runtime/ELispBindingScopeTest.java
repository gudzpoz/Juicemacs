package party.iroiro.juicemacs.elisp.runtime;

import org.eclipse.jdt.annotation.NonNull;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.forms.BaseFormTest;

import static org.junit.jupiter.api.Assertions.*;

public class ELispBindingScopeTest extends BaseFormTest {

    public static final String LEXICAL_BINDING_TEST = """
            ;;; -*- lexical-binding: t -*-
            (progn
              (setq x -1)
              (defalias 'getx #'(lambda () x))
              (setq x 1)
              (let ((x 2))
                (getx)))
            """;
    public static final String DYNAMIC_BINDING_TEST = """
            ;;; -*- lexical-binding: nil -*-
            (progn
              (setq x -1)
              (defalias 'getx #'(lambda () x))
              (setq x 1)
              (let ((x 2))
                (getx)))
            """;
    public static final String SPECIAL_IN_LEXICAL_BINDING_TEST = """
            ;;; -*- lexical-binding: t -*-
            (progn
              (setq case-fold-search nil)
              (defalias 'search #'(lambda () (string-match "A" "a")))
              (let ((case-fold-search t))
                (search)))
    """;

    public static final Object[] TESTS = {
            """
            ;;; -*- lexical-binding: t -*-
            (apply
             #'+
             (mapcar
              #'(lambda (f) (funcall f))
              (let ((i 3) (v (make-vector 3 nil)))
                (while (> i 0)
                  (aset v (1- i) #'(lambda () i))
                  (setq i (1- i)))
                v)))
            """, 0L,
            """
            ;;; -*- lexical-binding: t -*-
            (let* ((a 10)
                   (b #'(lambda () a))
                   (a 2))
              (+ (funcall b) a))
            """, 12L,
    };

    @Test
    public void testDynamicBinding() {
        try (Context context = Context.create()) {
            Value result = context.eval("elisp", DYNAMIC_BINDING_TEST
            );
            assertEquals(2L, result.asLong());
        }
    }

    @Test
    public void testLexicalBinding() {
        try (Context context = Context.create()) {
            Value result = context.eval("elisp", LEXICAL_BINDING_TEST);
            assertEquals(1L, result.asLong());

            Value special = context.eval("elisp", SPECIAL_IN_LEXICAL_BINDING_TEST);
            assertEquals(0L, special.asLong());
        }
    }

    @Override
    protected Object @NonNull[] entries() {
        return TESTS;
    }
}
