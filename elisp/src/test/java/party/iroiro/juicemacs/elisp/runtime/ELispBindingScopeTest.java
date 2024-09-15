package party.iroiro.juicemacs.elisp.runtime;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ELispBindingScopeTest {
    @Test
    public void testDynamicBinding() {
        try (Context context = Context.create()) {
            Value result = context.eval("elisp", """
                    ;;; -*- lexical-binding: nil -*-
                    (progn
                      (setq x -1)
                      (defalias 'getx #'(lambda () x))
                      (setq x 1)
                      (let ((x 2))
                        (getx)))
                    """
            );
            assertEquals(2L, result.asLong());
        }
    }

    @Test
    public void testLexicalBinding() {
        try (Context context = Context.create()) {
            Value result = context.eval("elisp", """
                    ;;; -*- lexical-binding: t -*-
                    (progn
                      (setq x -1)
                      (defalias 'getx #'(lambda () x))
                      (setq x 1)
                      (let ((x 2))
                        (getx)))
                    """
            );
            assertEquals(1L, result.asLong());
        }
    }
}
