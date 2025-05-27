package party.iroiro.juicemacs.elisp.runtime.pdump;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.ELispLanguageTest.getTestIOAccess;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContextBuilder;

public class ELispPortableDumperTest {
    private void testBasicDump(boolean bare) throws IOException {
        File dump = File.createTempFile("juicemacs", ".pdmp");
        try(Context context = getTestingContextBuilder()
                .option("elisp.bare", bare ? "true" : "false")
                .build()) {
            context.eval("elisp", "(setq some-value 42)");
            context.eval("elisp", "(defalias 'my-func #'(lambda () (1+ some-value)))");
            context.eval("elisp", "(dump-emacs-portable \"" + dump.getAbsolutePath() + "\")");
        }
        try (Context context = getTestingContextBuilder()
                .allowIO(getTestIOAccess())
                .allowExperimentalOptions(true)
                .option("elisp.dumpFile", dump.getAbsolutePath())
                .build()
        ) {
            Value value = context.eval("elisp", "(1- (my-func))");
            assertEquals(42L, value.asLong());
        }
    }

    @Test
    public void testBareDump() throws IOException {
        testBasicDump(true);
    }

    @Test
    public void testCoreDump() throws IOException {
        testBasicDump(false);
    }

    @Test
    public void testLoadedEmacsDump() {
        try (Context context = getTestingContextBuilder()
                     .allowIO(getTestIOAccess())
                     .build()
        ) {
            // Circumvent dependency on a bootstrapped environment
            context.eval("elisp", """
                        (setq load-suffixes '(".el") ;; .elc files expect bootstrapped environment
                              noninteractive t       ;; (noninteractive . nil) leads to usages of user-emacs-directory,
                                                     ;; which is only available after bootstrapping
                              dump-mode "pbootstrap")
                        """);
            // Loads until an error
            context.eval("elisp", "(load \"loadup\")");
        } catch (PolyglotException e) {
            // loadup.el calls (kill-emacs) after dumping
            assertEquals("(fatal nil)", e.getMessage(), e.getMessage());
        }
        try (Context context = getTestingContextBuilder()
                .allowIO(getTestIOAccess())
                .allowExperimentalOptions(true)
                .option("elisp.dumpFile", "bootstrap-emacs.pdmp")
                .build()
        ) {
            Value value = context.eval("elisp", """
                    (rx (? "bootstrap-") "emacs" (? ".pdmp"))
                    """);
            System.out.println(value);
        }
    }
}
