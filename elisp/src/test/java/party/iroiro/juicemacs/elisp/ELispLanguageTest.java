package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Context;


import org.graalvm.polyglot.PolyglotException;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;

public class ELispLanguageTest {

    @Disabled
    @Test
    public void test() {
        try (Context context = Context.newBuilder("elisp")
                .environment("EMACSLOADPATH", Path.of("emacs", "lisp").toAbsolutePath().toString())
                .environment("EMACSDATA", Path.of("emacs", "etc").toAbsolutePath().toString())
                .build()
        ) {
            context.eval("elisp", "(load \"loadup\")");
        } catch (PolyglotException e) {
            e.printStackTrace();
        }
    }

}
