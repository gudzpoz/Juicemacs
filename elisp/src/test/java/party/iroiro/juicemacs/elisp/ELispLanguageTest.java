package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Context;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import java.nio.file.Path;

public class ELispLanguageTest {

    @Disabled
    @Test
    public void test() {
        try (Context context = Context.newBuilder("elisp")
                .environment("ELISP_PATH", "")
                .build()
        ) {
            ELispGlobals.loadPath.setValue(new ELispCons(new ELispString(
                    Path.of(".", "emacs", "lisp").toAbsolutePath().toString()
            )));
            ELispContext.COMMAND_LINE_ARGS.setValue(NIL);
            context.eval("elisp", "(load \"loadup\")");
        }
    }

}
