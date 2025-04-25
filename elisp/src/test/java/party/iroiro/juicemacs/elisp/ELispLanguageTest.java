package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Context;


import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.io.FileSystem;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;

public class ELispLanguageTest {
    /// Returns a Truffle filesystem with working directory set correctly
    ///
    /// A correctly set PWD allows IDEA to click-to-jump to source location.
    private FileSystem getFileSystem() {
        FileSystem fileSystem = FileSystem.newDefaultFileSystem();
        fileSystem.setCurrentWorkingDirectory(Path.of("..").toAbsolutePath()); // project root dir
        return fileSystem;
    }

    public IOAccess getTestIOAccess() {
        return IOAccess.newBuilder().fileSystem(getFileSystem()).build();
    }

    @Test
    public void test() throws IOException {
        Path file = Files.createTempFile("juicemacs-ert", ".txt");
        String loadPath = Path.of("emacs", "lisp").toAbsolutePath().toString();
        String dataPath = Path.of("emacs", "etc").toAbsolutePath().toString();
        try (PrintStream out = createOut(file.toFile());
             Context context = Context.newBuilder("elisp")
                     .allowExperimentalOptions(true)
                     // Uncomment the following two lines to use an external debugger for Lisp before we get to edebug.
//                     .option("elisp.truffleDebug", "true")
//                     .option("inspect", "4242")
                     // Uncomment the following two lines to adjust compilation settings.
                     // Basically:
                     // - engine.Compilation=false: Debug Java code,
                     // - engine.Compilation=true && engine.CompilationFailureAction=Diagnose: Debug JIT compilation.
//                     .option("engine.Compilation", "false")
//                     .option("engine.CompilationFailureAction", "Diagnose")
                     .environment("EMACSLOADPATH", loadPath)
                     .environment("EMACSDATA", dataPath)
                     .allowIO(getTestIOAccess())
                     .out(out)
                     .build()
        ) {
            System.out.println("Output: " + file);
            // Loads until an error
            try {
                context.eval("elisp", "(load \"loadup\")");
            } catch (PolyglotException e) {
                e.printStackTrace(out);
            }
            // Tries to run ERT
            try {
                context.eval("elisp", """
                        ;; easy-mmode--mode-docstring and pp use lots of buffer operations,
                        ;; which are not supported yet.
                        (require 'pp)
                        (defun easy-mmode--mode-docstring (&rest _) "")
                        (defalias 'pp 'princ)
                        (defun cl-assert (&rest _) t)
                        
                        (require 'ert)
                        (load "../test/src/data-tests")
                        (ert-run-tests-batch)
                        nil
                        """);
            } catch (PolyglotException e) {
                e.printStackTrace(out);
            }
            // TODO: Make the test fail when there are errors, after we fully bootstrap loadup.el.
        }
    }

    private PrintStream createOut(File file) throws FileNotFoundException {
        return new PrintStream(new OutputStream() {
            final FileOutputStream out = new FileOutputStream(file);

            @Override
            public void write(int b) throws IOException {
                out.write(b);
                System.out.write(b);
            }

            @Override
            public void flush() throws IOException {
                out.flush();
                System.out.flush();
            }

            @Override
            public void close() throws IOException {
                out.close();
            }
        });
    }

}
