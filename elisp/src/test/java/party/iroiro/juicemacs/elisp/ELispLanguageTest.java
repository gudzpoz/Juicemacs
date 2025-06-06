package party.iroiro.juicemacs.elisp;

import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.polyglot.Context;


import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.io.FileSystem;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ELispLanguageTest {
    /// Returns a Truffle filesystem with working directory set correctly
    ///
    /// A correctly set PWD allows IDEA to click-to-jump to source location.
    private static FileSystem getFileSystem() {
        FileSystem fileSystem = FileSystem.newDefaultFileSystem();
        fileSystem.setCurrentWorkingDirectory(Path.of("..").toAbsolutePath()); // project root dir
        return fileSystem;
    }

    public static IOAccess getTestIOAccess() {
        return IOAccess.newBuilder().fileSystem(getFileSystem()).build();
    }

    public static Context.Builder getContextBuilder(@Nullable PrintStream out) {
        String loadPath = Path.of("emacs", "lisp").toAbsolutePath().toString();
        String dataPath = Path.of("emacs", "etc").toAbsolutePath().toString();
        Context.Builder builder = Context.newBuilder("elisp")
                .allowExperimentalOptions(true)
                // Uncomment the following two lines to use an external debugger for Lisp before we get to edebug.
//                .option("elisp.truffleDebug", "true")
//                .option("inspect", "4242")
                // Uncomment the following two lines to adjust compilation settings.
                // Basically:
                // - engine.Compilation=false: Debug Java code,
                // - engine.Compilation=true && engine.CompilationFailureAction=Diagnose: Debug JIT compilation.
//                .option("engine.Compilation", "false")
//                .option("engine.CompilationFailureAction", "Diagnose")
                .environment("EMACSLOADPATH", loadPath)
                .environment("EMACSDATA", dataPath)
                .allowIO(getTestIOAccess());
        if (out != null) {
            builder.out(out);
        }
        return builder;
    }

    public static boolean hasDump(boolean bootstrap) {
        File file = getFileSystem().toAbsolutePath(Path.of(bootstrap ? "bootstrap-emacs.pdmp" : "emacs.pdmp")).toFile();
        return file.isFile();
    }

    public static void tryDump(boolean bootstrap, @Nullable PrintStream out) {
        String dumpMode = bootstrap ? "pbootstrap" : "pdump";
        if (hasDump(bootstrap)) {
            Objects.requireNonNullElse(out, System.out).println(
                    dumpMode + " file exists, skip dumping"
            );
            return;
        }
        Context.Builder builder = getContextBuilder(out);
        if (!bootstrap) {
            builder.option("elisp.dumpFile", "bootstrap-emacs.pdmp");
        }
        try (Context context = builder.build()) {
            // Circumvent dependency on a bootstrapped environment
            // - .elc files expect bootstrapped environment
            context.eval("elisp", "(setq load-suffixes '(\".el\"))");
            // - (noninteractive . nil) leads to usages of user-emacs-directory,
            //   which is only available after bootstrapping
            context.eval("elisp", "(setq noninteractive t)");

            // Loads until an error
            context.eval("elisp", "(setq dump-mode \"" + dumpMode + "\")");
            context.eval("elisp", "(load \"loadup\")");
        } catch (PolyglotException e) {
            // loadup.el calls (kill-emacs) after dumping
            String message = e.getMessage();
            String expected = "(fatal kill-emacs nil)";
            if (!message.equals(expected)) {
                e.printStackTrace(Objects.requireNonNullElse(out, System.err));
            }
            assertEquals(expected, message);
        }
    }

    @Test
    public void test() throws IOException {
        Path file = Files.createTempFile("juicemacs-ert", ".txt");
        try (PrintStream out = createOut(file.toFile())) {
            System.out.println("Output: " + file);
            tryDump(true, out);
            tryDump(false, out);
            try (Context context = getContextBuilder(out).option("elisp.dumpFile", "emacs.pdmp").build()) {
                // Test bytecode compiler
                context.eval("elisp", """
                        ;; -*- lexical-binding: t -*-
                        (require 'bytecomp)
                        (setq byte-compile-debug t)
                        (message "%s" (byte-compile (lambda ())))
                        (message "%s" (byte-compile (lambda (x) (1+ x))))
                        (message "%s" (byte-compile (lambda (x) (+ x 2))))
                        """);
                // Try to run ERT
                context.eval("elisp", """
                        ;; TODO: pp requires a bunch of sexp parsing functions
                        (require 'pp)
                        (defalias 'pp 'princ)
                        ;; ert asserts about newlines produced by pp
                        (defun cl-assert (&rest _) t)

                        (require 'ert)
                        (load "../test/src/data-tests")
                        (load "../test/src/floatfns-tests")
                        (null (ert-run-tests-batch)) ; don't print the huge info object
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
