package party.iroiro.juicemacs.elisp;

import org.jspecify.annotations.Nullable;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.*;

/// Basic tests like pdump and byte-compile
public class ELispLanguageTest {
    public static boolean hasDump(boolean bootstrap) {
        File file = TestingUtils.getFileSystem().toAbsolutePath(Path.of(bootstrap ? "bootstrap-emacs.pdmp" : "emacs.pdmp")).toFile();
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
        Context.Builder builder = TestingUtils.getContextBuilder(out).option("elisp.portableDump", dumpMode);
        try (Context context = builder.build()) {
            if (bootstrap) {
                // During pbootstrap, Emacs expects to be in interpreted (*not bytecode*) mode.
                // Also, it should load ldef-boot.el instead of loaddefs.el[c], because emacs-bootstrap
                // is the one that will generate these files.
                // Instead of deleting these files (*.elc & loaddefs.el) every single time we test things,
                // we override some code to handle this automatically.
                context.eval("elisp", """
                        (setq load-suffixes '(".el"))
                        (defalias 'juicemacs---load (symbol-function 'load))
                        (defalias 'load #'(lambda (file &rest rest)
                          (if (equal "loaddefs" file)
                              (signal 'file-error nil)
                            (apply #'juicemacs---load file rest))))
                        """);
            }
            // Loads until an error
            context.eval("elisp", "(load \"loadup\")");
        } catch (PolyglotException e) {
            // loadup.el calls (kill-emacs) after dumping
            String message = e.getMessage();
            if (bootstrap) {
                assertEquals("(fatal kill-emacs nil)", message, () -> {
                    e.printStackTrace(System.err);
                    return e.getMessage();
                });
            } else {
                // During pdump, Emacs also renames the emacs binary,
                // which, of course, our Juicemacs does not provide.
                assertTrue(
                        message.startsWith("(file-error ")
                                && message.endsWith("/emacs\")"),
                        () -> {
                            e.printStackTrace(System.err);
                            return message;
                        }
                );
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testDump(boolean bootstrap) {
        tryDump(bootstrap, null);
    }

    public static Path testWithDumped(Consumer<Context> test) throws IOException {
        Path file = Files.createTempFile("juicemacs-ert", ".txt");
        try (PrintStream out = TestingUtils.createOut(file.toFile())) {
            System.out.println("Output: " + file);
            tryDump(false, out);
            try (Context context = TestingUtils.getContextBuilder(out)
                    .option("elisp.dumpFile", "emacs.pdmp")
                    .option("elisp.convertUnsupportedException", "true")
                    .build()) {
                try {
                    context.eval("elisp", "(eval top-level)");
                } catch (PolyglotException e) {
                    e.printStackTrace(out);
                }
                test.accept(context);
            } catch (PolyglotException e) {
                e.printStackTrace(out);
            }
        }
        file.toFile().deleteOnExit(); // only deletes if no exceptions
        return file;
    }

    @Test
    public void testBasicByteCompile() throws IOException {
        testWithDumped((context) -> context.eval("elisp", """
                    ;; -*- lexical-binding: t -*-
                    (require 'bytecomp)
                    (setq byte-compile-debug t)
                    (message "%s" (byte-compile (lambda ())))
                    (message "%s" (byte-compile (lambda (x) (1+ x))))
                    (message "%s" (byte-compile (lambda (x) (+ x 2))))
                    """));
    }

    @Test
    public void testClGeneric() throws IOException {
        testWithDumped((context) -> assertDoesNotThrow(() -> context.eval("elisp", """
                ;; -*- lexical-binding: t -*-
                (require 'map)
                (load "map.el")
                (require 'cl-macs)
                (cl-assert (equal (map-into #s(hash-table data (x 1 y 2)) 'list)
                                  '((x . 1) (y . 2))))
                (cl-assert (equal (map-into #s(hash-table data (x 1 y 2)) 'plist)
                                  '(x 1 y 2)))
                """)));
    }
}
