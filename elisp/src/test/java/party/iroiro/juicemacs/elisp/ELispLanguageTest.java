package party.iroiro.juicemacs.elisp;

import org.jspecify.annotations.Nullable;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;

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

    @Test
    public void test() throws IOException {
        Path file = Files.createTempFile("juicemacs-ert", ".txt");
        try (PrintStream out = TestingUtils.createOut(file.toFile())) {
            System.out.println("Output: " + file);
            tryDump(true, out);
            tryDump(false, out);
            try (Context context = TestingUtils.getContextBuilder(out).option("elisp.dumpFile", "emacs.pdmp").build()) {
                try {
                    context.eval("elisp", "(eval top-level)");
                } catch (PolyglotException e) {
                    e.printStackTrace(out);
                }
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
                        (defun cl--assertion-failed (&rest _) t)

                        (require 'ert)
                        (load "../test/src/data-tests")
                        (load "../test/src/fns-tests")
                        (load "../test/src/floatfns-tests")
                        (null (ert-run-tests-batch)) ; don't print the huge info object
                        """);
            } catch (PolyglotException e) {
                e.printStackTrace(out);
            }
        }
        // Extract failed tests
        // TODO: maybe use ert-write-junit-test-report later
        Pattern resultPattern = Pattern.compile(
                "^\\s+(passed|FAILED)\\s+(\\d+/\\d+)\\s+([^ ]+)"
        );
        try (BufferedReader reader = Files.newBufferedReader(file)) {
            record TestResult(String name, @Nullable String info, boolean passed) {}
            ArrayList<TestResult> tests = new ArrayList<>();
            String lastLine = null;
            String line;
            while ((line = reader.readLine()) != null) {
                Matcher matcher = resultPattern.matcher(line);
                if (matcher.find()) {
                    tests.add(new TestResult(matcher.group(3), lastLine, matcher.group(1).equals("passed")));
                }
                lastLine = line;
            }
            assertAll(tests.stream().map(test -> () ->
                    assertTrue(test.passed, test.name + (test.info == null ? "" : ": " + test.info))));
        }
    }
}
