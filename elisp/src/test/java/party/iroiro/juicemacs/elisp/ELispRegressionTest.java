package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Value;
import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.function.Executable;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.ELispLanguageTest.testWithDumped;

public class ELispRegressionTest {
    private static final String[] TEST_FILES = {
            // built-in function tests
            // - core ones
            "src/alloc-tests",
            "src/chartab-tests",
            "src/data-tests",
            "src/eval-tests",
            "src/fns-tests",
            "src/floatfns-tests",
            // - others
            "src/character-tests",
            "src/decompress-tests",

            // elisp regression tests
            "lisp/emacs-lisp/backquote-tests",
//            "lisp/emacs-lisp/bytecomp-tests", // TODO: needs font-lock-mode
            "lisp/emacs-lisp/byte-run-tests",
            "lisp/emacs-lisp/cconv-tests",
//            "lisp/emacs-lisp/cl-generic-tests", // TODO: needs edebug
            "lisp/emacs-lisp/cl-preloaded-tests",
            "lisp/emacs-lisp/macroexp-tests",
            "lisp/emacs-lisp/nadvice-tests",
            "lisp/emacs-lisp/oclosure-tests",
            "lisp/emacs-lisp/pcase-tests",
            "lisp/subr-tests",
    };
    private final CompletableFuture<List<DynamicTest>> testCases = new CompletableFuture<>();
    private final CompletableFuture<Map<String, TestResult>> testResults = new CompletableFuture<>();
    private final AtomicBoolean erroredOut = new AtomicBoolean();

    @TestFactory
    public Collection<DynamicTest> emacsRegressionTests() throws ExecutionException, InterruptedException {
        startTestRunner();
        return testCases.get();
    }

    private void startTestRunner() {
        Thread.ofPlatform().start(() -> noThrow(() -> testWithDumped((context) -> {
            try {
                // Extract available tests
                List<DynamicTest> testCases = new ArrayList<>();
                context.eval("elisp", """
                        ;; TODO: pp requires a bunch of sexp parsing functions
                        (require 'pp)
                        (defalias 'pp 'princ)
                        ;; ert asserts about newlines produced by pp
                        (defun cl--assertion-failed (&rest _) t)
                        
                        (require 'ert)
                        """);
                for (String test : TEST_FILES) {
                    context.eval("elisp", "(load \"../test/%s\")".formatted(test));
                }
                Value tests = context.eval("elisp", """
                        (apply #'vector (mapcar (lambda (test) (vector (ert-test-name test)
                                                                       (ert-test-file-name test)))
                                                (ert-select-tests t t)))
                        """);
                long count = tests.getArraySize();
                for (long i = 0; i < count; i++) {
                    Value info = tests.getArrayElement(i);
                    String name = info.getArrayElement(0).asString();
                    Path file = Path.of(info.getArrayElement(1).asString());
                    testCases.add(DynamicTest.dynamicTest(name + "@" + file.getFileName(), () -> {
                        Map<String, TestResult> results;
                        try {
                            results = testResults.get();
                        } catch (ExecutionException e) {
                            if (erroredOut.compareAndSet(false, true)) {
                                throw e;
                            }
                            Assumptions.abort();
                            return;
                        }
                        TestResult result = results.get(name);
                        if (result == null) {
                            Assumptions.abort();
                            return;
                        }
                        switch (result.state) {
                            case Failed -> fail(result.info == null ? result.name : result.info);
                            case Skipped -> Assumptions.abort("skipped");
                            case Passed -> {}
                        }
                    }));
                }
                this.testCases.complete(testCases);
            } catch (Throwable e) {
                this.testCases.completeExceptionally(e);
                return;
            }

            try {
                // Run tests
                context.eval("elisp", """
                        (null (ert-run-tests-batch)) ; don't print the huge info object
                        """);
            } catch (Throwable e) {
                this.testResults.completeExceptionally(e);
            }
        }, (file) -> {
            // Extract failed tests
            // TODO: maybe use ert-write-junit-test-report later
            Map<String, TestResult> results = Collections.synchronizedMap(new HashMap<>());
            Pattern resultPattern = Pattern.compile(
                    "^\\s+(passed|skipped|FAILED)\\s+(\\d+/\\d+)\\s+([^ ]+)"
            );
            noThrow(() -> {
                try (BufferedReader reader = Files.newBufferedReader(file)) {
                    String lastLine = null;
                    String line;
                    while ((line = reader.readLine()) != null) {
                        Matcher matcher = resultPattern.matcher(line);
                        if (matcher.find()) {
                            String name = matcher.group(3);
                            results.put(name, new TestResult(name, lastLine, TestState.fromString(matcher.group(1))));
                        }
                        lastLine = line;
                    }
                }
            });
            this.testResults.complete(results);
        })));
    }

    private void noThrow(Executable executable) {
        try {
            executable.execute();
        } catch (Throwable e) {
            testResults.completeExceptionally(e);
        }
    }

    private enum TestState {
        Passed,
        Failed,
        Skipped;

        public static TestState fromString(String string) {
            return switch (string) {
                case "passed" -> Passed;
                case "skipped" -> Skipped;
                case "FAILED" -> Failed;
                default -> throw new IllegalArgumentException("Unknown test state: " + string);
            };
        }
    }
    private record TestResult(String name, @Nullable String info, TestState state) {
    }
}
