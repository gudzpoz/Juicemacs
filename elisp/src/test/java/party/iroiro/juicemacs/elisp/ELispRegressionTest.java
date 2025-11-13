package party.iroiro.juicemacs.elisp;

import org.graalvm.polyglot.Value;
import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.function.Executable;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.ELispLanguageTest.testWithDumped;

public class ELispRegressionTest {
    private final CompletableFuture<List<DynamicTest>> testCases = new CompletableFuture<>();
    private final CompletableFuture<Map<String, TestResult>> testResults = new CompletableFuture<>();
    private final List<Throwable> outOfTestExceptions = Collections.synchronizedList(new ArrayList<>());

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
                Value tests = context.eval("elisp", """
                        ;; TODO: pp requires a bunch of sexp parsing functions
                        (require 'pp)
                        (defalias 'pp 'princ)
                        ;; ert asserts about newlines produced by pp
                        (defun cl--assertion-failed (&rest _) t)
                        
                        (require 'ert)
                        (load "../test/src/data-tests")
                        (load "../test/src/fns-tests")
                        (load "../test/src/floatfns-tests")
                        (apply #'vector (mapcar #'ert-test-name (ert-select-tests t t)))
                        """);
                long count = tests.getArraySize();
                for (long i = 0; i < count; i++) {
                    String name = tests.getArrayElement(i).asString();
                    testCases.add(DynamicTest.dynamicTest(name, () -> {
                        Map<String, TestResult> results = testResults.get();
                        TestResult result = results.get(name);
                        assertNotNull(result);
                        switch (result.state) {
                            case Failed -> fail(result.info == null ? result.name : result.info);
                            case Skipped -> Assumptions.abort("skipped");
                            case Passed -> {}
                        }
                        if (!outOfTestExceptions.isEmpty()) {
                            Throwable old = outOfTestExceptions.get(0);
                            outOfTestExceptions.clear();
                            assertDoesNotThrow(() -> {
                                throw old;
                            });
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
            outOfTestExceptions.add(e);
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
