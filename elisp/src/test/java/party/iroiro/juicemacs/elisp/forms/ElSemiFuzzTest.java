package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.mule.MuleString;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.TestingUtils.getContextBuilder;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class ElSemiFuzzTest {
    /// Exclude non-pure or context-dependent functions
    private static final Map<Class<? extends ELispBuiltIns>, List<String>> EXCLUDED_FUNCTIONS = Map.ofEntries(
            Map.entry(BuiltInData.class, List.of(
                    // context-dependent
                    "symbol-plist",
                    "symbol-value",
                    "symbol-function",
                    "default-boundp",
                    "default-value",
                    // non-pure
                    ".*-variable-watcher",
                    "make-variable-buffer-local",
                    ".*-local-variable",
                    ".*makunbound",
                    "native-comp-.*",
                    "variable-binding-locus",
                    // TODO
                    "setcar",
                    "setcdr",
                    "setplist",
                    "aset", // fuzzing aset is also meaningless because it returns the element as is
                    "subr-.*",
                    "command-modes",
                    "interactive-form",
                    "position-symbol",
                    "symbol-with-pos",
                    "remove-pos-from-symbol"
            ))
    );
    public static final String LISP_FLOAT_PATTERN = "([-0-9]+?\\.\\d+?|[-0-9]{19,}|[-0-9.]+e\\+[INFNaN]{3})";
    private static final Map<String, Predicate> EXPECTED_FAILURES = Map.ofEntries(
            // # data.c
            // We don't overflow for `0 << big_int`, for it is always 0.
            Map.entry("ash", Predicate.or(
                    Predicate.expectNoError("\\(ash '0 ", "overflow-error"),
                    Predicate.expectError("\\(ash '\\d+ '\\d{4,}", "overflow-error")
            )),
            // We compare doubles and big integers by their values instead of pointers.
            Map.entry("eq", Predicate.expectOutput(
                    "\\(eq '" + LISP_FLOAT_PATTERN + " '\\1\\)", "(124 . t)"
            )),
            // I do think the approach taken by Java to *not* distinguish between NaN values is better.
            Map.entry("mod", Predicate.expectNaNArithmetic()),
            Map.entry("+", Predicate.expectNaNArithmetic()),
            // Java: Both 5e-324 and 4.9e-324 prints as 4.9e-324
            // Emacs: Both 5e-324 and 4.9e-324 prints as 5e-324
            Map.entry("number-to-string", Predicate.expectReplaced(
                    "\"(-?)5e-324\"", "\"$14.9e-324\""
            )),
            // We have a different most-positive-fixnum. So when Emacs throws
            // wrong-type-argument errors from bignum objects, we might behave differently.
            Map.entry("aref", Predicate.expectError(" '[-0-9]{19,}\\)", "args-out-of-range")),
            Map.entry("cl-type-of", Predicate.expectOutputPattern(" '[-0-9]{19,}\\)", "fixnum"))
    );

    private boolean isExpectedFailure(String function, String input, String expected, String actual) {
        Predicate predicate = EXPECTED_FAILURES.get(function);
        if (predicate == null) {
            return false;
        }
        return predicate.test(input, expected, actual);
    }

    @SuppressWarnings("NotNullFieldNotInitialized")
    Context context;
    @SuppressWarnings("NotNullFieldNotInitialized")
    Value bindings;
    @BeforeAll
    public void setup() {
        context = getContextBuilder(System.out)
                .option("elisp.dumpFile", "emacs.pdmp")
                .build();
        context.eval("elisp", """
                    (defun fuzz-case (s)
                      (let* ((case (read (decode-coding-string s 'utf-8-emacs)))
                             (type (car case))
                             (expr (nth 1 case))
                             (result (cons type (nth 2 case)))
                             (actual (condition-case err
                                         (cons ?| (eval expr))
                                       (error (cons ?! (car err))))))
                        (vector (prin1-to-string result)
                                (prin1-to-string actual)
                                (car expr))))
                    """);
        bindings = context.getBindings("elisp");
    }
    @AfterAll
    public void tearDown() {
        context.close();
    }

    public static Stream<String> getFuzzableFunctions() {
        ELispBuiltIns[] builtIns = {
                new BuiltInData(),
        };
        return Arrays.stream(builtIns).flatMap(ElSemiFuzzTest::getFuzzableFunctions);
    }
    public static Stream<String> getFuzzableFunctions(ELispBuiltIns file) {
        List<Pattern> blocklist = EXCLUDED_FUNCTIONS.get(file.getClass()).stream()
                .map(Pattern::compile)
                .toList();
        return file.getNodeFactories().stream()
                .flatMap((factory) ->
                        Arrays.stream(factory.getNodeClass().getAnnotationsByType(ELispBuiltIn.class))
                )
                .map(ELispBuiltIn::name)
                .sorted()
                .filter((name) -> blocklist.stream().noneMatch(re -> re.matcher(name).find()));
    }

    @ParameterizedTest
    @MethodSource("getFuzzableFunctions")
    public void testElSemiFuzz(String functionName) throws IOException {
        String funcall = "(esfuzz-run-fuzz nil \"^" + functionName.replace("\"", "\\\"") + "$\" t)";
        String[] commands = new String[] {
                "emacs", "-Q", "-nw", "--batch",
                "-L", ".", "-l", "el-semi-fuzz",
                "--eval", funcall,
        };
        Path cwd = Path.of("scripts", "emacs-extractor");
        Process fuzzer = new ProcessBuilder(commands)
                .directory(cwd.toFile())
                .start();
        ArrayList<Throwable> failures = new ArrayList<>();
        int count = 0;
        try (InputStream is = fuzzer.getInputStream()) {
            while (failures.size() < 50) {
                int len = readInt(is);
                if (len <= 0) {
                    break;
                }
                byte[] bytes = is.readNBytes(len);
                ELispString expr = new ELispString(MuleString.fromRaw(bytes));
                try {
                    count++;
                    bindings.putMember("args", expr);
                    Value extracted = context.eval("elisp", "(fuzz-case args)");
                    String expected = extracted.getArrayElement(0).asString();
                    String actual = extracted.getArrayElement(1).asString();
                    String function = extracted.getArrayElement(2).asString();
                    if (!expected.equals(actual)) {
                        String input = new String(bytes);
                        if (!isExpectedFailure(function, input, expected, actual)) {
                            assertEquals(expected, actual, input);
                        }
                    }
                } catch (AssertionError e) {
                    System.err.println(e.getMessage());
                    failures.add(e);
                } catch (Throwable e) {
                    System.err.println(e.getMessage() + ": " + new String(bytes));
                    e.printStackTrace(System.err);
                    failures.add(e);
                    if (e instanceof UnsupportedOperationException) {
                        break;
                    }
                }
            }
        } finally {
            System.err.println("elfuzz: " + functionName + ": " + count + " tests");
            fuzzer.destroy();
            assertEquals(0, failures.size(), () -> getStackTraceString(failures.getFirst()));
        }
    }

    private String getStackTraceString(Throwable e) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        e.printStackTrace(new PrintStream(out));
        return out.toString();
    }

    private static int readInt(InputStream is) throws IOException {
        int c = ' ';
        while (c != -1 && Character.isWhitespace(c)) {
            c = is.read();
        }
        int integer = 0;
        while (c != -1 && Character.isDigit(c)) {
            integer = integer * 10 + c - '0';
            c = is.read();
        }
        return integer;
    }

    private interface Predicate {
        boolean test(String input, String expected, String actual);

        static Predicate expectNoError(String inputPattern, String error) {
            Pattern pattern = Pattern.compile(inputPattern);
            return (input, expected, actual) ->
                    expected.contains(error)
                    && !actual.contains(error)
                    && pattern.matcher(input).find();
        }

        static Predicate expectError(String inputPattern, String error) {
            Pattern pattern = Pattern.compile(inputPattern);
            return (input, expected, actual) ->
                    actual.contains(error)
                    && !expected.contains(error)
                    && pattern.matcher(input).find();
        }

        static Predicate expectOutput(String inputPattern, String exactOutput) {
            Pattern pattern = Pattern.compile(inputPattern);
            return (input, _, actual) ->
                    pattern.matcher(input).find()
                    && actual.equals(exactOutput);
        }

        static Predicate expectOutputPattern(String inputPattern, String outputPattern) {
            Pattern pattern = Pattern.compile(inputPattern);
            Pattern outPattern = Pattern.compile(outputPattern);
            return (input, _, actual) ->
                    pattern.matcher(input).find()
                    && outPattern.matcher(actual).find();
        }

        static Predicate expectReplaced(String expectedPattern, String replacement) {
            Pattern pattern = Pattern.compile(expectedPattern);
            return (_, expected, actual) -> {
                expected = pattern.matcher(expected).replaceAll(replacement);
                return expected.equals(actual);
            };
        }

        static Predicate expectNaNArithmetic() {
            return (input, expected, actual) ->
                    input.contains(".0e+NaN")
                    && expected.contains(".0e+NaN")
                    && actual.contains(".0e+NaN");
        }

        static Predicate or(Predicate... predicates) {
            return (input, expected, actual) -> Arrays.stream(predicates)
                    .anyMatch(p -> p.test(input, expected, actual));
        }
    }
}
