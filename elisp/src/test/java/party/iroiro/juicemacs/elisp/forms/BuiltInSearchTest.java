package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@Fork(1)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 3, time = 5)
@State(Scope.Benchmark)
public class BuiltInSearchTest extends BaseFormTest {
    private static final Object[] TESTS;

    private final static String[] SYNTAX_FREE_MATCHES = {
            "^abc$", "abc",
            "^\\(\\(abc\\)\\|\\(d\\)\\)\\(?:abc\\)\\{1\\}$", "dabc",
            "^[[:ascii:]][[:alnum:]][[:alpha:]][[:blank:]][[:cntrl:]][[:digit:]][[:graph:]][[:lower:]]" +
                    "[[:multibyte:]][[:nonascii:]][[:print:]][[:punct:]][[:space:]]" +
                    "[[:unibyte:]][[:upper:]][[:word:]][[:xdigit:]]$",
            "a1a \n1aa🧃🧃a, aAwB",
            "^[_[:alpha:]]\\{4\\}$", "_abc",
            "^[][]\\{4\\}$", "]][[",
            "^[\\]$", "\\",
            "^[[:a-z]+$", ":abcdefg",
            "^[[:a-z:-]+$", ":abcdefg-",
            "^()|{}$", "()|{}",
            "^\\(abc\\)\\1$", "abcabc",
            "^\\n$", "n",
            "\\`abc\\'", "abc",
            "\\babc\\b", "abc",
            "\\babc\\B", "abcD",
            "\\<abc\\>", "abc",
            "^\\w\\{3\\}\\W\\{3\\}$", "abc   ",
            "^(\\_<make-char-table\\_>)$", "(make-char-table)",
            "^[0-9]+\\.[0-9]+$", "123.456", // look-ahead optimization
            "^.?b$", "ab", // look-ahead optimization
            "^.??b$", "ab", // look-ahead optimization
            "^a\\{4\\}$", "aaaa", // a{4} -> aaaa optimization
            "^a\\{4,\\}$", "aaaaaaaa", // a{4,} -> aaaa+ optimization
    };

    public static final String REGEXP_TEST = """
            (progn
              (string-match
                "^a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaaaaaaaaaa"
                "aaaaaaaaaaaaaaaaaaaaaaaa")
              (match-end 0))
            """;

    static {
        Object[] tests = {
                "(string-match \"abc\" \"abc\")", 0L,
                "(string-match \"abc\" \"---abc\")", 3L,
                "(string-match \"abc\" \"abc---\")", 0L,
                "(string-match \"abc\" \"---cba---\")", false,
                """
            (let ((start (string-match "-\\\\(abc\\\\)d" "--abcd--")))
              (and (eq start 1)
                   (eq (match-beginning 0) 1) (eq (match-end 0) 6)
                   (eq (match-beginning 1) 2) (eq (match-end 1) 5)))""", true,
                "(progn (string-match \"abc\" \"abc\") (equal (match-data) '(0 3)))", true,
                "(progn (set-match-data '(1 2 3)) (equal (match-data) '(1 2)))", true,
        };
        ArrayList<Object> array = new ArrayList<>(Arrays.asList(tests));
        for (int i = 0; i < SYNTAX_FREE_MATCHES.length; i += 2) {
            String regex = SYNTAX_FREE_MATCHES[i].replace("\\", "\\\\");
            String str = SYNTAX_FREE_MATCHES[i + 1].replace("\\", "\\\\");
            array.add("(string-match \"" + regex + "\" \"" + str + "\")");
            array.add(0L);
        }
        TESTS = array.toArray();
    }

    @Override
    protected Object[] entries() {
        return TESTS;
    }

    @Test
    void testOsrShouldNotBailout() {
        try (Context context = getTestingContext()) {
            assertEquals(24L, context.eval("elisp", REGEXP_TEST).asLong());
            assertEquals(0L, context.eval("elisp", """
                    (string-match "\\\\`<[^ <>\\t\\n\\f][^>\\t\\n\\f]*>" "<mouse-1>")
                    """
            ).asLong());
            assertEquals(0L, context.eval("elisp", """
                    (string-match "^[_[:alpha:]]\\\\{4\\\\}$" "_abc")
                    """
            ).asLong());
        }
    }

    @Test
    void testCaseFold() {
        try (Context context = getTestingContext()) {
            assertEquals(0L, context.eval("elisp", """
                    (let ((case-fold-search t))
                      (string-match "^\\\\(az_\\\\)\\\\1[a-z][a-z][A-Z][A-Z]文🧃$" "AZ_az_lUlU文🧃"))
                    """).asLong());
            assertEquals(0L, context.eval("elisp", """
                    (let ((case-fold-search t))
                      (string-match "^\\\\(AZ_\\\\)\\\\1[a-z][a-z][A-Z][A-Z]$" "az_AZ_UlUl"))
                    """).asLong());
        }
    }

    @SuppressWarnings("NotNullFieldNotInitialized")
    private Context context;

    @Setup
    public void setup() throws IOException {
        context = getTestingContext();
        context.eval("elisp", "(setq case-fold-search nil)");
        context.eval(Source.newBuilder("elisp", """
                ;;; -*- lexical-binding: t -*-
                (defalias
                  'long-regexp
                  #'(lambda ()\s""" + REGEXP_TEST + " 1)) nil", "long-regexp").build());
        context.eval(Source.newBuilder("elisp", """
                ;;; -*- lexical-binding: t -*-
                (defalias
                  'version-regexp
                  #'(lambda ()
                      (string-match
                       "\\\\([^.].*?\\\\)-\\\\([0-9]+\\\\(?:[.][0-9]+\\\\|\\\\(?:pre\\\\|beta\\\\|alpha\\\\)[0-9]+\\\\)*\\\\)"
                       "some-package-0.1.0beta1")
                      (match-end 0)))
                """, "version-regexp").build());
    }

    @TearDown
    public void tearDown() {
        context.close();
    }

    @Benchmark
    public long regExp() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(long-regexp)", "<regexp-test>").build());
        return v.asLong();
    }
    @Benchmark
    public long versionRegExp() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(version-regexp)", "<version-regexp-test>").build());
        return v.asLong();
    }

    @Test
    public void versionRegExpTest() throws IOException {
        BuiltInSearchTest test = new BuiltInSearchTest();
        test.setup();
        for (int i = 0; i < 1000; i++) {
            assertEquals("some-package-0.1.0beta1".length(), test.versionRegExp());
        }
        test.tearDown();
    }

    private static final Pattern LONG_REGEXP_PATTERN = Pattern.compile(
            "^a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaaaaaaaaaa"
    );
    private static final Pattern VERSION_REGEXP_PATTERN = Pattern.compile(
            "([^.].*?)-([0-9]+(?:[.][0-9]+|(?:pre|beta|alpha)[0-9]+)*)"
    );

    @Benchmark
    public long regExpJava() {
        Matcher match = LONG_REGEXP_PATTERN.matcher("aaaaaaaaaaaaaaaaaaaaaaaa");
        return match.matches() ? match.end() : 0;
    }
    @Benchmark
    public long versionRegExpJava() {
        Matcher match = LONG_REGEXP_PATTERN.matcher("some-package-0.1.0beta1");
        return match.matches() ? match.end() : 0;
    }
}
