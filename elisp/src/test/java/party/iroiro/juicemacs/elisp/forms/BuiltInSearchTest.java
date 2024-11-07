package party.iroiro.juicemacs.elisp.forms;

import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

class BuiltInSearchTest extends BaseFormTest {
    private static final Object[] TESTS;

    private final static String[] SYNTAX_FREE_MATCHES = {
            "^abc$", "abc",
            "^\\(\\(abc\\)\\|\\(d\\)\\)\\(?:abc\\)\\{1\\}$", "dabc",
//            "^[[:ascii:]][[:alnum:]][[:alpha:]][[:blank:]][[:cntrl:]][[:digit:]][[:graph:]][[:lower:]]" +
//                    "[[:multibyte:]][[:nonascii:]][[:print:]][[:punct:]][[:space:]]" +
//                    "[[:unibyte:]][[:upper:]][[:word:]][[:xdigit:]]$",
//            "a1a \n1aa🧃🧃a, aAwB", // TODO: Syntax table
            "^[_[:alpha:]]\\{4\\}$", "_abc",
            "^[][]\\{4\\}$", "]][[",
            "^[\\]$", "\\",
            "^[[:a-z]+$", ":abcdefg",
            "^[[:a-z:-]+$", ":abcdefg-",
            "^()|{}$", "()|{}",
            "^\\(abc\\)\\1$", "abcabc",
            "^\\n$", "n",
            "\\`abc\\'", "abc",
//            "\\babc\\b", "abc", // TODO: Syntax table
//            "\\Babc\\B", "DabcD", // TODO: Syntax table
//            "\\<abc\\>", "abc", // TODO: Syntax table
//            "^\\w\\{3\\}\\W\\{3\\}$", "abc   ", // TODO: Syntax table
//            "^(\\_<make-char-table\\_>)$", "(make-char-table)", // TODO: Syntax table
    };

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
        try (Context context = Context.newBuilder("elisp")
                .build()
        ) {
            String source = "(string-match \"a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaaaaaaaaaa\"" +
                    " \"aaaaaaaaaaaaaaaaaaaaaaaa\")";
            context.eval("elisp", source);
        }
    }
}
