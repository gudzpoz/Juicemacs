package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.strings.TruffleString;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ELispRegExpTest {
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
            "\\Babc\\B", "DabcD",
            "\\<abc\\>", "abc",
            "^\\w\\{3\\}\\W\\{3\\}$", "abc   ",
            "^(\\_<make-char-table\\_>)$", "(make-char-table)",
    };

    @Test
    public void testNoSyntax() {
        for (int i = 0; i < SYNTAX_FREE_MATCHES.length; i += 2) {
            String regex = SYNTAX_FREE_MATCHES[i];
            String input = SYNTAX_FREE_MATCHES[i + 1];
            ELispRegExp exp = ELispRegExp.compile(
                    TruffleString.fromJavaStringUncached(regex, TruffleString.Encoding.UTF_16),
                    false
            );
            assertTrue(exp.matcher(input, null).matcher().find(), regex);
        }
    }

    @Test
    public void testGroups() {
        ELispRegExp exp = ELispRegExp.compile(
                TruffleString.fromJavaStringUncached("^\\(a+\\)\\=\\(?:c\\)\\(b+\\)$", TruffleString.Encoding.UTF_16),
                false
        );
        ELispRegExp.MatcherResult matcher = exp.matcher("aaacbbb", null);
        assertTrue(matcher.matcher().find());
        assertEquals(2, matcher.groupCount());
        assertEquals(3, matcher.matcher().groupCount());
        assertEquals(2, matcher.pointGroup());
        assertEquals(3, matcher.getPointPosition());
        assertEquals("aaa", matcher.matcher().group(matcher.normalizeGroup(1)));
        assertEquals("bbb", matcher.matcher().group(matcher.normalizeGroup(2)));
    }
}
