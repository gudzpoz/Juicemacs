package party.iroiro.juicemacs.elisp.forms.regex;

import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.graalvm.collections.Pair;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.mule.MuleString;

import static org.junit.jupiter.api.Assertions.*;

class ELispRegExpCompilerTest {
    private Pair<ELispRegExpCompiler.Compiled, ELispRegExp.CompiledRegExp> compile(String input) {
        ELispRegExpParser test = new ELispRegExpParser(MuleString.fromString(input), null);
        ELispRegExpParser.REAst ast = test.parse();
        ELispRegExpCompiler.Compiled compiled = ELispRegExpCompiler.compile(ast, test.getMaxGroup(), null);
        ELispRegExp.RegExpFunctionNode node =
                new ELispRegExp.RegExpFunctionNode(null, new ELispRegExpNode(compiled, false), input);
        ELispRegExp.CompiledRegExp regexp = new ELispRegExp.CompiledRegExp(node.getCallTarget());
        return Pair.create(compiled, regexp);
    }

    @Test
    public void testNoSearch() {
        assertEquals(
                false,
                compile("test").getRight()
                        .call(MuleString.fromString("1test"),
                                false, 0, -1, false));
        assertNotEquals(
                false,
                compile("test").getRight()
                        .call(MuleString.fromString("1test"),
                                true, 0, -1, false));
    }

    @Test
    public void testDisassemble() {
        testSuite(new String[][]{
                {"test", "1test", "(1 5)"},
                {"\\(hello\\|world\\)", "2world", "(1 6 1 6)", "2wordle", "false"},
                {".+1", "\n311", "(1 4)", "\n3456789", "false"},
                {".+?1", "\n311", "(1 3)", "\n3456789", "false"},
        }, true);
    }

    private void testSuite(String[][] testCases, boolean print) {
        for (String[] testCase : testCases) {
            String pattern = testCase[0];
            Pair<ELispRegExpCompiler.Compiled, ELispRegExp.CompiledRegExp> compiled = compile(pattern);
            if (print) {
                System.out.println(ELispRegExpCompiler.disassemble(IntArrayList.newListWith(compiled.getLeft().opcodes())));
            }
            for (int i = 1; i < testCase.length; i += 2) {
                String input = testCase[i];
                Object result = compiled.getRight().call(
                        MuleString.fromString(input), true, 0, -1, false
                );
                if (print) {
                    System.out.println(result);
                }
                assertEquals(
                        testCase[i + 1], result.toString(),
                        () -> pattern + ": " + input + "\n"
                                + ELispRegExpCompiler.disassemble(IntArrayList.newListWith(compiled.getLeft().opcodes())));
            }
        }
    }
    private void testSuite(String[][] testCases) {
        testSuite(testCases, false);
    }

    @Test
    public void testUnion() {
        testSuite(new String[][]{
                {"\\(?:a\\|b\\|c\\)d",
                        "ad", "(0 2)", "bd", "(0 2)", "cd", "(0 2)",
                        "a", "false", "b", "false", "c", "false", "d", "false",
                        "dd", "false", "ae", "false", "be", "false", "ce", "false"},
        });
    }

    @Test
    public void testQuantifier() {
        testSuite(new String[][]{
                {"\\(\\|a\\)*b",
                        "b", "(0 1 0 0)",
                        "ab", "(0 2 1 1)",
                        "aab", "(0 3 2 2)",
                        "a", "false",
                        "c", "false",
                },
                {"[0-9]\\{1,4\\}L",
                        "0L", "(0 2)",
                        "10L", "(0 3)",
                        "100L", "(0 4)",
                        "1000L", "(0 5)",
                        "10000L", "(1 6)",
                },
                {"[0-9]\\{4\\}",
                        "1234", "(0 4)",
                },
                {"[0-9]\\{3,4\\}3",
                        "01234", "(0 4)", "1234", "false",
                },
                {"\\(?:\\|[0-9]\\)+?L",
                        "123", "false",
                        "123L", "(0 4)",
                },
        });
    }

    @Test
    public void testVersionString() {
        testSuite(new String[][]{
                {"\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)",
                        "some-package-0.1.0beta1",
                        "(0 23 0 12 13 23)",
                }
        });
    }
}
