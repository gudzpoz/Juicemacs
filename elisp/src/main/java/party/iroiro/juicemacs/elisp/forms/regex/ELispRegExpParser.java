package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpLexer.REToken.*;

final class ELispRegExpParser {
    @Nullable
    private final AbstractTruffleString whitespaceRegExp;
    private final TruffleString.Encoding encoding;
    private final ArrayList<ELispRegExpLexer.REToken> stack;

    private ELispRegExpLexer lexer;
    @Nullable
    private ELispRegExpLexer parentLexer;

    private int groupIndex;
    private final IntArrayList processingGroupIndices;
    private final IntArrayList availableGroupIndices;

    public ELispRegExpParser(AbstractTruffleString regExp,
                             @Nullable AbstractTruffleString whitespaceRegExp,
                             TruffleString.Encoding encoding) {
        this.whitespaceRegExp = whitespaceRegExp;
        this.encoding = encoding;
        lexer = new ELispRegExpLexer(regExp, encoding);
        stack = new ArrayList<>();
        groupIndex = 0;
        processingGroupIndices = new IntArrayList();
        availableGroupIndices = new IntArrayList();
    }

    public int getMaxGroup() {
        return groupIndex;
    }

    @CompilerDirectives.TruffleBoundary
    public REAst parse() {
        stack.add(new GroupStart(0));
        while (true) {
            if (!lexer.hasNext()) {
                if (parentLexer == null) {
                    break;
                }
                lexer = parentLexer;
                parentLexer = null;
                continue;
            }
            handle(lexer.next());
        }
        REAst root = collectGroup();
        if (!stack.isEmpty()) {
            throw ELispSignals.error("Invalid regular expression");
        }
        return root;
    }

    private void handle(ELispRegExpLexer.REToken token) {
        switch (token) {
            case GroupStart(int index) when index == 0 -> {
                groupIndex++;
                processingGroupIndices.add(groupIndex);
                stack.add(new GroupStart(groupIndex));
            }
            case GroupStart(int index) when index != -1 -> {
                groupIndex = Math.max(index, groupIndex);
                processingGroupIndices.add(index);
                stack.add(token);
            }
            case GroupStart start -> {
                processingGroupIndices.add(-1);
                stack.add(start);
            }
            case GroupEnd() -> {
                if (processingGroupIndices.isEmpty()) {
                    throw ELispSignals.error("Unmatched group end");
                }
                int index = processingGroupIndices.removeAtIndex(processingGroupIndices.size() - 1);
                if (index != -1) {
                    availableGroupIndices.add(index);
                }
                stack.add(token);
            }
            case BackReference reference -> {
                if (!availableGroupIndices.contains(reference.index())) {
                    throw ELispSignals.error("Invalid back reference");
                }
                stack.add(reference);
            }
            case Char(int c) when c == ' ' -> {
                if (whitespaceRegExp == null || parentLexer != null) {
                    stack.add(token);
                    return;
                }
                boolean asIs = false;
                int whitespaces = 1;
                while (lexer.hasNext()) {
                    ELispRegExpLexer.REToken next = lexer.peek();
                    if (next instanceof Char(int space) && space == ' ') {
                        whitespaces++;
                    } else {
                        if (next instanceof Quantifier) {
                            asIs = true;
                        }
                        break;
                    }
                }
                if (asIs) {
                    for (int i = 0; i < whitespaces; i++) {
                        stack.add(token);
                    }
                } else {
                    parentLexer = lexer;
                    lexer = new ELispRegExpLexer(whitespaceRegExp, encoding);
                }
            }
            default -> stack.add(token);
        }
    }

    private REAst processStackTop(boolean compact) {
        ELispRegExpLexer.REToken top = stack.removeLast();
        return switch (top) {
            case Char(int c) -> {
                if (!compact) {
                    yield new REAst.Literal(new int[]{c});
                }
                IntArrayList chars = new IntArrayList();
                chars.add(c);
                while (!stack.isEmpty()) {
                    if (stack.getLast() instanceof Char(int prev)) {
                        stack.removeLast();
                        chars.add(prev);
                    } else {
                        break;
                    }
                }
                yield new REAst.Literal(chars.asReversed().toArray());
            }
            case AnyChar(),
                 StartOfString(), EndOfString(),
                 StartOfLine(), EndOfLine(),
                 StartOfWord(), EndOfWord(), WordBoundary(_),
                 StartOfSymbol(), EndOfSymbol(),
                 BufferPoint(),
                 BackReference _,
                 CharClass _,
                 SyntaxChar _,
                 CategoryChar _ -> new REAst.Atom(top);
            case Quantifier quantifier -> new REAst.Quantified(processStackTop(compact), quantifier);
            case GroupEnd() -> collectGroup();
            case GroupStart ignored -> throw ELispSignals.error("Unbalanced group start");
            case Alternation() -> throw CompilerDirectives.shouldNotReachHere(); // Processed by collectGroup
        };
    }

    private REAst collectGroup() {
        ArrayList<List<REAst>> alternations = new ArrayList<>();
        ArrayList<REAst> branch = new ArrayList<>();
        while (true) {
            if (stack.isEmpty()) {
                throw ELispSignals.error("Unbalanced group end");
            }
            ELispRegExpLexer.REToken peek = stack.getLast();
            switch (peek) {
                case GroupStart(int index) -> {
                    stack.removeLast();
                    alternations.add(branch);
                    REAst[][] children = new REAst[alternations.size()][];
                    for (int i = 0; i < alternations.size(); i++) {
                        children[i] = alternations.get(i).reversed().toArray(REAst[]::new);
                    }
                    return new REAst.Group(index, children);
                }
                case Alternation() -> {
                    stack.removeLast();
                    alternations.add(branch);
                    branch = new ArrayList<>();
                }
                default -> branch.add(processStackTop(true));
            }
        }
    }

    sealed interface REAst {
        record Atom(ELispRegExpLexer.REToken token) implements REAst {}
        record Group(int index, REAst[][] alternations) implements REAst {
            @Override
            public String toString() {
                StringBuilder builder = new StringBuilder("Group{index=");
                builder.append(index).append(", alternations=[");
                for (REAst[] alternation : alternations) {
                    builder.append(Arrays.toString(alternation)).append(", ");
                }
                return builder.append("]}").toString();
            }
        }
        record Literal(int[] chars) implements REAst {
            @Override
            public String toString() {
                StringBuilder builder = new StringBuilder("Literal{chars=[");
                for (int c : chars) {
                    builder.appendCodePoint(c);
                }
                return builder.append("]}").toString();
            }
        }
        record Quantified(REAst child, Quantifier quantifier) implements REAst {}
    }
}
