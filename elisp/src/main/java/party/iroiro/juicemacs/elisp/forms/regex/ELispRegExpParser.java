package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.regex.REToken.*;

final class ELispRegExpParser {
    @Nullable
    private final ELispString whitespaceRegExp;
    private final ArrayList<REToken> stack;

    private ELispRegExpLexer lexer;
    @Nullable
    private ELispRegExpLexer parentLexer;

    private int groupIndex;
    private final IntArrayList processingGroupIndices;
    private final IntArrayList availableGroupIndices;

    public ELispRegExpParser(ELispString regExp,
                             @Nullable ELispString whitespaceRegExp) {
        this.whitespaceRegExp = whitespaceRegExp;
        lexer = new ELispRegExpLexer(regExp);
        stack = new ArrayList<>();
        groupIndex = 0;
        processingGroupIndices = new IntArrayList();
        availableGroupIndices = new IntArrayList();
    }

    public int getMaxGroup() {
        return groupIndex;
    }

    @TruffleBoundary
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

    private void handle(REToken token) {
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
                    throw ELispSignals.invalidRegexp("Unmatched ) or \\)");
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
                    REToken next = lexer.peek();
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
                    lexer = new ELispRegExpLexer(whitespaceRegExp);
                }
            }
            default -> stack.add(token);
        }
    }

    private REAst processStackTop(boolean compact) {
        REToken top = stack.removeLast();
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
            case AnyChar(), ReallyAnyChar(),
                 StartOfString(), EndOfString(),
                 StartOfLine(), EndOfLine(),
                 StartOfWord(), EndOfWord(), WordBoundary(_),
                 StartOfSymbol(), EndOfSymbol(),
                 BufferPoint(),
                 BackReference _,
                 CharClass _,
                 SyntaxChar _,
                 CategoryChar _ -> new REAst.Atom(top);
            case Quantifier quantifier -> lookaheadQuantifier(quantifier);
            case GroupEnd() -> collectGroup();
            case GroupStart ignored -> throw ELispSignals.error("Unmatched ( or \\(");
            case Alternation() -> throw CompilerDirectives.shouldNotReachHere(); // Processed by collectGroup
        };
    }

    private REAst lookaheadQuantifier(Quantifier quantifier) {
        return new REAst.Quantified(processStackTop(false), quantifier);
    }

    private REAst collectGroup() {
        ArrayList<List<REAst>> alternations = new ArrayList<>();
        ArrayList<REAst> branch = new ArrayList<>();
        while (true) {
            if (stack.isEmpty()) {
                throw ELispSignals.invalidRegexp("Unmatched ) or \\)");
            }
            REToken peek = stack.getLast();
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

}
