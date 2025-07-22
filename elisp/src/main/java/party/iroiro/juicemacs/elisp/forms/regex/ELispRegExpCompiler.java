package party.iroiro.juicemacs.elisp.forms.regex;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import org.apache.commons.lang3.Strings;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;

import com.oracle.truffle.api.CompilerDirectives;

import party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpLexer.REToken;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;

import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpOpcode.*;

@SuppressWarnings({"PMD.NoBoxedPrimitivesRule", "PMD.ShortMethodName"})
final class ELispRegExpCompiler {
    public static final int SP_SLOT = 0;
    public static final int STACK_TOP_SLOT = 1;
    public static final int STACK_ARRAY_SLOT = 2;
    public static final int ALLOC_SLOT_START = 3;

    public static Compiled compile(
            ELispRegExpParser.REAst ast, int maxGroup,
            @Nullable ELispCharTable canon
    ) {
        ELispRegExpCompiler compiler = new ELispRegExpCompiler(canon);
        int[] body = compiler.compile(ast);
        int[] groups = new int[maxGroup + 1];
        Arrays.fill(groups, -1);
        for (Map.Entry<Integer, Integer> entry : compiler.groupSlotMap.entrySet()) {
            int group = entry.getKey();
            int slot = entry.getValue();
            groups[group] = slot;
        }
        return new Compiled(body, groups, compiler.frameBuilder.build());
    }

    @Nullable
    private final ELispCharTable canon;

    private final IntArrayList code = new IntArrayList();
    private final HashMap<Integer, Integer> groupSlotMap = new HashMap<>();
    private final FrameDescriptor.Builder frameBuilder = FrameDescriptor.newBuilder();

    private ELispRegExpCompiler(@Nullable ELispCharTable canon) {
        this.canon = canon;
        frameBuilder.addSlots(1, FrameSlotKind.Long);
        frameBuilder.addSlots(1, FrameSlotKind.Int);
        frameBuilder.addSlots(1, FrameSlotKind.Object);
    }

    private int allocateGroupPositionSlots(int groupIndex) {
        // start position slot and end position slot
        int slot = frameBuilder.addSlots(2, FrameSlotKind.Long);
        groupSlotMap.put(groupIndex, slot);
        return slot;
    }

    private int[] compile(ELispRegExpParser.REAst ast) {
        code.add(packNoArgOpcode(OP_PROLOG_SEARCH));
        code.add(packNoArgOpcode(OP_PROLOG$SEARCH));
        compile(ast, -1);
        code.add(packNoArgOpcode(OP_MATCH));
        return code.toArray();
    }

    @CompilerDirectives.TruffleBoundary
    private int compile(ELispRegExpParser.REAst ast, int backtrackJump) {
        return switch (ast) {
            case ELispRegExpParser.REAst.Atom(REToken token) -> atom(token, backtrackJump);
            case ELispRegExpParser.REAst.Group(int index, ELispRegExpParser.REAst[][] alternations) -> {
                if (index == -1) {
                    yield compileAlternations(alternations, 0, backtrackJump);
                }
                int slot = allocateGroupPositionSlots(index);
                code.add(packSingleArgOpcode(OP_CAPTURE_WRITE, slot));
                code.add(packSingleArgOpcode(OP_CAPTURE$WRITE, backtrackJump - 1));
                backtrackJump = compileAlternations(alternations, 0, -1);
                code.add(packSingleArgOpcode(OP_CAPTURE_WRITE, slot + 1));
                code.add(packSingleArgOpcode(OP_CAPTURE$WRITE, backtrackJump - 1));
                yield -1;
            }
            case ELispRegExpParser.REAst.Literal(int[] chars) -> {
                if (canon != null) {
                    for (int i = 0; i < chars.length; i++) {
                        chars[i] = ELispRegExpNode.translate(chars[i], canon);
                    }
                }
                code.ensureCapacity(chars.length * 2);
                for (int c : chars) {
                    code.add(packSingleArgOpcode(OP$CHAR, c));
                    code.add(backtrackJump);
                    backtrackJump -= 2;
                }
                yield backtrackJump;
            }
            case ELispRegExpParser.REAst.Quantified(
                    ELispRegExpParser.REAst child,
                    REToken.Quantifier quantifier
            ) -> {
                if (quantifier.min() == 0) {
                    ELispRegExpParser.REAst[][] unrolled = new ELispRegExpParser.REAst[2][];
                    ELispRegExpParser.REAst[] empty = new ELispRegExpParser.REAst[0];
                    ELispRegExpParser.REAst[] oneOrMore = new ELispRegExpParser.REAst[1];
                    if (quantifier.max() == 1) {
                        oneOrMore[0] = child;
                    } else {
                        oneOrMore[0] = new ELispRegExpParser.REAst.Quantified(child, new REToken.Quantifier(
                                1, quantifier.max(), quantifier.greedy()
                        ));
                    }
                    if (quantifier.greedy()) {
                        unrolled[0] = oneOrMore;
                        unrolled[1] = empty;
                    } else {
                        unrolled[0] = empty;
                        unrolled[1] = oneOrMore;
                    }
                    yield compile(new ELispRegExpParser.REAst.Group(-1, unrolled), backtrackJump);
                }
                yield quantified(child, quantifier, backtrackJump);
            }
        };
    }

    private int quantified(
            ELispRegExpParser.REAst child, REToken.Quantifier quantifier,
            int backtrackJump
    ) {
        int slot = frameBuilder.addSlots(2, FrameSlotKind.Long);
        code.add(packSingleArgOpcode(OP_QUANT_PRE, slot));
        boolean greedy = quantifier.greedy();
        code.add(packSingleArgOpcode(greedy ? OP_QUANT$PRE : OP_QUANT$PRE_LAZY, backtrackJump - 1));
        int innerBacktrackPointerIndex = code.size();
        code.add(0);
        code.add(quantifier.min());
        backtrackJump = compile(child, -3);
        code.set(innerBacktrackPointerIndex, greedy ? code.size() + 4 : backtrackJump + code.size());
        code.add(packSingleArgOpcode(greedy ? OP_QUANT_POST : OP_QUANT_POST_LAZY, slot));
        code.add(greedy ? innerBacktrackPointerIndex + 2 : quantifier.min());
        code.add(quantifier.max());
        code.add(packSingleArgOpcode(
                greedy ? OP_QUANT$POST : OP_QUANT$POST_LAZY,
                greedy
                        ? backtrackJump - 3
                        : innerBacktrackPointerIndex - 1 - code.size()));
        return -1;
    }

    private int compileAlternations(ELispRegExpParser.REAst[][] alternations, int i, int backtrackJump) {
        if (alternations.length - i == 0) {
            return backtrackJump;
        }
        if (alternations.length - i == 1) {
            return compileAlterationBranch(alternations[i], backtrackJump);
        }
        if (alternations.length - i == 2) {
            // Optimization #1: .\\|\n -> any ("." is actually [^\n])
            if (alternations[0].length == 1 && alternations[1].length == 1) {
                if (
                        alternations[0][0] instanceof ELispRegExpParser.REAst.Atom(REToken.AnyChar())
                        && alternations[1][0] instanceof ELispRegExpParser.REAst.Literal(int[] chars)
                        && chars.length == 1 && chars[0] == '\n'
                ) {
                    code.add(packNoArgOpcode(OP$ANY));
                    code.add(backtrackJump);
                    return backtrackJump - 2;
                }
            }
        }
        int slot = frameBuilder.addSlots(1, FrameSlotKind.Long);
        // start+0:
        code.add(packSingleArgOpcode(OP_UNION_PRE, slot));
        // start+1: to-be-filled #1
        int backtrackAbsPc = code.size();
        code.add(packSingleArgOpcode(OP_UNION$PRE, backtrackJump - 1));
        code.add(0);
        // start+3~N: branch 1
        int firstBranchBacktrackPc = compileAlterationBranch(alternations[i], -2);
        firstBranchBacktrackPc += code.size();
        // start+N: jump to-be-filled #2
        int firstJumpIndex = code.size();
        code.add(0);
        // to-be-filled #1: filling (second branch entry)
        code.set(backtrackAbsPc + 1, code.size());
        int secondBranchBacktrackPc = compileAlternations(alternations, i + 1, backtrackAbsPc - code.size());
        secondBranchBacktrackPc += code.size();
        // to-be-filled #2: filling
        code.set(firstJumpIndex, packSingleArgOpcode(OP_JUMP, code.size() + 4 - firstJumpIndex));
        code.add(packSingleArgOpcode(OP_JUMP, 4));
        code.add(packSingleArgOpcode(OP_UNION$POST, slot));
        code.add(firstBranchBacktrackPc);
        code.add(secondBranchBacktrackPc);
        return -3;
    }

    private int compileAlterationBranch(ELispRegExpParser.REAst[] alternation, int backtrackJump) {
        for (ELispRegExpParser.REAst item : alternation) {
            backtrackJump = compile(item, backtrackJump);
        }
        return backtrackJump;
    }

    private static int packNamedCharClassBitmap(ELispRegExpLexer.CharClassContent.NamedCharClass[] namedClasses, boolean invert) {
        int bits = 0;
        for (ELispRegExpLexer.CharClassContent.NamedCharClass namedClass : namedClasses) {
            bits |= namedClass.mask;
        }
        return bits | (invert ? (1 << 31) : 0);
    }

    private int atom(REToken token, int backtrackJump) {
        int start = code.size();
        if (token instanceof REToken.CharClass(
                ELispRegExpLexer.CharClassContent.NamedCharClass[] namedClasses,
                ELispRegExpLexer.CharClassContent.CharRange[] charRanges,
                boolean charRangesFitInInt,
                boolean invert
        )) {
            processCharClass(namedClasses, charRanges, charRangesFitInInt, invert, backtrackJump);
        } else {
            int opcode = switch (token) {
                case REToken.BackReference(int index) -> packSingleArgOpcode(OP$BACKREF, index);
                case REToken.WordBoundary(boolean invert) -> packSingleArgOpcode(OP$WORD_BOUND, invert ? -1 : 0);
                case REToken.CategoryChar(byte kind, boolean invert) ->
                        packSingleInvertibleArgOpcode(OP$CATEGORY_CHAR, kind, invert);
                case REToken.SyntaxChar(byte kind, boolean invert) ->
                        packSingleInvertibleArgOpcode(OP$SYNTAX_CHAR, kind, invert);
                case REToken.Quantifier _,
                     REToken.GroupStart _,
                     REToken.GroupEnd _,
                     REToken.Alternation _,
                     REToken.CharClass _,
                     REToken.Char _ -> throw CompilerDirectives.shouldNotReachHere();
                default -> packNoArgOpcode(switch (token) {
                    case REToken.AnyChar() -> OP$ANY_BUT;
                    case REToken.BufferPoint() -> OP$BUFFER_POINT;
                    case REToken.StartOfLine() -> OP$LINE_START;
                    case REToken.EndOfLine() -> OP$LINE_END;
                    case REToken.StartOfString() -> OP$STR_START;
                    case REToken.EndOfString() -> OP$STR_END;
                    case REToken.StartOfSymbol() -> OP$SYMBOL_START;
                    case REToken.EndOfSymbol() -> OP$SYMBOL_END;
                    case REToken.StartOfWord() -> OP$WORD_START;
                    case REToken.EndOfWord() -> OP$WORD_END;
                    default -> throw CompilerDirectives.shouldNotReachHere();
                });
            };
            code.ensureCapacity(2);
            code.add(opcode);
            code.add(backtrackJump);
        }
        return backtrackJump + (start - code.size());
    }

    private void processCharClass(
            ELispRegExpLexer.CharClassContent.NamedCharClass[] namedClasses,
            ELispRegExpLexer.CharClassContent.CharRange[] charRanges,
            boolean charRangesFitInInt,
            boolean invert,
            int backtrackJump
    ) {
        if (canon != null) {
            ArrayList<ELispRegExpLexer.CharClassContent.CharRange> newRanges = new ArrayList<>();
            charRangesFitInInt = translateRanges(charRanges, newRanges);
            charRanges = newRanges.toArray(ELispRegExpLexer.CharClassContent.CharRange[]::new);
        }
        int extraCodes = charRanges.length * (charRangesFitInInt ? 1 : 2) + 1;
        code.ensureCapacity(2 + extraCodes);
        code.add(packSingleArgOpcode(
                charRangesFitInInt ? OP$CHAR_CLASS : OP$CHAR_CLASS_32,
                extraCodes
        ));
        code.add(backtrackJump);
        code.add(packNamedCharClassBitmap(namedClasses, invert));
        for (ELispRegExpLexer.CharClassContent.CharRange range : charRanges) {
            if (charRangesFitInInt) {
                code.add((range.min()) | (range.max() << 16));
            } else {
                code.add(range.min());
                code.add(range.max());
            }
        }
    }

    private boolean translateRanges(
            ELispRegExpLexer.CharClassContent.CharRange[] charRanges,
            ArrayList<ELispRegExpLexer.CharClassContent.CharRange> newRanges
    ) {
        assert canon != null;
        boolean charRangesFitInInt = true;
        int start = -1;
        int end = -1;
        for (ELispRegExpLexer.CharClassContent.CharRange range : charRanges) {
            int min = range.min();
            int max = range.max();
            for (int i = min; i <= max; i++) {
                int translated = ELispRegExpNode.translate(i, canon);
                if (start == -1) {
                    start = end = translated;
                    continue;
                }
                if (translated < start - 1 || end + 1 < translated) {
                    charRangesFitInInt &= end <= 0xFFFF;
                    newRanges.add(new ELispRegExpLexer.CharClassContent.CharRange(start, end));
                    start = end = translated;
                }
                if (translated == start - 1) {
                    start = translated;
                } else if (translated == end + 1) {
                    end = translated;
                }
            }
        }
        charRangesFitInInt &= end <= 0xFFFF;
        newRanges.add(new ELispRegExpLexer.CharClassContent.CharRange(start, end));
        return charRangesFitInInt;
    }

    public static String disassemble(int[] code) {
        StringBuilder asm = new StringBuilder();
        int pcWidth = (int) Math.ceil(Math.log10(code.length));
        int bci = 0, depth = 0;
        while (bci < code.length) {
            String bciString = Integer.toString(bci);
            String prefix = "| ".repeat(depth);
            asm
                    .append(prefix)
                    .append("0".repeat(pcWidth - bciString.length()))
                    .append(bciString).append(": ");

            final int start = bci;
            int instruction = code[bci++];
            int op = (instruction >> 24) & 0xFF;
            int arg = (instruction << 8) >> 8;
            int backtrackRel = op >= OP$STR_START ? bci : -1;
            String s = switch (op) {
                case OP_MATCH -> "match!";
                case OP_PROLOG_SEARCH -> "prolog";
                case OP_PROLOG$SEARCH -> "prolog$";
                case OP_JUMP -> "jump " + (start + arg);
                case OP_UNION_PRE -> "union_pre#" + arg;
                case OP_UNION$PRE -> {
                    bci++;
                    yield printArgs("union_pre$ " + (start + arg), code, start, 1, "else->");
                }
                case OP_UNION$POST -> {
                    bci += 2;
                    yield printArgs("union_post$ " + (start + arg), code, start, 2, "1:$", "2:$");
                }

                case OP_CAPTURE_WRITE -> "capture#" + arg;
                case OP_CAPTURE$WRITE -> "capture$ " + (start + arg);

                case OP_QUANT_PRE -> "quant_pre#" + arg;
                case OP_QUANT$PRE -> {
                    bci += 2;
                    yield printArgs("greedy_pre$ " + (start + arg), code, start, 2, "$", "min");
                }
                case OP_QUANT_POST -> {
                    bci += 2;
                    yield printArgs("greedy_post#" + arg, code, start, 2, "->", "max");
                }
                case OP_QUANT$POST -> "greedy_post$ " + (start + arg);

                case OP_QUANT$PRE_LAZY -> {
                    bci += 2;
                    yield printArgs("lazy_pre$ " + (start + arg), code, start, 2, "$", "min");
                }
                case OP_QUANT_POST_LAZY -> {
                    bci += 2;
                    yield printArgs("lazy_post#" + arg, code, start, 2, "-> ", "max");
                }
                case OP_QUANT$POST_LAZY -> "lazy_post$ " + (start + arg);

                case OP$STR_START -> "str_start!";
                case OP$STR_END -> "str_end!";
                case OP$LINE_START -> "line_start!";
                case OP$LINE_END -> "line_end!";
                case OP$BUFFER_POINT -> "buffer_point!";
                case OP$WORD_START, OP$WORD_END, OP$WORD_BOUND -> "word_bound!";
                case OP$SYMBOL_START, OP$SYMBOL_END -> "symbol_bound!";
                case OP$CATEGORY_CHAR, OP$SYNTAX_CHAR -> "syntax/category_char!";
                case OP$BACKREF -> "backref! " + arg;
                case OP$CHAR_CLASS, OP$CHAR_CLASS_32 -> {
                    bci += arg;
                    yield "char_class!";
                }
                case OP$CHAR -> new StringBuilder("char! ").appendCodePoint(arg).toString();
                case OP$ANY_BUT -> "any_but!";
                case OP$ANY -> "any!";
                default -> throw CompilerDirectives.shouldNotReachHere();
            };
            int lineEnd = s.indexOf('\n');
            if (lineEnd != -1) {
                s = s.replace("\n", "\n" + prefix);
            } else {
                lineEnd = s.length();
            }
            asm.append(s).append('\n');
            if (backtrackRel != -1) {
                asm.append(prefix).append("    $ ").append(start + code[backtrackRel]).append('\n');
                bci++;
            }
            CharSequence line = s.subSequence(0, lineEnd);
            if (Strings.CS.contains(line, "pre$")) {
                depth++;
            } else if (Strings.CS.contains(line, "post$")) {
                depth--;
            }
        }
        return asm.toString();
    }
    private static String printArgs(String name, int[] code, int startBci, int n, String... tags) {
        StringBuilder sb = new StringBuilder();
        sb.append(name);
        int width = Arrays.stream(tags).mapToInt(String::length).max().orElse(0);
        for (int i = 0; i < n; i++) {
            sb.append("\n    ")
                    .append(tags[i])
                    .append(" ".repeat(width - tags[i].length()))
                    .append(' ')
                    .append(code[startBci + 1 + i]);
        }
        return sb.toString();
    }

    record Compiled(
            @CompilerDirectives.CompilationFinal(dimensions = 1) int[] opcodes,
            @CompilerDirectives.CompilationFinal(dimensions = 1) int[] groupSlotMap,
            FrameDescriptor frame
    ) {
    }
}
