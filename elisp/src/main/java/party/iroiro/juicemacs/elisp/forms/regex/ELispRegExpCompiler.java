package party.iroiro.juicemacs.elisp.forms.regex;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;

import com.oracle.truffle.api.CompilerDirectives;

import party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpLexer.REToken;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpOpcode.*;

final class ELispRegExpCompiler {

    public static Compiled compile(ELispRegExpParser.REAst ast, int maxGroup) {
        ELispRegExpCompiler compiler = new ELispRegExpCompiler();
        HalfCompiled searcher = compiler.compileAst(new ELispRegExpParser.REAst.Quantified(
                new ELispRegExpParser.REAst.Atom(new REToken.AnyChar()),
                new REToken.Quantifier(0, Integer.MAX_VALUE, false)
        ));
        HalfCompiled body = HalfCompiled.of(
                searcher,
                compiler.compileAst(ast),
                packNoArgOpcode(OP_MATCH)
        );
        int[] code = compileWithJumpTable(body, searcher.length());

        int[] groups = new int[maxGroup + 1];
        Arrays.fill(groups, -1);
        for (Map.Entry<Integer, Integer> entry : compiler.groupSlotMap.entrySet()) {
            int group = entry.getKey();
            int slot = entry.getValue();
            groups[group] = slot;
        }
        return new Compiled(code, groups, compiler.frameTop);
    }

    private static int[] compileWithJumpTable(HalfCompiled body, int searcherLength) {
        IntArrayList codes = new IntArrayList(body.length());
        body.emit(codes);

        IntArrayList jumpTable = new IntArrayList();
        jumpTable.add(0); // placeholder for the jump-table opcode
        jumpTable.add(0); // entry point before the .*? search prefix
        jumpTable.add(searcherLength); // entry point after the .*? search prefix
        visitAssemblyInstructions(codes, new AssemblyVisitor() {
            @Override
            public void visit(int i, HalfCompiled.Dual dualIntInstruction) {
                int code = dualIntInstruction.code;
                if (code >= 0 || !isSplitInstruction(code)) {
                    return;
                }
                int splitRelTarget = dualIntInstruction.arg;
                int splitAbsTarget = i + 2 + splitRelTarget;
                int jumpTableIndex = jumpTable.size() - 1;
                jumpTable.add(splitAbsTarget);
                codes.set(i + 1, jumpTableIndex);
            }
        });
        int jumpTableEntries = jumpTable.size() - 1;
        jumpTable.set(0, packSingleArgOpcode(OP_JUMP_TABLE, jumpTableEntries));
        int jumpTableLength = jumpTable.size();
        for (int i = 0; i < jumpTableEntries; i++) {
            jumpTable.set(i + 1, jumpTable.get(i + 1) + jumpTableLength);
        }
        jumpTable.addAll(codes);
        return jumpTable.toArray();
    }

    public int frameTop;

    private final HashMap<Integer, Integer> groupSlotMap = new HashMap<>();

    private ELispRegExpCompiler() {
        frameTop = 2; // Slot 0/1 is reserved for SP/PC
    }

    private int allocateStackSlot() {
        return frameTop++;
    }

    private int allocateGroupPositionSlots(int groupIndex) {
        int slot = allocateStackSlot(); // start position slot
        allocateStackSlot(); // end position slot
        groupSlotMap.put(groupIndex, slot);
        return slot;
    }

    private HalfCompiled compileAst(ELispRegExpParser.REAst ast) {
        return switch (ast) {
            case ELispRegExpParser.REAst.Atom(REToken token) -> atom(token);
            case ELispRegExpParser.REAst.Group(int index, ELispRegExpParser.REAst[][] alternations) -> {
                HalfCompiled inner = compileAlternations(alternations);
                if (index == -1) {
                    yield inner;
                }
                int slot = allocateGroupPositionSlots(index);
                yield HalfCompiled.of(
                        packSingleArgOpcode(OP_PROGRESS_REC, slot),
                        inner,
                        packSingleArgOpcode(OP_PROGRESS_REC, slot + 1)
                );
            }
            case ELispRegExpParser.REAst.Literal(int[] chars) -> literal(chars);
            case ELispRegExpParser.REAst.Quantified(
                    ELispRegExpParser.REAst child,
                    REToken.Quantifier(int min, int max, boolean greedy)
            ) -> {
                HalfCompiled inner = compileAst(child);
                yield quantified(inner, min, max, greedy);
            }
        };
    }

    private HalfCompiled compileAlternations(ELispRegExpParser.REAst[][] alternations) {
        @Nullable HalfCompiled compiled = null;
        for (ELispRegExpParser.REAst[] alternation : alternations) {
            HalfCompiled concat = concat(Arrays.stream(alternation).map(this::compileAst).toArray(HalfCompiled[]::new));
            compiled = compiled == null ? concat : union(compiled, concat);
        }
        return compiled == null ? empty() : compiled;
    }

    private static int packNamedCharClassBitmap(ELispRegExpLexer.CharClassContent.NamedCharClass[] namedClasses, boolean invert) {
        int bits = 0;
        for (ELispRegExpLexer.CharClassContent.NamedCharClass namedClass : namedClasses) {
            bits |= namedClass.mask;
        }
        return bits | (invert ? (1 << 31) : 0);
    }

    private static HalfCompiled atom(REToken token) {
        if (token instanceof REToken.CharClass(
                ELispRegExpLexer.CharClassContent.NamedCharClass[] namedClasses,
                ELispRegExpLexer.CharClassContent.CharRange[] charRanges,
                boolean charRangesFitInInt,
                boolean invert
        )) {
            int extraCodes = charRanges.length * (charRangesFitInInt ? 1 : 2) + 1;
            IntArrayList opcodes = new IntArrayList(1 + extraCodes);
            opcodes.add(packSingleArgOpcode(
                    charRangesFitInInt ? OP$CHAR_CLASS : OP$CHAR_CLASS_32,
                    extraCodes
            ));
            opcodes.add(packNamedCharClassBitmap(namedClasses, invert));
            for (ELispRegExpLexer.CharClassContent.CharRange range : charRanges) {
                if (charRangesFitInInt) {
                    opcodes.add((range.min()) | (range.max() << 16));
                } else {
                    opcodes.add(range.min());
                    opcodes.add(range.max());
                }
            }
            return new HalfCompiled.Multiple(opcodes.toArray());
        }
        int opcode = switch (token) {
            case REToken.BackReference(int index) -> packSingleArgOpcode(OP$BACKREF, index);
            case REToken.WordBoundary(boolean invert) -> packSingleArgOpcode(OP$WORD_BOUND, invert ? -1 : 0);
            case REToken.CategoryChar(byte kind, boolean invert) ->
                    packSingleArgOpcode(OP$CATEGORY_CHAR, kind | (invert ? ARG_BIT_FLAG : 0));
            case REToken.SyntaxChar(byte kind, boolean invert) ->
                    packSingleArgOpcode(OP$SYNTAX_CHAR, kind | (invert ? ARG_BIT_FLAG : 0));
            case REToken.Quantifier _,
                 REToken.GroupStart _,
                 REToken.GroupEnd _,
                 REToken.Alternation _,
                 REToken.CharClass _,
                 REToken.Char _ -> throw CompilerDirectives.shouldNotReachHere();
            default -> packNoArgOpcode(switch (token) {
                case REToken.AnyChar() -> OP$ANY;
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
        return HalfCompiled.of(opcode);
    }

    private static HalfCompiled empty() {
        return HalfCompiled.of();
    }

    private static HalfCompiled literal(int[] chars) {
        for (int i = 0; i < chars.length; i++) {
            chars[i] = packSingleArgOpcode(OP$CHAR, chars[i]);
        }
        return new HalfCompiled.Multiple(chars);
    }

    private static HalfCompiled concat(HalfCompiled... segments) {
        return HalfCompiled.of((Object[]) segments);
    }

    private static HalfCompiled union(HalfCompiled a, HalfCompiled b) {
        HalfCompiled jump = HalfCompiled.of(uncondJmp(b.length()));
        return concat(
                packSplitDual(OP_FLAG_JMP_UNCOND, 0, a.length() + jump.length()),
                a,
                jump,
                b
        );
    }

    private static HalfCompiled cmpOpcodeDual(int counter, int constant) {
        return HalfCompiled.of(packSingleArgOpcode(OP_COUNTER_CMP, counter), constant);
    }

    private HalfCompiled quantified(HalfCompiled inner, int min, int max, boolean greedy) {
        int counterSlot = allocateStackSlot();
        int progressSlot = allocateStackSlot();
        int headLength = 5;
        int innerLength = inner.length();
        int tailLength = 6;
        return HalfCompiled.of(
                /* entry */
                // ~ int i = 0;
                packSingleArgOpcode(OP_COUNTER_RESET, counterSlot),
                /* loop_head_start */
                // ~ int progress = sp;
                packSingleArgOpcode(OP_PROGRESS_REC, progressSlot),
                cmpOpcodeDual(counterSlot, min),
                greedy
                        // ~ if (i >= min) { first try body, then try loop_tail_end } (greedy)
                        ? packSplitDual(OP_FLAG_JMP_GE, 0, innerLength + tailLength)
                        // ~ if (i >= min) { first try loop_tail_end, then try body } (non-greedy)
                        : packSplitDual(OP_FLAG_JMP_GE, innerLength + tailLength, 0),
                /* loop_head_end */
                /* body */
                inner,
                /* loop_tail_start */
                packSingleArgOpcode(OP_PROGRESS_CMP, progressSlot),
                // ~ if (sp == progress) { break to loop_tail_end; }
                condJumpSingle(OP_FLAG_JMP_EQ, tailLength - 2),
                // ~ i++;
                packSingleArgOpcode(OP_COUNTER_INC, counterSlot),
                cmpOpcodeDual(counterSlot, max),
                // ~ if (i < max) { continue to loop_head_start; }
                condJumpSingle(OP_FLAG_JMP_LT, -(headLength + innerLength + tailLength))
                /* loop_tail_end */
        );
    }

    private static HalfCompiled packSplitDual(int cond, int jumpRel, int splitRel) {
        int opcode = (cond << OP_FLAG_JMP_COND_SHIFT)
                | (OP_FLAG_JMP_SPLIT << OP_FLAG_JMP_KIND_SHIFT)
                | OP_FLAG_JMP;
        return HalfCompiled.of(packSingleArgOpcode(opcode, jumpRel), splitRel);
    }

    static String disassemble(IntArrayList codes) {
        StringBuilder sb = new StringBuilder();
        visitAssemblyInstructions(codes, new AssemblyVisitor() {
            private int indentation = 0;

            private void printFirst(int bci, int code) {
                String name = opcodeToString(code);
                int arg = (code << 8) >> 8;
                int start = sb.length();
                sb.append(bci).append(' ');
                indentation = sb.length() - start;
                sb.append(name).append(' ').append(arg).append('\n');
            }
            private void printFirstChar(int bci, int code) {
                String name = opcodeToString(code);
                int arg = code & IN_PLACE_ARG_MASK;
                int start = sb.length();
                sb.append(bci).append(' ');
                indentation = sb.length() - start;
                sb.append(name).append(" '");
                if (Character.isValidCodePoint(arg)) {
                    sb.appendCodePoint(arg);
                } else {
                    sb.append("\\u").append(String.format("%08x", arg));
                }
                sb.append("'\n");
            }
            private StringBuilder indent() {
                return sb.append(" ".repeat(indentation));
            }

            private String opcodeToString(int instruction) {
                if (instruction < 0) {
                    if (isSplitInstruction(instruction)) {
                        return "split";
                    } else {
                        return "jmp";
                    }
                }
                int opcode = instruction >> 24;
                return switch (opcode) {
                    case OP_MATCH -> "match";
                    case OP_COUNTER_RESET -> "counter_reset";
                    case OP_COUNTER_INC -> "counter_inc";
                    case OP_COUNTER_CMP -> "counter_cmp";
                    case OP_PROGRESS_REC -> "progress_rec";
                    case OP_PROGRESS_CMP -> "progress_cmp";
                    case OP_JUMP_TABLE -> "jump_table";
                    case OP$STR_START -> "str_start!";
                    case OP$STR_END -> "str_end!";
                    case OP$LINE_START -> "line_start!";
                    case OP$LINE_END -> "line_end!";
                    case OP$BUFFER_POINT -> "buffer_point!";
                    case OP$WORD_START -> "word_start!";
                    case OP$WORD_END -> "word_end!";
                    case OP$WORD_BOUND -> "word_bound!";
                    case OP$SYMBOL_START -> "symbol_start!";
                    case OP$SYMBOL_END -> "symbol_end!";
                    case OP$SYNTAX_CHAR -> "syntax_char!";
                    case OP$CATEGORY_CHAR -> "category_char!";
                    case OP$BACKREF -> "backref!";
                    case OP$CHAR_CLASS -> "char_ranges!";
                    case OP$CHAR_CLASS_32 -> "char_classes_32!";
                    case OP$CHAR -> "char!";
                    case OP$ANY -> "any!";
                    default -> throw new IllegalArgumentException("Unknown opcode: " + opcode);
                };
            }

            @Override
            public void visit(int i, HalfCompiled.Single singleIntInstruction) {
                int opcode = singleIntInstruction.code >> 24;
                if (opcode == OP$CHAR) {
                    printFirstChar(i, singleIntInstruction.code);
                } else {
                    printFirst(i, singleIntInstruction.code);
                }
            }

            @Override
            public void visit(int i, HalfCompiled.Dual dualIntInstruction) {
                printFirst(i, dualIntInstruction.code);
                indent().append("  ").append(dualIntInstruction.arg).append('\n');
            }

            @Override
            public void visit(int i, HalfCompiled.Multiple multipleIntInstruction) {
                int[] args = multipleIntInstruction.args;
                int code = args[0];
                int opcode = code >> 24;
                printFirst(i, code);
                if (opcode == OP$CHAR_CLASS_32) {
                    displayCharClassBitFlags(args[1]);
                    for (int j = 2; j < args.length; j += 2) {
                        indent().append("  ").appendCodePoint(args[j]).append("-").appendCodePoint(args[j + 1]).append('\n');
                    }
                }
                for (int j = 1; j < args.length; j++) {
                    int arg = args[j];
                    if (opcode == OP$CHAR_CLASS) {
                        if (j == 1) {
                            displayCharClassBitFlags(arg);
                        } else {
                            int c1 = (arg & 0xFFFF);
                            int c2 = (arg >> 16) & 0xFFFF;
                            indent().append("  ").appendCodePoint(c1).append("-").appendCodePoint(c2).append('\n');
                        }
                    } else {
                        indent().append("  ").append(j - 1).append(" -> ").append(arg).append('\n');
                    }
                }
            }

            private void displayCharClassBitFlags(int arg) {
                StringBuilder sb = indent();
                if (arg < 0) {
                    sb.append("^ ");
                } else {
                    sb.append("  ");
                }
                for (ELispRegExpLexer.CharClassContent.NamedCharClass value
                        : ELispRegExpLexer.CharClassContent.NamedCharClass.values()) {
                    if (value.match(arg)) {
                        sb.append("[:").append(value.name()).append(":]");
                    }
                }
                sb.append('\n');
            }
        });
        return sb.toString();
    }

    private static void visitAssemblyInstructions(IntArrayList codes, AssemblyVisitor visitor) {
        for (int i = 0; i < codes.size(); i++) {
            final int start = i;
            int code = codes.get(i);
            if (code < 0) {
                // Jump instruction
                boolean split = isSplitInstruction(code);
                if (split) {
                    // Two-int instructions
                    i++;
                    int splitRel = codes.get(i);
                    visitor.visit(start, new HalfCompiled.Dual(code, splitRel));
                } else {
                    // Single-int instructions
                    visitor.visit(start, new HalfCompiled.Single(code));
                }
            } else {
                int opcode = code >> 24;
                int arg = (code << 8) >> 8;
                // Var-length instructions
                if (opcode == OP$CHAR_CLASS || opcode == OP$CHAR_CLASS_32 || opcode == OP_JUMP_TABLE) {
                    int[] opcodes = new int[arg + 1];
                    opcodes[0] = code;
                    for (int j = 0; j < arg; j++) {
                        i++;
                        opcodes[j + 1] = codes.get(i);
                    }
                    visitor.visit(start, new HalfCompiled.Multiple(opcodes));
                    continue;
                }
                // Two-int instructions
                if (opcode == OP_COUNTER_CMP) {
                    i++;
                    int constant = codes.get(i);
                    visitor.visit(start, new HalfCompiled.Dual(code, constant));
                    continue;
                }
                // Single-int instructions
                visitor.visit(start, new HalfCompiled.Single(code));
            }
        }
    }

    private static boolean isSplitInstruction(int code) {
        return code < 0 && ((code >> (24 + OP_FLAG_JMP_KIND_SHIFT)) & OP_FLAG_JMP_KIND_MASK) == OP_FLAG_JMP_SPLIT;
    }

    private sealed interface HalfCompiled {
        int length();

        void emit(IntArrayList codes);

        static HalfCompiled of(int code) {
            return new HalfCompiled.Single(code);
        }

        static HalfCompiled of(int code, int arg) {
            return new HalfCompiled.Dual(code, arg);
        }

        static HalfCompiled of(Object... segments) {
            ArrayList<HalfCompiled> compiled = new ArrayList<>();
            for (Object segment : segments) {
                switch (segment) {
                    case HalfCompiled sub -> compiled.add(sub);
                    case Integer code -> compiled.add(new HalfCompiled.Single(code));
                    default -> throw new IllegalArgumentException(segment.getClass().toString());
                }
            }
            return new HalfCompiled.Segments(
                    compiled.toArray(HalfCompiled[]::new),
                    compiled.stream().mapToInt(HalfCompiled::length).sum()
            );
        }

        record Segments(HalfCompiled[] segments, int length) implements HalfCompiled {
            @Override
            public void emit(IntArrayList codes) {
                for (HalfCompiled segment : segments) {
                    segment.emit(codes);
                }
            }
        }

        record Single(int code) implements HalfCompiled {
            @Override
            public int length() {
                return 1;
            }

            @Override
            public void emit(IntArrayList codes) {
                codes.add(code);
            }
        }

        record Dual(int code, int arg) implements HalfCompiled {
            @Override
            public int length() {
                return 2;
            }

            @Override
            public void emit(IntArrayList codes) {
                codes.add(code);
                codes.add(arg);
            }
        }

        record Multiple(int[] args) implements HalfCompiled {
            @Override
            public int length() {
                return args.length;
            }

            @Override
            public void emit(IntArrayList codes) {
                codes.addAll(args);
            }
        }
    }

    record Compiled(@CompilerDirectives.CompilationFinal(dimensions = 1) int[] opcodes,
                    @CompilerDirectives.CompilationFinal(dimensions = 1) int[] groupSlotMap,
                    int stackSize) {
    }

    private interface AssemblyVisitor {
        default void visit(int i, HalfCompiled.Single singleIntInstruction) {}
        default void visit(int i, HalfCompiled.Dual dualIntInstruction) {}
        default void visit(int i, HalfCompiled.Multiple multipleIntInstruction) {}
    }
}