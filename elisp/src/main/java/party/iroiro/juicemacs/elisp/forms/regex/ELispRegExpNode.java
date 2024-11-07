package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.HostCompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.BytecodeOSRNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBoolVector;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

import java.util.ArrayList;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.asBuffer;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.asCharTable;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpLexer.CharClassContent.NamedCharClass.*;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpOpcode.*;

class ELispRegExpNode extends Node implements BytecodeOSRNode {

    public static final int ARG_OBJ_INPUT   = 0;
    public static final int ARG_BOOL_SEARCH = 1;
    public static final int ARG_INT_START   = 2;
    public static final int ARG_INT_END     = 3;
    public static final int ARG_OBJ_BUFFER  = 4;

    private static final int SP_SLOT = 0;
    private static final int PC_SLOT = 1;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] code;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] groupSlotMap;
    private final int stackSize;

    @CompilerDirectives.CompilationFinal
    private Object osrMetadata;

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispRegExpInputNodes.InputLengthNode lengthNode;

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispRegExpInputNodes.InputGetCharNode getCharNode;

    protected ELispRegExpNode(ELispRegExpCompiler.Compiled compiled) {
        this.code = compiled.opcodes();
        this.stackSize = compiled.stackSize();
        this.groupSlotMap = compiled.groupSlotMap();
        lengthNode = ELispRegExpInputNodesFactory.InputLengthNodeGen.create();
        getCharNode = ELispRegExpInputNodesFactory.InputGetCharNodeGen.create();
    }

    public Object execute(VirtualFrame frame) {
        Object[] args = frame.getArguments();
        Object input = args[ARG_OBJ_INPUT];
        int start = (int) args[ARG_INT_START];
        int end = (int) args[ARG_INT_END];
        if (end == -1) {
            end = lengthNode.execute(frame, input);
        }
        boolean search = (boolean) args[ARG_BOOL_SEARCH];

        int[] initStack = initStack(start, search);
        ArrayList<int[]> stacks = new ArrayList<>();
        stacks.add(initStack);
        return dispatcher(frame, 0, input, start, end, stacks, frame.getArguments()[ARG_OBJ_BUFFER]);
    }

    private int[] initStack(int start, boolean search) {
        int[] initStack = new int[stackSize];
        initStack[SP_SLOT] = start;
        initStack[PC_SLOT] = search ? 0 : 1;
        for (int startSlot : groupSlotMap) {
            initStack[startSlot] = -1;
            int endSlot = startSlot + 1;
            initStack[endSlot] = -1;
        }
        return initStack;
    }

    @Override
    public Object getOSRMetadata() {
        return osrMetadata;
    }

    @Override
    public void setOSRMetadata(Object osrMetadata) {
        this.osrMetadata = osrMetadata;
    }

    @Override
    public Object executeOSR(VirtualFrame osrFrame, int target, Object interpreterState) {
        BacktrackingState state = (BacktrackingState) interpreterState;
        return dispatcher(osrFrame, target,
                state.input, state.start, state.length, state.stacks, state.buffer
        );
    }

    private Object dispatcher(VirtualFrame frame,
                              int bci,
                              Object input,
                              int start, int end,
                              ArrayList<int[]> stacks,
                              Object buffer) {
        // The outer backtracking loop.
        // This loop must be separated from the inner loop to allow MERGE_EXPLODE,
        // since we do not know how many backtracking states we will need.
        while (true) {
            Object lastRun = dispatchFromBCI(frame, bci, input, start, end, stacks, buffer);
            if (lastRun != Boolean.FALSE) {
                return lastRun;
            }
            if (stacks.isEmpty()) {
                return false;
            }
            // back-edge
            if (BytecodeOSRNode.pollOSRBackEdge(this)) {
                BacktrackingState state = new BacktrackingState(stacks, input, start, end, buffer);
                Object result = BytecodeOSRNode.tryOSR(this, bci, state, null, frame);
                if (result != null) {
                    return result;
                }
            }
        }
    }

    @HostCompilerDirectives.BytecodeInterpreterSwitch
    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    private Object dispatchFromBCI(VirtualFrame frame,
                                   int bci,
                                   Object input, int start, int end,
                                   ArrayList<int[]> stacks,
                                   Object buffer) {
        int[] stack = stacks.removeLast();
        int cmpFlags = 0;

        loop:
        while (true) {
            CompilerAsserts.partialEvaluationConstant(bci);
            int instruction = code[bci++];
            CompilerAsserts.partialEvaluationConstant(instruction);
            int opcode = instruction >> 24;
            int arg = (instruction << 8) >> 8;
            if (opcode < 0) {
                // Jump instructions
                int cond = (opcode >> OP_FLAG_JMP_COND_SHIFT) & OP_FLAG_JMP_COND_MASK;
                boolean success = switch (cond) {
                    case OP_FLAG_JMP_UNCOND -> true;
                    case OP_FLAG_JMP_NO_JUMP -> false;
                    case OP_FLAG_JMP_LE -> cmpFlags <= 0;
                    case OP_FLAG_JMP_GT -> cmpFlags > 0;
                    case OP_FLAG_JMP_LT -> cmpFlags < 0;
                    case OP_FLAG_JMP_GE -> cmpFlags >= 0;
                    case OP_FLAG_JMP_EQ -> cmpFlags == 0;
                    case OP_FLAG_JMP_NE -> cmpFlags != 0;
                    default -> throw CompilerDirectives.shouldNotReachHere();
                };
                // ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE merges
                // "copies of the loop body that have the exact same state (all local variables have the same value)".
                // So we need to reset cmpFlags to 0 after each jump.
                cmpFlags = 0;
                int kind = (opcode >> OP_FLAG_JMP_KIND_SHIFT) & OP_FLAG_JMP_KIND_MASK;
                if (!success) {
                    if (kind == OP_FLAG_JMP_SPLIT) {
                        bci++;
                    }
                    continue;
                }
                int nextBci;
                if (kind == OP_FLAG_JMP_JMP) {
                    nextBci = bci + arg;
                } else {
                    int[] newThread = stack.clone();
                    int jumpTableIndex = code[bci];
                    newThread[PC_SLOT] = jumpTableIndex;
                    stacks.add(newThread);
                    nextBci = bci + arg + 1;
                }
                if (nextBci < bci) { // back-edge
                    if (BytecodeOSRNode.pollOSRBackEdge(this)) {
                        stacks.add(stack);
                        BacktrackingState state = new BacktrackingState(stacks, input, start, end, buffer);
                        Object result = BytecodeOSRNode.tryOSR(this, nextBci, state, null, frame);
                        if (result != null) {
                            return result;
                        }
                    }
                }
                bci = nextBci;
                CompilerAsserts.partialEvaluationConstant(bci);
                continue;
            }
            final int sp = stack[SP_SLOT];
            boolean success = true;
            switch (opcode) {
                case OP_MATCH -> {
                    return packMatchResult(stack);
                }
                case OP_COUNTER_RESET -> stack[arg] = 0;
                case OP_COUNTER_INC -> ++stack[arg];
                case OP_COUNTER_CMP -> cmpFlags = Integer.compare(stack[arg], code[bci++]);
                case OP_PROGRESS_REC -> stack[arg] = sp;
                case OP_PROGRESS_CMP -> cmpFlags = Integer.compare(stack[arg], sp);
                case OP_JUMP_TABLE -> {
                    final int target = stack[PC_SLOT];
                    if (CompilerDirectives.inInterpreter()) {
                        bci = code[bci + target];
                        continue;
                    }
                    for (int i = 0; i < arg; ++i) {
                        if (i == target) {
                            bci = code[bci + i];
                            continue loop;
                        }
                    }
                    throw CompilerDirectives.shouldNotReachHere();
                }
                case OP$STR_START -> success = sp == start;
                case OP$STR_END -> success = sp == end;
                case OP$LINE_START -> success = sp == start
                        || (sp > start && getChar(frame, input, sp - 1) == '\n');
                case OP$LINE_END -> success = sp == end
                        || (sp < end && getChar(frame, input, sp) == '\n');
                case OP$BUFFER_POINT -> throw new UnsupportedOperationException();
                case OP$WORD_START, OP$WORD_END, OP$WORD_BOUND -> {
                    boolean hasWordBefore = sp != start && isWord(buffer, getChar(frame, input, sp - 1));
                    boolean hasWordAfter = sp != end && isWord(buffer, getChar(frame, input, sp));
                    success = switch (opcode) {
                        case OP$WORD_START -> !hasWordBefore && hasWordAfter;
                        case OP$WORD_END -> hasWordBefore && !hasWordAfter;
                        default -> (hasWordBefore == hasWordAfter) == (arg != 0);
                    };
                }
                case OP$SYMBOL_START, OP$SYMBOL_END -> {
                    boolean hasSymbolBefore = sp != start || isSymbol(buffer, getChar(frame, input, sp - 1));
                    boolean hasSymbolAfter = sp != end || isSymbol(buffer, getChar(frame, input, sp));
                    success = opcode == OP$SYMBOL_START
                            ? !hasSymbolBefore && hasSymbolAfter
                            : hasSymbolBefore && !hasSymbolAfter;
                }
                case OP$CATEGORY_CHAR, OP$SYNTAX_CHAR -> {
                    boolean invert = (arg & ARG_BIT_FLAG) != 0;
                    arg = arg & ARG_BIT_MASK;
                    if (sp < end) {
                        int c = getChar(frame, input, sp);
                        success = (opcode == OP$SYNTAX_CHAR
                                ? isSyntaxClass(buffer, c, arg)
                                : isCategoryClass(buffer, c, arg)) != invert; // xor
                    } else {
                        success = false;
                    }
                    stack[SP_SLOT] = sp + 1;
                }
                case OP$BACKREF -> {
                    int groupStartSlot = groupSlotMap[arg];
                    int groupEndSlot = groupStartSlot + 1;
                    int groupStart = stack[groupStartSlot];
                    int groupEnd = stack[groupEndSlot];
                    int groupLength = groupEnd - groupStart;
                    if (groupLength > 0) {
                        if (sp + groupLength > end) {
                            success = false;
                        } else {
                            success = substringEquals(frame, input, sp, groupStart, groupLength);
                            stack[SP_SLOT] = sp + groupLength;
                        }
                    }
                }
                case OP$CHAR_CLASS, OP$CHAR_CLASS_32 -> {
                    if (sp < end) {
                        int c = getChar(frame, input, sp);
                        stack[SP_SLOT] = sp + 1;
                        boolean invert = code[bci] < 0;
                        success = matchCharClassBitMap(buffer, c, code[bci]);
                        if (!success) {
                            for (int i = 1; i < arg; ++i) {
                                int from, to;
                                if (opcode == OP$CHAR_CLASS) {
                                    int encodedRange = code[bci + i];
                                    from = encodedRange & 0xFF_FF;
                                    to = (encodedRange >> 16) & 0xFF_FF;
                                } else {
                                    from = code[bci + i];
                                    ++i;
                                    to = code[bci + i];
                                }
                                if (from <= c && c <= to) {
                                    success = true;
                                    break;
                                }
                            }
                        }
                        bci += arg;
                        success = invert != success; // xor
                    } else {
                        success = false;
                    }
                }
                case OP$CHAR -> {
                    success = sp < end && getChar(frame, input, sp) == (arg & 0xFF_FF_FF);
                    stack[SP_SLOT] = sp + 1;
                }
                case OP$ANY -> {
                    success = sp < end;
                    stack[SP_SLOT] = sp + 1;
                }
                default -> throw CompilerDirectives.shouldNotReachHere();
            }
            if (!success) {
                return false;
            }
        }
    }

    private static boolean matchCharClassBitMap(Object buffer, int c, int bits) {
        return (alnum.match(bits) && (Character.isAlphabetic(c) || Character.isDigit(c)))
                || (alpha.match(bits) && Character.isAlphabetic(c))
                || (ascii.match(bits) && c < 0x80)
                || (blank.match(bits) && Character.isWhitespace(c))
                || (cntrl.match(bits) && c < ' ')
                || (digit.match(bits) && '0' <= c && c <= '9')
                || (graph.match(bits) && Character.isValidCodePoint(c)
                && !(Character.isWhitespace(c) || Character.getType(c) == Character.CONTROL))
                || (lower.match(bits) && isLowerCase(buffer, c))
                || (multibyte.match(bits) && c >= 0x100)
                || (nonascii.match(bits) && c >= 0x80)
                || (print.match(bits) && Character.isValidCodePoint(c) && Character.getType(c) != Character.CONTROL)
                || (punct.match(bits) && isPunct(buffer, c))
                || (space.match(bits) && isSpace(buffer, c))
                || (unibyte.match(bits) && c < 0x100)
                || (upper.match(bits) && isUpperCase(buffer, c))
                || (word.match(bits) && isWord(buffer, c))
                || (xdigit.match(bits) && (Character.isDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')));
    }

    //#region Syntax Table
    private static boolean isPunct(Object buffer, int c) {
        return isSyntaxClass(buffer, c, SPUNCT);
    }
    private static boolean isSpace(Object buffer, int c) {
        return isSyntaxClass(buffer, c, SWHITESPACE);
    }
    private static boolean isWord(Object buffer, int c) {
        return isSyntaxClass(buffer, c, SWORD);
    }
    private static boolean isSymbol(Object buffer, int c) {
        return isSyntaxClass(buffer, c, SSYMBOL);
    }
    private static boolean isSyntaxClass(Object buf, int c, int clazz) {
        ELispBuffer buffer = asBuffer(buf);
        ELispCharTable table = asCharTable(buffer.getSyntaxTable());
        return table.getChar(c) instanceof ELispCons cons
                && cons.car() instanceof Long l
                && (l & 0xFFFF) == clazz;
    }
    private static boolean isCategoryClass(Object buf, int c, int clazz) {
        ELispBuffer buffer = asBuffer(buf);
        ELispCharTable table = asCharTable(buffer.getSyntaxTable());
        return table.getChar(c) instanceof ELispBoolVector boolVector
                && boolVector.get(clazz);
    }
    //#endregion Syntax Table

    //#region Case Table
    private static boolean isUpperCase(Object buffer, int c) {
        // TODO
        return Character.isUpperCase(c);
    }
    private static boolean isLowerCase(Object buffer, int c) {
        // TODO
        return Character.isLowerCase(c);
    }
    //#endregion Case Table

    private int getChar(VirtualFrame frame, Object input, int sp) {
        return getCharNode.execute(frame, input, sp);
    }

    private boolean substringEquals(VirtualFrame frame, Object input, int sp, int groupStart, int groupLength) {
        for (int i = 0; i < groupLength; ++i) {
            int from = groupStart + i;
            int to = sp + i;
            if (getChar(frame, input, from) != getChar(frame, input, to)) {
                return false;
            }
        }
        return true;
    }

    private Object packMatchResult(int[] stack) {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (int i : groupSlotMap) {
            int start = stack[i];
            builder.add(start == -1 ? false : (long) start);
            int end = stack[i + 1];
            builder.add(start == -1 ? false : (long) end);
        }
        return builder.build();
    }

    @Override
    public String toString() {
        return ELispRegExpCompiler.disassemble(IntArrayList.newListWith(code));
    }

    private record BacktrackingState(ArrayList<int[]> stacks, Object input,
                                     int start, int length, Object buffer) {
    }

}
