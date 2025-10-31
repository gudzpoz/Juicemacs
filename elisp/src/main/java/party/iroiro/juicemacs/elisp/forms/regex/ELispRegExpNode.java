package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.HostCompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.BytecodeOSRNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBoolVector;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;

import java.util.Arrays;

import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.lowerCaseP;
import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.upperCaseP;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.regex.CharClassContent.Named.*;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpOpcode.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

class ELispRegExpNode extends Node implements BytecodeOSRNode {

    public static final int ARG_OBJ_INPUT   = 0;
    public static final int ARG_BOOL_SEARCH = 1;
    public static final int ARG_INT_START   = 2;
    public static final int ARG_INT_END     = 3;
    public static final int ARG_OBJ_BUFFER  = 4;

    private static final int SP_SLOT = 0;
    private static final int PC_SLOT = 1;

    private static final int TRUFFLE_SLOT_INPUT = 0;
    private static final int TRUFFLE_SLOT_STACK_POOL = 1;
    private static final int TRUFFLE_SLOT_BUFFER = 2;
    private static final int TRUFFLE_SLOT_START = 3;
    private static final int TRUFFLE_SLOT_END = 4;
    private static final int TRUFFLE_SLOT_SEARCH_END = 5;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] code;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] groupSlotMap;
    private final int stackSize;
    private final boolean caseFold;

    @Nullable
    @CompilerDirectives.CompilationFinal
    Object osrMetadata;

    @Child
    ELispRegExpInputNodes.InputLengthNode lengthNode;

    @Child
    ELispRegExpInputNodes.InputGetCharNode getCharNode;

    @Child
    ELispRegExpInputNodes.InputStartIndexNode startIndexNode;

    protected ELispRegExpNode(ELispRegExpCompiler.Compiled compiled, boolean caseFold) {
        this.code = compiled.opcodes();
        this.stackSize = compiled.stackSize();
        this.groupSlotMap = compiled.groupSlotMap();
        this.caseFold = caseFold;
        startIndexNode = ELispRegExpInputNodesFactory.InputStartIndexNodeGen.create();
        lengthNode = ELispRegExpInputNodesFactory.InputLengthNodeGen.create();
        getCharNode = ELispRegExpInputNodesFactory.InputGetCharNodeGen.create();
    }

    public Object execute(VirtualFrame frame) {
        Object[] args = frame.getArguments();
        Object input = args[ARG_OBJ_INPUT];
        long start = (long) args[ARG_INT_START];
        long end = (long) args[ARG_INT_END];
        long minIndex = startIndexNode.execute(input);
        long length = lengthNode.execute(input);
        if (end == -1) {
            end = length;
        }
        boolean search = (boolean) args[ARG_BOOL_SEARCH];

        LongArrayStackPool pool = new LongArrayStackPool(initStack(start, search));
        frame.setObject(TRUFFLE_SLOT_INPUT, input);
        frame.setObject(TRUFFLE_SLOT_STACK_POOL, pool);
        frame.setObject(TRUFFLE_SLOT_BUFFER, args[ARG_OBJ_BUFFER]);
        frame.setLong(TRUFFLE_SLOT_START, minIndex);
        frame.setLong(TRUFFLE_SLOT_END, length);
        frame.setLong(TRUFFLE_SLOT_SEARCH_END, end);
        return dispatcher(frame);
    }

    private long[] initStack(long start, boolean search) {
        long[] initStack = new long[stackSize];
        initStack[SP_SLOT] = start;
        initStack[PC_SLOT] = search ? 0 : 1;
        for (int startSlot : groupSlotMap) {
            initStack[startSlot] = -1;
            int endSlot = startSlot + 1;
            initStack[endSlot] = -1;
        }
        return initStack;
    }

    @Nullable
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
        return dispatcher(osrFrame);
    }

    private Object dispatcher(VirtualFrame frame) {
        // The outer backtracking loop.
        // This loop must be separated from the inner loop to allow MERGE_EXPLODE,
        // since we do not know how many backtracking states we will need.
        Object input = frame.getObject(TRUFFLE_SLOT_INPUT);
        LongArrayStackPool stacks = (LongArrayStackPool) frame.getObject(TRUFFLE_SLOT_STACK_POOL);
        Object buffer = frame.getObject(TRUFFLE_SLOT_BUFFER);
        long end = frame.getLong(TRUFFLE_SLOT_SEARCH_END);
        long minIndex = frame.getLong(TRUFFLE_SLOT_START);
        long length = frame.getLong(TRUFFLE_SLOT_END);
        ELispCharTable canon = caseFold ? asCharTable(asBuffer(buffer).getCaseCanonTable()) : null;

        while (true) {
            Object lastRun = dispatchFromBCI(input, end, minIndex, length, stacks, buffer, canon);
            if (lastRun != Boolean.FALSE) {
                return lastRun;
            }
            if (stacks.isEmpty()) {
                return false;
            }
            // back-edge
            if (BytecodeOSRNode.pollOSRBackEdge(this)) {
                // An interpreter must ensure this method returns true immediately before calling tryOSR.
                Object result = BytecodeOSRNode.tryOSR(this, 0, null, null, frame);
                if (result != null) {
                    return result;
                }
            }
        }
    }

    @HostCompilerDirectives.BytecodeInterpreterSwitch
    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    private Object dispatchFromBCI(Object input, long searchEnd,
                                   long pointMin, long pointMax,
                                   LongArrayStackPool stacks,
                                   Object buffer, @Nullable ELispCharTable canon) {
        final long[] stack = stacks.borrowStack();
        int bci = 0;
        int cmpFlags = 0;

        loop:
        while (true) {
            CompilerAsserts.partialEvaluationConstant(bci);
            final int instruction = code[bci++];
            CompilerAsserts.partialEvaluationConstant(instruction);
            final int opcode = instruction >> 24;
            final int arg = (instruction << 8) >> 8;
            if (opcode < 0) {
                // Jump instructions
                final int cond = (opcode >> OP_FLAG_JMP_COND_SHIFT) & OP_FLAG_JMP_COND_MASK;
                final boolean success = switch (cond) {
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
                if (!success) {
                    continue;
                }
                // Possible back-edge here, but we choose to do OSR in the dispatcher function
                // since backtracking are quite frequent.
                bci += arg;
                CompilerAsserts.partialEvaluationConstant(bci);
                continue;
            }
            final long sp = stack[SP_SLOT];
            boolean success = true;
            switch (opcode) {
                case OP_MATCH -> {
                    if (bci != code.length) {
                        throw ELispSignals.error("Internal regexp engine error: unexpected end of code");
                    }
                    return packMatchResult(stack);
                }
                case OP_COUNTER_RESET -> stack[arg] = 0;
                case OP_COUNTER_INC -> ++stack[arg];
                case OP_COUNTER_CMP -> cmpFlags = Long.compare(stack[arg], code[bci++]);
                case OP_PROGRESS_REC -> stack[arg] = sp;
                case OP_PROGRESS_CMP -> cmpFlags = Long.compare(stack[arg], sp);
                case OP_JUMP_TABLE -> {
                    final int target = Math.toIntExact(stack[PC_SLOT]);
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
                case OP_SPLIT -> stacks.forkStack(stack)[PC_SLOT] = arg;
                case OP_LOOKAHEAD_CMP ->
                        cmpFlags = searchEnd <= sp || getCharCanon(input, sp, canon) == arg ? 0 : -1;
                case OP_LOOKAHEAD_BIT_HASH ->
                        cmpFlags = searchEnd <= sp || REAst.checkCharMask(getCharCanon(input, sp, canon), arg)
                                ? 0 : -1;
                case OP_LOOKBEHIND_CMP ->
                        cmpFlags = sp <= pointMin || getCharCanon(input, sp - 1, canon) == arg ? 0 : -1;
                case OP$STR_START -> success = sp == pointMin;
                case OP$STR_END -> success = sp == pointMax;
                case OP$LINE_START -> success = sp == pointMin
                        || getCharNode.execute(input, sp - 1) == '\n';
                case OP$LINE_END -> success = sp == pointMax
                        || getCharNode.execute(input, sp) == '\n';
                case OP$BUFFER_POINT -> success = input instanceof ELispBuffer in && in.getPoint() == sp;
                case OP$WORD_START, OP$WORD_END, OP$WORD_BOUND -> {
                    boolean hasWordBefore = sp != pointMin && isWord(buffer, getCharNode.execute(input, sp - 1));
                    boolean hasWordAfter = sp != pointMax && isWord(buffer, getCharNode.execute(input, sp));
                    success = switch (opcode) {
                        case OP$WORD_START -> !hasWordBefore && hasWordAfter;
                        case OP$WORD_END -> hasWordBefore && !hasWordAfter;
                        default -> (hasWordBefore == hasWordAfter) == (arg != 0);
                    };
                }
                case OP$SYMBOL_START, OP$SYMBOL_END -> {
                    boolean hasSymbolBefore = sp != pointMin && isSymbol(buffer, getCharNode.execute(input, sp - 1));
                    boolean hasSymbolAfter = sp != pointMax && isSymbol(buffer, getCharNode.execute(input, sp));
                    success = opcode == OP$SYMBOL_START
                            ? !hasSymbolBefore && hasSymbolAfter
                            : hasSymbolBefore && !hasSymbolAfter;
                }
                case OP$CATEGORY_CHAR, OP$SYNTAX_CHAR -> {
                    final boolean invert = (arg & ARG_BIT_FLAG) != 0;
                    final int kind = arg & ARG_BIT_MASK;
                    if (sp < searchEnd) {
                        int c = getCharNode.execute(input, sp);
                        success = (opcode == OP$SYNTAX_CHAR
                                ? getSyntaxClass(buffer, c) == kind
                                : isCategoryClass(buffer, c, kind)) != invert; // xor
                    } else {
                        success = false;
                    }
                    stack[SP_SLOT] = sp + 1;
                }
                case OP$BACKREF -> {
                    int groupStartSlot = groupSlotMap[arg];
                    int groupEndSlot = groupStartSlot + 1;
                    long groupStart = stack[groupStartSlot];
                    long groupEnd = stack[groupEndSlot];
                    long groupLength = groupEnd - groupStart;
                    if (groupLength > 0) {
                        if (sp + groupLength > searchEnd) {
                            success = false;
                        } else {
                            success = substringEquals(input, sp, groupStart, groupLength, canon);
                            stack[SP_SLOT] = sp + groupLength;
                        }
                    }
                }
                case OP$CHAR_CLASS, OP$CHAR_CLASS_32 -> {
                    if (sp < searchEnd) {
                        int c = getCharCanon(input, sp, canon);
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
                        success = invert != success; // xor
                    } else {
                        success = false;
                    }
                    bci += arg;
                    CompilerAsserts.partialEvaluationConstant(bci);
                }
                case OP$CHAR -> {
                    success = sp < searchEnd && getCharCanon(input, sp, canon) == (arg & 0xFF_FF_FF);
                    stack[SP_SLOT] = sp + 1;
                }
                case OP$TRIE -> {
                    int deltaSp = trieLookup(bci, stacks, stack, input, sp, searchEnd, canon);
                    if (deltaSp == -1) {
                        success = false;
                    } else {
                        stack[SP_SLOT] = sp + deltaSp;
                    }
                    bci += arg;
                    CompilerAsserts.partialEvaluationConstant(bci);
                }
                case OP$ANY_BUT -> {
                    success = sp < searchEnd && getCharNode.execute(input, sp) != arg;
                    stack[SP_SLOT] = sp + 1;
                }
                case OP$ANY -> {
                    success = sp < searchEnd;
                    stack[SP_SLOT] = sp + 1;
                }
                default -> throw CompilerDirectives.shouldNotReachHere();
            }
            if (!success) {
                stacks.returnStack(stack);
                return false;
            }
        }
    }

    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    private int trieLookup(
            int bci, LongArrayStackPool stacks, long[] stack,
            Object input, long spStart, long searchEnd,
            @Nullable ELispCharTable canon
    ) {
        CompilerAsserts.partialEvaluationConstant(bci);
        final int paths = code[bci] >> 16;
        final int[] pathStats = new int[paths + 2];
        int pc = bci + 1 + paths;
        long sp = 0;
        loop:
        while (true) {
            int stateInfo = code[pc];
            int transitions = stateInfo & 0xFFFF;
            int endPath = stateInfo >> 16;
            if (endPath != -1 && pathStats[2 + endPath] == 0) {
                pathStats[0]++; // count
                pathStats[1] = endPath; // index
                pathStats[2 + endPath] = 1;
            }
            int c = spStart + sp < searchEnd ? getCharCanon(input, spStart + sp++, canon) : -1;
            for (int i = 0; i < transitions; i++) {
                int transition = code[pc + 1 + i];
                int expected = transition & 0xFFFF;
                if (c == expected) {
                    pc += transition >> 16;
                    continue loop;
                }
            }
            break;
        }
        if (pathStats[0] == 0) {
            return -1;
        } else if (pathStats[0] == 1) {
            return code[bci + 1 + pathStats[1]];
        }
        return trieCommit(bci, stacks, stack, spStart, pathStats);
    }

    private int trieCommit(int bci, LongArrayStackPool stacks, long[] stack, long spStart, int[] pathStats) {
        int now = -1;
        for (int i = 2; i < pathStats.length; i++) {
            if (pathStats[i] != 0) {
                if (now == -1) {
                    now = code[bci + 1 + i - 2];
                } else {
                    long[] newStack = stacks.forkStack(stack);
                    newStack[SP_SLOT] = spStart + code[bci + 1 + i - 2];
                    newStack[PC_SLOT] = code[bci] & 0xFFFF;
                }
            }
        }
        return now;
    }

    private boolean matchCharClassBitMap(Object buffer, int c, int bits) {
        return (alnum.match(bits) && (Character.isAlphabetic(c) || Character.isDigit(c)))
                || (alpha.match(bits) && Character.isAlphabetic(c))
                || (ascii.match(bits) && c < 0x80)
                || (blank.match(bits) && Character.isWhitespace(c))
                || (cntrl.match(bits) && c < ' ')
                || (digit.match(bits) && '0' <= c && c <= '9')
                || (graph.match(bits) && Character.isValidCodePoint(c)
                && !(Character.isWhitespace(c) || Character.getType(c) == Character.CONTROL))
                || (lower.match(bits) && (isLowerCase(buffer, c) || (caseFold && isUpperCase(buffer, c))))
                || (multibyte.match(bits) && c >= 0x100)
                || (nonascii.match(bits) && c >= 0x80)
                || (print.match(bits) && Character.isValidCodePoint(c) && Character.getType(c) != Character.CONTROL)
                || (punct.match(bits) && isPunct(buffer, c))
                || (space.match(bits) && isSpace(buffer, c))
                || (unibyte.match(bits) && c < 0x100)
                || (upper.match(bits) && (isUpperCase(buffer, c) || (caseFold && isLowerCase(buffer, c))))
                || (word.match(bits) && isWord(buffer, c))
                || (xdigit.match(bits) && (Character.isDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')));
    }

    //#region Syntax Table
    private static boolean isPunct(Object buffer, int c) {
        return getSyntaxClass(buffer, c) == SPUNCT;
    }
    private static boolean isSpace(Object buffer, int c) {
        return getSyntaxClass(buffer, c) == SWHITESPACE;
    }
    private static boolean isWord(Object buffer, int c) {
        return getSyntaxClass(buffer, c) == SWORD;
    }
    private static boolean isSymbol(Object buffer, int c) {
        int syntaxClass = getSyntaxClass(buffer, c);
        return syntaxClass == SWORD || syntaxClass == SSYMBOL;
    }
    private static int getSyntaxClass(Object buf, int c) {
        ELispBuffer buffer = asBuffer(buf);
        ELispCharTable table = asCharTable(buffer.getSyntaxTable());
        return table.getChar(c) instanceof ELispCons cons
                && cons.car() instanceof Long l
                ? Math.toIntExact(l & 0xFFFF)
                : -1;
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
        return upperCaseP(c, asBuffer(buffer));
    }
    private static boolean isLowerCase(Object buffer, int c) {
        return lowerCaseP(c, asBuffer(buffer));
    }
    //#endregion Case Table

    public static int translate(int current, ELispCharTable canon) {
        Object c = canon.getChar(current);
        return c instanceof Long l ? l.intValue() : current;
    }

    private int getCharCanon(Object input, long sp, @Nullable ELispCharTable canon) {
        int c = getCharNode.execute(input, sp);
        assert !caseFold || canon != null;
        return caseFold ? translate(c, assertNotNull(canon)) : c;
    }

    private boolean substringEquals(Object input, long sp, long groupStart, long groupLength,
                                    @Nullable ELispCharTable canon) {
        for (int i = 0; i < groupLength; ++i) {
            long from = groupStart + i;
            long to = sp + i;
            if (getCharCanon(input, from, canon) != getCharCanon(input, to, canon)) {
                return false;
            }
        }
        return true;
    }

    private Object packMatchResult(long[] stack) {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (int i : groupSlotMap) {
            long start = stack[i];
            builder.add(start == -1 ? false : start);
            long end = stack[i + 1];
            builder.add(start == -1 ? false : end);
        }
        return builder.build();
    }

    @Override
    public String toString() {
        return ELispRegExpCompiler.disassemble(IntArrayList.newListWith(code));
    }

    static final FrameDescriptor REGEXP_FRAME_DESCRIPTOR = getFrameDescriptor();

    static FrameDescriptor getFrameDescriptor() {
        FrameDescriptor.Builder builder = FrameDescriptor.newBuilder();
        builder.addSlots(3, FrameSlotKind.Object); // input, buffer, stackPool
        builder.addSlots(3, FrameSlotKind.Long); // start, end, searchEnd
        return builder.build();
    }

    private static final class LongArrayStackPool {
        private long[][] stacks;
        private int stackCount;
        private int top;

        private LongArrayStackPool(long @Nullable [] initStack) {
            stacks = new long[128][];
            top = 0;
            stackCount = 1;
            stacks[0] = initStack;
        }

        private long[] forkStack(long[] stack) {
            long[] copy;
            top++;
            if (top < stackCount) {
                copy = stacks[top];
                System.arraycopy(stack, 0, copy, 0, stack.length);
            } else {
                if (stackCount >= stacks.length) {
                    stacks = Arrays.copyOf(stacks, stacks.length * 2);
                }
                copy = stack.clone();
                stacks[top] = copy;
                stackCount++;
            }
            return copy;
        }

        private boolean isEmpty() {
            return top < 0;
        }

        private long[] borrowStack() {
            long[] stack = stacks[top];
            stacks[top--] = stacks[--stackCount];
            return stack;
        }

        private void returnStack(long[] stack) {
            if (stackCount >= stacks.length) {
                stacks = Arrays.copyOf(stacks, stacks.length * 2);
            }
            stacks[stackCount++] = stack;
        }
    }
}
