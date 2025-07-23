package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.BytecodeOSRNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBoolVector;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;

import java.util.Arrays;

import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.lowerCaseP;
import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.upperCaseP;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpCompiler.*;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpLexer.CharClassContent.NamedCharClass.*;
import static party.iroiro.juicemacs.elisp.forms.regex.ELispRegExpOpcode.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

class ELispRegExpNode extends Node implements BytecodeOSRNode {

    public static final int ARG_OBJ_INPUT = 0;
    public static final int ARG_BOOL_SEARCH = 1;
    public static final int ARG_INT_START = 2;
    public static final int ARG_INT_END = 3;
    public static final int ARG_OBJ_BUFFER = 4;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] code;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] groupSlotMap;
    private final boolean caseFold;
    private final int registerCount;

    @SuppressWarnings("NotNullFieldNotInitialized")
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
        this.groupSlotMap = compiled.groupSlotMap();
        this.registerCount = compiled.slots();
        this.caseFold = caseFold;
        startIndexNode = ELispRegExpInputNodesFactory.InputStartIndexNodeGen.create();
        lengthNode = ELispRegExpInputNodesFactory.InputLengthNodeGen.create();
        getCharNode = ELispRegExpInputNodesFactory.InputGetCharNodeGen.create();
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

    //#region Case Table
    private static boolean isUpperCase(Object buffer, int c) {
        return upperCaseP(c, asBuffer(buffer));
    }

    private static boolean isLowerCase(Object buffer, int c) {
        return lowerCaseP(c, asBuffer(buffer));
    }

    public static int translate(int current, ELispCharTable canon) {
        Object c = canon.getChar(current);
        return c instanceof Long l ? l.intValue() : current;
    }

    public Object execute(VirtualFrame frame) {
        Object[] args = frame.getArguments();
        Object input = args[ARG_OBJ_INPUT];
        long start = (long) args[ARG_INT_START];
        long end = (long) args[ARG_INT_END];
        long minIndex = startIndexNode.execute(frame, input);
        long length = lengthNode.execute(frame, input);
        if (end == -1) {
            end = length;
        }

        long[] registers = new long[registerCount];
        Arrays.fill(registers, -1);
        registers[SP_REG] = start;
        Object buffer = args[ARG_OBJ_BUFFER];
        ELispCharTable canon = caseFold ? asCharTable(asBuffer(buffer).getCaseCanonTable()) : null;
        InterpreterState state = new InterpreterState(
                registers,
                new ArrayStack(),
                input,
                end, minIndex, length,
                buffer,
                canon
        );
        return dispatchFromBCI(frame, 0, state);
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
        InterpreterState state = (InterpreterState) interpreterState;
        return dispatchFromBCI(osrFrame, target, state);
    }
    //#endregion Syntax Table

    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    private Object dispatchFromBCI(VirtualFrame frame, int bci, InterpreterState state) {
        CompilerAsserts.partialEvaluationConstant(bci);

        while (true) {
            CompilerAsserts.partialEvaluationConstant(bci);
            final int instruction = code[bci];
            final int opcode = instruction >> 24;
            final int arg = (instruction << 8) >> 8;
            CompilerAsserts.partialEvaluationConstant(instruction);
            CompilerAsserts.partialEvaluationConstant(opcode);
            CompilerAsserts.partialEvaluationConstant(arg);
            final long sp = state.sp();
            int nextBci;
            switch (opcode) {
                case OP_MATCH -> {
                    if (bci + 1 != code.length) {
                        throw ELispSignals.error("Internal regexp engine error: unexpected end of code");
                    }
                    return packMatchResult(state);
                }
                case OP_PROLOG_SEARCH -> {
                    boolean search = (boolean) frame.getArguments()[ARG_BOOL_SEARCH];
                    if (search) {
                        state.stack.push(sp);
                    }
                    nextBci = bci + 2;
                }
                case OP_PROLOG$SEARCH -> {
                    int top = state.stack.top();
                    if (top == -1) {
                        return false;
                    }
                    assert top == 0;
                    long nextSp = state.stack.pop() + 1;
                    if (nextSp > state.searchEnd) {
                        return false;
                    }
                    state.sp(nextSp);
                    state.stack.push(nextSp);
                    nextBci = bci + 1;
                }

                case OP_JUMP -> nextBci = bci + arg;
                case OP_UNION_PRE -> {
                    state.stack.push(state.get(arg), sp);
                    state.set(arg, 0);
                    nextBci = bci + 3;
                }
                case OP_UNION$PRE -> {
                    int slot = (code[bci - 1] << 8) >> 8;
                    long count = state.get(slot);
                    if (count == 0) {
                        state.set(slot, 1);
                        state.sp(state.stack.peek());
                        nextBci = code[bci + 1];
                        CompilerAsserts.partialEvaluationConstant(nextBci);
                        bci = nextBci;
                        continue;
                    } else {
                        state.stack.pop();
                        state.set(slot, state.stack.pop());
                        nextBci = bci + arg;
                    }
                }
                case OP_UNION$POST -> {
                    long count = state.get(arg);
                    if (count == 0) {
                        nextBci = code[bci + 1];
                    } else {
                        nextBci = code[bci + 2];
                        CompilerAsserts.partialEvaluationConstant(nextBci);
                        bci = nextBci;
                        continue;
                    }
                }

                case OP_CAPTURE_WRITE -> {
                    state.stack.push(state.get(arg));
                    state.set(arg, sp);
                    nextBci = bci + 2;
                }
                case OP_CAPTURE$WRITE -> {
                    int slot = (code[bci - 1] << 8) >> 8;
                    state.set(slot, state.stack.pop());
                    nextBci = bci + arg;
                }

                // prev$
                // quant_pre
                // quant$pre
                // body
                // body$
                // quant_post
                // quant$post
                // next
                case OP_QUANT_PRE -> {
                    state.stack.push(state.get(arg));
                    state.stack.push(sp);
                    state.set(arg, 0);
                    state.set(arg + 1, sp);
                    nextBci = bci + 4; // -> body
                }
                case OP_QUANT$PRE, OP_QUANT$PRE_LAZY -> {
                    int slot = (code[bci - 1] << 8) >> 8;
                    long count = state.get(slot);
                    if (count == 0) {
                        state.set(slot + 1, state.stack.pop());
                        state.set(slot, state.stack.pop());
                        nextBci = bci + arg; // -> prev$
                    } else {
                        if (opcode == OP_QUANT$PRE) {
                            long min = code[bci + 2];
                            state.sp(state.stack.peek());
                            if (count < min) {
                                // count < min: -> quant$post
                                nextBci = code[bci + 1] - 1;
                                CompilerAsserts.partialEvaluationConstant(nextBci);
                                bci = nextBci;
                                continue;
                            } else {
                                // count >= min: -> next
                                nextBci = code[bci + 1];
                                CompilerAsserts.partialEvaluationConstant(nextBci);
                                bci = nextBci;
                                continue;
                            }
                        } else {
                            state.set(slot, count - 1);
                            nextBci = code[bci + 1]; // -> body$
                            CompilerAsserts.partialEvaluationConstant(nextBci);
                            bci = nextBci;
                            continue;
                        }
                    }
                }
                case OP_QUANT_POST, OP_QUANT_POST_LAZY -> {
                    long count = state.get(arg) + 1;
                    state.set(arg, count);
                    int max = code[bci + 2];
                    long lastSp = state.get(arg + 1);
                    if (opcode == OP_QUANT_POST) {
                        int min = code[code[bci + 1] - 1];
                        state.set(arg + 1, sp);
                        state.stack.push(sp);
                        if ((min <= count && sp == lastSp) || count >= max) {
                            nextBci = bci + 4; // -> next
                            CompilerAsserts.partialEvaluationConstant(nextBci);
                            bci = nextBci;
                            continue;
                        } else {
                            nextBci = code[bci + 1]; // -> body
                        }
                    } else {
                        int min = code[bci + 1];
                        assert min <= count;
                        // Emacs regex does not have a syntax for lazy {min,max}.
                        // So min is either 0 or 1 and count always >= 1.
                        // This might be needed if we ever want to optimize "a\\{4\\}a+"
                        // into "a\\{5,\\}" though.
                        // Similarly, count <= max is always true as max is Integer.MAX_VALUE.

//                        if (min > count) {
//                            nextBci = bci + 3; // quant$post
//                        } else
                        if (sp != lastSp && count <= max) {
                            state.stack.push(sp);
                            nextBci = bci + 4; // -> next
                            CompilerAsserts.partialEvaluationConstant(nextBci);
                            bci = nextBci;
                            continue;
                        } else {
                            int offset = (code[bci + 3] << 8) >> 8;
                            nextBci = bci + 3 + offset; // -> quant$pre
                        }
                    }
                }
                case OP_QUANT$POST -> {
                    int slot = (code[bci - 3] << 8) >> 8;
                    state.set(slot, state.get(slot) - 1);
                    state.stack.pop();
                    nextBci = bci + arg; // -> body$
                }
                case OP_QUANT$POST_LAZY -> {
                    int slot = (code[bci - 3] << 8) >> 8;
                    long count = state.get(slot);
                    int max = code[bci - 1];
                    assert count < max;
                    long prevSp = state.stack.pop();
                    state.sp(prevSp);
                    state.set(slot + 1, prevSp);
                    nextBci = bci + arg + 3; // -> body
                    // See comments above. Count < max is always true.
//                    if (count < max) {
//                        state.set(slot + 1, sp);
//                        nextBci = bci + arg + 3; // -> body
//                    } else {
//                        nextBci = bci + arg; // quant$pre
//                        CompilerAsserts.partialEvaluationConstant(nextBci);
//                        bci = nextBci;
//                        continue;
//                    }
                }

                case OP$CHAR_CLASS, OP$CHAR_CLASS_32 -> {
                    boolean success;
                    int base = bci + 2;
                    if (sp < state.searchEnd) {
                        int c = getCharCanon(frame, sp, state);
                        state.sp(state.sp() + 1);
                        boolean invert = code[base] < 0;
                        success = matchCharClassBitMap(state.buffer, c, code[base]);
                        if (!success) {
                            for (int i = 1; i < arg; ++i) {
                                int from, to;
                                if (opcode == OP$CHAR_CLASS) {
                                    int encodedRange = code[base + i];
                                    from = encodedRange & 0xFF_FF;
                                    to = (encodedRange >> 16) & 0xFF_FF;
                                } else {
                                    from = code[base + i];
                                    ++i;
                                    to = code[base + i];
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
                    if (success) {
                        nextBci = base + arg;
                        bci = nextBci;
                        continue;
                    } else {
                        nextBci = bci + code[bci + 1];
                    }
                }
                default -> {
                    long searchEnd = state.searchEnd;
                    long pointMin = state.pointMin;
                    long pointMax = state.pointMax;
                    Object input = state.input;
                    Object buffer = state.buffer;
                    boolean success = switch (opcode) {
                        case OP$STR_START -> sp == pointMin;
                        case OP$STR_END -> sp == pointMax;
                        case OP$LINE_START -> sp == pointMin || getChar(frame, input, sp - 1) == '\n';
                        case OP$LINE_END -> sp == pointMax || getChar(frame, input, sp) == '\n';
                        case OP$BUFFER_POINT -> input instanceof ELispBuffer in && in.getPoint() - 1 == sp;
                        case OP$WORD_START, OP$WORD_END, OP$WORD_BOUND -> {
                            boolean hasWordBefore = sp != pointMin && isWord(buffer, getChar(frame, input, sp - 1));
                            boolean hasWordAfter = sp != pointMax && isWord(buffer, getChar(frame, input, sp));
                            yield switch (opcode) {
                                case OP$WORD_START -> !hasWordBefore && hasWordAfter;
                                case OP$WORD_END -> hasWordBefore && !hasWordAfter;
                                default -> (hasWordBefore == hasWordAfter) == (arg != 0);
                            };
                        }
                        case OP$SYMBOL_START, OP$SYMBOL_END -> {
                            boolean hasSymbolBefore = sp != pointMin && isSymbol(buffer, getChar(frame, input, sp - 1));
                            boolean hasSymbolAfter = sp != pointMax && isSymbol(buffer, getChar(frame, input, sp));
                            yield opcode == OP$SYMBOL_START
                                    ? !hasSymbolBefore && hasSymbolAfter
                                    : hasSymbolBefore && !hasSymbolAfter;
                        }
                        case OP$CATEGORY_CHAR, OP$SYNTAX_CHAR -> {
                            final boolean invert = (arg & ARG_BIT_FLAG) != 0;
                            final int kind = arg & ARG_BIT_MASK;
                            if (sp < searchEnd) {
                                int c = getChar(frame, input, sp);
                                state.sp(state.sp() + 1);
                                yield (opcode == OP$SYNTAX_CHAR
                                        ? getSyntaxClass(buffer, c) == kind
                                        : isCategoryClass(buffer, c, kind)) != invert; // xor
                            } else {
                                yield false;
                            }
                        }
                        case OP$BACKREF -> {
                            int groupStartSlot = groupSlotMap[arg];
                            int groupEndSlot = groupStartSlot + 1;
                            long groupStart = state.get(groupStartSlot);
                            long groupEnd = state.get(groupEndSlot);
                            long groupLength = groupEnd - groupStart;
                            if (groupLength > 0) {
                                if (sp + groupLength <= searchEnd) {
                                    state.sp(state.sp() + groupLength);
                                    yield substringEquals(frame, sp, groupStart, groupLength, state);
                                }
                            }
                            yield false;
                        }
                        case OP$CHAR -> {
                            state.sp(state.sp() + 1);
                            yield sp < searchEnd && getCharCanon(frame, sp, state) == (arg & 0xFF_FF_FF);
                        }
                        case OP$ANY_BUT -> {
                            state.sp(state.sp() + 1);
                            yield sp < searchEnd && getChar(frame, input, sp) != '\n';
                        }
                        case OP$ANY -> {
                            state.sp(state.sp() + 1);
                            yield sp < searchEnd;
                        }
                        default -> throw CompilerDirectives.shouldNotReachHere();
                    };
                    if (success) {
                        nextBci = bci + 2;
                        CompilerAsserts.partialEvaluationConstant(nextBci);
                        bci = nextBci;
                        continue;
                    } else {
                        nextBci = bci + code[bci + 1];
                        CompilerAsserts.partialEvaluationConstant(nextBci);
                    }
                }
            }
            if (nextBci < bci) {
                if (BytecodeOSRNode.pollOSRBackEdge(this)) {
                    Object result = BytecodeOSRNode.tryOSR(this, nextBci, state, null, frame);
                    if (result != null) {
                        return result;
                    }
                }
            }
            bci = nextBci;
        }
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

    private int getChar(VirtualFrame frame, Object input, long sp) {
        return getCharNode.execute(frame, input, sp);
    }

    private int getCharCanon(VirtualFrame frame, long sp, InterpreterState state) {
        int c = getChar(frame, state.input, sp);
        assert !caseFold || state.canon != null;
        return caseFold ? translate(c, state.canon) : c;
    }

    private boolean substringEquals(VirtualFrame frame,
                                    long sp, long groupStart, long groupLength,
                                    InterpreterState state) {
        for (int i = 0; i < groupLength; ++i) {
            long from = groupStart + i;
            long to = sp + i;
            if (getCharCanon(frame, from, state) != getCharCanon(frame, to, state)) {
                return false;
            }
        }
        return true;
    }

    @ExplodeLoop
    private Object packMatchResult(InterpreterState state) {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (int i : groupSlotMap) {
            long start = state.get(i);
            builder.add(start == -1 ? false : start);
            long end = state.get(i + 1);
            builder.add(start == -1 ? false : end);
        }
        return builder.build();
    }

    record InterpreterState(
            long[] registers,
            ArrayStack stack,
            Object input,
            long searchEnd, long pointMin, long pointMax,
            Object buffer,
            @Nullable ELispCharTable canon
    ) {
        public long sp() {
            return registers[SP_REG];
        }

        public void sp(long sp) {
            registers[SP_REG] = sp;
        }

        public long get(int reg) {
            return registers[reg];
        }

        public void set(int reg, long value) {
            registers[reg] = value;
        }
    }

    private static final class ArrayStack {
        private long[] stack = new long[256];
        private int top = -1;

        public void push(long value) {
            top++;
            if (top >= stack.length) {
                expand();
            }
            stack[top] = value;
        }

        public void push(long v1, long v2) {
            top += 2;
            if (top >= stack.length) {
                expand();
            }
            stack[top - 1] = v1;
            stack[top] = v2;
        }

        public long pop() {
            return stack[top--];
        }

        public long peek() {
            return stack[top];
        }

        public int top() {
            return top;
        }

        @CompilerDirectives.TruffleBoundary
        private void expand() {
            stack = Arrays.copyOf(stack, stack.length * 2);
        }
    }
}
