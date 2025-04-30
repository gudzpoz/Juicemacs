package party.iroiro.juicemacs.elisp.nodes.bytecode;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.HostCompilerDirectives;
import com.oracle.truffle.api.bytecode.GenerateBytecode;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.*;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static party.iroiro.juicemacs.elisp.nodes.bytecode.ByteCode.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// A fallback implementation for Emacs bytecode
///
/// We hope to eventually implement a bytecode interpreter with Truffle DSL
/// ([GenerateBytecode]). However, this DSL
/// seems to have quite strict requirements, so we might want to fall back
/// to a simpler implementation like this when the bytecode is really messy
/// (possibly user generated?).
///
/// ## Limitations
///
/// However, this implementation also poses some requirements for the bytecode:
/// 1. The stack height at each PC must be deterministic. This means you cannot
///    have bytecode like `(loop-construct (push-onto-stack) (without-popping-off))`.
///    Emacs byte compiler does not generate bytecode like this, and this kind
///    of bytecode cannot be compiled by Truffle.
/// 2. Bytecode instructions must be compact. This means you cannot have bytecode
///    like `(goto :label) (0) (0) ... invalid ... (0) (0) (:label (valid code))`.
///    Again, this cannot be compiled and might require complex analysis to support.
public class ELispBytecodeFallbackNode extends ELispExpressionNode implements BytecodeOSRNode {

    private final ELispBytecode object;
    private final int startStackTop;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    final byte[] bytecode;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    final Object[] constants;

    /// Metadata index for each bytecode instruction
    ///
    /// Different bytecodes might require different specializations, and we store
    /// the corresponding data in this array.
    ///
    /// ## Examples
    ///
    /// For example, for each [ByteCode#ADD1]/[ByteCode#CALL] instruction, we want one
    /// specialized node for it. And we store the index to [#nodes] in this
    /// indices array.
    ///
    /// For another, each [ByteCode#SWITCH] instruction wants separate jump tables,
    /// so for them, the array stores jump table indices to [#jumpTables].
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    final int[] indices;
    /// Specialized nodes for some instructions
    @Children
    final Node[] nodes;
    /// Jump tables for [ByteCode#SWITCH] instruction
    @CompilerDirectives.CompilationFinal(dimensions = 2)
    final int[][] switchJumpTables;

    /// Jump table for exception (catch/condition-case) handlers
    ///
    /// If `null`, the function has no handlers.
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    final int @Nullable[] exceptionJumps;
    /// Stack top positions for each exception handler
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    final int @Nullable[] exceptionJumpsStackTop;

    @SuppressWarnings("NotNullFieldNotInitialized")
    @CompilerDirectives.CompilationFinal
    private Object osrMetadata;

    public ELispBytecodeFallbackNode(ELispBytecode bytecode, int startStackTop) {
        CompilerAsserts.neverPartOfCompilation();
        this.object = bytecode;
        this.startStackTop = startStackTop;
        byte[] bytes = bytecode.getBytecode();
        this.bytecode = bytes;
        this.constants = bytecode.getConstants();

        indices = new int[bytes.length];
        // stackTops is used to check if each PC in the function
        // has deterministic stack top positions.
        int[] stackTops = new int[bytes.length];
        Arrays.fill(stackTops, -1);

        IntArrayList exceptionJumps = new IntArrayList();
        ArrayList<int[]> switchJumpTables = new ArrayList<>();
        ArrayList<Node> nodes = new ArrayList<>();

        int stackTop = startStackTop;
        int lastConstantI = -1;
        for (int i = 0; i < bytes.length; ) {
            int otherBranchTop = stackTops[i];
            if (otherBranchTop == -1) {
                stackTops[i] = stackTop;
            } else {
                if (stackTop == -1) {
                    stackTop = otherBranchTop;
                } else if (otherBranchTop != stackTop) {
                    throw ELispSignals.invalidFunction(object);
                }
            }
            byte op = bytes[i];
            int ref;
            int jumpTarget = -1, jumpTargetTop = -1;
            int stackDelta = BYTECODE_STACK_EFFECTS[Byte.toUnsignedInt(op)];
            if (stackDelta == 0x7F) {
                stackDelta = Integer.MAX_VALUE;
            }
            int newConstantI = -1;
            indices[i] = switch (op) {
                case STACK_REF1:                   // 1
                case STACK_REF2:                   // 2
                case STACK_REF3:                   // 3
                case STACK_REF4:                   // 4
                case STACK_REF5:                   // 5
                case STACK_REF6:                   // 6
                case STACK_REF7:                   // 7
                    ref = op == STACK_REF6 ? Byte.toUnsignedInt(bytes[i + 1]) :
                            op == STACK_REF7
                                    ? Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8)
                                    : op - STACK_REF;
                    nodes.add(ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(stackTop + 1, null,
                            ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(stackTop - ref, null)));
                    yield nodes.size() - 1;
                case VARREF:                       // 010
                case VARREF1:                      // 011
                case VARREF2:                      // 012
                case VARREF3:                      // 013
                case VARREF4:                      // 014
                case VARREF5:                      // 015
                case VARREF6:                      // 016
                case VARREF7:                      // 017
                    ref = op == VARREF6 ? Byte.toUnsignedInt(bytes[i + 1]) :
                            op == VARREF7
                                    ? Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8)
                                    : op - VARREF;
                    nodes.add(ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(stackTop + 1, null,
                            GlobalVariableReadNodeGen.create(asSym(constants[ref]))));
                    yield nodes.size() - 1;
                case VARSET:                       // 020
                case VARSET1:                      // 021
                case VARSET2:                      // 022
                case VARSET3:                      // 023
                case VARSET4:                      // 024
                case VARSET5:                      // 025
                case VARSET6:                      // 026
                case VARSET7:                      // 027
                    ref = op == VARSET6 ? Byte.toUnsignedInt(bytes[i + 1]) :
                            op == VARSET7
                                    ? Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8)
                                    : op - VARSET;
                    nodes.add(GlobalVariableWriteNodeGen.GlobalVariableDirectWriteNodeGen.create(asSym(constants[ref])));
                    yield nodes.size() - 1;
                case CALL:                         // 040
                case CALL1:                        // 041
                case CALL2:                        // 042
                case CALL3:                        // 043
                case CALL4:                        // 044
                case CALL5:                        // 045
                case CALL6:                        // 046
                case CALL7:                        // 047
                    ref = op == CALL6 ? Byte.toUnsignedInt(bytes[i + 1]) :
                            op == CALL7 ? Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8)
                                    : op - CALL;
                    stackDelta = -ref;
                    nodes.add(ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(
                            stackTop - ref,
                            null,
                            new InlinableCallNode(stackTop, ref)
                    ));
                    yield nodes.size() - 1;
                case PUSHCONDITIONCASE:            // 061
                case PUSHCATCH:                    // 062
                    ref = Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8);
                    exceptionJumps.add(ref);
                    jumpTarget = ref;
                    jumpTargetTop = stackTop;
                    yield 0;
                case LENGTH:                       // 0107
                    nodes.add(createUnaryFactoryNode(stackTop, BuiltInFnsFactory.FLengthFactory::create));
                    yield nodes.size() - 1;
                case AREF:                         // 0110
                    nodes.add(createBinaryFactoryNode(stackTop, BuiltInDataFactory.FArefFactory::create));
                    yield nodes.size() - 1;
                case ASET:                         // 0111
                    nodes.add(createTernaryFactoryNode(stackTop, BuiltInDataFactory.FAsetFactory::create));
                    yield nodes.size() - 1;
                case SUBSTRING:                    // 0117
                    nodes.add(createTernaryFactoryNode(stackTop, BuiltInFnsFactory.FSubstringFactory::create));
                    yield nodes.size() - 1;
                case SUB1:                         // 0123
                    nodes.add(createUnaryFactoryNode(stackTop, BuiltInDataFactory.FSub1Factory::create));
                    yield nodes.size() - 1;
                case ADD1:                         // 0124
                    nodes.add(createUnaryFactoryNode(stackTop, BuiltInDataFactory.FAdd1Factory::create));
                    yield nodes.size() - 1;
                case EQLSIGN:                      // 0125
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FEqlsignBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case GTR:                          // 0126
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FGtrBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case LSS:                          // 0127
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FLssBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case LEQ:                          // 0130
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FLeqBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case GEQ:                          // 0131
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FGeqBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case DIFF:                         // 0132
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FMinusBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case NEGATE:                       // 0133
                    nodes.add(createUnaryNode(stackTop, BuiltInDataFactory.FMinusUnaryNodeGen::create));
                    yield nodes.size() - 1;
                case PLUS:                         // 0134
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FPlusBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case MULT:                         // 0137
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FTimesBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case CONSTANT2:                    // 0201
                    ref = Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8);
                    newConstantI = ref;
                    yield 0;
                case GOTO:                         // 0202
                case GOTOIFNIL:                    // 0203
                case GOTOIFNONNIL:                 // 0204
                case GOTOIFNILELSEPOP:             // 0205
                case GOTOIFNONNILELSEPOP:          // 0206
                    boolean elsePop = op == GOTOIFNILELSEPOP || op == GOTOIFNONNILELSEPOP;
                    ref = Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8);
                    if (stackTop != -1) {
                        jumpTarget = ref;
                        jumpTargetTop = stackTop + (elsePop ? 0 : stackDelta);
                    }
                    if (elsePop) {
                        stackDelta = -1;
                    } else if (op == GOTO) {
                        stackDelta = Integer.MAX_VALUE;
                    }
                    yield 0;
                case UPCASE:                       // 0226
                    nodes.add(createUnaryFactoryNode(stackTop, BuiltInCaseFiddleFactory.FUpcaseFactory::create));
                    yield nodes.size() - 1;
                case DOWNCASE:                     // 0227
                    nodes.add(createUnaryFactoryNode(stackTop, BuiltInCaseFiddleFactory.FDowncaseFactory::create));
                    yield nodes.size() - 1;
                case ELT:                          // 0234
                    nodes.add(createBinaryFactoryNode(stackTop, BuiltInFnsFactory.FEltFactory::create));
                    yield nodes.size() - 1;
                case NREVERSE:                     // 0237
                    nodes.add(createUnaryFactoryNode(stackTop, BuiltInFnsFactory.FNreverseFactory::create));
                    yield nodes.size() - 1;
                case QUO:                          // 0245
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FQuoBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case REM:                          // 0246
                    nodes.add(createBinaryFactoryNode(stackTop, BuiltInDataFactory.FRemFactory::create));
                    yield nodes.size() - 1;
                case LISTN:                        // 0257
                case CONCATN:                      // 0260
                case INSERTN:                      // 0261
                    stackDelta = -Byte.toUnsignedInt(bytes[i + 1]) + 1;
                    yield 0;
                case STACK_SET:                    // 0262
                case STACK_SET2:                   // 0263
                    ref = op == STACK_SET ? Byte.toUnsignedInt(bytes[i + 1])
                            : Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8);
                    nodes.add(ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(stackTop - ref, null,
                            ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(stackTop, null)));
                    yield nodes.size() - 1;
                case DISCARDN:                     // 0266
                    ref = Byte.toUnsignedInt(bytes[i + 1]);
                    stackDelta = -(ref & 0x7F);
                    yield 0;
                case SWITCH:                       // 0267
                {
                    if (lastConstantI == -1) {
                        throw ELispSignals.invalidFunction(object);
                    }
                    int targetStackTop = stackTop == -1 ? -1 : stackTop - 2;
                    ELispHashtable jumpTable = asHashtable(constants[lastConstantI]);
                    IntArrayList jumps = new IntArrayList(jumpTable.size());
                    jumpTable.forEach((_, v) -> {
                        int target = asInt(v);
                        jumps.add(target);
                        int targetTrackedTop = stackTops[target];
                        if (targetStackTop != -1) {
                            if (targetTrackedTop == -1) {
                                stackTops[target] = targetStackTop;
                            } else if (targetTrackedTop != targetStackTop) {
                                throw ELispSignals.invalidFunction(object);
                            }
                        }
                    });
                    switchJumpTables.add(jumps.toArray());
                    yield switchJumpTables.size() - 1;
                }
                default: {
                    if ((op & CONSTANT) == CONSTANT) {
                        ref = op & (~CONSTANT);
                        newConstantI = ref;
                    }
                    yield 0;
                }
            };
            byte length = BYTECODE_LENGTHS[Byte.toUnsignedInt(op)];
            i += length;
            if (stackDelta == Integer.MAX_VALUE) {
                stackTop = -1;
            } else if (stackTop != -1) {
                stackTop += stackDelta;
            }
            if (jumpTarget != -1) {
                int tracked = stackTops[jumpTarget];
                if (tracked == -1) {
                    stackTops[jumpTarget] = jumpTargetTop;
                } else if (tracked != jumpTargetTop) {
                    throw ELispSignals.invalidFunction(object);
                }
            }
            lastConstantI = newConstantI;
        }

        this.nodes = nodes.toArray(new Node[0]);
        this.switchJumpTables = switchJumpTables.toArray(new int[0][]);

        if (exceptionJumps.isEmpty()) {
            this.exceptionJumps = null;
            this.exceptionJumpsStackTop = null;
        } else {
            this.exceptionJumps = exceptionJumps.toArray();
            this.exceptionJumpsStackTop = new int[this.exceptionJumps.length];
            for (int i = 0; i < this.exceptionJumps.length; i++) {
                stackTop = stackTops[this.exceptionJumps[i]];
                if (stackTop == -1) {
                    throw ELispSignals.invalidFunction(object);
                }
                this.exceptionJumpsStackTop[i] = stackTop;
            }
        }
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
    public void executeVoid(VirtualFrame frame) {
        executeGeneric(frame);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        try (Bindings bindings = new Bindings()) {
            frame.setObject(0, bindings);
            return executeBodyFromBci(frame, 0, startStackTop, bindings); // NOPMD
        } catch (OsrResultException result) {
            return result.result;
        }
    }

    @Override
    public Object executeOSR(VirtualFrame osrFrame, int target, Object interpreterState) {
        InterpreterState state = (InterpreterState) interpreterState;
        Bindings bindings = (Bindings) osrFrame.getObject(0);
        return executeBodyFromBci(osrFrame, target, state.stackTop, bindings);
    }

    @HostCompilerDirectives.BytecodeInterpreterSwitch
    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    private Object executeBodyFromBci(VirtualFrame frame, int startBci, int startTop, Bindings bindings) {
        CompilerAsserts.partialEvaluationConstant(startBci);
        CompilerAsserts.partialEvaluationConstant(startTop);
        int bci = startBci;
        int top = startTop;
        ELispContext context = getContext();
        loop:
        while (bci < bytecode.length) {
            try {
                CompilerAsserts.partialEvaluationConstant(bci);
                CompilerAsserts.partialEvaluationConstant(top);
                byte op = bytecode[bci];
                int oldTop = top;
                byte stackEffect = BYTECODE_STACK_EFFECTS[Byte.toUnsignedInt(op)];
                CompilerAsserts.partialEvaluationConstant(stackEffect);
                top += stackEffect == 0x7F ? 0 : stackEffect;

                int ref;
                ELispSymbol sym;
                Object value;
                switch (op) {
                    case STACK_REF:                    // 0
                        throw ELispSignals.invalidFunction(object);
                    case STACK_REF1:                   // 1
                    case STACK_REF2:                   // 2
                    case STACK_REF3:                   // 3
                    case STACK_REF4:                   // 4
                    case STACK_REF5:                   // 5
                    case STACK_REF6:                   // 6
                    case STACK_REF7:                   // 7
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case VARREF:                       // 010
                    case VARREF1:                      // 011
                    case VARREF2:                      // 012
                    case VARREF3:                      // 013
                    case VARREF4:                      // 014
                    case VARREF5:                      // 015
                    case VARREF6:                      // 016
                    case VARREF7:                      // 017
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case VARSET:                       // 020
                    case VARSET1:                      // 021
                    case VARSET2:                      // 022
                    case VARSET3:                      // 023
                    case VARSET4:                      // 024
                    case VARSET5:                      // 025
                    case VARSET6:                      // 026
                    case VARSET7:                      // 027
                        value = get(frame, oldTop);
                        ((GlobalVariableWriteNode.GlobalVariableDirectWriteNode) nodes[indices[bci]]).execute(frame, value);
                        break;
                    case VARBIND:                      // 030
                    case VARBIND1:                     // 031
                    case VARBIND2:                     // 032
                    case VARBIND3:                     // 033
                    case VARBIND4:                     // 034
                    case VARBIND5:                     // 035
                    case VARBIND6:                     // 036
                    case VARBIND7:                     // 037
                        ref = op == VARBIND6 ? Byte.toUnsignedInt(bytecode[bci + 1]) :
                                op == VARBIND7
                                        ? Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8)
                                        : op - VARBIND;
                        bindings.bind(asSym(constants[ref]), get(frame, oldTop));
                        break;
                    case CALL:                         // 040
                    case CALL1:                        // 041
                    case CALL2:                        // 042
                    case CALL3:                        // 043
                    case CALL4:                        // 044
                    case CALL5:                        // 045
                    case CALL6:                        // 046
                    case CALL7:                        // 047
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case UNBIND:                       // 050
                    case UNBIND1:                      // 051
                    case UNBIND2:                      // 052
                    case UNBIND3:                      // 053
                    case UNBIND4:                      // 054
                    case UNBIND5:                      // 055
                    case UNBIND6:                      // 056
                    case UNBIND7:                      // 057
                        ref = op == UNBIND6 ? Byte.toUnsignedInt(bytecode[bci + 1]) :
                                op == UNBIND7
                                        ? Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8)
                                        : op - UNBIND;
                        bindings.unbind(ref);
                        break;
                    case POPHANDLER:                   // 060
                        bindings.popHandler();
                        break;
                    case PUSHCONDITIONCASE:            // 061
                    case PUSHCATCH:                    // 062
                    {
                        assert exceptionJumps != null;
                        assert exceptionJumpsStackTop != null;
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        int target = -1;
                        for (int i = 0; i < exceptionJumps.length; i++) {
                            if (exceptionJumps[i] == ref) {
                                target = i;
                            }
                        }
                        if (target == -1) {
                            throw CompilerDirectives.shouldNotReachHere();
                        }
                        CompilerAsserts.partialEvaluationConstant(target);
                        bindings.pushHandler(get(frame, oldTop), op == PUSHCATCH, target);
                        assert exceptionJumpsStackTop[target] == top + 1;
                        break;
                    }
                    case NTH:                          // 070
                        frame.setObject(top, BuiltInFns.FNth.nth(
                                asLong(get(frame, top)),
                                get(frame, top + 1)
                        ));
                        break;
                    case SYMBOLP:                      // 071
                        frame.setObject(top, BuiltInData.FSymbolp.symbolp(get(frame, top)));
                        break;
                    case CONSP:                        // 072
                        frame.setObject(top, BuiltInData.FConsp.consp(get(frame, top)));
                        break;
                    case STRINGP:                      // 073
                        frame.setObject(top, BuiltInData.FStringp.stringp(get(frame, top)));
                        break;
                    case LISTP:                        // 074
                        frame.setObject(top, BuiltInData.FListp.listp(get(frame, top)));
                        break;
                    case EQ:                           // 075
                        frame.setObject(top, BuiltInData.FEq.eq(get(frame, top), get(frame, top + 1)));
                        break;
                    case MEMQ:                         // 076
                        frame.setObject(top, BuiltInFns.FMemq.memq(get(frame, top), get(frame, top + 1)));
                        break;
                    case NOT:                          // 077
                        frame.setObject(top, BuiltInData.FNull.null_(get(frame, top)));
                        break;
                    case CAR:                          // 0100
                        frame.setObject(top, BuiltInData.FCar.car(get(frame, top)));
                        break;
                    case CDR:                          // 0101
                        frame.setObject(top, BuiltInData.FCdr.cdr(get(frame, top)));
                        break;
                    case CONS:                         // 0102
                        frame.setObject(top, BuiltInAlloc.FCons.cons(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case LIST1:                        // 0103
                        frame.setObject(top, new ELispCons(get(frame, top), false));
                        break;
                    case LIST2:                        // 0104
                        frame.setObject(top, ELispCons.listOf(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case LIST3:                        // 0105
                        frame.setObject(top, ELispCons.listOf(
                                get(frame, top),
                                get(frame, top + 1),
                                get(frame, top + 2)
                        ));
                        break;
                    case LIST4:                        // 0106
                        frame.setObject(top, ELispCons.listOf(
                                get(frame, top),
                                get(frame, top + 1),
                                get(frame, top + 2),
                                get(frame, top + 3)
                        ));
                        break;
                    case LENGTH:                       // 0107
                    case AREF:                         // 0110
                    case ASET:                         // 0111
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case SYMBOL_VALUE:                 // 0112
                        frame.setObject(top, context.getValue(asSym(get(frame, top))));
                        break;
                    case SYMBOL_FUNCTION:              // 0113
                        frame.setObject(top, context.getFunctionStorage(asSym(get(frame, top))).get());
                        break;
                    case SET:                          // 0114
                        value = get(frame, top + 1);
                        context.setValue(asSym(get(frame, top)), value);
                        frame.setObject(top, value);
                        break;
                    case FSET:                         // 0115
                        value = get(frame, top + 1);
                        sym = asSym(get(frame, top));
                        context.getFunctionStorage(sym).set(value, sym);
                        frame.setObject(top, value);
                        break;
                    case GET:                          // 0116
                        frame.setObject(top, BuiltInFns.FGet.get(get(frame, top), get(frame, top + 1)));
                        break;
                    case SUBSTRING:                    // 0117
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case CONCAT2:                      // 0120
                    case CONCAT3:                      // 0121
                    case CONCAT4:                      // 0122
                    {
                        ref = op - CONCAT2 + 2;
                        Object[] args = new Object[ref];
                        for (int i = 0; i < ref; i++) {
                            args[i] = get(frame, top + i);
                        }
                        frame.setObject(top, BuiltInFns.FConcat.concat(args));
                        break;
                    }
                    case SUB1:                         // 0123
                    case ADD1:                         // 0124
                    case EQLSIGN:                      // 0125
                    case GTR:                          // 0126
                    case LSS:                          // 0127
                    case LEQ:                          // 0130
                    case GEQ:                          // 0131
                    case DIFF:                         // 0132
                    case NEGATE:                       // 0133
                    case PLUS:                         // 0134
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case MAX:                          // 0135
                        frame.setObject(top, BuiltInData.FMax.max(get(frame, top), new Object[]{
                                get(frame, top + 1),
                        }));
                        break;
                    case MIN:                          // 0136
                        frame.setObject(top, BuiltInData.FMin.min(get(frame, top), new Object[]{
                                get(frame, top + 1),
                        }));
                        break;
                    case MULT:                         // 0137
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case POINT:                        // 0140
                        set(frame, top, BuiltInEditFns.FPoint.point());
                        break;
                    case SAVE_CURRENT_BUFFER_OBSOLETE: // 0141
                        throw new UnsupportedOperationException();
                    case GOTO_CHAR:                    // 0142
                        context.currentBuffer().setPoint(asLong(get(frame, top)));
                        frame.setObject(top, true);
                        break;
                    case INSERT:                       // 0143
                        frame.setObject(top, BuiltInEditFns.FInsert.insert(new Object[]{get(frame, top)}));
                        break;
                    case POINT_MAX:                    // 0144
                        set(frame, top, context.currentBuffer().pointMax());
                        break;
                    case POINT_MIN:                    // 0145
                        set(frame, top, context.currentBuffer().pointMin());
                        break;
                    case CHAR_AFTER:                   // 0146
                        frame.setObject(top, BuiltInEditFns.FCharAfter.charAfterBuffer(
                                get(frame, top),
                                context.currentBuffer()
                        ));
                        break;
                    case FOLLOWING_CHAR:               // 0147
                        frame.setObject(top, BuiltInEditFns.FFollowingChar.followingCharBuffer(context.currentBuffer()));
                        break;
                    case PRECEDING_CHAR:               // 0150
                        frame.setObject(top, BuiltInEditFns.FPreviousChar.previousCharBuffer(context.currentBuffer()));
                        break;
                    case CURRENT_COLUMN:               // 0151
                        frame.setObject(top, getContext().currentBuffer().getPosition().column() - 1);
                        break;
                    case INDENT_TO:                    // 0152
                        frame.setObject(top, BuiltInIndent.FIndentTo.indentTo(get(frame, top), false));
                        break;
                    case EOLP:                         // 0154
                        frame.setObject(top, BuiltInEditFns.FEolp.eolpBuffer(context.currentBuffer()));
                        break;
                    case EOBP:                         // 0155
                        frame.setObject(top, BuiltInEditFns.FEobp.eobpBuffer(context.currentBuffer()));
                        break;
                    case BOLP:                         // 0156
                        frame.setObject(top, BuiltInEditFns.FEolp.eolpBuffer(context.currentBuffer()));
                        break;
                    case BOBP:                         // 0157
                        frame.setObject(top, BuiltInEditFns.FBobp.bobpBuffer(context.currentBuffer()));
                        break;
                    case CURRENT_BUFFER:               // 0160
                        frame.setObject(top, context.currentBuffer());
                        break;
                    case SET_BUFFER:                   // 0161
                        BuiltInBuffer.FSetBuffer.setBuffer(get(frame, top));
                        break;
                    case SAVE_CURRENT_BUFFER:          // 0162
                        bindings.saveCurrentBuffer(context.currentBuffer());
                        break;
                    case INTERACTIVE_P:                // 0164
                        // obsolete?
                        throw new UnsupportedOperationException();
                    case FORWARD_CHAR:                 // 0165
                        frame.setObject(top, BuiltInCmds.FForwardChar.forwardCharBuffer(
                                get(frame, top),
                                context.currentBuffer()
                        ));
                        break;
                    case FORWARD_WORD:                 // 0166
                        frame.setObject(top, BuiltInSyntax.FForwardWord.forwardWord(get(frame, top)));
                        break;
                    case SKIP_CHARS_FORWARD:           // 0167
                        frame.setObject(top, BuiltInSyntax.FSkipCharsForward.skipChars(
                                getLanguage(),
                                context.currentBuffer(),
                                asStr(get(frame, top)),
                                get(frame, top + 1)
                        ));
                        break;
                    case SKIP_CHARS_BACKWARD:          // 0170
                        frame.setObject(top, BuiltInSyntax.FSkipCharsBackward.skipChars(
                                getLanguage(),
                                context.currentBuffer(),
                                asStr(get(frame, top)),
                                get(frame, top + 1)
                        ));
                        break;
                    case FORWARD_LINE:                 // 0171
                        frame.setObject(top, BuiltInCmds.FForwardLine.forwardLineCtx(context.currentBuffer(), get(frame, top)));
                        break;
                    case CHAR_SYNTAX:                  // 0172
                        frame.setObject(top, BuiltInSyntax.FCharSyntax.charSyntax(get(frame, top)));
                        break;
                    case BUFFER_SUBSTRING:             // 0173
                        frame.setObject(top, BuiltInEditFns.FBufferSubstring.bufferSubstringBuffer(
                                asLong(get(frame, top)),
                                asLong(get(frame, top + 1)),
                                context.currentBuffer()
                        ));
                    case DELETE_REGION:                // 0174
                        frame.setObject(top, BuiltInEditFns.FDeleteRegion.deleteRegion(
                                asLong(get(frame, top)),
                                asLong(get(frame, top + 1))
                        ));
                        break;
                    case NARROW_TO_REGION:             // 0175
                    case WIDEN:                        // 0176
                        throw new UnsupportedOperationException();
                    case END_OF_LINE:                  // 0177
                        frame.setObject(top, BuiltInCmds.FEndOfLine.endOfLineBuffer(get(frame, top), context.currentBuffer()));
                        break;
                    case CONSTANT2:                    // 0201
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        frame.setObject(top, constants[ref]);
                        break;
                    case GOTO:                         // 0202
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        bci = backEdgePoll(frame, bci, ref, top);
                        continue;
                    case GOTOIFNIL:                    // 0203
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        // It is fine to use getObject directly.
                        // getObject: (1) object: ok; (2) primitive container (long/double): ok (never nil).
                        if (isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top);
                            continue;
                        }
                        break;
                    case GOTOIFNONNIL:                 // 0204
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (!isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top);
                            continue;
                        }
                        break;
                    case GOTOIFNILELSEPOP:             // 0205
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top);
                            continue;
                        } else {
                            top--;
                            CompilerAsserts.partialEvaluationConstant(top);
                        }
                        break;
                    case GOTOIFNONNILELSEPOP:          // 0206
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (!isNil(frame.getObject(oldTop))) {
                            top--;
                            CompilerAsserts.partialEvaluationConstant(top);
                        } else {
                            bci = backEdgePoll(frame, bci, ref, top);
                            continue;
                        }
                        break;
                    case RETURN:                       // 0207
                        return top < 0 ? false : get(frame, top);
                    case DISCARD:                      // 0210
                        break;
                    case DUP:                          // 0211
                        frame.setObject(top, get(frame, oldTop));
                        break;
                    case SAVE_EXCURSION:               // 0212
                        bindings.saveExcursion(context.currentBuffer());
                        break;
                    case SAVE_WINDOW_EXCURSION:        // 0213
                        // TODO
                        bindings.saveExcursion(context.currentBuffer());
                        break;
                    case SAVE_RESTRICTION:             // 0214
                        // TODO
                    case CATCH:                        // 0215
                        throw new UnsupportedOperationException();
                    case UNWIND_PROTECT:               // 0216
                        value = get(frame, oldTop);
                        bindings.unwindProtect(value);
                        break;
                    case CONDITION_CASE:               // 0217
                    case TEMP_OUTPUT_BUFFER_SETUP:     // 0220
                    case TEMP_OUTPUT_BUFFER_SHOW:      // 0221
                        throw new UnsupportedOperationException();
                    case SET_MARKER:                   // 0223
                    {
                        ELispMarker marker = asMarker(get(frame, top));
                        value = get(frame, top + 1);
                        if (isNil(value)) {
                            marker.setBuffer(null, -1);
                        } else {
                            Object buffer = get(frame, top + 2);
                            marker.setBuffer(isNil(buffer) ? getContext().currentBuffer() : asBuffer(buffer), asLong(value));
                        }
                        break;
                    }
                    case MATCH_BEGINNING:              // 0224
                        value = BuiltInSearch.matchData(this);
                        frame.setObject(top, BuiltInFns.FNth.nth(2 * asLong(get(frame, top)), value));
                        break;
                    case MATCH_END:                    // 0225
                        value = BuiltInSearch.matchData(this);
                        frame.setObject(top, BuiltInFns.FNth.nth(2 * asLong(get(frame, top)) + 1, value));
                        break;
                    case UPCASE:                       // 0226
                    case DOWNCASE:                     // 0227
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case STRINGEQLSIGN:                // 0230
                        frame.setObject(top, BuiltInFns.FStringEqual.stringEqual(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case STRINGLSS:                    // 0231
                        frame.setObject(top, BuiltInFns.FStringLessp.stringLessp(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case EQUAL:                        // 0232
                        frame.setObject(top, BuiltInFns.FEqual.equal(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case NTHCDR:                       // 0233
                        frame.setObject(top, BuiltInFns.FNthcdr.nthcdr(
                                asLong(get(frame, top)),
                                get(frame, top + 1)
                        ));
                        break;
                    case ELT:                          // 0234
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case MEMBER:                       // 0235
                        frame.setObject(top, BuiltInFns.FMember.member(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case ASSQ:                         // 0236
                        frame.setObject(top, BuiltInFns.FAssq.assq(
                                get(frame, top),
                                get(frame, top + 1)
                        ));
                        break;
                    case NREVERSE:                     // 0237
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case SETCAR:                       // 0240
                        frame.setObject(top, BuiltInData.FSetcar.setcar(asCons(get(frame, top)), get(frame, top + 1)));
                        break;
                    case SETCDR:                       // 0241
                        frame.setObject(top, BuiltInData.FSetcdr.setcdr(asCons(get(frame, top)), get(frame, top + 1)));
                        break;
                    case CAR_SAFE:                     // 0242
                        frame.setObject(top, BuiltInData.FCarSafe.carSafe(get(frame, top)));
                        break;
                    case CDR_SAFE:                     // 0243
                        frame.setObject(top, BuiltInData.FCdrSafe.cdrSafe(get(frame, top)));
                        break;
                    case NCONC:                        // 0244
                        frame.setObject(top, BuiltInFns.FNconc.nconc2(get(frame, top), get(frame, top + 1)));
                        break;
                    case QUO:                          // 0245
                    case REM:                          // 0246
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case NUMBERP:                      // 0247
                        frame.setObject(top, BuiltInData.FNumberp.numberp(get(frame, top)));
                        break;
                    case INTEGERP:                     // 0250
                        frame.setObject(top, BuiltInData.FIntegerp.integerp(get(frame, top)));
                        break;
                    case LISTN:                        // 0257
                    {
                        int count = Byte.toUnsignedInt(bytecode[bci + 1]);
                        top -= count - 1;
                        CompilerAsserts.partialEvaluationConstant(top);
                        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
                        for (int i = 0; i < count; i++) {
                            builder.add(get(frame, top + 1));
                        }
                        frame.setObject(top, builder.build());
                        break;
                    }
                    case CONCATN:                      // 0260
                    case INSERTN:                      // 0261
                    {
                        int count = Byte.toUnsignedInt(bytecode[bci + 1]);
                        top -= count - 1;
                        CompilerAsserts.partialEvaluationConstant(top);
                        Object[] args = new Object[count];
                        for (int i = 0; i < count; i++) {
                            args[i] = get(frame, top);
                        }
                        frame.setObject(top, op == CONCATN
                                ? BuiltInFns.FConcat.concat(args)
                                : BuiltInEditFns.FInsert.insert(args));
                        break;
                    }
                    case STACK_SET:                    // 0262
                    case STACK_SET2:                   // 0263
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case DISCARDN:                     // 0266
                    {
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]);
                        if ((ref & 0x80) != 0) {
                            ref &= 0x7F;
                            frame.setObject(top - ref, get(frame, top));
                        }
                        top -= ref;
                        CompilerAsserts.partialEvaluationConstant(top);
                        break;
                    }
                    case SWITCH:                       // 0267
                        value = get(frame, top + 1);
                        ref = asInt(asHashtable(get(frame, top + 2)).get(value, -1L));
                        if (ref != -1) {
                            if (CompilerDirectives.inInterpreter()) {
                                bci = ref;
                                continue;
                            } else {
                                int[] jumpTable = switchJumpTables[indices[bci]];
                                //noinspection ForLoopReplaceableByForEach
                                for (int i = 0; i < jumpTable.length; i++) {
                                    int target = jumpTable[i];
                                    if (target == ref) {
                                        CompilerAsserts.partialEvaluationConstant(target);
                                        bci = target;
                                        continue loop;
                                    }
                                }
                            }
                        }
                        break;
                    case CONSTANT:                     // 0300
                    default:
                        if ((op & CONSTANT) == CONSTANT) {
                            ref = op & (~CONSTANT);
                            set(frame, top, constants[ref]);
                        } else {
                            throw ELispSignals.invalidFunction(object);
                        }
                        break;
                }
                byte length = BYTECODE_LENGTHS[Byte.toUnsignedInt(op)];
                CompilerAsserts.partialEvaluationConstant(length);
                int nextBci = bci + length;
                CompilerAsserts.partialEvaluationConstant(nextBci);
                bci = nextBci;
            } catch (AbstractTruffleException e) {
                if (exceptionJumps == null || exceptionJumpsStackTop == null) {
                    throw e;
                }
                int target = getHandlingTarget(e, bindings);
                if (CompilerDirectives.inInterpreter()) {
                    bci = exceptionJumps[target];
                    top = exceptionJumpsStackTop[target];
                   continue;
                } else {
                    for (int i = 0; i < exceptionJumps.length; i++) {
                        if (i == target) {
                            CompilerAsserts.partialEvaluationConstant(i);
                            bci = exceptionJumps[i];
                            top = exceptionJumpsStackTop[i];
                            CompilerAsserts.partialEvaluationConstant(bci);
                            CompilerAsserts.partialEvaluationConstant(top);
                            continue loop;
                        }
                    }
                }
                throw CompilerDirectives.shouldNotReachHere();
            }
        }
        throw ELispSignals.invalidFunction(object);
    }

    @CompilerDirectives.TruffleBoundary
    private static int getHandlingTarget(AbstractTruffleException e, Bindings bindings) {
        AbstractTruffleException rethrow = e;
        Bindings.SignalHandler handler;
        while (true) {
            try {
                handler = bindings.popHandlerTil(rethrow);
                break;
            } catch (AbstractTruffleException inner) {
                rethrow = inner;
            }
        }
        if (handler == null) {
            throw rethrow;
        }
        return handler.target;
    }

    private int backEdgePoll(VirtualFrame frame, int bci, int nextBci, int stackTop) {
        CompilerAsserts.partialEvaluationConstant(nextBci);
        CompilerAsserts.partialEvaluationConstant(stackTop);
        if (nextBci < bci) {
            if (CompilerDirectives.inInterpreter() && BytecodeOSRNode.pollOSRBackEdge(this)) {
                InterpreterState newState = new InterpreterState(stackTop);
                Object result = BytecodeOSRNode.tryOSR(this, nextBci, newState, null, frame);
                if (result != null) {
                    throw new OsrResultException(result);
                }
            }
        }
        return nextBci;
    }

    private static Object get(VirtualFrame frame, int top) {
        Object o = frame.getObject(top);
        if (o instanceof ELispFrameSlotNode.SlotPrimitiveContainer container) {
            return container.asObject();
        }
        return o;
    }
    private static void set(VirtualFrame frame, int top, Object value) {
        if (value instanceof Long || value instanceof Double) {
            Object o = frame.getObject(top);
            if (o instanceof ELispFrameSlotNode.SlotPrimitiveContainer container) {
                if (value instanceof Long l) {
                    container.setLong(l);
                } else {
                    Double d = (Double) value;
                    container.setDouble(d);
                }
                return;
            }
        }
        frame.setObject(top, value);
    }

    private static Node createUnaryNode(int top, Function<ELispExpressionNode, ELispExpressionNode> function) {
        return ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(top, null,
                function.apply(ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top, null)));
    }
    private static Node createUnaryFactoryNode(int top, Function<ELispExpressionNode[], ELispExpressionNode> function) {
        return ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(top, null,
                function.apply(new ELispExpressionNode[]{
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top, null)
                })
        );
    }
    private static Node createBinaryFactoryNode(int top, Function<ELispExpressionNode[], ELispExpressionNode> function) {
        return ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(top - 1, null,
                function.apply(new ELispExpressionNode[]{
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top - 1, null),
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top, null)
                })
        );
    }
    private static Node createBinaryNode(int top, BiFunction<ELispExpressionNode, ELispExpressionNode, ELispExpressionNode> function) {
        return ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(top - 1, null,
                function.apply(
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top - 1, null),
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top, null)
                )
        );
    }
    private static Node createTernaryFactoryNode(int top, Function<ELispExpressionNode[], ELispExpressionNode> function) {
        return ELispFrameSlotNodeFactory.ELispFrameSlotWriteNodeGen.create(top - 2, null,
                function.apply(new ELispExpressionNode[]{
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top - 2, null),
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top - 1, null),
                        ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(top, null)
                })
        );
    }


    private static final class InlinableCallNode extends ELispExpressionNode {
        private final int base;
        private final int n;

        @CompilerDirectives.CompilationFinal
        @Nullable
        private ELispSymbol symbol = null;
        @CompilerDirectives.CompilationFinal
        private Assumption stable = Assumption.ALWAYS_VALID;

        @Child
        @Nullable
        private ELispExpressionNode callNode = null;

        public InlinableCallNode(int top, int n) {
            this.base = top - n;
            this.n = n;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            try {
                updateInnerNode(frame).executeVoid(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return updateInnerNode(frame).executeLong(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
            try {
                return updateInnerNode(frame).executeDouble(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            try {
                return updateInnerNode(frame).executeGeneric(frame);
            } catch (ELispSignals.ELispSignalException | ClassCastException | UnsupportedSpecializationException e) {
                throw ELispSignals.remapException(e, this);
            }
        }

        private ELispExpressionNode updateInnerNode(VirtualFrame frame) {
            Object function = frame.getObject(base);
            ELispExpressionNode node = callNode;
            if (node != null) {
                if (stable == Assumption.NEVER_VALID // given up, use CallSomeNode
                        || (symbol == function && stable.isValid())) {
                    return node;
                }
            }

            CompilerDirectives.transferToInterpreterAndInvalidate();
            if (symbol == null && function instanceof ELispSymbol s) {
                symbol = s;
                FunctionStorage storage = getContext().getFunctionStorage(s);
                Object inner = storage.get();
                if (inner instanceof ELispSubroutine(_, ELispSubroutine.InlineInfo inline, _) && inline != null) {
                    stable = storage.getStableAssumption();
                    node = insertOrReplace(generateInlineNode(function, inline), node);
                    callNode = node;
                    return node;
                }
            }

            stable = Assumption.NEVER_VALID;
            node = insertOrReplace(createCallSomeNode(base, n), node);
            callNode = node;

            return node;
        }

        private ELispExpressionNode[] getReadSlotNodes(int from, int to) {
            ELispExpressionNode[] nodes = new ELispExpressionNode[to - from + 1];
            for (int i = from; i <= to; i++) {
                nodes[i - from] = ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(i, null);
            }
            return nodes;
        }

        private ELispExpressionNode generateInlineNode(Object f, ELispSubroutine.InlineInfo inline) {
            ELispBuiltIn info = inline.info();
            if (n < info.minArgs() || (!info.varArgs() && info.maxArgs() < n)) {
                throw ELispSignals.wrongNumberOfArguments(f, n);
            }
            if (inline.isTailored()) {
                return inline.createNode(getReadSlotNodes(base + 1, base + n));
            }
            List<ELispExpressionNode> nodes = new ArrayList<>();
            List<ELispExpressionNode> restNodes = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                ELispExpressionNode node = ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(base + 1 + i, null);
                if (nodes.size() < info.maxArgs()) {
                    nodes.add(node);
                } else {
                    restNodes.add(node);
                }
            }
            while (nodes.size() < info.maxArgs()) {
                nodes.add(ELispInterpretedNode.literal(false));
            }
            if (info.varArgs()) {
                nodes.add(new ELispInterpretedNode.VarargToArrayNode(restNodes));
            }
            ELispExpressionNode[] arguments = nodes.toArray(ELispExpressionNode[]::new);
            return inline.createNode(arguments);
        }

    }

    static ELispExpressionNode createCallSomeNode(int base, int n) {
        int argBase = base + 1;
        ELispExpressionNode[] args = new ELispExpressionNode[n + 1];
        args[0] = ELispBytecodeFallbackNodeFactory.ToFunctionObjectNodeGen.create(
                ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(argBase, null)
        );
        for (int i = 0; i < n; i++) {
            args[i + 1] = ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(argBase + i, null);
        }
        return switch (n) {
            case 0 -> ELispBytecodeFallbackNodeFactory.Call0NodeGen.create(args);
            case 1 -> ELispBytecodeFallbackNodeFactory.Call1NodeGen.create(args);
            case 2 -> ELispBytecodeFallbackNodeFactory.Call2NodeGen.create(args);
            case 3 -> ELispBytecodeFallbackNodeFactory.Call3NodeGen.create(args);
            case 4 -> ELispBytecodeFallbackNodeFactory.Call4NodeGen.create(args);
            case 5 -> ELispBytecodeFallbackNodeFactory.Call5NodeGen.create(args);
            default -> ELispBytecodeFallbackNodeFactory.CallNNodeGen.create(args);
        };
    }

    @NodeChild(value = "function", type = ELispExpressionNode.class)
    abstract static class ToFunctionObjectNode extends ELispExpressionNode {
        public ELispFunctionObject toFunction(Object o) {
            return getFunctionObject(o);
        }

        @Specialization(assumptions = "storage.getStableAssumption()", guards = "symbol == lastSymbol", limit = "2")
        public ELispFunctionObject symbolToObject(
                ELispSymbol symbol,
                @Cached("symbol") ELispSymbol lastSymbol,
                @Cached("getContext().getFunctionStorage(lastSymbol)") FunctionStorage storage,
                @Cached("toFunction(storage.get())") ELispFunctionObject o
        ) {
            return o;
        }

        @Specialization
        public ELispFunctionObject symbolToObject(ELispSymbol symbol) {
            return toFunction(getContext().getFunctionStorage(symbol).get());
        }

        @Fallback
        public ELispFunctionObject getFunctionObject(Object o) {
            return toFunction(o);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call0Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call();
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget());
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call1Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call2Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call3Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2, arg3);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2, arg3);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call4Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2, arg3, arg4);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2, arg3, arg4);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class Call5Node extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                Object arg5,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arg1, arg2, arg3, arg4, arg5);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object arg1,
                Object arg2,
                Object arg3,
                Object arg4,
                Object arg5,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), arg1, arg2, arg3, arg4, arg5);
        }
    }

    @GenerateInline(value = false)
    @NodeChild(value = "args", type = ELispExpressionNode[].class)
    abstract static class CallNNode extends ELispExpressionNode {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object[] args,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(args);
        }

        @Specialization(replaces = "dispatchDirectly")
        protected static Object dispatchIndirectly(
                ELispFunctionObject function,
                Object[] args,
                @Cached IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.callTarget(), args);
        }
    }

    private static final class InterpreterState {
        final int stackTop;

        private InterpreterState(int stackTop) {
            this.stackTop = stackTop;
        }
    }

    private static final class OsrResultException extends ControlFlowException {
        final Object result;

        private OsrResultException(Object result) {
            this.result = result;
        }
    }

    final class Bindings implements AutoCloseable {
        private final ArrayList<Object> bindings;
        private final ArrayList<SignalHandler> signalHandlers;

        Bindings() {
            this.bindings = new ArrayList<>();
            this.signalHandlers = new ArrayList<>();
        }

        void bind(ELispSymbol symbol, Object value) {
            bindings.add(ELispLexical.pushDynamic(new ELispSymbol[]{symbol}, new Object[]{value}));
        }

        void saveCurrentBuffer(Object buffer) {
            bindings.add(new SaveCurrentBuffer(buffer));
        }

        public void saveExcursion(ELispBuffer buffer) {
            bindings.add(new SaveExcursion(
                    buffer,
                    new ELispMarker(buffer, buffer.getPoint())
            ));
        }

        public void unwindProtect(Object value) {
            bindings.add(new UnwindProtect(value));
        }

        public void unbind(int count) {
            AbstractTruffleException rethrow = null;
            for (int i = 0; i < count; i++) {
                try {
                    switch (bindings.removeLast()) {
                        case ELispLexical.Dynamic d -> d.close();
                        case SaveCurrentBuffer(Object buffer) ->
                                BuiltInBuffer.FSetBuffer.setBuffer(buffer);
                        case SaveExcursion(ELispBuffer buffer, ELispMarker marker) -> {
                            BuiltInBuffer.FSetBuffer.setBuffer(buffer);
                            buffer.setPoint(marker.point());
                            marker.setBuffer(null, -1);
                        }
                        case UnwindProtect(Object function) ->
                                BuiltInEval.FFuncall.funcall(ELispBytecodeFallbackNode.this, function);
                        default -> throw new IllegalStateException();
                    }
                } catch (AbstractTruffleException e) {
                    rethrow = e;
                }
            }
            if (rethrow != null) {
                throw rethrow;
            }
        }

        public void pushHandler(Object tag, boolean catchThrow, int target) {
            signalHandlers.add(new SignalHandler(tag, catchThrow, target, bindings.size()));
        }

        public void popHandler() {
            signalHandlers.removeLast();
        }

        @Nullable
        public SignalHandler popHandlerTil(AbstractTruffleException e) {
            boolean catchThrow;
            Object tag;
            if (e instanceof ELispSignals.ELispSignalException s) {
                tag = s.getTag();
                catchThrow = false;
            } else if (e instanceof ELispSignals.ELispCatchException c) {
                tag = c.getTag();
                catchThrow = true;
            } else {
                return null;
            }
            while (!signalHandlers.isEmpty()) {
                SignalHandler last = signalHandlers.removeLast();
                if (last.catchThrow != catchThrow) {
                    continue;
                }
                if (catchThrow) {
                    if (!BuiltInData.FEq.eq(tag, last.tag)) {
                        continue;
                    }
                } else {
                    if (!BuiltInEval.FConditionCase.shouldHandle(tag, last.tag)) {
                        continue;
                    }
                }
                unbind(bindings.size() - last.bindingsSize);
                return last;
            }
            return null;
        }

        @Override
        public void close() {
            assert signalHandlers.isEmpty();
            unbind(bindings.size());
        }

        record SaveCurrentBuffer(Object buffer) {
        }
        record SaveExcursion(ELispBuffer buffer, ELispMarker marker) {
        }
        record UnwindProtect(Object function) {
        }
        record SignalHandler(Object tag, boolean catchThrow, int target, int bindingsSize) {
        }
    }
}
