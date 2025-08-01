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
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispFrameSlotReadNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispFrameSlotWriteNode;
import party.iroiro.juicemacs.elisp.runtime.*;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
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

    private final int startStackTop;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    final byte[] bytecode;

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

    final int[] stackTops;

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

    public ELispBytecodeFallbackNode(ELispBytecode bytecode, int argsOnStack) {
        CompilerAsserts.neverPartOfCompilation();
        this.startStackTop = argsOnStack - 1;
        byte[] bytes = bytecode.getBytecode();
        this.bytecode = bytes;
        // Lambdas that share code may have different constant vectors.
        // However, typically these vectors only differ in captured variables,
        // and we just assume anything else is actually "constant" across lambdas.
        Object[] constants = bytecode.getConstants();

        indices = new int[bytes.length];
        // stackTops is used to check if each PC in the function
        // has deterministic stack top positions.
        int[] stackTops = new int[bytes.length];
        // TODO
        this.stackTops = stackTops;
        Arrays.fill(stackTops, Integer.MIN_VALUE);

        IntArrayList exceptionJumps = new IntArrayList();
        ArrayList<int[]> switchJumpTables = new ArrayList<>();
        ArrayList<Node> nodes = new ArrayList<>();

        int stackTop = startStackTop;
        int lastConstantI = Integer.MIN_VALUE;
        for (int i = 0; i < bytes.length; ) {
            int otherBranchTop = stackTops[i];
            if (otherBranchTop == Integer.MIN_VALUE) {
                stackTops[i] = stackTop;
            } else {
                if (stackTop == Integer.MIN_VALUE) {
                    stackTop = otherBranchTop;
                } else if (otherBranchTop != stackTop) {
                    throw ELispSignals.invalidFunction(bytecode);
                }
            }
            byte op = bytes[i];
            int ref;
            int jumpTarget = Integer.MIN_VALUE, jumpTargetTop = Integer.MIN_VALUE;
            int stackDelta = BYTECODE_STACK_EFFECTS[Byte.toUnsignedInt(op)];
            if (stackDelta == 0x7F) {
                stackDelta = Integer.MIN_VALUE;
            }
            int newConstantI = Integer.MIN_VALUE;
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
                    nodes.add(ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(
                            stackTop + 1,
                            new ReadStackSlotNode(stackTop - ref)
                    ));
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
                    nodes.add(ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(
                            stackTop + 1,
                            GlobalVariableReadNodeGen.create(asSym(constants[ref]))
                    ));
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
                    nodes.add(ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(
                            stackTop - ref,
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
                case MAX:                          // 0135
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FMaxBinaryNodeGen::create));
                    yield nodes.size() - 1;
                case MIN:                          // 0136
                    nodes.add(createBinaryNode(stackTop, BuiltInDataFactory.FMinBinaryNodeGen::create));
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
                    if (stackTop != Integer.MIN_VALUE) {
                        jumpTarget = ref;
                        jumpTargetTop = stackTop + (elsePop ? 0 : stackDelta);
                    }
                    if (elsePop) {
                        stackDelta = -1;
                    } else if (op == GOTO) {
                        stackDelta = Integer.MIN_VALUE;
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
                    nodes.add(ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(
                            stackTop - ref,
                            new ReadStackSlotNode(stackTop)
                    ));
                    yield nodes.size() - 1;
                case DISCARDN:                     // 0266
                    ref = Byte.toUnsignedInt(bytes[i + 1]);
                    stackDelta = -(ref & 0x7F);
                    yield 0;
                case SWITCH:                       // 0267
                {
                    if (lastConstantI == Integer.MIN_VALUE) {
                        throw ELispSignals.invalidFunction(bytecode);
                    }
                    int targetStackTop = stackTop == Integer.MIN_VALUE ? Integer.MIN_VALUE : stackTop - 2;
                    ELispHashtable jumpTable = asHashtable(constants[lastConstantI]);
                    IntArrayList jumps = new IntArrayList(jumpTable.size());
                    jumpTable.forEach((_, v) -> {
                        int target = asInt(v);
                        jumps.add(target);
                        int targetTrackedTop = stackTops[target];
                        if (targetStackTop != Integer.MIN_VALUE) {
                            if (targetTrackedTop == Integer.MIN_VALUE) {
                                stackTops[target] = targetStackTop;
                            } else if (targetTrackedTop != targetStackTop) {
                                throw ELispSignals.invalidFunction(bytecode);
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
            if (stackDelta == Integer.MIN_VALUE) {
                stackTop = Integer.MIN_VALUE;
            } else if (stackTop != Integer.MIN_VALUE) {
                stackTop += stackDelta;
            }
            if (jumpTarget != Integer.MIN_VALUE) {
                int tracked = stackTops[jumpTarget];
                if (tracked == Integer.MIN_VALUE) {
                    stackTops[jumpTarget] = jumpTargetTop;
                } else if (tracked != jumpTargetTop) {
                    throw ELispSignals.invalidFunction(bytecode);
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
                if (stackTop == Integer.MIN_VALUE) {
                    throw ELispSignals.invalidFunction(bytecode);
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
            Object[] constants = ((ELispBytecode) frame.getArguments()[0]).getConstants();
            return executeBodyFromBci(frame, 0, startStackTop, constants, bindings); // NOPMD
        } catch (OsrResultException result) {
            return result.result;
        }
    }

    @Override
    public Object executeOSR(VirtualFrame osrFrame, int target, Object interpreterState) {
        InterpreterState state = (InterpreterState) interpreterState;
        return executeBodyFromBci(osrFrame, target, state.stackTop, state.constants, state.bindings);
    }

    @HostCompilerDirectives.BytecodeInterpreterSwitch
    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    private Object executeBodyFromBci(VirtualFrame frame, int startBci, int startTop, Object[] constants, Bindings bindings) {
        CompilerAsserts.partialEvaluationConstant(startBci);
        CompilerAsserts.partialEvaluationConstant(startTop);
        int bci = startBci;
        int top = startTop;
        final ELispContext context = getContext();
        loop:
        while (bci < bytecode.length) {
//            if (top != this.stackTops[bci]) {
//                throw invalidFunction(frame);
//            }
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
                        throw invalidFunction(frame);
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
                        value = frame.getObject(oldTop);
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
                        bindings.bind(asSym(constants[ref]), frame.getObject(oldTop));
                        break;
                    case CALL6:                        // 046
                    case CALL7:                        // 047
                        ref = op == CALL6 ? Byte.toUnsignedInt(bytecode[bci + 1]) :
                                Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        top -= ref;
                        // fallthrough
                    case CALL:                         // 040
                    case CALL1:                        // 041
                    case CALL2:                        // 042
                    case CALL3:                        // 043
                    case CALL4:                        // 044
                    case CALL5:                        // 045
                        // TODO
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
                        bindings.pushHandler(frame.getObject(oldTop), op == PUSHCATCH, target);
                        assert exceptionJumpsStackTop[target] == top + 1;
                        break;
                    }
                    case NTH:                          // 070
                        frame.setObject(top, BuiltInFns.FNth.nth(
                                asLong(frame.getObject(top)),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case SYMBOLP:                      // 071
                        frame.setObject(top, BuiltInData.FSymbolp.symbolp(frame.getObject(top)));
                        break;
                    case CONSP:                        // 072
                        frame.setObject(top, BuiltInData.FConsp.consp(frame.getObject(top)));
                        break;
                    case STRINGP:                      // 073
                        frame.setObject(top, BuiltInData.FStringp.stringp(frame.getObject(top)));
                        break;
                    case LISTP:                        // 074
                        frame.setObject(top, BuiltInData.FListp.listp(frame.getObject(top)));
                        break;
                    case EQ:                           // 075
                        frame.setObject(top, BuiltInData.FEq.eq(frame.getObject(top), frame.getObject(top + 1)));
                        break;
                    case MEMQ:                         // 076
                        frame.setObject(top, BuiltInFns.FMemq.memq(frame.getObject(top), frame.getObject(top + 1)));
                        break;
                    case NOT:                          // 077
                        frame.setObject(top, BuiltInData.FNull.null_(frame.getObject(top)));
                        break;
                    case CAR:                          // 0100
                        frame.setObject(top, BuiltInData.FCar.car(frame.getObject(top)));
                        break;
                    case CDR:                          // 0101
                        frame.setObject(top, BuiltInData.FCdr.cdr(frame.getObject(top)));
                        break;
                    case CONS:                         // 0102
                        frame.setObject(top, BuiltInAlloc.FCons.cons(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case LIST1:                        // 0103
                        frame.setObject(top, ELispCons.listOf(frame.getObject(top)));
                        break;
                    case LIST2:                        // 0104
                        frame.setObject(top, ELispCons.listOf(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case LIST3:                        // 0105
                        frame.setObject(top, ELispCons.listOf(
                                frame.getObject(top),
                                frame.getObject(top + 1),
                                frame.getObject(top + 2)
                        ));
                        break;
                    case LIST4:                        // 0106
                        frame.setObject(top, ELispCons.listOf(
                                frame.getObject(top),
                                frame.getObject(top + 1),
                                frame.getObject(top + 2),
                                frame.getObject(top + 3)
                        ));
                        break;
                    case LENGTH:                       // 0107
                    case AREF:                         // 0110
                    case ASET:                         // 0111
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case SYMBOL_VALUE:                 // 0112
                        frame.setObject(top, context.getValue(asSym(frame.getObject(top))));
                        break;
                    case SYMBOL_FUNCTION:              // 0113
                        frame.setObject(top, context.getFunctionStorage(asSym(frame.getObject(top))).get());
                        break;
                    case SET:                          // 0114
                        value = frame.getObject(top + 1);
                        context.setValue(asSym(frame.getObject(top)), value);
                        frame.setObject(top, value);
                        break;
                    case FSET:                         // 0115
                        value = frame.getObject(top + 1);
                        sym = asSym(frame.getObject(top));
                        context.getFunctionStorage(sym).set(value, sym);
                        frame.setObject(top, value);
                        break;
                    case GET:                          // 0116
                        frame.setObject(top, BuiltInFns.FGet.get(frame.getObject(top), frame.getObject(top + 1)));
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
                            args[i] = frame.getObject(top + i);
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
                    case MAX:                          // 0135
                    case MIN:                          // 0136
                    case MULT:                         // 0137
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case POINT:                        // 0140
                        frame.setObject(top, context.currentBuffer().getPoint());
                        break;
                    case SAVE_CURRENT_BUFFER_OBSOLETE: // 0141
                        throw new UnsupportedOperationException();
                    case GOTO_CHAR:                    // 0142
                        context.currentBuffer().setPoint(asLong(frame.getObject(top)));
                        frame.setObject(top, true);
                        break;
                    case INSERT:                       // 0143
                        frame.setObject(top, BuiltInEditFns.FInsert.insert(new Object[]{frame.getObject(top)}));
                        break;
                    case POINT_MAX:                    // 0144
                        frame.setObject(top, context.currentBuffer().pointMax());
                        break;
                    case POINT_MIN:                    // 0145
                        frame.setObject(top, context.currentBuffer().pointMin());
                        break;
                    case CHAR_AFTER:                   // 0146
                        frame.setObject(top, BuiltInEditFns.FCharAfter.charAfterBuffer(
                                frame.getObject(top),
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
                        frame.setObject(top, BuiltInIndent.FIndentTo.indentTo(frame.getObject(top), false));
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
                        BuiltInBuffer.FSetBuffer.setBuffer(frame.getObject(top));
                        break;
                    case SAVE_CURRENT_BUFFER:          // 0162
                        bindings.saveCurrentBuffer(context.currentBuffer());
                        break;
                    case INTERACTIVE_P:                // 0164
                        // obsolete?
                        throw new UnsupportedOperationException();
                    case FORWARD_CHAR:                 // 0165
                        frame.setObject(top, BuiltInCmds.FForwardChar.forwardCharBuffer(
                                frame.getObject(top),
                                context.currentBuffer()
                        ));
                        break;
                    case FORWARD_WORD:                 // 0166
                        frame.setObject(top, BuiltInSyntax.FForwardWord.forwardWord(frame.getObject(top)));
                        break;
                    case SKIP_CHARS_FORWARD:           // 0167
                        frame.setObject(top, BuiltInSyntax.FSkipCharsForward.skipChars(
                                getLanguage(),
                                context.currentBuffer(),
                                asStr(frame.getObject(top)),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case SKIP_CHARS_BACKWARD:          // 0170
                        frame.setObject(top, BuiltInSyntax.FSkipCharsBackward.skipChars(
                                getLanguage(),
                                context.currentBuffer(),
                                asStr(frame.getObject(top)),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case FORWARD_LINE:                 // 0171
                        frame.setObject(top, BuiltInCmds.FForwardLine.forwardLineCtx(context.currentBuffer(), frame.getObject(top)));
                        break;
                    case CHAR_SYNTAX:                  // 0172
                        frame.setObject(top, BuiltInSyntax.FCharSyntax.charSyntax(frame.getObject(top)));
                        break;
                    case BUFFER_SUBSTRING:             // 0173
                        frame.setObject(top, BuiltInEditFns.FBufferSubstring.bufferSubstringBuffer(
                                asLong(frame.getObject(top)),
                                asLong(frame.getObject(top + 1)),
                                context.currentBuffer()
                        ));
                        break;
                    case DELETE_REGION:                // 0174
                        frame.setObject(top, BuiltInEditFns.FDeleteRegion.deleteRegion(
                                asLong(frame.getObject(top)),
                                asLong(frame.getObject(top + 1))
                        ));
                        break;
                    case NARROW_TO_REGION:             // 0175
                    case WIDEN:                        // 0176
                        throw new UnsupportedOperationException();
                    case END_OF_LINE:                  // 0177
                        frame.setObject(top, BuiltInCmds.FEndOfLine.endOfLineBuffer(frame.getObject(top), context.currentBuffer()));
                        break;
                    case CONSTANT2:                    // 0201
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        frame.setObject(top, constants[ref]);
                        break;
                    case GOTO:                         // 0202
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        bci = backEdgePoll(frame, bci, ref, top, constants, bindings);
                        continue;
                    case GOTOIFNIL:                    // 0203
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top, constants, bindings);
                            continue;
                        }
                        break;
                    case GOTOIFNONNIL:                 // 0204
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (!isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top, constants, bindings);
                            continue;
                        }
                        break;
                    case GOTOIFNILELSEPOP:             // 0205
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top, constants, bindings);
                            continue;
                        }
                        top--;
                        CompilerAsserts.partialEvaluationConstant(top);
                        break;
                    case GOTOIFNONNILELSEPOP:          // 0206
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (!isNil(frame.getObject(oldTop))) {
                            bci = backEdgePoll(frame, bci, ref, top, constants, bindings);
                            continue;
                        }
                        top--;
                        CompilerAsserts.partialEvaluationConstant(top);
                        break;
                    case RETURN:                       // 0207
                        return top < 0 ? false : frame.getObject(top);
                    case DISCARD:                      // 0210
                        break;
                    case DUP:                          // 0211
                        frame.setObject(top, frame.getObject(oldTop));
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
                        value = frame.getObject(oldTop);
                        bindings.unwindProtect(value);
                        break;
                    case CONDITION_CASE:               // 0217
                    case TEMP_OUTPUT_BUFFER_SETUP:     // 0220
                    case TEMP_OUTPUT_BUFFER_SHOW:      // 0221
                        throw new UnsupportedOperationException();
                    case SET_MARKER:                   // 0223
                    {
                        ELispMarker marker = asMarker(frame.getObject(top));
                        value = frame.getObject(top + 1);
                        if (isNil(value)) {
                            marker.setBuffer(null, -1);
                        } else {
                            Object buffer = frame.getObject(top + 2);
                            marker.setBuffer(isNil(buffer) ? getContext().currentBuffer() : asBuffer(buffer), asLong(value));
                        }
                        break;
                    }
                    case MATCH_BEGINNING:              // 0224
                        value = BuiltInSearch.matchData(this);
                        frame.setObject(top, BuiltInFns.FNth.nth(2 * asLong(frame.getObject(top)), value));
                        break;
                    case MATCH_END:                    // 0225
                        value = BuiltInSearch.matchData(this);
                        frame.setObject(top, BuiltInFns.FNth.nth(2 * asLong(frame.getObject(top)) + 1, value));
                        break;
                    case UPCASE:                       // 0226
                    case DOWNCASE:                     // 0227
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case STRINGEQLSIGN:                // 0230
                        frame.setObject(top, BuiltInFns.FStringEqual.stringEqual(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case STRINGLSS:                    // 0231
                        frame.setObject(top, BuiltInFns.FStringLessp.stringLessp(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case EQUAL:                        // 0232
                        frame.setObject(top, BuiltInFns.FEqual.equal(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case NTHCDR:                       // 0233
                        frame.setObject(top, BuiltInFns.FNthcdr.nthcdr(
                                asLong(frame.getObject(top)),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case ELT:                          // 0234
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case MEMBER:                       // 0235
                        frame.setObject(top, BuiltInFns.FMember.member(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case ASSQ:                         // 0236
                        frame.setObject(top, BuiltInFns.FAssq.assq(
                                frame.getObject(top),
                                frame.getObject(top + 1)
                        ));
                        break;
                    case NREVERSE:                     // 0237
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case SETCAR:                       // 0240
                        frame.setObject(top, BuiltInData.FSetcar.setcar(asCons(frame.getObject(top)), frame.getObject(top + 1)));
                        break;
                    case SETCDR:                       // 0241
                        frame.setObject(top, BuiltInData.FSetcdr.setcdr(asCons(frame.getObject(top)), frame.getObject(top + 1)));
                        break;
                    case CAR_SAFE:                     // 0242
                        frame.setObject(top, BuiltInData.FCarSafe.carSafe(frame.getObject(top)));
                        break;
                    case CDR_SAFE:                     // 0243
                        frame.setObject(top, BuiltInData.FCdrSafe.cdrSafe(frame.getObject(top)));
                        break;
                    case NCONC:                        // 0244
                        frame.setObject(top, BuiltInFns.FNconc.nconc2(frame.getObject(top), frame.getObject(top + 1)));
                        break;
                    case QUO:                          // 0245
                    case REM:                          // 0246
                        ((ELispExpressionNode) nodes[indices[bci]]).executeVoid(frame);
                        break;
                    case NUMBERP:                      // 0247
                        frame.setObject(top, BuiltInData.FNumberp.numberp(frame.getObject(top)));
                        break;
                    case INTEGERP:                     // 0250
                        frame.setObject(top, BuiltInData.FIntegerp.integerp(frame.getObject(top)));
                        break;
                    case LISTN:                        // 0257
                    {
                        int count = Byte.toUnsignedInt(bytecode[bci + 1]);
                        top -= count - 1;
                        CompilerAsserts.partialEvaluationConstant(top);
                        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
                        for (int i = 0; i < count; i++) {
                            builder.add(frame.getObject(top + i));
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
                            args[i] = frame.getObject(top + i);
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
                            frame.setObject(top - ref, frame.getObject(top));
                        }
                        top -= ref;
                        CompilerAsserts.partialEvaluationConstant(top);
                        break;
                    }
                    case SWITCH:                       // 0267
                        value = frame.getObject(top + 1);
                        ref = asInt(asHashtable(frame.getObject(top + 2)).get(value, -1L));
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
                            frame.setObject(top, constants[ref]);
                        } else {
                            throw invalidFunction(frame);
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
                    frame.setObject(top, getExceptionData(e));
                   continue;
                } else {
                    for (int i = 0; i < exceptionJumps.length; i++) {
                        if (i == target) {
                            CompilerAsserts.partialEvaluationConstant(i);
                            bci = exceptionJumps[i];
                            top = exceptionJumpsStackTop[i];
                            CompilerAsserts.partialEvaluationConstant(bci);
                            CompilerAsserts.partialEvaluationConstant(top);
                            frame.setObject(top, getExceptionData(e));
                            continue loop;
                        }
                    }
                }
                throw CompilerDirectives.shouldNotReachHere();
            }
        }
        throw invalidFunction(frame);
    }

    private static ELispSignals.ELispSignalException invalidFunction(VirtualFrame frame) {
        ELispBytecode function = (ELispBytecode) frame.getArguments()[0];
        return ELispSignals.invalidFunction(function);
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

    @CompilerDirectives.TruffleBoundary
    private static Object getExceptionData(AbstractTruffleException e) {
        if (e instanceof ELispSignals.ELispSignalException signal) {
            return ELispCons.cons(signal.getTag(), signal.getData());
        } else {
            return ((ELispSignals.ELispCatchException) e).getData();
        }
    }

    private int backEdgePoll(VirtualFrame frame, int bci, int nextBci, int stackTop, Object[] constants, Bindings bindings) {
        CompilerAsserts.partialEvaluationConstant(nextBci);
        CompilerAsserts.partialEvaluationConstant(stackTop);
        if (nextBci < bci) {
            if (CompilerDirectives.inInterpreter() && BytecodeOSRNode.pollOSRBackEdge(this)) {
                InterpreterState newState = new InterpreterState(stackTop, constants, bindings);
                Object result = BytecodeOSRNode.tryOSR(this, nextBci, newState, null, frame);
                if (result != null) {
                    throw new OsrResultException(result);
                }
            }
        }
        return nextBci;
    }

    private static Node createUnaryNode(int top, Function<ELispExpressionNode, ELispExpressionNode> function) {
        return ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(top, function.apply(new ReadStackSlotNode(top)));
    }
    private static Node createUnaryFactoryNode(int top, Function<ELispExpressionNode[], ELispExpressionNode> function) {
        return ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(top, function.apply(new ELispExpressionNode[]{new ReadStackSlotNode(top)}));
    }
    private static Node createBinaryFactoryNode(int top, Function<ELispExpressionNode[], ELispExpressionNode> function) {
        return ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(top - 1,
                function.apply(new ELispExpressionNode[]{
                        new ReadStackSlotNode(top - 1),
                        new ReadStackSlotNode(top)
                })
        );
    }
    private static Node createBinaryNode(int top, BiFunction<ELispExpressionNode, ELispExpressionNode, ELispExpressionNode> function) {
        return ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(top - 1,
                function.apply(
                        new ReadStackSlotNode(top - 1),
                        new ReadStackSlotNode(top)
                )
        );
    }
    private static Node createTernaryFactoryNode(int top, Function<ELispExpressionNode[], ELispExpressionNode> function) {
        return ELispBytecodeFallbackNodeFactory.WriteStackSlotNodeGen.create(top - 2,
                function.apply(new ELispExpressionNode[]{
                        new ReadStackSlotNode(top - 2),
                        new ReadStackSlotNode(top - 1),
                        new ReadStackSlotNode(top)
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
            if (symbol == null && toSym(function) instanceof ELispSymbol s) {
                symbol = s;
                FunctionStorage storage = getContext().getFunctionStorage(s);
                Object inner = storage.get();
                if (inner instanceof ELispSubroutine subroutine && subroutine.inlinable() && !subroutine.specialForm()) {
                    stable = storage.getStableAssumption();
                    node = insertOrReplace(generateInlineNode(function, subroutine), node);
                    callNode = node; // NOPMD: insertOrReplace called
                    return node;
                }
            }

            stable = Assumption.NEVER_VALID;
            node = insertOrReplace(createCallSomeNode(base, n), node);
            callNode = node; // NOPMD: insertOrReplace called
            return node;
        }

        private static ELispExpressionNode[] getReadSlotNodes(int from, int to) {
            ELispExpressionNode[] nodes = new ELispExpressionNode[to - from + 1];
            for (int i = from; i <= to; i++) {
                nodes[i - from] = new ReadStackSlotNode(i);
            }
            return nodes;
        }

        private ELispExpressionNode generateInlineNode(Object f, ELispSubroutine inline) {
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
                ELispExpressionNode node = new ReadStackSlotNode(base + 1 + i);
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
        args[0] = new ReadStackSlotNode(base);
        for (int i = 0; i < n; i++) {
            args[i + 1] = new ReadStackSlotNode(argBase + i);
        }
        return FuncallDispatchNode.createSpecializedCallNode(args);
    }

    /// Reads values from [VirtualFrame]
    ///
    /// ## Why don't use `ELispFrameSlotRead/WriteNode`
    ///
    /// In node-based Lisp interpreter ([ELispInterpretedNode]), we rely on two nodes
    /// ([ELispFrameSlotReadNode] and
    /// [ELispFrameSlotWriteNode])
    /// to read from/write to stack frames. They speculate about stack slot types
    /// (long or double) to reduce GC pressure (reducing primitive boxing costs).
    ///
    /// However, we choose to differ in this bytecode interpreter because a primitive
    /// container will not save too much boxing in this case: bytecodes reuse stack
    /// slots, and if the slot of a primitive overlaps with a non-primitive (which
    /// is very likely in bytecodes), we still need boxing & unboxing.
    static class ReadStackSlotNode extends ELispExpressionNode {
        final int i;
        ReadStackSlotNode(int i) {
            this.i = i;
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return frame.getObject(i);
        }
    }
    /// Writes value produced by [#supplier] to the stack
    ///
    /// See [ReadStackSlotNode] for comments.
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    abstract static class WriteStackSlotNode extends ELispExpressionNode {
        final int i;
        WriteStackSlotNode(int i) {
            this.i = i;
        }
        @Specialization
        public Object write(VirtualFrame frame, Object value) {
            frame.setObject(i, value);
            return value;
        }
    }

    private record InterpreterState(int stackTop, Object[] constants, Bindings bindings) {
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
            bindings.add(Dynamic.pushDynamic(symbol, value));
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
                        case Dynamic d -> d.close();
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
