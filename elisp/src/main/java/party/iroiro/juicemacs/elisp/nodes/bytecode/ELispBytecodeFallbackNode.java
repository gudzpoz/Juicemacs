package party.iroiro.juicemacs.elisp.nodes.bytecode;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.HostCompilerDirectives;
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInEval.FFuncall.getFunctionObject;
import static party.iroiro.juicemacs.elisp.nodes.bytecode.ByteCode.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.SEQUENCEP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// A fallback implementation for Emacs bytecode
///
/// We hope to eventually implement a bytecode interpreter with Truffle DSL
/// ([com.oracle.truffle.api.bytecode.GenerateBytecode]). However, this DSL
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
    byte[] bytecode;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    Object[] constants;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    int[] jumps;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    int[] jumpsStackTop;

    @Children
    Node[] bytecodeNodes;
    @CompilerDirectives.CompilationFinal
    private Object osrMetadata;

    public ELispBytecodeFallbackNode(ELispBytecode bytecode, int startStackTop) {
        this.object = bytecode;
        this.startStackTop = startStackTop;
        byte[] bytes = bytecode.getBytecode();
        this.bytecode = bytes;
        this.constants = bytecode.getConstants();
        this.bytecodeNodes = new Node[bytes.length];

        IntArrayList jumps = new IntArrayList();
        int[] stackTops = new int[bytes.length];
        Arrays.fill(stackTops, -1);

        int stackTop = startStackTop;
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
            bytecodeNodes[i] = switch (op) {
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
                    yield GlobalVariableReadNodeGen.create(asSym(constants[ref]));
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
                    yield GlobalVariableWriteNodeGen.GlobalVariableDirectWriteNodeGen.create(asSym(constants[ref]));
                case CALL:                         // 040
                    yield ELispBytecodeFallbackNodeFactory.Call0NodeGen.create();
                case CALL1:                        // 041
                    yield ELispBytecodeFallbackNodeFactory.Call1NodeGen.create();
                case CALL2:                        // 042
                    yield ELispBytecodeFallbackNodeFactory.Call2NodeGen.create();
                case CALL3:                        // 043
                    yield ELispBytecodeFallbackNodeFactory.Call3NodeGen.create();
                case CALL4:                        // 044
                    yield ELispBytecodeFallbackNodeFactory.Call4NodeGen.create();
                case CALL5:                        // 045
                    yield ELispBytecodeFallbackNodeFactory.Call5NodeGen.create();
                case CALL6:                        // 046
                case CALL7:                        // 047
                    ref = op == CALL6 ? Byte.toUnsignedInt(bytes[i + 1]) :
                            Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 1]) << 8);
                    stackDelta = -ref + 1;
                    yield ELispBytecodeFallbackNodeFactory.CallNNodeGen.create();
                case PUSHCONDITIONCASE:            // 061
                case PUSHCATCH:                    // 062
                    ref = Byte.toUnsignedInt(bytes[i + 1]) + (Byte.toUnsignedInt(bytes[i + 2]) << 8);
                    jumps.add(ref);
                    jumpTarget = ref;
                    jumpTargetTop = stackTop;
                    yield null;
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
                    yield null;
                case LISTN:                        // 0257
                case CONCATN:                      // 0260
                case INSERTN:                      // 0261
                    stackDelta = -Byte.toUnsignedInt(bytes[i + 1]) + 1;
                    yield null;
                case DISCARDN:                     // 0266
                    ref = Byte.toUnsignedInt(bytes[i + 1]);
                    stackDelta = -(ref & 0x7F);
                    yield null;
                default:
                    yield null;
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
                } else {
                    if (tracked != jumpTargetTop) {
                        throw ELispSignals.invalidFunction(object);
                    }
                }
            }
        }
        this.jumps = jumps.toArray();

        this.jumpsStackTop = new int[this.jumps.length];
        for (int i = 0; i < jumpsStackTop.length; i++) {
            stackTop = stackTops[this.jumps[i]];
            if (stackTop == -1) {
                throw ELispSignals.invalidFunction(object);
            }
            this.jumpsStackTop[i] = stackTop;
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
            return executeOSR(frame, 0, startStackTop); // NOPMD
        }
    }

    @HostCompilerDirectives.BytecodeInterpreterSwitch
    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
    @Override
    public Object executeOSR(VirtualFrame frame, int bci, Object interpreterState) {
        int stackTop = (Integer) interpreterState;
        ELispContext context = getContext();
        Bindings bindings = (Bindings) frame.getObject(0);
        loop:
        while (true) {
            try {
                byte op = bytecode[bci];
                int nextBci = bci + BYTECODE_LENGTHS[Byte.toUnsignedInt(op)];
                CompilerAsserts.partialEvaluationConstant(bci);
                CompilerAsserts.partialEvaluationConstant(op);
                CompilerAsserts.partialEvaluationConstant(stackTop);

                int oldStackTop = stackTop;
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
                        ref = op == STACK_REF6 ? Byte.toUnsignedInt(bytecode[bci + 1]) :
                                op == STACK_REF7
                                        ? Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8)
                                        : op - STACK_REF;
                        frame.setObject(++stackTop, frame.getObject(oldStackTop - ref));
                        break;
                    case VARREF:                       // 010
                    case VARREF1:                      // 011
                    case VARREF2:                      // 012
                    case VARREF3:                      // 013
                    case VARREF4:                      // 014
                    case VARREF5:                      // 015
                    case VARREF6:                      // 016
                    case VARREF7:                      // 017
                        frame.setObject(++stackTop, ((GlobalVariableReadNode) bytecodeNodes[bci]).executeGeneric(frame));
                        break;
                    case VARSET:                       // 020
                    case VARSET1:                      // 021
                    case VARSET2:                      // 022
                    case VARSET3:                      // 023
                    case VARSET4:                      // 024
                    case VARSET5:                      // 025
                    case VARSET6:                      // 026
                    case VARSET7:                      // 027
                        value = frame.getObject(stackTop--);
                        ((GlobalVariableWriteNode.GlobalVariableDirectWriteNode) bytecodeNodes[bci]).execute(frame, value);
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
                        bindings.bind(asSym(constants[ref]), frame.getObject(stackTop--));
                        break;
                    case CALL:                         // 040
                        frame.setObject(
                                stackTop,
                                ((Call0Node) bytecodeNodes[bci])
                                        .execute(getFunctionObject(frame.getObject(stackTop)))
                        );
                        break;
                    case CALL1:                        // 041
                        stackTop -= 1;
                        frame.setObject(
                                stackTop,
                                ((Call1Node) bytecodeNodes[bci])
                                        .execute(
                                                getFunctionObject(frame.getObject(stackTop)),
                                                frame.getObject(stackTop + 1)
                                        )
                        );
                        break;
                    case CALL2:                        // 042
                        stackTop -= 2;
                        frame.setObject(
                                stackTop,
                                ((Call2Node) bytecodeNodes[bci])
                                        .execute(
                                                getFunctionObject(frame.getObject(stackTop)),
                                                frame.getObject(stackTop + 1),
                                                frame.getObject(stackTop + 2)
                                        )
                        );
                        break;
                    case CALL3:                        // 043
                        stackTop -= 3;
                        frame.setObject(
                                stackTop,
                                ((Call3Node) bytecodeNodes[bci])
                                        .execute(
                                                getFunctionObject(frame.getObject(stackTop)),
                                                frame.getObject(stackTop + 1),
                                                frame.getObject(stackTop + 2),
                                                frame.getObject(stackTop + 3)
                                        )
                        );
                        break;
                    case CALL4:                        // 044
                        stackTop -= 4;
                        frame.setObject(
                                stackTop,
                                ((Call4Node) bytecodeNodes[bci])
                                        .execute(
                                                getFunctionObject(frame.getObject(stackTop)),
                                                frame.getObject(stackTop + 1),
                                                frame.getObject(stackTop + 2),
                                                frame.getObject(stackTop + 3),
                                                frame.getObject(stackTop + 4)
                                        )
                        );
                        break;
                    case CALL5:                        // 045
                        stackTop -= 5;
                        frame.setObject(
                                stackTop,
                                ((Call5Node) bytecodeNodes[bci])
                                        .execute(
                                                getFunctionObject(frame.getObject(stackTop)),
                                                frame.getObject(stackTop + 1),
                                                frame.getObject(stackTop + 2),
                                                frame.getObject(stackTop + 3),
                                                frame.getObject(stackTop + 4),
                                                frame.getObject(stackTop + 5)
                                        )
                        );
                        break;
                    case CALL6:                        // 046
                    case CALL7:                        // 047
                    {
                        int count = op == CALL6 ? Byte.toUnsignedInt(bytecode[bci + 1]) :
                                Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 1]) << 8);
                        stackTop -= count;
                        Object[] args = new Object[count];
                        for (int i = 1; i <= count; i++) {
                            args[i] = frame.getObject(stackTop + count);
                        }
                        frame.setObject(
                                stackTop,
                                ((CallNNode) bytecodeNodes[bci])
                                        .execute(getFunctionObject(frame.getObject(stackTop)), args)
                        );
                        break;
                    }
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
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        int target = -1;
                        for (int i = 0; i < jumps.length; i++) {
                            if (jumps[i] == ref) {
                                target = i;
                            }
                        }
                        if (target == -1) {
                            throw CompilerDirectives.shouldNotReachHere();
                        }
                        CompilerAsserts.partialEvaluationConstant(target);
                        bindings.pushHandler(frame.getObject(stackTop--), op == PUSHCATCH, target);
                        assert jumpsStackTop[target] == stackTop + 1;
                        break;
                    }
                    case NTH:                          // 070
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FNth.nth(
                                asLong(frame.getObject(stackTop)),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case SYMBOLP:                      // 071
                        frame.setObject(stackTop, BuiltInData.FSymbolp.symbolp(frame.getObject(stackTop)));
                        break;
                    case CONSP:                        // 072
                        frame.setObject(stackTop, BuiltInData.FConsp.consp(frame.getObject(stackTop)));
                        break;
                    case STRINGP:                      // 073
                        frame.setObject(stackTop, BuiltInData.FStringp.stringp(frame.getObject(stackTop)));
                        break;
                    case LISTP:                        // 074
                        frame.setObject(stackTop, BuiltInData.FListp.listp(frame.getObject(stackTop)));
                        break;
                    case EQ:                           // 075
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FEq.eq(frame.getObject(stackTop), frame.getObject(stackTop + 1)));
                        break;
                    case MEMQ:                         // 076
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FMemq.memq(frame.getObject(stackTop), frame.getObject(stackTop + 1)));
                        break;
                    case NOT:                          // 077
                        frame.setObject(stackTop, BuiltInData.FNull.null_(frame.getObject(stackTop)));
                        break;
                    case CAR:                          // 0100
                        frame.setObject(stackTop, BuiltInData.FCar.car(frame.getObject(stackTop)));
                        break;
                    case CDR:                          // 0101
                        frame.setObject(stackTop, BuiltInData.FCdr.cdr(frame.getObject(stackTop)));
                        break;
                    case CONS:                         // 0102
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInAlloc.FCons.cons(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case LIST1:                        // 0103
                        frame.setObject(stackTop, new ELispCons(frame.getObject(stackTop), false));
                        break;
                    case LIST2:                        // 0104
                        stackTop -= 1;
                        frame.setObject(stackTop, ELispCons.listOf(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case LIST3:                        // 0105
                        stackTop -= 2;
                        frame.setObject(stackTop, ELispCons.listOf(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1),
                                frame.getObject(stackTop + 2)
                        ));
                        break;
                    case LIST4:                        // 0106
                        stackTop -= 3;
                        frame.setObject(stackTop, ELispCons.listOf(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1),
                                frame.getObject(stackTop + 2),
                                frame.getObject(stackTop + 3)
                        ));
                        break;
                    case LENGTH:                       // 0107
                        value = frame.getObject(stackTop);
                        frame.setObject(stackTop, switch (value) {
                            case ELispString s -> BuiltInFns.FLength.lengthString(s);
                            case List<?> list -> (long) list.size();
                            default -> {
                                if (isNil(value)) {
                                    yield 0L;
                                }
                                throw ELispSignals.wrongTypeArgument(SEQUENCEP, value);
                            }
                        });
                        break;
                    case AREF:                         // 0110
                        stackTop -= 1;
                        value = frame.getObject(stackTop);
                        ref = asInt(frame.getObject(stackTop + 1));
                        frame.setObject(stackTop, BuiltInData.FAref.aref(value, ref));
                        break;
                    case ASET:                         // 0111
                        stackTop -= 2;
                        ref = asInt(frame.getObject(stackTop + 1));
                        value = frame.getObject(stackTop + 2);
                        switch (frame.getObject(stackTop)) {
                            case ELispVector list -> BuiltInData.FAset.aset(list, ref, value);
                            case ELispRecord record -> BuiltInData.FAset.asetRecord(record, ref, value);
                            case ELispCharTable table -> BuiltInData.FAset.asetCharTable(table, ref, value);
                            default -> throw ELispSignals.wrongTypeArgument(SEQUENCEP, value);
                        }
                        frame.setObject(stackTop, value);
                        break;
                    case SYMBOL_VALUE:                 // 0112
                        frame.setObject(stackTop, context.getValue(asSym(frame.getObject(stackTop))));
                        break;
                    case SYMBOL_FUNCTION:              // 0113
                        frame.setObject(stackTop, context.getFunctionStorage(asSym(frame.getObject(stackTop))).get());
                        break;
                    case SET:                          // 0114
                        stackTop -= 1;
                        value = frame.getObject(stackTop + 1);
                        context.setValue(asSym(frame.getObject(stackTop)), value);
                        frame.setObject(stackTop, value);
                        break;
                    case FSET:                         // 0115
                        stackTop -= 1;
                        value = frame.getObject(stackTop + 1);
                        sym = asSym(frame.getObject(stackTop));
                        context.getFunctionStorage(sym).set(value, sym);
                        frame.setObject(stackTop, value);
                        break;
                    case GET:                          // 0116
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FGet.get(frame.getObject(stackTop), frame.getObject(stackTop + 1)));
                        break;
                    case SUBSTRING:                    // 0117
                    {
                        stackTop -= 2;
                        value = frame.getObject(stackTop);
                        Object start = frame.getObject(stackTop + 1);
                        Object end = frame.getObject(stackTop + 2);
                        frame.setObject(stackTop, value instanceof ELispString s
                                ? BuiltInFns.FSubstring.substring(s, start, end)
                                : BuiltInFns.FSubstring.substringVector(asVector(value), start, end));
                    }
                    case CONCAT2:                      // 0120
                    case CONCAT3:                      // 0121
                    case CONCAT4:                      // 0122
                    {
                        ref = op - CONCAT2 + 2;
                        stackTop -= ref - 1;
                        Object[] args = new Object[ref];
                        for (int i = 0; i < ref; i++) {
                            args[i] = frame.getObject(stackTop + i);
                        }
                        frame.setObject(stackTop, BuiltInFns.FConcat.concat(args));
                        break;
                    }
                    case SUB1:                         // 0123
                        frame.setObject(stackTop, BuiltInData.FSub1.sub1(frame.getObject(stackTop)));
                        break;
                    case ADD1:                         // 0124
                        frame.setObject(stackTop, BuiltInData.FAdd1.add1(frame.getObject(stackTop)));
                        break;
                    case EQLSIGN:                      // 0125
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.compareTo(frame.getObject(stackTop), frame.getObject(stackTop + 1)) == 0);
                        break;
                    case GTR:                          // 0126
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.compareTo(frame.getObject(stackTop), frame.getObject(stackTop + 1)) > 0);
                        break;
                    case LSS:                          // 0127
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.compareTo(frame.getObject(stackTop), frame.getObject(stackTop + 1)) < 0);
                        break;
                    case LEQ:                          // 0130
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.compareTo(frame.getObject(stackTop), frame.getObject(stackTop + 1)) <= 0);
                        break;
                    case GEQ:                          // 0131
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.compareTo(frame.getObject(stackTop), frame.getObject(stackTop + 1)) >= 0);
                        break;
                    case DIFF:                         // 0132
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FMinus.minusAny(new Object[]{
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1),
                        }));
                        break;
                    case NEGATE:                       // 0133
                        frame.setObject(stackTop, BuiltInData.FMinus.minusAny(new Object[]{frame.getObject(stackTop)}));
                        break;
                    case PLUS:                         // 0134
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FPlus.plusAny(new Object[]{
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1),
                        }));
                        break;
                    case MAX:                          // 0135
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FMax.max(frame.getObject(stackTop), new Object[]{
                                frame.getObject(stackTop + 1),
                        }));
                        break;
                    case MIN:                          // 0136
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FMin.min(frame.getObject(stackTop), new Object[]{
                                frame.getObject(stackTop + 1),
                        }));
                        break;
                    case MULT:                         // 0137
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FTimes.timesAny(new Object[]{
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1),
                        }));
                        break;
                    case POINT:                        // 0140
                        frame.setObject(++stackTop, BuiltInEditFns.FPoint.point());
                        break;
                    case SAVE_CURRENT_BUFFER_OBSOLETE: // 0141
                        throw new UnsupportedOperationException();
                    case GOTO_CHAR:                    // 0142
                        context.currentBuffer().setPoint(asLong(frame.getObject(stackTop)));
                        frame.setObject(stackTop, true);
                        break;
                    case INSERT:                       // 0143
                        frame.setObject(stackTop, BuiltInEditFns.FInsert.insert(new Object[]{frame.getObject(stackTop)}));
                        break;
                    case POINT_MAX:                    // 0144
                        frame.setObject(++stackTop, context.currentBuffer().pointMax());
                        break;
                    case POINT_MIN:                    // 0145
                        frame.setObject(++stackTop, context.currentBuffer().pointMin());
                        break;
                    case CHAR_AFTER:                   // 0146
                        frame.setObject(stackTop, BuiltInEditFns.FCharAfter.charAfterBuffer(
                                frame.getObject(stackTop),
                                context.currentBuffer()
                        ));
                        break;
                    case FOLLOWING_CHAR:               // 0147
                        frame.setObject(++stackTop, BuiltInEditFns.FFollowingChar.followingCharBuffer(context.currentBuffer()));
                        break;
                    case PRECEDING_CHAR:               // 0150
                        frame.setObject(++stackTop, BuiltInEditFns.FPreviousChar.previousCharBuffer(context.currentBuffer()));
                        break;
                    case CURRENT_COLUMN:               // 0151
                        frame.setObject(++stackTop, getContext().currentBuffer().getPosition().column() - 1);
                        break;
                    case INDENT_TO:                    // 0152
                        frame.setObject(stackTop, BuiltInIndent.FIndentTo.indentTo(frame.getObject(stackTop), false));
                        break;
                    case EOLP:                         // 0154
                        frame.setObject(++stackTop, BuiltInEditFns.FEolp.eolpBuffer(context.currentBuffer()));
                        break;
                    case EOBP:                         // 0155
                        frame.setObject(++stackTop, BuiltInEditFns.FEobp.eobpBuffer(context.currentBuffer()));
                        break;
                    case BOLP:                         // 0156
                        frame.setObject(++stackTop, BuiltInEditFns.FEolp.eolpBuffer(context.currentBuffer()));
                        break;
                    case BOBP:                         // 0157
                        frame.setObject(++stackTop, BuiltInEditFns.FBobp.bobpBuffer(context.currentBuffer()));
                        break;
                    case CURRENT_BUFFER:               // 0160
                        frame.setObject(++stackTop, context.currentBuffer());
                        break;
                    case SET_BUFFER:                   // 0161
                        BuiltInBuffer.FSetBuffer.setBuffer(frame.getObject(stackTop));
                        break;
                    case SAVE_CURRENT_BUFFER:          // 0162
                        bindings.saveCurrentBuffer(context.currentBuffer());
                        break;
                    case INTERACTIVE_P:                // 0164
                        // obsolete?
                        throw new UnsupportedOperationException();
                    case FORWARD_CHAR:                 // 0165
                        frame.setObject(stackTop, BuiltInCmds.FForwardChar.forwardCharBuffer(
                                frame.getObject(stackTop),
                                context.currentBuffer()
                        ));
                        break;
                    case FORWARD_WORD:                 // 0166
                        frame.setObject(stackTop, BuiltInSyntax.FForwardWord.forwardWord(frame.getObject(stackTop)));
                        break;
                    case SKIP_CHARS_FORWARD:           // 0167
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInSyntax.FSkipCharsForward.skipChars(
                                getLanguage(),
                                context.currentBuffer(),
                                asStr(frame.getObject(stackTop)),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case SKIP_CHARS_BACKWARD:          // 0170
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInSyntax.FSkipCharsBackward.skipChars(
                                getLanguage(),
                                context.currentBuffer(),
                                asStr(frame.getObject(stackTop)),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case FORWARD_LINE:                 // 0171
                        frame.setObject(stackTop, BuiltInCmds.FForwardLine.forwardLineCtx(context.currentBuffer(), frame.getObject(stackTop)));
                        break;
                    case CHAR_SYNTAX:                  // 0172
                        frame.setObject(stackTop, BuiltInSyntax.FCharSyntax.charSyntax(frame.getObject(stackTop)));
                        break;
                    case BUFFER_SUBSTRING:             // 0173
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInEditFns.FBufferSubstring.bufferSubstringBuffer(
                                asLong(frame.getObject(stackTop)),
                                asLong(frame.getObject(stackTop + 1)),
                                context.currentBuffer()
                        ));
                    case DELETE_REGION:                // 0174
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInEditFns.FDeleteRegion.deleteRegion(
                                asLong(frame.getObject(stackTop)),
                                asLong(frame.getObject(stackTop + 1))
                        ));
                        break;
                    case NARROW_TO_REGION:             // 0175
                    case WIDEN:                        // 0176
                        throw new UnsupportedOperationException();
                    case END_OF_LINE:                  // 0177
                        frame.setObject(stackTop, BuiltInCmds.FEndOfLine.endOfLineBuffer(frame.getObject(stackTop), context.currentBuffer()));
                        break;
                    case CONSTANT2:                    // 0201
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        frame.setObject(++stackTop, constants[ref]);
                        break;
                    case GOTO:                         // 0202
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        nextBci = ref;
                        break;
                    case GOTOIFNIL:                    // 0203
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (isNil(frame.getObject(stackTop--))) {
                            nextBci = ref;
                        }
                        break;
                    case GOTOIFNONNIL:                 // 0204
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (!isNil(frame.getObject(stackTop--))) {
                            nextBci = ref;
                        }
                        break;
                    case GOTOIFNILELSEPOP:             // 0205
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (isNil(frame.getObject(stackTop))) {
                            nextBci = ref;
                        } else {
                            stackTop--;
                        }
                        break;
                    case GOTOIFNONNILELSEPOP:          // 0206
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        if (isNil(frame.getObject(stackTop))) {
                            stackTop--;
                        } else {
                            nextBci = ref;
                        }
                        break;
                    case RETURN:                       // 0207
                        return stackTop == 0 ? false : frame.getObject(stackTop);
                    case DISCARD:                      // 0210
                        stackTop--;
                        break;
                    case DUP:                          // 0211
                        frame.setObject(++stackTop, frame.getObject(oldStackTop));
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
                        value = frame.getObject(stackTop--);
                        bindings.unwindProtect(value);
                        break;
                    case CONDITION_CASE:               // 0217
                    case TEMP_OUTPUT_BUFFER_SETUP:     // 0220
                    case TEMP_OUTPUT_BUFFER_SHOW:      // 0221
                        throw new UnsupportedOperationException();
                    case SET_MARKER:                   // 0223
                    {
                        stackTop -= 2;
                        ELispMarker marker = asMarker(frame.getObject(stackTop));
                        value = frame.getObject(stackTop + 1);
                        if (isNil(value)) {
                            marker.setBuffer(null, -1);
                        } else {
                            Object buffer = frame.getObject(stackTop + 2);
                            marker.setBuffer(isNil(buffer) ? getContext().currentBuffer() : asBuffer(buffer), asLong(value));
                        }
                        break;
                    }
                    case MATCH_BEGINNING:              // 0224
                        value = BuiltInSearch.matchData(this);
                        frame.setObject(stackTop, BuiltInFns.FNth.nth(2 * asLong(frame.getObject(stackTop)), value));
                        break;
                    case MATCH_END:                    // 0225
                        value = BuiltInSearch.matchData(this);
                        frame.setObject(stackTop, BuiltInFns.FNth.nth(2 * asLong(frame.getObject(stackTop)) + 1, value));
                        break;
                    case UPCASE:                       // 0226
                        // TODO: use node
                        value = frame.getObject(stackTop);
                        frame.setObject(stackTop, value instanceof Long l
                                ? BuiltInCaseFiddle.FUpcase.upcaseChar(l)
                                : BuiltInCaseFiddle.FUpcase.upcaseString(asStr(value))
                        );
                        break;
                    case DOWNCASE:                     // 0227
                        // TODO: use node
                        value = frame.getObject(stackTop);
                        frame.setObject(stackTop, value instanceof Long l
                                ? BuiltInCaseFiddle.FDowncase.downcaseChar(l)
                                : BuiltInCaseFiddle.FDowncase.downcaseString(asStr(value))
                        );
                        break;
                    case STRINGEQLSIGN:                // 0230
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FStringEqual.stringEqual(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case STRINGLSS:                    // 0231
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FStringLessp.stringLessp(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case EQUAL:                        // 0232
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FEqual.equal(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case NTHCDR:                       // 0233
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FNthcdr.nthcdr(
                                asLong(frame.getObject(stackTop)),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case ELT:                          // 0234
                    {
                        // TODO: use node
                        stackTop -= 1;
                        value = frame.getObject(stackTop);
                        long n = asLong(frame.getObject(stackTop + 1));
                        frame.setObject(stackTop, isNil(value) ? false : switch (value) {
                            case ELispCons cons -> BuiltInFns.FElt.elt(cons, n);
                            case ELispVector v -> BuiltInFns.FElt.eltVec(v, n);
                            case ELispCharTable table -> BuiltInFns.FElt.eltCharTable(table, n);
                            default -> throw ELispSignals.wrongTypeArgument(SEQUENCEP, value);
                        });
                        break;
                    }
                    case MEMBER:                       // 0235
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FMember.member(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case ASSQ:                         // 0236
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FAssq.assq(
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1)
                        ));
                        break;
                    case NREVERSE:                     // 0237
                        // TODO: use node
                        value = frame.getObject(stackTop);
                        frame.setObject(stackTop, isNil(value) ? false : switch (value) {
                            case ELispCons cons -> BuiltInFns.FNreverse.nreverseList(cons);
                            case ELispString s -> BuiltInFns.FNreverse.nreverseString(s);
                            case ELispVector v -> BuiltInFns.FNreverse.nreverseVec(v);
                            case ELispBoolVector boolVector -> BuiltInFns.FNreverse.nreverseBoolVec(boolVector);
                            default -> throw ELispSignals.wrongTypeArgument(SEQUENCEP, value);
                        });
                        break;
                    case SETCAR:                       // 0240
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FSetcar.setcar(asCons(frame.getObject(stackTop)), frame.getObject(stackTop + 1)));
                        break;
                    case SETCDR:                       // 0241
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FSetcdr.setcdr(asCons(frame.getObject(stackTop)), frame.getObject(stackTop + 1)));
                        break;
                    case CAR_SAFE:                     // 0242
                        frame.setObject(stackTop, BuiltInData.FCarSafe.carSafe(frame.getObject(stackTop)));
                        break;
                    case CDR_SAFE:                     // 0243
                        frame.setObject(stackTop, BuiltInData.FCdrSafe.cdrSafe(frame.getObject(stackTop)));
                        break;
                    case NCONC:                        // 0244
                        // TODO: use node
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInFns.FNconc.nconc(new Object[]{
                                frame.getObject(stackTop),
                                frame.getObject(stackTop + 1),
                        }));
                        break;
                    case QUO:                          // 0245
                        // TODO: use node
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FQuo.quoAny(
                                frame.getObject(stackTop),
                                new Object[]{frame.getObject(stackTop + 1)}
                        ));
                        break;
                    case REM:                          // 0246
                        // TODO: use node, also bigint
                        stackTop -= 1;
                        frame.setObject(stackTop, BuiltInData.FRem.remLong(
                                asLong(frame.getObject(stackTop)),
                                asLong(frame.getObject(stackTop + 1))
                        ));
                        break;
                    case NUMBERP:                      // 0247
                        frame.setObject(stackTop, BuiltInData.FNumberp.numberp(frame.getObject(stackTop)));
                        break;
                    case INTEGERP:                     // 0250
                        frame.setObject(stackTop, BuiltInData.FIntegerp.integerp(frame.getObject(stackTop)));
                        break;
                    case LISTN:                        // 0257
                    {
                        int count = Byte.toUnsignedInt(bytecode[bci + 1]);
                        stackTop -= count - 1;
                        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
                        for (int i = 0; i < count; i++) {
                            builder.add(frame.getObject(stackTop + 1));
                        }
                        frame.setObject(stackTop, builder.build());
                        break;
                    }
                    case CONCATN:                      // 0260
                    case INSERTN:                      // 0261
                    {
                        int count = Byte.toUnsignedInt(bytecode[bci + 1]);
                        stackTop -= count - 1;
                        Object[] args = new Object[count];
                        for (int i = 0; i < count; i++) {
                            args[i] = frame.getObject(stackTop);
                        }
                        frame.setObject(stackTop, op == CONCATN
                                ? BuiltInFns.FConcat.concat(args)
                                : BuiltInEditFns.FInsert.insert(args));
                        break;
                    }
                    case STACK_SET:                    // 0262
                    case STACK_SET2:                   // 0263
                        ref = op == STACK_SET ? Byte.toUnsignedInt(bytecode[bci + 1])
                                : Byte.toUnsignedInt(bytecode[bci + 1]) + (Byte.toUnsignedInt(bytecode[bci + 2]) << 8);
                        frame.setObject((stackTop--) - ref, frame.getObject(oldStackTop));
                        break;
                    case DISCARDN:                     // 0266
                    {
                        ref = Byte.toUnsignedInt(bytecode[bci + 1]);
                        if ((ref & 0x80) != 0) {
                            ref &= 0x7F;
                            frame.setObject(stackTop - ref, frame.getObject(stackTop));
                        }
                        stackTop -= ref;
                        break;
                    }
                    case SWITCH:                       // 0267
                        // TODO
                        throw new UnsupportedOperationException();
                    case CONSTANT:                     // 0300
                    default:
                        ref = op & (~CONSTANT);
                        frame.setObject(++stackTop, constants[ref]);
                        break;
                }
                if (nextBci < bci) {
                    if (BytecodeOSRNode.pollOSRBackEdge(this)) {
                        Object result = BytecodeOSRNode.tryOSR(this, nextBci, stackTop, null, frame);
                        if (result != null) {
                            return result;
                        }
                    }
                }
                bci = nextBci;
            } catch (AbstractTruffleException e) {
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
                int target = handler.target;
                if (CompilerDirectives.inInterpreter()) {
                    bci = jumps[target];
                    stackTop = jumpsStackTop[target];
                   continue;
                } else {
                    for (int i = 0; i < jumps.length; i++) {
                        if (i == target) {
                            bci = jumps[i];
                            stackTop = jumpsStackTop[i];
                            continue loop;
                        }
                    }
                }
                throw CompilerDirectives.shouldNotReachHere();
            }
        }
    }

    @GenerateInline(value = false)
    abstract static class Call0Node extends Node {
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

        abstract Object execute(Object function);
    }

    @GenerateInline(value = false)
    abstract static class Call1Node extends Node {
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

        abstract Object execute(Object function, Object arg);
    }

    @GenerateInline(value = false)
    abstract static class Call2Node extends Node {
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

        abstract Object execute(Object function, Object arg1, Object arg2);
    }

    @GenerateInline(value = false)
    abstract static class Call3Node extends Node {
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

        abstract Object execute(Object function, Object arg1, Object arg2, Object arg3);
    }

    @GenerateInline(value = false)
    abstract static class Call4Node extends Node {
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

        abstract Object execute(Object function, Object arg1, Object arg2, Object arg3, Object arg4);
    }

    @GenerateInline(value = false)
    abstract static class Call5Node extends Node {
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

        abstract Object execute(Object function, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5);
    }

    @GenerateInline(value = false)
    abstract static class CallNNode extends Node {
        @Specialization(guards = "function.callTarget() == directCallNode.getCallTarget()", limit = "2")
        protected static Object dispatchDirectly(
                ELispFunctionObject function,
                Object[] args,
                @Cached(value = "create(function.callTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(args);
        }

        abstract Object execute(Object function, Object[] args);
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
