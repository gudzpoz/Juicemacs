package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.math.BigInteger;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

/**
 * Built-in functions from {@code src/data.c}
 */
public class BuiltInData extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInDataFactory.getFactories();
    }

    @ELispBuiltIn(name = "eq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eq(Object a, Object b) {
            // Simulate the Emacs behavior of packed integers
            if (a instanceof Long) {
                return a.equals(b);
            }
            if (ELispSymbol.isNil(a)) {
                return ELispSymbol.isNil(b);
            }
            if (ELispSymbol.isT(a)) {
                return ELispSymbol.isT(b);
            }
            return a == b;
        }
    }

    @ELispBuiltIn(name = "null", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNull extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean isNull(Object a) {
            return ELispSymbol.isNil(a);
        }
    }

    @ELispBuiltIn(name = "type-of", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTypeOf extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol typeOf(Object a) {
            return switch (a) {
                case Boolean _, ELispSymbol _ -> SYMBOL;
                case Long _, ELispBigNum _ -> INTEGER;
                case ELispSubroutine _ -> SUBR;
                default -> FClTypeOf.clTypeOf(a);
            };
        }
    }

    @ELispBuiltIn(name = "cl-type-of", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClTypeOf extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol clTypeOf(Object a) {
            return switch (a) {
                case Long _ -> FIXNUM;
                case Double _ -> FLOAT;
                case ELispBigNum _ -> BIGNUM;
                case Boolean b when (boolean) b -> BOOLEAN;
                case ELispSymbol sym when sym == T -> BOOLEAN;
                case Boolean _ -> NULL;
                case ELispSymbol sym when sym == NIL -> NULL;
                case ELispSymbol _ -> SYMBOL;
                case ELispString _ -> STRING;
                case ELispVector _ -> VECTOR;
                case ELispBoolVector _ -> BOOL_VECTOR;
                case ELispCharTable _ -> CHAR_TABLE;
                // TODO: Handle other pseudo-vectors
                case ELispCons _ -> CONS;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "consp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FConsp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean consp(Object a) {
            return a instanceof ELispCons;
        }
    }

    @ELispBuiltIn(name = "atom", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAtom extends ELispBuiltInBaseNode {
        @Specialization
        public boolean atom(Object a) {
            return !(a instanceof ELispCons);
        }
    }

    @ELispBuiltIn(name = "listp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FListp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean listp(Object a) {
            return a instanceof ELispCons || ELispSymbol.isNil(a);
        }
    }

    @ELispBuiltIn(name = "nlistp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNlistp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nlistp(Object a) {
            return !FListp.listp(a);
        }
    }

    @ELispBuiltIn(name = "bare-symbol-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBareSymbolP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean bareSymbolP(Object a) {
            return a instanceof ELispSymbol;
        }
    }

    @ELispBuiltIn(name = "symbol-with-pos-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolWithPosP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean symbolWithPosP(Object ignored) {
            return true;
        }
    }

    @ELispBuiltIn(name = "symbolp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean symbolp(Object a) {
            return a instanceof ELispSymbol || a instanceof Boolean;
        }
    }

    @ELispBuiltIn(name = "keywordp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKeywordp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean keywordp(Object a) {
            if (a instanceof ELispSymbol symbol) {
                if (symbol.name().startsWith(":")) {
                    // TODO: if SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P ?
                    return true;
                }
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "vectorp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVectorp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean vectorp(Object a) {
            return a instanceof ELispVector;
        }
    }

    @ELispBuiltIn(name = "recordp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRecordp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean recordp(Object a) {
            return a instanceof ELispRecord;
        }
    }

    @ELispBuiltIn(name = "stringp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean stringp(Object a) {
            return a instanceof ELispString;
        }
    }

    @ELispBuiltIn(name = "multibyte-string-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMultibyteStringP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean multibyteStringP(Object a) {
            return a instanceof ELispString s && s.isMultibyte();
        }
    }

    @ELispBuiltIn(name = "char-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charTableP(Object a) {
            return a instanceof ELispCharTable;
        }
    }

    @ELispBuiltIn(name = "vector-or-char-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVectorOrCharTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean vectorOrCharTableP(Object a) {
            return a instanceof ELispVector || FCharTableP.charTableP(a);
        }
    }

    @ELispBuiltIn(name = "bool-vector-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBoolVectorP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean boolVectorP(Object a) {
            return a instanceof ELispBoolVector;
        }
    }

    @ELispBuiltIn(name = "arrayp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FArrayp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean arrayp(Object a) {
            return a instanceof ELispString || a instanceof ELispVector;
        }
    }

    @ELispBuiltIn(name = "sequencep", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSequencep extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean sequencep(Object a) {
            return FListp.listp(a) || FArrayp.arrayp(a);
        }
    }

    @ELispBuiltIn(name = "bufferp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "markerp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMarkerp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object markerp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-ptrp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUserPtrp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userPtrp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subrp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean subrp(Object a) {
            return a instanceof ELispSubroutine;
        }
    }

    @ELispBuiltIn(name = "closurep", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClosurep extends ELispBuiltInBaseNode {
        @Specialization
        public static Object closurep(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "byte-code-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FByteCodeFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object byteCodeFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "interpreted-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInterpretedFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object interpretedFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "module-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FModuleFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object moduleFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-or-string-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharOrStringP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charOrStringP(Object a) {
            return a instanceof ELispString
                    || ELispString.toValidChar(a) != null;
        }
    }

    @ELispBuiltIn(name = "integerp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIntegerp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean integerp(Object a) {
            return a instanceof Long || a instanceof ELispBigNum;
        }
    }

    @ELispBuiltIn(name = "integer-or-marker-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIntegerOrMarkerP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object integerOrMarkerP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "natnump", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNatnump extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean natnump(Object a) {
            return (a instanceof Long l && l >= 0)
                    || (a instanceof ELispBigNum n && n.value.signum() >= 0);
        }
    }

    @ELispBuiltIn(name = "numberp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumberp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean numberp(Object a) {
            return FIntegerp.integerp(a) || FFloatp.floatp(a);
        }
    }

    @ELispBuiltIn(name = "number-or-marker-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumberOrMarkerP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object numberOrMarkerP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "floatp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFloatp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean floatp(Object a) {
            return a instanceof Double;
        }
    }

    @ELispBuiltIn(name = "threadp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FThreadp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object threadp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mutexp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMutexp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mutexp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "condition-variable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FConditionVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object conditionVariableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "car", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object car(Object a) {
            return switch (a) {
                case ELispCons cons -> cons.car();
                case Boolean b when !b -> false;
                case ELispSymbol sym when sym == NIL -> NIL;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "car-safe", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCarSafe extends ELispBuiltInBaseNode {
        @Specialization
        public static Object carSafe(Object a) {
            if (a instanceof ELispCons cons) {
                return cons.car();
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "cdr", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCdr extends ELispBuiltInBaseNode {
        @Specialization
        public Object cdr(Object a) {
            return switch (a) {
                case ELispCons cons -> cons.cdr();
                case Boolean b when !b -> false;
                case ELispSymbol sym when sym == NIL -> NIL;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "cdr-safe", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCdrSafe extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cdrSafe(Object a) {
            if (a instanceof ELispCons cons) {
                return cons.cdr();
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "setcar", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetcar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setcar(Object a, Object b) {
            ((ELispCons) a).setCar(b);
            return b;
        }
    }

    @ELispBuiltIn(name = "setcdr", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetcdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setcdr(Object a, Object b) {
            ((ELispCons) a).setCdr(b);
            return b;
        }
    }

    @ELispBuiltIn(name = "boundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBoundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean boundp(ELispSymbol a) {
            return a.isBound();
        }
    }

    @ELispBuiltIn(name = "fboundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFboundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean fboundp(ELispSymbol a) {
            return !ELispSymbol.isNil(a.getFunction());
        }
    }

    @ELispBuiltIn(name = "makunbound", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makunbound(ELispSymbol a) {
            a.makeUnbound();
            return a;
        }
    }

    @ELispBuiltIn(name = "fmakunbound", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFmakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fmakunbound(ELispSymbol a) {
            a.setFunction(NIL);
            return a;
        }
    }

    @ELispBuiltIn(name = "symbol-function", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolFunction(ELispSymbol a) {
            return a.getFunction();
        }
    }

    @ELispBuiltIn(name = "symbol-plist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolPlist(ELispSymbol a) {
            return a.getProperties();
        }
    }

    @ELispBuiltIn(name = "symbol-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString symbolName(ELispSymbol a) {
            return new ELispString(a.name());
        }
    }

    @ELispBuiltIn(name = "bare-symbol", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBareSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol bareSymbol(ELispSymbol a) {
            return a;
        }
    }

    @ELispBuiltIn(name = "symbol-with-pos-pos", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolWithPosPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolWithPosPos(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "remove-pos-from-symbol", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRemovePosFromSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object removePosFromSymbol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "position-symbol", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPositionSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object positionSymbol(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "fset", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fset(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defalias", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefalias extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defalias(ELispSymbol symbol, ELispValue def, Object c) {
            // TODO: Handle defalias-fset-function
            System.out.println(symbol);
            if (def instanceof ELispSymbol target) {
                symbol.aliasSymbol(target);
            } else {
                symbol.setFunction(def);
            }
            return symbol;
        }
    }

    @ELispBuiltIn(name = "setplist", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetplist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setplist(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-arity", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrArity extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrArity(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNativeCompFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-native-lambda-list", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrNativeLambdaList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrNativeLambdaList(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-type", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrType extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrType(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-native-comp-unit", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrNativeCompUnit extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrNativeCompUnit(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-unit-file", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNativeCompUnitFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompUnitFile(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-unit-set-file", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNativeCompUnitSetFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompUnitSetFile(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "interactive-form", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInteractiveForm extends ELispBuiltInBaseNode {
        @Specialization
        public static Object interactiveForm(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "command-modes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCommandModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object commandModes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "indirect-variable", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIndirectVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object indirectVariable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "symbol-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolValue(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object set(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "add-variable-watcher", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAddVariableWatcher extends ELispBuiltInBaseNode {
        @Specialization
        public static Object addVariableWatcher(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "remove-variable-watcher", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRemoveVariableWatcher extends ELispBuiltInBaseNode {
        @Specialization
        public static Object removeVariableWatcher(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-variable-watchers", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetVariableWatchers extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getVariableWatchers(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "default-boundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDefaultBoundp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultBoundp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "default-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDefaultValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultValue(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-default", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetDefault extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefault(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-variable-buffer-local", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeVariableBufferLocal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeVariableBufferLocal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-local-variable", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeLocalVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeLocalVariable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-local-variable", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKillLocalVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killLocalVariable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "local-variable-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLocalVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object localVariableP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "local-variable-if-set-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLocalVariableIfSetP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object localVariableIfSetP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "variable-binding-locus", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVariableBindingLocus extends ELispBuiltInBaseNode {
        @Specialization
        public static Object variableBindingLocus(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "indirect-function", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FIndirectFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object indirectFunction(Object a, Object _noError) {
            if (a instanceof ELispSymbol symbol) {
                return symbol.getFunction();
            }
            return a;
        }
    }

    @ELispBuiltIn(name = "aref", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAref extends ELispBuiltInBaseNode {
        @Specialization
        public static Object aref(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "aset", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object aset(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    public static int compareTo(Object a, Object b) {
        if (!FNumberp.numberp(a)) {
            throw new IllegalArgumentException();
        }
        return switch (a) {
            case Double d -> d.compareTo(ELispTypeSystemGen.asImplicitDouble(b));
            case ELispBigNum n when !(b instanceof Double) -> n.value.compareTo(
                    ELispTypeSystemGen.asImplicitELispBigNum(b).value
            );
            case Long l when b instanceof Long lb -> l.compareTo(lb);
            default -> -compareTo(b, a);
        };
    }

    @ELispBuiltIn(name = "=", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FEqlsign extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static boolean eqlsignLong(long a, Object[] args) {
            for (Object arg : args) {
                if (a != (Long) arg) {
                    return false;
                }
            }
            return true;
        }

        @Specialization(rewriteOn = ClassCastException.class)
        public static boolean eqlsignDouble(double a, Object[] args) {
            for (Object arg : args) {
                if (arg instanceof Long l) {
                    if (a != l) {
                        return false;
                    }
                } else if (a != (Double) arg) {
                    return false;
                }
            }
            return true;
        }

        @Specialization(replaces = {"eqlsignLong", "eqlsignDouble"})
        public static boolean eqlsign(Object a, Object[] args) {
            for (Object arg : args) {
                if (compareTo(a, arg) != 0) {
                    return false;
                }
            }
            return true;
        }
    }

    @ELispBuiltIn(name = "<", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLss extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean lss(Object a, Object[] args) {
            Object prev = a;
            for (Object arg : args) {
                if (compareTo(prev, arg) >= 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    @ELispBuiltIn(name = ">", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FGtr extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean gtr(Object a, Object[] args) {
            Object prev = a;
            for (Object arg : args) {
                if (compareTo(prev, arg) <= 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    @ELispBuiltIn(name = "<=", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean leq(Object a, Object[] args) {
            Object prev = a;
            for (Object arg : args) {
                if (compareTo(prev, arg) > 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    @ELispBuiltIn(name = ">=", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FGeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean geq(Object a, Object[] args) {
            Object prev = a;
            for (Object arg : args) {
                if (compareTo(prev, arg) < 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    @ELispBuiltIn(name = "/=", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean neq(Object a, Object b) {
            return compareTo(a, b) != 0;
        }
    }

    @ELispBuiltIn(name = "number-to-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumberToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object numberToString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-number", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringToNumber extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToNumber(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "+", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FPlus extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long plusLong(Object[] args) {
            long sum = 0;
            for (Object arg : args) {
                sum = Math.addExact(sum, (Long) arg);
            }
            return sum;
        }

        @Specialization(replaces = "plusLong")
        public static Object plusAny(Object[] args) {
            return tryAddLong(args);
        }

        public static Object tryAddLong(Object[] args) {
            long sum = 0;
            for (int i = 0; i < args.length; i++) {
                switch (args[i]) {
                    case Long l -> {
                        try {
                            sum = Math.addExact(sum, l);
                        } catch (ArithmeticException e) {
                            return tryAddBigNum(sum, i, args);
                        }
                    }
                    case Double _ -> {
                        return tryAddDouble((double) sum, i, args);
                    }
                    case ELispBigNum _ -> {
                        return tryAddBigNum(sum, i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return sum;
        }

        private static Object tryAddBigNum(long prev, int i, Object[] args) {
            BigInteger sum = BigInteger.valueOf(prev);
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> sum = sum.add(n.value);
                    case Long l -> sum = sum.add(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryAddDouble(sum.doubleValue(), i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return ELispBigNum.wrap(sum);
        }

        private static double tryAddDouble(double prev, int i, Object[] args) {
            double sum = prev;
            for (; i < args.length; i++) {
                sum += ELispTypeSystemGen.asImplicitDouble(args[i]);
            }
            return sum;
        }
    }

    @ELispBuiltIn(name = "-", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMinus extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long minusLong(Object[] args) {
            if (args.length == 0) {
                return 0;
            }
            long result = (Long) args[0];
            if (args.length == 1) {
                return Math.negateExact(result);
            }
            for (int i = 1; i < args.length; i++) {
                result = Math.subtractExact(result, (Long) args[i]);
            }
            return result;
        }

        @Specialization(replaces = "minusLong")
        public static Object minusAny(Object[] args) {
            if (args.length == 0) {
                return 0L;
            }
            if (args.length == 1) {
                return switch (args[0]) {
                    case Long l when l > Long.MIN_VALUE -> Math.negateExact(l);
                    case Long l -> ELispBigNum.wrap(BigInteger.valueOf(l).negate());
                    case Double d -> -d;
                    case ELispBigNum n -> ELispBigNum.wrap(n.value.negate());
                    default -> throw new IllegalArgumentException();
                };
            }
            return switch (args[0]) {
                case Long l -> tryMinusLong(l, 1, args);
                case ELispBigNum n -> tryMinusBigNum(n.value, 1, args);
                case Double d -> tryMinusDouble(d, 1, args);
                default -> throw new IllegalArgumentException();
            };
        }

        public static Object tryMinusLong(long result, int i, Object[] args) {
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case Long l -> {
                        try {
                            result = Math.subtractExact(result, l);
                        } catch (ArithmeticException e) {
                            return tryMinusBigNum(BigInteger.valueOf(result), i, args);
                        }
                    }
                    case Double _ -> {
                        return tryMinusDouble((double) result, i, args);
                    }
                    case ELispBigNum _ -> {
                        return tryMinusBigNum(BigInteger.valueOf(result), i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return result;
        }

        private static Object tryMinusBigNum(BigInteger result, int i, Object[] args) {
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> result = result.subtract(n.value);
                    case Long l -> result = result.subtract(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryMinusDouble(result.doubleValue(), i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return ELispBigNum.wrap(result);
        }

        private static double tryMinusDouble(double prev, int i, Object[] args) {
            double result = prev;
            for (; i < args.length; i++) {
                result -= ELispTypeSystemGen.asImplicitDouble(args[i]);
            }
            return result;
        }
    }

    @ELispBuiltIn(name = "*", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FTimes extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long timesLong(Object[] args) {
            long result = 1;
            for (Object arg : args) {
                result = Math.multiplyExact(result, (Long) arg);
            }
            return result;
        }

        @Specialization(replaces = "timesLong")
        public static Object timesAny(Object[] args) {
            return tryTimesLong(args);
        }

        public static Object tryTimesLong(Object[] args) {
            long product = 1;
            for (int i = 0; i < args.length; i++) {
                switch (args[i]) {
                    case Long l -> {
                        try {
                            product = Math.multiplyExact(product, l);
                        } catch (ArithmeticException e) {
                            return tryTimesBigNum(product, i, args);
                        }
                    }
                    case Double _ -> {
                        return tryTimesDouble((double) product, i, args);
                    }
                    case ELispBigNum _ -> {
                        return tryTimesBigNum(product, i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return product;
        }

        private static Object tryTimesBigNum(long prev, int i, Object[] args) {
            BigInteger product = BigInteger.valueOf(prev);
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> product = product.multiply(n.value);
                    case Long l -> product = product.multiply(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryTimesDouble(product.doubleValue(), i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return ELispBigNum.wrap(product);
        }

        private static double tryTimesDouble(double prev, int i, Object[] args) {
            double product = prev;
            for (; i < args.length; i++) {
                product *= ELispTypeSystemGen.asImplicitDouble(args[i]);
            }
            return product;
        }
    }

    @ELispBuiltIn(name = "/", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FQuo extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long quoLong(long a, Object[] args) {
            long result = a;
            for (Object arg : args) {
                result /= (Long) arg;
            }
            return result;
        }

        @Specialization(replaces = "quoLong")
        public static Object quoAny(Object a, Object[] args) {
            for (Object arg : args) {
                if (arg instanceof Double) {
                    return tryQuoDouble(ELispTypeSystemGen.asImplicitDouble(a), args);
                }
            }
            return switch (a) {
                case Long l -> tryQuoLong(l, args);
                case ELispBigNum n -> tryQuoBigNum(n.value, 0, args);
                case Double d -> tryQuoDouble(d, args);
                default -> throw new IllegalArgumentException();
            };
        }

        public static Object tryQuoLong(long a, Object[] args) {
            long quo = a;
            for (int i = 0; i < args.length; i++) {
                switch (args[i]) {
                    case Long l -> {
                        try {
                            quo = Math.divideExact(quo, l);
                        } catch (ArithmeticException e) {
                            return tryQuoBigNum(BigInteger.valueOf(quo), i, args);
                        }
                    }
                    case ELispBigNum _ -> {
                        return tryQuoBigNum(BigInteger.valueOf(quo), i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return quo;
        }

        private static Object tryQuoBigNum(BigInteger prev, int i, Object[] args) {
            BigInteger quo = prev;
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> quo = quo.divide(n.value);
                    case Long l -> quo = quo.divide(BigInteger.valueOf(l));
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            return ELispBigNum.wrap(quo);
        }

        private static double tryQuoDouble(double prev, Object[] args) {
            double quo = prev;
            for (Object arg : args) {
                quo /= ELispTypeSystemGen.asImplicitDouble(arg);
            }
            return quo;
        }
    }

    @ELispBuiltIn(name = "%", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRem extends ELispBuiltInBaseNode {
        @Specialization
        public static long remLong(long a, long b) {
            return a % b;
        }

        @Specialization
        public static Object rem(ELispBigNum a, ELispBigNum b) {
            return ELispBigNum.wrap(a.value.remainder(b.value));
        }
    }

    @ELispBuiltIn(name = "mod", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMod extends ELispBuiltInBaseNode {
        @Specialization
        public static long modLong(long a, long b) {
            return Long.remainderUnsigned(a, b);
        }

        @Specialization
        public static Object mod(ELispBigNum a, ELispBigNum b) {
            return ELispBigNum.wrap(a.value.mod(b.value));
        }
    }

    @ELispBuiltIn(name = "max", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMax extends ELispBuiltInBaseNode {
        @Specialization
        public static Object max(Object a, Object[] args) {
            Object result = a;
            for (Object arg : args) {
                if (compareTo(result, arg) < 0) {
                    result = arg;
                }
            }
            return result;
        }
    }

    @ELispBuiltIn(name = "min", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMin extends ELispBuiltInBaseNode {
        @Specialization
        public static Object min(Object a, Object[] args) {
            Object result = a;
            for (Object arg : args) {
                if (compareTo(result, arg) > 0) {
                    result = arg;
                }
            }
            return result;
        }
    }

    @ELispBuiltIn(name = "logand", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLogand extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logandLong(Object[] args) {
            long result = -1;
            for (Object arg : args) {
                result &= (long) arg;
            }
            return result;
        }

        @Specialization(replaces = "logandLong")
        public static Object logand(Object[] args) {
            BigInteger result = BigInteger.ONE.negate();
            for (Object arg : args) {
                result = result.and(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.value;
                    default -> throw new IllegalArgumentException();
                });
            }
            return ELispBigNum.wrap(result);
        }
    }

    @ELispBuiltIn(name = "logior", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLogior extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logorLong(Object[] args) {
            long result = 0;
            for (Object arg : args) {
                result |= (long) arg;
            }
            return result;
        }

        @Specialization(replaces = "logorLong")
        public static Object logior(Object[] args) {
            BigInteger result = BigInteger.ZERO;
            for (Object arg : args) {
                result = result.or(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.value;
                    default -> throw new IllegalArgumentException();
                });
            }
            return ELispBigNum.wrap(result);
        }
    }

    @ELispBuiltIn(name = "logxor", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLogxor extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logxorLong(Object[] args) {
            long result = 0;
            for (Object arg : args) {
                result ^= (long) arg;
            }
            return result;
        }

        @Specialization(replaces = "logxorLong")
        public static Object logxor(Object[] args) {
            BigInteger result = BigInteger.ZERO;
            for (Object arg : args) {
                result = result.xor(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.value;
                    default -> throw new IllegalArgumentException();
                });
            }
            return ELispBigNum.wrap(result);
        }
    }

    @ELispBuiltIn(name = "logcount", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLogcount extends ELispBuiltInBaseNode {
        @Specialization
        public static long logcountLong(long a) {
            return a >= 0 ? Long.bitCount(a) : Long.SIZE - Long.bitCount(a);
        }

        @Specialization
        public static long logcount(ELispBigNum a) {
            return a.value.bitCount();
        }
    }

    @ELispBuiltIn(name = "ash", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAsh extends ELispBuiltInBaseNode {
        @Specialization
        public static Object ashLong(long a, long b) {
            if (b == 0) {
                return a;
            }
            if (b < 0) {
                long shift = -b;
                if (shift >= Long.SIZE) {
                    return a < 0 ? -1L : 0L;
                }
                return a >> shift;
            }
            BigInteger v = BigInteger.valueOf(a);
            return ELispBigNum.wrap(v.shiftLeft((int) b));
        }

        @Specialization
        public static Object ash(ELispBigNum a, long b) {
            return ELispBigNum.wrap(a.value.shiftLeft((int) b));
        }
    }

    @ELispBuiltIn(name = "1+", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAdd1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object add1(Object a) {
            return switch (a) {
                case Long l when l < Long.MAX_VALUE -> l + 1;
                case Long l -> ELispBigNum.wrap(BigInteger.valueOf(l).add(BigInteger.ONE));
                case Double d -> d + 1;
                case ELispBigNum n -> ELispBigNum.wrap(n.value.add(BigInteger.ONE));
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "1-", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSub1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sub1(Object a) {
            return switch (a) {
                case Long l when l > Long.MIN_VALUE -> l - 1;
                case Long l -> ELispBigNum.wrap(BigInteger.valueOf(l).subtract(BigInteger.ONE));
                case Double d -> d - 1;
                case ELispBigNum n -> ELispBigNum.wrap(n.value.subtract(BigInteger.ONE));
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "lognot", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLognot extends ELispBuiltInBaseNode {
        @Specialization
        public static long lognot(long a) {
            return ~a;
        }

        @Specialization
        public static Object lognot(ELispBigNum a) {
            return ELispBigNum.wrap(a.value.not());
        }
    }

    @ELispBuiltIn(name = "byteorder", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FByteorder extends ELispBuiltInBaseNode {
        @Specialization
        public static long byteorder() {
            return 'B';
        }
    }

    @ELispBuiltIn(name = "bool-vector-exclusive-or", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorExclusiveOr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorExclusiveOr(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-union", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorUnion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorUnion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-intersection", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorIntersection extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorIntersection(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-set-difference", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorSetDifference extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorSetDifference(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-subsetp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBoolVectorSubsetp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorSubsetp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-not", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBoolVectorNot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorNot(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-count-population", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBoolVectorCountPopulation extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorCountPopulation(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-count-consecutive", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorCountConsecutive extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorCountConsecutive(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }
}
