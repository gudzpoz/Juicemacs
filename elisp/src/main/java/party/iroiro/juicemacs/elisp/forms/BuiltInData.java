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

    @ELispBuiltIn(name = "eq", minArgs = 2, maxArgs = 2, doc = "Return t if the two args are the same Lisp object.")
    @GenerateNodeFactory
    public abstract static class FEq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eq(Object a, Object b) {
            // Simulate the Emacs behavior of packed integers
            return a instanceof Long ? a.equals(b) : a == b;
        }
    }

    @ELispBuiltIn(name = "null", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is nil, and return nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FNull extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean isNull(Object a) {
            return ELispSymbol.isNil(a);
        }
    }

    @ELispBuiltIn(name = "type-of", minArgs = 1, maxArgs = 1, doc = "Return a symbol representing the type of OBJECT.\nThe symbol returned names the object's basic type;\nfor example, (type-of 1) returns `integer'.\nContrary to `cl-type-of', the returned type is not always the most\nprecise type possible, because instead this function tries to preserve\ncompatibility with the return value of previous Emacs versions.")
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

    @ELispBuiltIn(name = "cl-type-of", minArgs = 1, maxArgs = 1, doc = "Return a symbol representing the type of OBJECT.\nThe returned symbol names the most specific possible type of the object.\nfor example, (cl-type-of nil) returns `null'.\nThe specific type returned may change depending on Emacs versions,\nso we recommend you use `cl-typep', `cl-typecase', or other predicates\nrather than compare the return value of this function against\na fixed set of types.")
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
                // TODO: Handle other pseudo-vectors
                case ELispCons _ -> CONS;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "consp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a cons cell.")
    @GenerateNodeFactory
    public abstract static class FConsp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean consp(Object a) {
            return a instanceof ELispCons;
        }
    }

    @ELispBuiltIn(name = "atom", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is not a cons cell.  This includes nil.")
    @GenerateNodeFactory
    public abstract static class FAtom extends ELispBuiltInBaseNode {
        @Specialization
        public boolean atom(Object a) {
            return !(a instanceof ELispCons);
        }
    }

    @ELispBuiltIn(name = "listp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a list, that is, a cons cell or nil.\nOtherwise, return nil.")
    @GenerateNodeFactory
    public abstract static class FListp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean listp(Object a) {
            return a instanceof ELispCons || ELispSymbol.isNil(a);
        }
    }

    @ELispBuiltIn(name = "nlistp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is not a list.  Lists include nil.")
    @GenerateNodeFactory
    public abstract static class FNlistp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nlistp(Object a) {
            return !FListp.listp(a);
        }
    }

    @ELispBuiltIn(name = "bare-symbol-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a symbol, but not a symbol together with position.")
    @GenerateNodeFactory
    public abstract static class FBareSymbolP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean bareSymbolP(Object a) {
            return a instanceof ELispSymbol;
        }
    }

    @ELispBuiltIn(name = "symbol-with-pos-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a symbol together with position.\nIgnore `symbols-with-pos-enabled'.")
    @GenerateNodeFactory
    public abstract static class FSymbolWithPosP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean symbolWithPosP(Object ignored) {
            return true;
        }
    }

    @ELispBuiltIn(name = "symbolp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a symbol.")
    @GenerateNodeFactory
    public abstract static class FSymbolp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean symbolp(Object a) {
            return a instanceof ELispSymbol || a instanceof Boolean;
        }
    }

    @ELispBuiltIn(name = "keywordp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a keyword.\nThis means that it is a symbol with a print name beginning with `:'\ninterned in the initial obarray.")
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

    @ELispBuiltIn(name = "vectorp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a vector.")
    @GenerateNodeFactory
    public abstract static class FVectorp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean vectorp(Object a) {
            return a instanceof ELispVector;
        }
    }

    @ELispBuiltIn(name = "recordp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a record.")
    @GenerateNodeFactory
    public abstract static class FRecordp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean recordp(Object a) {
            return a instanceof ELispRecord;
        }
    }

    @ELispBuiltIn(name = "stringp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a string.")
    @GenerateNodeFactory
    public abstract static class FStringp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean stringp(Object a) {
            return a instanceof ELispString;
        }
    }

    @ELispBuiltIn(name = "multibyte-string-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a multibyte string.\nReturn nil if OBJECT is either a unibyte string, or not a string.")
    @GenerateNodeFactory
    public abstract static class FMultibyteStringP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean multibyteStringP(Object a) {
            return a instanceof ELispString s && s.isMultibyte();
        }
    }

    @ELispBuiltIn(name = "char-table-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a char-table.")
    @GenerateNodeFactory
    public abstract static class FCharTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charTableP(Object a) {
            return a instanceof ELispCharTable;
        }
    }

    @ELispBuiltIn(name = "vector-or-char-table-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a char-table or vector.")
    @GenerateNodeFactory
    public abstract static class FVectorOrCharTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean vectorOrCharTableP(Object a) {
            return a instanceof ELispVector || FCharTableP.charTableP(a);
        }
    }

    @ELispBuiltIn(name = "bool-vector-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a bool-vector.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean boolVectorP(Object a) {
            return a instanceof ELispBoolVector;
        }
    }

    @ELispBuiltIn(name = "arrayp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is an array (string or vector).")
    @GenerateNodeFactory
    public abstract static class FArrayp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean arrayp(Object a) {
            return a instanceof ELispString || a instanceof ELispVector;
        }
    }

    @ELispBuiltIn(name = "sequencep", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a sequence (list or array).")
    @GenerateNodeFactory
    public abstract static class FSequencep extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean sequencep(Object a) {
            return FListp.listp(a) || FArrayp.arrayp(a);
        }
    }

    @ELispBuiltIn(name = "bufferp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is an editor buffer.")
    @GenerateNodeFactory
    public abstract static class FBufferp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "markerp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a marker (editor pointer).")
    @GenerateNodeFactory
    public abstract static class FMarkerp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object markerp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-ptrp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a module user pointer.")
    @GenerateNodeFactory
    public abstract static class FUserPtrp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userPtrp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subrp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a built-in or native compiled Lisp function.\n\nSee also `primitive-function-p' and `native-comp-function-p'.")
    @GenerateNodeFactory
    public abstract static class FSubrp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean subrp(Object a) {
            return a instanceof ELispSubroutine;
        }
    }

    @ELispBuiltIn(name = "closurep", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a function of type `closure'.")
    @GenerateNodeFactory
    public abstract static class FClosurep extends ELispBuiltInBaseNode {
        @Specialization
        public static Object closurep(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "byte-code-function-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a byte-compiled function object.")
    @GenerateNodeFactory
    public abstract static class FByteCodeFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object byteCodeFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "interpreted-function-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a function of type `interpreted-function'.")
    @GenerateNodeFactory
    public abstract static class FInterpretedFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object interpretedFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "module-function-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a function loaded from a dynamic module.")
    @GenerateNodeFactory
    public abstract static class FModuleFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object moduleFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-or-string-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a character or a string.")
    @GenerateNodeFactory
    public abstract static class FCharOrStringP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charOrStringP(Object a) {
            return a instanceof ELispString
                    || ELispString.toValidChar(a) != null;
        }
    }

    @ELispBuiltIn(name = "integerp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is an integer.")
    @GenerateNodeFactory
    public abstract static class FIntegerp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean integerp(Object a) {
            return a instanceof Long || a instanceof ELispBigNum;
        }
    }

    @ELispBuiltIn(name = "integer-or-marker-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is an integer or a marker (editor pointer).")
    @GenerateNodeFactory
    public abstract static class FIntegerOrMarkerP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object integerOrMarkerP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "natnump", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a nonnegative integer.")
    @GenerateNodeFactory
    public abstract static class FNatnump extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean natnump(Object a) {
            return (a instanceof Long l && l >= 0)
                    || (a instanceof ELispBigNum n && n.value.signum() >= 0);
        }
    }

    @ELispBuiltIn(name = "numberp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a number (floating point or integer).")
    @GenerateNodeFactory
    public abstract static class FNumberp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean numberp(Object a) {
            return FIntegerp.integerp(a) || FFloatp.floatp(a);
        }
    }

    @ELispBuiltIn(name = "number-or-marker-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a number or a marker.")
    @GenerateNodeFactory
    public abstract static class FNumberOrMarkerP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object numberOrMarkerP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "floatp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a floating point number.")
    @GenerateNodeFactory
    public abstract static class FFloatp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean floatp(Object a) {
            return a instanceof Double;
        }
    }

    @ELispBuiltIn(name = "threadp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a thread.")
    @GenerateNodeFactory
    public abstract static class FThreadp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object threadp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mutexp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a mutex.")
    @GenerateNodeFactory
    public abstract static class FMutexp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mutexp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "condition-variable-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a condition variable.")
    @GenerateNodeFactory
    public abstract static class FConditionVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object conditionVariableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "car", minArgs = 1, maxArgs = 1, doc = "Return the car of LIST.  If LIST is nil, return nil.\nError if LIST is not nil and not a cons cell.  See also `car-safe'.\n\nSee Info node `(elisp)Cons Cells' for a discussion of related basic\nLisp concepts such as car, cdr, cons cell and list.")
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

    @ELispBuiltIn(name = "car-safe", minArgs = 1, maxArgs = 1, doc = "Return the car of OBJECT if it is a cons cell, or else nil.")
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

    @ELispBuiltIn(name = "cdr", minArgs = 1, maxArgs = 1, doc = "Return the cdr of LIST.  If LIST is nil, return nil.\nError if LIST is not nil and not a cons cell.  See also `cdr-safe'.\n\nSee Info node `(elisp)Cons Cells' for a discussion of related basic\nLisp concepts such as cdr, car, cons cell and list.")
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

    @ELispBuiltIn(name = "cdr-safe", minArgs = 1, maxArgs = 1, doc = "Return the cdr of OBJECT if it is a cons cell, or else nil.")
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

    @ELispBuiltIn(name = "setcar", minArgs = 2, maxArgs = 2, doc = "Set the car of CELL to be NEWCAR.  Returns NEWCAR.")
    @GenerateNodeFactory
    public abstract static class FSetcar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setcar(Object a, Object b) {
            ((ELispCons) a).setCar(b);
            return b;
        }
    }

    @ELispBuiltIn(name = "setcdr", minArgs = 2, maxArgs = 2, doc = "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.")
    @GenerateNodeFactory
    public abstract static class FSetcdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setcdr(Object a, Object b) {
            ((ELispCons) a).setCdr(b);
            return b;
        }
    }

    @ELispBuiltIn(name = "boundp", minArgs = 1, maxArgs = 1, doc = "Return t if SYMBOL's value is not void.\nNote that if `lexical-binding' is in effect, this refers to the\nglobal value outside of any lexical scope.")
    @GenerateNodeFactory
    public abstract static class FBoundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean boundp(ELispSymbol a) {
            return a.isBound();
        }
    }

    @ELispBuiltIn(name = "fboundp", minArgs = 1, maxArgs = 1, doc = "Return t if SYMBOL's function definition is not void.")
    @GenerateNodeFactory
    public abstract static class FFboundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean fboundp(ELispSymbol a) {
            return !ELispSymbol.isNil(a.getFunction());
        }
    }

    @ELispBuiltIn(name = "makunbound", minArgs = 1, maxArgs = 1, doc = "Empty out the value cell of SYMBOL, making it void as a variable.\nReturn SYMBOL.\n\nIf a variable is void, trying to evaluate the variable signals a\n`void-variable' error, instead of returning a value.  For more\ndetails, see Info node `(elisp) Void Variables'.\n\nSee also `fmakunbound'.")
    @GenerateNodeFactory
    public abstract static class FMakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makunbound(ELispSymbol a) {
            a.makeUnbound();
            return a;
        }
    }

    @ELispBuiltIn(name = "fmakunbound", minArgs = 1, maxArgs = 1, doc = "Make SYMBOL's function definition be void.\nReturn SYMBOL.\n\nIf a function definition is void, trying to call a function by that\nname will cause a `void-function' error.  For more details, see Info\nnode `(elisp) Function Cells'.\n\nSee also `makunbound'.")
    @GenerateNodeFactory
    public abstract static class FFmakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fmakunbound(ELispSymbol a) {
            a.setFunction(NIL);
            return a;
        }
    }

    @ELispBuiltIn(name = "symbol-function", minArgs = 1, maxArgs = 1, doc = "Return SYMBOL's function definition, or nil if that is void.")
    @GenerateNodeFactory
    public abstract static class FSymbolFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolFunction(ELispSymbol a) {
            return a.getFunction();
        }
    }

    @ELispBuiltIn(name = "symbol-plist", minArgs = 1, maxArgs = 1, doc = "Return SYMBOL's property list.")
    @GenerateNodeFactory
    public abstract static class FSymbolPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolPlist(ELispSymbol a) {
            return a.getProperties();
        }
    }

    @ELispBuiltIn(name = "symbol-name", minArgs = 1, maxArgs = 1, doc = "Return SYMBOL's name, a string.\n\nWarning: never alter the string returned by `symbol-name'.\nDoing that might make Emacs dysfunctional, and might even crash Emacs.")
    @GenerateNodeFactory
    public abstract static class FSymbolName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString symbolName(ELispSymbol a) {
            return new ELispString(a.name());
        }
    }

    @ELispBuiltIn(name = "bare-symbol", minArgs = 1, maxArgs = 1, doc = "Extract, if need be, the bare symbol from SYM.\nSYM is either a symbol or a symbol with position.\nIgnore `symbols-with-pos-enabled'.")
    @GenerateNodeFactory
    public abstract static class FBareSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol bareSymbol(ELispSymbol a) {
            return a;
        }
    }

    @ELispBuiltIn(name = "symbol-with-pos-pos", minArgs = 1, maxArgs = 1, doc = "Extract the position from the symbol with position SYMPOS.\nIgnore `symbols-with-pos-enabled'.")
    @GenerateNodeFactory
    public abstract static class FSymbolWithPosPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolWithPosPos(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "remove-pos-from-symbol", minArgs = 1, maxArgs = 1, doc = "If ARG is a symbol with position, return it without the position.\nOtherwise, return ARG unchanged.  Ignore `symbols-with-pos-enabled'.\nCompare with `bare-symbol'.")
    @GenerateNodeFactory
    public abstract static class FRemovePosFromSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object removePosFromSymbol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "position-symbol", minArgs = 2, maxArgs = 2, doc = "Make a new symbol with position.\nSYM is a symbol, with or without position, the symbol to position.\nPOS, the position, is either a nonnegative fixnum,\nor a symbol with position from which the position will be taken.\nIgnore `symbols-with-pos-enabled'.")
    @GenerateNodeFactory
    public abstract static class FPositionSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object positionSymbol(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "fset", minArgs = 2, maxArgs = 2, doc = "Set SYMBOL's function definition to DEFINITION, and return DEFINITION.\nIf the resulting chain of function definitions would contain a loop,\nsignal a `cyclic-function-indirection' error.")
    @GenerateNodeFactory
    public abstract static class FFset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fset(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "defalias", minArgs = 2, maxArgs = 3, doc = "Set SYMBOL's function definition to DEFINITION.\nAssociates the function with the current load file, if any.\nThe optional third argument DOCSTRING specifies the documentation string\nfor SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string\ndetermined by DEFINITION.\n\nInternally, this normally uses `fset', but if SYMBOL has a\n`defalias-fset-function' property, the associated value is used instead.\n\nThe return value is undefined.")
    @GenerateNodeFactory
    public abstract static class FDefalias extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defalias(ELispSymbol symbol, ELispValue def, Object c) {
            // TODO: Handle defalias-fset-function
            if (def instanceof ELispSymbol target) {
                symbol.aliasSymbol(target);
            } else {
                symbol.setFunction(def);
            }
            return symbol;
        }
    }

    @ELispBuiltIn(name = "setplist", minArgs = 2, maxArgs = 2, doc = "Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.")
    @GenerateNodeFactory
    public abstract static class FSetplist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setplist(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-arity", minArgs = 1, maxArgs = 1, doc = "Return minimum and maximum number of args allowed for SUBR.\nSUBR must be a built-in function.\nThe returned value is a pair (MIN . MAX).  MIN is the minimum number\nof args.  MAX is the maximum number or the symbol `many', for a\nfunction with `&rest' args, or `unevalled' for a special form.")
    @GenerateNodeFactory
    public abstract static class FSubrArity extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrArity(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-name", minArgs = 1, maxArgs = 1, doc = "Return name of subroutine SUBR.\nSUBR must be a built-in function.")
    @GenerateNodeFactory
    public abstract static class FSubrName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-function-p", minArgs = 1, maxArgs = 1, doc = "Return t if the object is native compiled Lisp function, nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FNativeCompFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompFunctionP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-native-lambda-list", minArgs = 1, maxArgs = 1, doc = "Return the lambda list for a native compiled lisp/d\nfunction or t otherwise.")
    @GenerateNodeFactory
    public abstract static class FSubrNativeLambdaList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrNativeLambdaList(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-type", minArgs = 1, maxArgs = 1, doc = "Return the type of SUBR.")
    @GenerateNodeFactory
    public abstract static class FSubrType extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrType(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subr-native-comp-unit", minArgs = 1, maxArgs = 1, doc = "Return the native compilation unit.")
    @GenerateNodeFactory
    public abstract static class FSubrNativeCompUnit extends ELispBuiltInBaseNode {
        @Specialization
        public static Object subrNativeCompUnit(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-unit-file", minArgs = 1, maxArgs = 1, doc = "Return the file of the native compilation unit.")
    @GenerateNodeFactory
    public abstract static class FNativeCompUnitFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompUnitFile(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "native-comp-unit-set-file", minArgs = 2, maxArgs = 2, doc = "Return the file of the native compilation unit.")
    @GenerateNodeFactory
    public abstract static class FNativeCompUnitSetFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nativeCompUnitSetFile(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "interactive-form", minArgs = 1, maxArgs = 1, doc = "Return the interactive form of CMD or nil if none.\nIf CMD is not a command, the return value is nil.\nValue, if non-nil, is a list (interactive SPEC).")
    @GenerateNodeFactory
    public abstract static class FInteractiveForm extends ELispBuiltInBaseNode {
        @Specialization
        public static Object interactiveForm(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "command-modes", minArgs = 1, maxArgs = 1, doc = "Return the modes COMMAND is defined for.\nIf COMMAND is not a command, the return value is nil.\nThe value, if non-nil, is a list of mode name symbols.")
    @GenerateNodeFactory
    public abstract static class FCommandModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object commandModes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "indirect-variable", minArgs = 1, maxArgs = 1, doc = "Return the variable at the end of OBJECT's variable chain.\nIf OBJECT is a symbol, follow its variable indirections (if any), and\nreturn the variable at the end of the chain of aliases.  See Info node\n`(elisp)Variable Aliases'.\n\nIf OBJECT is not a symbol, just return it.")
    @GenerateNodeFactory
    public abstract static class FIndirectVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object indirectVariable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "symbol-value", minArgs = 1, maxArgs = 1, doc = "Return SYMBOL's value.  Error if that is void.\nNote that if `lexical-binding' is in effect, this returns the\nglobal value outside of any lexical scope.")
    @GenerateNodeFactory
    public abstract static class FSymbolValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolValue(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set", minArgs = 2, maxArgs = 2, doc = "Set SYMBOL's value to NEWVAL, and return NEWVAL.")
    @GenerateNodeFactory
    public abstract static class FSet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object set(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "add-variable-watcher", minArgs = 2, maxArgs = 2, doc = "Cause WATCH-FUNCTION to be called when SYMBOL is about to be set.\n\nIt will be called with 4 arguments: (SYMBOL NEWVAL OPERATION WHERE).\nSYMBOL is the variable being changed.\nNEWVAL is the value it will be changed to.  (The variable still has\nthe old value when WATCH-FUNCTION is called.)\nOPERATION is a symbol representing the kind of change, one of: `set',\n`let', `unlet', `makunbound', and `defvaralias'.\nWHERE is a buffer if the buffer-local value of the variable is being\nchanged, nil otherwise.\n\nAll writes to aliases of SYMBOL will call WATCH-FUNCTION too.")
    @GenerateNodeFactory
    public abstract static class FAddVariableWatcher extends ELispBuiltInBaseNode {
        @Specialization
        public static Object addVariableWatcher(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "remove-variable-watcher", minArgs = 2, maxArgs = 2, doc = "Undo the effect of `add-variable-watcher'.\nRemove WATCH-FUNCTION from the list of functions to be called when\nSYMBOL (or its aliases) are set.")
    @GenerateNodeFactory
    public abstract static class FRemoveVariableWatcher extends ELispBuiltInBaseNode {
        @Specialization
        public static Object removeVariableWatcher(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-variable-watchers", minArgs = 1, maxArgs = 1, doc = "Return a list of SYMBOL's active watchers.")
    @GenerateNodeFactory
    public abstract static class FGetVariableWatchers extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getVariableWatchers(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "default-boundp", minArgs = 1, maxArgs = 1, doc = "Return t if SYMBOL has a non-void default value.\nA variable may have a buffer-local value.  This function says whether\nthe variable has a non-void value outside of the current buffer\ncontext.  Also see `default-value'.")
    @GenerateNodeFactory
    public abstract static class FDefaultBoundp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultBoundp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "default-value", minArgs = 1, maxArgs = 1, doc = "Return SYMBOL's default value.\nThis is the value that is seen in buffers that do not have their own values\nfor this variable.  The default value is meaningful for variables with\nlocal bindings in certain buffers.")
    @GenerateNodeFactory
    public abstract static class FDefaultValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultValue(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-default", minArgs = 2, maxArgs = 2, doc = "Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.\nThe default value is seen in buffers that do not have their own values\nfor this variable.")
    @GenerateNodeFactory
    public abstract static class FSetDefault extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefault(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-variable-buffer-local", minArgs = 1, maxArgs = 1, doc = "Make VARIABLE become buffer-local whenever it is set.\nAt any time, the value for the current buffer is in effect,\nunless the variable has never been set in this buffer,\nin which case the default value is in effect.\nNote that binding the variable with `let', or setting it while\na `let'-style binding made in this buffer is in effect,\ndoes not make the variable buffer-local.  Return VARIABLE.\n\nThis globally affects all uses of this variable, so it belongs together with\nthe variable declaration, rather than with its uses (if you just want to make\na variable local to the current buffer for one particular use, use\n`make-local-variable').  Buffer-local bindings are normally cleared\nwhile setting up a new major mode, unless they have a `permanent-local'\nproperty.\n\nThe function `default-value' gets the default value and `set-default' sets it.\n\nSee also `defvar-local'.")
    @GenerateNodeFactory
    public abstract static class FMakeVariableBufferLocal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeVariableBufferLocal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-local-variable", minArgs = 1, maxArgs = 1, doc = "Make VARIABLE have a separate value in the current buffer.\nOther buffers will continue to share a common default value.\n\\(The buffer-local value of VARIABLE starts out as the same value\nVARIABLE previously had.  If VARIABLE was void, it remains void.)\nReturn VARIABLE.\n\nIf the variable is already arranged to become local when set,\nthis function causes a local value to exist for this buffer,\njust as setting the variable would do.\n\nThis function returns VARIABLE, and therefore\n  (set (make-local-variable \\\\='VARIABLE) VALUE-EXP)\nworks.\n\nSee also `make-variable-buffer-local'.\n\nDo not use `make-local-variable' to make a hook variable buffer-local.\nInstead, use `add-hook' and specify t for the LOCAL argument.")
    @GenerateNodeFactory
    public abstract static class FMakeLocalVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeLocalVariable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-local-variable", minArgs = 1, maxArgs = 1, doc = "Make VARIABLE no longer have a separate value in the current buffer.\nFrom now on the default value will apply in this buffer.  Return VARIABLE.")
    @GenerateNodeFactory
    public abstract static class FKillLocalVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killLocalVariable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "local-variable-p", minArgs = 1, maxArgs = 2, doc = "Non-nil if VARIABLE has a local binding in buffer BUFFER.\nBUFFER defaults to the current buffer.\n\nAlso see `buffer-local-boundp'.")
    @GenerateNodeFactory
    public abstract static class FLocalVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object localVariableP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "local-variable-if-set-p", minArgs = 1, maxArgs = 2, doc = "Non-nil if VARIABLE is local in buffer BUFFER when set there.\nBUFFER defaults to the current buffer.\n\nMore precisely, return non-nil if either VARIABLE already has a local\nvalue in BUFFER, or if VARIABLE is automatically buffer-local (see\n`make-variable-buffer-local').")
    @GenerateNodeFactory
    public abstract static class FLocalVariableIfSetP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object localVariableIfSetP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "variable-binding-locus", minArgs = 1, maxArgs = 1, doc = "Return a value indicating where VARIABLE's current binding comes from.\nIf the current binding is buffer-local, the value is the current buffer.\nIf the current binding is global (the default), the value is nil.")
    @GenerateNodeFactory
    public abstract static class FVariableBindingLocus extends ELispBuiltInBaseNode {
        @Specialization
        public static Object variableBindingLocus(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "indirect-function", minArgs = 1, maxArgs = 2, doc = "Return the function at the end of OBJECT's function chain.\nIf OBJECT is not a symbol, just return it.  Otherwise, follow all\nfunction indirections to find the final function binding and return it.")
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

    @ELispBuiltIn(name = "aref", minArgs = 2, maxArgs = 2, doc = "Return the element of ARRAY at index IDX.\nARRAY may be a vector, a string, a char-table, a bool-vector, a record,\nor a byte-code object.  IDX starts at 0.")
    @GenerateNodeFactory
    public abstract static class FAref extends ELispBuiltInBaseNode {
        @Specialization
        public static Object aref(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "aset", minArgs = 3, maxArgs = 3, doc = "Store into the element of ARRAY at index IDX the value NEWELT.\nReturn NEWELT.  ARRAY may be a vector, a string, a char-table or a\nbool-vector.  IDX starts at 0.")
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

    @ELispBuiltIn(name = "=", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return t if args, all numbers or markers, are equal.\nusage: (= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "<", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return t if each arg (a number or marker), is less than the next arg.\nusage: (< NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = ">", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return t if each arg (a number or marker) is greater than the next arg.\nusage: (> NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "<=", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return t if each arg (a number or marker) is less than or equal to the next.\nusage: (<= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = ">=", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return t if each arg (a number or marker) is greater than or equal to the next.\nusage: (>= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "/=", minArgs = 2, maxArgs = 2, doc = "Return t if first arg is not equal to second arg.  Both must be numbers or markers.")
    @GenerateNodeFactory
    public abstract static class FNeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean neq(Object a, Object b) {
            return compareTo(a, b) != 0;
        }
    }

    @ELispBuiltIn(name = "number-to-string", minArgs = 1, maxArgs = 1, doc = "Return the decimal representation of NUMBER as a string.\nUses a minus sign if negative.\nNUMBER may be an integer or a floating point number.")
    @GenerateNodeFactory
    public abstract static class FNumberToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object numberToString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-number", minArgs = 1, maxArgs = 2, doc = "Parse STRING as a decimal number and return the number.\nIgnore leading spaces and tabs, and all trailing chars.  Return 0 if\nSTRING cannot be parsed as an integer or floating point number.\n\nIf BASE, interpret STRING as a number in that base.  If BASE isn't\npresent, base 10 is used.  BASE must be between 2 and 16 (inclusive).\nIf the base used is not 10, STRING is always parsed as an integer.")
    @GenerateNodeFactory
    public abstract static class FStringToNumber extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToNumber(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "+", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return sum of any number of arguments, which are numbers or markers.\nusage: (+ &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "-", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Negate number or subtract numbers or markers and return the result.\nWith one arg, negates it.  With more than one arg,\nsubtracts all but the first from the first.\nusage: (- &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "*", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return product of any number of arguments, which are numbers or markers.\nusage: (* &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "/", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Divide number by divisors and return the result.\nWith two or more arguments, return first argument divided by the rest.\nWith one argument, return 1 divided by the argument.\nThe arguments must be numbers or markers.\nusage: (/ NUMBER &rest DIVISORS)")
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

    @ELispBuiltIn(name = "%", minArgs = 2, maxArgs = 2, doc = "Return remainder of X divided by Y.\nBoth must be integers or markers.")
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

    @ELispBuiltIn(name = "mod", minArgs = 2, maxArgs = 2, doc = "Return X modulo Y.\nThe result falls between zero (inclusive) and Y (exclusive).\nBoth X and Y must be numbers or markers.")
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

    @ELispBuiltIn(name = "max", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return largest of all the arguments (which must be numbers or markers).\nThe value is always a number; markers are converted to numbers.\nusage: (max NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "min", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return smallest of all the arguments (which must be numbers or markers).\nThe value is always a number; markers are converted to numbers.\nusage: (min NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "logand", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return bitwise-and of all the arguments.\nArguments may be integers, or markers converted to integers.\nusage: (logand &rest INTS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "logior", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return bitwise-or of all the arguments.\nArguments may be integers, or markers converted to integers.\nusage: (logior &rest INTS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "logxor", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return bitwise-exclusive-or of all the arguments.\nArguments may be integers, or markers converted to integers.\nusage: (logxor &rest INTS-OR-MARKERS)")
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

    @ELispBuiltIn(name = "logcount", minArgs = 1, maxArgs = 1, doc = "Return population count of VALUE.\nThis is the number of one bits in the two's complement representation\nof VALUE.  If VALUE is negative, return the number of zero bits in the\nrepresentation.")
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

    @ELispBuiltIn(name = "ash", minArgs = 2, maxArgs = 2, doc = "Return integer VALUE with its bits shifted left by COUNT bit positions.\nIf COUNT is negative, shift VALUE to the right instead.\nVALUE and COUNT must be integers.\nMathematically, the return value is VALUE multiplied by 2 to the\npower of COUNT, rounded down.  If the result is non-zero, its sign\nis the same as that of VALUE.\nIn terms of bits, when COUNT is positive, the function moves\nthe bits of VALUE to the left, adding zero bits on the right; when\nCOUNT is negative, it moves the bits of VALUE to the right,\ndiscarding bits.")
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

    @ELispBuiltIn(name = "1+", minArgs = 1, maxArgs = 1, doc = "Return NUMBER plus one.  NUMBER may be a number or a marker.\nMarkers are converted to integers.")
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

    @ELispBuiltIn(name = "1-", minArgs = 1, maxArgs = 1, doc = "Return NUMBER minus one.  NUMBER may be a number or a marker.\nMarkers are converted to integers.")
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

    @ELispBuiltIn(name = "lognot", minArgs = 1, maxArgs = 1, doc = "Return the bitwise complement of NUMBER.  NUMBER must be an integer.")
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

    @ELispBuiltIn(name = "byteorder", minArgs = 0, maxArgs = 0, doc = "Return the byteorder for the machine.\nReturns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII\nlowercase l) for small endian machines.")
    @GenerateNodeFactory
    public abstract static class FByteorder extends ELispBuiltInBaseNode {
        @Specialization
        public static long byteorder() {
            return 'B';
        }
    }

    @ELispBuiltIn(name = "bool-vector-exclusive-or", minArgs = 2, maxArgs = 3, doc = "Return A ^ B, bitwise exclusive or.\nIf optional third argument C is given, store result into C.\nA, B, and C must be bool vectors of the same length.\nReturn the destination vector if it changed or nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorExclusiveOr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorExclusiveOr(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-union", minArgs = 2, maxArgs = 3, doc = "Return A | B, bitwise or.\nIf optional third argument C is given, store result into C.\nA, B, and C must be bool vectors of the same length.\nReturn the destination vector if it changed or nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorUnion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorUnion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-intersection", minArgs = 2, maxArgs = 3, doc = "Return A & B, bitwise and.\nIf optional third argument C is given, store result into C.\nA, B, and C must be bool vectors of the same length.\nReturn the destination vector if it changed or nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorIntersection extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorIntersection(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-set-difference", minArgs = 2, maxArgs = 3, doc = "Return A &~ B, set difference.\nIf optional third argument C is given, store result into C.\nA, B, and C must be bool vectors of the same length.\nReturn the destination vector if it changed or nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorSetDifference extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorSetDifference(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-subsetp", minArgs = 2, maxArgs = 2, doc = "Return t if every t value in A is also t in B, nil otherwise.\nA and B must be bool vectors of the same length.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorSubsetp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorSubsetp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-not", minArgs = 1, maxArgs = 2, doc = "Compute ~A, set complement.\nIf optional second argument B is given, store result into B.\nA and B must be bool vectors of the same length.\nReturn the destination vector.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorNot extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorNot(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-count-population", minArgs = 1, maxArgs = 1, doc = "Count how many elements in A are t.\nA is a bool vector.  To count A's nil elements, subtract the return\nvalue from A's length.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorCountPopulation extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorCountPopulation(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector-count-consecutive", minArgs = 3, maxArgs = 3, doc = "Count how many consecutive elements in A equal B starting at I.\nA is a bool vector, B is t or nil, and I is an index into A.")
    @GenerateNodeFactory
    public abstract static class FBoolVectorCountConsecutive extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVectorCountConsecutive(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }
}
