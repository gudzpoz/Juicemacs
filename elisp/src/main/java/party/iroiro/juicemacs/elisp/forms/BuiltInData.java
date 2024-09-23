package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.io.IOException;
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

    /**
     * <pre>
     * Return t if the two args are the same Lisp object.
     * </pre>
     */
    @ELispBuiltIn(name = "eq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eq(Object obj1, Object obj2) {
            // Simulate the Emacs behavior of packed integers
            if (obj1 instanceof Long) {
                return obj1.equals(obj2);
            }
            if (ELispSymbol.isNil(obj1)) {
                return ELispSymbol.isNil(obj2);
            }
            if (ELispSymbol.isT(obj1)) {
                return ELispSymbol.isT(obj2);
            }
            return obj1 == obj2;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is nil, and return nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "null", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNull extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean null_(Object object) {
            return ELispSymbol.isNil(object);
        }
    }

    /**
     * <pre>
     * Return a symbol representing the type of OBJECT.
     * The symbol returned names the object's basic type;
     * for example, (type-of 1) returns `integer'.
     * Contrary to `cl-type-of', the returned type is not always the most
     * precise type possible, because instead this function tries to preserve
     * compatibility with the return value of previous Emacs versions.
     * </pre>
     */
    @ELispBuiltIn(name = "type-of", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTypeOf extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol typeOf(Object object) {
            return switch (object) {
                case Boolean _, ELispSymbol _ -> SYMBOL;
                case Long _, ELispBigNum _ -> INTEGER;
                case ELispSubroutine _ -> SUBR;
                default -> FClTypeOf.clTypeOf(object);
            };
        }
    }

    /**
     * <pre>
     * Return a symbol representing the type of OBJECT.
     * The returned symbol names the most specific possible type of the object.
     * for example, (cl-type-of nil) returns `null'.
     * The specific type returned may change depending on Emacs versions,
     * so we recommend you use `cl-typep', `cl-typecase', or other predicates
     * rather than compare the return value of this function against
     * a fixed set of types.
     * </pre>
     */
    @ELispBuiltIn(name = "cl-type-of", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClTypeOf extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol clTypeOf(Object object) {
            return switch (object) {
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

    /**
     * <pre>
     * Return t if OBJECT is a cons cell.
     * </pre>
     */
    @ELispBuiltIn(name = "consp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FConsp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean consp(Object object) {
            return object instanceof ELispCons;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is not a cons cell.  This includes nil.
     * </pre>
     */
    @ELispBuiltIn(name = "atom", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAtom extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean atom(Object object) {
            return !(object instanceof ELispCons);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a list, that is, a cons cell or nil.
     * Otherwise, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "listp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FListp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean listp(Object object) {
            return object instanceof ELispCons || ELispSymbol.isNil(object);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is not a list.  Lists include nil.
     * </pre>
     */
    @ELispBuiltIn(name = "nlistp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNlistp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nlistp(Object object) {
            return !FListp.listp(object);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a symbol, but not a symbol together with position.
     * </pre>
     */
    @ELispBuiltIn(name = "bare-symbol-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBareSymbolP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean bareSymbolP(Object object) {
            return object instanceof ELispSymbol;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a symbol together with position.
     * Ignore `symbols-with-pos-enabled'.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-with-pos-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolWithPosP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean symbolWithPosP(Object object) {
            return true;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a symbol.
     * </pre>
     */
    @ELispBuiltIn(name = "symbolp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean symbolp(Object object) {
            return object instanceof ELispSymbol || object instanceof Boolean;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a keyword.
     * This means that it is a symbol with a print name beginning with `:'
     * interned in the initial obarray.
     * </pre>
     */
    @ELispBuiltIn(name = "keywordp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKeywordp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean keywordp(Object object) {
            if (object instanceof ELispSymbol symbol) {
                if (symbol.name().startsWith(":")) {
                    // TODO: if SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P ?
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a vector.
     * </pre>
     */
    @ELispBuiltIn(name = "vectorp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVectorp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean vectorp(Object object) {
            return object instanceof ELispVector;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a record.
     * </pre>
     */
    @ELispBuiltIn(name = "recordp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRecordp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean recordp(Object object) {
            return object instanceof ELispRecord;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a string.
     * </pre>
     */
    @ELispBuiltIn(name = "stringp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean stringp(Object object) {
            return object instanceof ELispString;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a multibyte string.
     * Return nil if OBJECT is either a unibyte string, or not a string.
     * </pre>
     */
    @ELispBuiltIn(name = "multibyte-string-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMultibyteStringP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean multibyteStringP(Object object) {
            return object instanceof ELispString s && s.isMultibyte();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a char-table.
     * </pre>
     */
    @ELispBuiltIn(name = "char-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charTableP(Object object) {
            return object instanceof ELispCharTable;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a char-table or vector.
     * </pre>
     */
    @ELispBuiltIn(name = "vector-or-char-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVectorOrCharTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean vectorOrCharTableP(Object object) {
            return object instanceof ELispVector || FCharTableP.charTableP(object);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a bool-vector.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBoolVectorP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean boolVectorP(Object object) {
            return object instanceof ELispBoolVector;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is an array (string or vector).
     * </pre>
     */
    @ELispBuiltIn(name = "arrayp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FArrayp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean arrayp(Object object) {
            return object instanceof ELispString || object instanceof ELispVector
                    || object instanceof ELispBoolVector || object instanceof ELispCharTable;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a sequence (list or array).
     * </pre>
     */
    @ELispBuiltIn(name = "sequencep", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSequencep extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean sequencep(Object object) {
            return FListp.listp(object) || FArrayp.arrayp(object);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is an editor buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "bufferp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a marker (editor pointer).
     * </pre>
     */
    @ELispBuiltIn(name = "markerp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMarkerp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void markerp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a module user pointer.
     * </pre>
     */
    @ELispBuiltIn(name = "user-ptrp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUserPtrp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void userPtrp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a built-in or native compiled Lisp function.
     *
     * See also `primitive-function-p' and `native-comp-function-p'.
     * </pre>
     */
    @ELispBuiltIn(name = "subrp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean subrp(Object object) {
            return object instanceof ELispSubroutine;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a function of type `closure'.
     * </pre>
     */
    @ELispBuiltIn(name = "closurep", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClosurep extends ELispBuiltInBaseNode {
        @Specialization
        public static Void closurep(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a byte-compiled function object.
     * </pre>
     */
    @ELispBuiltIn(name = "byte-code-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FByteCodeFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void byteCodeFunctionP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a function of type `interpreted-function'.
     * </pre>
     */
    @ELispBuiltIn(name = "interpreted-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInterpretedFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void interpretedFunctionP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a function loaded from a dynamic module.
     * </pre>
     */
    @ELispBuiltIn(name = "module-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FModuleFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void moduleFunctionP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a character or a string.
     * </pre>
     */
    @ELispBuiltIn(name = "char-or-string-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharOrStringP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean charOrStringP(Object object) {
            return object instanceof ELispString
                    || ELispString.toValidChar(object) != null;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "integerp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIntegerp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean integerp(Object object) {
            return object instanceof Long || object instanceof ELispBigNum;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is an integer or a marker (editor pointer).
     * </pre>
     */
    @ELispBuiltIn(name = "integer-or-marker-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIntegerOrMarkerP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void integerOrMarkerP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a nonnegative integer.
     * </pre>
     */
    @ELispBuiltIn(name = "natnump", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNatnump extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean natnump(Object object) {
            return (object instanceof Long l && l >= 0)
                    || (object instanceof ELispBigNum n && n.value.signum() >= 0);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a number (floating point or integer).
     * </pre>
     */
    @ELispBuiltIn(name = "numberp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumberp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean numberp(Object object) {
            return FIntegerp.integerp(object) || FFloatp.floatp(object);
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a number or a marker.
     * </pre>
     */
    @ELispBuiltIn(name = "number-or-marker-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumberOrMarkerP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void numberOrMarkerP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a floating point number.
     * </pre>
     */
    @ELispBuiltIn(name = "floatp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFloatp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean floatp(Object object) {
            return object instanceof Double;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a thread.
     * </pre>
     */
    @ELispBuiltIn(name = "threadp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FThreadp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void threadp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a mutex.
     * </pre>
     */
    @ELispBuiltIn(name = "mutexp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMutexp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mutexp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a condition variable.
     * </pre>
     */
    @ELispBuiltIn(name = "condition-variable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FConditionVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void conditionVariableP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the car of LIST.  If LIST is nil, return nil.
     * Error if LIST is not nil and not a cons cell.  See also `car-safe'.
     *
     * See Info node `(elisp)Cons Cells' for a discussion of related basic
     * Lisp concepts such as car, cdr, cons cell and list.
     * </pre>
     */
    @ELispBuiltIn(name = "car", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object car(Object list) {
            return switch (list) {
                case ELispCons cons -> cons.car();
                case Boolean b when !b -> false;
                case ELispSymbol sym when sym == NIL -> false;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    /**
     * <pre>
     * Return the car of OBJECT if it is a cons cell, or else nil.
     * </pre>
     */
    @ELispBuiltIn(name = "car-safe", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCarSafe extends ELispBuiltInBaseNode {
        @Specialization
        public static Object carSafe(Object object) {
            if (object instanceof ELispCons cons) {
                return cons.car();
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return the cdr of LIST.  If LIST is nil, return nil.
     * Error if LIST is not nil and not a cons cell.  See also `cdr-safe'.
     *
     * See Info node `(elisp)Cons Cells' for a discussion of related basic
     * Lisp concepts such as cdr, car, cons cell and list.
     * </pre>
     */
    @ELispBuiltIn(name = "cdr", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cdr(Object list) {
            return switch (list) {
                case ELispCons cons -> cons.cdr();
                case Boolean b when !b -> false;
                case ELispSymbol sym when sym == NIL -> false;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    /**
     * <pre>
     * Return the cdr of OBJECT if it is a cons cell, or else nil.
     * </pre>
     */
    @ELispBuiltIn(name = "cdr-safe", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCdrSafe extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cdrSafe(Object object) {
            if (object instanceof ELispCons cons) {
                return cons.cdr();
            }
            return false;
        }
    }

    /**
     * <pre>
     * Set the car of CELL to be NEWCAR.  Returns NEWCAR.
     * </pre>
     */
    @ELispBuiltIn(name = "setcar", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetcar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setcar(ELispCons cell, Object newcar) {
            cell.setCar(newcar);
            return newcar;
        }
    }

    /**
     * <pre>
     * Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.
     * </pre>
     */
    @ELispBuiltIn(name = "setcdr", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetcdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setcdr(ELispCons cell, Object newcdr) {
            cell.setCdr(newcdr);
            return newcdr;
        }
    }

    /**
     * <pre>
     * Return t if SYMBOL's value is not void.
     * Note that if `lexical-binding' is in effect, this refers to the
     * global value outside of any lexical scope.
     * </pre>
     */
    @ELispBuiltIn(name = "boundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBoundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean boundp(ELispSymbol symbol) {
            return symbol.isBound();
        }
    }

    /**
     * <pre>
     * Return t if SYMBOL's function definition is not void.
     * </pre>
     */
    @ELispBuiltIn(name = "fboundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFboundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean fboundp(ELispSymbol symbol) {
            return !ELispSymbol.isNil(symbol.getFunction());
        }
    }

    /**
     * <pre>
     * Empty out the value cell of SYMBOL, making it void as a variable.
     * Return SYMBOL.
     *
     * If a variable is void, trying to evaluate the variable signals a
     * `void-variable' error, instead of returning a value.  For more
     * details, see Info node `(elisp) Void Variables'.
     *
     * See also `fmakunbound'.
     * </pre>
     */
    @ELispBuiltIn(name = "makunbound", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol makunbound(ELispSymbol symbol) {
            symbol.makeUnbound();
            return symbol;
        }
    }

    /**
     * <pre>
     * Make SYMBOL's function definition be void.
     * Return SYMBOL.
     *
     * If a function definition is void, trying to call a function by that
     * name will cause a `void-function' error.  For more details, see Info
     * node `(elisp) Function Cells'.
     *
     * See also `makunbound'.
     * </pre>
     */
    @ELispBuiltIn(name = "fmakunbound", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFmakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol fmakunbound(ELispSymbol symbol) {
            symbol.setFunction(false);
            return symbol;
        }
    }

    /**
     * <pre>
     * Return SYMBOL's function definition, or nil if that is void.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-function", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolFunction(ELispSymbol symbol) {
            return symbol.getFunction();
        }
    }

    /**
     * <pre>
     * Return SYMBOL's property list.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-plist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object symbolPlist(ELispSymbol symbol) {
            return symbol.getProperties();
        }
    }

    /**
     * <pre>
     * Return SYMBOL's name, a string.
     *
     * Warning: never alter the string returned by `symbol-name'.
     * Doing that might make Emacs dysfunctional, and might even crash Emacs.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString symbolName(ELispSymbol symbol) {
            return new ELispString(symbol.name());
        }
    }

    /**
     * <pre>
     * Extract, if need be, the bare symbol from SYM.
     * SYM is either a symbol or a symbol with position.
     * Ignore `symbols-with-pos-enabled'.
     * </pre>
     */
    @ELispBuiltIn(name = "bare-symbol", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBareSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol bareSymbol(ELispSymbol sym) {
            return sym;
        }
    }

    /**
     * <pre>
     * Extract the position from the symbol with position SYMPOS.
     * Ignore `symbols-with-pos-enabled'.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-with-pos-pos", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolWithPosPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Void symbolWithPosPos(Object sympos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * If ARG is a symbol with position, return it without the position.
     * Otherwise, return ARG unchanged.  Ignore `symbols-with-pos-enabled'.
     * Compare with `bare-symbol'.
     * </pre>
     */
    @ELispBuiltIn(name = "remove-pos-from-symbol", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRemovePosFromSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Void removePosFromSymbol(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make a new symbol with position.
     * SYM is a symbol, with or without position, the symbol to position.
     * POS, the position, is either a nonnegative fixnum,
     * or a symbol with position from which the position will be taken.
     * Ignore `symbols-with-pos-enabled'.
     * </pre>
     */
    @ELispBuiltIn(name = "position-symbol", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPositionSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Void positionSymbol(Object sym, Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
     * If the resulting chain of function definitions would contain a loop,
     * signal a `cyclic-function-indirection' error.
     * </pre>
     */
    @ELispBuiltIn(name = "fset", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFset extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol fset(ELispSymbol symbol, Object definition) {
            symbol.setFunction(definition);
            return symbol;
        }
    }

    /**
     * <pre>
     * Set SYMBOL's function definition to DEFINITION.
     * Associates the function with the current load file, if any.
     * The optional third argument DOCSTRING specifies the documentation string
     * for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
     * determined by DEFINITION.
     *
     * Internally, this normally uses `fset', but if SYMBOL has a
     * `defalias-fset-function' property, the associated value is used instead.
     *
     * The return value is undefined.
     * </pre>
     */
    @ELispBuiltIn(name = "defalias", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefalias extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol defalias(ELispSymbol symbol, Object definition, Object docstring) {
            // TODO: Handle defalias-fset-function
            System.out.println(symbol);
            FFset.fset(symbol, definition);
            return symbol;
        }
    }

    /**
     * <pre>
     * Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.
     * </pre>
     */
    @ELispBuiltIn(name = "setplist", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetplist extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setplist(Object symbol, Object newplist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return minimum and maximum number of args allowed for SUBR.
     * SUBR must be a built-in function.
     * The returned value is a pair (MIN . MAX).  MIN is the minimum number
     * of args.  MAX is the maximum number or the symbol `many', for a
     * function with `&amp;rest' args, or `unevalled' for a special form.
     * </pre>
     */
    @ELispBuiltIn(name = "subr-arity", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrArity extends ELispBuiltInBaseNode {
        @Specialization
        public static Void subrArity(Object subr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return name of subroutine SUBR.
     * SUBR must be a built-in function.
     * </pre>
     */
    @ELispBuiltIn(name = "subr-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void subrName(Object subr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if the object is native compiled Lisp function, nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "native-comp-function-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNativeCompFunctionP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nativeCompFunctionP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the lambda list for a native compiled lisp/d
     * function or t otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "subr-native-lambda-list", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrNativeLambdaList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void subrNativeLambdaList(Object subr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the type of SUBR.
     * </pre>
     */
    @ELispBuiltIn(name = "subr-type", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void subrType(Object subr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the native compilation unit.
     * </pre>
     */
    @ELispBuiltIn(name = "subr-native-comp-unit", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrNativeCompUnit extends ELispBuiltInBaseNode {
        @Specialization
        public static Void subrNativeCompUnit(Object subr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the file of the native compilation unit.
     * </pre>
     */
    @ELispBuiltIn(name = "native-comp-unit-file", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNativeCompUnitFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nativeCompUnitFile(Object compUnit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the file of the native compilation unit.
     * </pre>
     */
    @ELispBuiltIn(name = "native-comp-unit-set-file", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNativeCompUnitSetFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nativeCompUnitSetFile(Object compUnit, Object newFile) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the interactive form of CMD or nil if none.
     * If CMD is not a command, the return value is nil.
     * Value, if non-nil, is a list (interactive SPEC).
     * </pre>
     */
    @ELispBuiltIn(name = "interactive-form", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInteractiveForm extends ELispBuiltInBaseNode {
        @Specialization
        public static Void interactiveForm(Object cmd) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the modes COMMAND is defined for.
     * If COMMAND is not a command, the return value is nil.
     * The value, if non-nil, is a list of mode name symbols.
     * </pre>
     */
    @ELispBuiltIn(name = "command-modes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCommandModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void commandModes(Object command) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the variable at the end of OBJECT's variable chain.
     * If OBJECT is a symbol, follow its variable indirections (if any), and
     * return the variable at the end of the chain of aliases.  See Info node
     * `(elisp)Variable Aliases'.
     *
     * If OBJECT is not a symbol, just return it.
     * </pre>
     */
    @ELispBuiltIn(name = "indirect-variable", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIndirectVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void indirectVariable(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return SYMBOL's value.  Error if that is void.
     * Note that if `lexical-binding' is in effect, this returns the
     * global value outside of any lexical scope.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Void symbolValue(Object symbol) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set SYMBOL's value to NEWVAL, and return NEWVAL.
     * </pre>
     */
    @ELispBuiltIn(name = "set", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object set(ELispSymbol symbol, Object newval) {
            symbol.setValue(newval);
            return newval;
        }
    }

    /**
     * <pre>
     * Cause WATCH-FUNCTION to be called when SYMBOL is about to be set.
     *
     * It will be called with 4 arguments: (SYMBOL NEWVAL OPERATION WHERE).
     * SYMBOL is the variable being changed.
     * NEWVAL is the value it will be changed to.  (The variable still has
     * the old value when WATCH-FUNCTION is called.)
     * OPERATION is a symbol representing the kind of change, one of: `set',
     * `let', `unlet', `makunbound', and `defvaralias'.
     * WHERE is a buffer if the buffer-local value of the variable is being
     * changed, nil otherwise.
     *
     * All writes to aliases of SYMBOL will call WATCH-FUNCTION too.
     * </pre>
     */
    @ELispBuiltIn(name = "add-variable-watcher", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAddVariableWatcher extends ELispBuiltInBaseNode {
        @Specialization
        public static Void addVariableWatcher(Object symbol, Object watchFunction) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Undo the effect of `add-variable-watcher'.
     * Remove WATCH-FUNCTION from the list of functions to be called when
     * SYMBOL (or its aliases) are set.
     * </pre>
     */
    @ELispBuiltIn(name = "remove-variable-watcher", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRemoveVariableWatcher extends ELispBuiltInBaseNode {
        @Specialization
        public static Void removeVariableWatcher(Object symbol, Object watchFunction) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of SYMBOL's active watchers.
     * </pre>
     */
    @ELispBuiltIn(name = "get-variable-watchers", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetVariableWatchers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getVariableWatchers(Object symbol) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if SYMBOL has a non-void default value.
     * A variable may have a buffer-local value.  This function says whether
     * the variable has a non-void value outside of the current buffer
     * context.  Also see `default-value'.
     * </pre>
     */
    @ELispBuiltIn(name = "default-boundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDefaultBoundp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean defaultBoundp(ELispSymbol symbol) {
            return symbol.isDefaultBound();
        }
    }

    /**
     * <pre>
     * Return SYMBOL's default value.
     * This is the value that is seen in buffers that do not have their own values
     * for this variable.  The default value is meaningful for variables with
     * local bindings in certain buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "default-value", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDefaultValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultValue(ELispSymbol symbol) {
            return symbol.getDefaultValue();
        }
    }

    /**
     * <pre>
     * Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
     * The default value is seen in buffers that do not have their own values
     * for this variable.
     * </pre>
     */
    @ELispBuiltIn(name = "set-default", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetDefault extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefault(ELispSymbol symbol, Object value) {
            symbol.setDefaultValue(value);
            return value;
        }
    }

    /**
     * <pre>
     * Make VARIABLE become buffer-local whenever it is set.
     * At any time, the value for the current buffer is in effect,
     * unless the variable has never been set in this buffer,
     * in which case the default value is in effect.
     * Note that binding the variable with `let', or setting it while
     * a `let'-style binding made in this buffer is in effect,
     * does not make the variable buffer-local.  Return VARIABLE.
     *
     * This globally affects all uses of this variable, so it belongs together with
     * the variable declaration, rather than with its uses (if you just want to make
     * a variable local to the current buffer for one particular use, use
     * `make-local-variable').  Buffer-local bindings are normally cleared
     * while setting up a new major mode, unless they have a `permanent-local'
     * property.
     *
     * The function `default-value' gets the default value and `set-default' sets it.
     *
     * See also `defvar-local'.
     * </pre>
     */
    @ELispBuiltIn(name = "make-variable-buffer-local", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeVariableBufferLocal extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol makeVariableBufferLocal(ELispSymbol variable) {
            variable.setBufferLocal(true);
            return variable;
        }
    }

    /**
     * <pre>
     * Make VARIABLE have a separate value in the current buffer.
     * Other buffers will continue to share a common default value.
     * \(The buffer-local value of VARIABLE starts out as the same value
     * VARIABLE previously had.  If VARIABLE was void, it remains void.)
     * Return VARIABLE.
     *
     * If the variable is already arranged to become local when set,
     * this function causes a local value to exist for this buffer,
     * just as setting the variable would do.
     *
     * This function returns VARIABLE, and therefore
     *   (set (make-local-variable \\='VARIABLE) VALUE-EXP)
     * works.
     *
     * See also `make-variable-buffer-local'.
     *
     * Do not use `make-local-variable' to make a hook variable buffer-local.
     * Instead, use `add-hook' and specify t for the LOCAL argument.
     * </pre>
     */
    @ELispBuiltIn(name = "make-local-variable", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeLocalVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol makeLocalVariable(ELispSymbol variable) {
            variable.setBufferLocal(false);
            return variable;
        }
    }

    /**
     * <pre>
     * Make VARIABLE no longer have a separate value in the current buffer.
     * From now on the default value will apply in this buffer.  Return VARIABLE.
     * </pre>
     */
    @ELispBuiltIn(name = "kill-local-variable", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKillLocalVariable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void killLocalVariable(Object variable) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Non-nil if VARIABLE has a local binding in buffer BUFFER.
     * BUFFER defaults to the current buffer.
     *
     * Also see `buffer-local-boundp'.
     * </pre>
     */
    @ELispBuiltIn(name = "local-variable-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLocalVariableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean localVariableP(ELispSymbol variable, Object buffer) {
            return variable.isBufferLocal(buffer);
        }
    }

    /**
     * <pre>
     * Non-nil if VARIABLE is local in buffer BUFFER when set there.
     * BUFFER defaults to the current buffer.
     *
     * More precisely, return non-nil if either VARIABLE already has a local
     * value in BUFFER, or if VARIABLE is automatically buffer-local (see
     * `make-variable-buffer-local').
     * </pre>
     */
    @ELispBuiltIn(name = "local-variable-if-set-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLocalVariableIfSetP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean localVariableIfSetP(ELispSymbol variable, Object buffer) {
            return variable.isBufferLocalIfSet(buffer);
        }
    }

    /**
     * <pre>
     * Return a value indicating where VARIABLE's current binding comes from.
     * If the current binding is buffer-local, the value is the current buffer.
     * If the current binding is global (the default), the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "variable-binding-locus", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVariableBindingLocus extends ELispBuiltInBaseNode {
        @Specialization
        public static Void variableBindingLocus(Object variable) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the function at the end of OBJECT's function chain.
     * If OBJECT is not a symbol, just return it.  Otherwise, follow all
     * function indirections to find the final function binding and return it.
     * </pre>
     */
    @ELispBuiltIn(name = "indirect-function", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FIndirectFunction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object indirectFunction(Object object, Object noerror) {
            if (object instanceof ELispSymbol symbol) {
                // noerror is ignored by GNU Emacs?
                return symbol.getIndirectFunction();
            }
            return object;
        }
    }

    /**
     * <pre>
     * Return the element of ARRAY at index IDX.
     * ARRAY may be a vector, a string, a char-table, a bool-vector, a record,
     * or a byte-code object.  IDX starts at 0.
     * </pre>
     */
    @ELispBuiltIn(name = "aref", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAref extends ELispBuiltInBaseNode {
        @Specialization
        public static Void aref(Object array, Object idx) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Store into the element of ARRAY at index IDX the value NEWELT.
     * Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
     * bool-vector.  IDX starts at 0.
     * </pre>
     */
    @ELispBuiltIn(name = "aset", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object aset(ELispVector array, long idx, Object newelt) {
            array.set((int) idx, newelt);
            return newelt;
        }
    }

    /**
     * <pre>
     * Return t if args, all numbers or markers, are equal.
     * usage: (= NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "=", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FEqlsign extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static boolean eqlsignLong(long numberOrMarker, Object[] numbersOrMarkers) {
            for (Object arg : numbersOrMarkers) {
                if (numberOrMarker != (Long) arg) {
                    return false;
                }
            }
            return true;
        }

        @Specialization(rewriteOn = ClassCastException.class)
        public static boolean eqlsignDouble(double numberOrMarker, Object[] numbersOrMarkers) {
            for (Object arg : numbersOrMarkers) {
                if (arg instanceof Long l) {
                    if (numberOrMarker != l) {
                        return false;
                    }
                } else if (numberOrMarker != (Double) arg) {
                    return false;
                }
            }
            return true;
        }

        @Specialization(replaces = {"eqlsignLong", "eqlsignDouble"})
        public static boolean eqlsign(Object numberOrMarker, Object[] numbersOrMarkers) {
            for (Object arg : numbersOrMarkers) {
                if (compareTo(numberOrMarker, arg) != 0) {
                    return false;
                }
            }
            return true;
        }
    }

    /**
     * <pre>
     * Return t if each arg (a number or marker), is less than the next arg.
     * usage: (&lt; NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "<", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLss extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean lss(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if (compareTo(prev, arg) >= 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    /**
     * <pre>
     * Return t if each arg (a number or marker) is greater than the next arg.
     * usage: (&gt; NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = ">", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FGtr extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean gtr(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if (compareTo(prev, arg) <= 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    /**
     * <pre>
     * Return t if each arg (a number or marker) is less than or equal to the next.
     * usage: (&lt;= NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "<=", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean leq(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if (compareTo(prev, arg) > 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    /**
     * <pre>
     * Return t if each arg (a number or marker) is greater than or equal to the next.
     * usage: (&gt;= NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = ">=", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FGeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean geq(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if (compareTo(prev, arg) < 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }
    }

    /**
     * <pre>
     * Return t if first arg is not equal to second arg.  Both must be numbers or markers.
     * </pre>
     */
    @ELispBuiltIn(name = "/=", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNeq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean neq(Object num1, Object num2) {
            return compareTo(num1, num2) != 0;
        }
    }

    /**
     * <pre>
     * Return the decimal representation of NUMBER as a string.
     * Uses a minus sign if negative.
     * NUMBER may be an integer or a floating point number.
     * </pre>
     */
    @ELispBuiltIn(name = "number-to-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumberToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void numberToString(Object number) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Parse STRING as a decimal number and return the number.
     * Ignore leading spaces and tabs, and all trailing chars.  Return 0 if
     * STRING cannot be parsed as an integer or floating point number.
     *
     * If BASE, interpret STRING as a number in that base.  If BASE isn't
     * present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
     * If the base used is not 10, STRING is always parsed as an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "string-to-number", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringToNumber extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToNumber(ELispString string, Object base) {
            String s = string.toString();
            long b = ELispSymbol.or(base, 10);
            if (b != 10) {
                s = s.trim();
                s = "#" + b + "r" + s;
            }
            try {
                Object read = ELispParser.read(s);
                if (read instanceof Long || read instanceof Double || read instanceof ELispBigNum) {
                    return read;
                }
            } catch (IOException ignored) {
            }
            return 0;
        }
    }

    /**
     * <pre>
     * Return sum of any number of arguments, which are numbers or markers.
     * usage: (+ &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "+", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FPlus extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long plusLong(Object[] numbersOrMarkers) {
            long sum = 0;
            for (Object arg : numbersOrMarkers) {
                sum = Math.addExact(sum, (Long) arg);
            }
            return sum;
        }

        @Specialization(replaces = "plusLong")
        public static Object plusAny(Object[] numbersOrMarkers) {
            return tryAddLong(numbersOrMarkers);
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

    /**
     * <pre>
     * Negate number or subtract numbers or markers and return the result.
     * With one arg, negates it.  With more than one arg,
     * subtracts all but the first from the first.
     * usage: (- &amp;optional NUMBER-OR-MARKER &amp;rest MORE-NUMBERS-OR-MARKERS)
     * </pre>
     */
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

    /**
     * <pre>
     * Return product of any number of arguments, which are numbers or markers.
     * usage: (* &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "*", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FTimes extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long timesLong(Object[] numbersOrMarkers) {
            long result = 1;
            for (Object arg : numbersOrMarkers) {
                result = Math.multiplyExact(result, (Long) arg);
            }
            return result;
        }

        @Specialization(replaces = "timesLong")
        public static Object timesAny(Object[] numbersOrMarkers) {
            return tryTimesLong(numbersOrMarkers);
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

    /**
     * <pre>
     * Divide number by divisors and return the result.
     * With two or more arguments, return first argument divided by the rest.
     * With one argument, return 1 divided by the argument.
     * The arguments must be numbers or markers.
     * usage: (/ NUMBER &amp;rest DIVISORS)
     * </pre>
     */
    @ELispBuiltIn(name = "/", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FQuo extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public static long quoLong(long number, Object[] divisors) {
            long result = number;
            for (Object arg : divisors) {
                result /= (Long) arg;
            }
            return result;
        }

        @Specialization(replaces = "quoLong")
        public static Object quoAny(Object number, Object[] divisors) {
            for (Object arg : divisors) {
                if (arg instanceof Double) {
                    return tryQuoDouble(ELispTypeSystemGen.asImplicitDouble(number), divisors);
                }
            }
            return switch (number) {
                case Long l -> tryQuoLong(l, divisors);
                case ELispBigNum n -> tryQuoBigNum(n.value, 0, divisors);
                case Double d -> tryQuoDouble(d, divisors);
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

    /**
     * <pre>
     * Return remainder of X divided by Y.
     * Both must be integers or markers.
     * </pre>
     */
    @ELispBuiltIn(name = "%", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRem extends ELispBuiltInBaseNode {
        @Specialization
        public static long remLong(long x, long y) {
            return x % y;
        }

        @Specialization
        public static Object rem(ELispBigNum x, ELispBigNum y) {
            return ELispBigNum.wrap(x.value.remainder(y.value));
        }
    }

    /**
     * <pre>
     * Return X modulo Y.
     * The result falls between zero (inclusive) and Y (exclusive).
     * Both X and Y must be numbers or markers.
     * </pre>
     */
    @ELispBuiltIn(name = "mod", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMod extends ELispBuiltInBaseNode {
        @Specialization
        public static long modLong(long x, long y) {
            return Long.remainderUnsigned(x, y);
        }

        @Specialization
        public static Object mod(ELispBigNum x, ELispBigNum y) {
            return ELispBigNum.wrap(x.value.mod(y.value));
        }
    }

    /**
     * <pre>
     * Return largest of all the arguments (which must be numbers or markers).
     * The value is always a number; markers are converted to numbers.
     * usage: (max NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "max", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMax extends ELispBuiltInBaseNode {
        @Specialization
        public static Object max(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object result = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if (compareTo(result, arg) < 0) {
                    result = arg;
                }
            }
            return result;
        }
    }

    /**
     * <pre>
     * Return smallest of all the arguments (which must be numbers or markers).
     * The value is always a number; markers are converted to numbers.
     * usage: (min NUMBER-OR-MARKER &amp;rest NUMBERS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "min", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMin extends ELispBuiltInBaseNode {
        @Specialization
        public static Object min(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object result = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if (compareTo(result, arg) > 0) {
                    result = arg;
                }
            }
            return result;
        }
    }

    /**
     * <pre>
     * Return bitwise-and of all the arguments.
     * Arguments may be integers, or markers converted to integers.
     * usage: (logand &amp;rest INTS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "logand", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLogand extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logandLong(Object[] intsOrMarkers) {
            long result = -1;
            for (Object arg : intsOrMarkers) {
                result &= (long) arg;
            }
            return result;
        }

        @Specialization(replaces = "logandLong")
        public static Object logand(Object[] intsOrMarkers) {
            BigInteger result = BigInteger.ONE.negate();
            for (Object arg : intsOrMarkers) {
                result = result.and(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.value;
                    default -> throw new IllegalArgumentException();
                });
            }
            return ELispBigNum.wrap(result);
        }
    }

    /**
     * <pre>
     * Return bitwise-or of all the arguments.
     * Arguments may be integers, or markers converted to integers.
     * usage: (logior &amp;rest INTS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "logior", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLogior extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logorLong(Object[] intsOrMarkers) {
            long result = 0;
            for (Object arg : intsOrMarkers) {
                result |= (long) arg;
            }
            return result;
        }

        @Specialization(replaces = "logorLong")
        public static Object logior(Object[] intsOrMarkers) {
            BigInteger result = BigInteger.ZERO;
            for (Object arg : intsOrMarkers) {
                result = result.or(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.value;
                    default -> throw new IllegalArgumentException();
                });
            }
            return ELispBigNum.wrap(result);
        }
    }

    /**
     * <pre>
     * Return bitwise-exclusive-or of all the arguments.
     * Arguments may be integers, or markers converted to integers.
     * usage: (logxor &amp;rest INTS-OR-MARKERS)
     * </pre>
     */
    @ELispBuiltIn(name = "logxor", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FLogxor extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logxorLong(Object[] intsOrMarkers) {
            long result = 0;
            for (Object arg : intsOrMarkers) {
                result ^= (long) arg;
            }
            return result;
        }

        @Specialization(replaces = "logxorLong")
        public static Object logxor(Object[] intsOrMarkers) {
            BigInteger result = BigInteger.ZERO;
            for (Object arg : intsOrMarkers) {
                result = result.xor(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.value;
                    default -> throw new IllegalArgumentException();
                });
            }
            return ELispBigNum.wrap(result);
        }
    }

    /**
     * <pre>
     * Return population count of VALUE.
     * This is the number of one bits in the two's complement representation
     * of VALUE.  If VALUE is negative, return the number of zero bits in the
     * representation.
     * </pre>
     */
    @ELispBuiltIn(name = "logcount", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLogcount extends ELispBuiltInBaseNode {
        @Specialization
        public static long logcountLong(long value) {
            return value >= 0 ? Long.bitCount(value) : Long.SIZE - Long.bitCount(value);
        }

        @Specialization
        public static long logcount(ELispBigNum value) {
            return value.value.bitCount();
        }
    }

    /**
     * <pre>
     * Return integer VALUE with its bits shifted left by COUNT bit positions.
     * If COUNT is negative, shift VALUE to the right instead.
     * VALUE and COUNT must be integers.
     * Mathematically, the return value is VALUE multiplied by 2 to the
     * power of COUNT, rounded down.  If the result is non-zero, its sign
     * is the same as that of VALUE.
     * In terms of bits, when COUNT is positive, the function moves
     * the bits of VALUE to the left, adding zero bits on the right; when
     * COUNT is negative, it moves the bits of VALUE to the right,
     * discarding bits.
     * </pre>
     */
    @ELispBuiltIn(name = "ash", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAsh extends ELispBuiltInBaseNode {
        @Specialization
        public static Object ashLong(long value, long count) {
            if (count == 0) {
                return value;
            }
            if (count < 0) {
                long shift = -count;
                if (shift >= Long.SIZE) {
                    return value < 0 ? -1L : 0L;
                }
                return value >> shift;
            }
            BigInteger v = BigInteger.valueOf(value);
            return ELispBigNum.wrap(v.shiftLeft((int) count));
        }

        @Specialization
        public static Object ash(ELispBigNum value, long count) {
            return ELispBigNum.wrap(value.value.shiftLeft((int) count));
        }
    }

    /**
     * <pre>
     * Return NUMBER plus one.  NUMBER may be a number or a marker.
     * Markers are converted to integers.
     * </pre>
     */
    @ELispBuiltIn(name = "1+", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAdd1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object add1(Object number) {
            return switch (number) {
                case Long l when l < Long.MAX_VALUE -> l + 1;
                case Long l -> ELispBigNum.wrap(BigInteger.valueOf(l).add(BigInteger.ONE));
                case Double d -> d + 1;
                case ELispBigNum n -> ELispBigNum.wrap(n.value.add(BigInteger.ONE));
                default -> throw new IllegalArgumentException();
            };
        }
    }

    /**
     * <pre>
     * Return NUMBER minus one.  NUMBER may be a number or a marker.
     * Markers are converted to integers.
     * </pre>
     */
    @ELispBuiltIn(name = "1-", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSub1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sub1(Object number) {
            return switch (number) {
                case Long l when l > Long.MIN_VALUE -> l - 1;
                case Long l -> ELispBigNum.wrap(BigInteger.valueOf(l).subtract(BigInteger.ONE));
                case Double d -> d - 1;
                case ELispBigNum n -> ELispBigNum.wrap(n.value.subtract(BigInteger.ONE));
                default -> throw new IllegalArgumentException();
            };
        }
    }

    /**
     * <pre>
     * Return the bitwise complement of NUMBER.  NUMBER must be an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "lognot", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLognot extends ELispBuiltInBaseNode {
        @Specialization
        public static long lognot(long number) {
            return ~number;
        }

        @Specialization
        public static Object lognot(ELispBigNum number) {
            return ELispBigNum.wrap(number.value.not());
        }
    }

    /**
     * <pre>
     * Return the byteorder for the machine.
     * Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII
     * lowercase l) for small endian machines.
     * </pre>
     */
    @ELispBuiltIn(name = "byteorder", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FByteorder extends ELispBuiltInBaseNode {
        @Specialization
        public static long byteorder() {
            return 'B';
        }
    }

    /**
     * <pre>
     * Return A ^ B, bitwise exclusive or.
     * If optional third argument C is given, store result into C.
     * A, B, and C must be bool vectors of the same length.
     * Return the destination vector if it changed or nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-exclusive-or", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorExclusiveOr extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorExclusiveOr(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return A | B, bitwise or.
     * If optional third argument C is given, store result into C.
     * A, B, and C must be bool vectors of the same length.
     * Return the destination vector if it changed or nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-union", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorUnion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorUnion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return A &amp; B, bitwise and.
     * If optional third argument C is given, store result into C.
     * A, B, and C must be bool vectors of the same length.
     * Return the destination vector if it changed or nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-intersection", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorIntersection extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorIntersection(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return A &amp;~ B, set difference.
     * If optional third argument C is given, store result into C.
     * A, B, and C must be bool vectors of the same length.
     * Return the destination vector if it changed or nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-set-difference", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorSetDifference extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorSetDifference(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if every t value in A is also t in B, nil otherwise.
     * A and B must be bool vectors of the same length.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-subsetp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBoolVectorSubsetp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorSubsetp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Compute ~A, set complement.
     * If optional second argument B is given, store result into B.
     * A and B must be bool vectors of the same length.
     * Return the destination vector.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-not", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBoolVectorNot extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorNot(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Count how many elements in A are t.
     * A is a bool vector.  To count A's nil elements, subtract the return
     * value from A's length.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-count-population", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBoolVectorCountPopulation extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorCountPopulation(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Count how many consecutive elements in A equal B starting at I.
     * A is a bool vector, B is t or nil, and I is an index into A.
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector-count-consecutive", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBoolVectorCountConsecutive extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVectorCountConsecutive(Object a, Object b, Object i) {
            throw new UnsupportedOperationException();
        }
    }
}
