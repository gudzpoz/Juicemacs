package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispFrame;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.Collections;
import java.util.NoSuchElementException;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_CHAR;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;

/**
 * Type system for ELisp
 *
 * <p>
 * Several Java primitives are utilized:
 * </p>
 * <ul>
 *     <li>{@code boolean} is {@code t} or {@code nil}.</li>
 *     <li>{@code long} is {@code fixnum}.</li>
 *     <li>{@code double} is {@code float}.</li>
 * </ul>
 *
 * <p>
 *     Otherwise,
 * </p>
 * <ul>
 *     <li>{@code nil} can also be a symbol.</li>
 *     <li>{@code t} can also be a symbol.</li>
 *     <li>{@link ELispBigNum} is {@code bignum}.</li>
 *     <li>{@link ELispSymbol} is {@code symbol}.</li>
 *     <li>{@link ELispCons} is always {@code cons}.
 *     When a {@code nil (car=null, cdr=null)} is obtained,
 *     it must be converted to a symbol immediately.</li>
 * </ul>
 */
@TypeSystem({
        /* {@code t} or {@code nil} */
        boolean.class,

        /* Numerics */
        long.class,
        double.class,
        ELispBigNum.class,

        /* Vector or vector-like */
        ELispString.class,
        ELispBoolVector.class,
        ELispCharTable.class,
        ELispHashtable.class,
        ELispRecord.class,
        ELispVector.class,

        /* Others */
        ELispCons.class,
        ELispFunctionObject.class,
        ELispSubroutine.class,
        ELispSymbol.class,
})
public abstract class ELispTypeSystem {

    @ImplicitCast
    public static ELispSymbol castBooleanToSymbol(boolean bool) {
        return bool ? T : NIL;
    }

    @ImplicitCast
    public static boolean castObjectToBoolean(ELispValue object) {
        return object != NIL;
    }

    @ImplicitCast
    public static boolean castNumberToBoolean(Number ignored) {
        return true;
    }

    @ImplicitCast
    public static ELispBigNum castLongToBigNum(long value) {
        return ELispBigNum.forceWrap(value);
    }

    public static boolean isNil(Object nil) {
        return nil == NIL || nil == Boolean.FALSE;
    }

    public static boolean isT(Object nil) {
        return nil == T || nil == Boolean.TRUE;
    }

    public static long notNilOr(Object maybeNil, long defaultValue) {
        if (isNil(maybeNil)) {
            return defaultValue;
        }
        return (long) maybeNil;
    }

    @SuppressWarnings("PMD.ShortMethodName")
    public static Object or(Object... candidates) {
        for (Object candidate : candidates) {
            if (!isNil(candidate)) {
                return candidate;
            }
        }
        return false;
    }

    public static int asInt(Object value) {
        if (value instanceof Long l) {
            return Math.toIntExact(l);
        }
        throw ELispSignals.wrongTypeArgument(INTEGERP, value);
    }

    public static long asRanged(Object value, long left, long right) {
        long i = asLong(value);
        if (i < left || i > right) {
            throw ELispSignals.argsOutOfRange(value, left, right);
        }
        return i;
    }

    public static int asRanged(Object value, int left, int right) {
        int i = asInt(value);
        if (i < left || i > right) {
            throw ELispSignals.argsOutOfRange(value, left, right);
        }
        return i;
    }

    public static int asChar(Object value) {
        return asRanged(value, 0, MAX_CHAR);
    }

    public static long asLong(Object value) {
        if (value instanceof Long l) {
            return l;
        }
        throw ELispSignals.wrongTypeArgument(INTEGERP, value);
    }

    public static long asNat(Object value) {
        return asRanged(value, 0, Long.MAX_VALUE);
    }

    public static int asNatInt(Object value) {
        return asRanged(value, 0, Integer.MAX_VALUE);
    }

    public static long consToUnsigned(Object value, long max) {
        long v = switch (value) {
            case Long l -> l;
            case Double d -> d.longValue();
            case ELispCons cons -> {
                long hi = asLong(cons.car());
                Object rest = cons.cdr();
                if (hi <= Long.MAX_VALUE >> 24 >> 16
                        && rest instanceof ELispCons restCons
                        && restCons.car() instanceof Long mid && mid < 1 << 24
                        && restCons.cdr() instanceof Long lo && lo < 1 << 16) {
                    yield (hi << 24 << 16) | (mid << 16) | lo;
                } else {
                    if (Long.MAX_VALUE >> 16 < hi) {
                        throw ELispSignals.argsOutOfRange(value, 0, max);
                    }
                    yield hi << 16 | asLong(rest instanceof ELispCons restCons ? restCons.car() : rest);
                }
            }
            default -> throw ELispSignals.wrongTypeArgument(INTEGERP, value);
        };
        if (v < 0 || max < v) {
            throw ELispSignals.argsOutOfRange(value, 0, max);
        }
        return v;
    }

    public static double asDouble(Object value) {
        if (value instanceof Double d) {
            return d;
        }
        throw ELispSignals.wrongTypeArgument(FLOATP, value);
    }

    public static double toDouble(double value) {
        return value;
    }

    public static boolean asBool(Object value) {
        if (value instanceof Boolean b) {
            return b;
        }
        if (isT(value)) {
            return true;
        }
        if (isNil(value)) {
            return false;
        }
        throw ELispSignals.wrongTypeArgument(BOOLEANP, value);
    }

    public static Number asNum(Object value) {
        if (value instanceof Number n) {
            return n;
        }
        throw ELispSignals.wrongTypeArgument(NUMBERP, value);
    }

    public static ELispCons asCons(Object value) {
        if (value instanceof ELispCons c) {
            return c;
        }
        throw ELispSignals.wrongTypeArgument(CONSP, value);
    }

    public static Iterable<Object> asConsOrNil(Object value) {
        if (isNil(value)) {
            return Collections.emptyList();
        }
        return asCons(value);
    }

    public static ELispCons.ConsIterator asConsIter(Object value) {
        if (isNil(value)) {
            return new ELispCons.ConsIterator() {
                @Override
                public boolean hasNextCons() {
                    return false;
                }

                @Override
                public ELispCons nextCons() {
                    throw new NoSuchElementException();
                }
            };
        }
        return asCons(value).consIterator(0);
    }

    public static Object asList(Object value) {
        if (isNil(value)) {
            return false;
        }
        return asCons(value);
    }

    public static ELispSymbol asSym(Object value) {
        if (isNil(value)) {
            return NIL;
        }
        if (isT(value)) {
            return T;
        }
        if (value instanceof ELispSymbol s) { // NOPMD
            return s;
        }
        throw ELispSignals.wrongTypeArgument(SYMBOLP, value);
    }

    public static Object toSym(Object value) {
        if (value == Boolean.FALSE) {
            return NIL;
        }
        if (value == Boolean.TRUE) {
            return T;
        }
        return value;
    }

    public static ELispString asStr(Object s) {
        if (s instanceof ELispString str) {
            return str;
        }
        throw ELispSignals.wrongTypeArgument(STRINGP, s);
    }

    public static ELispVector asVector(Object value) {
        if (value instanceof ELispVector v) {
            return v;
        }
        throw ELispSignals.wrongTypeArgument(VECTORP, value);
    }

    public static ELispBoolVector asBoolVec(Object value) {
        if (value instanceof ELispBoolVector v) {
            return v;
        }
        throw ELispSignals.wrongTypeArgument(BOOL_VECTOR_P, value);
    }

    public static ELispHashtable asHashtable(Object value) {
        if (value instanceof ELispHashtable h) {
            return h;
        }
        throw ELispSignals.wrongTypeArgument(HASH_TABLE_P, value);
    }

    public static ELispObarray asObarray(Object value) {
        if (value instanceof ELispObarray o) {
            return o;
        }
        throw ELispSignals.wrongTypeArgument(OBARRAYP, value);
    }

    public static ELispBuffer asBuffer(Object buffer) {
        if (buffer instanceof ELispBuffer b) {
            return b;
        }
        throw ELispSignals.wrongTypeArgument(BUFFERP, buffer);
    }

    public static ELispCharTable asCharTable(Object table) {
        if (table instanceof ELispCharTable t) {
            return t;
        }
        throw ELispSignals.wrongTypeArgument(CHAR_TABLE_P, table);
    }

    public static ELispFrame asFrame(Object frame) {
        if (frame instanceof ELispFrame f) {
            return f;
        }
        throw ELispSignals.wrongTypeArgument(FRAMEP, frame);
    }
}
