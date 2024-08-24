package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.math.BigInteger;

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
 *     it must be converted to {@code boolean} or a symbol immediately.</li>
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
        ELispHashtable.class,
        ELispRecord.class,
        ELispVector.class,

        /* Others */
        ELispCons.class,
        ELispFunctionObject.class,
        ELispSymbol.class,
})
public abstract class ELispTypeSystem {

    @ImplicitCast
    public static ELispBigNum castLongToBigNum(long value) {
        return new ELispBigNum(BigInteger.valueOf(value));
    }

    @ImplicitCast
    public static double castLongToDouble(long value) {
        return value;
    }

    @ImplicitCast
    public static double castBigIntegerToDouble(BigInteger value) {
        return value.doubleValue();
    }

}
