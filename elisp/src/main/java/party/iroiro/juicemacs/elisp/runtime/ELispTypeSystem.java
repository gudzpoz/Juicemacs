package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;
import com.oracle.truffle.api.strings.MutableTruffleString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.math.BigInteger;

@TypeSystem({
        boolean.class,

        long.class,
        double.class,
        ELispBigNum.class,

        ELispString.class,

        ELispSymbol.class,
        ELispCons.class,
        ELispFunctionObject.class,
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

    @ImplicitCast
    public static ELispCons castFalseToNil(boolean value) {
        if (value) {
            throw new IllegalArgumentException();
        }
        return new ELispCons();
    }
}
