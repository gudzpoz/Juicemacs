package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.*;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public class BuiltInFloatFns extends ELispBuiltIns {
    public BuiltInFloatFns() {
        super(true);
    }

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFloatFnsFactory.getFactories();
    }

    @TypeSystem({
            long.class,
            double.class,
            ELispBigNum.class,
    })
    public static class FloatFnsTypeSystem extends ELispTypeSystem {
        @ImplicitCast
        public static double longToDouble(long arg) {
            return arg;
        }
        @ImplicitCast
        public static double bigNumToDouble(ELispBigNum arg) {
            return arg.doubleValue();
        }
    }

    @TypeSystemReference(FloatFnsTypeSystem.class)
    abstract static class ELispFloatFnsNode extends ELispBuiltInBaseNode {
    }

    /**
     * <pre>
     * Return the inverse cosine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "acos", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAcos extends ELispFloatFnsNode {
        @Specialization
        public static double acos(double arg) {
            return Math.acos(arg);
        }
    }

    /**
     * <pre>
     * Return the inverse sine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "asin", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAsin extends ELispFloatFnsNode {
        @Specialization
        public static double asin(double arg) {
            return Math.asin(arg);
        }
    }

    /**
     * <pre>
     * Return the inverse tangent of the arguments.
     * If only one argument Y is given, return the inverse tangent of Y.
     * If two arguments Y and X are given, return the inverse tangent of Y
     * divided by X, i.e. the angle in radians between the vector (X, Y)
     * and the x-axis.
     * </pre>
     */
    @ELispBuiltIn(name = "atan", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAtan extends ELispFloatFnsNode {
        @Specialization
        public static double atan(double y, Object x) {
            if (isNil(x)) {
                return Math.atan(y);
            }
            return Math.atan2(y, FloatFnsTypeSystemGen.asImplicitDouble(x));
        }
    }

    /**
     * <pre>
     * Return the cosine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "cos", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCos extends ELispFloatFnsNode {
        @Specialization
        public static double cos(double arg) {
            return Math.cos(arg);
        }
    }

    /**
     * <pre>
     * Return the sine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "sin", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSin extends ELispFloatFnsNode {
        @Specialization
        public static double sin(double arg) {
            return Math.sin(arg);
        }
    }

    /**
     * <pre>
     * Return the tangent of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "tan", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTan extends ELispFloatFnsNode {
        @Specialization
        public static double tan(double arg) {
            return Math.tan(arg);
        }
    }

    /**
     * <pre>
     * Return non-nil if argument X is a NaN.
     * </pre>
     */
    @ELispBuiltIn(name = "isnan", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIsnan extends ELispFloatFnsNode {
        @Specialization
        public static boolean isnan(double x) {
            return Double.isNaN(x);
        }
    }

    /**
     * <pre>
     * Copy sign of X2 to value of X1, and return the result.
     * Cause an error if X1 or X2 is not a float.
     * </pre>
     */
    @ELispBuiltIn(name = "copysign", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCopysign extends ELispFloatFnsNode {
        @Specialization
        public static double copysign(double x1, double x2) {
            return Math.copySign(x1, x2);
        }
    }

    /**
     * <pre>
     * Get significand and exponent of a floating point number.
     * Breaks the floating point number X into its binary significand SGNFCAND
     * \(a floating point value between 0.5 (included) and 1.0 (excluded))
     * and an integral exponent EXP for 2, such that:
     *
     *   X = SGNFCAND * 2^EXP
     *
     * The function returns the cons cell (SGNFCAND . EXP).
     * If X is zero, both parts (SGNFCAND and EXP) are zero.
     * </pre>
     */
    @ELispBuiltIn(name = "frexp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrexp extends ELispFloatFnsNode {
        @Specialization
        public static ELispCons frexp(double x) {
            int exponent = Math.getExponent(x);
            double significand = Math.scalb(x, -exponent);
            return new ELispCons(significand, (long) exponent);
        }
    }

    /**
     * <pre>
     * Return SGNFCAND * 2**EXPONENT, as a floating point number.
     * EXPONENT must be an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "ldexp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLdexp extends ELispFloatFnsNode {
        @Specialization
        public static double ldexp(double sgnfcand, long exponent) {
            return Math.scalb(sgnfcand, Math.clamp(exponent, Integer.MIN_VALUE, Integer.MAX_VALUE));
        }
    }

    /**
     * <pre>
     * Return the exponential base e of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "exp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FExp extends ELispFloatFnsNode {
        @Specialization
        public static double exp(double arg) {
            return Math.exp(arg);
        }
    }

    /**
     * <pre>
     * Return the exponential ARG1 ** ARG2.
     * </pre>
     */
    @ELispBuiltIn(name = "expt", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FExpt extends ELispFloatFnsNode {
        @Specialization
        public static double expt(double arg1, double arg2) {
            return Math.pow(arg1, arg2);
        }
    }

    /**
     * <pre>
     * Return the natural logarithm of ARG.
     * If the optional argument BASE is given, return log ARG using that base.
     * </pre>
     */
    @ELispBuiltIn(name = "log", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLog extends ELispFloatFnsNode {
        @Specialization
        public static double log(double arg, Object base) {
            if (isNil(base)) {
                return Math.log(arg);
            }
            return Math.log(arg) / Math.log(FloatFnsTypeSystemGen.asImplicitDouble(base));
        }
    }

    /**
     * <pre>
     * Return the square root of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "sqrt", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSqrt extends ELispFloatFnsNode {
        @Specialization
        public static double sqrt(double arg) {
            return Math.sqrt(arg);
        }
    }

    /**
     * <pre>
     * Return the absolute value of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "abs", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAbs extends ELispBuiltInBaseNode {
        static boolean safeLong(long arg) {
            return arg != Long.MIN_VALUE;
        }

        @Specialization(guards = "safeLong(arg)")
        public static long absLong(long arg) {
            return Math.abs(arg);
        }
        @Specialization
        public static Number absBigNum(ELispBigNum arg) {
            return arg.abs();
        }
        @Specialization
        public static double absDouble(double arg) {
            return Math.abs(arg);
        }
    }

    /**
     * <pre>
     * Return the floating point number equal to ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "float", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFloat extends ELispFloatFnsNode {
        @Specialization
        public static double float_(double arg) {
            return arg;
        }
    }

    /**
     * <pre>
     * Returns largest integer &lt;= the base 2 log of the magnitude of ARG.
     * This is the same as the exponent of a float.
     * </pre>
     */
    @ELispBuiltIn(name = "logb", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLogb extends ELispBuiltInBaseNode {
        @Specialization
        public static long logbFloat(double arg) {
            return Math.getExponent(arg);
        }
        @Specialization(guards = "arg > 0")
        public static long logbLongPos(long arg) {
            return Long.SIZE - 1 - Long.numberOfTrailingZeros(arg);
        }
        @Specialization
        public static Object logbLong(long arg) {
            if (arg <= 0) {
                return Double.NaN;
            }
            return logbLongPos(arg);
        }
        @Specialization
        public static Object logbBigNum(ELispBigNum arg) {
            return arg.log2();
        }
    }

    /**
     * <pre>
     * Return the smallest integer no less than ARG.
     * This rounds the value towards +inf.
     * With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR.
     * </pre>
     */
    @ELispBuiltIn(name = "ceiling", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCeiling extends ELispFloatFnsNode {
        @Specialization
        public static long ceiling(double arg, Object divisor) {
            // TODO: handle bignums
            if (isNil(divisor)) {
                return (long) Math.ceil(arg);
            }
            double div = FloatFnsTypeSystemGen.asImplicitDouble(divisor);
            return (long) Math.ceil(arg / div);
        }
    }

    /**
     * <pre>
     * Return the largest integer no greater than ARG.
     * This rounds the value towards -inf.
     * With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.
     * </pre>
     */
    @ELispBuiltIn(name = "floor", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFloor extends ELispFloatFnsNode {
        @Specialization
        public static long floor(double arg, Object divisor) {
            // TODO: handle bignums
            if (isNil(divisor)) {
                return (long) Math.floor(arg);
            }
            double div = FloatFnsTypeSystemGen.asImplicitDouble(divisor);
            return (long) Math.floor(arg / div);
        }
    }

    /**
     * <pre>
     * Return the nearest integer to ARG.
     * With optional DIVISOR, return the nearest integer to ARG/DIVISOR.
     *
     * Rounding a value equidistant between two integers may choose the
     * integer closer to zero, or it may prefer an even integer, depending on
     * your machine.  For example, (round 2.5) can return 3 on some
     * systems, but 2 on others.
     * </pre>
     */
    @ELispBuiltIn(name = "round", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRound extends ELispFloatFnsNode {
        @Specialization
        public static long round(double arg, Object divisor) {
            // TODO: handle bignums
            if (isNil(divisor)) {
                return Math.round(arg);
            }
            double div = FloatFnsTypeSystemGen.asImplicitDouble(divisor);
            return Math.round(arg / div);
        }
    }

    /**
     * <pre>
     * Truncate a floating point number to an int.
     * Rounds ARG toward zero.
     * With optional DIVISOR, truncate ARG/DIVISOR.
     * </pre>
     */
    @ELispBuiltIn(name = "truncate", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTruncate extends ELispFloatFnsNode {
        @Specialization
        public static long truncate(double arg, Object divisor) {
            // TODO: handle bignums
            if (isNil(divisor)) {
                return (long) arg;
            }
            double div = FloatFnsTypeSystemGen.asImplicitDouble(divisor);
            return (long) (arg / div);
        }
    }

    /**
     * <pre>
     * Return the smallest integer no less than ARG, as a float.
     * \(Round toward +inf.)
     * </pre>
     */
    @ELispBuiltIn(name = "fceiling", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFceiling extends ELispFloatFnsNode {
        @Specialization
        public static double fceiling(double arg) {
            return Math.ceil(arg);
        }
    }

    /**
     * <pre>
     * Return the largest integer no greater than ARG, as a float.
     * \(Round toward -inf.)
     * </pre>
     */
    @ELispBuiltIn(name = "ffloor", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFfloor extends ELispFloatFnsNode {
        @Specialization
        public static double ffloor(double arg) {
            return Math.floor(arg);
        }
    }

    /**
     * <pre>
     * Return the nearest integer to ARG, as a float.
     * </pre>
     */
    @ELispBuiltIn(name = "fround", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFround extends ELispFloatFnsNode {
        @Specialization
        public static double fround(double arg) {
            return Math.round(arg);
        }
    }

    /**
     * <pre>
     * Truncate a floating point number to an integral float value.
     * \(Round toward zero.)
     * </pre>
     */
    @ELispBuiltIn(name = "ftruncate", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFtruncate extends ELispFloatFnsNode {
        @Specialization
        public static double ftruncate(double arg) {
            // TODO: handle bignums
            return (long) arg;
        }
    }
}
