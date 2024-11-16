package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInFloatFns extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFloatFnsFactory.getFactories();
    }

    /**
     * <pre>
     * Return the inverse cosine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "acos", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAcos extends ELispBuiltInBaseNode {
        @Specialization
        public static Void acos(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the inverse sine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "asin", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAsin extends ELispBuiltInBaseNode {
        @Specialization
        public static Void asin(Object arg) {
            throw new UnsupportedOperationException();
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
    public abstract static class FAtan extends ELispBuiltInBaseNode {
        @Specialization
        public static Void atan(Object y, Object x) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the cosine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "cos", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCos extends ELispBuiltInBaseNode {
        @Specialization
        public static Void cos(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the sine of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "sin", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSin extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sin(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the tangent of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "tan", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTan extends ELispBuiltInBaseNode {
        @Specialization
        public static Void tan(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if argument X is a NaN.
     * </pre>
     */
    @ELispBuiltIn(name = "isnan", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIsnan extends ELispBuiltInBaseNode {
        @Specialization
        public static Void isnan(Object x) {
            throw new UnsupportedOperationException();
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
    public abstract static class FCopysign extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copysign(Object x1, Object x2) {
            throw new UnsupportedOperationException();
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
    public abstract static class FFrexp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frexp(Object x) {
            throw new UnsupportedOperationException();
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
    public abstract static class FLdexp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ldexp(Object sgnfcand, Object exponent) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the exponential base e of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "exp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FExp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void exp(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the exponential ARG1 ** ARG2.
     * </pre>
     */
    @ELispBuiltIn(name = "expt", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FExpt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void expt(Object arg1, Object arg2) {
            throw new UnsupportedOperationException();
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
    public abstract static class FLog extends ELispBuiltInBaseNode {
        @Specialization
        public static Void log(Object arg, Object base) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the square root of ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "sqrt", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSqrt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void sqrt(Object arg) {
            throw new UnsupportedOperationException();
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
        @Specialization
        public static Void abs(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the floating point number equal to ARG.
     * </pre>
     */
    @ELispBuiltIn(name = "float", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFloat extends ELispBuiltInBaseNode {
        @Specialization
        public static Void float_(Object arg) {
            throw new UnsupportedOperationException();
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
        public static Void logb(Object arg) {
            throw new UnsupportedOperationException();
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
    public abstract static class FCeiling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ceiling(Object arg, Object divisor) {
            throw new UnsupportedOperationException();
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
    public abstract static class FFloor extends ELispBuiltInBaseNode {
        @Specialization
        public static Void floor(Object arg, Object divisor) {
            throw new UnsupportedOperationException();
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
    public abstract static class FRound extends ELispBuiltInBaseNode {
        @Specialization
        public static Void round(Object arg, Object divisor) {
            throw new UnsupportedOperationException();
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
    public abstract static class FTruncate extends ELispBuiltInBaseNode {
        @Specialization
        public static Void truncate(Object arg, Object divisor) {
            throw new UnsupportedOperationException();
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
    public abstract static class FFceiling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fceiling(Object arg) {
            throw new UnsupportedOperationException();
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
    public abstract static class FFfloor extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ffloor(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the nearest integer to ARG, as a float.
     * </pre>
     */
    @ELispBuiltIn(name = "fround", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFround extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fround(Object arg) {
            throw new UnsupportedOperationException();
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
    public abstract static class FFtruncate extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ftruncate(Object arg) {
            throw new UnsupportedOperationException();
        }
    }
}
