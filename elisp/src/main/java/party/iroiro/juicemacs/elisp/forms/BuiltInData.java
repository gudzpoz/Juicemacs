package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.strings.InternalByteArray;
import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleString.CodeRange;
import com.oracle.truffle.api.strings.TruffleString.GetCodeRangeImpreciseNode;
import org.apache.commons.lang3.StringUtils;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval.FFuncall;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.GlobalIndirectFunctionLookupNode;
import party.iroiro.juicemacs.elisp.nodes.GlobalIndirectLookupNode;
import party.iroiro.juicemacs.elisp.nodes.ast.ELispLiteralNodes;
import party.iroiro.juicemacs.elisp.parser.ELispLexer;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;

import java.io.IOException;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.math.BigInteger;
import java.util.BitSet;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/**
 * Built-in functions from {@code src/data.c}
 */
public class BuiltInData extends ELispBuiltIns {
    public BuiltInData() {
        super(true);
    }

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInDataFactory.getFactories();
    }

    public static boolean isMultibyte(ELispString value) {
        return value.state() > StringSupport.STATE_BYTES;
    }

    @ReportPolymorphism
    @NodeChild(value = "arg", type = ELispExpressionNode.class)
    abstract static class NumberAsIsUnary extends ELispExpressionNode {
        @Specialization
        public static long longAsIs(long value) {
            return value;
        }
        @Specialization
        public static double doubleAsIs(double value) {
            return value;
        }
        @Specialization(replaces = {"longAsIs", "doubleAsIs"})
        public static Object objectAsIs(Object o) {
            if (FNumberp.numberp(o)) {
                return o;
            }
            if (o instanceof ELispMarker marker) {
                return marker.longValue();
            }
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, o);
        }
    }

    @ReportPolymorphism
    @NodeChild(value = "arg", type = ELispExpressionNode.class)
    abstract static class BitAsIsUnary extends ELispExpressionNode {
        @Specialization
        public static long longAsIs(long value) {
            return value;
        }
        @Specialization(replaces = {"longAsIs"})
        public static Object objectAsIs(Object o) {
            if (FIntegerp.integerp(o)) {
                return o;
            }
            if (o instanceof ELispMarker marker) {
                return marker.longValue();
            }
            throw ELispSignals.wrongTypeArgument(INTEGER_OR_MARKER_P, o);
        }
    }

    @ReportPolymorphism
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    public abstract static class FMinusUnary extends ELispExpressionNode {
        @Specialization(rewriteOn = ArithmeticException.class)
        public long negLong(long value) {
            return Math.negateExact(value);
        }
        @Specialization
        public double negDouble(double value) {
            return -value;
        }
        @Specialization
        public Object negBigNum(ELispBigNum bigNum) {
            return bigNum.negate();
        }
        @Fallback
        public Object negObject(Object value) {
            throw ELispSignals.wrongTypeArgument(NUMBERP, value);
        }
    }

    @ReportPolymorphism
    @NodeChild(value = "value", type = ELispExpressionNode.class)
    public abstract static class FQuoUnary extends ELispExpressionNode {
        @Specialization
        public long quoLong(long value) {
            return 1 / value;
        }
        @Specialization
        public double quoDouble(double value) {
            return 1 / value;
        }
        @Specialization
        public Object quoBigNum(ELispBigNum bigNum) {
            return bigNum.reciprocal();
        }
        @Fallback
        public Object quoObject(Object value) {
            throw ELispSignals.wrongTypeArgument(NUMBERP, value);
        }
    }

    @ReportPolymorphism
    public abstract static class BinaryArithmeticNode extends ELispExpressionNode {
        @Child @Executed protected ELispExpressionNode left;
        @Child @Executed protected ELispExpressionNode right;

        protected BinaryArithmeticNode(ELispExpressionNode left, ELispExpressionNode right) {
            this.left = left;
            this.right = right;
        }

        public abstract long longs(long left, long right);
        public abstract Object longBigNum(long left, ELispBigNum right);
        public abstract Object bigNumLong(ELispBigNum left, long right);
        public abstract Object bigNums(ELispBigNum left, ELispBigNum right);
        public abstract double longDouble(long left, double right);
        public abstract double doubleLong(double left, long right);
        public abstract double doubles(double left, double right);
        public abstract Number generalCase(Number left, Number right);
        public Number fallback(Object left, Object right) {
            if (!(left instanceof Number nl)) {
                throw ELispSignals.wrongTypeArgument(NUMBERP, left);
            }
            if (!(right instanceof Number nr)) {
                throw ELispSignals.wrongTypeArgument(NUMBERP, right);
            }
            return generalCase(nl, nr);
        }
    }

    public abstract static class FPlusBinary extends BinaryArithmeticNode {
        protected FPlusBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization(rewriteOn = ArithmeticException.class)
        public long longs(long left, long right) {
            return Math.addExact(left, right);
        }
        @Override
        @Specialization
        public Object longBigNum(long left, ELispBigNum right) {
            return right.add(ELispBigNum.forceWrap(left));
        }
        @Override
        @Specialization
        public Object bigNumLong(ELispBigNum left, long right) {
            return left.add(ELispBigNum.forceWrap(right));
        }
        @Override
        @Specialization
        public Object bigNums(ELispBigNum left, ELispBigNum right) {
            return left.add(right);
        }
        @Override
        @Specialization
        public double longDouble(long left, double right) {
            return left + right;
        }
        @Override
        @Specialization
        public double doubleLong(double left, long right) {
            return left + right;
        }
        @Override
        @Specialization
        public double doubles(double left, double right) {
            return left + right;
        }
        @Override
        @Specialization(replaces = {"longs", "longBigNum", "bigNumLong", "bigNums", "longDouble", "doubleLong", "doubles"})
        public Number fallback(Object left, Object right) {
            return super.fallback(left, right);
        }
        @Override
        @TruffleBoundary
        public Number generalCase(Number left, Number right) {
            if (left instanceof Long ll && right instanceof Long rr) {
                if (Math.abs(ll) < Integer.MAX_VALUE && Math.abs(rr) < Integer.MAX_VALUE) {
                    return ll + rr;
                }
            }
            if (left instanceof Double dl) {
                return dl + right.doubleValue();
            }
            if (right instanceof Double dr) {
                return left.doubleValue() + dr;
            }
            return ELispTypeSystemGen.asImplicitELispBigNum(left).add(ELispTypeSystemGen.asImplicitELispBigNum(right));
        }
    }

    public abstract static class FMinusBinary extends BinaryArithmeticNode {
        protected FMinusBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization(rewriteOn = ArithmeticException.class)
        public long longs(long left, long right) {
            return Math.subtractExact(left, right);
        }
        @Override
        @Specialization
        public Object longBigNum(long left, ELispBigNum right) {
            return ELispBigNum.forceWrap(left).subtract(right);
        }
        @Override
        @Specialization
        public Object bigNumLong(ELispBigNum left, long right) {
            return left.subtract(ELispBigNum.forceWrap(right));
        }
        @Override
        @Specialization
        public Object bigNums(ELispBigNum left, ELispBigNum right) {
            return left.subtract(right);
        }
        @Override
        @Specialization
        public double longDouble(long left, double right) {
            return left - right;
        }
        @Override
        @Specialization
        public double doubleLong(double left, long right) {
            return left - right;
        }
        @Override
        @Specialization
        public double doubles(double left, double right) {
            return left - right;
        }
        @Override
        @Specialization(replaces = {"longs", "longBigNum", "bigNumLong", "bigNums", "longDouble", "doubleLong", "doubles"})
        public Number fallback(Object left, Object right) {
            return super.fallback(left, right);
        }
        @Override
        @TruffleBoundary
        public Number generalCase(Number left, Number right) {
            if (left instanceof Long ll && right instanceof Long rr) {
                if (Math.abs(ll) < Integer.MAX_VALUE && Math.abs(rr) < Integer.MAX_VALUE) {
                    return ll - rr;
                }
            }
            if (left instanceof Double dl) {
                return dl - right.doubleValue();
            }
            if (right instanceof Double dr) {
                return left.doubleValue() - dr;
            }
            return ELispTypeSystemGen.asImplicitELispBigNum(left).subtract(ELispTypeSystemGen.asImplicitELispBigNum(right));
        }
    }

    public abstract static class FTimesBinary extends BinaryArithmeticNode {
        protected FTimesBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization(rewriteOn = ArithmeticException.class)
        public long longs(long left, long right) {
            return Math.multiplyExact(left, right);
        }
        @Override
        @Specialization
        public Object longBigNum(long left, ELispBigNum right) {
            return ELispBigNum.forceWrap(left).multiply(right);
        }
        @Override
        @Specialization
        public Object bigNumLong(ELispBigNum left, long right) {
            return left.multiply(ELispBigNum.forceWrap(right));
        }
        @Override
        @Specialization
        public Object bigNums(ELispBigNum left, ELispBigNum right) {
            return left.multiply(right);
        }
        @Override
        @Specialization
        public double longDouble(long left, double right) {
            return left * right;
        }
        @Override
        @Specialization
        public double doubleLong(double left, long right) {
            return left * right;
        }
        @Override
        @Specialization
        public double doubles(double left, double right) {
            return left * right;
        }
        @Override
        @Specialization(replaces = {"longs", "longBigNum", "bigNumLong", "bigNums", "longDouble", "doubleLong", "doubles"})
        public Number fallback(Object left, Object right) {
            return super.fallback(left, right);
        }
        @Override
        @TruffleBoundary
        public Number generalCase(Number left, Number right) {
            if (left instanceof Long ll && right instanceof Long rr) {
                if (Math.abs(ll) < Integer.MAX_VALUE && Math.abs(rr) < Integer.MAX_VALUE) {
                    return ll * rr;
                }
            }
            if (left instanceof Double dl) {
                return dl * right.doubleValue();
            }
            if (right instanceof Double dr) {
                return left.doubleValue() * dr;
            }
            return ELispTypeSystemGen.asImplicitELispBigNum(left).multiply(ELispTypeSystemGen.asImplicitELispBigNum(right));
        }
    }

    public abstract static class FQuoBinary extends BinaryArithmeticNode {
        protected FQuoBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization(rewriteOn = ArithmeticException.class)
        public long longs(long left, long right) {
            return Math.divideExact(left, right);
        }
        @Override
        @Specialization
        public Object longBigNum(long left, ELispBigNum right) {
            return ELispBigNum.forceWrap(left).divide(right);
        }
        @Override
        @Specialization
        public Object bigNumLong(ELispBigNum left, long right) {
            return left.divide(ELispBigNum.forceWrap(right));
        }
        @Override
        @Specialization
        public Object bigNums(ELispBigNum left, ELispBigNum right) {
            return left.divide(right);
        }
        @Override
        @Specialization
        public double longDouble(long left, double right) {
            return left / right;
        }
        @Override
        @Specialization
        public double doubleLong(double left, long right) {
            return left / right;
        }
        @Override
        @Specialization
        public double doubles(double left, double right) {
            return left / right;
        }
        @Override
        @Specialization(replaces = {"longs", "longBigNum", "bigNumLong", "bigNums", "longDouble", "doubleLong", "doubles"})
        public Number fallback(Object left, Object right) {
            return super.fallback(left, right);
        }
        @Override
        @TruffleBoundary
        public Number generalCase(Number left, Number right) {
            if (left instanceof Long ll && right instanceof Long rr) {
                if (Math.abs(ll) < Integer.MAX_VALUE) {
                    return ll / rr;
                }
            }
            if (left instanceof Double dl) {
                return dl / right.doubleValue();
            }
            if (right instanceof Double dr) {
                return left.doubleValue() / dr;
            }
            return ELispTypeSystemGen.asImplicitELispBigNum(left).divide(ELispTypeSystemGen.asImplicitELispBigNum(right));
        }
    }

    @ReportPolymorphism
    @NodeChild(value = "left", type = ELispExpressionNode.class)
    @NodeChild(value = "right", type = ELispExpressionNode.class)
    public abstract static class FMaxBinary extends ELispExpressionNode {
        @Specialization
        public long longs(long left, long right) {
            return Math.max(left, right);
        }
        @Specialization
        public Object longDouble(long left, double right) {
            return Double.isNaN(right)
                    ? right
                    : (right > left ? right : (Object) left);
        }
        @Specialization
        public Object doubleLong(double left, long right) {
            return Double.isNaN(left)
                    ? left
                    : (right > left ? (Object) right : left);
        }
        @Specialization
        public double doubles(double left, double right) {
            return Math.max(left, right);
        }
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public Object generalCase(Number left, Number right) {
            if (left instanceof Double dl && Double.isNaN(dl)) {
                return dl;
            }
            if (right instanceof Double dr && Double.isNaN(dr)) {
                return dr;
            }
            return FMax.convMarker(compareTo(right, left) > 0 ? right : left); // NOPMD: total ordering needed
        }
    }

    @ReportPolymorphism
    @NodeChild(value = "left", type = ELispExpressionNode.class)
    @NodeChild(value = "right", type = ELispExpressionNode.class)
    public abstract static class FMinBinary extends ELispExpressionNode {
        @Specialization
        public long longs(long left, long right) {
            return Math.min(left, right);
        }
        @Specialization
        public Object longDouble(long left, double right) {
            return Double.isNaN(right)
                    ? right
                    : (right < left ? right : (Object) left);
        }
        @Specialization
        public Object doubleLong(double left, long right) {
            return Double.isNaN(left)
                    ? left
                    : (right < left ? (Object) right : left);
        }
        @Specialization
        public double doubles(double left, double right) {
            return Math.min(left, right);
        }
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public Object generalCase(Number left, Number right) {
            if (left instanceof Double dl && Double.isNaN(dl)) {
                return dl;
            }
            if (right instanceof Double dr && Double.isNaN(dr)) {
                return dr;
            }
            return FMax.convMarker(compareTo(right, left) < 0 ? right : left); // NOPMD: total ordering needed
        }
    }

    @ReportPolymorphism
    public abstract static class BinaryBitwiseNode extends ELispExpressionNode {
        @Child @Executed protected ELispExpressionNode left;
        @Child @Executed protected ELispExpressionNode right;

        protected BinaryBitwiseNode(ELispExpressionNode left, ELispExpressionNode right) {
            this.left = left;
            this.right = right;
        }

        public abstract long longs(long left, long right);
        public abstract Number fallback(ELispBigNum left, ELispBigNum right);
    }

    public abstract static class FLogxorBinary extends BinaryBitwiseNode {
        protected FLogxorBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public long longs(long left, long right) {
            return left ^ right;
        }
        @Override
        @Specialization
        public Number fallback(ELispBigNum left, ELispBigNum right) {
            return left.xor(right);
        }
    }

    public abstract static class FLogandBinary extends BinaryBitwiseNode {
        protected FLogandBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public long longs(long left, long right) {
            return left & right;
        }
        @Override
        @Specialization
        public Number fallback(ELispBigNum left, ELispBigNum right) {
            return left.and(right);
        }
    }

    public abstract static class FLogiorBinary extends BinaryBitwiseNode {
        protected FLogiorBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public long longs(long left, long right) {
            return left | right;
        }
        @Override
        @Specialization
        public Number fallback(ELispBigNum left, ELispBigNum right) {
            return left.or(right);
        }
    }

    private static ELispExpressionNode varArgsToBinary(
            ELispExpressionNode[] arguments,
            int startI,
            BiFunction<ELispExpressionNode, ELispExpressionNode, ELispExpressionNode> factory
    ) {
        ELispExpressionNode node = arguments[startI];
        for (int i = startI + 1; i < arguments.length; i++) {
            node = factory.apply(node, arguments[i]);
        }
        return node;
    }

    /// Similar to [Comparable#compareTo(Object)]
    ///
    /// This function imposes a total ordering, that is, it does not
    /// respect IEEE 754 `NaN` values. Use [#arithCompare(Object, Object)]
    /// unless you are implementing `max`, `min` or `sort`.
    ///
    /// @see #arithCompare(Object, Object)
    @TruffleBoundary
    public static int compareTo(Object a, Object b) {
        if (!(a instanceof Number na)) {
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, a);
        }
        if (!(b instanceof Number nb)) {
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, b);
        }
        if (na instanceof Double || nb instanceof Double) {
            double da = na.doubleValue();
            double db = nb.doubleValue();
            // We do not use Double.compare(da, db) here because we expect
            // compareTo(0.0, -0.0) == 0, while Double.compare imposes a total order.
            if (da < db) {
                return -1;
            } else if (da == db) {
                return 0;
            } else if (da > db) {
                return 1;
            }
            return Long.compare(Double.doubleToRawLongBits(da), Double.doubleToRawLongBits(db));
        }
        if (a instanceof ELispBigNum n) {
            return n.compareTo(ELispTypeSystemGen.asImplicitELispBigNum(b));
        }
        if (b instanceof ELispBigNum n) {
            return -n.compareTo(ELispTypeSystemGen.asImplicitELispBigNum(a));
        }
        return Long.compare(asLong(a), asLong(b));
    }

    public static final byte ARITH_COMPARE_LT = 0b0001;
    public static final byte ARITH_COMPARE_EQ = 0b0010;
    public static final byte ARITH_COMPARE_GT = 0b0100;
    /// Returns a bit mask representing comparison results
    ///
    /// Unlike [#compareTo(Object, Object)], this function allows correct NaN value
    /// handling.
    @TruffleBoundary
    public static byte arithCompare(Object a, Object b) {
        if (!(a instanceof Number na)) {
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, a);
        }
        if (!(b instanceof Number nb)) {
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, b);
        }
        boolean lt, eq, gt;
        if (na instanceof Long la && nb instanceof Long lb) {
            lt = la < lb;
            eq = la.longValue() == lb.longValue();
            gt = la > lb;
        } else if (na instanceof Double || nb instanceof Double) {
            double da = na.doubleValue();
            double db = nb.doubleValue();
            lt = da < db;
            eq = da == db;
            gt = da > db;
        } else {
            ELispBigNum bigA = ELispTypeSystemGen.asImplicitELispBigNum(na);
            ELispBigNum bigB = ELispTypeSystemGen.asImplicitELispBigNum(nb);
            int compare = bigA.compareTo(bigB);
            lt = compare < 0;
            eq = compare == 0;
            gt = compare > 0;
        }
        return (byte) ((lt ? ARITH_COMPARE_LT : 0)
                       | (eq ? ARITH_COMPARE_EQ : 0)
                       | (gt ? ARITH_COMPARE_GT : 0));
    }

    @ReportPolymorphism
    public abstract static class BinaryCompareNode extends ELispExpressionNode {
        @Child @Executed protected ELispExpressionNode left;
        @Child @Executed protected ELispExpressionNode right;

        protected BinaryCompareNode(ELispExpressionNode left, ELispExpressionNode right) {
            this.left = left;
            this.right = right;
        }

        public abstract boolean longs(long left, long right);
        public abstract boolean longDouble(long left, double right);
        public abstract boolean doubleLong(double left, long right);
        public abstract boolean doubles(double left, double right);
        public abstract boolean fallback(Object left, Object right);
    }

    public abstract static class FEqlsignBinary extends BinaryCompareNode {
        protected FEqlsignBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public boolean longs(long left, long right) {
            return left == right;
        }
        @Override
        @Specialization
        public boolean longDouble(long left, double right) {
            return left == right;
        }
        @Override
        @Specialization
        public boolean doubleLong(double left, long right) {
            return left == right;
        }
        @Override
        @Specialization
        public boolean doubles(double left, double right) {
            return left == right;
        }
        @Override
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public boolean fallback(Object left, Object right) {
            return arithCompare(left, right) == ARITH_COMPARE_EQ;
        }
    }

    public abstract static class FLssBinary extends BinaryCompareNode {
        protected FLssBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public boolean longs(long left, long right) {
            return left < right;
        }
        @Override
        @Specialization
        public boolean longDouble(long left, double right) {
            return left < right;
        }
        @Override
        @Specialization
        public boolean doubleLong(double left, long right) {
            return left < right;
        }
        @Override
        @Specialization
        public boolean doubles(double left, double right) {
            return left < right;
        }
        @Override
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public boolean fallback(Object left, Object right) {
            return (arithCompare(left, right) & ARITH_COMPARE_LT) != 0;
        }
    }

    public abstract static class FLeqBinary extends BinaryCompareNode {
        protected FLeqBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public boolean longs(long left, long right) {
            return left <= right;
        }
        @Override
        @Specialization
        public boolean longDouble(long left, double right) {
            return left <= right;
        }
        @Override
        @Specialization
        public boolean doubleLong(double left, long right) {
            return left <= right;
        }
        @Override
        @Specialization
        public boolean doubles(double left, double right) {
            return left <= right;
        }
        @Override
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public boolean fallback(Object left, Object right) {
            return (arithCompare(left, right) & (ARITH_COMPARE_LT | ARITH_COMPARE_EQ)) != 0;
        }
    }

    public abstract static class FGeqBinary extends BinaryCompareNode {
        protected FGeqBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public boolean longs(long left, long right) {
            return left >= right;
        }
        @Override
        @Specialization
        public boolean longDouble(long left, double right) {
            return left >= right;
        }
        @Override
        @Specialization
        public boolean doubleLong(double left, long right) {
            return left >= right;
        }
        @Override
        @Specialization
        public boolean doubles(double left, double right) {
            return left >= right;
        }
        @Override
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public boolean fallback(Object left, Object right) {
            return (arithCompare(left, right) & (ARITH_COMPARE_GT | ARITH_COMPARE_EQ)) != 0;
        }
    }

    public abstract static class FGtrBinary extends BinaryCompareNode {
        protected FGtrBinary(ELispExpressionNode left, ELispExpressionNode right) {
            super(left, right);
        }
        @Override
        @Specialization
        public boolean longs(long left, long right) {
            return left > right;
        }
        @Override
        @Specialization
        public boolean longDouble(long left, double right) {
            return left > right;
        }
        @Override
        @Specialization
        public boolean doubleLong(double left, long right) {
            return left > right;
        }
        @Override
        @Specialization
        public boolean doubles(double left, double right) {
            return left > right;
        }
        @Override
        @Specialization(replaces = {"longs", "longDouble", "doubleLong", "doubles"})
        public boolean fallback(Object left, Object right) {
            return (arithCompare(left, right) & ARITH_COMPARE_GT) != 0;
        }
    }

    @NodeChild(value = "left", type = ELispExpressionNode.class)
    @NodeChild(value = "right", type = ELispExpressionNode.class)
    public abstract static class HelperAndNode extends ELispExpressionNode {
        @Specialization
        public static boolean and(boolean left, boolean right) {
            return left && right;
        }
    }

    private static ELispExpressionNode varArgsComparisonToBinary(
            ELispExpressionNode[] args,
            BiFunction<ELispExpressionNode, ELispExpressionNode, ELispExpressionNode> factory
    ) {
        if (args.length == 1) {
            return ELispLiteralNodes.of(true);
        }
        ELispExpressionNode node = factory.apply(args[0], args[1]);
        for (int i = 2; i < args.length; i++) {
            node = BuiltInDataFactory.HelperAndNodeGen.create(node, factory.apply(args[i - 1], args[i]));
        }
        return node;
    }

    /**
     * <pre>
     * Return t if the two args are the same Lisp object.
     * </pre>
     */
    @ELispBuiltIn(name = "eq", minArgs = 2, maxArgs = 2)
    @TypeSystemReference(None.class)
    @GenerateNodeFactory
    public abstract static class FEq extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eqSymbol(ELispSymbol obj1, ELispSymbol obj2) {
            return obj1 == obj2;
        }
        @Specialization(replaces = {"eqSymbol"})
        public static boolean eq(Object obj1, Object obj2) {
            if (obj1 == obj2) {
                return true;
            }
            return switch (obj1) {
                // Simulate the Emacs behavior of packed integers
                case Long l1 -> obj2 instanceof Long l2 && l1.longValue() == l2.longValue();
                case ELispBigNum n1 -> obj2 instanceof ELispBigNum n2 && n1.lispEquals(n2);
                // In `(setq a2 (setq a1 (double-value)))`, due to boxing and unboxing,
                // it is possible that `a1 != a2`. We do not want that, so we choose
                // to let eq compare them by value.
                case Double d1 -> obj2 instanceof Double d2 &&
                        Double.doubleToRawLongBits(d1) == Double.doubleToRawLongBits(d2);
                default -> (isNil(obj1) && isNil(obj2)) || (isT(obj1) && isT(obj2));
            };
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
            return isNil(object);
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
        public static Object typeOf(Object object) {
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
        public static Object clTypeOf(Object object) {
            return switch (object) {
                case Long _ -> FIXNUM;
                case Double _ -> FLOAT;
                case ELispBigNum _ -> BIGNUM;
                case Boolean b when ELispTypeSystemGen.asBoolean(b) -> BOOLEAN;
                case ELispSymbol sym when sym == T -> BOOLEAN;
                case Boolean _ -> NULL;
                case ELispSymbol sym when sym == NIL -> NULL;
                case ELispSymbol _ -> SYMBOL;
                case ELispString _ -> STRING;
                case ELispCons _ -> CONS;

                // vector
                case ELispVector _ -> VECTOR;
                case ELispBoolVector _ -> BOOL_VECTOR;
                case ELispCharTable _ -> CHAR_TABLE;
                case ELispRecord record -> {
                    if (record.get(0) instanceof ELispRecord clazz && clazz.size() > 1) {
                        yield clazz.get(1);
                    }
                    yield record.get(0);
                }
                // TODO: Handle other pseudo-vectors
                case ELispInterpretedClosure _ -> INTERPRETED_FUNCTION;
                case ELispBytecode _ -> BYTE_CODE_FUNCTION;

                // identity objects
                case ELispHashtable _ -> HASH_TABLE;
                case ELispObarray _ -> OBARRAY;
                case ELispBuffer _ -> BUFFER;
                case ELispMarker _ -> MARKER;
                case ELispSubroutine sub when sub.specialForm() -> SPECIAL_FORM;
                case ELispSubroutine _ -> PRIMITIVE_FUNCTION;
                default -> throw new UnsupportedOperationException();
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
            return object instanceof ELispCons || isNil(object);
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
            return toSym(object) instanceof ELispSymbol;
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
            return false;
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
            return toSym(object) instanceof ELispSymbol;
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
        public boolean keywordp(Object object) {
            return toSym(object) instanceof ELispSymbol symbol && symbol.isKeyword()
                    && getContext().obarray().internSoft(symbol.name()) != null;
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
            return object instanceof ELispString s && isMultibyte(s);
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
        public static boolean bufferp(Object object) {
            return object instanceof ELispBuffer;
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
        public static boolean markerp(Object object) {
            return object instanceof ELispMarker;
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
        public static boolean userPtrp(Object object) {
            // TODO: implement
            return false;
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
        public static boolean closurep(Object object) {
            return object instanceof AbstractELispClosure;
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
        public static boolean byteCodeFunctionP(Object object) {
            return object instanceof ELispBytecode;
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
        public static boolean interpretedFunctionP(Object object) {
            return object instanceof ELispInterpretedClosure;
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
        public static boolean moduleFunctionP(Object object) {
            // TODO
            return false;
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
                    || (object instanceof Long c && StringSupport.isValidChar(c));
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
        public static boolean integerOrMarkerP(Object object) {
            return FIntegerp.integerp(object) || object instanceof ELispMarker;
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
                    || (object instanceof ELispBigNum n && n.signum() >= 0);
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
        public static boolean numberOrMarkerP(Object object) {
            return FNumberp.numberp(object) || object instanceof ELispMarker;
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
        public static boolean threadp(Object object) {
            // TODO
            return false;
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
        public static boolean mutexp(Object object) {
            // TODO
            return false;
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
        public static boolean conditionVariableP(Object object) {
            // TODO
            return false;
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
        public static Object carCons(ELispCons list) {
            return list.car();
        }
        @Specialization
        public static Object car(Object list) {
            return isNil(list) ? false : asCons(list).car();
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
        public static Object carSafeCons(ELispCons object) {
            return object.car();
        }
        @Specialization(replaces = "carSafeCons")
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
        public static Object cdrCons(ELispCons list) {
            return list.cdr();
        }
        @Specialization
        public static Object cdr(Object list) {
            return isNil(list) ? false : asCons(list).cdr();
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
        public static Object cdrSafeCons(ELispCons object) {
            return object.cdr();
        }
        @Specialization(replaces = "cdrSafeCons")
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
        public boolean boundp(ELispSymbol symbol, @Cached GlobalIndirectLookupNode lookup) {
            if (symbol.isKeyword()) {
                return true;
            }
            Optional<ValueStorage> storage = lookup.execute(this, symbol);
            return storage.isPresent() && storage.get().isBound();
        }
    }

    /**
     * <pre>
     * Return t if SYMBOL's function definition is neither void nor nil.
     * </pre>
     */
    @ELispBuiltIn(name = "fboundp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFboundp extends ELispBuiltInBaseNode {
        @Specialization
        public boolean fboundp(ELispSymbol symbol, @Cached GlobalIndirectFunctionLookupNode lookup) {
            Optional<FunctionStorage> storage = lookup.execute(this, symbol);
            return storage.isPresent() && !isNil(storage.get().get());
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
        public ELispSymbol makunbound(ELispSymbol symbol, @Cached GlobalIndirectLookupNode lookup) {
            if (symbol.isKeyword()) {
                throw ELispSignals.settingConstant(symbol);
            }
            Optional<ValueStorage> storage = lookup.execute(this, symbol);
            if (storage.isPresent()) {
                ValueStorage value = storage.get();
                if (value.isConstant()) {
                    throw ELispSignals.settingConstant(symbol);
                }
                value.makeUnbound(getContext());
            }
            return symbol;
        }
    }

    /**
     * <pre>
     * Make SYMBOL's function definition be nil.
     * Return SYMBOL.
     *
     * If a function definition is nil or void, trying to call a function by
     * that name will cause a `void-function' error.  For more details, see
     * Info node `(elisp) Function Cells'.
     *
     * See also `makunbound'.
     * </pre>
     */
    @ELispBuiltIn(name = "fmakunbound", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFmakunbound extends ELispBuiltInBaseNode {
        @Specialization
        public ELispSymbol fmakunbound(ELispSymbol symbol, @Cached GlobalIndirectFunctionLookupNode lookup) {
            Optional<FunctionStorage> storage = lookup.execute(this, symbol);
            if (storage.isPresent()) {
                FunctionStorage value = storage.get();
                value.set(false, symbol);
            }
            return symbol;
        }
    }

    /**
     * <pre>
     * Return SYMBOL's function definition, or nil if that is void or nil.
     * </pre>
     */
    @ELispBuiltIn(name = "symbol-function", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSymbolFunction extends ELispBuiltInBaseNode {
        @Specialization
        public Object symbolFunction(ELispSymbol symbol, @Cached GlobalIndirectFunctionLookupNode lookup) {
            Optional<FunctionStorage> storage = lookup.execute(this, symbol);
            return storage.map(FunctionStorage::get).orElse(false);
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
        public Object symbolPlist(ELispSymbol symbol, @Cached GlobalIndirectLookupNode lookup) {
            Optional<ValueStorage> storage = lookup.execute(this, symbol);
            return storage.map(ValueStorage::getProperties).orElse(false);
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
        public static Object positionSymbol(Object sym, Object pos) {
            // TODO
            return sym;
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
        public ELispSymbol fsetDynamic(ELispSymbol symbol, Object definition) {
            fset(symbol, definition, getContext());
            return symbol;
        }

        public static void fset(ELispSymbol symbol, Object definition, ELispContext context) {
            if (symbol == NIL && !isNil(definition)) {
                throw ELispSignals.settingConstant(symbol);
            }
            if (toSym(definition) instanceof ELispSymbol indirection) {
                checkCyclicAlias(symbol, indirection);
            }
            context.getFunctionStorage(symbol).set(definition, symbol);
        }

        @TruffleBoundary
        private static void checkCyclicAlias(ELispSymbol symbol, ELispSymbol indirection) {
            while (!isNil(indirection)) {
                if (indirection == symbol) {
                    throw ELispSignals.cyclicFunctionIndirection(symbol);
                }
                if (toSym(indirection.getFunction()) instanceof ELispSymbol next) {
                    indirection = next;
                } else {
                    break;
                }
            }
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
        public ELispSymbol defalias(ELispSymbol symbol, Object definition, Object docstring) {
            // TODO: handle add_to_function_history & autoload_queue
            Optional<ValueStorage> storage = getContext().getStorageLazy(symbol);
            if (storage.isPresent()) {
                Object hook = storage.get().getProperty(DEFALIAS_FSET_FUNCTION);
                if (!isNil(hook)) {
                    FFuncall.funcall(this, hook, symbol, definition);
                    return symbol;
                }
            }
            FFset.fset(symbol, definition, getContext());
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
        @TruffleBoundary
        @Specialization
        public static ELispCons subrArity(ELispSubroutine subr) {
            ELispBuiltIn info = subr.info();
            long min = info.minArgs();
            long max = info.maxArgs();
            return ELispCons.cons(
                    min,
                    info.rawArg() ? UNEVALLED : (info.varArgs() ? MANY : max)
            );
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
        public static boolean nativeCompFunctionP(Object object) {
            // TODO
            return false;
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
        public static boolean interactiveForm(Object cmd) {
            // TODO
            return false;
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
        public Object indirectVariable(Object object) {
            if (!(toSym(object) instanceof ELispSymbol symbol)) {
                return object;
            }
            ELispContext context = getContext();
            Optional<ValueStorage> storage = context.getStorageLazy(symbol);
            if (storage.isEmpty()) {
                return symbol;
            }
            return storage.get().getIndirectVariable(context, symbol);
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
        public Object symbolValue(ELispSymbol symbol, @Cached GlobalIndirectLookupNode lookupNode) {
            if (symbol.isKeyword()) {
                return symbol;
            }
            Optional<ValueStorage> storage = lookupNode.execute(this, symbol);
            if (storage.isEmpty()) {
                throw ELispSignals.voidVariable(symbol);
            }
            return storage.get().getValue(symbol);
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
        public Object set(ELispSymbol symbol, Object newval) {
            getContext().setValue(symbol, newval);
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
        public static boolean addVariableWatcher(Object symbol, Object watchFunction) {
            // TODO
            return false;
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
        public boolean defaultBoundp(
                ELispSymbol symbol,
                @Cached GlobalIndirectLookupNode lookup
        ) {
            Optional<ValueStorage> storage = lookup.execute(this, symbol);
            return storage.isPresent() && storage.get().isDefaultBound();
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
        public Object defaultValue(ELispSymbol symbol, @Cached GlobalIndirectLookupNode lookup) {
            Optional<ValueStorage> storage = lookup.execute(this, symbol);
            return storage.map(ValueStorage::getDefaultValue).orElse(false);
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
            if (toSym(object) instanceof ELispSymbol symbol) {
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
        @Idempotent
        static boolean isValid(GetCodeRangeImpreciseNode node, ELispString s) {
            return node.execute(s.value(), StringSupport.UTF_32) != CodeRange.BROKEN;
        }

        @Specialization(guards = "valid")
        public static long arefStringValid(
                ELispString array, long idx,
                @Cached @Shared GetCodeRangeImpreciseNode range,
                @Bind("isValid(range, array)") boolean valid,
                @Cached TruffleString.CodePointAtIndexNode charAt
        ) {
            checkRange(array.length(), idx);
            return charAt.execute(array.value(), (int) idx, StringSupport.UTF_32);
        }
        @Specialization(guards = "!valid")
        public static long arefStringRaw(
                ELispString array, long idx,
                @Cached @Shared TruffleString.GetCodeRangeImpreciseNode range,
                @Bind("isValid(range, array)") boolean valid,
                @Cached TruffleString.GetInternalByteArrayNode getInternal
        ) {
            checkRange(array.length(), idx);
            InternalByteArray inner = getInternal.execute(array.value(), StringSupport.UTF_32);
            return getIntAtIndex(idx, inner);
        }
        @TruffleBoundary
        private static int getIntAtIndex(long idx, InternalByteArray inner) {
            return MemorySegment.ofArray(inner.getArray()).get(
                    ValueLayout.JAVA_INT_UNALIGNED,
                    (int) (idx << 2) + inner.getOffset()
            );
        }
        public static long arefStringUncached(ELispString array, long idx) {
            GetCodeRangeImpreciseNode range = GetCodeRangeImpreciseNode.getUncached();
            if (isValid(range, array)) {
                return arefStringValid(
                        array, idx,
                        range, true,
                        TruffleString.CodePointAtIndexNode.getUncached()
                );
            } else {
                return arefStringRaw(
                        array, idx,
                        range, false,
                        TruffleString.GetInternalByteArrayNode.getUncached()
                );
            }
        }

        @Specialization
        public static Object arefVec(ELispVector array, long idx) {
            checkRange(array.size(), idx);
            return array.get((int) idx);
        }
        @Specialization
        public static Object arefRecord(ELispRecord array, long idx) {
            checkRange(array.size(), idx);
            return array.get((int) idx);
        }
        @Specialization
        public static Object arefVecVirtual(ELispVectorLike<?> array, long idx) {
            checkRange(array.size(), idx);
            return array.get((int) idx);
        }

        @Specialization
        public static Object aref(Object array, long idx) {
            return switch (array) {
                case ELispString str -> arefStringUncached(str, idx);
                case ELispVectorLike<?> vec -> {
                    checkRange(vec.size(), idx);
                    yield vec.get((int) idx);
                }
                default -> throw ELispSignals.wrongTypeArgument(ARRAYP, array);
            };
        }

        @Specialization
        public static Object arefFallback(Object array, Object idx) {
            if (idx instanceof Long index) {
                return aref(array, index);
            }
            if (idx instanceof ELispBigNum) {
                throw ELispSignals.argsOutOfRange(Long.MAX_VALUE);
            }
            throw ELispSignals.wrongTypeArgument(INTEGERP, idx);
        }

        private static void checkRange(long length, long index) {
            if (index < 0 || index >= length) {
                throw ELispSignals.argsOutOfRange(index);
            }
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
        @Specialization(guards = "array.state() <= 1")
        public static long asetString(
                ELispString array, long idx, long newelt,
                @Cached MutableTruffleString.AsMutableTruffleStringNode convert,
                @Cached MutableTruffleString.WriteByteNode write
        ) {
            FAref.checkRange(array.length(), idx);
            write.execute(
                    array.asMutableTruffleString(convert),
                    (int) idx,
                    (byte) newelt,
                    StringSupport.UTF_32
            );
            return newelt;
        }

        @Specialization
        public static Object asetVec(ELispVector array, long idx, Object newelt) {
            array.set((int) idx, newelt);
            return newelt;
        }
        @Specialization
        public static Object asetRecord(ELispRecord array, long idx, Object newelt) {
            array.set((int) idx, newelt);
            return newelt;
        }
        @Specialization
        public static boolean asetBool(ELispBoolVector array, long idx, Object newelt) {
            boolean elt = !isNil(newelt);
            array.set((int) idx, elt);
            return elt;
        }
        @Specialization
        public static Object aset(AbstractELispVector array, long idx, Object newelt) {
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
    public abstract static class FEqlsign extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static boolean eqlsign(Object numberOrMarker, Object[] numbersOrMarkers) {
            for (Object arg : numbersOrMarkers) {
                if (arithCompare(numberOrMarker, arg) != ARITH_COMPARE_EQ) {
                    return false;
                }
            }
            return true;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return varArgsComparisonToBinary(arguments, BuiltInDataFactory.FEqlsignBinaryNodeGen::create);
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
    public abstract static class FLss extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static boolean lss(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if ((arithCompare(prev, arg) & ARITH_COMPARE_LT) == 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return varArgsComparisonToBinary(arguments, BuiltInDataFactory.FLssBinaryNodeGen::create);
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
    public abstract static class FGtr extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static boolean gtr(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if ((arithCompare(prev, arg) & ARITH_COMPARE_GT) == 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return varArgsComparisonToBinary(arguments, BuiltInDataFactory.FGtrBinaryNodeGen::create);
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
    public abstract static class FLeq extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static boolean leq(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if ((arithCompare(prev, arg) & (ARITH_COMPARE_LT | ARITH_COMPARE_EQ)) == 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return varArgsComparisonToBinary(arguments, BuiltInDataFactory.FLeqBinaryNodeGen::create);
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
    public abstract static class FGeq extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static boolean geq(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object prev = numberOrMarker;
            for (Object arg : numbersOrMarkers) {
                if ((arithCompare(prev, arg) & (ARITH_COMPARE_GT | ARITH_COMPARE_EQ)) == 0) {
                    return false;
                }
                prev = arg;
            }
            return true;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return varArgsComparisonToBinary(arguments, BuiltInDataFactory.FGeqBinaryNodeGen::create);
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
            return (arithCompare(num1, num2) & ARITH_COMPARE_EQ) == 0;
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
        public static ELispString numberToStringFixed(long number) {
            return new ELispString(Long.toString(number));
        }
        @TruffleBoundary
        @Specialization
        public static ELispString numberToStringFloat(double number) {
            if (Double.isNaN(number)) {
                long bits = Double.doubleToRawLongBits(number);
                boolean neg = (bits & Long.MIN_VALUE) != 0;
                long significand = bits & ELispLexer.SIGNIFICAND_MASK;
                return new ELispString((neg ? "-" : "") + significand + ".0e+NaN");
            } else if (Double.isInfinite(number)) {
                return new ELispString(number > 0 ? "1.0e+INF" : "-1.0e+INF");
            }
            String floatString = Double.toString(number);
            int eIndex = floatString.indexOf('E');
            if (eIndex != -1) {
                StringBuilder sb = new StringBuilder()
                        .append(floatString, 0, eIndex)
                        .append('e');
                if (floatString.charAt(eIndex + 1) != '-') {
                    sb.append('+');
                }
                sb.append(floatString, eIndex + 1, floatString.length());
                floatString = sb.toString();
            }
            return new ELispString(floatString);
        }
        @Specialization
        public static ELispString numberToStringBigNum(ELispBigNum number) {
            return new ELispString(number.toString());
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
        private static final Pattern FLOAT_SEARCH_PATTERN = Pattern.compile(ELispLexer.FLOAT_PATTERN_STRING);

        @TruffleBoundary
        @Specialization
        public Object stringToNumber(ELispString string, Object base) {
            String s = StringUtils.strip(string.toString(), " \t");
            long iBase = notNilOr(base, 10);
            try {
                if (iBase == 10) {
                    Matcher matcher = FLOAT_SEARCH_PATTERN.matcher(s);
                    if (matcher.find()) {
                        return ELispParser.read(getContext(), s.substring(0, matcher.end()));
                    }
                }
                if (iBase < 2 || iBase > 16) {
                    throw ELispSignals.argsOutOfRange(iBase);
                }
                int i = 0;
                while (i < s.length()) {
                    char c = s.charAt(i);
                    if ('0' <= c && c <= '9') {
                        if (c < '0' + iBase) {
                            i++;
                        } else {
                            break;
                        }
                    } else if (iBase > 10) {
                        if ('a' <= c && c <= 'f') {
                            if (c < 'a' + iBase - 10) {
                                i++;
                            } else {
                                break;
                            }
                        } else if ('A' <= c && c <= 'F') {
                            if (c < 'A' + iBase - 10) {
                                i++;
                            } else {
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
                return ELispParser.read(getContext(), "#" + iBase + "r" + s.substring(0, i));
            } catch (IOException | ELispSignals.ELispSignalException ignored) {
                // returns 0 on invalid inputs
            }
            return 0L;
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
    public abstract static class FPlus extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static Object plusAny(Object[] numbersOrMarkers) {
            long sum = 0;
            for (int i = 0; i < numbersOrMarkers.length; i++) {
                switch (numbersOrMarkers[i]) {
                    case Long l -> {
                        try {
                            sum = Math.addExact(sum, l);
                        } catch (ArithmeticException e) {
                            return tryAddBigNum(sum, i, numbersOrMarkers);
                        }
                    }
                    case Double _ -> {
                        return tryAddDouble((double) sum, i, numbersOrMarkers);
                    }
                    case ELispBigNum _ -> {
                        return tryAddBigNum(sum, i, numbersOrMarkers);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, numbersOrMarkers[i]);
                }
            }
            return sum;
        }

        @TruffleBoundary
        private static Object tryAddBigNum(long prev, int i, Object[] args) {
            BigInteger sum = BigInteger.valueOf(prev);
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> sum = sum.add(n.asBigInteger());
                    case Long l -> sum = sum.add(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryAddDouble(sum.doubleValue(), i, args);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, args[i]);
                }
            }
            return ELispBigNum.wrap(sum);
        }

        @TruffleBoundary
        private static double tryAddDouble(double prev, int i, Object[] args) {
            double sum = prev;
            for (; i < args.length; i++) {
                sum += asNum(args[i]).doubleValue();
            }
            return sum;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 0) {
                return ELispLiteralNodes.of(0L);
            }
            if (arguments.length == 1) {
                return BuiltInDataFactory.NumberAsIsUnaryNodeGen.create(arguments[0]);
            }
            return varArgsToBinary(arguments, 0, BuiltInDataFactory.FPlusBinaryNodeGen::create);
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
    public abstract static class FMinus extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static Object minusAny(Object[] args) {
            if (args.length == 0) {
                return 0L;
            }
            Object arg0 = args[0];
            if (args.length == 1) {
                return switch (arg0) {
                    case Long l when l > Long.MIN_VALUE -> Math.negateExact(l);
                    case Long l -> ELispBigNum.forceWrap(l).negate();
                    case Double d -> -d;
                    case ELispBigNum n -> n.negate();
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, arg0);
                };
            }
            return switch (arg0) {
                case Long l -> tryMinusLong(l, 1, args);
                case ELispBigNum n -> tryMinusBigNum(n, 1, args);
                case Double d -> tryMinusDouble(d, 1, args);
                default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, arg0);
            };
        }

        public static Object tryMinusLong(long result, int i, Object[] args) {
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case Long l -> {
                        try {
                            result = Math.subtractExact(result, l);
                        } catch (ArithmeticException e) {
                            return tryMinusBigNum(ELispBigNum.forceWrap(result), i, args);
                        }
                    }
                    case Double _ -> {
                        return tryMinusDouble((double) result, i, args);
                    }
                    case ELispBigNum _ -> {
                        return tryMinusBigNum(ELispBigNum.forceWrap(result), i, args);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, args[i]);
                }
            }
            return result;
        }

        @TruffleBoundary
        private static Object tryMinusBigNum(ELispBigNum current, int i, Object[] args) {
            BigInteger result = current.asBigInteger();
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> result = result.subtract(n.asBigInteger());
                    case Long l -> result = result.subtract(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryMinusDouble(result.doubleValue(), i, args);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, args[i]);
                }
            }
            return ELispBigNum.wrap(result);
        }

        @TruffleBoundary
        private static double tryMinusDouble(double prev, int i, Object[] args) {
            double result = prev;
            for (; i < args.length; i++) {
                result -= asNum(args[i]).doubleValue();
            }
            return result;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 0) {
                return ELispLiteralNodes.of(0L);
            }
            if (arguments.length == 1) {
                return BuiltInDataFactory.FMinusUnaryNodeGen.create(arguments[0]);
            }
            return varArgsToBinary(arguments, 0, BuiltInDataFactory.FMinusBinaryNodeGen::create);
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
    public abstract static class FTimes extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static Object timesAny(Object[] numbersOrMarkers) {
            long product = 1;
            for (int i = 0; i < numbersOrMarkers.length; i++) {
                switch (numbersOrMarkers[i]) {
                    case Long l -> {
                        try {
                            product = Math.multiplyExact(product, l);
                        } catch (ArithmeticException e) {
                            return tryTimesBigNum(product, i, numbersOrMarkers);
                        }
                    }
                    case Double _ -> {
                        return tryTimesDouble((double) product, i, numbersOrMarkers);
                    }
                    case ELispBigNum _ -> {
                        return tryTimesBigNum(product, i, numbersOrMarkers);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, numbersOrMarkers[i]);
                }
            }
            return product;
        }

        @TruffleBoundary
        private static Object tryTimesBigNum(long prev, int i, Object[] args) {
            BigInteger product = BigInteger.valueOf(prev);
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> product = product.multiply(n.asBigInteger());
                    case Long l -> product = product.multiply(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryTimesDouble(product.doubleValue(), i, args);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, args[i]);
                }
            }
            return ELispBigNum.wrap(product);
        }

        @TruffleBoundary
        private static double tryTimesDouble(double prev, int i, Object[] args) {
            double product = prev;
            for (; i < args.length; i++) {
                product *= asNum(args[i]).doubleValue();
            }
            return product;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 0) {
                return ELispLiteralNodes.of(1L);
            }
            if (arguments.length == 1) {
                return BuiltInDataFactory.NumberAsIsUnaryNodeGen.create(arguments[0]);
            }
            return varArgsToBinary(arguments, 0, BuiltInDataFactory.FTimesBinaryNodeGen::create);
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
    public abstract static class FQuo extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @TruffleBoundary
        @Specialization
        public static Object quoAny(Object number, Object[] divisors) {
            for (Object arg : divisors) {
                if (arg instanceof Double) {
                    return tryQuoDouble(asNum(number).doubleValue(), divisors);
                }
            }
            return switch (number) {
                case Long l -> tryQuoLong(l, divisors);
                case ELispBigNum n -> tryQuoBigNum(n, 0, divisors);
                case Double d -> tryQuoDouble(d, divisors);
                default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, number);
            };
        }

        public static Object tryQuoLong(long number, Object[] divisors) {
            long quo = number;
            for (int i = 0; i < divisors.length; i++) {
                switch (divisors[i]) {
                    case Long l -> {
                        try {
                            quo = Math.divideExact(quo, l);
                        } catch (ArithmeticException e) {
                            return tryQuoBigNum(ELispBigNum.forceWrap(quo), i, divisors);
                        }
                    }
                    case ELispBigNum _ -> {
                        return tryQuoBigNum(ELispBigNum.forceWrap(quo), i, divisors);
                    }
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, divisors[i]);
                }
            }
            return quo;
        }

        @TruffleBoundary
        private static Object tryQuoBigNum(ELispBigNum prev, int i, Object[] args) {
            BigInteger quo = prev.asBigInteger();
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum n -> quo = quo.divide(n.asBigInteger());
                    case Long l -> quo = quo.divide(BigInteger.valueOf(l));
                    default -> throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, args[i]);
                }
            }
            return ELispBigNum.wrap(quo);
        }

        @TruffleBoundary
        private static double tryQuoDouble(double prev, Object[] args) {
            double quo = prev;
            for (Object arg : args) {
                quo /= asNum(arg).doubleValue();
            }
            return quo;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 1) {
                return BuiltInDataFactory.FQuoUnaryNodeGen.create(arguments[0]);
            }
            if (arguments.length == 2) {
                return BuiltInDataFactory.FQuoBinaryNodeGen.create(arguments[0], arguments[1]);
            }
            return BuiltInDataFactory.FQuoBinaryNodeGen.create(
                    arguments[0],
                    varArgsToBinary(arguments, 1, BuiltInDataFactory.FTimesBinaryNodeGen::create)
            );
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
            if (y == 0) {
                throw ELispSignals.arithError();
            }
            return x % y;
        }

        @Specialization
        public static Object rem(ELispBigNum x, ELispBigNum y) {
            return x.remainder(y);
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
            if (y == 0) {
                throw ELispSignals.arithError();
            }
            long r = x % y;
            if (y < 0 ? r > 0 : r < 0) {
                r += y;
            }
            return r;
        }

        @Specialization
        public static Object modBigNum(ELispBigNum x, ELispBigNum y) {
            return x.mod(y);
        }

        @Specialization
        public static double modDouble(double x, double y) {
            double r = x % y;
            if (y < 0 ? r > 0 : r < 0) {
                r += y;
            }
            return r;
        }

        @Specialization
        public static double modBigNumDouble(ELispBigNum x, double y) {
            return modDouble(x.doubleValue(), y);
        }

        @Specialization
        public static double modDoubleBigNum(double x, ELispBigNum y) {
            return modDouble(x, y.doubleValue());
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
    public abstract static class FMax extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        public static Object convMarker(Object numberOrMarker) {
            return numberOrMarker instanceof ELispMarker m ? m.point() : numberOrMarker;
        }

        public static boolean isNaN(Object numberOrMarker) {
            return numberOrMarker instanceof Double d && Double.isNaN(d);
        }

        @Specialization
        public static Object max(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object result = convMarker(numberOrMarker);
            Object hasNaN = isNaN(result) ? result : null;
            for (Object arg : numbersOrMarkers) {
                if (hasNaN == null && isNaN(arg)) {
                    hasNaN = arg;
                }
                if (compareTo(result, arg) < 0) { // NOPMD: total ordering needed
                    result = convMarker(arg);
                }
            }
            return hasNaN == null ? result : hasNaN;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 1) {
                return BuiltInDataFactory.NumberAsIsUnaryNodeGen.create(arguments[0]);
            }
            return varArgsToBinary(arguments, 0, BuiltInDataFactory.FMaxBinaryNodeGen::create);
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
    public abstract static class FMin extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static Object min(Object numberOrMarker, Object[] numbersOrMarkers) {
            Object result = FMax.convMarker(numberOrMarker);
            Object hasNaN = FMax.isNaN(result) ? result : null;
            for (Object arg : numbersOrMarkers) {
                if (hasNaN == null && FMax.isNaN(arg)) {
                    hasNaN = arg;
                }
                if (compareTo(result, arg) > 0) { // NOPMD: total ordering needed
                    result = FMax.convMarker(arg);
                }
            }
            return hasNaN == null ? result : hasNaN;
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            if (arguments.length == 1) {
                return BuiltInDataFactory.NumberAsIsUnaryNodeGen.create(arguments[0]);
            }
            return varArgsToBinary(arguments, 0, BuiltInDataFactory.FMinBinaryNodeGen::create);
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
    public abstract static class FLogand extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logandLong(Object[] intsOrMarkers) {
            long result = -1;
            for (Object arg : intsOrMarkers) {
                result &= (long) arg;
            }
            return result;
        }

        @TruffleBoundary
        @Specialization(replaces = "logandLong")
        public static Object logand(Object[] intsOrMarkers) {
            BigInteger result = BigInteger.ONE.negate();
            for (Object arg : intsOrMarkers) {
                result = result.and(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.asBigInteger();
                    default -> throw ELispSignals.wrongTypeArgument(INTEGER_OR_MARKER_P, arg);
                });
            }
            return ELispBigNum.wrap(result);
        }

        public static ELispExpressionNode createBitwiseNode(
                long defaultValue, ELispExpressionNode[] arguments,
                BiFunction<ELispExpressionNode,ELispExpressionNode, ELispExpressionNode> factory
        ) {
            if (arguments.length == 0) {
                return ELispLiteralNodes.of(defaultValue);
            }
            if (arguments.length == 1) {
                return BuiltInDataFactory.BitAsIsUnaryNodeGen.create(arguments[0]);
            }
            return varArgsToBinary(arguments, 0, factory);
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return createBitwiseNode(-1, arguments, BuiltInDataFactory.FLogandBinaryNodeGen::create);
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
    public abstract static class FLogior extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logorLong(Object[] intsOrMarkers) {
            long result = 0;
            for (Object arg : intsOrMarkers) {
                result |= (long) arg;
            }
            return result;
        }

        @TruffleBoundary
        @Specialization(replaces = "logorLong")
        public static Object logior(Object[] intsOrMarkers) {
            BigInteger result = BigInteger.ZERO;
            for (Object arg : intsOrMarkers) {
                result = result.or(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.asBigInteger();
                    default -> throw ELispSignals.wrongTypeArgument(INTEGER_OR_MARKER_P, arg);
                });
            }
            return ELispBigNum.wrap(result);
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return FLogand.createBitwiseNode(0, arguments, BuiltInDataFactory.FLogiorBinaryNodeGen::create);
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
    public abstract static class FLogxor extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization(rewriteOn = ClassCastException.class)
        public static long logxorLong(Object[] intsOrMarkers) {
            long result = 0;
            for (Object arg : intsOrMarkers) {
                result ^= (long) arg;
            }
            return result;
        }

        @TruffleBoundary
        @Specialization(replaces = "logxorLong")
        public static Object logxor(Object[] intsOrMarkers) {
            BigInteger result = BigInteger.ZERO;
            for (Object arg : intsOrMarkers) {
                result = result.xor(switch (arg) {
                    case Long l -> BigInteger.valueOf(l);
                    case ELispBigNum n -> n.asBigInteger();
                    default -> throw ELispSignals.wrongTypeArgument(INTEGER_OR_MARKER_P, arg);
                });
            }
            return ELispBigNum.wrap(result);
        }

        @Override
        public ELispExpressionNode createNode(ELispExpressionNode[] arguments) {
            return FLogand.createBitwiseNode(0, arguments, BuiltInDataFactory.FLogxorBinaryNodeGen::create);
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
            return value.bitCount();
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
        public static boolean isSafeAsh(long value, long count) {
            return 0 <= value && value <= Integer.MAX_VALUE && Math.abs(count) <= 31;
        }

        @Specialization(guards = {"isSafeAsh(value, count)"})
        public static long ashInt(long value, long count) {
            if (count == 0) {
                return value;
            }
            if (count < 0) {
                return value >> -count;
            }
            return value << count;
        }

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
            if (CompilerDirectives.injectBranchProbability(
                    CompilerDirectives.FASTPATH_PROBABILITY,
                    0 <= value && value <= Integer.MAX_VALUE && count <= 31
            )) {
                return value << count;
            }
            if (value == 0) {
                return 0L;
            }
            return ash(ELispBigNum.forceWrap(value), count);
        }

        @Specialization
        public static Object ash(ELispBigNum value, long count) {
            if (count > Short.MAX_VALUE) {
                throw ELispSignals.overflowError();
            }
            return value.shiftLeft(count);
        }

        @Fallback
        public static Object ashEdgeCases(Object value, Object count) {
            // @Fallback: we expect (long | ELispBigNum, ELispBigNum) as args
            if (!(count instanceof ELispBigNum c)) {
                throw ELispSignals.wrongTypeArgument(INTEGER_OR_MARKER_P, count);
            }
            int signum = switch (value) {
                case Long l -> Long.signum(l);
                case ELispBigNum n -> n.signum();
                default -> throw ELispSignals.wrongTypeArgument(INTEGER_OR_MARKER_P, value);
            };
            if (signum == 0) {
                return 0L;
            }
            if (c.signum() < 0) {
                return signum < 0 ? -1L : 0L; // signbit
            }
            throw ELispSignals.overflowError();
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
        public static boolean isSafeLong(long number) {
            return number < Long.MAX_VALUE;
        }
        @Specialization(guards = {"isSafeLong(number)"})
        public static long add1LongSafe(long number) {
            return number + 1;
        }
        @Specialization
        public static Object add1Long(long number) {
            return ELispBigNum.forceWrap(number).add1();
        }
        @Specialization
        public static Object add1BigNum(ELispBigNum number) {
            return number.add1();
        }
        @Specialization
        public static double add1Double(double number) {
            return number + 1;
        }
        @Fallback
        public static Object add1(Object number) {
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, number);
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
        public static boolean isSafeLong(long number) {
            return number > Long.MIN_VALUE;
        }
        @Specialization(guards = {"isSafeLong(number)"})
        public static long sub1LongSafe(long number) {
            return number - 1;
        }
        @Specialization
        public static Object sub1Long(long number) {
            return ELispBigNum.forceWrap(number).sub1();
        }
        @Specialization
        public static Object sub1BigNum(ELispBigNum number) {
            return number.sub1();
        }
        @Specialization
        public static double sub1Double(double number) {
            return number - 1;
        }
        @Fallback
        public static Object sub1(Object number) {
            throw ELispSignals.wrongTypeArgument(NUMBER_OR_MARKER_P, number);
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
            return number.not();
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
        public static Object boolVectorExclusiveOr(ELispBoolVector a, ELispBoolVector b, Object c) {
            if (a.size() != b.size()) {
                throw ELispSignals.wrongLengthArgument(a.size(), b.size());
            }
            ELispBoolVector dest = FBoolVectorNot.getOrNew(c, a);
            long[] destBits = dest.getBits();
            long[] aBits = a.getBits();
            long[] bBits = b.getBits();
            boolean changed = false;
            for (int i = 0; i < destBits.length; i++) {
                long old = destBits[i];
                destBits[i] = aBits[i] ^ bBits[i];
                changed = changed || destBits[i] != old;
            }
            return dest == c
                    ? (changed ? dest : false)
                    : dest;
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
        public static Object boolVectorUnion(ELispBoolVector a, ELispBoolVector b, Object c) {
            if (a.size() != b.size()) {
                throw ELispSignals.wrongLengthArgument(a.size(), b.size());
            }
            ELispBoolVector dest = FBoolVectorNot.getOrNew(c, a);
            long[] destBits = dest.getBits();
            long[] aBits = a.getBits();
            long[] bBits = b.getBits();
            boolean changed = false;
            for (int i = 0; i < destBits.length; i++) {
                long old = destBits[i];
                destBits[i] = aBits[i] | bBits[i];
                changed = changed || destBits[i] != old;
            }
            return dest == c
                    ? (changed ? dest : false)
                    : dest;
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
        public static Object boolVectorIntersection(ELispBoolVector a, ELispBoolVector b, Object c) {
            if (a.size() != b.size()) {
                throw ELispSignals.wrongLengthArgument(a.size(), b.size());
            }
            ELispBoolVector dest = FBoolVectorNot.getOrNew(c, a);
            long[] destBits = dest.getBits();
            long[] aBits = a.getBits();
            long[] bBits = b.getBits();
            boolean changed = false;
            for (int i = 0; i < destBits.length; i++) {
                long old = destBits[i];
                destBits[i] = aBits[i] & bBits[i];
                changed = changed || destBits[i] != old;
            }
            return dest == c
                    ? (changed ? dest : false)
                    : dest;
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
        public static Object boolVectorSetDifference(ELispBoolVector a, ELispBoolVector b, Object c) {
            if (a.size() != b.size()) {
                throw ELispSignals.wrongLengthArgument(a.size(), b.size());
            }
            ELispBoolVector dest = FBoolVectorNot.getOrNew(c, a);
            long[] destBits = dest.getBits();
            long[] aBits = a.getBits();
            long[] bBits = b.getBits();
            boolean changed = false;
            for (int i = 0; i < destBits.length; i++) {
                long old = destBits[i];
                destBits[i] = aBits[i] & ~bBits[i];
                changed = changed || destBits[i] != old;
            }
            return dest == c
                    ? (changed ? dest : false)
                    : dest;
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
        public static boolean boolVectorSubsetp(ELispBoolVector a, ELispBoolVector b) {
            if (a.size() != b.size()) {
                throw ELispSignals.wrongLengthArgument(a.size(), b.size());
            }
            long[] aBits = a.getBits();
            long[] bBits = b.getBits();
            boolean subset = true;
            for (int i = 0; i < aBits.length; i++) {
                subset = subset && aBits[i] == (aBits[i] & bBits[i]);
            }
            return subset;
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
        public static ELispBoolVector getOrNew(Object c, ELispBoolVector a) {
            if (isNil(c)) {
                return new ELispBoolVector(new long[a.getBits().length], a.size());
            }
            ELispBoolVector vec = asBoolVec(c);
            if (vec.size() != a.size()) {
                throw ELispSignals.wrongLengthArgument(a.size(), vec.size());
            }
            return vec;
        }


        @Specialization
        public static ELispBoolVector boolVectorNot(ELispBoolVector a, Object b) {
            ELispBoolVector dest = getOrNew(b, a);
            long[] destBits = dest.getBits();
            long[] aBits = a.getBits();
            for (int i = 0; i < destBits.length; i++) {
                destBits[i] = ~aBits[i];
            }
            dest.trimTrailingBits();
            return dest;
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
        public static long boolVectorCountPopulation(ELispBoolVector a) {
            return a.cardinality();
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
        public static long boolVectorCountConsecutive(ELispBoolVector a, boolean b, long i) {
            BitSet bits = BitSet.valueOf(a.getBits());
            int start = (int) i;
            int end = b ? bits.nextClearBit(start) : bits.nextSetBit(start);
            int size = a.size();
            return (end == -1 || end > size ? size : end) - i;
        }
    }
}
