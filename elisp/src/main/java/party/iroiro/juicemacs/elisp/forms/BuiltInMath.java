package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;

import java.math.BigInteger;
import java.util.List;

public class BuiltInMath extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInMathFactory.getFactories();
    }

    @ELispBuiltIn(name = "1+", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FAdd1 extends ELispBuiltInBaseNode {
        @Specialization
        public Object add1Long(Object arg) {
            return switch (arg) {
                case Long l when l < Long.MAX_VALUE -> l + 1;
                case Long l -> new ELispBigNum(BigInteger.valueOf(l).add(BigInteger.ONE));
                case Double d -> d + 1;
                case ELispBigNum(BigInteger i) -> new ELispBigNum(i.add(BigInteger.ONE));
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "1-", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSub1 extends ELispBuiltInBaseNode {
        @Specialization
        public Object add1Long(Object arg) {
            return switch (arg) {
                case Long l when l > Long.MIN_VALUE -> l - 1;
                case Long l -> new ELispBigNum(BigInteger.valueOf(l).subtract(BigInteger.ONE));
                case Double d -> d - 1;
                case ELispBigNum(BigInteger i) -> new ELispBigNum(i.subtract(BigInteger.ONE));
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @ELispBuiltIn(name = "+", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class Add extends ELispBuiltInBaseNode {
        @Specialization(rewriteOn = {ArithmeticException.class, ClassCastException.class})
        public long addLong(Object[] args) {
            long sum = 0;
            for (Object arg : args) {
                sum = Math.addExact(sum, (Long) arg);
            }
            return sum;
        }

        @Specialization(replaces = "addLong")
        public Object addAny(Object[] args) {
            return tryAddLong(args);
        }

        public Object tryAddLong(Object[] args) {
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

        private Object tryAddBigNum(long prev, int i, Object[] args) {
            BigInteger sum = BigInteger.valueOf(prev);
            for (; i < args.length; i++) {
                switch (args[i]) {
                    case ELispBigNum(BigInteger n) -> sum = sum.add(n);
                    case Long l -> sum = sum.add(BigInteger.valueOf(l));
                    case Double _ -> {
                        return tryAddDouble(sum.doubleValue(), i, args);
                    }
                    case null, default -> throw new IllegalArgumentException();
                }
            }
            ELispBigNum result = new ELispBigNum(sum);
            return result.fitsInLong() ? result.value().longValue() : result;
        }

        private double tryAddDouble(double prev, int i, Object[] args) {
            double sum = prev;
            for (; i < args.length; i++) {
                sum += ELispTypeSystemGen.asImplicitDouble(args[i]);
            }
            return sum;
        }
    }

}
