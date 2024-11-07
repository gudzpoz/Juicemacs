package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

@NodeChild(value = "arguments", type = ELispExpressionNode[].class)
public abstract class ELispBuiltInBaseNode extends ELispExpressionNode {
    public static final Source JAVA_SOURCE = Source.newBuilder("java", "", "<built-in>")
            .content(Source.CONTENT_NONE)
            .internal(true)
            .build();

    @Override
    public SourceSection getSourceSection() {
        return JAVA_SOURCE.createUnavailableSection();
    }

    public static int asInt(Object value) {
        if (value instanceof Long l) {
            return l.intValue();
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.INTEGERP, value);
    }

    public static long asRanged(Object value, long left, long right) {
        long i = asLong(value);
        if (i < left || i > right) {
            throw ELispSignals.argsOutOfRange(value, left, right);
        }
        return i;
    }

    public static long asLong(Object value) {
        if (value instanceof Long l) {
            return l;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.INTEGERP, value);
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
            default -> throw ELispSignals.wrongTypeArgument(ELispContext.INTEGERP, value);
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
        throw ELispSignals.wrongTypeArgument(ELispContext.FLOATP, value);
    }

    public static double toDouble(double value) {
        return value;
    }

    public static Number asNum(Object value) {
        if (value instanceof Number n) {
            return n;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.NUMBERP, value);
    }

    public static ELispCons asCons(Object value) {
        if (value instanceof ELispCons c) {
            return c;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.CONSP, value);
    }

    public static ELispSymbol asSym(Object value) {
        if (value instanceof ELispSymbol s) {
            return s;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.SYMBOLP, value);
    }

    public static ELispString asStr(Object s) {
        if (s instanceof ELispString str) {
            return str;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.STRINGP, s);
    }

    public static ELispBuffer asBuffer(Object buffer) {
        if (buffer instanceof ELispBuffer b) {
            return b;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.BUFFERP, buffer);
    }

    public static ELispCharTable asCharTable(Object table) {
        if (table instanceof ELispCharTable t) {
            return t;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.CHAR_TABLE_P, table);
    }

    /// A factory interface for hand-rolled built-in function inlining
    ///
    /// For example, as a function, `+` needs to handle all numeric types and is relatively
    /// slow in cases like `(+ 1 2.0 some-big-num)`. Instead, we can inline the function
    /// into `(ast_+ (ast_+ 1 2.0) some-big-num)` and avoid the costs of function calls and
    /// have each AST node get their own specialized implementation.
    ///
    /// @see BuiltInData.FPlusBinary
    /// @see BuiltInData.FPlus
    public interface InlineFactory {
        ELispExpressionNode createNode(ELispExpressionNode[] arguments);
    }
}
