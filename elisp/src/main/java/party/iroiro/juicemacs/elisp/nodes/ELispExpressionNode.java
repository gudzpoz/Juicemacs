package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystemGen;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@TypeSystemReference(ELispTypeSystem.class)
public abstract class ELispExpressionNode extends Node {

    public void executeVoid(VirtualFrame frame) {
        executeGeneric(frame);
    }

    public boolean executeBoolean(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectBoolean(executeGeneric(frame));
    }

    public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectLong(executeGeneric(frame));
    }

    public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
        return ELispTypeSystemGen.expectDouble(executeGeneric(frame));
    }

    public abstract Object executeGeneric(VirtualFrame frame);

    private final static Pattern CLASS_CAST_GUESS =
            Pattern.compile("(?=class )?(\\S+) cannot be cast to (=?class )?(\\S+)");

    private final static Map<String, ELispSymbol> CLASS_CAST_MAP;

    static {
        CLASS_CAST_MAP = Map.of(
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol",
                ELispContext.SYMBOLP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispCons",
                ELispContext.CONSP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispString",
                ELispContext.STRINGP,
                "java.lang.Long",
                ELispContext.INTEGERP,
                "java.lang.Double",
                ELispContext.FLOATP
        );
    }

    @CompilerDirectives.TruffleBoundary
    public static RuntimeException remapException(RuntimeException e) {
        if (e instanceof ClassCastException) {
            Matcher matcher = CLASS_CAST_GUESS.matcher(e.getMessage());
            if (matcher.find()) {
                String actual = matcher.group(1);
                String expected = matcher.group(2);
                ELispSymbol predicate = CLASS_CAST_MAP.get(expected);
                if (predicate == null) {
                    predicate = ELispContext.intern(expected);
                }
                throw ELispSignals.wrongTypeArgument(predicate, actual);
            }
            return ELispSignals.wrongTypeArgument(ELispContext.UNSPECIFIED, e.getMessage());
        }
        if (e instanceof UnsupportedSpecializationException) {
            return ELispSignals.wrongTypeArgument(ELispContext.UNSPECIFIED, e.getMessage());
        }
        return e;
    }

}
