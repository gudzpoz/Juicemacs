package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

@NodeChild(value = "arguments", type = ReadFunctionArgNode[].class)
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

    public static long asLong(Object value) {
        if (value instanceof Long l) {
            return l;
        }
        throw ELispSignals.wrongTypeArgument(ELispContext.INTEGERP, value);
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
}
