package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.interop.NodeLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.scopes.DebuggerScopeObject;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

@GenerateWrapper
@ExportLibrary(NodeLibrary.class)
public abstract class ConsCallNode extends ELispExpressionNode implements InstrumentableNode {
    protected final ELispCons cons;

    public ConsCallNode(ELispCons cons) {
        this.cons = cons;
    }

    public ConsCallNode(ConsCallNode copy) {
        this(copy.cons);
    }

    protected static ELispExpressionNode[] initChildren(ELispCons cons, int reserved) {
        // Trade time for allocation amount. Hopefully, most conses are short.
        ELispExpressionNode[] children = new ELispExpressionNode[cons.size() - 1 + reserved];
        ConsIterator argIterator = cons.listIterator(1);
        for (int i = reserved; argIterator.hasNext(); i++) {
            Object arg = argIterator.next();
            children[i] = ELispInterpretedNode.create(arg);
        }
        return children;
    }

    public static Object[] argsArray(ELispCons cons) {
        Object cdr = cons.cdr();
        if (isNil(cdr)) {
            return new Object[0];
        } else {
            return asCons(cdr).toArray();
        }
    }

    public static Object[] argsArrayWithFunc(Object function, ELispCons cons) {
        Object cdr = cons.cdr();
        if (isNil(cdr)) {
            return new Object[]{function};
        } else {
            Object[] array = cons.toArray();
            array[0] = function;
            return array;
        }
    }

    public ELispCons getCons() {
        return cons;
    }

    @Override
    public boolean isInstrumentable() {
        SourceSection source = getSourceSection();
        return source != null && source.isAvailable();
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ConsCallNodeWrapper(this, this, probe);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        return tag == StandardTags.StatementTag.class
               || tag == StandardTags.CallTag.class
               || tag == StandardTags.ExpressionTag.class;
    }

    @Override
    public SourceSection getSourceSection() {
        return ELispInterpretedNode.getConsSourceSection(this, cons);
    }

    //#region NodeLibrary
    @ExportMessage
    public boolean hasScope(Frame frame) {
        ELispLexical.@Nullable Scope lexical = ELispLexical.getScope(this);
        return frame != null && lexical != null;
    }

    @ExportMessage
    public Object getScope(Frame frame, boolean nodeEnter) throws UnsupportedMessageException {
        ELispLexical.@Nullable Scope lexical = ELispLexical.getScope(this);
        if (frame == null || lexical == null) {
            throw UnsupportedMessageException.create();
        }
        return new DebuggerScopeObject(getContext(), lexical, frame.materialize());
    }
    //#endregion NodeLibrary
}
