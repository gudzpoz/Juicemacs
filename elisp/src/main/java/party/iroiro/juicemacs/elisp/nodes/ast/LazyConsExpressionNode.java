package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNodeGen;
import party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionObjectNodes;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBytecode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.Set;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.MACRO;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.toSym;

public final class LazyConsExpressionNode extends ELispExpressionNode implements InstrumentableNode {
    public final ELispCons cons;

    public LazyConsExpressionNode(ELispCons cons) {
        this.cons = cons;
    }

    @Override
    public void executeVoid(VirtualFrame frame) {
        updateInnerNode().executeVoid(frame);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return updateInnerNode().executeGeneric(frame);
    }

    private ELispExpressionNode updateInnerNode() {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        Object cardinal = cons.car();
        @Nullable Assumption stable = toSym(cardinal) instanceof ELispSymbol sym
                ? getContext().getFunctionStorage(sym).getStableAssumption()
                : null;
        Object function = ReadFunctionObjectNodes.getFunctionUncached(this, cardinal);
        if (function instanceof ELispSubroutine sub) {
            if (sub.specialForm()) {
                return replace(sub.createNode(ConsCallNode.argsArray(cons)));
            }
        }
        ELispExpressionNode created = switch (function) {
            case ELispCons c when c.car() == MACRO -> {
                Object callable = ReadFunctionObjectNodes.getFunctionUncached(this, c.cdr());
                try (Dynamic _ = Dynamic.withLexicalBinding(ELispLexical.getScope(this) != null)) {
                    Object[] args = ConsCallNode.argsArrayWithFunc(callable, cons);
                    Object o = FuncallDispatchNodeGen.getUncached().executeDispatch(this, args);
                    yield ELispInterpretedNode.createWithLocation(o, cons);
                }
            }
            case ELispSubroutine sub when sub.inlinable() && stable != null && stable != Assumption.NEVER_VALID ->
                    new ConsInlinedAstNode(stable, sub, cons);
            case ELispSubroutine _, ELispInterpretedClosure _, ELispBytecode _ ->
                    ConsFunctionCallNodeGen.create(cons);
            default -> throw ELispSignals.invalidFunction(cons.car());
        };
        return replace(created);
    }

    @Override
    public SourceSection getSourceSection() {
        return ELispInterpretedNode.getConsSourceSection(this, cons);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        return false;
    }

    @Override
    public InstrumentableNode materializeInstrumentableNodes(Set<Class<? extends Tag>> materializedTags) {
        expandAllChildren();
        return this;
    }

    private void expandAllChildren() {
        try {
            updateInnerNode(); // NOPMD
        } catch (Throwable ignored) {
        }
    }
}
