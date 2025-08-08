package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIn;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public final class ConsInlinedAstNode extends ConsCallNode {
    @Child
    ELispExpressionNode inlinedNode;

    private final Assumption stable;

    ConsInlinedAstNode(Assumption stable, ELispSubroutine inline, ELispCons cons) {
        super(cons);
        this.inlinedNode = generateInlineNode(cons, Objects.requireNonNull(inline));
        this.stable = stable;
    }

    private static ELispExpressionNode generateInlineNode(ELispCons cons, ELispSubroutine inline) {
        ELispBuiltIn info = inline.info();
        int args = cons.size() - 1;
        if (args < info.minArgs() || (!info.varArgs() && info.maxArgs() < args)) {
            throw ELispSignals.wrongNumberOfArguments(cons.car(), args);
        }
        if (inline.specialForm()) {
            return inline.createNode(argsArray(cons));
        } else if (inline.isTailored()) {
            return inline.createNode(initChildren(cons, 0));
        }
        List<ELispExpressionNode> nodes = new ArrayList<>();
        List<ELispExpressionNode> restNodes = new ArrayList<>();
        ELispCons.ConsIterator argIterator = cons.listIterator(1);
        while (argIterator.hasNext()) {
            ELispExpressionNode node = ELispInterpretedNode.create(argIterator.next());
            if (nodes.size() < info.maxArgs()) {
                nodes.add(node);
            } else {
                restNodes.add(node);
            }
        }
        while (nodes.size() < info.maxArgs()) {
            nodes.add(ELispLiteralNodes.of(false));
        }
        if (info.varArgs()) {
            nodes.add(new VarargToArrayNode(restNodes));
        }
        ELispExpressionNode[] arguments = nodes.toArray(ELispExpressionNode[]::new);
        return inline.createNode(arguments);
    }

    @Override
    public void executeVoid(VirtualFrame frame) {
        if (!stable.isValid()) {
            rewriteNode().executeVoid(frame);
            return;
        }
        try {
            inlinedNode.executeVoid(frame);
        } catch (ELispSignals.ELispSignalException
                 | ArithmeticException
                 | ClassCastException | UnsupportedSpecializationException e) {
            throw rewriteException(e);
        }
    }

    @Override
    public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        if (!stable.isValid()) {
            return rewriteNode().executeLong(frame);
        }
        try {
            return inlinedNode.executeLong(frame);
        } catch (ELispSignals.ELispSignalException
                 | ArithmeticException
                 | ClassCastException | UnsupportedSpecializationException e) {
            throw rewriteException(e);
        }
    }

    @Override
    public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
        if (!stable.isValid()) {
            return rewriteNode().executeDouble(frame);
        }
        try {
            return inlinedNode.executeDouble(frame);
        } catch (ELispSignals.ELispSignalException
                 | ArithmeticException
                 | ClassCastException | UnsupportedSpecializationException e) {
            throw rewriteException(e);
        }
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (!stable.isValid()) {
            return rewriteNode().executeGeneric(frame);
        }
        try {
            return inlinedNode.executeGeneric(frame);
        } catch (ELispSignals.ELispSignalException
                 | ArithmeticException
                 | ClassCastException | UnsupportedSpecializationException e) {
            throw rewriteException(e);
        }
    }

    @CompilerDirectives.TruffleBoundary
    public RuntimeException rewriteException(RuntimeException e) {
        return ELispSignals.remapException(e, getParent() == null ? this : getParent());
    }

    public ELispExpressionNode rewriteNode() {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        return replace(new LazyConsExpressionNode(cons));
    }

    public static class VarargToArrayNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ELispExpressionNode[] restArgs;

        public VarargToArrayNode(List<ELispExpressionNode> restNodes) {
            restArgs = restNodes.toArray(ELispExpressionNode[]::new);
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            super.executeVoid(frame);
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Object[] args = new Object[restArgs.length];
            for (int i = 0; i < restArgs.length; i++) {
                args[i] = restArgs[i].executeGeneric(frame);
            }
            return args;
        }
    }
}
