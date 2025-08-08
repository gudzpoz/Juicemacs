package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.GlobalVariableReadNodeGen;
import party.iroiro.juicemacs.elisp.nodes.local.ELispFrameSlotReadNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

public final class ELispSymbolDereferenceNode extends ELispExpressionNode {
    private final ELispSymbol symbol;

    public ELispSymbolDereferenceNode(ELispSymbol symbol) {
        this.symbol = symbol;
    }

    @Override
    public void executeVoid(VirtualFrame frame) {
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return updateSlotInfo().executeGeneric(frame);
    }

    private ELispExpressionNode updateSlotInfo() {
        ELispLexical.LexicalReference lexical = ELispLexical.getLexicalReference(this, symbol);
        CompilerDirectives.transferToInterpreterAndInvalidate();

        if (lexical == null) {
            return replace(GlobalVariableReadNodeGen.create(symbol));
        } else {
            ELispExpressionNode reader = ELispFrameSlotReadNode.createRead(lexical);
            return replace(reader);
        }
    }
}
