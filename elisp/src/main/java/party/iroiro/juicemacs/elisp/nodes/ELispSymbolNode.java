package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

public class ELispSymbolNode extends ELispExpressionNode {

    private final ELispSymbol symbol;

    public ELispSymbolNode(ELispSymbol symbol) {
        this.symbol = symbol;
    }

    @Override
    public ELispSymbol executeGeneric(VirtualFrame frame) {
        return symbol;
    }

}
