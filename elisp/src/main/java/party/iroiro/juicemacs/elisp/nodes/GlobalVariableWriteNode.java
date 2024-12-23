package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

public abstract class GlobalVariableWriteNode extends ELispExpressionNode {
    public final ELispSymbol symbol;

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispExpressionNode value;

    protected GlobalVariableWriteNode(ELispSymbol symbol, ELispExpressionNode value) {
        this.symbol = symbol;
        this.value = value;
    }

    @Specialization
    Object write(
            VirtualFrame frame,
            @Cached(value = "getContext().getStorage(symbol)", neverDefault = true) ValueStorage storage
    ) {
        Object value = this.value.executeGeneric(frame);
        storage.setValue(value, symbol);
        return value;
    }
}
