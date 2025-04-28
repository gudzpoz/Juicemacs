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
        storage.setValue(value, symbol, getContext());
        return value;
    }

    public static abstract class GlobalVariableDirectWriteNode extends ELispBaseNode {
        public final ELispSymbol symbol;

        protected GlobalVariableDirectWriteNode(ELispSymbol symbol) {
            this.symbol = symbol;
        }

        public abstract void execute(VirtualFrame frame, Object value);

        @Specialization
        void write(
                Object value,
                @Cached(value = "getContext().getStorage(symbol)", neverDefault = true) ValueStorage storage
        ) {
            storage.setValue(value, symbol, getContext());
        }
    }
}
