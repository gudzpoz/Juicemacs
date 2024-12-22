package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

public final class GlobalVariableLookupNode extends ELispBaseNode {
    private final ELispSymbol symbol;
    @CompilerDirectives.CompilationFinal
    private int index = -1;

    public GlobalVariableLookupNode(ELispSymbol symbol) {
        this.symbol = symbol;
    }

    public ValueStorage execute() {
        if (index == -1) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            index = getLanguage().getGlobalVariableIndex(symbol);
        }
        return getContext().getValueStorage(index);
    }
}
