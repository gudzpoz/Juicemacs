package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;

import java.util.HashMap;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol.Value.Forwarded;

public final class ELispBuffer implements ELispValue {
    private final HashMap<ELispSymbol, Forwarded> localVariables;

    public ELispBuffer() {
        localVariables = new HashMap<>();
    }

    @Nullable
    public Forwarded getLocal(ELispSymbol symbol) {
        return localVariables.get(symbol);
    }

    public Forwarded makeLocal(ELispSymbol symbol) {
        return localVariables.computeIfAbsent(symbol, _ -> new Forwarded());
    }

    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }
}
