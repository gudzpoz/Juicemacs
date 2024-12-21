package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.concurrent.ConcurrentHashMap;

public record ELispObarray(ConcurrentHashMap<MuleString, ELispSymbol> symbols) {
    public ELispSymbol intern(MuleString name) {
        return symbols.computeIfAbsent(name, ELispSymbol::new);
    }

    public ELispSymbol intern(String name) {
        return intern(MuleString.fromString(name));
    }

    @Nullable
    public ELispSymbol internSoft(MuleString value) {
        return symbols.get(value);
    }

    @Nullable
    public ELispSymbol unintern(MuleString name) {
        return symbols.remove(name);
    }
}
