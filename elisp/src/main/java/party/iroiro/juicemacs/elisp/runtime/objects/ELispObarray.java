package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.HashMap;

public record ELispObarray(HashMap<MuleString, ELispSymbol> symbols) {
    @CompilerDirectives.TruffleBoundary
    public ELispSymbol intern(MuleString name) {
        return symbols.computeIfAbsent(name, ELispSymbol::new);
    }

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    public ELispSymbol intern(String name) {
        return intern(MuleString.fromString(name));
    }

    @CompilerDirectives.TruffleBoundary
    @Nullable
    public ELispSymbol internSoft(MuleString value) {
        return symbols.get(value);
    }

    @CompilerDirectives.TruffleBoundary
    @Nullable
    public ELispSymbol unintern(MuleString name) {
        return symbols.remove(name);
    }
}
