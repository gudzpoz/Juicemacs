package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.HashMap;

public final class ELispObarray extends AbstractELispIdentityObject implements ELispValue {
    private final HashMap<String, ELispSymbol> symbols;

    public ELispObarray(HashMap<String, ELispSymbol> symbols) {
        this.symbols = symbols;
    }

    public HashMap<String, ELispSymbol> symbols() {
        return symbols;
    }

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    public ELispSymbol intern(AbstractTruffleString name) {
        return intern(name.toString());
    }

    @CompilerDirectives.TruffleBoundary
    public ELispSymbol intern(String name) {
        return symbols.computeIfAbsent(name, ELispSymbol::new);
    }

    @CompilerDirectives.TruffleBoundary
    @Nullable
    public ELispSymbol internSoft(String value) {
        return symbols.get(value);
    }

    @CompilerDirectives.TruffleBoundary
    @Nullable
    public ELispSymbol unintern(String name) {
        return symbols.remove(name);
    }

    @Override
    public void display(ELispPrint print) {
        print.print(toString());
    }

    @Override
    public String toString() {
        return "#<obarray@" + Integer.toHexString(System.identityHashCode(this)) + ">";
    }
}
