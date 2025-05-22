package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.MACRO;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.toSym;

public final class FunctionStorage {
    private final CyclicAssumption stableAssumption = new CyclicAssumption("stable function");

    private Object function = false;

    public Object get() {
        return function;
    }

    public void set(Object function, ELispSymbol symbol) {
        if (function == NIL) {
            function = false;
        }
        this.function = function;
        stableAssumption.invalidate();
        switch (toSym(function)) {
            case ELispSymbol _ -> stableAssumption.getAssumption().invalidate();
            case AbstractELispClosure closure -> closure.setName(symbol);
            case ELispCons cons when cons.car() == MACRO -> {
                if (cons.cdr() instanceof AbstractELispClosure closure) {
                    closure.setName(symbol);
                }
            }
            default -> {}
        }
    }

    public Assumption getStableAssumption() {
        return stableAssumption.getAssumption();
    }
}
