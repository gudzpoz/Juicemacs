package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.MACRO;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;

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
        switch (function) {
            case ELispSymbol _ -> stableAssumption.getAssumption().invalidate();
            case ELispInterpretedClosure closure -> closure.setName(symbol);
            case ELispCons cons when cons.car() == MACRO && cons.cdr() instanceof ELispInterpretedClosure closure ->
                    closure.setName(symbol);
            default -> {}
        }
    }

    public Assumption getStableAssumption() {
        return stableAssumption.getAssumption();
    }
}
