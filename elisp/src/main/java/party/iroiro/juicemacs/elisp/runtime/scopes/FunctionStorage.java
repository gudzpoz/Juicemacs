package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBytecode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

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
            case ELispInterpretedClosure closure -> closure.setName(symbol);
            case ELispBytecode bytecode -> bytecode.setName(symbol);
            case ELispCons cons when cons.car() == MACRO -> {
                switch (cons.cdr()) {
                    case ELispInterpretedClosure closure -> closure.setName(symbol);
                    case ELispBytecode bytecode -> bytecode.setName(symbol);
                    default -> {}
                }
            }
            default -> {}
        }
    }

    public Assumption getStableAssumption() {
        return stableAssumption.getAssumption();
    }
}
