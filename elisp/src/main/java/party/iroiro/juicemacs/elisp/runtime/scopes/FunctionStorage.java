package party.iroiro.juicemacs.elisp.runtime.scopes;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.MACRO;

public final class FunctionStorage {
    private Object function = false;

    public Object get() {
        return function;
    }

    public void set(Object function, ELispSymbol symbol) {
        Object original = get();
        if (original instanceof ELispSubroutine(_, _, ELispSubroutine.InlineInfo inline) && inline != null) {
            inline.stable().invalidate();
        }
        if (function instanceof ELispInterpretedClosure closure) {
            closure.setName(symbol);
        }
        if (function instanceof ELispCons cons && cons.car() == MACRO
                && cons.cdr() instanceof ELispInterpretedClosure closure) {
            closure.setName(symbol);
        }
        this.function = function;
    }
}
