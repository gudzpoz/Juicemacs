package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.List;

public class ELispInterpretedClosure extends AbstractELispVector {
    public ELispInterpretedClosure(
            Object args, ELispCons body, Object env, Object doc, Object iForm) {
        super(List.of(args, body, env, doc, iForm));
    }
}
