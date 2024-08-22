package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.ArrayList;

public class ELispVector extends ArrayList<Object> implements ELispValue {

    @Override
    public String type() {
        return "vector";
    }

}
