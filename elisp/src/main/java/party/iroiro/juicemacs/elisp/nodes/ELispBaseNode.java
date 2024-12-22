package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

public abstract class ELispBaseNode extends Node {
    protected final <T extends Node> T insertOrReplace(T node, @Nullable T old) {
        return old == null ? insert(node) : old.replace(node);
    }

    protected final ELispLanguage getLanguage() {
        return ELispLanguage.get(this);
    }

    protected final ELispContext getContext() {
        return ELispContext.get(this);
    }

}
