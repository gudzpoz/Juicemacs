package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.util.Optional;

@GenerateInline(inlineByDefault = true)
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public abstract class GlobalIndirectLookupNode extends ELispBaseNode {
    public abstract Optional<ValueStorage> execute(Node node, ELispSymbol symbol);

    @Idempotent
    boolean isPresent(Optional<?> optional) {
        return optional.isPresent();
    }

    @Specialization(
            guards = {"lastSymbol == symbol", "isPresent(storage)"},
            limit = "3"
    )
    Optional<ValueStorage> lookupCached(
            ELispSymbol symbol,
            @Cached("symbol") ELispSymbol lastSymbol,
            @Cached("getContext().getStorageLazy(lastSymbol)") Optional<ValueStorage> storage
    ) {
        return storage;
    }

    @Specialization
    Optional<ValueStorage> lookupUncached(ELispSymbol symbol) {
        return getContext().getStorageLazy(symbol);
    }
}
