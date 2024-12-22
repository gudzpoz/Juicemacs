package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;

import java.util.Optional;

@GenerateInline(inlineByDefault = true)
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public abstract class GlobalIndirectFunctionLookupNode extends ELispBaseNode {
    public abstract Optional<FunctionStorage> execute(Node node, ELispSymbol symbol);

    @Idempotent
    boolean isPresent(Optional<?> optional) {
        return optional.isPresent();
    }

    @Specialization(
            guards = {"lastSymbol == symbol", "isPresent(storage)"},
            limit = "3"
    )
    Optional<FunctionStorage> lookupCached(
            ELispSymbol symbol,
            @Cached("symbol") ELispSymbol lastSymbol,
            @Cached("getContext().getFunctionStorageLazy(lastSymbol)") Optional<FunctionStorage> storage
    ) {
        return storage;
    }

    @Specialization
    Optional<FunctionStorage> lookupUncached(ELispSymbol symbol) {
        return getContext().getFunctionStorageLazy(symbol);
    }
}
