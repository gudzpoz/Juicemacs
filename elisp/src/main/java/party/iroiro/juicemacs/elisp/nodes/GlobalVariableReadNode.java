package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.dsl.Specialization;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.util.Optional;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public abstract class GlobalVariableReadNode extends ELispExpressionNode {
    public final ELispSymbol symbol;
    @Nullable
    @Child
    GlobalVariableLookupNode lookupNode;

    protected GlobalVariableReadNode(ELispSymbol symbol) {
        this.symbol = symbol;
    }

    @Idempotent
    static boolean isPresent(Optional<?> optional) {
        return optional.isPresent();
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    @Idempotent
    static ValueStorage getStorageInner(Optional<ValueStorage> storageLazy) {
        return storageLazy.get();
    }

    @Specialization(
            assumptions = "storage.getUnchangedAssumption()",
            guards = "isPresent(storageLazy)"
    )
    Object readConstant(
            @Cached(value = "getContext().getStorageLazy(symbol)", neverDefault = true) Optional<ValueStorage> storageLazy,
            @Bind(value = "getStorageInner(storageLazy)") ValueStorage storage,
            @Cached(value = "storage.getAnyValue()", neverDefault = true) Object rawValue
    ) {
        if (ValueStorage.UNBOUND == rawValue) {
            throw ELispSignals.voidVariable(symbol);
        }
        return rawValue;
    }

    @Specialization
    Object read() {
        if (lookupNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            lookupNode = insert(new GlobalVariableLookupNode(symbol));
        }
        ValueStorage storage = lookupNode.execute();
        Object rawValue = storage.getAnyValue();
        if (ValueStorage.UNBOUND == rawValue) {
            throw ELispSignals.voidVariable(symbol);
        }
        return rawValue;
    }
}
