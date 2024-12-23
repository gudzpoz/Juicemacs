package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispObarray;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.MuleString;

import java.lang.ref.Cleaner;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asBuffer;

/**
 * ELisp runtime context
 */
public final class ELispContext implements ELispParser.InternContext {
    private static final TruffleLanguage.ContextReference<ELispContext> CONTEXT_REFERENCE =
            TruffleLanguage.ContextReference.create(ELispLanguage.class);

    private final ELispLanguage language;
    private final ConcurrentHashMap<String, String> env;
    private final Options options;
    private final boolean postInit;

    private final ELispGlobals globals;
    private final SharedIndicesMap.ContextArray<ValueStorage> variablesArray;
    private final SharedIndicesMap.ContextArray<FunctionStorage> functionsArray;
    private final Options options;
    private final CyclicAssumption specialVariablesUnchanged;
    private final Cleaner cleaner = Cleaner.create();

    public ELispContext(ELispLanguage language, TruffleLanguage.@Nullable Env env) {
        this.language = language;
        if (env == null) {
            this.env = new ConcurrentHashMap<>();
            this.postInit = false;
            this.options = new Options(3);
        } else {
            this.env = new ConcurrentHashMap<>(env.getEnvironment());
            this.postInit = !env.getOptions().get(ELispLanguage.BARE);
            this.options = new Options(env.getOptions().get(ELispLanguage.GLOBAL_MAX_INVALIDATIONS));
        }
        this.globals = new ELispGlobals(this);
        variablesArray = new SharedIndicesMap.ContextArray<>(
                language.globalVariablesMap,
                ValueStorage[]::new,
                ValueStorage::new
        );
        functionsArray = new SharedIndicesMap.ContextArray<>(
                language.globalFunctionsMap,
                FunctionStorage[]::new,
                FunctionStorage::new
        );
        specialVariablesUnchanged = new CyclicAssumption("Special variables unchanged");
    }

    public ELispGlobals globals() {
        return globals;
    }

    public ELispObarray obarray() {
        return globals.globalObarray;
    }

    public Options options() {
        return options;
    }

    //#region InternContext
    @Override
    public ELispSymbol intern(String name) {
        return obarray().intern(name);
    }
    @Override
    public ELispSymbol intern(MuleString name) {
        return obarray().intern(name);
    }
    @Override
    public MuleString applyShorthands(MuleString symbol) {
        // TODO: Implementation
        return symbol;
    }
    //#endregion InternContext

    public ConcurrentHashMap<String, String> env() {
        return env;
    }

    @Nullable
    public String getEnv(String key) {
        return env.get(key);
    }

    public ELispBuffer currentBuffer() {
        return asBuffer(language.currentBuffer().getValue());
    }

    public void initGlobal(ELispLanguage language) {
        globals.init(language, postInit);
    }

    //#region Symbol lookup
    public ValueStorage getStorage(ELispSymbol symbol) {
        int index = language.getGlobalVariableIndex(symbol);
        return variablesArray.getDynamic(index);
    }
    @Idempotent
    public Optional<ValueStorage> getStorageLazy(ELispSymbol symbol) {
        int index = language.tryGetGlobalVariableIndex(symbol);
        if (index == -1) {
            return Optional.empty();
        }
        return Optional.of(variablesArray.getDynamic(index));
    }
    public FunctionStorage getFunctionStorage(ELispSymbol symbol) {
        int index = language.getGlobalFunctionIndex(symbol);
        return functionsArray.getDynamic(index);
    }
    @Idempotent
    public Optional<FunctionStorage> getFunctionStorageLazy(ELispSymbol symbol) {
        int index = language.tryGetGlobalFunctionIndex(symbol);
        if (index == -1) {
            return Optional.empty();
        }
        return Optional.of(functionsArray.getDynamic(index));
    }
    public ValueStorage getValueStorage(int index) {
        return variablesArray.get(index);
    }
    public Object getValue(ELispSymbol symbol) {
        Optional<ValueStorage> storageLazy = getStorageLazy(symbol);
        if (storageLazy.isEmpty()) {
            throw ELispSignals.voidVariable(symbol);
        }
        Object rawValue = storageLazy.get().getAnyValue();
        if (rawValue == ValueStorage.UNBOUND) {
            throw ELispSignals.voidVariable(symbol);
        }
        return rawValue;
    }
    public void setValue(ELispSymbol symbol, Object value) {
        ValueStorage storage = getStorage(symbol);
        if (storage.isConstant()) {
            throw ELispSignals.settingConstant(symbol);
        }
        storage.setValue(value, symbol, this);
    }

    public void forwardTo(ELispSymbol symbol, ValueStorage.AbstractForwarded<?> value) {
        int index = language.getGlobalVariableIndex(symbol);
        ValueStorage storage = new ValueStorage(value);
        storage.setSpecial(this, true);
        variablesArray.set(index, storage);
    }

    public Assumption getSpecialVariablesUnchangedAssumption() {
        return specialVariablesUnchanged.getAssumption();
    }

    public void invalidateSpecialVariables() {
        specialVariablesUnchanged.invalidate();
    }

    public void autoCleanUp(Object symbols) {
        // TODO
    }
    //#endregion Symbol lookup

    public static ELispExpressionNode valueToExpression(Object[] expressions, boolean lexicalBinding) {
        return ELispInterpretedNode.create(expressions, lexicalBinding);
    }

    public static ELispContext get(@Nullable Node node) {
        return CONTEXT_REFERENCE.get(node);
    }

    public record Options(
            int globalVariableMaxInvalidations
    ) {
    }
}
