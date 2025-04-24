package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.options.OptionValues;
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

import java.io.PrintStream;
import java.lang.ref.Cleaner;
import java.util.HashMap;
import java.util.Optional;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asBuffer;

/**
 * ELisp runtime context
 */
public final class ELispContext implements ELispParser.InternContext {
    private static final TruffleLanguage.ContextReference<ELispContext> CONTEXT_REFERENCE =
            TruffleLanguage.ContextReference.create(ELispLanguage.class);

    private final ELispLanguage language;
    private final HashMap<String, String> env;
    private final TruffleLanguage.Env truffleEnv;
    private final PrintStream out;
    private final Options options;

    private final ELispGlobals globals;
    private final SharedIndicesMap.ContextArray<ValueStorage> variablesArray;
    private final SharedIndicesMap.ContextArray<FunctionStorage> functionsArray;
    private final CyclicAssumption specialVariablesUnchanged;
    private final Cleaner cleaner = Cleaner.create();

    public ELispContext(ELispLanguage language, TruffleLanguage.Env env) {
        this.language = language;
        this.env = new HashMap<>(env.getEnvironment());
        this.options = Options.load(env);
        this.globals = new ELispGlobals(this);
        this.truffleEnv = env;
        this.out = new PrintStream(env.out());
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
        specialVariablesUnchanged = new CyclicAssumption("special variables unchanged");
    }

    public ELispLanguage language() {
        return language;
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

    public PrintStream out() {
        return out;
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

    public HashMap<String, String> env() {
        return env;
    }

    public ELispLanguage.Env truffleEnv() {
        return truffleEnv;
    }

    @Nullable
    public String getEnv(String key) {
        return env.get(key);
    }

    public ELispBuffer currentBuffer() {
        return asBuffer(language.currentBuffer().getValue());
    }

    public void initGlobal(ELispLanguage language) {
        globals.init(language, options.postInit);
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
            int globalVariableMaxInvalidations,
            boolean postInit,
            boolean hardExit,
            boolean debug
    ) {
        public static Options load(TruffleLanguage.Env env) {
            OptionValues options = env.getOptions();
            int invalidations = options.get(ELispLanguage.GLOBAL_MAX_INVALIDATIONS);
            boolean postInit = !options.get(ELispLanguage.BARE);
            boolean hardExit = options.get(ELispLanguage.HARD_EXIT);
            boolean debug = options.get(ELispLanguage.TRUFFLE_DEBUG);

            return new Options(invalidations, postInit, hardExit, debug);
        }
    }
}
