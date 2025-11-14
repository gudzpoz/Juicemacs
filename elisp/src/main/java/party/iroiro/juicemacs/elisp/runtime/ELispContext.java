package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fory.Fory;
import org.apache.fory.memory.MemoryBuffer;
import org.apache.fory.serializer.Serializer;
import org.jspecify.annotations.Nullable;
import org.graalvm.options.OptionValues;
import org.graalvm.polyglot.SandboxPolicy;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.forms.BuiltInFileIO.FExpandFileName;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.pdump.DumpUtils;
import party.iroiro.juicemacs.elisp.runtime.pdump.ELispPortableDumper;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage.Forwarded;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

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

    @CompilerDirectives.CompilationFinal
    private ELispGlobals globals;
    private final SharedIndicesMap.ContextArray<ValueStorage> variablesArray;
    private final SharedIndicesMap.ContextArray<FunctionStorage> functionsArray;
    private final CyclicAssumption specialVariablesUnchanged;

    public ELispContext(ELispLanguage language, TruffleLanguage.Env env) {
        this.language = language;
        this.env = env.getSandboxPolicy() == SandboxPolicy.TRUSTED
                ? new HashMap<>(env.getEnvironment())
                : new HashMap<>();
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
    public ELispSymbol intern(ELispString name) {
        return obarray().intern(name);
    }
    @Override
    public String applyShorthands(String symbol) {
        // TODO: Implementation
        return symbol;
    }
    //#endregion InternContext

    public TruffleLanguage.Env truffleEnv() {
        return truffleEnv;
    }
    public TruffleFile getFile(Object file) {
        return truffleEnv.getPublicTruffleFile(TruffleUtils.toString(file));
    }
    public TruffleFile getFileExpanded(ELispString file) {
        Path path = FExpandFileName.expandFileNamePath(file, false);
        return getFile(path);
    }


    @TruffleBoundary
    @Nullable
    public String getEnv(String key) {
        return env.get(key);
    }

    public ELispBuffer currentBuffer() {
        return asBuffer(language.currentBuffer().getValue());
    }

    public <T> T withCurrentBuffer(ELispBuffer buffer, Supplier<T> runnable) {
        Forwarded slot = language.currentBuffer();
        Object old = slot.getValue();
        try {
            slot.setValue(buffer);
            return runnable.get();
        } finally {
            slot.setValue(old);
        }
    }

    public void initGlobal(ELispLanguage language) {
        if (options.dumpFile == null) {
            globals.init(language);
            if (!options.pdump.isEmpty()) {
                setValue(ELispGlobals.DUMP_MODE, new ELispString(options.pdump));
            }
            return;
        }
        TruffleFile file = truffleEnv.getPublicTruffleFile(options.dumpFile);
        try {
            InputStream input = file.newInputStream(StandardOpenOption.READ);
            try {
                ELispPortableDumper.deserializeIntoContext(input, this);
                patchContext();
            } finally {
                input.close();
            }
        } catch (IOException e) {
            throw ELispSignals.reportFileError(e, new ELispString(options.dumpFile));
        }
    }

    public void patchContext() {
        globals.patchGlobals();
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
    //#endregion Symbol lookup

    public static ELispContext get(@Nullable Node node) {
        return CONTEXT_REFERENCE.get(node);
    }

    public record Options(
            int globalVariableMaxInvalidations,
            boolean hardExit,
            String pdump,
            @Nullable String dumpFile,
            boolean debug,
            boolean catchUnsupported
    ) {
        public static Options load(TruffleLanguage.Env env) {
            OptionValues options = env.getOptions();
            int invalidations = Math.toIntExact(options.get(ELispLanguage.GLOBAL_MAX_INVALIDATIONS));
            boolean hardExit = options.get(ELispLanguage.HARD_EXIT);
            String pdump = options.get(ELispLanguage.PORTABLE_DUMP);
            String dumpFile = options.get(ELispLanguage.DUMP_FILE);
            dumpFile = dumpFile.isEmpty() ? null : dumpFile;
            boolean debug = options.get(ELispLanguage.TRUFFLE_DEBUG);
            boolean catchUnsupported = options.get(ELispLanguage.CONVERT_UNSUPPORTED_EXCEPTION);

            return new Options(invalidations, hardExit, pdump, dumpFile, debug, catchUnsupported);
        }
    }

    public static final class ContextSerializer extends Serializer<ELispContext> {
        @Nullable
        private static ELispContext context = null;
        public static void setCurrentContext(@Nullable ELispContext context) {
            ContextSerializer.context = context;
        }

        public ContextSerializer(Fory fory) {
            super(fory, ELispContext.class);
        }

        @Override
        public void write(MemoryBuffer buffer, ELispContext context) {
            DumpUtils.writeAnchor(fory, buffer, Boolean.FALSE);
            DumpUtils.writeAnchor(fory, buffer, Boolean.TRUE);

            for (ELispSymbol[] symbols : ELispGlobals.getAllSymbols()) {
                DumpUtils.writeAnchors(fory, buffer, symbols);
            }

            ELispGlobals globals = context.globals;
            ELispSubroutine[] subroutines = Objects.requireNonNull(globals.takeSubroutines());
            buffer.writeInt32(subroutines.length);
            for (ELispSubroutine subroutine : subroutines) {
                fory.writeJavaString(buffer, subroutine.info().name());
            }
            DumpUtils.writeAnchors(fory, buffer, subroutines);

            DumpUtils.writeAnchor(fory, buffer, ELispLexical.DYNAMIC);
            DumpUtils.writeAnchor(fory, buffer, ValueStorage.UNBOUND);

            fory.writeRef(buffer, globals);
            DumpUtils.writeContextArray(fory, buffer, context.language.globalVariablesMap, context.variablesArray);
            DumpUtils.writeContextArray(fory, buffer, context.language.globalFunctionsMap, context.functionsArray);
        }

        @Override
        public ELispContext read(MemoryBuffer buffer) {
            Objects.requireNonNull(context);
            fory.getRefResolver().reference(context);
            DumpUtils.readAnchor(fory, buffer, Boolean.FALSE);
            DumpUtils.readAnchor(fory, buffer, Boolean.TRUE);

            for (ELispSymbol[] symbols : ELispGlobals.getAllSymbols()) {
                DumpUtils.readAnchors(fory, buffer, symbols);
            }

            int length = buffer.readInt32();
            ELispSubroutine[] subroutines = new ELispSubroutine[length];
            HashMap<String, ELispSubroutine> subroutineMap = new HashMap<>(length);
            ELispGlobals tempGlobals = context.globals;
            tempGlobals.initSubroutines(context.language);
            for (ELispSubroutine subroutine : Objects.requireNonNull(tempGlobals.takeSubroutines())) {
                subroutineMap.put(subroutine.info().name(), subroutine);
            }
            for (int i = 0; i < length; i++) {
                String name = fory.readJavaString(buffer);
                subroutines[i] = Objects.requireNonNull(subroutineMap.get(name));
            }
            DumpUtils.readAnchors(fory, buffer, subroutines);

            DumpUtils.readAnchor(fory, buffer, ELispLexical.DYNAMIC);
            DumpUtils.readAnchor(fory, buffer, ValueStorage.UNBOUND);

            context.globals = (ELispGlobals) fory.readRef(buffer);
            DumpUtils.readContextArray(fory, buffer, context.language.globalVariablesMap, context.variablesArray);
            DumpUtils.readContextArray(fory, buffer, context.language.globalFunctionsMap, context.functionsArray);
            return context;
        }
    }
}
