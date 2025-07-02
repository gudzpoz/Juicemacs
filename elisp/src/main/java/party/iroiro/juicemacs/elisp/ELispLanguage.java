package party.iroiro.juicemacs.elisp;

import com.oracle.truffle.api.*;

import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExecutableNode;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionStability;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;

@TruffleLanguage.Registration(
    id = ELispLanguage.ID,
    name = "ELisp",
    implementationName = "TruffleELisp",
    defaultMimeType = ELispLanguage.MIME_TYPE,
    characterMimeTypes = ELispLanguage.MIME_TYPE,
    fileTypeDetectors = ELispFileDetector.class,
    contextPolicy = TruffleLanguage.ContextPolicy.EXCLUSIVE,
    website = "https://www.gnu.org/software/emacs/"
)
@ProvidedTags({
        StandardTags.StatementTag.class,
        StandardTags.ExpressionTag.class,
        StandardTags.CallTag.class,
        StandardTags.RootTag.class,
})
public final class ELispLanguage extends TruffleLanguage<ELispContext> {
    public static final String ID = "elisp";
    public static final String MIME_TYPE = "text/x-elisp";

    @Option(
            name = "bare", help = "Inhibit initializing globals.",
            category = OptionCategory.INTERNAL, stability = OptionStability.STABLE
    )
    public static final OptionKey<Boolean> BARE = new OptionKey<>(false);

    @Option(
            name = "hardExit",
            help = "True to use Truffle hard exits for kill-emacs, false for an exception-based soft exit.",
            category = OptionCategory.EXPERT,
            stability = OptionStability.STABLE
    )
    public static final OptionKey<Boolean> HARD_EXIT = new OptionKey<>(false);

    @Option(
            name = "globalVariableMaxInvalidations",
            help = "Number of assignments to a global variable to disable auto-constant folding.",
            category = OptionCategory.EXPERT,
            stability = OptionStability.STABLE
    )
    public static final OptionKey<Long> GLOBAL_MAX_INVALIDATIONS = new OptionKey<>(3L);

    @Option(
            name = "portableDump",
            help = "Dump Emacs.",
            category = OptionCategory.EXPERT,
            stability = OptionStability.EXPERIMENTAL
    )
    public static final OptionKey<String> PORTABLE_DUMP = new OptionKey<>("");

    @Option(
            name = "dumpFile",
            help = "The dump file to restore from.",
            category = OptionCategory.EXPERT,
            stability = OptionStability.EXPERIMENTAL
    )
    public static final OptionKey<String> DUMP_FILE = new OptionKey<>("");

    @Option(
            name = "truffleDebug",
            help = "Maintain extra attributes and metadata for Truffle debugging.",
            category = OptionCategory.EXPERT,
            stability = OptionStability.STABLE
    )
    public static final OptionKey<Boolean> TRUFFLE_DEBUG = new OptionKey<>(false);

    private static final LanguageReference<ELispLanguage> REFERENCE = LanguageReference.create(ELispLanguage.class);

    public final SharedIndicesMap globalVariablesMap = new SharedIndicesMap();
    public final SharedIndicesMap globalFunctionsMap = new SharedIndicesMap();
    private final ContextThreadLocal<ValueStorage.Forwarded> currentBuffer =
            locals.createContextThreadLocal((_, _) -> new ValueStorage.Forwarded());
    private final ContextThreadLocal<ValueStorage.Forwarded> currentFrame =
            locals.createContextThreadLocal((_, _) -> new ValueStorage.Forwarded());

    @Override
    protected OptionDescriptors getOptionDescriptors() {
        return new ELispLanguageOptionDescriptors();
    }

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        ELispRootNode root = ELispParser.parse(this, ELispContext.get(null), request.getSource());
        return root.getCallTarget();
    }

    @Override
    protected ExecutableNode parse(InlineParsingRequest request) throws Exception {
        ELispLexical.@Nullable Scope scope = ELispLexical.getScope(request.getLocation());
        return ELispParser.parseDebugEval(this, ELispContext.get(null), request.getSource(), scope);
    }

    @Override
    protected ELispContext createContext(Env env) {
        return new ELispContext(this, env);
    }

    @Override
    protected void initializeContext(ELispContext context) {
        context.initGlobal(this);
    }

    @Override
    protected Object getScope(ELispContext context) {
        return new TopLevelScope(context);
    }

    public int tryGetGlobalVariableIndex(ELispSymbol symbol) {
        return globalVariablesMap.tryLookup(symbol);
    }

    public int getGlobalVariableIndex(ELispSymbol symbol) {
        return globalVariablesMap.lookup(symbol);
    }

    public int tryGetGlobalFunctionIndex(ELispSymbol symbol) {
        return globalFunctionsMap.tryLookup(symbol);
    }

    public int getGlobalFunctionIndex(ELispSymbol name) {
        return globalFunctionsMap.lookup(name);
    }

    public ValueStorage.Forwarded currentBuffer() {
        return currentBuffer.get();
    }

    public ValueStorage.Forwarded currentFrame() {
        return currentFrame.get();
    }

    public static ELispLanguage get(@Nullable Node node) {
        return REFERENCE.get(node);
    }

    @ExportLibrary(InteropLibrary.class)
    final class TopLevelScope implements TruffleObject {
        private final ELispContext context;
        private final HashMap<String, ELispSymbol> symbolCache;
        private final HashMap<String, ValueStorage> cache;

        TopLevelScope(ELispContext context) {
            this.context = context;
            this.symbolCache = new HashMap<>();
            this.cache = new HashMap<>();
        }

        @ExportMessage
        boolean isScope() {
            return true;
        }
        @ExportMessage
        String toDisplayString(boolean allowSideEffects) {
            return toString();
        }

        @ExportMessage
        boolean hasLanguage() {
            return true;
        }
        @ExportMessage
        Class<ELispLanguage> getLanguage() {
            return ELispLanguage.class;
        }

        @ExportMessage
        boolean hasMembers() {
            return true;
        }
        @CompilerDirectives.TruffleBoundary
        @ExportMessage
        public Object getMembers(boolean includeInternal) {
            ArrayList<ELispString> members = new ArrayList<>();
            globalVariablesMap.keySet().forEach((sym) -> members.add(new ELispString(sym.name())));
            globalFunctionsMap.keySet().forEach((sym) -> members.add(new ELispString(sym.name())));
            return new ELispVector(members.toArray());
        }
        @ExportMessage
        boolean isMemberReadable(String member) {
            @Nullable ValueStorage storage = cache.get(member);
            if (storage == null) {
                @Nullable ELispSymbol symbol = context.obarray().internSoft(MuleString.fromString(member));
                if (symbol == null) {
                    return false;
                }
                symbolCache.put(member, symbol);
                Optional<ValueStorage> valueStorage = context.getStorageLazy(symbol);
                if (valueStorage.isPresent()) {
                    storage = valueStorage.get();
                    cache.put(member, storage);
                } else {
                    return false;
                }
            }
            return storage.isBound();
        }
        @ExportMessage
        Object readMember(String member) throws UnknownIdentifierException {
            if (!isMemberReadable(member)) {
                throw UnknownIdentifierException.create(member);
            }
            try {
                Object v = cache.get(member).getValue(symbolCache.get(member));
                if (InteropLibrary.isValidValue(v)) {
                    return v;
                }
            } catch (ELispSignals.ELispSignalException ignored) {
            }
            return ELispGlobals.NIL;
        }
        @ExportMessage
        boolean isMemberInsertable(String member) {
            return !isMemberReadable(member);
        }
        @ExportMessage
        boolean isMemberModifiable(String member) {
            return isMemberReadable(member) && !cache.get(member).isConstant();
        }
        @ExportMessage
        void writeMember(String member, Object value) throws UnknownIdentifierException, UnsupportedTypeException {
            ELispSymbol symbol = symbolCache.get(member);
            ValueStorage storage = cache.get(member);
            if (storage == null) {
                symbol = context.obarray().intern(member);
                storage = context.getStorage(symbol);
                symbolCache.put(member, symbol);
                cache.put(member, storage);
            }
            if (storage.isConstant()) {
                throw UnknownIdentifierException.create(member);
            }
            if (InteropLibrary.isValidValue(value)) {
                storage.setValue(value, symbol, context);
            } else {
                throw UnsupportedTypeException.create(new Object[]{value});
            }
        }
    }
}
