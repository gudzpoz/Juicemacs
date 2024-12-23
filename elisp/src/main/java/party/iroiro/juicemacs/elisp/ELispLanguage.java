package party.iroiro.juicemacs.elisp;

import com.oracle.truffle.api.*;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionStability;
import party.iroiro.juicemacs.elisp.collections.SharedIndicesMap;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionDispatchNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.ArrayList;

@TruffleLanguage.Registration(
    id = ELispLanguage.ID,
    name = "ELisp",
    implementationName = "TruffleELisp",
    defaultMimeType = ELispLanguage.MIME_TYPE,
    characterMimeTypes = ELispLanguage.MIME_TYPE,
    fileTypeDetectors = ELispFileDetector.class,
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
    website = "https://www.gnu.org/software/emacs/"
)
public final class ELispLanguage extends TruffleLanguage<ELispContext> {
    public static final String ID = "elisp";
    public static final String MIME_TYPE = "text/x-elisp";

    @Option(
            name = "bare", help = "Inhibit initializing globals.",
            category = OptionCategory.INTERNAL, stability = OptionStability.STABLE
    )
    public static final OptionKey<Boolean> BARE = new OptionKey<>(false);

    private static final LanguageReference<ELispLanguage> REFERENCE = LanguageReference.create(ELispLanguage.class);

    public final SharedIndicesMap globalVariablesMap = new SharedIndicesMap();
    public final SharedIndicesMap globalFunctionsMap = new SharedIndicesMap();

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

    public static ELispLanguage get(Node node) {
        return REFERENCE.get(node);
    }

    /// Finds a [Node] instance by walking the stack
    ///
    /// If you have a [Node] instance at hand, please use [#get(Node)] instead.
    /// This method is slow and should only be used if you have no other choice,
    /// i.e., Emacs calls `eval_sub` out of nowhere in a relatively rare situation.
    public static ELispLanguage getLanguageSlow() {
        @Nullable
        Node node = Truffle.getRuntime().iterateFrames((frame) -> {
            CallTarget callTarget = frame.getCallTarget();
            if (callTarget instanceof RootCallTarget rootCallTarget) {
                RootNode rootNode = rootCallTarget.getRootNode();
                if (rootNode instanceof ELispRootNode || rootNode instanceof FunctionRootNode) {
                    return rootNode;
                }
            }
            Node callNode = frame.getCallNode();
            if (callNode instanceof ELispExpressionNode || callNode instanceof FunctionDispatchNode) {
                return callNode;
            }
            return null;
        });
        return get(node);
    }

    @ExportLibrary(InteropLibrary.class)
    final class TopLevelScope implements TruffleObject {
        private final ELispContext context;

        TopLevelScope(ELispContext context) {
            this.context = context;
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
            return context.obarray().symbols().containsKey(MuleString.fromString(member));
        }
        @ExportMessage
        Object readMember(String member) throws UnknownIdentifierException {
            MuleString name = MuleString.fromString(member);
            if (context.obarray().symbols().containsKey(name)) {
                try {
                    return context.getValue(context.intern(name));
                } catch (ELispSignals.ELispSignalException e) {
                    throw UnknownIdentifierException.create(member, e);
                }
            } else {
                throw UnknownIdentifierException.create(member);
            }
        }
    }
}
