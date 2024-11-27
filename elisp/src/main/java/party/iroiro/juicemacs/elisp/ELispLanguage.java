package party.iroiro.juicemacs.elisp;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionDispatchNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

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

    private static final LanguageReference<ELispLanguage> REFERENCE = LanguageReference.create(ELispLanguage.class);

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

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        ELispRootNode root = ELispParser.parse(this, request.getSource());
        return root.getCallTarget();
    }

    @Override
    protected ELispContext createContext(Env env) {
        return ELispContext.getInstance();
    }

    @Override
    protected void initializeContext(ELispContext context) {
        context.initGlobal(this);
    }
}
