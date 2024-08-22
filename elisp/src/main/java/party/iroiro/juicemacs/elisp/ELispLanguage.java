package party.iroiro.juicemacs.elisp;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;

import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
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

    @Nullable
    private volatile ELispContext languageContext;

    public static ELispLanguage get(Node node) {
        return REFERENCE.get(node);
    }

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        ELispExpressionNode node = ELispParser.parse(request.getSource(), languageContext);
        ELispRootNode root = new ELispRootNode(this, node);
        return root.getCallTarget();
    }

    @Override
    protected ELispContext createContext(Env env) {
        ELispContext context = languageContext;
        if (context != null) {
            return context;
        }
        context = new ELispContext();
        languageContext = context;
        return context;
    }

    @Override
    protected void initializeContext(ELispContext context) {
        context.initGlobal(this);
    }
}
