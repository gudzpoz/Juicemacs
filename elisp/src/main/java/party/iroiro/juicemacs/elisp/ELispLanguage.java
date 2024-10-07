package party.iroiro.juicemacs.elisp;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;

import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
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
