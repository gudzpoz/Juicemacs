package party.iroiro.juicemacs.elisp.runtime;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispObarray;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;

import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/// Custom globals
///
/// The majority of globals are automatically generated from Emacs source,
/// placed in [ELispGlobals]. However, that file might turn out too large to
/// be editable in an IDE. So for non-lisp global states, we choose to store
/// them in this class.
public sealed abstract class ELispGlobalsBase permits ELispGlobals {
    final ELispObarray globalObarray;
    protected final ELispContext ctx;
    protected final ELispBuffer bufferDefaults = new ELispBuffer(Collections.nCopies(77, false).toArray());

    protected ELispGlobalsBase(ELispContext context) {
        this.ctx = context;
        this.globalObarray = new ELispObarray(new ConcurrentHashMap<>(4096));
    }

    public final ELispBuffer getBufferDefaults() {
        return Objects.requireNonNull(bufferDefaults);
    }

    public final ELispContext getContext() {
        return ctx;
    }

    public void init(ELispLanguage language, boolean postInit) {
        initBuiltIns(language, new BuiltInAlloc());
        initBuiltIns(language, new BuiltInBuffer());
        initBuiltIns(language, new BuiltInCallInt());
        initBuiltIns(language, new BuiltInCallProc());
        initBuiltIns(language, new BuiltInCaseFiddle());
        initBuiltIns(language, new BuiltInCaseTab());
        initBuiltIns(language, new BuiltInCategory());
        initBuiltIns(language, new BuiltInCcl());
        initBuiltIns(language, new BuiltInCharacter());
        initBuiltIns(language, new BuiltInCharSet());
        initBuiltIns(language, new BuiltInCharTab());
        initBuiltIns(language, new BuiltInCmds());
        initBuiltIns(language, new BuiltInCoding());
        initBuiltIns(language, new BuiltInComp());
        initBuiltIns(language, new BuiltInComposite());
        initBuiltIns(language, new BuiltInData());
        initBuiltIns(language, new BuiltInDoc());
        initBuiltIns(language, new BuiltInEditFns());
        initBuiltIns(language, new BuiltInEmacs());
        initBuiltIns(language, new BuiltInEval());
        initBuiltIns(language, new BuiltInFileIO());
        initBuiltIns(language, new BuiltInFloatFns());
        initBuiltIns(language, new BuiltInFns());
        initBuiltIns(language, new BuiltInFrame());
        initBuiltIns(language, new BuiltInKeyboard());
        initBuiltIns(language, new BuiltInKeymap());
        initBuiltIns(language, new BuiltInLRead());
        initBuiltIns(language, new BuiltInMacros());
        initBuiltIns(language, new BuiltInMiniBuf());
        initBuiltIns(language, new BuiltInPrint());
        initBuiltIns(language, new BuiltInProcess());
        initBuiltIns(language, new BuiltInSearch());
        initBuiltIns(language, new BuiltInSyntax());
        initBuiltIns(language, new BuiltInTextProp());
        initBuiltIns(language, new BuiltInTimeFns());
        initBuiltIns(language, new BuiltInWindow());
        initBuiltIns(language, new BuiltInXDisp());
        initBuiltIns(language, new BuiltInXFaces());
    }

    public void registerFunction(ELispSymbol symbol, ELispValue function) {
        ctx.getFunctionStorage(symbol).set(function, symbol);
    }

    private void initBuiltIns(@Nullable ELispLanguage language, ELispBuiltIns builtIns) {
        //noinspection DataFlowIssue
        builtIns.initialize(language, this);
    }

    public ELispSymbol intern(String name) {
        return globalObarray.intern(name);
    }
}
