package party.iroiro.juicemacs.elisp.runtime;

import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCodings;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

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

    public final BuiltInBuffer builtInBuffer = new BuiltInBuffer();
    public final BuiltInCaseTab builtInCaseTab = new BuiltInCaseTab();
    public final BuiltInCcl builtInCcl = new BuiltInCcl();
    public final BuiltInCharSet builtInCharSet = new BuiltInCharSet();
    public final BuiltInCoding builtInCoding = new BuiltInCoding();
    public final BuiltInFns builtInFns = new BuiltInFns();
    public final BuiltInKeymap builtInKeymap = new BuiltInKeymap();
    public final BuiltInSearch builtInSearch = new BuiltInSearch();
    public final BuiltInSyntax builtInSyntax = new BuiltInSyntax();

    private final ELispBuiltIns[] builtIns = {
            new BuiltInAlloc(),
            builtInBuffer,
            new BuiltInBytecode(),
            new BuiltInCallInt(),
            new BuiltInCallProc(),
            new BuiltInCaseFiddle(),
            builtInCaseTab,
            new BuiltInCategory(),
            builtInCcl,
            new BuiltInCharacter(),
            builtInCharSet,
            new BuiltInCharTab(),
            new BuiltInCmds(),
            builtInCoding,
            new BuiltInComp(),
            new BuiltInComposite(),
            new BuiltInData(),
            new BuiltInDired(),
            new BuiltInDispNew(),
            new BuiltInDoc(),
            new BuiltInEditFns(),
            new BuiltInEmacs(),
            new BuiltInEval(),
            new BuiltInFileIO(),
            new BuiltInFileLock(),
            new BuiltInFloatFns(),
            builtInFns,
            new BuiltInFrame(),
            new BuiltInIndent(),
            new BuiltInKeyboard(),
            builtInKeymap,
            new BuiltInLRead(),
            new BuiltInMacros(),
            new BuiltInMarker(),
            new BuiltInMiniBuf(),
            new BuiltInPdumper(),
            new BuiltInPrint(),
            new BuiltInProcess(),
            builtInSearch,
            builtInSyntax,
            new BuiltInTerm(),
            new BuiltInTerminal(),
            new BuiltInTextProp(),
            new BuiltInTimeFns(),
            new BuiltInTreesit(),
            new BuiltInWindow(),
            new BuiltInXDisp(),
            new BuiltInXFaces(),
    };

    private ELispSubroutine @Nullable[] subroutines = null;

    protected ELispGlobalsBase(ELispContext context) {
        this.ctx = context;
        this.globalObarray = new ELispObarray(new HashMap<>(4096));
    }

    public final ELispBuffer getBufferDefaults() {
        return Objects.requireNonNull(bufferDefaults);
    }

    public final ELispCodings getCodings() {
        return builtInCoding.codings;
    }

    public final ELispContext getContext() {
        return ctx;
    }

    public ELispBuiltIns[] getBuiltIns() {
        return builtIns;
    }

    public ELispSubroutine @Nullable [] takeSubroutines() {
        ELispSubroutine[] subroutines = this.subroutines;
        this.subroutines = null;
        return subroutines;
    }

    public void initSubroutines(ELispLanguage language) {
        AtomicInteger remaining = new AtomicInteger(builtIns.length);
        ELispBuiltIns.@Nullable InitializationResult[] results = new ELispBuiltIns.InitializationResult[builtIns.length];
        for (int i = 0; i < builtIns.length; i++) {
            initBuiltIns(language, remaining, builtIns[i], i, results);
        }
        synchronized (this) {
            while (remaining.get() != 0) {
                try {
                    this.wait();
                } catch (InterruptedException ignored) {
                }
            }
        }

        ArrayList<ELispSubroutine> subroutines = new ArrayList<>();
        for (int i = 0; i < results.length; i++) {
            ELispBuiltIns.InitializationResult result = results[i];
            if (result == null) {
                throw ELispSignals.fatal(builtIns[i].toString());
            }
            for (ELispBuiltIns.SemiInitializedBuiltIn subroutine : result.subroutines()) {
                ELispSymbol symbol = globalObarray.intern(subroutine.symbol());
                registerFunction(symbol, subroutine.subroutine());
                subroutine.node().setLispFunction(symbol);
                subroutines.add(subroutine.subroutine());
            }
        }
        this.subroutines = subroutines.toArray(new ELispSubroutine[0]);
    }

    public void registerFunction(ELispSymbol symbol, ELispValue function) {
        ctx.getFunctionStorage(symbol).set(function, symbol);
    }

    private void initBuiltIns(ELispLanguage language, AtomicInteger lock, ELispBuiltIns builtIns,
                              int i, ELispBuiltIns.@Nullable InitializationResult[] results) {
        ELispGlobalsBase globals = this;
        Thread.ofVirtual().start(() -> {
            try {
                results[i] = builtIns.initialize(language);
            } finally {
                synchronized (globals) {
                    if (lock.decrementAndGet() == 0) {
                        globals.notify();
                    }
                }
            }
        });
    }

    public abstract void patchGlobals();
}
