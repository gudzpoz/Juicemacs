package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInCharTab.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInKeymap.*;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.Collections;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCharSet.defineCharsetInternal;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

@SuppressWarnings({
        "PMD.UnnecessaryCast",
        "PMD.UseExplicitTypes",
        "PMD.UseUnderscoresInNumericLiterals",
        "RedundantCast",
        "UnnecessaryUnicodeEscape"
})
public class ELispGlobals {
    public static void initGlobalVariables() {
        allocVars();
        bufferVars();
        callintVars();
        callprocVars();
        casefiddleVars();
        casetabVars();
        categoryVars();
        cclVars();
        characterVars();
        charsetVars();
        chartabVars();
        cmdsVars();
        codingVars();
        compVars();
        dataVars();
        docVars();
        editfnsVars();
        emacsVars();
        evalVars();
        fileioVars();
        floatfnsVars();
        fnsVars();
        frameVars();
        keyboardVars();
        keymapVars();
        lreadVars();
        macrosVars();
        minibufVars();
        printVars();
        processVars();
        searchVars();
        syntaxVars();
        textpropVars();
        timefnsVars();
        windowVars();
        xdispVars();
        xfacesVars();
    }

    public static void postInitVariables() {
        allocPostInitVars();
        bufferPostInitVars();
        callintPostInitVars();
        callprocPostInitVars();
        casefiddlePostInitVars();
        casetabPostInitVars();
        categoryPostInitVars();
        cclPostInitVars();
        characterPostInitVars();
        charsetPostInitVars();
        chartabPostInitVars();
        cmdsPostInitVars();
        codingPostInitVars();
        compPostInitVars();
        dataPostInitVars();
        docPostInitVars();
        editfnsPostInitVars();
        emacsPostInitVars();
        evalPostInitVars();
        fnsPostInitVars();
        fileioPostInitVars();
        floatfnsPostInitVars();
        framePostInitVars();
        keyboardPostInitVars();
        keymapPostInitVars();
        lreadPostInitVars();
        macrosPostInitVars();
        minibufPostInitVars();
        printPostInitVars();
        processPostInitVars();
        searchPostInitVars();
        syntaxPostInitVars();
        textpropPostInitVars();
        timefnsPostInitVars();
        windowPostInitVars();
        xdispPostInitVars();
        xfacesPostInitVars();
    }

    //#region alloc.c
    public static final ELispSymbol.Value.ForwardedLong gcConsThreshold = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded gcConsPercentage = new ELispSymbol.Value.Forwarded((Object) 0.1);
    public static final ELispSymbol.Value.ForwardedLong pureBytesUsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong consCellsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong floatsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong vectorCellsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong symbolsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong stringCharsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong intervalsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong stringsConsed = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded purifyFlag = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.ForwardedBool garbageCollectionMessages = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded postGcHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded memorySignalData = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded memoryFull = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded gcElapsed = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.ForwardedLong gcsDone = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong integerWidth = new ELispSymbol.Value.ForwardedLong((long) 0);

    private static void allocVars() {
        GC_CONS_THRESHOLD.initForwardTo(gcConsThreshold);
        GC_CONS_PERCENTAGE.initForwardTo(gcConsPercentage);
        PURE_BYTES_USED.initForwardTo(pureBytesUsed);
        CONS_CELLS_CONSED.initForwardTo(consCellsConsed);
        FLOATS_CONSED.initForwardTo(floatsConsed);
        VECTOR_CELLS_CONSED.initForwardTo(vectorCellsConsed);
        SYMBOLS_CONSED.initForwardTo(symbolsConsed);
        STRING_CHARS_CONSED.initForwardTo(stringCharsConsed);
        INTERVALS_CONSED.initForwardTo(intervalsConsed);
        STRINGS_CONSED.initForwardTo(stringsConsed);
        PURIFY_FLAG.initForwardTo(purifyFlag);
        GARBAGE_COLLECTION_MESSAGES.initForwardTo(garbageCollectionMessages);
        POST_GC_HOOK.initForwardTo(postGcHook);
        MEMORY_SIGNAL_DATA.initForwardTo(memorySignalData);
        MEMORY_FULL.initForwardTo(memoryFull);
        GC_ELAPSED.initForwardTo(gcElapsed);
        GCS_DONE.initForwardTo(gcsDone);
        INTEGER_WIDTH.initForwardTo(integerWidth);
    }
    private static void allocPostInitVars() {
        var memorySignalDataJInit = ELispCons.listOf(ERROR, new ELispString("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs"));
        memorySignalData.setValue(memorySignalDataJInit);
    }
    //#endregion alloc.c
    //#region chartab.c
    public static final ELispSymbol.Value.Forwarded charCodePropertyAlist = new ELispSymbol.Value.Forwarded(false);

    private static void chartabVars() {
        CHAR_CODE_PROPERTY_ALIST.initForwardTo(charCodePropertyAlist);
    }
    private static void chartabPostInitVars() {

    }
    //#endregion chartab.c
    //#region comp.c
    public static final ELispSymbol.Value.ForwardedBool nativeCompJitCompilation = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded compCtxt = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded compSubrList = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded compAbiHash = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded compNativeVersionDir = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded compDeferredPendingH = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded compElnToElH = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded nativeCompElnLoadPath = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded nativeCompEnableSubrTrampolines = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded compInstalledTrampolinesH = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded compNoNativeFileH = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool compFilePreloadedP = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded compLoadedCompUnitsH = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded compSubrAritiesH = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool compSanitizerActive = new ELispSymbol.Value.ForwardedBool(false);

    private static void compVars() {
        NATIVE_COMP_JIT_COMPILATION.initForwardTo(nativeCompJitCompilation);
        COMP_CTXT.initForwardTo(compCtxt);
        COMP_SUBR_LIST.initForwardTo(compSubrList);
        COMP_ABI_HASH.initForwardTo(compAbiHash);
        COMP_NATIVE_VERSION_DIR.initForwardTo(compNativeVersionDir);
        COMP_DEFERRED_PENDING_H.initForwardTo(compDeferredPendingH);
        COMP_ELN_TO_EL_H.initForwardTo(compElnToElH);
        NATIVE_COMP_ELN_LOAD_PATH.initForwardTo(nativeCompElnLoadPath);
        NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES.initForwardTo(nativeCompEnableSubrTrampolines);
        COMP_INSTALLED_TRAMPOLINES_H.initForwardTo(compInstalledTrampolinesH);
        COMP_NO_NATIVE_FILE_H.initForwardTo(compNoNativeFileH);
        COMP_FILE_PRELOADED_P.initForwardTo(compFilePreloadedP);
        COMP_LOADED_COMP_UNITS_H.initForwardTo(compLoadedCompUnitsH);
        COMP_SUBR_ARITIES_H.initForwardTo(compSubrAritiesH);
        COMP_SANITIZER_ACTIVE.initForwardTo(compSanitizerActive);
    }
    private static void compPostInitVars() {
        NATIVE_COMPILER_ERROR.putProperty(ERROR_CONDITIONS, ELispCons.listOf(NATIVE_COMPILER_ERROR, ERROR));
        NATIVE_COMPILER_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Native compiler error"));
        NATIVE_ICE.putProperty(ERROR_CONDITIONS, ELispCons.listOf(NATIVE_ICE, NATIVE_COMPILER_ERROR, ERROR));
        NATIVE_ICE.putProperty(ERROR_MESSAGE, new ELispString("Internal native compiler error"));
        NATIVE_LISP_LOAD_FAILED.putProperty(ERROR_CONDITIONS, ELispCons.listOf(NATIVE_LISP_LOAD_FAILED, ERROR));
        NATIVE_LISP_LOAD_FAILED.putProperty(ERROR_MESSAGE, new ELispString("Native elisp load failed"));
        NATIVE_LISP_WRONG_RELOC.putProperty(ERROR_CONDITIONS, ELispCons.listOf(NATIVE_LISP_WRONG_RELOC, NATIVE_LISP_LOAD_FAILED, ERROR));
        NATIVE_LISP_WRONG_RELOC.putProperty(ERROR_MESSAGE, new ELispString("Primitive redefined or wrong relocation"));
        WRONG_REGISTER_SUBR_CALL.putProperty(ERROR_CONDITIONS, ELispCons.listOf(WRONG_REGISTER_SUBR_CALL, NATIVE_LISP_LOAD_FAILED, ERROR));
        WRONG_REGISTER_SUBR_CALL.putProperty(ERROR_MESSAGE, new ELispString("comp--register-subr can only be called during native lisp load phase."));
        NATIVE_LISP_FILE_INCONSISTENT.putProperty(ERROR_CONDITIONS, ELispCons.listOf(NATIVE_LISP_FILE_INCONSISTENT, NATIVE_LISP_LOAD_FAILED, ERROR));
        NATIVE_LISP_FILE_INCONSISTENT.putProperty(ERROR_MESSAGE, new ELispString("eln file inconsistent with current runtime configuration, please recompile"));
        COMP_SANITIZER_ERROR.putProperty(ERROR_CONDITIONS, ELispCons.listOf(COMP_SANITIZER_ERROR, ERROR));
        COMP_SANITIZER_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Native code sanitizer runtime error"));
        var compDeferredPendingHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
        compDeferredPendingH.setValue(compDeferredPendingHJInit);
        var compElnToElHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
        compElnToElH.setValue(compElnToElHJInit);
        var nativeCompElnLoadPathJInit = new ELispCons(new ELispString("../native-lisp/"), NIL);
        nativeCompElnLoadPath.setValue(nativeCompElnLoadPathJInit);
        var compInstalledTrampolinesHJInit = FMakeHashTable.makeHashTable(new Object[]{});
        compInstalledTrampolinesH.setValue(compInstalledTrampolinesHJInit);
        var compNoNativeFileHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
        compNoNativeFileH.setValue(compNoNativeFileHJInit);
        var compLoadedCompUnitsHJInit = FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, VALUE, CTEST, EQUAL});
        compLoadedCompUnitsH.setValue(compLoadedCompUnitsHJInit);
        var compSubrAritiesHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
        compSubrAritiesH.setValue(compSubrAritiesHJInit);
        FProvide.provide(ELispContext.intern("native-compile"), NIL);
    }
    //#endregion comp.c
    //#region data.c
    public static final ELispSymbol.Value.Forwarded mostPositiveFixnum = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded mostNegativeFixnum = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool symbolsWithPosEnabled = new ELispSymbol.Value.ForwardedBool(false);

    private static void dataVars() {
        MOST_POSITIVE_FIXNUM.initForwardTo(mostPositiveFixnum);
        MOST_NEGATIVE_FIXNUM.initForwardTo(mostNegativeFixnum);
        SYMBOLS_WITH_POS_ENABLED.initForwardTo(symbolsWithPosEnabled);
    }
    private static void dataPostInitVars() {
        ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(ERROR, NIL));
        ERROR.putProperty(ERROR_MESSAGE, new ELispString("error"));
        QUIT.putProperty(ERROR_CONDITIONS, new ELispCons(QUIT, NIL));
        QUIT.putProperty(ERROR_MESSAGE, new ELispString("Quit"));
        MINIBUFFER_QUIT.putProperty(ERROR_CONDITIONS, new ELispCons(MINIBUFFER_QUIT, new ELispCons(QUIT, NIL)));
        MINIBUFFER_QUIT.putProperty(ERROR_MESSAGE, new ELispString("Quit"));
        USER_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(USER_ERROR, new ELispCons(ERROR, NIL)));
        USER_ERROR.putProperty(ERROR_MESSAGE, new ELispString(""));
        WRONG_LENGTH_ARGUMENT.putProperty(ERROR_CONDITIONS, new ELispCons(WRONG_LENGTH_ARGUMENT, new ELispCons(ERROR, NIL)));
        WRONG_LENGTH_ARGUMENT.putProperty(ERROR_MESSAGE, new ELispString("Wrong length argument"));
        WRONG_TYPE_ARGUMENT.putProperty(ERROR_CONDITIONS, new ELispCons(WRONG_TYPE_ARGUMENT, new ELispCons(ERROR, NIL)));
        WRONG_TYPE_ARGUMENT.putProperty(ERROR_MESSAGE, new ELispString("Wrong type argument"));
        TYPE_MISMATCH.putProperty(ERROR_CONDITIONS, new ELispCons(TYPE_MISMATCH, new ELispCons(ERROR, NIL)));
        TYPE_MISMATCH.putProperty(ERROR_MESSAGE, new ELispString("Types do not match"));
        ARGS_OUT_OF_RANGE.putProperty(ERROR_CONDITIONS, new ELispCons(ARGS_OUT_OF_RANGE, new ELispCons(ERROR, NIL)));
        ARGS_OUT_OF_RANGE.putProperty(ERROR_MESSAGE, new ELispString("Args out of range"));
        VOID_FUNCTION.putProperty(ERROR_CONDITIONS, new ELispCons(VOID_FUNCTION, new ELispCons(ERROR, NIL)));
        VOID_FUNCTION.putProperty(ERROR_MESSAGE, new ELispString("Symbol's function definition is void"));
        CYCLIC_FUNCTION_INDIRECTION.putProperty(ERROR_CONDITIONS, new ELispCons(CYCLIC_FUNCTION_INDIRECTION, new ELispCons(ERROR, NIL)));
        CYCLIC_FUNCTION_INDIRECTION.putProperty(ERROR_MESSAGE, new ELispString("Symbol's chain of function indirections contains a loop"));
        CYCLIC_VARIABLE_INDIRECTION.putProperty(ERROR_CONDITIONS, new ELispCons(CYCLIC_VARIABLE_INDIRECTION, new ELispCons(ERROR, NIL)));
        CYCLIC_VARIABLE_INDIRECTION.putProperty(ERROR_MESSAGE, new ELispString("Symbol's chain of variable indirections contains a loop"));
        CIRCULAR_LIST.putProperty(ERROR_CONDITIONS, new ELispCons(CIRCULAR_LIST, new ELispCons(ERROR, NIL)));
        CIRCULAR_LIST.putProperty(ERROR_MESSAGE, new ELispString("List contains a loop"));
        VOID_VARIABLE.putProperty(ERROR_CONDITIONS, new ELispCons(VOID_VARIABLE, new ELispCons(ERROR, NIL)));
        VOID_VARIABLE.putProperty(ERROR_MESSAGE, new ELispString("Symbol's value as variable is void"));
        SETTING_CONSTANT.putProperty(ERROR_CONDITIONS, new ELispCons(SETTING_CONSTANT, new ELispCons(ERROR, NIL)));
        SETTING_CONSTANT.putProperty(ERROR_MESSAGE, new ELispString("Attempt to set a constant symbol"));
        TRAPPING_CONSTANT.putProperty(ERROR_CONDITIONS, new ELispCons(TRAPPING_CONSTANT, new ELispCons(ERROR, NIL)));
        TRAPPING_CONSTANT.putProperty(ERROR_MESSAGE, new ELispString("Attempt to trap writes to a constant symbol"));
        INVALID_READ_SYNTAX.putProperty(ERROR_CONDITIONS, new ELispCons(INVALID_READ_SYNTAX, new ELispCons(ERROR, NIL)));
        INVALID_READ_SYNTAX.putProperty(ERROR_MESSAGE, new ELispString("Invalid read syntax"));
        INVALID_FUNCTION.putProperty(ERROR_CONDITIONS, new ELispCons(INVALID_FUNCTION, new ELispCons(ERROR, NIL)));
        INVALID_FUNCTION.putProperty(ERROR_MESSAGE, new ELispString("Invalid function"));
        WRONG_NUMBER_OF_ARGUMENTS.putProperty(ERROR_CONDITIONS, new ELispCons(WRONG_NUMBER_OF_ARGUMENTS, new ELispCons(ERROR, NIL)));
        WRONG_NUMBER_OF_ARGUMENTS.putProperty(ERROR_MESSAGE, new ELispString("Wrong number of arguments"));
        NO_CATCH.putProperty(ERROR_CONDITIONS, new ELispCons(NO_CATCH, new ELispCons(ERROR, NIL)));
        NO_CATCH.putProperty(ERROR_MESSAGE, new ELispString("No catch for tag"));
        END_OF_FILE.putProperty(ERROR_CONDITIONS, new ELispCons(END_OF_FILE, new ELispCons(ERROR, NIL)));
        END_OF_FILE.putProperty(ERROR_MESSAGE, new ELispString("End of file during parsing"));
        ARITH_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(ARITH_ERROR, new ELispCons(ERROR, NIL)));
        ARITH_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Arithmetic error"));
        BEGINNING_OF_BUFFER.putProperty(ERROR_CONDITIONS, new ELispCons(BEGINNING_OF_BUFFER, new ELispCons(ERROR, NIL)));
        BEGINNING_OF_BUFFER.putProperty(ERROR_MESSAGE, new ELispString("Beginning of buffer"));
        END_OF_BUFFER.putProperty(ERROR_CONDITIONS, new ELispCons(END_OF_BUFFER, new ELispCons(ERROR, NIL)));
        END_OF_BUFFER.putProperty(ERROR_MESSAGE, new ELispString("End of buffer"));
        BUFFER_READ_ONLY.putProperty(ERROR_CONDITIONS, new ELispCons(BUFFER_READ_ONLY, new ELispCons(ERROR, NIL)));
        BUFFER_READ_ONLY.putProperty(ERROR_MESSAGE, new ELispString("Buffer is read-only"));
        TEXT_READ_ONLY.putProperty(ERROR_CONDITIONS, new ELispCons(TEXT_READ_ONLY, new ELispCons(BUFFER_READ_ONLY, new ELispCons(ERROR, NIL))));
        TEXT_READ_ONLY.putProperty(ERROR_MESSAGE, new ELispString("Text is read-only"));
        INHIBITED_INTERACTION.putProperty(ERROR_CONDITIONS, new ELispCons(INHIBITED_INTERACTION, new ELispCons(ERROR, NIL)));
        INHIBITED_INTERACTION.putProperty(ERROR_MESSAGE, new ELispString("User interaction while inhibited"));
        DOMAIN_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(DOMAIN_ERROR, new ELispCons(ARITH_ERROR, new ELispCons(ERROR, NIL))));
        DOMAIN_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Arithmetic domain error"));
        RANGE_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(RANGE_ERROR, new ELispCons(ARITH_ERROR, new ELispCons(ERROR, NIL))));
        RANGE_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Arithmetic range error"));
        SINGULARITY_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(SINGULARITY_ERROR, new ELispCons(DOMAIN_ERROR, new ELispCons(ARITH_ERROR, new ELispCons(ERROR, NIL)))));
        SINGULARITY_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Arithmetic singularity error"));
        OVERFLOW_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(OVERFLOW_ERROR, new ELispCons(RANGE_ERROR, new ELispCons(ARITH_ERROR, new ELispCons(ERROR, NIL)))));
        OVERFLOW_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Arithmetic overflow error"));
        UNDERFLOW_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(UNDERFLOW_ERROR, new ELispCons(RANGE_ERROR, new ELispCons(ARITH_ERROR, new ELispCons(ERROR, NIL)))));
        UNDERFLOW_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Arithmetic underflow error"));
        RECURSION_ERROR.putProperty(ERROR_CONDITIONS, new ELispCons(RECURSION_ERROR, new ELispCons(ERROR, NIL)));
        RECURSION_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Excessive recursive calling error"));
        EXCESSIVE_LISP_NESTING.putProperty(ERROR_CONDITIONS, new ELispCons(EXCESSIVE_LISP_NESTING, new ELispCons(RECURSION_ERROR, new ELispCons(ERROR, NIL))));
        EXCESSIVE_LISP_NESTING.putProperty(ERROR_MESSAGE, new ELispString("Lisp nesting exceeds `max-lisp-eval-depth'"));
        EXCESSIVE_VARIABLE_BINDING.putProperty(ERROR_CONDITIONS, new ELispCons(EXCESSIVE_VARIABLE_BINDING, new ELispCons(RECURSION_ERROR, new ELispCons(ERROR, NIL))));
        EXCESSIVE_VARIABLE_BINDING.putProperty(ERROR_MESSAGE, new ELispString("Variable binding depth exceeds max-specpdl-size"));
        var mostPositiveFixnumJInit = (long) (Long.MAX_VALUE);
        mostPositiveFixnum.setValue(mostPositiveFixnumJInit);
        ELispContext.intern("most-positive-fixnum").setConstant(true);
        var mostNegativeFixnumJInit = (long) (Long.MIN_VALUE);
        mostNegativeFixnum.setValue(mostNegativeFixnumJInit);
        ELispContext.intern("most-negative-fixnum").setConstant(true);
    }
    //#endregion data.c
    //#region eval.c
    public static final ELispSymbol.Value.ForwardedLong maxLispEvalDepth = new ELispSymbol.Value.ForwardedLong((long) 1_600);
    public static final ELispSymbol.Value.ForwardedLong lispEvalDepthReserve = new ELispSymbol.Value.ForwardedLong((long) 200);
    public static final ELispSymbol.Value.Forwarded quitFlag = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitQuit = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitDebugger = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded debugOnError = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded debugIgnoredErrors = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool debugOnQuit = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool debugOnNextCall = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool backtraceOnRedisplayError = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool debuggerMayContinue = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool debuggerStackFrameAsList = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded debugger = new ELispSymbol.Value.Forwarded((Object) DEBUG_EARLY);
    public static final ELispSymbol.Value.Forwarded signalHookFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded debugOnSignal = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool backtraceOnErrorNoninteractive = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedLong whenEnteredDebugger = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded internalInterpreterEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded internalMakeInterpretedClosureFunction = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded runHooks = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded autoloadQueue = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded signalingFunction = new ELispSymbol.Value.Forwarded(false);

    private static void evalVars() {
        MAX_LISP_EVAL_DEPTH.initForwardTo(maxLispEvalDepth);
        LISP_EVAL_DEPTH_RESERVE.initForwardTo(lispEvalDepthReserve);
        QUIT_FLAG.initForwardTo(quitFlag);
        INHIBIT_QUIT.initForwardTo(inhibitQuit);
        INHIBIT_DEBUGGER.initForwardTo(inhibitDebugger);
        DEBUG_ON_ERROR.initForwardTo(debugOnError);
        DEBUG_IGNORED_ERRORS.initForwardTo(debugIgnoredErrors);
        DEBUG_ON_QUIT.initForwardTo(debugOnQuit);
        DEBUG_ON_NEXT_CALL.initForwardTo(debugOnNextCall);
        BACKTRACE_ON_REDISPLAY_ERROR.initForwardTo(backtraceOnRedisplayError);
        DEBUGGER_MAY_CONTINUE.initForwardTo(debuggerMayContinue);
        DEBUGGER_STACK_FRAME_AS_LIST.initForwardTo(debuggerStackFrameAsList);
        DEBUGGER.initForwardTo(debugger);
        SIGNAL_HOOK_FUNCTION.initForwardTo(signalHookFunction);
        DEBUG_ON_SIGNAL.initForwardTo(debugOnSignal);
        BACKTRACE_ON_ERROR_NONINTERACTIVE.initForwardTo(backtraceOnErrorNoninteractive);
        INTERNAL_WHEN_ENTERED_DEBUGGER.initForwardTo(whenEnteredDebugger);
        INTERNAL_INTERPRETER_ENVIRONMENT.initForwardTo(internalInterpreterEnvironment);
        INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION.initForwardTo(internalMakeInterpretedClosureFunction);
    }
    private static void evalPostInitVars() {
        ELispContext.unintern(INTERNAL_INTERPRETER_ENVIRONMENT);
        var runHooksJInit = ELispContext.intern("run-hooks");
        runHooks.setValue(runHooksJInit);
    }
    //#endregion eval.c
    //#region fns.c
    public static final ELispSymbol.Value.Forwarded overridingPlistEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded features = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool useDialogBox = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool useFileDialog = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool useShortAnswers = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded yesOrNoPrompt = new ELispSymbol.Value.Forwarded(false);

    private static void fnsVars() {
        OVERRIDING_PLIST_ENVIRONMENT.initForwardTo(overridingPlistEnvironment);
        FEATURES.initForwardTo(features);
        USE_DIALOG_BOX.initForwardTo(useDialogBox);
        USE_FILE_DIALOG.initForwardTo(useFileDialog);
        USE_SHORT_ANSWERS.initForwardTo(useShortAnswers);
        YES_OR_NO_PROMPT.initForwardTo(yesOrNoPrompt);
    }
    private static void fnsPostInitVars() {
        YES_OR_NO_P_HISTORY.setValue(NIL);
        var featuresJInit = new ELispCons(EMACS);
        features.setValue(featuresJInit);
        FEATURES.setSpecial(false);
        var yesOrNoPromptJInit = new ELispString("(yes or no) ");
        yesOrNoPrompt.setValue(yesOrNoPromptJInit);
    }
    //#endregion fns.c
    //#region lread.c
    public static final ELispSymbol.Value.Forwarded obarray = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded values = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded standardInput = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded readCircle = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded loadPath = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded loadSuffixes = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded moduleFileSuffix = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded dynamicLibrarySuffixes = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded loadFileRepSuffixes = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool loadInProgress = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded afterLoadAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded loadHistory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded loadFileName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded loadTrueFileName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded userInitFile = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded currentLoadList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded loadReadFunction = new ELispSymbol.Value.Forwarded((Object) READ);
    public static final ELispSymbol.Value.Forwarded loadSourceFileFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool loadForceDocStrings = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool loadConvertToUnibyte = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded sourceDirectory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded preloadedFileList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded byteBooleanVars = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool loadDangerousLibraries = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool forceLoadMessages = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded bytecompVersionRegexp = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lexicalBinding = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded evalBufferList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lreadUnescapedCharacterLiterals = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool loadPreferNewer = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool loadNoNative = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded readSymbolShorthands = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded macroexpDynvars = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded loadsInProgress = new ELispSymbol.Value.Forwarded(false);

    private static void lreadVars() {
        OBARRAY.initForwardTo(obarray);
        VALUES.initForwardTo(values);
        STANDARD_INPUT.initForwardTo(standardInput);
        READ_CIRCLE.initForwardTo(readCircle);
        LOAD_PATH.initForwardTo(loadPath);
        LOAD_SUFFIXES.initForwardTo(loadSuffixes);
        MODULE_FILE_SUFFIX.initForwardTo(moduleFileSuffix);
        DYNAMIC_LIBRARY_SUFFIXES.initForwardTo(dynamicLibrarySuffixes);
        LOAD_FILE_REP_SUFFIXES.initForwardTo(loadFileRepSuffixes);
        LOAD_IN_PROGRESS.initForwardTo(loadInProgress);
        AFTER_LOAD_ALIST.initForwardTo(afterLoadAlist);
        LOAD_HISTORY.initForwardTo(loadHistory);
        LOAD_FILE_NAME.initForwardTo(loadFileName);
        LOAD_TRUE_FILE_NAME.initForwardTo(loadTrueFileName);
        USER_INIT_FILE.initForwardTo(userInitFile);
        CURRENT_LOAD_LIST.initForwardTo(currentLoadList);
        LOAD_READ_FUNCTION.initForwardTo(loadReadFunction);
        LOAD_SOURCE_FILE_FUNCTION.initForwardTo(loadSourceFileFunction);
        LOAD_FORCE_DOC_STRINGS.initForwardTo(loadForceDocStrings);
        LOAD_CONVERT_TO_UNIBYTE.initForwardTo(loadConvertToUnibyte);
        SOURCE_DIRECTORY.initForwardTo(sourceDirectory);
        PRELOADED_FILE_LIST.initForwardTo(preloadedFileList);
        BYTE_BOOLEAN_VARS.initForwardTo(byteBooleanVars);
        LOAD_DANGEROUS_LIBRARIES.initForwardTo(loadDangerousLibraries);
        FORCE_LOAD_MESSAGES.initForwardTo(forceLoadMessages);
        BYTECOMP_VERSION_REGEXP.initForwardTo(bytecompVersionRegexp);
        LEXICAL_BINDING.initForwardTo(lexicalBinding);
        EVAL_BUFFER_LIST.initForwardTo(evalBufferList);
        LREAD__UNESCAPED_CHARACTER_LITERALS.initForwardTo(lreadUnescapedCharacterLiterals);
        LOAD_PREFER_NEWER.initForwardTo(loadPreferNewer);
        LOAD_NO_NATIVE.initForwardTo(loadNoNative);
        READ_SYMBOL_SHORTHANDS.initForwardTo(readSymbolShorthands);
        MACROEXP__DYNVARS.initForwardTo(macroexpDynvars);
    }
    private static void lreadPostInitVars() {
        ELispContext.intern("values").setSpecial(false);
        var loadSuffixesJInit = ELispCons.listOf(new ELispString(".elc"), new ELispString(".el"));
        loadSuffixes.setValue(loadSuffixesJInit);
        var loadFileRepSuffixesJInit = new ELispCons(new ELispString(""));
        loadFileRepSuffixes.setValue(loadFileRepSuffixesJInit);
        var sourceDirectoryJInit = new ELispString("");
        sourceDirectory.setValue(sourceDirectoryJInit);
        var bytecompVersionRegexpJInit = new ELispString("^;;;.\\(?:in Emacs version\\|bytecomp version FSF\\)");
        bytecompVersionRegexp.setValue(bytecompVersionRegexpJInit);
        LEXICAL_BINDING.setBufferLocal(true);
    }
    //#endregion lread.c
    //#region process.c
    public static final ELispSymbol.Value.ForwardedBool deleteExitedProcesses = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded processConnectionType = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded processAdaptiveReadBuffering = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool processPrioritizeLowerFds = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded interruptProcessFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded signalProcessFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded internalDaemonSockname = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong readProcessOutputMax = new ELispSymbol.Value.ForwardedLong((long) 65_536);
    public static final ELispSymbol.Value.ForwardedBool fastReadProcessOutput = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedLong processErrorPauseTime = new ELispSymbol.Value.ForwardedLong((long) 1);

    private static void processVars() {
        DELETE_EXITED_PROCESSES.initForwardTo(deleteExitedProcesses);
        PROCESS_CONNECTION_TYPE.initForwardTo(processConnectionType);
        PROCESS_ADAPTIVE_READ_BUFFERING.initForwardTo(processAdaptiveReadBuffering);
        PROCESS_PRIORITIZE_LOWER_FDS.initForwardTo(processPrioritizeLowerFds);
        INTERRUPT_PROCESS_FUNCTIONS.initForwardTo(interruptProcessFunctions);
        SIGNAL_PROCESS_FUNCTIONS.initForwardTo(signalProcessFunctions);
        INTERNAL__DAEMON_SOCKNAME.initForwardTo(internalDaemonSockname);
        READ_PROCESS_OUTPUT_MAX.initForwardTo(readProcessOutputMax);
        FAST_READ_PROCESS_OUTPUT.initForwardTo(fastReadProcessOutput);
        PROCESS_ERROR_PAUSE_TIME.initForwardTo(processErrorPauseTime);
    }
    private static void processPostInitVars() {
        var interruptProcessFunctionsJInit = new ELispCons(INTERNAL_DEFAULT_INTERRUPT_PROCESS);
        interruptProcessFunctions.setValue(interruptProcessFunctionsJInit);
        var signalProcessFunctionsJInit = new ELispCons(INTERNAL_DEFAULT_SIGNAL_PROCESS);
        signalProcessFunctions.setValue(signalProcessFunctionsJInit);
        FProvide.provide(ELispContext.intern("make-network-process"), new ELispCons(new ELispCons(CSERVER, new ELispCons(T, NIL)), new ELispCons(new ELispCons(CFAMILY, new ELispCons(IPV4, NIL)), new ELispCons(new ELispCons(CNOWAIT, new ELispCons(T, NIL)), NIL))));
    }
    //#endregion process.c
    //#region charset.c
    public static final ELispSymbol.Value.Forwarded charsetMapPath = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitLoadCharsetMap = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded charsetList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded currentIso639Language = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded charsetOrderedList = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded iso2022CharsetList = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded emacsMuleCharsetList = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded charsetHashTable = new ELispSymbol.Value.Forwarded(false);

    private static void charsetVars() {
        CHARSET_MAP_PATH.initForwardTo(charsetMapPath);
        INHIBIT_LOAD_CHARSET_MAP.initForwardTo(inhibitLoadCharsetMap);
        CHARSET_LIST.initForwardTo(charsetList);
        CURRENT_ISO639_LANGUAGE.initForwardTo(currentIso639Language);
    }
    private static void charsetPostInitVars() {
        var charsetHashTableJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
        charsetHashTable.setValue(charsetHashTableJInit);
        defineCharsetInternal(ASCII, 1, "\u0000\u007f\u0000\u0000\u0000\u0000\u0000", 0, 127, 66, -1, 0, 1, 0, 0);
        defineCharsetInternal(ISO_8859_1, 1, "\u0000\u00ff\u0000\u0000\u0000\u0000\u0000", 0, 255, -1, -1, -1, 1, 0, 0);
        defineCharsetInternal(UNICODE, 3, "\u0000\u00ff\u0000\u00ff\u0000\u0010\u0000", 0, 1114111, -1, 0, -1, 1, 0, 0);
        defineCharsetInternal(EMACS, 3, "\u0000\u00ff\u0000\u00ff\u0000?\u0000", 0, 4194175, -1, 0, -1, 1, 1, 0);
        defineCharsetInternal(EIGHT_BIT, 1, "\u0080\u00ff\u0000\u0000\u0000\u0000\u0000", 128, 255, -1, 0, -1, 0, 1, 4194176);
    }
    //#endregion charset.c
    //#region fileio.c
    public static final ELispSymbol.Value.Forwarded fileNameCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded defaultFileNameCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded fileNameHandlerAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded setAutoCodingFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded afterInsertFileFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded writeRegionAnnotateFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded writeRegionPostAnnotationFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded writeRegionAnnotationsSoFar = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitFileNameHandlers = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitFileNameOperation = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded autoSaveListFileName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded autoSaveVisitedFileName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded autoSaveIncludeBigDeletions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool writeRegionInhibitFsync = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool deleteByMovingToTrash = new ELispSymbol.Value.ForwardedBool(false);

    private static void fileioVars() {
        FILE_NAME_CODING_SYSTEM.initForwardTo(fileNameCodingSystem);
        DEFAULT_FILE_NAME_CODING_SYSTEM.initForwardTo(defaultFileNameCodingSystem);
        FILE_NAME_HANDLER_ALIST.initForwardTo(fileNameHandlerAlist);
        SET_AUTO_CODING_FUNCTION.initForwardTo(setAutoCodingFunction);
        AFTER_INSERT_FILE_FUNCTIONS.initForwardTo(afterInsertFileFunctions);
        WRITE_REGION_ANNOTATE_FUNCTIONS.initForwardTo(writeRegionAnnotateFunctions);
        WRITE_REGION_POST_ANNOTATION_FUNCTION.initForwardTo(writeRegionPostAnnotationFunction);
        WRITE_REGION_ANNOTATIONS_SO_FAR.initForwardTo(writeRegionAnnotationsSoFar);
        INHIBIT_FILE_NAME_HANDLERS.initForwardTo(inhibitFileNameHandlers);
        INHIBIT_FILE_NAME_OPERATION.initForwardTo(inhibitFileNameOperation);
        AUTO_SAVE_LIST_FILE_NAME.initForwardTo(autoSaveListFileName);
        AUTO_SAVE_VISITED_FILE_NAME.initForwardTo(autoSaveVisitedFileName);
        AUTO_SAVE_INCLUDE_BIG_DELETIONS.initForwardTo(autoSaveIncludeBigDeletions);
        WRITE_REGION_INHIBIT_FSYNC.initForwardTo(writeRegionInhibitFsync);
        DELETE_BY_MOVING_TO_TRASH.initForwardTo(deleteByMovingToTrash);
    }
    private static void fileioPostInitVars() {
        FILE_NAME_HISTORY.setValue(NIL);
        FILE_ERROR.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(FILE_ERROR, ERROR)));
        FILE_ERROR.putProperty(ERROR_MESSAGE, new ELispString("File error"));
        FILE_ALREADY_EXISTS.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(FILE_ALREADY_EXISTS, FILE_ERROR, ERROR)));
        FILE_ALREADY_EXISTS.putProperty(ERROR_MESSAGE, new ELispString("File already exists"));
        FILE_DATE_ERROR.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(FILE_DATE_ERROR, FILE_ERROR, ERROR)));
        FILE_DATE_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Cannot set file date"));
        FILE_MISSING.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(FILE_MISSING, FILE_ERROR, ERROR)));
        FILE_MISSING.putProperty(ERROR_MESSAGE, new ELispString("File is missing"));
        PERMISSION_DENIED.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(PERMISSION_DENIED, FILE_ERROR, ERROR)));
        PERMISSION_DENIED.putProperty(ERROR_MESSAGE, new ELispString("Cannot access file or directory"));
        FILE_NOTIFY_ERROR.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(FILE_NOTIFY_ERROR, FILE_ERROR, ERROR)));
        FILE_NOTIFY_ERROR.putProperty(ERROR_MESSAGE, new ELispString("File notification error"));
        REMOTE_FILE_ERROR.putProperty(ERROR_CONDITIONS, FPurecopy.purecopy(ELispCons.listOf(REMOTE_FILE_ERROR, FILE_ERROR, ERROR)));
        REMOTE_FILE_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Remote file error"));
    }
    //#endregion fileio.c
    //#region editfns.c
    public static final ELispSymbol.Value.Forwarded inhibitFieldTextMotion = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded bufferAccessFontifyFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded bufferAccessFontifiedProperty = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded systemName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded userFullName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded userLoginName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded userRealLoginName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded operatingSystemRelease = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.ForwardedBool binaryAsUnsigned = new ELispSymbol.Value.ForwardedBool(false);

    private static void editfnsVars() {
        INHIBIT_FIELD_TEXT_MOTION.initForwardTo(inhibitFieldTextMotion);
        BUFFER_ACCESS_FONTIFY_FUNCTIONS.initForwardTo(bufferAccessFontifyFunctions);
        BUFFER_ACCESS_FONTIFIED_PROPERTY.initForwardTo(bufferAccessFontifiedProperty);
        SYSTEM_NAME.initForwardTo(systemName);
        USER_FULL_NAME.initForwardTo(userFullName);
        USER_LOGIN_NAME.initForwardTo(userLoginName);
        USER_REAL_LOGIN_NAME.initForwardTo(userRealLoginName);
        OPERATING_SYSTEM_RELEASE.initForwardTo(operatingSystemRelease);
        BINARY_AS_UNSIGNED.initForwardTo(binaryAsUnsigned);
    }
    private static void editfnsPostInitVars() {
        ELispContext.unintern(OUTERMOST_RESTRICTION);
    }
    //#endregion editfns.c
    //#region emacs.c
    public static final ELispSymbol.Value.Forwarded commandLineArgs = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded systemType = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded systemConfiguration = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded systemConfigurationOptions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded systemConfigurationFeatures = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool noninteractive1 = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded killEmacsHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded pathSeparator = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded invocationName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded invocationDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded installationDirectory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded systemMessagesLocale = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded systemTimeLocale = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded beforeInitTime = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded afterInitTime = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitXResources = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded emacsCopyright = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded emacsVersion = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded reportEmacsBugAddress = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded dumpMode = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded dynamicLibraryAlist = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded previousSystemTimeLocale = new ELispSymbol.Value.Forwarded(false);

    private static void emacsVars() {
        COMMAND_LINE_ARGS.initForwardTo(commandLineArgs);
        SYSTEM_TYPE.initForwardTo(systemType);
        SYSTEM_CONFIGURATION.initForwardTo(systemConfiguration);
        SYSTEM_CONFIGURATION_OPTIONS.initForwardTo(systemConfigurationOptions);
        SYSTEM_CONFIGURATION_FEATURES.initForwardTo(systemConfigurationFeatures);
        NONINTERACTIVE.initForwardTo(noninteractive1);
        KILL_EMACS_HOOK.initForwardTo(killEmacsHook);
        PATH_SEPARATOR.initForwardTo(pathSeparator);
        INVOCATION_NAME.initForwardTo(invocationName);
        INVOCATION_DIRECTORY.initForwardTo(invocationDirectory);
        INSTALLATION_DIRECTORY.initForwardTo(installationDirectory);
        SYSTEM_MESSAGES_LOCALE.initForwardTo(systemMessagesLocale);
        SYSTEM_TIME_LOCALE.initForwardTo(systemTimeLocale);
        BEFORE_INIT_TIME.initForwardTo(beforeInitTime);
        AFTER_INIT_TIME.initForwardTo(afterInitTime);
        INHIBIT_X_RESOURCES.initForwardTo(inhibitXResources);
        EMACS_COPYRIGHT.initForwardTo(emacsCopyright);
        EMACS_VERSION.initForwardTo(emacsVersion);
        REPORT_EMACS_BUG_ADDRESS.initForwardTo(reportEmacsBugAddress);
        DUMP_MODE.initForwardTo(dumpMode);
        DYNAMIC_LIBRARY_ALIST.initForwardTo(dynamicLibraryAlist);
    }
    private static void emacsPostInitVars() {
        var systemTypeJInit = ELispContext.intern("jvm");
        systemType.setValue(systemTypeJInit);
        var systemConfigurationJInit = new ELispString("");
        systemConfiguration.setValue(systemConfigurationJInit);
        var systemConfigurationOptionsJInit = new ELispString("");
        systemConfigurationOptions.setValue(systemConfigurationOptionsJInit);
        var systemConfigurationFeaturesJInit = new ELispString("");
        systemConfigurationFeatures.setValue(systemConfigurationFeaturesJInit);
        var pathSeparatorJInit = new ELispString(":");
        pathSeparator.setValue(pathSeparatorJInit);
        var emacsCopyrightJInit = new ELispString("TODO: Copy over GPL");
        emacsCopyright.setValue(emacsCopyrightJInit);
        var emacsVersionJInit = new ELispString("30.0");
        emacsVersion.setValue(emacsVersionJInit);
        var reportEmacsBugAddressJInit = new ELispString("");
        reportEmacsBugAddress.setValue(reportEmacsBugAddressJInit);
        ELispContext.intern("dynamic-library-alist").putProperty(RISKY_LOCAL_VARIABLE, T);
    }
    //#endregion emacs.c
    //#region search.c
    public static final ELispSymbol.Value.Forwarded searchSpacesRegexp = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitChangingMatchData = new ELispSymbol.Value.Forwarded(false);

    private static void searchVars() {
        SEARCH_SPACES_REGEXP.initForwardTo(searchSpacesRegexp);
        INHIBIT_CHANGING_MATCH_DATA.initForwardTo(inhibitChangingMatchData);
    }
    private static void searchPostInitVars() {
        SEARCH_FAILED.putProperty(ERROR_CONDITIONS, ELispCons.listOf(SEARCH_FAILED, ERROR));
        SEARCH_FAILED.putProperty(ERROR_MESSAGE, new ELispString("Search failed"));
        USER_SEARCH_FAILED.putProperty(ERROR_CONDITIONS, ELispCons.listOf(USER_SEARCH_FAILED, USER_ERROR, SEARCH_FAILED, ERROR));
        USER_SEARCH_FAILED.putProperty(ERROR_MESSAGE, new ELispString("Search failed"));
        INVALID_REGEXP.putProperty(ERROR_CONDITIONS, ELispCons.listOf(INVALID_REGEXP, ERROR));
        INVALID_REGEXP.putProperty(ERROR_MESSAGE, new ELispString("Invalid regexp"));
    }
    //#endregion search.c
    //#region buffer.c
    public static final ELispSymbol.Value.Forwarded beforeChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded afterChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded firstChangeHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded transientMarkMode = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitReadOnly = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded killBufferQueryFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded changeMajorModeHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded bufferListUpdateHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool killBufferDeleteAutoSaveFiles = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool deleteAutoSaveFiles = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded caseFoldSearch = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded cloneIndirectBufferHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded longLineThreshold = new ELispSymbol.Value.Forwarded((Object) 50000);
    public static final ELispSymbol.Value.ForwardedLong longLineOptimizationsRegionSize = new ELispSymbol.Value.ForwardedLong((long) 500_000);
    public static final ELispSymbol.Value.ForwardedLong longLineOptimizationsBolSearchLimit = new ELispSymbol.Value.ForwardedLong((long) 128);
    public static final ELispSymbol.Value.ForwardedLong largeHscrollThreshold = new ELispSymbol.Value.ForwardedLong((long) 10_000);

    private static void bufferVars() {
        BEFORE_CHANGE_FUNCTIONS.initForwardTo(beforeChangeFunctions);
        AFTER_CHANGE_FUNCTIONS.initForwardTo(afterChangeFunctions);
        FIRST_CHANGE_HOOK.initForwardTo(firstChangeHook);
        TRANSIENT_MARK_MODE.initForwardTo(transientMarkMode);
        INHIBIT_READ_ONLY.initForwardTo(inhibitReadOnly);
        KILL_BUFFER_QUERY_FUNCTIONS.initForwardTo(killBufferQueryFunctions);
        CHANGE_MAJOR_MODE_HOOK.initForwardTo(changeMajorModeHook);
        BUFFER_LIST_UPDATE_HOOK.initForwardTo(bufferListUpdateHook);
        KILL_BUFFER_DELETE_AUTO_SAVE_FILES.initForwardTo(killBufferDeleteAutoSaveFiles);
        DELETE_AUTO_SAVE_FILES.initForwardTo(deleteAutoSaveFiles);
        CASE_FOLD_SEARCH.initForwardTo(caseFoldSearch);
        CLONE_INDIRECT_BUFFER_HOOK.initForwardTo(cloneIndirectBufferHook);
        LONG_LINE_THRESHOLD.initForwardTo(longLineThreshold);
        LONG_LINE_OPTIMIZATIONS_REGION_SIZE.initForwardTo(longLineOptimizationsRegionSize);
        LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT.initForwardTo(longLineOptimizationsBolSearchLimit);
        LARGE_HSCROLL_THRESHOLD.initForwardTo(largeHscrollThreshold);
        ELispBuffer.initBufferLocalVars();
    }
    private static void bufferPostInitVars() {
        VERTICAL_SCROLL_BAR.putProperty(CHOICE, ELispCons.listOf(NIL, T, LEFT, RIGHT));
        FRACTION.putProperty(RANGE, new ELispCons(0.0, 1.0));
        OVERWRITE_MODE.putProperty(CHOICE, ELispCons.listOf(NIL, ELispContext.intern("overwrite-mode-textual"), OVERWRITE_MODE_BINARY));
        PROTECTED_FIELD.putProperty(ERROR_CONDITIONS, ELispCons.listOf(PROTECTED_FIELD, ERROR));
        PROTECTED_FIELD.putProperty(ERROR_MESSAGE, new ELispString("Attempt to modify a protected field"));
        ELispContext.intern("enable-multibyte-characters").setConstant(true);
        CASE_FOLD_SEARCH.setBufferLocal(true);
        ELispContext.intern("erase-buffer").putProperty(DISABLED, T);
    }
    //#endregion buffer.c
    //#region keymap.c
    public static final ELispSymbol.Value.Forwarded minibufferLocalMap = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minorModeMapAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minorModeOverridingMapAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded emulationModeMapAlists = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded whereIsPreferredModifier = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded describeBindingsCheckShadowingInRanges = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded mouseEvents = new ELispSymbol.Value.Forwarded(false);

    private static void keymapVars() {
        MINIBUFFER_LOCAL_MAP.initForwardTo(minibufferLocalMap);
        MINOR_MODE_MAP_ALIST.initForwardTo(minorModeMapAlist);
        MINOR_MODE_OVERRIDING_MAP_ALIST.initForwardTo(minorModeOverridingMapAlist);
        EMULATION_MODE_MAP_ALISTS.initForwardTo(emulationModeMapAlists);
        WHERE_IS_PREFERRED_MODIFIER.initForwardTo(whereIsPreferredModifier);
        DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES.initForwardTo(describeBindingsCheckShadowingInRanges);
    }
    private static void keymapPostInitVars() {
        KEYMAP.putProperty(CHAR_TABLE_EXTRA_SLOTS, (long) (0));
        var minibufferLocalMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        minibufferLocalMap.setValue(minibufferLocalMapJInit);
        var mouseEventsJInit = ELispCons.listOf(MENU_BAR, TAB_BAR, TOOL_BAR, TAB_LINE, HEADER_LINE, MODE_LINE, ELispContext.intern("mouse-1"), ELispContext.intern("mouse-2"), ELispContext.intern("mouse-3"), ELispContext.intern("mouse-4"), ELispContext.intern("mouse-5"));
        mouseEvents.setValue(mouseEventsJInit);
    }
    //#endregion keymap.c
    //#region print.c
    public static final ELispSymbol.Value.Forwarded standardOutput = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded floatOutputFormat = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool printIntegersAsCharacters = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded printLength = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printLevel = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool printEscapeNewlines = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool printEscapeControlCharacters = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool printEscapeNonascii = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool printEscapeMultibyte = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool printQuoted = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded printGensym = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printCircle = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printContinuousNumbering = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printNumberTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printCharsetTextProperty = new ELispSymbol.Value.Forwarded((Object) DEFAULT);
    public static final ELispSymbol.Value.ForwardedBool printSymbolsBare = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded printUnreadableFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printUnreadableCallbackBuffer = new ELispSymbol.Value.Forwarded(false);

    private static void printVars() {
        STANDARD_OUTPUT.initForwardTo(standardOutput);
        FLOAT_OUTPUT_FORMAT.initForwardTo(floatOutputFormat);
        PRINT_INTEGERS_AS_CHARACTERS.initForwardTo(printIntegersAsCharacters);
        PRINT_LENGTH.initForwardTo(printLength);
        PRINT_LEVEL.initForwardTo(printLevel);
        PRINT_ESCAPE_NEWLINES.initForwardTo(printEscapeNewlines);
        PRINT_ESCAPE_CONTROL_CHARACTERS.initForwardTo(printEscapeControlCharacters);
        PRINT_ESCAPE_NONASCII.initForwardTo(printEscapeNonascii);
        PRINT_ESCAPE_MULTIBYTE.initForwardTo(printEscapeMultibyte);
        PRINT_QUOTED.initForwardTo(printQuoted);
        PRINT_GENSYM.initForwardTo(printGensym);
        PRINT_CIRCLE.initForwardTo(printCircle);
        PRINT_CONTINUOUS_NUMBERING.initForwardTo(printContinuousNumbering);
        PRINT_NUMBER_TABLE.initForwardTo(printNumberTable);
        PRINT_CHARSET_TEXT_PROPERTY.initForwardTo(printCharsetTextProperty);
        PRINT_SYMBOLS_BARE.initForwardTo(printSymbolsBare);
        PRINT_UNREADABLE_FUNCTION.initForwardTo(printUnreadableFunction);
        PRINT__UNREADABLE_CALLBACK_BUFFER.initForwardTo(printUnreadableCallbackBuffer);
    }
    private static void printPostInitVars() {
        ELispContext.unintern(PRINT__UNREADABLE_CALLBACK_BUFFER);
    }
    //#endregion print.c
    //#region xfaces.c
    public static final ELispSymbol.Value.ForwardedBool faceFiltersAlwaysMatch = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded faceNewFrameDefaults = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded faceDefaultStipple = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded ttyDefinedColorAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded scalableFontsAllowed = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded faceIgnoredFonts = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded faceRemappingAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded faceFontRescaleAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong faceNearSameColorThreshold = new ELispSymbol.Value.ForwardedLong((long) 30_000);
    public static final ELispSymbol.Value.Forwarded faceFontLaxMatchedAttributes = new ELispSymbol.Value.Forwarded((Object) T);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded faceAlternativeFontFamilyAlist = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded faceAlternativeFontRegistryAlist = new ELispSymbol.Value.Forwarded(false);

    private static void xfacesVars() {
        FACE_FILTERS_ALWAYS_MATCH.initForwardTo(faceFiltersAlwaysMatch);
        FACE__NEW_FRAME_DEFAULTS.initForwardTo(faceNewFrameDefaults);
        FACE_DEFAULT_STIPPLE.initForwardTo(faceDefaultStipple);
        TTY_DEFINED_COLOR_ALIST.initForwardTo(ttyDefinedColorAlist);
        SCALABLE_FONTS_ALLOWED.initForwardTo(scalableFontsAllowed);
        FACE_IGNORED_FONTS.initForwardTo(faceIgnoredFonts);
        FACE_REMAPPING_ALIST.initForwardTo(faceRemappingAlist);
        FACE_FONT_RESCALE_ALIST.initForwardTo(faceFontRescaleAlist);
        FACE_NEAR_SAME_COLOR_THRESHOLD.initForwardTo(faceNearSameColorThreshold);
        FACE_FONT_LAX_MATCHED_ATTRIBUTES.initForwardTo(faceFontLaxMatchedAttributes);
    }
    private static void xfacesPostInitVars() {
        var faceNewFrameDefaultsJInit = new ELispHashtable();
        faceNewFrameDefaults.setValue(faceNewFrameDefaultsJInit);
        var faceDefaultStippleJInit = new ELispString("gray3");
        faceDefaultStipple.setValue(faceDefaultStippleJInit);
    }
    //#endregion xfaces.c
    //#region timefns.c
    public static final ELispSymbol.Value.ForwardedBool currentTimeList = new ELispSymbol.Value.ForwardedBool(true);

    private static void timefnsVars() {
        CURRENT_TIME_LIST.initForwardTo(currentTimeList);
    }
    private static void timefnsPostInitVars() {

    }
    //#endregion timefns.c
    //#region casetab.c

    private static void casetabVars() {

    }
    private static void casetabPostInitVars() {

    }
    //#endregion casetab.c
    //#region cmds.c
    public static final ELispSymbol.Value.Forwarded postSelfInsertHook = new ELispSymbol.Value.Forwarded(false);

    private static void cmdsVars() {
        POST_SELF_INSERT_HOOK.initForwardTo(postSelfInsertHook);
    }
    private static void cmdsPostInitVars() {

    }
    //#endregion cmds.c
    //#region keyboard.c
    public static final ELispSymbol.Value.Forwarded internalTopLevelMessage = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lastCommandEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded lastNonmenuEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded lastInputEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded unreadCommandEvents = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded unreadPostInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded unreadInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded metaPrefixChar = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lastCommand = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded realLastCommand = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded lastRepeatableCommand = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded thisCommand = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded realThisCommand = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded currentMinibufferCommand = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded thisCommandKeysShiftTranslated = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded thisOriginalCommand = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong autoSaveInterval = new ELispSymbol.Value.ForwardedLong((long) 300);
    public static final ELispSymbol.Value.ForwardedBool autoSaveNoMessage = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded autoSaveTimeout = new ELispSymbol.Value.Forwarded((Object) 30);
    public static final ELispSymbol.Value.Forwarded echoKeystrokes = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool echoKeystrokesHelp = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded pollingPeriod = new ELispSymbol.Value.Forwarded((Object) 2.0);
    public static final ELispSymbol.Value.Forwarded doubleClickTime = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong doubleClickFuzz = new ELispSymbol.Value.ForwardedLong((long) 3);
    public static final ELispSymbol.Value.ForwardedLong numInputKeys = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong numNonmacroInputEvents = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded lastEventFrame = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lastEventDevice = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded ttyEraseChar = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded helpChar = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded helpEventList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded helpForm = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded prefixHelpCommand = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded topLevel = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded keyboardTranslateTable = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.ForwardedBool cannotSuspend = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool menuPrompting = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded menuPromptMoreChar = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong extraKeyboardModifiers = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded deactivateMark = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded preCommandHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded postCommandHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool lucidMenuGrabKeyboard = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded menuBarFinalItems = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded tabBarSeparatorImageExpression = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded toolBarSeparatorImageExpression = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded overridingTerminalLocalMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded overridingLocalMap = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded overridingLocalMapMenuFlag = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded specialEventMap = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded trackMouse = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded systemKeyAlist = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded localFunctionKeyMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded inputDecodeMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded functionKeyMap = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded keyTranslationMap = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded delayedWarningsList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded timerList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded timerIdleList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inputMethodFunction = new ELispSymbol.Value.Forwarded((Object) LIST);
    public static final ELispSymbol.Value.Forwarded inputMethodPreviousMessage = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded showHelpFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded disablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded globalDisablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferMessageTimeout = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded throwOnInput = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded commandErrorFunction = new ELispSymbol.Value.Forwarded((Object) COMMAND_ERROR_DEFAULT_FUNCTION);
    public static final ELispSymbol.Value.Forwarded enableDisabledMenusAndButtons = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded selectActiveRegions = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded savedRegionSelection = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded selectionInhibitUpdateCommands = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded debugOnEvent = new ELispSymbol.Value.Forwarded((Object) SIGUSR2);
    public static final ELispSymbol.Value.ForwardedBool attemptStackOverflowRecovery = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool attemptOrderlyShutdownOnFatalSignal = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded whileNoInputIgnoreEvents = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool translateUpperCaseKeyBindings = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool inputPendingPFilterEvents = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool mwheelCoalesceScrollEvents = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded displayMonitorsChangedFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitRecordChar = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool recordAllKeys = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded postSelectRegionHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool disableInhibitTextConversion = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded currentKeyRemapSequence = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded lispyMouseStem = new ELispSymbol.Value.Forwarded(false);

    private static void keyboardVars() {
        INTERNAL__TOP_LEVEL_MESSAGE.initForwardTo(internalTopLevelMessage);
        LAST_COMMAND_EVENT.initForwardTo(lastCommandEvent);
        LAST_NONMENU_EVENT.initForwardTo(lastNonmenuEvent);
        LAST_INPUT_EVENT.initForwardTo(lastInputEvent);
        UNREAD_COMMAND_EVENTS.initForwardTo(unreadCommandEvents);
        UNREAD_POST_INPUT_METHOD_EVENTS.initForwardTo(unreadPostInputMethodEvents);
        UNREAD_INPUT_METHOD_EVENTS.initForwardTo(unreadInputMethodEvents);
        META_PREFIX_CHAR.initForwardTo(metaPrefixChar);
        LAST_COMMAND.initForwardTo(lastCommand);
        REAL_LAST_COMMAND.initForwardTo(realLastCommand);
        LAST_REPEATABLE_COMMAND.initForwardTo(lastRepeatableCommand);
        THIS_COMMAND.initForwardTo(thisCommand);
        REAL_THIS_COMMAND.initForwardTo(realThisCommand);
        CURRENT_MINIBUFFER_COMMAND.initForwardTo(currentMinibufferCommand);
        THIS_COMMAND_KEYS_SHIFT_TRANSLATED.initForwardTo(thisCommandKeysShiftTranslated);
        THIS_ORIGINAL_COMMAND.initForwardTo(thisOriginalCommand);
        AUTO_SAVE_INTERVAL.initForwardTo(autoSaveInterval);
        AUTO_SAVE_NO_MESSAGE.initForwardTo(autoSaveNoMessage);
        AUTO_SAVE_TIMEOUT.initForwardTo(autoSaveTimeout);
        ECHO_KEYSTROKES.initForwardTo(echoKeystrokes);
        ECHO_KEYSTROKES_HELP.initForwardTo(echoKeystrokesHelp);
        POLLING_PERIOD.initForwardTo(pollingPeriod);
        DOUBLE_CLICK_TIME.initForwardTo(doubleClickTime);
        DOUBLE_CLICK_FUZZ.initForwardTo(doubleClickFuzz);
        NUM_INPUT_KEYS.initForwardTo(numInputKeys);
        NUM_NONMACRO_INPUT_EVENTS.initForwardTo(numNonmacroInputEvents);
        LAST_EVENT_FRAME.initForwardTo(lastEventFrame);
        LAST_EVENT_DEVICE.initForwardTo(lastEventDevice);
        TTY_ERASE_CHAR.initForwardTo(ttyEraseChar);
        HELP_CHAR.initForwardTo(helpChar);
        HELP_EVENT_LIST.initForwardTo(helpEventList);
        HELP_FORM.initForwardTo(helpForm);
        PREFIX_HELP_COMMAND.initForwardTo(prefixHelpCommand);
        TOP_LEVEL.initForwardTo(topLevel);
        KEYBOARD_TRANSLATE_TABLE.initForwardTo(keyboardTranslateTable);
        CANNOT_SUSPEND.initForwardTo(cannotSuspend);
        MENU_PROMPTING.initForwardTo(menuPrompting);
        MENU_PROMPT_MORE_CHAR.initForwardTo(menuPromptMoreChar);
        EXTRA_KEYBOARD_MODIFIERS.initForwardTo(extraKeyboardModifiers);
        DEACTIVATE_MARK.initForwardTo(deactivateMark);
        PRE_COMMAND_HOOK.initForwardTo(preCommandHook);
        POST_COMMAND_HOOK.initForwardTo(postCommandHook);
        LUCID__MENU_GRAB_KEYBOARD.initForwardTo(lucidMenuGrabKeyboard);
        MENU_BAR_FINAL_ITEMS.initForwardTo(menuBarFinalItems);
        TAB_BAR_SEPARATOR_IMAGE_EXPRESSION.initForwardTo(tabBarSeparatorImageExpression);
        TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION.initForwardTo(toolBarSeparatorImageExpression);
        OVERRIDING_TERMINAL_LOCAL_MAP.initForwardTo(overridingTerminalLocalMap);
        OVERRIDING_LOCAL_MAP.initForwardTo(overridingLocalMap);
        OVERRIDING_LOCAL_MAP_MENU_FLAG.initForwardTo(overridingLocalMapMenuFlag);
        SPECIAL_EVENT_MAP.initForwardTo(specialEventMap);
        TRACK_MOUSE.initForwardTo(trackMouse);
        SYSTEM_KEY_ALIST.initForwardTo(systemKeyAlist);
        LOCAL_FUNCTION_KEY_MAP.initForwardTo(localFunctionKeyMap);
        INPUT_DECODE_MAP.initForwardTo(inputDecodeMap);
        FUNCTION_KEY_MAP.initForwardTo(functionKeyMap);
        KEY_TRANSLATION_MAP.initForwardTo(keyTranslationMap);
        DELAYED_WARNINGS_LIST.initForwardTo(delayedWarningsList);
        TIMER_LIST.initForwardTo(timerList);
        TIMER_IDLE_LIST.initForwardTo(timerIdleList);
        INPUT_METHOD_FUNCTION.initForwardTo(inputMethodFunction);
        INPUT_METHOD_PREVIOUS_MESSAGE.initForwardTo(inputMethodPreviousMessage);
        SHOW_HELP_FUNCTION.initForwardTo(showHelpFunction);
        DISABLE_POINT_ADJUSTMENT.initForwardTo(disablePointAdjustment);
        GLOBAL_DISABLE_POINT_ADJUSTMENT.initForwardTo(globalDisablePointAdjustment);
        MINIBUFFER_MESSAGE_TIMEOUT.initForwardTo(minibufferMessageTimeout);
        THROW_ON_INPUT.initForwardTo(throwOnInput);
        COMMAND_ERROR_FUNCTION.initForwardTo(commandErrorFunction);
        ENABLE_DISABLED_MENUS_AND_BUTTONS.initForwardTo(enableDisabledMenusAndButtons);
        SELECT_ACTIVE_REGIONS.initForwardTo(selectActiveRegions);
        SAVED_REGION_SELECTION.initForwardTo(savedRegionSelection);
        SELECTION_INHIBIT_UPDATE_COMMANDS.initForwardTo(selectionInhibitUpdateCommands);
        DEBUG_ON_EVENT.initForwardTo(debugOnEvent);
        ATTEMPT_STACK_OVERFLOW_RECOVERY.initForwardTo(attemptStackOverflowRecovery);
        ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL.initForwardTo(attemptOrderlyShutdownOnFatalSignal);
        WHILE_NO_INPUT_IGNORE_EVENTS.initForwardTo(whileNoInputIgnoreEvents);
        TRANSLATE_UPPER_CASE_KEY_BINDINGS.initForwardTo(translateUpperCaseKeyBindings);
        INPUT_PENDING_P_FILTER_EVENTS.initForwardTo(inputPendingPFilterEvents);
        MWHEEL_COALESCE_SCROLL_EVENTS.initForwardTo(mwheelCoalesceScrollEvents);
        DISPLAY_MONITORS_CHANGED_FUNCTIONS.initForwardTo(displayMonitorsChangedFunctions);
        INHIBIT__RECORD_CHAR.initForwardTo(inhibitRecordChar);
        RECORD_ALL_KEYS.initForwardTo(recordAllKeys);
        POST_SELECT_REGION_HOOK.initForwardTo(postSelectRegionHook);
        DISABLE_INHIBIT_TEXT_CONVERSION.initForwardTo(disableInhibitTextConversion);
        CURRENT_KEY_REMAP_SEQUENCE.initForwardTo(currentKeyRemapSequence);
    }
    private static void keyboardPostInitVars() {
        var lispyMouseStemJInit = new ELispString("mouse");
        lispyMouseStem.setValue(lispyMouseStemJInit);
        var internalTopLevelMessageJInit = new ELispString("Back to top level");
        internalTopLevelMessage.setValue(internalTopLevelMessageJInit);
        INPUT_METHOD_EXIT_ON_FIRST_CHAR.setValue(NIL);
        INPUT_METHOD_USE_ECHO_AREA.setValue(NIL);
        var metaPrefixCharJInit = (long) (27);
        metaPrefixChar.setValue(metaPrefixCharJInit);
        var echoKeystrokesJInit = (long) (1);
        echoKeystrokes.setValue(echoKeystrokesJInit);
        var doubleClickTimeJInit = (long) (500);
        doubleClickTime.setValue(doubleClickTimeJInit);
        var helpCharJInit = (long) (8);
        helpChar.setValue(helpCharJInit);
        TOP_LEVEL.setSpecial(false);
        var menuPromptMoreCharJInit = (long) (32);
        menuPromptMoreChar.setValue(menuPromptMoreCharJInit);
        DEACTIVATE_MARK.setBufferLocal(true);
        ECHO_AREA_CLEAR_HOOK.setValue(NIL);
        var specialEventMapJInit = new ELispCons(KEYMAP);
        specialEventMap.setValue(specialEventMapJInit);
        var functionKeyMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        functionKeyMap.setValue(functionKeyMapJInit);
        var keyTranslationMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        keyTranslationMap.setValue(keyTranslationMapJInit);
        var minibufferMessageTimeoutJInit = (long) (2);
        minibufferMessageTimeout.setValue(minibufferMessageTimeoutJInit);
        var selectionInhibitUpdateCommandsJInit = ELispCons.listOf(HANDLE_SWITCH_FRAME, HANDLE_SELECT_WINDOW);
        selectionInhibitUpdateCommands.setValue(selectionInhibitUpdateCommandsJInit);
    }
    //#endregion keyboard.c
    //#region callint.c
    public static final ELispSymbol.Value.Forwarded prefixArg = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded lastPrefixArg = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded currentPrefixArg = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded commandHistory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded commandDebugStatus = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded markEvenIfInactive = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded mouseLeaveBufferHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitMouseEventCheck = new ELispSymbol.Value.ForwardedBool(false);

    private static void callintVars() {
        PREFIX_ARG.initForwardTo(prefixArg);
        LAST_PREFIX_ARG.initForwardTo(lastPrefixArg);
        CURRENT_PREFIX_ARG.initForwardTo(currentPrefixArg);
        COMMAND_HISTORY.initForwardTo(commandHistory);
        COMMAND_DEBUG_STATUS.initForwardTo(commandDebugStatus);
        MARK_EVEN_IF_INACTIVE.initForwardTo(markEvenIfInactive);
        MOUSE_LEAVE_BUFFER_HOOK.initForwardTo(mouseLeaveBufferHook);
        INHIBIT_MOUSE_EVENT_CHECK.initForwardTo(inhibitMouseEventCheck);
    }
    private static void callintPostInitVars() {

    }
    //#endregion callint.c
    //#region casefiddle.c
    public static final ELispSymbol.Value.Forwarded regionExtractFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool caseSymbolsAsWords = new ELispSymbol.Value.ForwardedBool(false);

    private static void casefiddleVars() {
        REGION_EXTRACT_FUNCTION.initForwardTo(regionExtractFunction);
        CASE_SYMBOLS_AS_WORDS.initForwardTo(caseSymbolsAsWords);
    }
    private static void casefiddlePostInitVars() {
        CASE_SYMBOLS_AS_WORDS.setBufferLocal(true);
    }
    //#endregion casefiddle.c
    //#region coding.c
    public static final ELispSymbol.Value.Forwarded codingSystemList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded codingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded codingCategoryList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded codingSystemForRead = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded codingSystemForWrite = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lastCodingSystemUsed = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded lastCodeConversionError = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitEolConversion = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inheritProcessCodingSystem = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded fileCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded processCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded networkCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded localeCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded eolMnemonicUnix = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded eolMnemonicDos = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded eolMnemonicMac = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded eolMnemonicUndecided = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded enableCharacterTranslation = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded standardTranslationTableForDecode = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded standardTranslationTableForEncode = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded charsetRevisionTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded defaultProcessCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded latinExtraCodeTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded selectSafeCodingSystemFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool codingSystemRequireWarning = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitIsoEscapeDetection = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitNullByteDetection = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool disableAsciiOptimization = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded translationTableForInput = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded codingSystemHashTable = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded sjisCodingSystem = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded big5CodingSystem = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded codeConversionReusedWorkbuf = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded codeConversionWorkbufName = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded codingCategoryTable = new ELispSymbol.Value.Forwarded(false);

    private static void codingVars() {
        CODING_SYSTEM_LIST.initForwardTo(codingSystemList);
        CODING_SYSTEM_ALIST.initForwardTo(codingSystemAlist);
        CODING_CATEGORY_LIST.initForwardTo(codingCategoryList);
        CODING_SYSTEM_FOR_READ.initForwardTo(codingSystemForRead);
        CODING_SYSTEM_FOR_WRITE.initForwardTo(codingSystemForWrite);
        LAST_CODING_SYSTEM_USED.initForwardTo(lastCodingSystemUsed);
        LAST_CODE_CONVERSION_ERROR.initForwardTo(lastCodeConversionError);
        INHIBIT_EOL_CONVERSION.initForwardTo(inhibitEolConversion);
        INHERIT_PROCESS_CODING_SYSTEM.initForwardTo(inheritProcessCodingSystem);
        FILE_CODING_SYSTEM_ALIST.initForwardTo(fileCodingSystemAlist);
        PROCESS_CODING_SYSTEM_ALIST.initForwardTo(processCodingSystemAlist);
        NETWORK_CODING_SYSTEM_ALIST.initForwardTo(networkCodingSystemAlist);
        LOCALE_CODING_SYSTEM.initForwardTo(localeCodingSystem);
        EOL_MNEMONIC_UNIX.initForwardTo(eolMnemonicUnix);
        EOL_MNEMONIC_DOS.initForwardTo(eolMnemonicDos);
        EOL_MNEMONIC_MAC.initForwardTo(eolMnemonicMac);
        EOL_MNEMONIC_UNDECIDED.initForwardTo(eolMnemonicUndecided);
        ENABLE_CHARACTER_TRANSLATION.initForwardTo(enableCharacterTranslation);
        STANDARD_TRANSLATION_TABLE_FOR_DECODE.initForwardTo(standardTranslationTableForDecode);
        STANDARD_TRANSLATION_TABLE_FOR_ENCODE.initForwardTo(standardTranslationTableForEncode);
        CHARSET_REVISION_TABLE.initForwardTo(charsetRevisionTable);
        DEFAULT_PROCESS_CODING_SYSTEM.initForwardTo(defaultProcessCodingSystem);
        LATIN_EXTRA_CODE_TABLE.initForwardTo(latinExtraCodeTable);
        SELECT_SAFE_CODING_SYSTEM_FUNCTION.initForwardTo(selectSafeCodingSystemFunction);
        CODING_SYSTEM_REQUIRE_WARNING.initForwardTo(codingSystemRequireWarning);
        INHIBIT_ISO_ESCAPE_DETECTION.initForwardTo(inhibitIsoEscapeDetection);
        INHIBIT_NULL_BYTE_DETECTION.initForwardTo(inhibitNullByteDetection);
        DISABLE_ASCII_OPTIMIZATION.initForwardTo(disableAsciiOptimization);
        TRANSLATION_TABLE_FOR_INPUT.initForwardTo(translationTableForInput);
    }
    private static void codingPostInitVars() {
        var codingSystemHashTableJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
        codingSystemHashTable.setValue(codingSystemHashTableJInit);
        var codeConversionWorkbufNameJInit = new ELispString(" *code-conversion-work*");
        codeConversionWorkbufName.setValue(codeConversionWorkbufNameJInit);
        CODING_SYSTEM_HISTORY.setValue(NIL);
        INSERT_FILE_CONTENTS.putProperty(TARGET_IDX, (long) (0));
        WRITE_REGION.putProperty(TARGET_IDX, (long) (2));
        CALL_PROCESS.putProperty(TARGET_IDX, (long) (0));
        CALL_PROCESS_REGION.putProperty(TARGET_IDX, (long) (2));
        START_PROCESS.putProperty(TARGET_IDX, (long) (2));
        OPEN_NETWORK_STREAM.putProperty(TARGET_IDX, (long) (3));
        CODING_SYSTEM_ERROR.putProperty(ERROR_CONDITIONS, ELispCons.listOf(CODING_SYSTEM_ERROR, ERROR));
        CODING_SYSTEM_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Invalid coding system"));
        TRANSLATION_TABLE.putProperty(CHAR_TABLE_EXTRA_SLOTS, (long) (2));
        var codingCategoryTableJInit = new ELispVector(Collections.nCopies(CODING_CATEGORY_MAX, false));
        codingCategoryTable.setValue(codingCategoryTableJInit);
        codingCategoryTableJInit.set(CODING_CATEGORY_ISO_7, ELispContext.intern("coding-category-iso-7"));
        codingCategoryTableJInit.set(CODING_CATEGORY_ISO_7_TIGHT, ELispContext.intern("coding-category-iso-7-tight"));
        codingCategoryTableJInit.set(CODING_CATEGORY_ISO_8_1, ELispContext.intern("coding-category-iso-8-1"));
        codingCategoryTableJInit.set(CODING_CATEGORY_ISO_8_2, ELispContext.intern("coding-category-iso-8-2"));
        codingCategoryTableJInit.set(CODING_CATEGORY_ISO_7_ELSE, ELispContext.intern("coding-category-iso-7-else"));
        codingCategoryTableJInit.set(CODING_CATEGORY_ISO_8_ELSE, ELispContext.intern("coding-category-iso-8-else"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_8_AUTO, ELispContext.intern("coding-category-utf-8-auto"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_8_NOSIG, ELispContext.intern("coding-category-utf-8"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_8_SIG, ELispContext.intern("coding-category-utf-8-sig"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_16_BE, ELispContext.intern("coding-category-utf-16-be"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_16_AUTO, ELispContext.intern("coding-category-utf-16-auto"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_16_LE, ELispContext.intern("coding-category-utf-16-le"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_16_BE_NOSIG, ELispContext.intern("coding-category-utf-16-be-nosig"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UTF_16_LE_NOSIG, ELispContext.intern("coding-category-utf-16-le-nosig"));
        codingCategoryTableJInit.set(CODING_CATEGORY_CHARSET, ELispContext.intern("coding-category-charset"));
        codingCategoryTableJInit.set(CODING_CATEGORY_SJIS, ELispContext.intern("coding-category-sjis"));
        codingCategoryTableJInit.set(CODING_CATEGORY_BIG5, ELispContext.intern("coding-category-big5"));
        codingCategoryTableJInit.set(CODING_CATEGORY_CCL, ELispContext.intern("coding-category-ccl"));
        codingCategoryTableJInit.set(CODING_CATEGORY_EMACS_MULE, ELispContext.intern("coding-category-emacs-mule"));
        codingCategoryTableJInit.set(CODING_CATEGORY_RAW_TEXT, ELispContext.intern("coding-category-raw-text"));
        codingCategoryTableJInit.set(CODING_CATEGORY_UNDECIDED, ELispContext.intern("coding-category-undecided"));
        var codingCategoryListJInit = false /* TODO */;
        codingCategoryList.setValue(codingCategoryListJInit);
        var eolMnemonicUnixJInit = new ELispString(":");
        eolMnemonicUnix.setValue(eolMnemonicUnixJInit);
        var eolMnemonicDosJInit = new ELispString("\\");
        eolMnemonicDos.setValue(eolMnemonicDosJInit);
        var eolMnemonicMacJInit = new ELispString("/");
        eolMnemonicMac.setValue(eolMnemonicMacJInit);
        var eolMnemonicUndecidedJInit = new ELispString(":");
        eolMnemonicUndecided.setValue(eolMnemonicUndecidedJInit);
        var latinExtraCodeTableJInit = new ELispVector(Collections.nCopies(256, false));
        latinExtraCodeTable.setValue(latinExtraCodeTableJInit);
        // TODO: setup coding system
    }
    //#endregion coding.c
    //#region character.c
    public static final ELispSymbol.Value.Forwarded translationTableVector = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded autoFillChars = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded charWidthTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded ambiguousWidthChars = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded printableChars = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded charScriptTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded scriptRepresentativeChars = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded unicodeCategoryTable = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded charUnifyTable = new ELispSymbol.Value.Forwarded(false);

    private static void characterVars() {
        TRANSLATION_TABLE_VECTOR.initForwardTo(translationTableVector);
        AUTO_FILL_CHARS.initForwardTo(autoFillChars);
        CHAR_WIDTH_TABLE.initForwardTo(charWidthTable);
        AMBIGUOUS_WIDTH_CHARS.initForwardTo(ambiguousWidthChars);
        PRINTABLE_CHARS.initForwardTo(printableChars);
        CHAR_SCRIPT_TABLE.initForwardTo(charScriptTable);
        SCRIPT_REPRESENTATIVE_CHARS.initForwardTo(scriptRepresentativeChars);
        UNICODE_CATEGORY_TABLE.initForwardTo(unicodeCategoryTable);
    }
    private static void characterPostInitVars() {
        var translationTableVectorJInit = new ELispVector(Collections.nCopies(16, false));
        translationTableVector.setValue(translationTableVectorJInit);
        var autoFillCharsJInit = FMakeCharTable.makeCharTable(AUTO_FILL_CHARS, NIL);
        autoFillChars.setValue(autoFillCharsJInit);
        autoFillCharsJInit.setChar(32, T);
        autoFillCharsJInit.setChar(10, T);
        var charWidthTableJInit = FMakeCharTable.makeCharTable(NIL, (long) (1));
        charWidthTable.setValue(charWidthTableJInit);
        charWidthTableJInit.setRange(128, 159, (long) (4));
        charWidthTableJInit.setRange(4194176, 4194303, (long) (4));
        var ambiguousWidthCharsJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        ambiguousWidthChars.setValue(ambiguousWidthCharsJInit);
        var printableCharsJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        printableChars.setValue(printableCharsJInit);
        FSetCharTableRange.setCharTableRange(printableCharsJInit, new ELispCons((long) (32), (long) (126)), T);
        FSetCharTableRange.setCharTableRange(printableCharsJInit, new ELispCons((long) (160), (long) (4194175)), T);
        CHAR_SCRIPT_TABLE.putProperty(CHAR_TABLE_EXTRA_SLOTS, (long) (1));
        var charScriptTableJInit = FMakeCharTable.makeCharTable(CHAR_SCRIPT_TABLE, NIL);
        charScriptTable.setValue(charScriptTableJInit);
    }
    //#endregion character.c
    //#region window.c
    public static final ELispSymbol.Value.Forwarded tempBufferShowFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufScrollWindow = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool modeLineInNonSelectedWindows = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded otherWindowScrollBuffer = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded otherWindowScrollDefault = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool autoWindowVscrollP = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedLong nextScreenContextLines = new ELispSymbol.Value.ForwardedLong((long) 2);
    public static final ELispSymbol.Value.Forwarded scrollPreserveScreenPosition = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowPointInsertionType = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowBufferChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowSizeChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowSelectionChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowStateChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowStateChangeHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowConfigurationChangeHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowRestoreKilledBufferWindows = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded recenterRedisplay = new ELispSymbol.Value.Forwarded((Object) TTY);
    public static final ELispSymbol.Value.Forwarded windowCombinationResize = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowCombinationLimit = new ELispSymbol.Value.Forwarded((Object) WINDOW_SIZE);
    public static final ELispSymbol.Value.Forwarded windowPersistentParameters = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool windowResizePixelwise = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool fastButImpreciseScrolling = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded windowDeadWindowsTable = new ELispSymbol.Value.Forwarded(false);

    private static void windowVars() {
        TEMP_BUFFER_SHOW_FUNCTION.initForwardTo(tempBufferShowFunction);
        MINIBUFFER_SCROLL_WINDOW.initForwardTo(minibufScrollWindow);
        MODE_LINE_IN_NON_SELECTED_WINDOWS.initForwardTo(modeLineInNonSelectedWindows);
        OTHER_WINDOW_SCROLL_BUFFER.initForwardTo(otherWindowScrollBuffer);
        OTHER_WINDOW_SCROLL_DEFAULT.initForwardTo(otherWindowScrollDefault);
        AUTO_WINDOW_VSCROLL.initForwardTo(autoWindowVscrollP);
        NEXT_SCREEN_CONTEXT_LINES.initForwardTo(nextScreenContextLines);
        SCROLL_PRESERVE_SCREEN_POSITION.initForwardTo(scrollPreserveScreenPosition);
        WINDOW_POINT_INSERTION_TYPE.initForwardTo(windowPointInsertionType);
        WINDOW_BUFFER_CHANGE_FUNCTIONS.initForwardTo(windowBufferChangeFunctions);
        WINDOW_SIZE_CHANGE_FUNCTIONS.initForwardTo(windowSizeChangeFunctions);
        WINDOW_SELECTION_CHANGE_FUNCTIONS.initForwardTo(windowSelectionChangeFunctions);
        WINDOW_STATE_CHANGE_FUNCTIONS.initForwardTo(windowStateChangeFunctions);
        WINDOW_STATE_CHANGE_HOOK.initForwardTo(windowStateChangeHook);
        WINDOW_CONFIGURATION_CHANGE_HOOK.initForwardTo(windowConfigurationChangeHook);
        WINDOW_RESTORE_KILLED_BUFFER_WINDOWS.initForwardTo(windowRestoreKilledBufferWindows);
        RECENTER_REDISPLAY.initForwardTo(recenterRedisplay);
        WINDOW_COMBINATION_RESIZE.initForwardTo(windowCombinationResize);
        WINDOW_COMBINATION_LIMIT.initForwardTo(windowCombinationLimit);
        WINDOW_PERSISTENT_PARAMETERS.initForwardTo(windowPersistentParameters);
        WINDOW_RESIZE_PIXELWISE.initForwardTo(windowResizePixelwise);
        FAST_BUT_IMPRECISE_SCROLLING.initForwardTo(fastButImpreciseScrolling);
        WINDOW_DEAD_WINDOWS_TABLE.initForwardTo(windowDeadWindowsTable);
    }
    private static void windowPostInitVars() {
        SCROLL_UP.putProperty(SCROLL_COMMAND, T);
        SCROLL_DOWN.putProperty(SCROLL_COMMAND, T);
        var windowPersistentParametersJInit = new ELispCons(new ELispCons(CLONE_OF, T));
        windowPersistentParameters.setValue(windowPersistentParametersJInit);
        var windowDeadWindowsTableJInit = FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, T});
        windowDeadWindowsTable.setValue(windowDeadWindowsTableJInit);
    }
    //#endregion window.c
    //#region frame.c
    public static final ELispSymbol.Value.Forwarded xResourceName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded xResourceClass = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded frameAlphaLowerLimit = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded defaultFrameAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded defaultFrameScrollBars = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool scrollBarAdjustThumbPortionP = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded terminalFrame = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded mousePositionFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded mouseHighlight = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded makePointerInvisible = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded moveFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded deleteFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded afterDeleteFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded menuBarMode = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded tabBarMode = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded toolBarMode = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded defaultMinibufferFrame = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded resizeMiniFrames = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded focusFollowsMouse = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool frameResizePixelwise = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded frameInhibitImpliedResize = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded frameSizeHistory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool tooltipReuseHiddenFrame = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool useSystemTooltips = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded iconifyChildFrame = new ELispSymbol.Value.Forwarded((Object) ICONIFY_TOP_LEVEL);
    public static final ELispSymbol.Value.Forwarded frameInternalParameters = new ELispSymbol.Value.Forwarded(false);

    private static void frameVars() {
        X_RESOURCE_NAME.initForwardTo(xResourceName);
        X_RESOURCE_CLASS.initForwardTo(xResourceClass);
        FRAME_ALPHA_LOWER_LIMIT.initForwardTo(frameAlphaLowerLimit);
        DEFAULT_FRAME_ALIST.initForwardTo(defaultFrameAlist);
        DEFAULT_FRAME_SCROLL_BARS.initForwardTo(defaultFrameScrollBars);
        SCROLL_BAR_ADJUST_THUMB_PORTION.initForwardTo(scrollBarAdjustThumbPortionP);
        TERMINAL_FRAME.initForwardTo(terminalFrame);
        MOUSE_POSITION_FUNCTION.initForwardTo(mousePositionFunction);
        MOUSE_HIGHLIGHT.initForwardTo(mouseHighlight);
        MAKE_POINTER_INVISIBLE.initForwardTo(makePointerInvisible);
        MOVE_FRAME_FUNCTIONS.initForwardTo(moveFrameFunctions);
        DELETE_FRAME_FUNCTIONS.initForwardTo(deleteFrameFunctions);
        AFTER_DELETE_FRAME_FUNCTIONS.initForwardTo(afterDeleteFrameFunctions);
        MENU_BAR_MODE.initForwardTo(menuBarMode);
        TAB_BAR_MODE.initForwardTo(tabBarMode);
        TOOL_BAR_MODE.initForwardTo(toolBarMode);
        DEFAULT_MINIBUFFER_FRAME.initForwardTo(defaultMinibufferFrame);
        RESIZE_MINI_FRAMES.initForwardTo(resizeMiniFrames);
        FOCUS_FOLLOWS_MOUSE.initForwardTo(focusFollowsMouse);
        FRAME_RESIZE_PIXELWISE.initForwardTo(frameResizePixelwise);
        FRAME_INHIBIT_IMPLIED_RESIZE.initForwardTo(frameInhibitImpliedResize);
        FRAME_SIZE_HISTORY.initForwardTo(frameSizeHistory);
        TOOLTIP_REUSE_HIDDEN_FRAME.initForwardTo(tooltipReuseHiddenFrame);
        USE_SYSTEM_TOOLTIPS.initForwardTo(useSystemTooltips);
        ICONIFY_CHILD_FRAME.initForwardTo(iconifyChildFrame);
        FRAME_INTERNAL_PARAMETERS.initForwardTo(frameInternalParameters);
    }
    private static void framePostInitVars() {
        FPut.put(AUTO_RAISE, X_FRAME_PARAMETER, (long) 0);
        FPut.put(AUTO_LOWER, X_FRAME_PARAMETER, (long) 1);
        FPut.put(BACKGROUND_COLOR, X_FRAME_PARAMETER, (long) 2);
        FPut.put(BORDER_COLOR, X_FRAME_PARAMETER, (long) 3);
        FPut.put(BORDER_WIDTH, X_FRAME_PARAMETER, (long) 4);
        FPut.put(CURSOR_COLOR, X_FRAME_PARAMETER, (long) 5);
        FPut.put(CURSOR_TYPE, X_FRAME_PARAMETER, (long) 6);
        FPut.put(FONT, X_FRAME_PARAMETER, (long) 7);
        FPut.put(FOREGROUND_COLOR, X_FRAME_PARAMETER, (long) 8);
        FPut.put(ICON_NAME, X_FRAME_PARAMETER, (long) 9);
        FPut.put(ICON_TYPE, X_FRAME_PARAMETER, (long) 10);
        FPut.put(CHILD_FRAME_BORDER_WIDTH, X_FRAME_PARAMETER, (long) 11);
        FPut.put(INTERNAL_BORDER_WIDTH, X_FRAME_PARAMETER, (long) 12);
        FPut.put(RIGHT_DIVIDER_WIDTH, X_FRAME_PARAMETER, (long) 13);
        FPut.put(BOTTOM_DIVIDER_WIDTH, X_FRAME_PARAMETER, (long) 14);
        FPut.put(MENU_BAR_LINES, X_FRAME_PARAMETER, (long) 15);
        FPut.put(MOUSE_COLOR, X_FRAME_PARAMETER, (long) 16);
        FPut.put(NAME, X_FRAME_PARAMETER, (long) 17);
        FPut.put(SCROLL_BAR_WIDTH, X_FRAME_PARAMETER, (long) 18);
        FPut.put(SCROLL_BAR_HEIGHT, X_FRAME_PARAMETER, (long) 19);
        FPut.put(TITLE, X_FRAME_PARAMETER, (long) 20);
        FPut.put(UNSPLITTABLE, X_FRAME_PARAMETER, (long) 21);
        FPut.put(VERTICAL_SCROLL_BARS, X_FRAME_PARAMETER, (long) 22);
        FPut.put(HORIZONTAL_SCROLL_BARS, X_FRAME_PARAMETER, (long) 23);
        FPut.put(VISIBILITY, X_FRAME_PARAMETER, (long) 24);
        FPut.put(TAB_BAR_LINES, X_FRAME_PARAMETER, (long) 25);
        FPut.put(TOOL_BAR_LINES, X_FRAME_PARAMETER, (long) 26);
        FPut.put(SCROLL_BAR_FOREGROUND, X_FRAME_PARAMETER, (long) 27);
        FPut.put(SCROLL_BAR_BACKGROUND, X_FRAME_PARAMETER, (long) 28);
        FPut.put(SCREEN_GAMMA, X_FRAME_PARAMETER, (long) 29);
        FPut.put(LINE_SPACING, X_FRAME_PARAMETER, (long) 30);
        FPut.put(LEFT_FRINGE, X_FRAME_PARAMETER, (long) 31);
        FPut.put(RIGHT_FRINGE, X_FRAME_PARAMETER, (long) 32);
        FPut.put(WAIT_FOR_WM, X_FRAME_PARAMETER, (long) 33);
        FPut.put(FULLSCREEN, X_FRAME_PARAMETER, (long) 34);
        FPut.put(FONT_BACKEND, X_FRAME_PARAMETER, (long) 35);
        FPut.put(ALPHA, X_FRAME_PARAMETER, (long) 36);
        FPut.put(STICKY, X_FRAME_PARAMETER, (long) 37);
        FPut.put(TOOL_BAR_POSITION, X_FRAME_PARAMETER, (long) 38);
        FPut.put(INHIBIT_DOUBLE_BUFFERING, X_FRAME_PARAMETER, (long) 39);
        FPut.put(UNDECORATED, X_FRAME_PARAMETER, (long) 40);
        FPut.put(PARENT_FRAME, X_FRAME_PARAMETER, (long) 41);
        FPut.put(SKIP_TASKBAR, X_FRAME_PARAMETER, (long) 42);
        FPut.put(NO_FOCUS_ON_MAP, X_FRAME_PARAMETER, (long) 43);
        FPut.put(NO_ACCEPT_FOCUS, X_FRAME_PARAMETER, (long) 44);
        FPut.put(Z_GROUP, X_FRAME_PARAMETER, (long) 45);
        FPut.put(OVERRIDE_REDIRECT, X_FRAME_PARAMETER, (long) 46);
        FPut.put(NO_SPECIAL_GLYPHS, X_FRAME_PARAMETER, (long) 47);
        FPut.put(ALPHA_BACKGROUND, X_FRAME_PARAMETER, (long) 48);
        FPut.put(USE_FRAME_SYNCHRONIZATION, X_FRAME_PARAMETER, (long) 49);
        var frameInternalParametersJInit = ELispCons.listOf(NAME, PARENT_ID, WINDOW_ID);
        frameInternalParameters.setValue(frameInternalParametersJInit);
    }
    //#endregion frame.c
    //#region textprop.c
    public static final ELispSymbol.Value.Forwarded defaultTextProperties = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded charPropertyAliasAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded inhibitPointMotionHooks = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded textPropertyDefaultNonsticky = new ELispSymbol.Value.Forwarded(false);

    private static void textpropVars() {
        DEFAULT_TEXT_PROPERTIES.initForwardTo(defaultTextProperties);
        CHAR_PROPERTY_ALIAS_ALIST.initForwardTo(charPropertyAliasAlist);
        INHIBIT_POINT_MOTION_HOOKS.initForwardTo(inhibitPointMotionHooks);
        TEXT_PROPERTY_DEFAULT_NONSTICKY.initForwardTo(textPropertyDefaultNonsticky);
    }
    private static void textpropPostInitVars() {
        var textPropertyDefaultNonstickyJInit = ELispCons.listOf(new ELispCons(SYNTAX_TABLE, T), new ELispCons(DISPLAY, T));
        textPropertyDefaultNonsticky.setValue(textPropertyDefaultNonstickyJInit);
    }
    //#endregion textprop.c
    //#region syntax.c
    public static final ELispSymbol.Value.Forwarded commentUseSyntaxPpss = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool parseSexpIgnoreComments = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool parseSexpLookupProperties = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedLong syntaxPropertizeDone = new ELispSymbol.Value.ForwardedLong((long) -1);
    public static final ELispSymbol.Value.ForwardedBool wordsIncludeEscapes = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool multibyteSyntaxAsSymbol = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool openParenInColumn0IsDefunStart = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded findWordBoundaryFunctionTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool commentEndCanBeEscaped = new ELispSymbol.Value.ForwardedBool(false);

    private static void syntaxVars() {
        COMMENT_USE_SYNTAX_PPSS.initForwardTo(commentUseSyntaxPpss);
        PARSE_SEXP_IGNORE_COMMENTS.initForwardTo(parseSexpIgnoreComments);
        PARSE_SEXP_LOOKUP_PROPERTIES.initForwardTo(parseSexpLookupProperties);
        SYNTAX_PROPERTIZE__DONE.initForwardTo(syntaxPropertizeDone);
        WORDS_INCLUDE_ESCAPES.initForwardTo(wordsIncludeEscapes);
        MULTIBYTE_SYNTAX_AS_SYMBOL.initForwardTo(multibyteSyntaxAsSymbol);
        OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START.initForwardTo(openParenInColumn0IsDefunStart);
        FIND_WORD_BOUNDARY_FUNCTION_TABLE.initForwardTo(findWordBoundaryFunctionTable);
        COMMENT_END_CAN_BE_ESCAPED.initForwardTo(commentEndCanBeEscaped);
    }
    private static void syntaxPostInitVars() {
        SCAN_ERROR.putProperty(ERROR_CONDITIONS, ELispCons.listOf(SCAN_ERROR, ERROR));
        SCAN_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Scan error"));
        ELispContext.intern("syntax-propertize--done").setBufferLocal(true);
        var findWordBoundaryFunctionTableJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        findWordBoundaryFunctionTable.setValue(findWordBoundaryFunctionTableJInit);
        COMMENT_END_CAN_BE_ESCAPED.setBufferLocal(true);
    }
    //#endregion syntax.c
    //#region xdisp.c
    public static final ELispSymbol.Value.ForwardedBool scrollMinibufferConservatively = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool inhibitMessage = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded messagesBufferName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool xStretchCursorP = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded showTrailingWhitespace = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded modeLineCompact = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded nobreakCharDisplay = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool nobreakCharAsciiDisplay = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded voidTextAreaPointer = new ELispSymbol.Value.Forwarded((Object) ARROW);
    public static final ELispSymbol.Value.Forwarded inhibitRedisplay = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded globalModeString = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded overlayArrowPosition = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded overlayArrowString = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded overlayArrowVariableList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong emacsScrollStep = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong scrollConservatively = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong scrollMargin = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded maximumScrollMargin = new ELispSymbol.Value.Forwarded((Object) 0.25);
    public static final ELispSymbol.Value.Forwarded displayPixelsPerInch = new ELispSymbol.Value.Forwarded((Object) 72.0);
    public static final ELispSymbol.Value.ForwardedLong debugEndPos = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded truncatePartialWidthWindows = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool wordWrapByCategory = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded lineNumberDisplayLimit = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong lineNumberDisplayLimitWidth = new ELispSymbol.Value.ForwardedLong((long) 200);
    public static final ELispSymbol.Value.ForwardedBool highlightNonselectedWindows = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool multipleFrames = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded frameTitleFormat = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded iconTitleFormat = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded messageLogMax = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded windowScrollFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded mouseAutoselectWindow = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded autoResizeTabBars = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool autoRaiseTabBarButtonsP = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded autoResizeToolBars = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool autoRaiseToolBarButtonsP = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded makeCursorLineFullyVisible = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool makeWindowStartVisible = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded tabBarBorder = new ELispSymbol.Value.Forwarded((Object) INTERNAL_BORDER_WIDTH);
    public static final ELispSymbol.Value.Forwarded tabBarButtonMargin = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong tabBarButtonRelief = new ELispSymbol.Value.ForwardedLong((long) 1);
    public static final ELispSymbol.Value.Forwarded toolBarBorder = new ELispSymbol.Value.Forwarded((Object) INTERNAL_BORDER_WIDTH);
    public static final ELispSymbol.Value.Forwarded toolBarButtonMargin = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong toolBarButtonRelief = new ELispSymbol.Value.ForwardedLong((long) 1);
    public static final ELispSymbol.Value.Forwarded toolBarStyle = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong toolBarMaxLabelSize = new ELispSymbol.Value.ForwardedLong((long) 14);
    public static final ELispSymbol.Value.Forwarded fontificationFunctions = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool unibyteDisplayViaLanguageEnvironment = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded maxMiniWindowHeight = new ELispSymbol.Value.Forwarded((Object) 0.25);
    public static final ELispSymbol.Value.Forwarded resizeMiniWindows = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded blinkCursorAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded automaticHscrolling = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedLong hscrollMargin = new ELispSymbol.Value.ForwardedLong((long) 5);
    public static final ELispSymbol.Value.Forwarded hscrollStep = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool messageTruncateLines = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded menuBarUpdateHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded menuUpdatingFrame = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitMenubarUpdate = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded wrapPrefix = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded linePrefix = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded displayLineNumbers = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded displayLineNumbersWidth = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded displayLineNumbersCurrentAbsolute = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool displayLineNumbersWiden = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedLong displayLineNumbersOffset = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedBool displayFillColumnIndicator = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded displayFillColumnIndicatorColumn = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.Forwarded displayFillColumnIndicatorCharacter = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedLong displayLineNumbersMajorTick = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedLong displayLineNumbersMinorTick = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.ForwardedBool inhibitEvalDuringRedisplay = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitFreeRealizedFaces = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitBidiMirroring = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool bidiInhibitBpa = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitTryWindowId = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitTryWindowReusing = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitTryCursorMovement = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedLong overlineMargin = new ELispSymbol.Value.ForwardedLong((long) 2);
    public static final ELispSymbol.Value.ForwardedLong underlineMinimumOffset = new ELispSymbol.Value.ForwardedLong((long) 1);
    public static final ELispSymbol.Value.ForwardedBool displayHourglassP = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.Forwarded hourglassDelay = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded preRedisplayFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded glyphlessCharDisplay = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded debugOnMessage = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded setMessageFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded clearMessageFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded redisplayAllWindowsCause = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded redisplayModeLinesCause = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool redisplayInhibitBidi = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool displayRawBytesAsHex = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool mouseFineGrainedTracking = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool tabBarDraggingInProgress = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool redisplaySkipInitialFrame = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool redisplaySkipFontificationOnInput = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool redisplayAdhocScrollInResizeMiniWindows = new ELispSymbol.Value.ForwardedBool(true);
    public static final ELispSymbol.Value.ForwardedBool compositionBreakAtPoint = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedLong maxRedisplayTicks = new ELispSymbol.Value.ForwardedLong((long) 0);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded withEchoAreaSaveVector = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded messageStack = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded modeLineUnwindVector = new ELispSymbol.Value.Forwarded(false);

    private static void xdispVars() {
        SCROLL_MINIBUFFER_CONSERVATIVELY.initForwardTo(scrollMinibufferConservatively);
        INHIBIT_MESSAGE.initForwardTo(inhibitMessage);
        MESSAGES_BUFFER_NAME.initForwardTo(messagesBufferName);
        X_STRETCH_CURSOR.initForwardTo(xStretchCursorP);
        SHOW_TRAILING_WHITESPACE.initForwardTo(showTrailingWhitespace);
        MODE_LINE_COMPACT.initForwardTo(modeLineCompact);
        NOBREAK_CHAR_DISPLAY.initForwardTo(nobreakCharDisplay);
        NOBREAK_CHAR_ASCII_DISPLAY.initForwardTo(nobreakCharAsciiDisplay);
        VOID_TEXT_AREA_POINTER.initForwardTo(voidTextAreaPointer);
        INHIBIT_REDISPLAY.initForwardTo(inhibitRedisplay);
        GLOBAL_MODE_STRING.initForwardTo(globalModeString);
        OVERLAY_ARROW_POSITION.initForwardTo(overlayArrowPosition);
        OVERLAY_ARROW_STRING.initForwardTo(overlayArrowString);
        OVERLAY_ARROW_VARIABLE_LIST.initForwardTo(overlayArrowVariableList);
        SCROLL_STEP.initForwardTo(emacsScrollStep);
        SCROLL_CONSERVATIVELY.initForwardTo(scrollConservatively);
        SCROLL_MARGIN.initForwardTo(scrollMargin);
        MAXIMUM_SCROLL_MARGIN.initForwardTo(maximumScrollMargin);
        DISPLAY_PIXELS_PER_INCH.initForwardTo(displayPixelsPerInch);
        DEBUG_END_POS.initForwardTo(debugEndPos);
        TRUNCATE_PARTIAL_WIDTH_WINDOWS.initForwardTo(truncatePartialWidthWindows);
        WORD_WRAP_BY_CATEGORY.initForwardTo(wordWrapByCategory);
        LINE_NUMBER_DISPLAY_LIMIT.initForwardTo(lineNumberDisplayLimit);
        LINE_NUMBER_DISPLAY_LIMIT_WIDTH.initForwardTo(lineNumberDisplayLimitWidth);
        HIGHLIGHT_NONSELECTED_WINDOWS.initForwardTo(highlightNonselectedWindows);
        MULTIPLE_FRAMES.initForwardTo(multipleFrames);
        FRAME_TITLE_FORMAT.initForwardTo(frameTitleFormat);
        ICON_TITLE_FORMAT.initForwardTo(iconTitleFormat);
        MESSAGE_LOG_MAX.initForwardTo(messageLogMax);
        WINDOW_SCROLL_FUNCTIONS.initForwardTo(windowScrollFunctions);
        MOUSE_AUTOSELECT_WINDOW.initForwardTo(mouseAutoselectWindow);
        AUTO_RESIZE_TAB_BARS.initForwardTo(autoResizeTabBars);
        AUTO_RAISE_TAB_BAR_BUTTONS.initForwardTo(autoRaiseTabBarButtonsP);
        AUTO_RESIZE_TOOL_BARS.initForwardTo(autoResizeToolBars);
        AUTO_RAISE_TOOL_BAR_BUTTONS.initForwardTo(autoRaiseToolBarButtonsP);
        MAKE_CURSOR_LINE_FULLY_VISIBLE.initForwardTo(makeCursorLineFullyVisible);
        MAKE_WINDOW_START_VISIBLE.initForwardTo(makeWindowStartVisible);
        TAB_BAR_BORDER.initForwardTo(tabBarBorder);
        TAB_BAR_BUTTON_MARGIN.initForwardTo(tabBarButtonMargin);
        TAB_BAR_BUTTON_RELIEF.initForwardTo(tabBarButtonRelief);
        TOOL_BAR_BORDER.initForwardTo(toolBarBorder);
        TOOL_BAR_BUTTON_MARGIN.initForwardTo(toolBarButtonMargin);
        TOOL_BAR_BUTTON_RELIEF.initForwardTo(toolBarButtonRelief);
        TOOL_BAR_STYLE.initForwardTo(toolBarStyle);
        TOOL_BAR_MAX_LABEL_SIZE.initForwardTo(toolBarMaxLabelSize);
        FONTIFICATION_FUNCTIONS.initForwardTo(fontificationFunctions);
        UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT.initForwardTo(unibyteDisplayViaLanguageEnvironment);
        MAX_MINI_WINDOW_HEIGHT.initForwardTo(maxMiniWindowHeight);
        RESIZE_MINI_WINDOWS.initForwardTo(resizeMiniWindows);
        BLINK_CURSOR_ALIST.initForwardTo(blinkCursorAlist);
        AUTO_HSCROLL_MODE.initForwardTo(automaticHscrolling);
        HSCROLL_MARGIN.initForwardTo(hscrollMargin);
        HSCROLL_STEP.initForwardTo(hscrollStep);
        MESSAGE_TRUNCATE_LINES.initForwardTo(messageTruncateLines);
        MENU_BAR_UPDATE_HOOK.initForwardTo(menuBarUpdateHook);
        MENU_UPDATING_FRAME.initForwardTo(menuUpdatingFrame);
        INHIBIT_MENUBAR_UPDATE.initForwardTo(inhibitMenubarUpdate);
        WRAP_PREFIX.initForwardTo(wrapPrefix);
        LINE_PREFIX.initForwardTo(linePrefix);
        DISPLAY_LINE_NUMBERS.initForwardTo(displayLineNumbers);
        DISPLAY_LINE_NUMBERS_WIDTH.initForwardTo(displayLineNumbersWidth);
        DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE.initForwardTo(displayLineNumbersCurrentAbsolute);
        DISPLAY_LINE_NUMBERS_WIDEN.initForwardTo(displayLineNumbersWiden);
        DISPLAY_LINE_NUMBERS_OFFSET.initForwardTo(displayLineNumbersOffset);
        DISPLAY_FILL_COLUMN_INDICATOR.initForwardTo(displayFillColumnIndicator);
        DISPLAY_FILL_COLUMN_INDICATOR_COLUMN.initForwardTo(displayFillColumnIndicatorColumn);
        DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER.initForwardTo(displayFillColumnIndicatorCharacter);
        DISPLAY_LINE_NUMBERS_MAJOR_TICK.initForwardTo(displayLineNumbersMajorTick);
        DISPLAY_LINE_NUMBERS_MINOR_TICK.initForwardTo(displayLineNumbersMinorTick);
        INHIBIT_EVAL_DURING_REDISPLAY.initForwardTo(inhibitEvalDuringRedisplay);
        INHIBIT_FREE_REALIZED_FACES.initForwardTo(inhibitFreeRealizedFaces);
        INHIBIT_BIDI_MIRRORING.initForwardTo(inhibitBidiMirroring);
        BIDI_INHIBIT_BPA.initForwardTo(bidiInhibitBpa);
        INHIBIT_TRY_WINDOW_ID.initForwardTo(inhibitTryWindowId);
        INHIBIT_TRY_WINDOW_REUSING.initForwardTo(inhibitTryWindowReusing);
        INHIBIT_TRY_CURSOR_MOVEMENT.initForwardTo(inhibitTryCursorMovement);
        OVERLINE_MARGIN.initForwardTo(overlineMargin);
        UNDERLINE_MINIMUM_OFFSET.initForwardTo(underlineMinimumOffset);
        DISPLAY_HOURGLASS.initForwardTo(displayHourglassP);
        HOURGLASS_DELAY.initForwardTo(hourglassDelay);
        PRE_REDISPLAY_FUNCTION.initForwardTo(preRedisplayFunction);
        GLYPHLESS_CHAR_DISPLAY.initForwardTo(glyphlessCharDisplay);
        DEBUG_ON_MESSAGE.initForwardTo(debugOnMessage);
        SET_MESSAGE_FUNCTION.initForwardTo(setMessageFunction);
        CLEAR_MESSAGE_FUNCTION.initForwardTo(clearMessageFunction);
        REDISPLAY__ALL_WINDOWS_CAUSE.initForwardTo(redisplayAllWindowsCause);
        REDISPLAY__MODE_LINES_CAUSE.initForwardTo(redisplayModeLinesCause);
        REDISPLAY__INHIBIT_BIDI.initForwardTo(redisplayInhibitBidi);
        DISPLAY_RAW_BYTES_AS_HEX.initForwardTo(displayRawBytesAsHex);
        MOUSE_FINE_GRAINED_TRACKING.initForwardTo(mouseFineGrainedTracking);
        TAB_BAR__DRAGGING_IN_PROGRESS.initForwardTo(tabBarDraggingInProgress);
        REDISPLAY_SKIP_INITIAL_FRAME.initForwardTo(redisplaySkipInitialFrame);
        REDISPLAY_SKIP_FONTIFICATION_ON_INPUT.initForwardTo(redisplaySkipFontificationOnInput);
        REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS.initForwardTo(redisplayAdhocScrollInResizeMiniWindows);
        COMPOSITION_BREAK_AT_POINT.initForwardTo(compositionBreakAtPoint);
        MAX_REDISPLAY_TICKS.initForwardTo(maxRedisplayTicks);
    }
    private static void xdispPostInitVars() {
        var messagesBufferNameJInit = new ELispString("*Messages*");
        messagesBufferName.setValue(messagesBufferNameJInit);
        var overlayArrowStringJInit = new ELispString("=>");
        overlayArrowString.setValue(overlayArrowStringJInit);
        var overlayArrowVariableListJInit = new ELispCons(ELispContext.intern("overlay-arrow-position"));
        overlayArrowVariableList.setValue(overlayArrowVariableListJInit);
        var truncatePartialWidthWindowsJInit = (long) (50);
        truncatePartialWidthWindows.setValue(truncatePartialWidthWindowsJInit);
        var iconTitleFormatJInit = ELispCons.listOf(ELispContext.intern("multiple-frames"), new ELispString("%b"), ELispCons.listOf(new ELispString(""), new ELispString("%b - GNU Emacs at "), ELispContext.intern("system-name")));
        iconTitleFormat.setValue(iconTitleFormatJInit);
        var frameTitleFormatJInit = ELispCons.listOf(ELispContext.intern("multiple-frames"), new ELispString("%b"), ELispCons.listOf(new ELispString(""), new ELispString("%b - GNU Emacs at "), ELispContext.intern("system-name")));
        frameTitleFormat.setValue(frameTitleFormatJInit);
        var messageLogMaxJInit = (long) (1000);
        messageLogMax.setValue(messageLogMaxJInit);
        MAKE_WINDOW_START_VISIBLE.setBufferLocal(true);
        var tabBarButtonMarginJInit = (long) (1);
        tabBarButtonMargin.setValue(tabBarButtonMarginJInit);
        var toolBarButtonMarginJInit = (long) (4);
        toolBarButtonMargin.setValue(toolBarButtonMarginJInit);
        FONTIFICATION_FUNCTIONS.setBufferLocal(true);
        var hscrollStepJInit = (long) (0);
        hscrollStep.setValue(hscrollStepJInit);
        WRAP_PREFIX.setBufferLocal(true);
        LINE_PREFIX.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS_WIDTH.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS_WIDEN.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS_OFFSET.setBufferLocal(true);
        DISPLAY_FILL_COLUMN_INDICATOR.setBufferLocal(true);
        DISPLAY_FILL_COLUMN_INDICATOR_COLUMN.setBufferLocal(true);
        DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER.setBufferLocal(true);
        var hourglassDelayJInit = (long) (1);
        hourglassDelay.setValue(hourglassDelayJInit);
        var preRedisplayFunctionJInit = ELispContext.intern("ignore");
        preRedisplayFunction.setValue(preRedisplayFunctionJInit);
        GLYPHLESS_CHAR_DISPLAY.putProperty(CHAR_TABLE_EXTRA_SLOTS, (long) (1));
        var glyphlessCharDisplayJInit = FMakeCharTable.makeCharTable(GLYPHLESS_CHAR_DISPLAY, NIL);
        glyphlessCharDisplay.setValue(glyphlessCharDisplayJInit);
        glyphlessCharDisplayJInit.setExtra((int) (long) (0), EMPTY_BOX);
        var redisplayAllWindowsCauseJInit = FMakeHashTable.makeHashTable(new Object[]{});
        redisplayAllWindowsCause.setValue(redisplayAllWindowsCauseJInit);
        var redisplayModeLinesCauseJInit = FMakeHashTable.makeHashTable(new Object[]{});
        redisplayModeLinesCause.setValue(redisplayModeLinesCauseJInit);
    }
    //#endregion xdisp.c
    //#region category.c
    public static final ELispSymbol.Value.Forwarded wordCombiningCategories = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded wordSeparatingCategories = new ELispSymbol.Value.Forwarded(false);

    private static void categoryVars() {
        WORD_COMBINING_CATEGORIES.initForwardTo(wordCombiningCategories);
        WORD_SEPARATING_CATEGORIES.initForwardTo(wordSeparatingCategories);
    }
    private static void categoryPostInitVars() {

    }
    //#endregion category.c
    //#region doc.c
    public static final ELispSymbol.Value.Forwarded docFileName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded buildFiles = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded textQuotingStyle = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool textQuotingFlag = new ELispSymbol.Value.ForwardedBool(false);

    private static void docVars() {
        INTERNAL_DOC_FILE_NAME.initForwardTo(docFileName);
        BUILD_FILES.initForwardTo(buildFiles);
        TEXT_QUOTING_STYLE.initForwardTo(textQuotingStyle);
        INTERNAL__TEXT_QUOTING_FLAG.initForwardTo(textQuotingFlag);
    }
    private static void docPostInitVars() {

    }
    //#endregion doc.c
    //#region minibuf.c
    public static final ELispSymbol.Value.Forwarded readExpressionHistory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded readBufferFunction = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferFollowsSelectedFrame = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool readBufferCompletionIgnoreCase = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded minibufferSetupHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferExitHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded historyLength = new ELispSymbol.Value.Forwarded((Object) 100);
    public static final ELispSymbol.Value.ForwardedBool historyDeleteDuplicates = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded historyAddNewInput = new ELispSymbol.Value.Forwarded((Object) T);
    public static final ELispSymbol.Value.ForwardedBool completionIgnoreCase = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool enableRecursiveMinibuffers = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded minibufferCompletionTable = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferCompletionPredicate = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferCompletionConfirm = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferCompletingFileName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferHelpForm = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded minibufferHistoryVariable = new ELispSymbol.Value.Forwarded((Object) 0);
    public static final ELispSymbol.Value.Forwarded minibufferHistoryPosition = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool minibufferAutoRaise = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded completionRegexpList = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool minibufferAllowTextProperties = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.Forwarded minibufferPromptProperties = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded readHideChar = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.ForwardedBool inhibitInteraction = new ELispSymbol.Value.ForwardedBool(false);
    public static final ELispSymbol.Value.ForwardedBool readMinibufferRestoreWindows = new ELispSymbol.Value.ForwardedBool(true);

    private static void minibufVars() {
        READ_EXPRESSION_HISTORY.initForwardTo(readExpressionHistory);
        READ_BUFFER_FUNCTION.initForwardTo(readBufferFunction);
        MINIBUFFER_FOLLOWS_SELECTED_FRAME.initForwardTo(minibufferFollowsSelectedFrame);
        READ_BUFFER_COMPLETION_IGNORE_CASE.initForwardTo(readBufferCompletionIgnoreCase);
        MINIBUFFER_SETUP_HOOK.initForwardTo(minibufferSetupHook);
        MINIBUFFER_EXIT_HOOK.initForwardTo(minibufferExitHook);
        HISTORY_LENGTH.initForwardTo(historyLength);
        HISTORY_DELETE_DUPLICATES.initForwardTo(historyDeleteDuplicates);
        HISTORY_ADD_NEW_INPUT.initForwardTo(historyAddNewInput);
        COMPLETION_IGNORE_CASE.initForwardTo(completionIgnoreCase);
        ENABLE_RECURSIVE_MINIBUFFERS.initForwardTo(enableRecursiveMinibuffers);
        MINIBUFFER_COMPLETION_TABLE.initForwardTo(minibufferCompletionTable);
        MINIBUFFER_COMPLETION_PREDICATE.initForwardTo(minibufferCompletionPredicate);
        MINIBUFFER_COMPLETION_CONFIRM.initForwardTo(minibufferCompletionConfirm);
        MINIBUFFER_COMPLETING_FILE_NAME.initForwardTo(minibufferCompletingFileName);
        MINIBUFFER_HELP_FORM.initForwardTo(minibufferHelpForm);
        MINIBUFFER_HISTORY_VARIABLE.initForwardTo(minibufferHistoryVariable);
        MINIBUFFER_HISTORY_POSITION.initForwardTo(minibufferHistoryPosition);
        MINIBUFFER_AUTO_RAISE.initForwardTo(minibufferAutoRaise);
        COMPLETION_REGEXP_LIST.initForwardTo(completionRegexpList);
        MINIBUFFER_ALLOW_TEXT_PROPERTIES.initForwardTo(minibufferAllowTextProperties);
        MINIBUFFER_PROMPT_PROPERTIES.initForwardTo(minibufferPromptProperties);
        READ_HIDE_CHAR.initForwardTo(readHideChar);
        INHIBIT_INTERACTION.initForwardTo(inhibitInteraction);
        READ_MINIBUFFER_RESTORE_WINDOWS.initForwardTo(readMinibufferRestoreWindows);
    }
    private static void minibufPostInitVars() {
        MINIBUFFER_DEFAULT.setValue(NIL);
        CUSTOM_VARIABLE_HISTORY.setValue(NIL);
        BUFFER_NAME_HISTORY.setValue(NIL);
        var minibufferPromptPropertiesJInit = ELispCons.listOf(READ_ONLY, T);
        minibufferPromptProperties.setValue(minibufferPromptPropertiesJInit);
    }
    //#endregion minibuf.c
    //#region callproc.c
    public static final ELispSymbol.Value.Forwarded shellFileName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded execPath = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded execSuffixes = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded execDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded dataDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded docDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded configureInfoDirectory = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded sharedGameScoreDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.Forwarded initialEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded processEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded ctagsProgramName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded etagsProgramName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded hexlProgramName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded emacsclientProgramName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded movemailProgramName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded ebrowseProgramName = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded rcs2logProgramName = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded tempFileNamePattern = new ELispSymbol.Value.Forwarded(false);

    private static void callprocVars() {
        SHELL_FILE_NAME.initForwardTo(shellFileName);
        EXEC_PATH.initForwardTo(execPath);
        EXEC_SUFFIXES.initForwardTo(execSuffixes);
        EXEC_DIRECTORY.initForwardTo(execDirectory);
        DATA_DIRECTORY.initForwardTo(dataDirectory);
        DOC_DIRECTORY.initForwardTo(docDirectory);
        CONFIGURE_INFO_DIRECTORY.initForwardTo(configureInfoDirectory);
        SHARED_GAME_SCORE_DIRECTORY.initForwardTo(sharedGameScoreDirectory);
        INITIAL_ENVIRONMENT.initForwardTo(initialEnvironment);
        PROCESS_ENVIRONMENT.initForwardTo(processEnvironment);
        CTAGS_PROGRAM_NAME.initForwardTo(ctagsProgramName);
        ETAGS_PROGRAM_NAME.initForwardTo(etagsProgramName);
        HEXL_PROGRAM_NAME.initForwardTo(hexlProgramName);
        EMACSCLIENT_PROGRAM_NAME.initForwardTo(emacsclientProgramName);
        MOVEMAIL_PROGRAM_NAME.initForwardTo(movemailProgramName);
        EBROWSE_PROGRAM_NAME.initForwardTo(ebrowseProgramName);
        RCS2LOG_PROGRAM_NAME.initForwardTo(rcs2logProgramName);
    }
    private static void callprocPostInitVars() {
        var tempFileNamePatternJInit = new ELispString("emacsXXXXXX");
        tempFileNamePattern.setValue(tempFileNamePatternJInit);
        var configureInfoDirectoryJInit = new ELispString("/usr/share/info");
        configureInfoDirectory.setValue(configureInfoDirectoryJInit);
        var ctagsProgramNameJInit = new ELispString("ctags");
        ctagsProgramName.setValue(ctagsProgramNameJInit);
        var etagsProgramNameJInit = new ELispString("etags");
        etagsProgramName.setValue(etagsProgramNameJInit);
        var hexlProgramNameJInit = new ELispString("hexl");
        hexlProgramName.setValue(hexlProgramNameJInit);
        var emacsclientProgramNameJInit = new ELispString("emacsclient");
        emacsclientProgramName.setValue(emacsclientProgramNameJInit);
        var movemailProgramNameJInit = new ELispString("movemail");
        movemailProgramName.setValue(movemailProgramNameJInit);
        var ebrowseProgramNameJInit = new ELispString("ebrowse");
        ebrowseProgramName.setValue(ebrowseProgramNameJInit);
        var rcs2logProgramNameJInit = new ELispString("rcs2log");
        rcs2logProgramName.setValue(rcs2logProgramNameJInit);
    }
    //#endregion callproc.c
    //#region macros.c
    public static final ELispSymbol.Value.Forwarded kbdMacroTerminationHook = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded definingKbdMacro = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static final ELispSymbol.Value.Forwarded executingKbdMacro = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static final ELispSymbol.Value.ForwardedLong executingKbdMacroIndex = new ELispSymbol.Value.ForwardedLong((long) 0);
    public static final ELispSymbol.Value.Forwarded lastKbdMacro = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);

    private static void macrosVars() {
        KBD_MACRO_TERMINATION_HOOK.initForwardTo(kbdMacroTerminationHook);
        DEFINING_KBD_MACRO.initForwardTo(definingKbdMacro);
        EXECUTING_KBD_MACRO.initForwardTo(executingKbdMacro);
        EXECUTING_KBD_MACRO_INDEX.initForwardTo(executingKbdMacroIndex);
        LAST_KBD_MACRO.initForwardTo(lastKbdMacro);
    }
    private static void macrosPostInitVars() {

    }
    //#endregion macros.c
    //#region floatfns.c

    private static void floatfnsVars() {

    }
    private static void floatfnsPostInitVars() {

    }
    //#endregion floatfns.c
    //#region ccl.c
    public static final ELispSymbol.Value.Forwarded codeConversionMapVector = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded fontCclEncoderAlist = new ELispSymbol.Value.Forwarded(false);
    public static final ELispSymbol.Value.Forwarded translationHashTableVector = new ELispSymbol.Value.Forwarded(false);
    /// A local C variable not exported to any Lisp vars
    public static final ELispSymbol.Value.Forwarded cclProgramTable = new ELispSymbol.Value.Forwarded(false);

    private static void cclVars() {
        CODE_CONVERSION_MAP_VECTOR.initForwardTo(codeConversionMapVector);
        FONT_CCL_ENCODER_ALIST.initForwardTo(fontCclEncoderAlist);
        TRANSLATION_HASH_TABLE_VECTOR.initForwardTo(translationHashTableVector);
    }
    private static void cclPostInitVars() {
        var cclProgramTableJInit = new ELispVector(Collections.nCopies(32, false));
        cclProgramTable.setValue(cclProgramTableJInit);
        var codeConversionMapVectorJInit = new ELispVector(Collections.nCopies(16, false));
        codeConversionMapVector.setValue(codeConversionMapVectorJInit);
    }
    //#endregion ccl.c
}
