package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInKeymap.*;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispHashtable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCharSet.defineCharsetInternal;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

@SuppressWarnings({
        "PMD.UnnecessaryCast",
        "PMD.UseUnderscoresInNumericLiterals",
        "RedundantCast",
        "UnnecessaryUnicodeEscape"
})
public class ELispGlobals {
    public static void initGlobalVariables() {
        allocVars();
        bufferVars();
        callintVars();
        casetabVars();
        charsetVars();
        chartabVars();
        cmdsVars();
        compVars();
        dataVars();
        editfnsVars();
        emacsVars();
        evalVars();
        fileioVars();
        fnsVars();
        keyboardVars();
        keymapVars();
        lreadVars();
        printVars();
        processVars();
        searchVars();
        timefnsVars();
        xfacesVars();
    }

    public static void postInitVariables() {
        allocPostInitVars();
        bufferPostInitVars();
        callintPostInitVars();
        casetabPostInitVars();
        charsetPostInitVars();
        chartabPostInitVars();
        cmdsPostInitVars();
        compPostInitVars();
        dataPostInitVars();
        editfnsPostInitVars();
        emacsPostInitVars();
        evalPostInitVars();
        fnsPostInitVars();
        fileioPostInitVars();
        keyboardPostInitVars();
        keymapPostInitVars();
        lreadPostInitVars();
        printPostInitVars();
        processPostInitVars();
        searchPostInitVars();
        timefnsPostInitVars();
        xfacesPostInitVars();
    }

    /* @generated region="alloc.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded gcConsThreshold = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded gcConsPercentage = new ELispSymbol.Value.Forwarded((Object) 0.1);
    public static ELispSymbol.Value.Forwarded pureBytesUsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded consCellsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded floatsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded vectorCellsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded symbolsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded stringCharsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded intervalsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded stringsConsed = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded purifyFlag = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded garbageCollectionMessages = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded postGcHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded memorySignalData = new ELispSymbol.Value.Forwarded((Object) ELispCons.listOf(ERROR, new ELispString("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs")));
    public static ELispSymbol.Value.Forwarded memoryFull = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded gcElapsed = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded gcsDone = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded integerWidth = new ELispSymbol.Value.Forwarded((long) 0);

    private static void allocVars() {
        GC_CONS_THRESHOLD.forwardTo(gcConsThreshold);
        GC_CONS_PERCENTAGE.forwardTo(gcConsPercentage);
        PURE_BYTES_USED.forwardTo(pureBytesUsed);
        CONS_CELLS_CONSED.forwardTo(consCellsConsed);
        FLOATS_CONSED.forwardTo(floatsConsed);
        VECTOR_CELLS_CONSED.forwardTo(vectorCellsConsed);
        SYMBOLS_CONSED.forwardTo(symbolsConsed);
        STRING_CHARS_CONSED.forwardTo(stringCharsConsed);
        INTERVALS_CONSED.forwardTo(intervalsConsed);
        STRINGS_CONSED.forwardTo(stringsConsed);
        PURIFY_FLAG.forwardTo(purifyFlag);
        GARBAGE_COLLECTION_MESSAGES.forwardTo(garbageCollectionMessages);
        POST_GC_HOOK.forwardTo(postGcHook);
        MEMORY_SIGNAL_DATA.forwardTo(memorySignalData);
        MEMORY_FULL.forwardTo(memoryFull);
        GC_ELAPSED.forwardTo(gcElapsed);
        GCS_DONE.forwardTo(gcsDone);
        INTEGER_WIDTH.forwardTo(integerWidth);
    }
    private static void allocPostInitVars() {

    }
    /* @end region="alloc.c" */
    /* @generated region="chartab.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded charCodePropertyAlist = new ELispSymbol.Value.Forwarded(false);

    private static void chartabVars() {
        CHAR_CODE_PROPERTY_ALIST.forwardTo(charCodePropertyAlist);
    }
    private static void chartabPostInitVars() {

    }
    /* @end region="chartab.c" */
    /* @generated region="comp.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded nativeCompJitCompilation = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded compCtxt = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compSubrList = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded compAbiHash = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compNativeVersionDir = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compDeferredPendingH = new ELispSymbol.Value.Forwarded((Object) FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ}));
    public static ELispSymbol.Value.Forwarded compElnToElH = new ELispSymbol.Value.Forwarded((Object) FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL}));
    public static ELispSymbol.Value.Forwarded nativeCompElnLoadPath = new ELispSymbol.Value.Forwarded((Object) new ELispCons(new ELispString("../native-lisp/"), NIL));
    public static ELispSymbol.Value.Forwarded nativeCompEnableSubrTrampolines = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded compInstalledTrampolinesH = new ELispSymbol.Value.Forwarded((Object) FMakeHashTable.makeHashTable(new Object[]{}));
    public static ELispSymbol.Value.Forwarded compNoNativeFileH = new ELispSymbol.Value.Forwarded((Object) FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL}));
    public static ELispSymbol.Value.Forwarded compFilePreloadedP = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compLoadedCompUnitsH = new ELispSymbol.Value.Forwarded((Object) FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, VALUE, CTEST, EQUAL}));
    public static ELispSymbol.Value.Forwarded compSubrAritiesH = new ELispSymbol.Value.Forwarded((Object) FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL}));
    public static ELispSymbol.Value.Forwarded compSanitizerActive = new ELispSymbol.Value.Forwarded(false);

    private static void compVars() {
        NATIVE_COMP_JIT_COMPILATION.forwardTo(nativeCompJitCompilation);
        COMP_CTXT.forwardTo(compCtxt);
        COMP_SUBR_LIST.forwardTo(compSubrList);
        COMP_ABI_HASH.forwardTo(compAbiHash);
        COMP_NATIVE_VERSION_DIR.forwardTo(compNativeVersionDir);
        COMP_DEFERRED_PENDING_H.forwardTo(compDeferredPendingH);
        COMP_ELN_TO_EL_H.forwardTo(compElnToElH);
        NATIVE_COMP_ELN_LOAD_PATH.forwardTo(nativeCompElnLoadPath);
        NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES.forwardTo(nativeCompEnableSubrTrampolines);
        COMP_INSTALLED_TRAMPOLINES_H.forwardTo(compInstalledTrampolinesH);
        COMP_NO_NATIVE_FILE_H.forwardTo(compNoNativeFileH);
        COMP_FILE_PRELOADED_P.forwardTo(compFilePreloadedP);
        COMP_LOADED_COMP_UNITS_H.forwardTo(compLoadedCompUnitsH);
        COMP_SUBR_ARITIES_H.forwardTo(compSubrAritiesH);
        COMP_SANITIZER_ACTIVE.forwardTo(compSanitizerActive);
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
        FProvide.provide(ELispContext.intern("native-compile"), NIL);
    }
    /* @end region="comp.c" */
    /* @generated region="data.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded mostPositiveFixnum = new ELispSymbol.Value.Forwarded((Object) (long) (Long.MAX_VALUE));
    public static ELispSymbol.Value.Forwarded mostNegativeFixnum = new ELispSymbol.Value.Forwarded((Object) (long) (Long.MIN_VALUE));
    public static ELispSymbol.Value.Forwarded symbolsWithPosEnabled = new ELispSymbol.Value.Forwarded(false);

    private static void dataVars() {
        MOST_POSITIVE_FIXNUM.forwardTo(mostPositiveFixnum);
        MOST_NEGATIVE_FIXNUM.forwardTo(mostNegativeFixnum);
        SYMBOLS_WITH_POS_ENABLED.forwardTo(symbolsWithPosEnabled);
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
        ELispContext.intern("most-positive-fixnum").setConstant(true);
        ELispContext.intern("most-negative-fixnum").setConstant(true);
    }
    /* @end region="data.c" */
    /* @generated region="eval.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded maxLispEvalDepth = new ELispSymbol.Value.Forwarded((long) 1_600);
    public static ELispSymbol.Value.Forwarded lispEvalDepthReserve = new ELispSymbol.Value.Forwarded((long) 200);
    public static ELispSymbol.Value.Forwarded quitFlag = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitQuit = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitDebugger = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugOnError = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugIgnoredErrors = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugOnQuit = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugOnNextCall = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded backtraceOnRedisplayError = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debuggerMayContinue = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded debuggerStackFrameAsList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugger = new ELispSymbol.Value.Forwarded((Object) DEBUG_EARLY);
    public static ELispSymbol.Value.Forwarded signalHookFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugOnSignal = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded backtraceOnErrorNoninteractive = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded whenEnteredDebugger = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded internalInterpreterEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded internalMakeInterpretedClosureFunction = new ELispSymbol.Value.Forwarded(false);

    private static void evalVars() {
        MAX_LISP_EVAL_DEPTH.forwardTo(maxLispEvalDepth);
        LISP_EVAL_DEPTH_RESERVE.forwardTo(lispEvalDepthReserve);
        QUIT_FLAG.forwardTo(quitFlag);
        INHIBIT_QUIT.forwardTo(inhibitQuit);
        INHIBIT_DEBUGGER.forwardTo(inhibitDebugger);
        DEBUG_ON_ERROR.forwardTo(debugOnError);
        DEBUG_IGNORED_ERRORS.forwardTo(debugIgnoredErrors);
        DEBUG_ON_QUIT.forwardTo(debugOnQuit);
        DEBUG_ON_NEXT_CALL.forwardTo(debugOnNextCall);
        BACKTRACE_ON_REDISPLAY_ERROR.forwardTo(backtraceOnRedisplayError);
        DEBUGGER_MAY_CONTINUE.forwardTo(debuggerMayContinue);
        DEBUGGER_STACK_FRAME_AS_LIST.forwardTo(debuggerStackFrameAsList);
        DEBUGGER.forwardTo(debugger);
        SIGNAL_HOOK_FUNCTION.forwardTo(signalHookFunction);
        DEBUG_ON_SIGNAL.forwardTo(debugOnSignal);
        BACKTRACE_ON_ERROR_NONINTERACTIVE.forwardTo(backtraceOnErrorNoninteractive);
        INTERNAL_WHEN_ENTERED_DEBUGGER.forwardTo(whenEnteredDebugger);
        INTERNAL_INTERPRETER_ENVIRONMENT.forwardTo(internalInterpreterEnvironment);
        INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION.forwardTo(internalMakeInterpretedClosureFunction);
    }
    private static void evalPostInitVars() {
        ELispContext.unintern(INTERNAL_INTERPRETER_ENVIRONMENT);
    }
    /* @end region="eval.c" */
    /* @generated region="fns.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded overridingPlistEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded features = new ELispSymbol.Value.Forwarded((Object) new ELispCons(EMACS));
    public static ELispSymbol.Value.Forwarded useDialogBox = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded useFileDialog = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded useShortAnswers = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded yesOrNoPrompt = new ELispSymbol.Value.Forwarded((Object) new ELispString("(yes or no) "));

    private static void fnsVars() {
        OVERRIDING_PLIST_ENVIRONMENT.forwardTo(overridingPlistEnvironment);
        FEATURES.forwardTo(features);
        USE_DIALOG_BOX.forwardTo(useDialogBox);
        USE_FILE_DIALOG.forwardTo(useFileDialog);
        USE_SHORT_ANSWERS.forwardTo(useShortAnswers);
        YES_OR_NO_PROMPT.forwardTo(yesOrNoPrompt);
    }
    private static void fnsPostInitVars() {
        YES_OR_NO_P_HISTORY.setValue(NIL);
        FEATURES.setSpecial(false);
    }
    /* @end region="fns.c" */
    /* @generated region="lread.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded obarray = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded values = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded standardInput = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded readCircle = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded loadPath = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded loadSuffixes = new ELispSymbol.Value.Forwarded((Object) ELispCons.listOf(new ELispString(".elc"), new ELispString(".el")));
    public static ELispSymbol.Value.Forwarded moduleFileSuffix = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded dynamicLibrarySuffixes = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadFileRepSuffixes = new ELispSymbol.Value.Forwarded((Object) new ELispCons(new ELispString("")));
    public static ELispSymbol.Value.Forwarded loadInProgress = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded afterLoadAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadHistory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadFileName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadTrueFileName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded userInitFile = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded currentLoadList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadReadFunction = new ELispSymbol.Value.Forwarded((Object) READ);
    public static ELispSymbol.Value.Forwarded loadSourceFileFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadForceDocStrings = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadConvertToUnibyte = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded sourceDirectory = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded preloadedFileList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded byteBooleanVars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadDangerousLibraries = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded forceLoadMessages = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded bytecompVersionRegexp = new ELispSymbol.Value.Forwarded((Object) new ELispString("^;;;.\\(?:in Emacs version\\|bytecomp version FSF\\)"));
    public static ELispSymbol.Value.Forwarded lexicalBinding = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded evalBufferList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lreadUnescapedCharacterLiterals = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadPreferNewer = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadNoNative = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded readSymbolShorthands = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded macroexpDynvars = new ELispSymbol.Value.Forwarded(false);

    private static void lreadVars() {
        OBARRAY.forwardTo(obarray);
        VALUES.forwardTo(values);
        STANDARD_INPUT.forwardTo(standardInput);
        READ_CIRCLE.forwardTo(readCircle);
        LOAD_PATH.forwardTo(loadPath);
        LOAD_SUFFIXES.forwardTo(loadSuffixes);
        MODULE_FILE_SUFFIX.forwardTo(moduleFileSuffix);
        DYNAMIC_LIBRARY_SUFFIXES.forwardTo(dynamicLibrarySuffixes);
        LOAD_FILE_REP_SUFFIXES.forwardTo(loadFileRepSuffixes);
        LOAD_IN_PROGRESS.forwardTo(loadInProgress);
        AFTER_LOAD_ALIST.forwardTo(afterLoadAlist);
        LOAD_HISTORY.forwardTo(loadHistory);
        LOAD_FILE_NAME.forwardTo(loadFileName);
        LOAD_TRUE_FILE_NAME.forwardTo(loadTrueFileName);
        USER_INIT_FILE.forwardTo(userInitFile);
        CURRENT_LOAD_LIST.forwardTo(currentLoadList);
        LOAD_READ_FUNCTION.forwardTo(loadReadFunction);
        LOAD_SOURCE_FILE_FUNCTION.forwardTo(loadSourceFileFunction);
        LOAD_FORCE_DOC_STRINGS.forwardTo(loadForceDocStrings);
        LOAD_CONVERT_TO_UNIBYTE.forwardTo(loadConvertToUnibyte);
        SOURCE_DIRECTORY.forwardTo(sourceDirectory);
        PRELOADED_FILE_LIST.forwardTo(preloadedFileList);
        BYTE_BOOLEAN_VARS.forwardTo(byteBooleanVars);
        LOAD_DANGEROUS_LIBRARIES.forwardTo(loadDangerousLibraries);
        FORCE_LOAD_MESSAGES.forwardTo(forceLoadMessages);
        BYTECOMP_VERSION_REGEXP.forwardTo(bytecompVersionRegexp);
        LEXICAL_BINDING.forwardTo(lexicalBinding);
        EVAL_BUFFER_LIST.forwardTo(evalBufferList);
        LREAD__UNESCAPED_CHARACTER_LITERALS.forwardTo(lreadUnescapedCharacterLiterals);
        LOAD_PREFER_NEWER.forwardTo(loadPreferNewer);
        LOAD_NO_NATIVE.forwardTo(loadNoNative);
        READ_SYMBOL_SHORTHANDS.forwardTo(readSymbolShorthands);
        MACROEXP__DYNVARS.forwardTo(macroexpDynvars);
    }
    private static void lreadPostInitVars() {
        ELispContext.intern("values").setSpecial(false);
        LEXICAL_BINDING.setBufferLocal(true);
    }
    /* @end region="lread.c" */
    /* @generated region="process.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded deleteExitedProcesses = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded processConnectionType = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded processAdaptiveReadBuffering = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded processPrioritizeLowerFds = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded interruptProcessFunctions = new ELispSymbol.Value.Forwarded((Object) new ELispCons(INTERNAL_DEFAULT_INTERRUPT_PROCESS));
    public static ELispSymbol.Value.Forwarded signalProcessFunctions = new ELispSymbol.Value.Forwarded((Object) new ELispCons(INTERNAL_DEFAULT_SIGNAL_PROCESS));
    public static ELispSymbol.Value.Forwarded internalDaemonSockname = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded readProcessOutputMax = new ELispSymbol.Value.Forwarded((long) 65_536);
    public static ELispSymbol.Value.Forwarded fastReadProcessOutput = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded processErrorPauseTime = new ELispSymbol.Value.Forwarded((long) 1);

    private static void processVars() {
        DELETE_EXITED_PROCESSES.forwardTo(deleteExitedProcesses);
        PROCESS_CONNECTION_TYPE.forwardTo(processConnectionType);
        PROCESS_ADAPTIVE_READ_BUFFERING.forwardTo(processAdaptiveReadBuffering);
        PROCESS_PRIORITIZE_LOWER_FDS.forwardTo(processPrioritizeLowerFds);
        INTERRUPT_PROCESS_FUNCTIONS.forwardTo(interruptProcessFunctions);
        SIGNAL_PROCESS_FUNCTIONS.forwardTo(signalProcessFunctions);
        INTERNAL__DAEMON_SOCKNAME.forwardTo(internalDaemonSockname);
        READ_PROCESS_OUTPUT_MAX.forwardTo(readProcessOutputMax);
        FAST_READ_PROCESS_OUTPUT.forwardTo(fastReadProcessOutput);
        PROCESS_ERROR_PAUSE_TIME.forwardTo(processErrorPauseTime);
    }
    private static void processPostInitVars() {
        FProvide.provide(ELispContext.intern("make-network-process"), new ELispCons(new ELispCons(CSERVER, new ELispCons(T, NIL)), new ELispCons(new ELispCons(CFAMILY, new ELispCons(IPV4, NIL)), new ELispCons(new ELispCons(CNOWAIT, new ELispCons(T, NIL)), NIL))));
    }
    /* @end region="process.c" */
    /* @generated region="charset.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded charsetMapPath = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitLoadCharsetMap = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded charsetList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded currentIso639Language = new ELispSymbol.Value.Forwarded(false);

    private static void charsetVars() {
        CHARSET_MAP_PATH.forwardTo(charsetMapPath);
        INHIBIT_LOAD_CHARSET_MAP.forwardTo(inhibitLoadCharsetMap);
        CHARSET_LIST.forwardTo(charsetList);
        CURRENT_ISO639_LANGUAGE.forwardTo(currentIso639Language);
    }
    private static void charsetPostInitVars() {
        defineCharsetInternal(ASCII, 1, "\u0000\u007f\u0000\u0000\u0000\u0000\u0000", 0, 127, 66, -1, 0, 1, 0, 0);
        defineCharsetInternal(ISO_8859_1, 1, "\u0000\u00ff\u0000\u0000\u0000\u0000\u0000", 0, 255, -1, -1, -1, 1, 0, 0);
        defineCharsetInternal(UNICODE, 3, "\u0000\u00ff\u0000\u00ff\u0000\u0010\u0000", 0, 1114111, -1, 0, -1, 1, 0, 0);
        defineCharsetInternal(EMACS, 3, "\u0000\u00ff\u0000\u00ff\u0000?\u0000", 0, 4194175, -1, 0, -1, 1, 1, 0);
        defineCharsetInternal(EIGHT_BIT, 1, "\u0080\u00ff\u0000\u0000\u0000\u0000\u0000", 128, 255, -1, 0, -1, 0, 1, 4194176);
    }
    /* @end region="charset.c" */
    /* @generated region="fileio.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded fileNameCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded defaultFileNameCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded fileNameHandlerAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded setAutoCodingFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded afterInsertFileFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded writeRegionAnnotateFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded writeRegionPostAnnotationFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded writeRegionAnnotationsSoFar = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitFileNameHandlers = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitFileNameOperation = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoSaveListFileName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoSaveVisitedFileName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoSaveIncludeBigDeletions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded writeRegionInhibitFsync = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded deleteByMovingToTrash = new ELispSymbol.Value.Forwarded(false);

    private static void fileioVars() {
        FILE_NAME_CODING_SYSTEM.forwardTo(fileNameCodingSystem);
        DEFAULT_FILE_NAME_CODING_SYSTEM.forwardTo(defaultFileNameCodingSystem);
        FILE_NAME_HANDLER_ALIST.forwardTo(fileNameHandlerAlist);
        SET_AUTO_CODING_FUNCTION.forwardTo(setAutoCodingFunction);
        AFTER_INSERT_FILE_FUNCTIONS.forwardTo(afterInsertFileFunctions);
        WRITE_REGION_ANNOTATE_FUNCTIONS.forwardTo(writeRegionAnnotateFunctions);
        WRITE_REGION_POST_ANNOTATION_FUNCTION.forwardTo(writeRegionPostAnnotationFunction);
        WRITE_REGION_ANNOTATIONS_SO_FAR.forwardTo(writeRegionAnnotationsSoFar);
        INHIBIT_FILE_NAME_HANDLERS.forwardTo(inhibitFileNameHandlers);
        INHIBIT_FILE_NAME_OPERATION.forwardTo(inhibitFileNameOperation);
        AUTO_SAVE_LIST_FILE_NAME.forwardTo(autoSaveListFileName);
        AUTO_SAVE_VISITED_FILE_NAME.forwardTo(autoSaveVisitedFileName);
        AUTO_SAVE_INCLUDE_BIG_DELETIONS.forwardTo(autoSaveIncludeBigDeletions);
        WRITE_REGION_INHIBIT_FSYNC.forwardTo(writeRegionInhibitFsync);
        DELETE_BY_MOVING_TO_TRASH.forwardTo(deleteByMovingToTrash);
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
    /* @end region="fileio.c" */
    /* @generated region="editfns.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded inhibitFieldTextMotion = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded bufferAccessFontifyFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded bufferAccessFontifiedProperty = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded userFullName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded userLoginName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded userRealLoginName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded operatingSystemRelease = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded binaryAsUnsigned = new ELispSymbol.Value.Forwarded(false);

    private static void editfnsVars() {
        INHIBIT_FIELD_TEXT_MOTION.forwardTo(inhibitFieldTextMotion);
        BUFFER_ACCESS_FONTIFY_FUNCTIONS.forwardTo(bufferAccessFontifyFunctions);
        BUFFER_ACCESS_FONTIFIED_PROPERTY.forwardTo(bufferAccessFontifiedProperty);
        SYSTEM_NAME.forwardTo(systemName);
        USER_FULL_NAME.forwardTo(userFullName);
        USER_LOGIN_NAME.forwardTo(userLoginName);
        USER_REAL_LOGIN_NAME.forwardTo(userRealLoginName);
        OPERATING_SYSTEM_RELEASE.forwardTo(operatingSystemRelease);
        BINARY_AS_UNSIGNED.forwardTo(binaryAsUnsigned);
    }
    private static void editfnsPostInitVars() {
        ELispContext.unintern(OUTERMOST_RESTRICTION);
    }
    /* @end region="editfns.c" */
    /* @generated region="emacs.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded commandLineArgs = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded systemType = new ELispSymbol.Value.Forwarded((Object) ELispContext.intern("jvm"));
    public static ELispSymbol.Value.Forwarded systemConfiguration = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded systemConfigurationOptions = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded systemConfigurationFeatures = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded noninteractive1 = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded killEmacsHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded pathSeparator = new ELispSymbol.Value.Forwarded((Object) new ELispString(":"));
    public static ELispSymbol.Value.Forwarded invocationName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded invocationDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded installationDirectory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemMessagesLocale = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemTimeLocale = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded beforeInitTime = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded afterInitTime = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitXResources = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded emacsCopyright = new ELispSymbol.Value.Forwarded((Object) new ELispString("TODO: Copy over GPL"));
    public static ELispSymbol.Value.Forwarded emacsVersion = new ELispSymbol.Value.Forwarded((Object) new ELispString("30.0"));
    public static ELispSymbol.Value.Forwarded reportEmacsBugAddress = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded dumpMode = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded dynamicLibraryAlist = new ELispSymbol.Value.Forwarded(false);

    private static void emacsVars() {
        COMMAND_LINE_ARGS.forwardTo(commandLineArgs);
        SYSTEM_TYPE.forwardTo(systemType);
        SYSTEM_CONFIGURATION.forwardTo(systemConfiguration);
        SYSTEM_CONFIGURATION_OPTIONS.forwardTo(systemConfigurationOptions);
        SYSTEM_CONFIGURATION_FEATURES.forwardTo(systemConfigurationFeatures);
        NONINTERACTIVE.forwardTo(noninteractive1);
        KILL_EMACS_HOOK.forwardTo(killEmacsHook);
        PATH_SEPARATOR.forwardTo(pathSeparator);
        INVOCATION_NAME.forwardTo(invocationName);
        INVOCATION_DIRECTORY.forwardTo(invocationDirectory);
        INSTALLATION_DIRECTORY.forwardTo(installationDirectory);
        SYSTEM_MESSAGES_LOCALE.forwardTo(systemMessagesLocale);
        SYSTEM_TIME_LOCALE.forwardTo(systemTimeLocale);
        BEFORE_INIT_TIME.forwardTo(beforeInitTime);
        AFTER_INIT_TIME.forwardTo(afterInitTime);
        INHIBIT_X_RESOURCES.forwardTo(inhibitXResources);
        EMACS_COPYRIGHT.forwardTo(emacsCopyright);
        EMACS_VERSION.forwardTo(emacsVersion);
        REPORT_EMACS_BUG_ADDRESS.forwardTo(reportEmacsBugAddress);
        DUMP_MODE.forwardTo(dumpMode);
        DYNAMIC_LIBRARY_ALIST.forwardTo(dynamicLibraryAlist);
    }
    private static void emacsPostInitVars() {
        ELispContext.intern("dynamic-library-alist").putProperty(RISKY_LOCAL_VARIABLE, T);
    }
    /* @end region="emacs.c" */
    /* @generated region="search.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded searchSpacesRegexp = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitChangingMatchData = new ELispSymbol.Value.Forwarded(false);

    private static void searchVars() {
        SEARCH_SPACES_REGEXP.forwardTo(searchSpacesRegexp);
        INHIBIT_CHANGING_MATCH_DATA.forwardTo(inhibitChangingMatchData);
    }
    private static void searchPostInitVars() {
        SEARCH_FAILED.putProperty(ERROR_CONDITIONS, ELispCons.listOf(SEARCH_FAILED, ERROR));
        SEARCH_FAILED.putProperty(ERROR_MESSAGE, new ELispString("Search failed"));
        USER_SEARCH_FAILED.putProperty(ERROR_CONDITIONS, ELispCons.listOf(USER_SEARCH_FAILED, USER_ERROR, SEARCH_FAILED, ERROR));
        USER_SEARCH_FAILED.putProperty(ERROR_MESSAGE, new ELispString("Search failed"));
        INVALID_REGEXP.putProperty(ERROR_CONDITIONS, ELispCons.listOf(INVALID_REGEXP, ERROR));
        INVALID_REGEXP.putProperty(ERROR_MESSAGE, new ELispString("Invalid regexp"));
    }
    /* @end region="search.c" */
    /* @generated region="buffer.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded beforeChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded afterChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded firstChangeHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded transientMarkMode = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitReadOnly = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded killBufferQueryFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded changeMajorModeHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded bufferListUpdateHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded killBufferDeleteAutoSaveFiles = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded deleteAutoSaveFiles = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded caseFoldSearch = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded cloneIndirectBufferHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded longLineThreshold = new ELispSymbol.Value.Forwarded((Object) 50000);
    public static ELispSymbol.Value.Forwarded longLineOptimizationsRegionSize = new ELispSymbol.Value.Forwarded((long) 500_000);
    public static ELispSymbol.Value.Forwarded longLineOptimizationsBolSearchLimit = new ELispSymbol.Value.Forwarded((long) 128);
    public static ELispSymbol.Value.Forwarded largeHscrollThreshold = new ELispSymbol.Value.Forwarded((long) 10_000);

    private static void bufferVars() {
        BEFORE_CHANGE_FUNCTIONS.forwardTo(beforeChangeFunctions);
        AFTER_CHANGE_FUNCTIONS.forwardTo(afterChangeFunctions);
        FIRST_CHANGE_HOOK.forwardTo(firstChangeHook);
        TRANSIENT_MARK_MODE.forwardTo(transientMarkMode);
        INHIBIT_READ_ONLY.forwardTo(inhibitReadOnly);
        KILL_BUFFER_QUERY_FUNCTIONS.forwardTo(killBufferQueryFunctions);
        CHANGE_MAJOR_MODE_HOOK.forwardTo(changeMajorModeHook);
        BUFFER_LIST_UPDATE_HOOK.forwardTo(bufferListUpdateHook);
        KILL_BUFFER_DELETE_AUTO_SAVE_FILES.forwardTo(killBufferDeleteAutoSaveFiles);
        DELETE_AUTO_SAVE_FILES.forwardTo(deleteAutoSaveFiles);
        CASE_FOLD_SEARCH.forwardTo(caseFoldSearch);
        CLONE_INDIRECT_BUFFER_HOOK.forwardTo(cloneIndirectBufferHook);
        LONG_LINE_THRESHOLD.forwardTo(longLineThreshold);
        LONG_LINE_OPTIMIZATIONS_REGION_SIZE.forwardTo(longLineOptimizationsRegionSize);
        LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT.forwardTo(longLineOptimizationsBolSearchLimit);
        LARGE_HSCROLL_THRESHOLD.forwardTo(largeHscrollThreshold);
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
    /* @end region="buffer.c" */
    /* @generated region="keymap.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded minibufferLocalMap = new ELispSymbol.Value.Forwarded((Object) FMakeSparseKeymap.makeSparseKeymap(NIL));
    public static ELispSymbol.Value.Forwarded minorModeMapAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded minorModeOverridingMapAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded emulationModeMapAlists = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded whereIsPreferredModifier = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded describeBindingsCheckShadowingInRanges = new ELispSymbol.Value.Forwarded(false);

    private static void keymapVars() {
        MINIBUFFER_LOCAL_MAP.forwardTo(minibufferLocalMap);
        MINOR_MODE_MAP_ALIST.forwardTo(minorModeMapAlist);
        MINOR_MODE_OVERRIDING_MAP_ALIST.forwardTo(minorModeOverridingMapAlist);
        EMULATION_MODE_MAP_ALISTS.forwardTo(emulationModeMapAlists);
        WHERE_IS_PREFERRED_MODIFIER.forwardTo(whereIsPreferredModifier);
        DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES.forwardTo(describeBindingsCheckShadowingInRanges);
    }
    private static void keymapPostInitVars() {
        KEYMAP.putProperty(CHAR_TABLE_EXTRA_SLOTS, (long) (0));
    }
    /* @end region="keymap.c" */
    /* @generated region="print.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded standardOutput = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded floatOutputFormat = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printIntegersAsCharacters = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printLength = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printLevel = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printEscapeNewlines = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printEscapeControlCharacters = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printEscapeNonascii = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printEscapeMultibyte = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printQuoted = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded printGensym = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printCircle = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printContinuousNumbering = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printNumberTable = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printCharsetTextProperty = new ELispSymbol.Value.Forwarded((Object) DEFAULT);
    public static ELispSymbol.Value.Forwarded printSymbolsBare = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printUnreadableFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printUnreadableCallbackBuffer = new ELispSymbol.Value.Forwarded(false);

    private static void printVars() {
        STANDARD_OUTPUT.forwardTo(standardOutput);
        FLOAT_OUTPUT_FORMAT.forwardTo(floatOutputFormat);
        PRINT_INTEGERS_AS_CHARACTERS.forwardTo(printIntegersAsCharacters);
        PRINT_LENGTH.forwardTo(printLength);
        PRINT_LEVEL.forwardTo(printLevel);
        PRINT_ESCAPE_NEWLINES.forwardTo(printEscapeNewlines);
        PRINT_ESCAPE_CONTROL_CHARACTERS.forwardTo(printEscapeControlCharacters);
        PRINT_ESCAPE_NONASCII.forwardTo(printEscapeNonascii);
        PRINT_ESCAPE_MULTIBYTE.forwardTo(printEscapeMultibyte);
        PRINT_QUOTED.forwardTo(printQuoted);
        PRINT_GENSYM.forwardTo(printGensym);
        PRINT_CIRCLE.forwardTo(printCircle);
        PRINT_CONTINUOUS_NUMBERING.forwardTo(printContinuousNumbering);
        PRINT_NUMBER_TABLE.forwardTo(printNumberTable);
        PRINT_CHARSET_TEXT_PROPERTY.forwardTo(printCharsetTextProperty);
        PRINT_SYMBOLS_BARE.forwardTo(printSymbolsBare);
        PRINT_UNREADABLE_FUNCTION.forwardTo(printUnreadableFunction);
        PRINT__UNREADABLE_CALLBACK_BUFFER.forwardTo(printUnreadableCallbackBuffer);
    }
    private static void printPostInitVars() {
        ELispContext.unintern(PRINT__UNREADABLE_CALLBACK_BUFFER);
    }
    /* @end region="print.c" */
    /* @generated region="xfaces.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded faceFiltersAlwaysMatch = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceNewFrameDefaults = new ELispSymbol.Value.Forwarded((Object) new ELispHashtable());
    public static ELispSymbol.Value.Forwarded faceDefaultStipple = new ELispSymbol.Value.Forwarded((Object) new ELispString("gray3"));
    public static ELispSymbol.Value.Forwarded ttyDefinedColorAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded scalableFontsAllowed = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceIgnoredFonts = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceRemappingAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceFontRescaleAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceNearSameColorThreshold = new ELispSymbol.Value.Forwarded((long) 30_000);
    public static ELispSymbol.Value.Forwarded faceFontLaxMatchedAttributes = new ELispSymbol.Value.Forwarded((Object) T);

    private static void xfacesVars() {
        FACE_FILTERS_ALWAYS_MATCH.forwardTo(faceFiltersAlwaysMatch);
        FACE__NEW_FRAME_DEFAULTS.forwardTo(faceNewFrameDefaults);
        FACE_DEFAULT_STIPPLE.forwardTo(faceDefaultStipple);
        TTY_DEFINED_COLOR_ALIST.forwardTo(ttyDefinedColorAlist);
        SCALABLE_FONTS_ALLOWED.forwardTo(scalableFontsAllowed);
        FACE_IGNORED_FONTS.forwardTo(faceIgnoredFonts);
        FACE_REMAPPING_ALIST.forwardTo(faceRemappingAlist);
        FACE_FONT_RESCALE_ALIST.forwardTo(faceFontRescaleAlist);
        FACE_NEAR_SAME_COLOR_THRESHOLD.forwardTo(faceNearSameColorThreshold);
        FACE_FONT_LAX_MATCHED_ATTRIBUTES.forwardTo(faceFontLaxMatchedAttributes);
    }
    private static void xfacesPostInitVars() {

    }
    /* @end region="xfaces.c" */
    /* @generated region="timefns.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded currentTimeList = new ELispSymbol.Value.Forwarded(true);

    private static void timefnsVars() {
        CURRENT_TIME_LIST.forwardTo(currentTimeList);
    }
    private static void timefnsPostInitVars() {

    }
    /* @end region="timefns.c" */
    /* @generated region="casetab.c" by="extract-emacs-src.py" */

    private static void casetabVars() {

    }
    private static void casetabPostInitVars() {

    }
    /* @end region="casetab.c" */
    /* @generated region="cmds.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded postSelfInsertHook = new ELispSymbol.Value.Forwarded(false);

    private static void cmdsVars() {
        POST_SELF_INSERT_HOOK.forwardTo(postSelfInsertHook);
    }
    private static void cmdsPostInitVars() {

    }
    /* @end region="cmds.c" */
    /* @generated region="keyboard.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded internalTopLevelMessage = new ELispSymbol.Value.Forwarded((Object) new ELispString("Back to top level"));
    public static ELispSymbol.Value.Forwarded lastCommandEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded lastNonmenuEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded lastInputEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded unreadCommandEvents = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded unreadPostInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded unreadInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded metaPrefixChar = new ELispSymbol.Value.Forwarded((Object) (long) (27));
    public static ELispSymbol.Value.Forwarded lastCommand = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded realLastCommand = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded lastRepeatableCommand = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded thisCommand = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded realThisCommand = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded currentMinibufferCommand = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded thisCommandKeysShiftTranslated = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded thisOriginalCommand = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoSaveInterval = new ELispSymbol.Value.Forwarded((long) 300);
    public static ELispSymbol.Value.Forwarded autoSaveNoMessage = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoSaveTimeout = new ELispSymbol.Value.Forwarded((Object) 30);
    public static ELispSymbol.Value.Forwarded echoKeystrokes = new ELispSymbol.Value.Forwarded((Object) (long) (1));
    public static ELispSymbol.Value.Forwarded echoKeystrokesHelp = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded pollingPeriod = new ELispSymbol.Value.Forwarded((Object) 2.0);
    public static ELispSymbol.Value.Forwarded doubleClickTime = new ELispSymbol.Value.Forwarded((Object) (long) (500));
    public static ELispSymbol.Value.Forwarded doubleClickFuzz = new ELispSymbol.Value.Forwarded((long) 3);
    public static ELispSymbol.Value.Forwarded numInputKeys = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded numNonmacroInputEvents = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded lastEventFrame = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lastEventDevice = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded ttyEraseChar = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded helpChar = new ELispSymbol.Value.Forwarded((Object) (long) (8));
    public static ELispSymbol.Value.Forwarded helpEventList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded helpForm = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded prefixHelpCommand = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded topLevel = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded keyboardTranslateTable = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded cannotSuspend = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded menuPrompting = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded menuPromptMoreChar = new ELispSymbol.Value.Forwarded((Object) (long) (32));
    public static ELispSymbol.Value.Forwarded extraKeyboardModifiers = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded deactivateMark = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded preCommandHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded postCommandHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lucidMenuGrabKeyboard = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded menuBarFinalItems = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded tabBarSeparatorImageExpression = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded toolBarSeparatorImageExpression = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded overridingTerminalLocalMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded overridingLocalMap = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded overridingLocalMapMenuFlag = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded specialEventMap = new ELispSymbol.Value.Forwarded((Object) new ELispCons(KEYMAP));
    public static ELispSymbol.Value.Forwarded trackMouse = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded systemKeyAlist = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded localFunctionKeyMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded inputDecodeMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded functionKeyMap = new ELispSymbol.Value.Forwarded((Object) FMakeSparseKeymap.makeSparseKeymap(NIL));
    public static ELispSymbol.Value.Forwarded keyTranslationMap = new ELispSymbol.Value.Forwarded((Object) FMakeSparseKeymap.makeSparseKeymap(NIL));
    public static ELispSymbol.Value.Forwarded delayedWarningsList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded timerList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded timerIdleList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inputMethodFunction = new ELispSymbol.Value.Forwarded((Object) LIST);
    public static ELispSymbol.Value.Forwarded inputMethodPreviousMessage = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded showHelpFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded disablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded globalDisablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded minibufferMessageTimeout = new ELispSymbol.Value.Forwarded((Object) (long) (2));
    public static ELispSymbol.Value.Forwarded throwOnInput = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded commandErrorFunction = new ELispSymbol.Value.Forwarded((Object) COMMAND_ERROR_DEFAULT_FUNCTION);
    public static ELispSymbol.Value.Forwarded enableDisabledMenusAndButtons = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded selectActiveRegions = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded savedRegionSelection = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded selectionInhibitUpdateCommands = new ELispSymbol.Value.Forwarded((Object) ELispCons.listOf(HANDLE_SWITCH_FRAME, HANDLE_SELECT_WINDOW));
    public static ELispSymbol.Value.Forwarded debugOnEvent = new ELispSymbol.Value.Forwarded((Object) SIGUSR2);
    public static ELispSymbol.Value.Forwarded attemptStackOverflowRecovery = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded attemptOrderlyShutdownOnFatalSignal = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded whileNoInputIgnoreEvents = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded translateUpperCaseKeyBindings = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded inputPendingPFilterEvents = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded mwheelCoalesceScrollEvents = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded displayMonitorsChangedFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitRecordChar = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded recordAllKeys = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded postSelectRegionHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded disableInhibitTextConversion = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded currentKeyRemapSequence = new ELispSymbol.Value.Forwarded(false);

    private static void keyboardVars() {
        INTERNAL__TOP_LEVEL_MESSAGE.forwardTo(internalTopLevelMessage);
        LAST_COMMAND_EVENT.forwardTo(lastCommandEvent);
        LAST_NONMENU_EVENT.forwardTo(lastNonmenuEvent);
        LAST_INPUT_EVENT.forwardTo(lastInputEvent);
        UNREAD_COMMAND_EVENTS.forwardTo(unreadCommandEvents);
        UNREAD_POST_INPUT_METHOD_EVENTS.forwardTo(unreadPostInputMethodEvents);
        UNREAD_INPUT_METHOD_EVENTS.forwardTo(unreadInputMethodEvents);
        META_PREFIX_CHAR.forwardTo(metaPrefixChar);
        LAST_COMMAND.forwardTo(lastCommand);
        REAL_LAST_COMMAND.forwardTo(realLastCommand);
        LAST_REPEATABLE_COMMAND.forwardTo(lastRepeatableCommand);
        THIS_COMMAND.forwardTo(thisCommand);
        REAL_THIS_COMMAND.forwardTo(realThisCommand);
        CURRENT_MINIBUFFER_COMMAND.forwardTo(currentMinibufferCommand);
        THIS_COMMAND_KEYS_SHIFT_TRANSLATED.forwardTo(thisCommandKeysShiftTranslated);
        THIS_ORIGINAL_COMMAND.forwardTo(thisOriginalCommand);
        AUTO_SAVE_INTERVAL.forwardTo(autoSaveInterval);
        AUTO_SAVE_NO_MESSAGE.forwardTo(autoSaveNoMessage);
        AUTO_SAVE_TIMEOUT.forwardTo(autoSaveTimeout);
        ECHO_KEYSTROKES.forwardTo(echoKeystrokes);
        ECHO_KEYSTROKES_HELP.forwardTo(echoKeystrokesHelp);
        POLLING_PERIOD.forwardTo(pollingPeriod);
        DOUBLE_CLICK_TIME.forwardTo(doubleClickTime);
        DOUBLE_CLICK_FUZZ.forwardTo(doubleClickFuzz);
        NUM_INPUT_KEYS.forwardTo(numInputKeys);
        NUM_NONMACRO_INPUT_EVENTS.forwardTo(numNonmacroInputEvents);
        LAST_EVENT_FRAME.forwardTo(lastEventFrame);
        LAST_EVENT_DEVICE.forwardTo(lastEventDevice);
        TTY_ERASE_CHAR.forwardTo(ttyEraseChar);
        HELP_CHAR.forwardTo(helpChar);
        HELP_EVENT_LIST.forwardTo(helpEventList);
        HELP_FORM.forwardTo(helpForm);
        PREFIX_HELP_COMMAND.forwardTo(prefixHelpCommand);
        TOP_LEVEL.forwardTo(topLevel);
        KEYBOARD_TRANSLATE_TABLE.forwardTo(keyboardTranslateTable);
        CANNOT_SUSPEND.forwardTo(cannotSuspend);
        MENU_PROMPTING.forwardTo(menuPrompting);
        MENU_PROMPT_MORE_CHAR.forwardTo(menuPromptMoreChar);
        EXTRA_KEYBOARD_MODIFIERS.forwardTo(extraKeyboardModifiers);
        DEACTIVATE_MARK.forwardTo(deactivateMark);
        PRE_COMMAND_HOOK.forwardTo(preCommandHook);
        POST_COMMAND_HOOK.forwardTo(postCommandHook);
        LUCID__MENU_GRAB_KEYBOARD.forwardTo(lucidMenuGrabKeyboard);
        MENU_BAR_FINAL_ITEMS.forwardTo(menuBarFinalItems);
        TAB_BAR_SEPARATOR_IMAGE_EXPRESSION.forwardTo(tabBarSeparatorImageExpression);
        TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION.forwardTo(toolBarSeparatorImageExpression);
        OVERRIDING_TERMINAL_LOCAL_MAP.forwardTo(overridingTerminalLocalMap);
        OVERRIDING_LOCAL_MAP.forwardTo(overridingLocalMap);
        OVERRIDING_LOCAL_MAP_MENU_FLAG.forwardTo(overridingLocalMapMenuFlag);
        SPECIAL_EVENT_MAP.forwardTo(specialEventMap);
        TRACK_MOUSE.forwardTo(trackMouse);
        SYSTEM_KEY_ALIST.forwardTo(systemKeyAlist);
        LOCAL_FUNCTION_KEY_MAP.forwardTo(localFunctionKeyMap);
        INPUT_DECODE_MAP.forwardTo(inputDecodeMap);
        FUNCTION_KEY_MAP.forwardTo(functionKeyMap);
        KEY_TRANSLATION_MAP.forwardTo(keyTranslationMap);
        DELAYED_WARNINGS_LIST.forwardTo(delayedWarningsList);
        TIMER_LIST.forwardTo(timerList);
        TIMER_IDLE_LIST.forwardTo(timerIdleList);
        INPUT_METHOD_FUNCTION.forwardTo(inputMethodFunction);
        INPUT_METHOD_PREVIOUS_MESSAGE.forwardTo(inputMethodPreviousMessage);
        SHOW_HELP_FUNCTION.forwardTo(showHelpFunction);
        DISABLE_POINT_ADJUSTMENT.forwardTo(disablePointAdjustment);
        GLOBAL_DISABLE_POINT_ADJUSTMENT.forwardTo(globalDisablePointAdjustment);
        MINIBUFFER_MESSAGE_TIMEOUT.forwardTo(minibufferMessageTimeout);
        THROW_ON_INPUT.forwardTo(throwOnInput);
        COMMAND_ERROR_FUNCTION.forwardTo(commandErrorFunction);
        ENABLE_DISABLED_MENUS_AND_BUTTONS.forwardTo(enableDisabledMenusAndButtons);
        SELECT_ACTIVE_REGIONS.forwardTo(selectActiveRegions);
        SAVED_REGION_SELECTION.forwardTo(savedRegionSelection);
        SELECTION_INHIBIT_UPDATE_COMMANDS.forwardTo(selectionInhibitUpdateCommands);
        DEBUG_ON_EVENT.forwardTo(debugOnEvent);
        ATTEMPT_STACK_OVERFLOW_RECOVERY.forwardTo(attemptStackOverflowRecovery);
        ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL.forwardTo(attemptOrderlyShutdownOnFatalSignal);
        WHILE_NO_INPUT_IGNORE_EVENTS.forwardTo(whileNoInputIgnoreEvents);
        TRANSLATE_UPPER_CASE_KEY_BINDINGS.forwardTo(translateUpperCaseKeyBindings);
        INPUT_PENDING_P_FILTER_EVENTS.forwardTo(inputPendingPFilterEvents);
        MWHEEL_COALESCE_SCROLL_EVENTS.forwardTo(mwheelCoalesceScrollEvents);
        DISPLAY_MONITORS_CHANGED_FUNCTIONS.forwardTo(displayMonitorsChangedFunctions);
        INHIBIT__RECORD_CHAR.forwardTo(inhibitRecordChar);
        RECORD_ALL_KEYS.forwardTo(recordAllKeys);
        POST_SELECT_REGION_HOOK.forwardTo(postSelectRegionHook);
        DISABLE_INHIBIT_TEXT_CONVERSION.forwardTo(disableInhibitTextConversion);
        CURRENT_KEY_REMAP_SEQUENCE.forwardTo(currentKeyRemapSequence);
    }
    private static void keyboardPostInitVars() {
        INPUT_METHOD_EXIT_ON_FIRST_CHAR.setValue(NIL);
        INPUT_METHOD_USE_ECHO_AREA.setValue(NIL);
        TOP_LEVEL.setSpecial(false);
        DEACTIVATE_MARK.setBufferLocal(true);
        ECHO_AREA_CLEAR_HOOK.setValue(NIL);
    }
    /* @end region="keyboard.c" */
    /* @generated region="callint.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded prefixArg = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded lastPrefixArg = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded currentPrefixArg = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded commandHistory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded commandDebugStatus = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded markEvenIfInactive = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded mouseLeaveBufferHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitMouseEventCheck = new ELispSymbol.Value.Forwarded(false);

    private static void callintVars() {
        PREFIX_ARG.forwardTo(prefixArg);
        LAST_PREFIX_ARG.forwardTo(lastPrefixArg);
        CURRENT_PREFIX_ARG.forwardTo(currentPrefixArg);
        COMMAND_HISTORY.forwardTo(commandHistory);
        COMMAND_DEBUG_STATUS.forwardTo(commandDebugStatus);
        MARK_EVEN_IF_INACTIVE.forwardTo(markEvenIfInactive);
        MOUSE_LEAVE_BUFFER_HOOK.forwardTo(mouseLeaveBufferHook);
        INHIBIT_MOUSE_EVENT_CHECK.forwardTo(inhibitMouseEventCheck);
    }
    private static void callintPostInitVars() {

    }
    /* @end region="callint.c" */
}
