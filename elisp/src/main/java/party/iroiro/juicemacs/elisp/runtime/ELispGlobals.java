package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInCharTab.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInKeymap.*;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.Collections;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCharSet.defineCharsetInternal;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCoding.*;
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
        casefiddleVars();
        casetabVars();
        characterVars();
        charsetVars();
        chartabVars();
        cmdsVars();
        codingVars();
        compVars();
        dataVars();
        editfnsVars();
        emacsVars();
        evalVars();
        fileioVars();
        fnsVars();
        frameVars();
        keyboardVars();
        keymapVars();
        lreadVars();
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
        casefiddlePostInitVars();
        casetabPostInitVars();
        characterPostInitVars();
        charsetPostInitVars();
        chartabPostInitVars();
        cmdsPostInitVars();
        codingPostInitVars();
        compPostInitVars();
        dataPostInitVars();
        editfnsPostInitVars();
        emacsPostInitVars();
        evalPostInitVars();
        fnsPostInitVars();
        fileioPostInitVars();
        framePostInitVars();
        keyboardPostInitVars();
        keymapPostInitVars();
        lreadPostInitVars();
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
    public static ELispSymbol.Value.Forwarded memorySignalData = new ELispSymbol.Value.Forwarded(false);
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
        var memorySignalDataJInit = ELispCons.listOf(ERROR, new ELispString("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs"));
        memorySignalData.setValue(memorySignalDataJInit);
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
    public static ELispSymbol.Value.Forwarded compDeferredPendingH = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compElnToElH = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded nativeCompElnLoadPath = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded nativeCompEnableSubrTrampolines = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded compInstalledTrampolinesH = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compNoNativeFileH = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compFilePreloadedP = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compLoadedCompUnitsH = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded compSubrAritiesH = new ELispSymbol.Value.Forwarded(false);
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
    /* @end region="comp.c" */
    /* @generated region="data.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded mostPositiveFixnum = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded mostNegativeFixnum = new ELispSymbol.Value.Forwarded(false);
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
        var mostPositiveFixnumJInit = (long) (Long.MAX_VALUE);
        mostPositiveFixnum.setValue(mostPositiveFixnumJInit);
        ELispContext.intern("most-positive-fixnum").setConstant(true);
        var mostNegativeFixnumJInit = (long) (Long.MIN_VALUE);
        mostNegativeFixnum.setValue(mostNegativeFixnumJInit);
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
    public static ELispSymbol.Value.Forwarded features = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded useDialogBox = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded useFileDialog = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded useShortAnswers = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded yesOrNoPrompt = new ELispSymbol.Value.Forwarded(false);

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
        var featuresJInit = new ELispCons(EMACS);
        features.setValue(featuresJInit);
        FEATURES.setSpecial(false);
        var yesOrNoPromptJInit = new ELispString("(yes or no) ");
        yesOrNoPrompt.setValue(yesOrNoPromptJInit);
    }
    /* @end region="fns.c" */
    /* @generated region="lread.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded obarray = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded values = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded standardInput = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded readCircle = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded loadPath = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded loadSuffixes = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded moduleFileSuffix = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded dynamicLibrarySuffixes = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadFileRepSuffixes = new ELispSymbol.Value.Forwarded(false);
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
    public static ELispSymbol.Value.Forwarded sourceDirectory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded preloadedFileList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded byteBooleanVars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded loadDangerousLibraries = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded forceLoadMessages = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded bytecompVersionRegexp = new ELispSymbol.Value.Forwarded(false);
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
    /* @end region="lread.c" */
    /* @generated region="process.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded deleteExitedProcesses = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded processConnectionType = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded processAdaptiveReadBuffering = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded processPrioritizeLowerFds = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded interruptProcessFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded signalProcessFunctions = new ELispSymbol.Value.Forwarded(false);
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
        var interruptProcessFunctionsJInit = new ELispCons(INTERNAL_DEFAULT_INTERRUPT_PROCESS);
        interruptProcessFunctions.setValue(interruptProcessFunctionsJInit);
        var signalProcessFunctionsJInit = new ELispCons(INTERNAL_DEFAULT_SIGNAL_PROCESS);
        signalProcessFunctions.setValue(signalProcessFunctionsJInit);
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
    public static ELispSymbol.Value.Forwarded systemType = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemConfiguration = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemConfigurationOptions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemConfigurationFeatures = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded noninteractive1 = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded killEmacsHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded pathSeparator = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded invocationName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded invocationDirectory = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded installationDirectory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemMessagesLocale = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded systemTimeLocale = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded beforeInitTime = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded afterInitTime = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitXResources = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded emacsCopyright = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded emacsVersion = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded reportEmacsBugAddress = new ELispSymbol.Value.Forwarded(false);
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
    public static ELispSymbol.Value.Forwarded minibufferLocalMap = new ELispSymbol.Value.Forwarded(false);
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
        var minibufferLocalMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        minibufferLocalMap.setValue(minibufferLocalMapJInit);
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
    public static ELispSymbol.Value.Forwarded faceNewFrameDefaults = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceDefaultStipple = new ELispSymbol.Value.Forwarded(false);
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
        var faceNewFrameDefaultsJInit = new ELispHashtable();
        faceNewFrameDefaults.setValue(faceNewFrameDefaultsJInit);
        var faceDefaultStippleJInit = new ELispString("gray3");
        faceDefaultStipple.setValue(faceDefaultStippleJInit);
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
    public static ELispSymbol.Value.Forwarded internalTopLevelMessage = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lastCommandEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded lastNonmenuEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded lastInputEvent = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded unreadCommandEvents = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded unreadPostInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded unreadInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded metaPrefixChar = new ELispSymbol.Value.Forwarded(false);
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
    public static ELispSymbol.Value.Forwarded echoKeystrokes = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded echoKeystrokesHelp = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded pollingPeriod = new ELispSymbol.Value.Forwarded((Object) 2.0);
    public static ELispSymbol.Value.Forwarded doubleClickTime = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded doubleClickFuzz = new ELispSymbol.Value.Forwarded((long) 3);
    public static ELispSymbol.Value.Forwarded numInputKeys = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded numNonmacroInputEvents = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded lastEventFrame = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lastEventDevice = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded ttyEraseChar = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded helpChar = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded helpEventList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded helpForm = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded prefixHelpCommand = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded topLevel = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded keyboardTranslateTable = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded cannotSuspend = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded menuPrompting = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded menuPromptMoreChar = new ELispSymbol.Value.Forwarded(false);
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
    public static ELispSymbol.Value.Forwarded specialEventMap = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded trackMouse = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded systemKeyAlist = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded localFunctionKeyMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded inputDecodeMap = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded functionKeyMap = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded keyTranslationMap = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded delayedWarningsList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded timerList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded timerIdleList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inputMethodFunction = new ELispSymbol.Value.Forwarded((Object) LIST);
    public static ELispSymbol.Value.Forwarded inputMethodPreviousMessage = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded showHelpFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded disablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded globalDisablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded minibufferMessageTimeout = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded throwOnInput = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded commandErrorFunction = new ELispSymbol.Value.Forwarded((Object) COMMAND_ERROR_DEFAULT_FUNCTION);
    public static ELispSymbol.Value.Forwarded enableDisabledMenusAndButtons = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded selectActiveRegions = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded savedRegionSelection = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded selectionInhibitUpdateCommands = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugOnEvent = new ELispSymbol.Value.Forwarded((Object) SIGUSR2);
    public static ELispSymbol.Value.Forwarded attemptStackOverflowRecovery = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded attemptOrderlyShutdownOnFatalSignal = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded whileNoInputIgnoreEvents = new ELispSymbol.Value.Forwarded(false);
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
    /* @generated region="casefiddle.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded regionExtractFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded caseSymbolsAsWords = new ELispSymbol.Value.Forwarded(false);

    private static void casefiddleVars() {
        REGION_EXTRACT_FUNCTION.forwardTo(regionExtractFunction);
        CASE_SYMBOLS_AS_WORDS.forwardTo(caseSymbolsAsWords);
    }
    private static void casefiddlePostInitVars() {
        CASE_SYMBOLS_AS_WORDS.setBufferLocal(true);
    }
    /* @end region="casefiddle.c" */
    /* @generated region="coding.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded codingSystemList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded codingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded codingCategoryList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded codingSystemForRead = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded codingSystemForWrite = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lastCodingSystemUsed = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lastCodeConversionError = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitEolConversion = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inheritProcessCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded fileCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded processCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded networkCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded localeCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded eolMnemonicUnix = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded eolMnemonicDos = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded eolMnemonicMac = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded eolMnemonicUndecided = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded enableCharacterTranslation = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded standardTranslationTableForDecode = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded standardTranslationTableForEncode = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded charsetRevisionTable = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded defaultProcessCodingSystem = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded latinExtraCodeTable = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded selectSafeCodingSystemFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded codingSystemRequireWarning = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitIsoEscapeDetection = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitNullByteDetection = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded disableAsciiOptimization = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded translationTableForInput = new ELispSymbol.Value.Forwarded(false);

    private static void codingVars() {
        CODING_SYSTEM_LIST.forwardTo(codingSystemList);
        CODING_SYSTEM_ALIST.forwardTo(codingSystemAlist);
        CODING_CATEGORY_LIST.forwardTo(codingCategoryList);
        CODING_SYSTEM_FOR_READ.forwardTo(codingSystemForRead);
        CODING_SYSTEM_FOR_WRITE.forwardTo(codingSystemForWrite);
        LAST_CODING_SYSTEM_USED.forwardTo(lastCodingSystemUsed);
        LAST_CODE_CONVERSION_ERROR.forwardTo(lastCodeConversionError);
        INHIBIT_EOL_CONVERSION.forwardTo(inhibitEolConversion);
        INHERIT_PROCESS_CODING_SYSTEM.forwardTo(inheritProcessCodingSystem);
        FILE_CODING_SYSTEM_ALIST.forwardTo(fileCodingSystemAlist);
        PROCESS_CODING_SYSTEM_ALIST.forwardTo(processCodingSystemAlist);
        NETWORK_CODING_SYSTEM_ALIST.forwardTo(networkCodingSystemAlist);
        LOCALE_CODING_SYSTEM.forwardTo(localeCodingSystem);
        EOL_MNEMONIC_UNIX.forwardTo(eolMnemonicUnix);
        EOL_MNEMONIC_DOS.forwardTo(eolMnemonicDos);
        EOL_MNEMONIC_MAC.forwardTo(eolMnemonicMac);
        EOL_MNEMONIC_UNDECIDED.forwardTo(eolMnemonicUndecided);
        ENABLE_CHARACTER_TRANSLATION.forwardTo(enableCharacterTranslation);
        STANDARD_TRANSLATION_TABLE_FOR_DECODE.forwardTo(standardTranslationTableForDecode);
        STANDARD_TRANSLATION_TABLE_FOR_ENCODE.forwardTo(standardTranslationTableForEncode);
        CHARSET_REVISION_TABLE.forwardTo(charsetRevisionTable);
        DEFAULT_PROCESS_CODING_SYSTEM.forwardTo(defaultProcessCodingSystem);
        LATIN_EXTRA_CODE_TABLE.forwardTo(latinExtraCodeTable);
        SELECT_SAFE_CODING_SYSTEM_FUNCTION.forwardTo(selectSafeCodingSystemFunction);
        CODING_SYSTEM_REQUIRE_WARNING.forwardTo(codingSystemRequireWarning);
        INHIBIT_ISO_ESCAPE_DETECTION.forwardTo(inhibitIsoEscapeDetection);
        INHIBIT_NULL_BYTE_DETECTION.forwardTo(inhibitNullByteDetection);
        DISABLE_ASCII_OPTIMIZATION.forwardTo(disableAsciiOptimization);
        TRANSLATION_TABLE_FOR_INPUT.forwardTo(translationTableForInput);
    }
    private static void codingPostInitVars() {
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
        ELispVector codingCategoryTable = new ELispVector(Collections.nCopies(CODING_CATEGORY_MAX, false));
        codingCategoryTable.set(CODING_CATEGORY_ISO_7, ELispContext.intern("coding-category-iso-7"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_7_TIGHT, ELispContext.intern("coding-category-iso-7-tight"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_8_1, ELispContext.intern("coding-category-iso-8-1"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_8_2, ELispContext.intern("coding-category-iso-8-2"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_7_ELSE, ELispContext.intern("coding-category-iso-7-else"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_8_ELSE, ELispContext.intern("coding-category-iso-8-else"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_8_AUTO, ELispContext.intern("coding-category-utf-8-auto"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_8_NOSIG, ELispContext.intern("coding-category-utf-8"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_8_SIG, ELispContext.intern("coding-category-utf-8-sig"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_BE, ELispContext.intern("coding-category-utf-16-be"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_AUTO, ELispContext.intern("coding-category-utf-16-auto"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_LE, ELispContext.intern("coding-category-utf-16-le"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_BE_NOSIG, ELispContext.intern("coding-category-utf-16-be-nosig"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_LE_NOSIG, ELispContext.intern("coding-category-utf-16-le-nosig"));
        codingCategoryTable.set(CODING_CATEGORY_CHARSET, ELispContext.intern("coding-category-charset"));
        codingCategoryTable.set(CODING_CATEGORY_SJIS, ELispContext.intern("coding-category-sjis"));
        codingCategoryTable.set(CODING_CATEGORY_BIG5, ELispContext.intern("coding-category-big5"));
        codingCategoryTable.set(CODING_CATEGORY_CCL, ELispContext.intern("coding-category-ccl"));
        codingCategoryTable.set(CODING_CATEGORY_EMACS_MULE, ELispContext.intern("coding-category-emacs-mule"));
        codingCategoryTable.set(CODING_CATEGORY_RAW_TEXT, ELispContext.intern("coding-category-raw-text"));
        codingCategoryTable.set(CODING_CATEGORY_UNDECIDED, ELispContext.intern("coding-category-undecided"));
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
    /* @end region="coding.c" */
    /* @generated region="character.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded translationTableVector = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoFillChars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded charWidthTable = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded ambiguousWidthChars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded printableChars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded charScriptTable = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded scriptRepresentativeChars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded unicodeCategoryTable = new ELispSymbol.Value.Forwarded(false);

    private static void characterVars() {
        TRANSLATION_TABLE_VECTOR.forwardTo(translationTableVector);
        AUTO_FILL_CHARS.forwardTo(autoFillChars);
        CHAR_WIDTH_TABLE.forwardTo(charWidthTable);
        AMBIGUOUS_WIDTH_CHARS.forwardTo(ambiguousWidthChars);
        PRINTABLE_CHARS.forwardTo(printableChars);
        CHAR_SCRIPT_TABLE.forwardTo(charScriptTable);
        SCRIPT_REPRESENTATIVE_CHARS.forwardTo(scriptRepresentativeChars);
        UNICODE_CATEGORY_TABLE.forwardTo(unicodeCategoryTable);
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
    /* @end region="character.c" */
    /* @generated region="window.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded tempBufferShowFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded minibufScrollWindow = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded modeLineInNonSelectedWindows = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded otherWindowScrollBuffer = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded otherWindowScrollDefault = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoWindowVscrollP = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded nextScreenContextLines = new ELispSymbol.Value.Forwarded((long) 2);
    public static ELispSymbol.Value.Forwarded scrollPreserveScreenPosition = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowPointInsertionType = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowBufferChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowSizeChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowSelectionChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowStateChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowStateChangeHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowConfigurationChangeHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowRestoreKilledBufferWindows = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded recenterRedisplay = new ELispSymbol.Value.Forwarded((Object) TTY);
    public static ELispSymbol.Value.Forwarded windowCombinationResize = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowCombinationLimit = new ELispSymbol.Value.Forwarded((Object) WINDOW_SIZE);
    public static ELispSymbol.Value.Forwarded windowPersistentParameters = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowResizePixelwise = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded fastButImpreciseScrolling = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowDeadWindowsTable = new ELispSymbol.Value.Forwarded(false);

    private static void windowVars() {
        TEMP_BUFFER_SHOW_FUNCTION.forwardTo(tempBufferShowFunction);
        MINIBUFFER_SCROLL_WINDOW.forwardTo(minibufScrollWindow);
        MODE_LINE_IN_NON_SELECTED_WINDOWS.forwardTo(modeLineInNonSelectedWindows);
        OTHER_WINDOW_SCROLL_BUFFER.forwardTo(otherWindowScrollBuffer);
        OTHER_WINDOW_SCROLL_DEFAULT.forwardTo(otherWindowScrollDefault);
        AUTO_WINDOW_VSCROLL.forwardTo(autoWindowVscrollP);
        NEXT_SCREEN_CONTEXT_LINES.forwardTo(nextScreenContextLines);
        SCROLL_PRESERVE_SCREEN_POSITION.forwardTo(scrollPreserveScreenPosition);
        WINDOW_POINT_INSERTION_TYPE.forwardTo(windowPointInsertionType);
        WINDOW_BUFFER_CHANGE_FUNCTIONS.forwardTo(windowBufferChangeFunctions);
        WINDOW_SIZE_CHANGE_FUNCTIONS.forwardTo(windowSizeChangeFunctions);
        WINDOW_SELECTION_CHANGE_FUNCTIONS.forwardTo(windowSelectionChangeFunctions);
        WINDOW_STATE_CHANGE_FUNCTIONS.forwardTo(windowStateChangeFunctions);
        WINDOW_STATE_CHANGE_HOOK.forwardTo(windowStateChangeHook);
        WINDOW_CONFIGURATION_CHANGE_HOOK.forwardTo(windowConfigurationChangeHook);
        WINDOW_RESTORE_KILLED_BUFFER_WINDOWS.forwardTo(windowRestoreKilledBufferWindows);
        RECENTER_REDISPLAY.forwardTo(recenterRedisplay);
        WINDOW_COMBINATION_RESIZE.forwardTo(windowCombinationResize);
        WINDOW_COMBINATION_LIMIT.forwardTo(windowCombinationLimit);
        WINDOW_PERSISTENT_PARAMETERS.forwardTo(windowPersistentParameters);
        WINDOW_RESIZE_PIXELWISE.forwardTo(windowResizePixelwise);
        FAST_BUT_IMPRECISE_SCROLLING.forwardTo(fastButImpreciseScrolling);
        WINDOW_DEAD_WINDOWS_TABLE.forwardTo(windowDeadWindowsTable);
    }
    private static void windowPostInitVars() {
        SCROLL_UP.putProperty(SCROLL_COMMAND, T);
        SCROLL_DOWN.putProperty(SCROLL_COMMAND, T);
        var windowPersistentParametersJInit = new ELispCons(new ELispCons(CLONE_OF, T));
        windowPersistentParameters.setValue(windowPersistentParametersJInit);
        var windowDeadWindowsTableJInit = FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, T});
        windowDeadWindowsTable.setValue(windowDeadWindowsTableJInit);
    }
    /* @end region="window.c" */
    /* @generated region="frame.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded xResourceName = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded xResourceClass = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded frameAlphaLowerLimit = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded defaultFrameAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded defaultFrameScrollBars = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded scrollBarAdjustThumbPortionP = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded terminalFrame = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded mousePositionFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded mouseHighlight = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded makePointerInvisible = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded moveFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded deleteFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded afterDeleteFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded menuBarMode = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded tabBarMode = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded toolBarMode = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded defaultMinibufferFrame = new ELispSymbol.Value.Forwarded((Object) false /* TODO */);
    public static ELispSymbol.Value.Forwarded resizeMiniFrames = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded focusFollowsMouse = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded frameResizePixelwise = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded frameInhibitImpliedResize = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded frameSizeHistory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded tooltipReuseHiddenFrame = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded useSystemTooltips = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded iconifyChildFrame = new ELispSymbol.Value.Forwarded((Object) ICONIFY_TOP_LEVEL);
    public static ELispSymbol.Value.Forwarded frameInternalParameters = new ELispSymbol.Value.Forwarded(false);

    private static void frameVars() {
        X_RESOURCE_NAME.forwardTo(xResourceName);
        X_RESOURCE_CLASS.forwardTo(xResourceClass);
        FRAME_ALPHA_LOWER_LIMIT.forwardTo(frameAlphaLowerLimit);
        DEFAULT_FRAME_ALIST.forwardTo(defaultFrameAlist);
        DEFAULT_FRAME_SCROLL_BARS.forwardTo(defaultFrameScrollBars);
        SCROLL_BAR_ADJUST_THUMB_PORTION.forwardTo(scrollBarAdjustThumbPortionP);
        TERMINAL_FRAME.forwardTo(terminalFrame);
        MOUSE_POSITION_FUNCTION.forwardTo(mousePositionFunction);
        MOUSE_HIGHLIGHT.forwardTo(mouseHighlight);
        MAKE_POINTER_INVISIBLE.forwardTo(makePointerInvisible);
        MOVE_FRAME_FUNCTIONS.forwardTo(moveFrameFunctions);
        DELETE_FRAME_FUNCTIONS.forwardTo(deleteFrameFunctions);
        AFTER_DELETE_FRAME_FUNCTIONS.forwardTo(afterDeleteFrameFunctions);
        MENU_BAR_MODE.forwardTo(menuBarMode);
        TAB_BAR_MODE.forwardTo(tabBarMode);
        TOOL_BAR_MODE.forwardTo(toolBarMode);
        DEFAULT_MINIBUFFER_FRAME.forwardTo(defaultMinibufferFrame);
        RESIZE_MINI_FRAMES.forwardTo(resizeMiniFrames);
        FOCUS_FOLLOWS_MOUSE.forwardTo(focusFollowsMouse);
        FRAME_RESIZE_PIXELWISE.forwardTo(frameResizePixelwise);
        FRAME_INHIBIT_IMPLIED_RESIZE.forwardTo(frameInhibitImpliedResize);
        FRAME_SIZE_HISTORY.forwardTo(frameSizeHistory);
        TOOLTIP_REUSE_HIDDEN_FRAME.forwardTo(tooltipReuseHiddenFrame);
        USE_SYSTEM_TOOLTIPS.forwardTo(useSystemTooltips);
        ICONIFY_CHILD_FRAME.forwardTo(iconifyChildFrame);
        FRAME_INTERNAL_PARAMETERS.forwardTo(frameInternalParameters);
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
    /* @end region="frame.c" */
    /* @generated region="textprop.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded defaultTextProperties = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded charPropertyAliasAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitPointMotionHooks = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded textPropertyDefaultNonsticky = new ELispSymbol.Value.Forwarded(false);

    private static void textpropVars() {
        DEFAULT_TEXT_PROPERTIES.forwardTo(defaultTextProperties);
        CHAR_PROPERTY_ALIAS_ALIST.forwardTo(charPropertyAliasAlist);
        INHIBIT_POINT_MOTION_HOOKS.forwardTo(inhibitPointMotionHooks);
        TEXT_PROPERTY_DEFAULT_NONSTICKY.forwardTo(textPropertyDefaultNonsticky);
    }
    private static void textpropPostInitVars() {
        var textPropertyDefaultNonstickyJInit = ELispCons.listOf(new ELispCons(SYNTAX_TABLE, T), new ELispCons(DISPLAY, T));
        textPropertyDefaultNonsticky.setValue(textPropertyDefaultNonstickyJInit);
    }
    /* @end region="textprop.c" */
    /* @generated region="syntax.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded commentUseSyntaxPpss = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded parseSexpIgnoreComments = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded parseSexpLookupProperties = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded syntaxPropertizeDone = new ELispSymbol.Value.Forwarded((long) -1);
    public static ELispSymbol.Value.Forwarded wordsIncludeEscapes = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded multibyteSyntaxAsSymbol = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded openParenInColumn0IsDefunStart = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded findWordBoundaryFunctionTable = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded commentEndCanBeEscaped = new ELispSymbol.Value.Forwarded(false);

    private static void syntaxVars() {
        COMMENT_USE_SYNTAX_PPSS.forwardTo(commentUseSyntaxPpss);
        PARSE_SEXP_IGNORE_COMMENTS.forwardTo(parseSexpIgnoreComments);
        PARSE_SEXP_LOOKUP_PROPERTIES.forwardTo(parseSexpLookupProperties);
        SYNTAX_PROPERTIZE__DONE.forwardTo(syntaxPropertizeDone);
        WORDS_INCLUDE_ESCAPES.forwardTo(wordsIncludeEscapes);
        MULTIBYTE_SYNTAX_AS_SYMBOL.forwardTo(multibyteSyntaxAsSymbol);
        OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START.forwardTo(openParenInColumn0IsDefunStart);
        FIND_WORD_BOUNDARY_FUNCTION_TABLE.forwardTo(findWordBoundaryFunctionTable);
        COMMENT_END_CAN_BE_ESCAPED.forwardTo(commentEndCanBeEscaped);
    }
    private static void syntaxPostInitVars() {
        SCAN_ERROR.putProperty(ERROR_CONDITIONS, ELispCons.listOf(SCAN_ERROR, ERROR));
        SCAN_ERROR.putProperty(ERROR_MESSAGE, new ELispString("Scan error"));
        ELispContext.intern("syntax-propertize--done").setBufferLocal(true);
        var findWordBoundaryFunctionTableJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        findWordBoundaryFunctionTable.setValue(findWordBoundaryFunctionTableJInit);
        COMMENT_END_CAN_BE_ESCAPED.setBufferLocal(true);
    }
    /* @end region="syntax.c" */
    /* @generated region="xdisp.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded scrollMinibufferConservatively = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded inhibitMessage = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded messagesBufferName = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded xStretchCursorP = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded showTrailingWhitespace = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded modeLineCompact = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded nobreakCharDisplay = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded nobreakCharAsciiDisplay = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded voidTextAreaPointer = new ELispSymbol.Value.Forwarded((Object) ARROW);
    public static ELispSymbol.Value.Forwarded inhibitRedisplay = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded globalModeString = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded overlayArrowPosition = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded overlayArrowString = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded overlayArrowVariableList = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded emacsScrollStep = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded scrollConservatively = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded scrollMargin = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded maximumScrollMargin = new ELispSymbol.Value.Forwarded((Object) 0.25);
    public static ELispSymbol.Value.Forwarded displayPixelsPerInch = new ELispSymbol.Value.Forwarded((Object) 72.0);
    public static ELispSymbol.Value.Forwarded debugEndPos = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded truncatePartialWidthWindows = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded wordWrapByCategory = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lineNumberDisplayLimit = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded lineNumberDisplayLimitWidth = new ELispSymbol.Value.Forwarded((long) 200);
    public static ELispSymbol.Value.Forwarded highlightNonselectedWindows = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded multipleFrames = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded frameTitleFormat = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded iconTitleFormat = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded messageLogMax = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded windowScrollFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded mouseAutoselectWindow = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded autoResizeTabBars = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded autoRaiseTabBarButtonsP = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded autoResizeToolBars = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded autoRaiseToolBarButtonsP = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded makeCursorLineFullyVisible = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded makeWindowStartVisible = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded tabBarBorder = new ELispSymbol.Value.Forwarded((Object) INTERNAL_BORDER_WIDTH);
    public static ELispSymbol.Value.Forwarded tabBarButtonMargin = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded tabBarButtonRelief = new ELispSymbol.Value.Forwarded((long) 1);
    public static ELispSymbol.Value.Forwarded toolBarBorder = new ELispSymbol.Value.Forwarded((Object) INTERNAL_BORDER_WIDTH);
    public static ELispSymbol.Value.Forwarded toolBarButtonMargin = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded toolBarButtonRelief = new ELispSymbol.Value.Forwarded((long) 1);
    public static ELispSymbol.Value.Forwarded toolBarStyle = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded toolBarMaxLabelSize = new ELispSymbol.Value.Forwarded((long) 14);
    public static ELispSymbol.Value.Forwarded fontificationFunctions = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded unibyteDisplayViaLanguageEnvironment = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded maxMiniWindowHeight = new ELispSymbol.Value.Forwarded((Object) 0.25);
    public static ELispSymbol.Value.Forwarded resizeMiniWindows = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded blinkCursorAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded automaticHscrolling = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded hscrollMargin = new ELispSymbol.Value.Forwarded((long) 5);
    public static ELispSymbol.Value.Forwarded hscrollStep = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded messageTruncateLines = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded menuBarUpdateHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded menuUpdatingFrame = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitMenubarUpdate = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded wrapPrefix = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded linePrefix = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded displayLineNumbers = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded displayLineNumbersWidth = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded displayLineNumbersCurrentAbsolute = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded displayLineNumbersWiden = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded displayLineNumbersOffset = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded displayFillColumnIndicator = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded displayFillColumnIndicatorColumn = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded displayFillColumnIndicatorCharacter = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded displayLineNumbersMajorTick = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded displayLineNumbersMinorTick = new ELispSymbol.Value.Forwarded((long) 0);
    public static ELispSymbol.Value.Forwarded inhibitEvalDuringRedisplay = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitFreeRealizedFaces = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitBidiMirroring = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded bidiInhibitBpa = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitTryWindowId = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitTryWindowReusing = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitTryCursorMovement = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded overlineMargin = new ELispSymbol.Value.Forwarded((long) 2);
    public static ELispSymbol.Value.Forwarded underlineMinimumOffset = new ELispSymbol.Value.Forwarded((long) 1);
    public static ELispSymbol.Value.Forwarded displayHourglassP = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded hourglassDelay = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded preRedisplayFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded glyphlessCharDisplay = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded debugOnMessage = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded setMessageFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded clearMessageFunction = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded redisplayAllWindowsCause = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded redisplayModeLinesCause = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded redisplayInhibitBidi = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded displayRawBytesAsHex = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded mouseFineGrainedTracking = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded tabBarDraggingInProgress = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded redisplaySkipInitialFrame = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded redisplaySkipFontificationOnInput = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded redisplayAdhocScrollInResizeMiniWindows = new ELispSymbol.Value.Forwarded(true);
    public static ELispSymbol.Value.Forwarded compositionBreakAtPoint = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded maxRedisplayTicks = new ELispSymbol.Value.Forwarded((long) 0);

    private static void xdispVars() {
        SCROLL_MINIBUFFER_CONSERVATIVELY.forwardTo(scrollMinibufferConservatively);
        INHIBIT_MESSAGE.forwardTo(inhibitMessage);
        MESSAGES_BUFFER_NAME.forwardTo(messagesBufferName);
        X_STRETCH_CURSOR.forwardTo(xStretchCursorP);
        SHOW_TRAILING_WHITESPACE.forwardTo(showTrailingWhitespace);
        MODE_LINE_COMPACT.forwardTo(modeLineCompact);
        NOBREAK_CHAR_DISPLAY.forwardTo(nobreakCharDisplay);
        NOBREAK_CHAR_ASCII_DISPLAY.forwardTo(nobreakCharAsciiDisplay);
        VOID_TEXT_AREA_POINTER.forwardTo(voidTextAreaPointer);
        INHIBIT_REDISPLAY.forwardTo(inhibitRedisplay);
        GLOBAL_MODE_STRING.forwardTo(globalModeString);
        OVERLAY_ARROW_POSITION.forwardTo(overlayArrowPosition);
        OVERLAY_ARROW_STRING.forwardTo(overlayArrowString);
        OVERLAY_ARROW_VARIABLE_LIST.forwardTo(overlayArrowVariableList);
        SCROLL_STEP.forwardTo(emacsScrollStep);
        SCROLL_CONSERVATIVELY.forwardTo(scrollConservatively);
        SCROLL_MARGIN.forwardTo(scrollMargin);
        MAXIMUM_SCROLL_MARGIN.forwardTo(maximumScrollMargin);
        DISPLAY_PIXELS_PER_INCH.forwardTo(displayPixelsPerInch);
        DEBUG_END_POS.forwardTo(debugEndPos);
        TRUNCATE_PARTIAL_WIDTH_WINDOWS.forwardTo(truncatePartialWidthWindows);
        WORD_WRAP_BY_CATEGORY.forwardTo(wordWrapByCategory);
        LINE_NUMBER_DISPLAY_LIMIT.forwardTo(lineNumberDisplayLimit);
        LINE_NUMBER_DISPLAY_LIMIT_WIDTH.forwardTo(lineNumberDisplayLimitWidth);
        HIGHLIGHT_NONSELECTED_WINDOWS.forwardTo(highlightNonselectedWindows);
        MULTIPLE_FRAMES.forwardTo(multipleFrames);
        FRAME_TITLE_FORMAT.forwardTo(frameTitleFormat);
        ICON_TITLE_FORMAT.forwardTo(iconTitleFormat);
        MESSAGE_LOG_MAX.forwardTo(messageLogMax);
        WINDOW_SCROLL_FUNCTIONS.forwardTo(windowScrollFunctions);
        MOUSE_AUTOSELECT_WINDOW.forwardTo(mouseAutoselectWindow);
        AUTO_RESIZE_TAB_BARS.forwardTo(autoResizeTabBars);
        AUTO_RAISE_TAB_BAR_BUTTONS.forwardTo(autoRaiseTabBarButtonsP);
        AUTO_RESIZE_TOOL_BARS.forwardTo(autoResizeToolBars);
        AUTO_RAISE_TOOL_BAR_BUTTONS.forwardTo(autoRaiseToolBarButtonsP);
        MAKE_CURSOR_LINE_FULLY_VISIBLE.forwardTo(makeCursorLineFullyVisible);
        MAKE_WINDOW_START_VISIBLE.forwardTo(makeWindowStartVisible);
        TAB_BAR_BORDER.forwardTo(tabBarBorder);
        TAB_BAR_BUTTON_MARGIN.forwardTo(tabBarButtonMargin);
        TAB_BAR_BUTTON_RELIEF.forwardTo(tabBarButtonRelief);
        TOOL_BAR_BORDER.forwardTo(toolBarBorder);
        TOOL_BAR_BUTTON_MARGIN.forwardTo(toolBarButtonMargin);
        TOOL_BAR_BUTTON_RELIEF.forwardTo(toolBarButtonRelief);
        TOOL_BAR_STYLE.forwardTo(toolBarStyle);
        TOOL_BAR_MAX_LABEL_SIZE.forwardTo(toolBarMaxLabelSize);
        FONTIFICATION_FUNCTIONS.forwardTo(fontificationFunctions);
        UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT.forwardTo(unibyteDisplayViaLanguageEnvironment);
        MAX_MINI_WINDOW_HEIGHT.forwardTo(maxMiniWindowHeight);
        RESIZE_MINI_WINDOWS.forwardTo(resizeMiniWindows);
        BLINK_CURSOR_ALIST.forwardTo(blinkCursorAlist);
        AUTO_HSCROLL_MODE.forwardTo(automaticHscrolling);
        HSCROLL_MARGIN.forwardTo(hscrollMargin);
        HSCROLL_STEP.forwardTo(hscrollStep);
        MESSAGE_TRUNCATE_LINES.forwardTo(messageTruncateLines);
        MENU_BAR_UPDATE_HOOK.forwardTo(menuBarUpdateHook);
        MENU_UPDATING_FRAME.forwardTo(menuUpdatingFrame);
        INHIBIT_MENUBAR_UPDATE.forwardTo(inhibitMenubarUpdate);
        WRAP_PREFIX.forwardTo(wrapPrefix);
        LINE_PREFIX.forwardTo(linePrefix);
        DISPLAY_LINE_NUMBERS.forwardTo(displayLineNumbers);
        DISPLAY_LINE_NUMBERS_WIDTH.forwardTo(displayLineNumbersWidth);
        DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE.forwardTo(displayLineNumbersCurrentAbsolute);
        DISPLAY_LINE_NUMBERS_WIDEN.forwardTo(displayLineNumbersWiden);
        DISPLAY_LINE_NUMBERS_OFFSET.forwardTo(displayLineNumbersOffset);
        DISPLAY_FILL_COLUMN_INDICATOR.forwardTo(displayFillColumnIndicator);
        DISPLAY_FILL_COLUMN_INDICATOR_COLUMN.forwardTo(displayFillColumnIndicatorColumn);
        DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER.forwardTo(displayFillColumnIndicatorCharacter);
        DISPLAY_LINE_NUMBERS_MAJOR_TICK.forwardTo(displayLineNumbersMajorTick);
        DISPLAY_LINE_NUMBERS_MINOR_TICK.forwardTo(displayLineNumbersMinorTick);
        INHIBIT_EVAL_DURING_REDISPLAY.forwardTo(inhibitEvalDuringRedisplay);
        INHIBIT_FREE_REALIZED_FACES.forwardTo(inhibitFreeRealizedFaces);
        INHIBIT_BIDI_MIRRORING.forwardTo(inhibitBidiMirroring);
        BIDI_INHIBIT_BPA.forwardTo(bidiInhibitBpa);
        INHIBIT_TRY_WINDOW_ID.forwardTo(inhibitTryWindowId);
        INHIBIT_TRY_WINDOW_REUSING.forwardTo(inhibitTryWindowReusing);
        INHIBIT_TRY_CURSOR_MOVEMENT.forwardTo(inhibitTryCursorMovement);
        OVERLINE_MARGIN.forwardTo(overlineMargin);
        UNDERLINE_MINIMUM_OFFSET.forwardTo(underlineMinimumOffset);
        DISPLAY_HOURGLASS.forwardTo(displayHourglassP);
        HOURGLASS_DELAY.forwardTo(hourglassDelay);
        PRE_REDISPLAY_FUNCTION.forwardTo(preRedisplayFunction);
        GLYPHLESS_CHAR_DISPLAY.forwardTo(glyphlessCharDisplay);
        DEBUG_ON_MESSAGE.forwardTo(debugOnMessage);
        SET_MESSAGE_FUNCTION.forwardTo(setMessageFunction);
        CLEAR_MESSAGE_FUNCTION.forwardTo(clearMessageFunction);
        REDISPLAY__ALL_WINDOWS_CAUSE.forwardTo(redisplayAllWindowsCause);
        REDISPLAY__MODE_LINES_CAUSE.forwardTo(redisplayModeLinesCause);
        REDISPLAY__INHIBIT_BIDI.forwardTo(redisplayInhibitBidi);
        DISPLAY_RAW_BYTES_AS_HEX.forwardTo(displayRawBytesAsHex);
        MOUSE_FINE_GRAINED_TRACKING.forwardTo(mouseFineGrainedTracking);
        TAB_BAR__DRAGGING_IN_PROGRESS.forwardTo(tabBarDraggingInProgress);
        REDISPLAY_SKIP_INITIAL_FRAME.forwardTo(redisplaySkipInitialFrame);
        REDISPLAY_SKIP_FONTIFICATION_ON_INPUT.forwardTo(redisplaySkipFontificationOnInput);
        REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS.forwardTo(redisplayAdhocScrollInResizeMiniWindows);
        COMPOSITION_BREAK_AT_POINT.forwardTo(compositionBreakAtPoint);
        MAX_REDISPLAY_TICKS.forwardTo(maxRedisplayTicks);
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
    /* @end region="xdisp.c" */
}
