package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInKeymap.*;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispHashtable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

@SuppressWarnings({"RedundantCast"})
public class ELispGlobals {
    public static void initGlobalVariables() {
        allocVars();
        bufferVars();
        charsetVars();
        chartabVars();
        compVars();
        dataVars();
        editfnsVars();
        emacsVars();
        evalVars();
        fileioVars();
        fnsVars();
        keymapVars();
        lreadVars();
        printVars();
        processVars();
        searchVars();
        timefnsVars();
        xfacesVars();
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
    /* @end region="alloc.c" */
    /* @generated region="chartab.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded charCodePropertyAlist = new ELispSymbol.Value.Forwarded(false);

    private static void chartabVars() {
        CHAR_CODE_PROPERTY_ALIST.forwardTo(charCodePropertyAlist);
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
    public static ELispSymbol.Value.Forwarded nativeCompElnLoadPath = new ELispSymbol.Value.Forwarded((Object) FCons.cons(new ELispString("../native-lisp/"), NIL));
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
    /* @end region="comp.c" */
    /* @generated region="data.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded mostPositiveFixnum = new ELispSymbol.Value.Forwarded((Object) Long.MAX_VALUE);
    public static ELispSymbol.Value.Forwarded mostNegativeFixnum = new ELispSymbol.Value.Forwarded((Object) Long.MIN_VALUE);
    public static ELispSymbol.Value.Forwarded symbolsWithPosEnabled = new ELispSymbol.Value.Forwarded(false);

    private static void dataVars() {
        MOST_POSITIVE_FIXNUM.forwardTo(mostPositiveFixnum);
        MOST_NEGATIVE_FIXNUM.forwardTo(mostNegativeFixnum);
        SYMBOLS_WITH_POS_ENABLED.forwardTo(symbolsWithPosEnabled);
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
    /* @end region="fns.c" */
    /* @generated region="lread.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded obarray = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded values = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded standardInput = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded readCircle = new ELispSymbol.Value.Forwarded((Object) T);
    public static ELispSymbol.Value.Forwarded loadPath = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded loadSuffixes = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded moduleFileSuffix = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded dynamicLibrarySuffixes = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
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
    /* @end region="editfns.c" */
    /* @generated region="emacs.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded commandLineArgs = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
    public static ELispSymbol.Value.Forwarded systemType = new ELispSymbol.Value.Forwarded((Object) new ELispString("jvm"));
    public static ELispSymbol.Value.Forwarded systemConfiguration = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded systemConfigurationOptions = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded systemConfigurationFeatures = new ELispSymbol.Value.Forwarded((Object) new ELispString(""));
    public static ELispSymbol.Value.Forwarded noninteractive1 = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded killEmacsHook = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded pathSeparator = new ELispSymbol.Value.Forwarded((Object) new ELispString("/"));
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
    public static ELispSymbol.Value.Forwarded dynamicLibraryAlist = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);

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
    /* @end region="emacs.c" */
    /* @generated region="search.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded searchSpacesRegexp = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded inhibitChangingMatchData = new ELispSymbol.Value.Forwarded(false);

    private static void searchVars() {
        SEARCH_SPACES_REGEXP.forwardTo(searchSpacesRegexp);
        INHIBIT_CHANGING_MATCH_DATA.forwardTo(inhibitChangingMatchData);
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
    public static ELispSymbol.Value.Forwarded longLineThreshold = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
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
    /* @end region="print.c" */
    /* @generated region="xfaces.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded faceFiltersAlwaysMatch = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceNewFrameDefaults = new ELispSymbol.Value.Forwarded((Object) new ELispHashtable());
    public static ELispSymbol.Value.Forwarded faceDefaultStipple = new ELispSymbol.Value.Forwarded((Object) new ELispString("gray3"));
    public static ELispSymbol.Value.Forwarded ttyDefinedColorAlist = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded scalableFontsAllowed = new ELispSymbol.Value.Forwarded(false);
    public static ELispSymbol.Value.Forwarded faceIgnoredFonts = new ELispSymbol.Value.Forwarded((Object) false /* uninitialized */);
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
    /* @end region="xfaces.c" */
    /* @generated region="timefns.c" by="extract-emacs-src.py" */
    public static ELispSymbol.Value.Forwarded currentTimeList = new ELispSymbol.Value.Forwarded(true);

    private static void timefnsVars() {
        CURRENT_TIME_LIST.forwardTo(currentTimeList);
    }
    /* @end region="timefns.c" */
}
