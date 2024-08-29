package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFileIO.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public class ELispGlobals {
    public static void initGlobalVariables() {
        allocVars();
        charsetVars();
        chartabVars();
        compVars();
        dataVars();
        editfnsVars();
        evalVars();
        fileioVars();
        fnsVars();
        lreadVars();
        processVars();
    }

    /* @generated region="alloc.c" by="extract-emacs-src.py" */
    public static long gcConsThreshold = 0;
    public static Object gcConsPercentage = 0.1;
    public static long pureBytesUsed = 0;
    public static long consCellsConsed = 0;
    public static long floatsConsed = 0;
    public static long vectorCellsConsed = 0;
    public static long symbolsConsed = 0;
    public static long stringCharsConsed = 0;
    public static long intervalsConsed = 0;
    public static long stringsConsed = 0;
    public static Object purifyFlag = NIL /* uninitialized */;
    public static boolean garbageCollectionMessages = false;
    public static Object postGcHook = NIL;
    public static Object memorySignalData = ELispCons.of(ERROR, new ELispString("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs"));
    public static Object memoryFull = NIL;
    public static Object gcElapsed = NIL /* uninitialized */;
    public static long gcsDone = 0;
    public static long integerWidth = 0;

    private static void allocVars() {
        GC_CONS_THRESHOLD.forwardTo(() -> gcConsThreshold);
        GC_CONS_PERCENTAGE.forwardTo(() -> gcConsPercentage);
        PURE_BYTES_USED.forwardTo(() -> pureBytesUsed);
        CONS_CELLS_CONSED.forwardTo(() -> consCellsConsed);
        FLOATS_CONSED.forwardTo(() -> floatsConsed);
        VECTOR_CELLS_CONSED.forwardTo(() -> vectorCellsConsed);
        SYMBOLS_CONSED.forwardTo(() -> symbolsConsed);
        STRING_CHARS_CONSED.forwardTo(() -> stringCharsConsed);
        INTERVALS_CONSED.forwardTo(() -> intervalsConsed);
        STRINGS_CONSED.forwardTo(() -> stringsConsed);
        PURIFY_FLAG.forwardTo(() -> purifyFlag);
        GARBAGE_COLLECTION_MESSAGES.forwardTo(() -> garbageCollectionMessages);
        POST_GC_HOOK.forwardTo(() -> postGcHook);
        MEMORY_SIGNAL_DATA.forwardTo(() -> memorySignalData);
        MEMORY_FULL.forwardTo(() -> memoryFull);
        GC_ELAPSED.forwardTo(() -> gcElapsed);
        GCS_DONE.forwardTo(() -> gcsDone);
        INTEGER_WIDTH.forwardTo(() -> integerWidth);
    }
    /* @end region="alloc.c" */
    /* @generated region="chartab.c" by="extract-emacs-src.py" */
    public static Object charCodePropertyAlist = NIL;

    private static void chartabVars() {
        CHAR_CODE_PROPERTY_ALIST.forwardTo(() -> charCodePropertyAlist);
    }
    /* @end region="chartab.c" */
    /* @generated region="comp.c" by="extract-emacs-src.py" */
    public static boolean nativeCompJitCompilation = true;
    public static Object compCtxt = NIL;
    public static Object compSubrList = NIL /* uninitialized */;
    public static Object compAbiHash = NIL;
    public static Object compNativeVersionDir = NIL;
    public static Object compDeferredPendingH = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
    public static Object compElnToElH = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
    public static Object nativeCompElnLoadPath = FCons.cons(new ELispString("../native-lisp/"), NIL);
    public static Object nativeCompEnableSubrTrampolines = NIL /* uninitialized */;
    public static Object compInstalledTrampolinesH = FMakeHashTable.makeHashTable(new Object[]{});
    public static Object compNoNativeFileH = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
    public static boolean compFilePreloadedP = false;
    public static Object compLoadedCompUnitsH = FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, VALUE, CTEST, EQUAL});
    public static Object compSubrAritiesH = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
    public static boolean compSanitizerActive = false;

    private static void compVars() {
        NATIVE_COMP_JIT_COMPILATION.forwardTo(() -> nativeCompJitCompilation);
        COMP_CTXT.forwardTo(() -> compCtxt);
        COMP_SUBR_LIST.forwardTo(() -> compSubrList);
        COMP_ABI_HASH.forwardTo(() -> compAbiHash);
        COMP_NATIVE_VERSION_DIR.forwardTo(() -> compNativeVersionDir);
        COMP_DEFERRED_PENDING_H.forwardTo(() -> compDeferredPendingH);
        COMP_ELN_TO_EL_H.forwardTo(() -> compElnToElH);
        NATIVE_COMP_ELN_LOAD_PATH.forwardTo(() -> nativeCompElnLoadPath);
        NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES.forwardTo(() -> nativeCompEnableSubrTrampolines);
        COMP_INSTALLED_TRAMPOLINES_H.forwardTo(() -> compInstalledTrampolinesH);
        COMP_NO_NATIVE_FILE_H.forwardTo(() -> compNoNativeFileH);
        COMP_FILE_PRELOADED_P.forwardTo(() -> compFilePreloadedP);
        COMP_LOADED_COMP_UNITS_H.forwardTo(() -> compLoadedCompUnitsH);
        COMP_SUBR_ARITIES_H.forwardTo(() -> compSubrAritiesH);
        COMP_SANITIZER_ACTIVE.forwardTo(() -> compSanitizerActive);
    }
    /* @end region="comp.c" */
    /* @generated region="data.c" by="extract-emacs-src.py" */
    public static Object mostPositiveFixnum = Long.MAX_VALUE;
    public static Object mostNegativeFixnum = Long.MIN_VALUE;
    public static boolean symbolsWithPosEnabled = false;

    private static void dataVars() {
        MOST_POSITIVE_FIXNUM.forwardTo(() -> mostPositiveFixnum);
        MOST_NEGATIVE_FIXNUM.forwardTo(() -> mostNegativeFixnum);
        SYMBOLS_WITH_POS_ENABLED.forwardTo(() -> symbolsWithPosEnabled);
    }
    /* @end region="data.c" */
    /* @generated region="eval.c" by="extract-emacs-src.py" */
    public static long maxLispEvalDepth = 1600;
    public static long lispEvalDepthReserve = 200;
    public static Object quitFlag = NIL;
    public static Object inhibitQuit = NIL;
    public static Object inhibitDebugger = NIL;
    public static Object debugOnError = NIL;
    public static Object debugIgnoredErrors = NIL;
    public static boolean debugOnQuit = false;
    public static boolean debugOnNextCall = false;
    public static boolean backtraceOnRedisplayError = false;
    public static boolean debuggerMayContinue = true;
    public static boolean debuggerStackFrameAsList = false;
    public static Object debugger = DEBUG_EARLY;
    public static Object signalHookFunction = NIL;
    public static Object debugOnSignal = NIL;
    public static boolean backtraceOnErrorNoninteractive = true;
    public static long whenEnteredDebugger = 0;
    public static Object internalInterpreterEnvironment = NIL;
    public static Object internalMakeInterpretedClosureFunction = NIL;

    private static void evalVars() {
        MAX_LISP_EVAL_DEPTH.forwardTo(() -> maxLispEvalDepth);
        LISP_EVAL_DEPTH_RESERVE.forwardTo(() -> lispEvalDepthReserve);
        QUIT_FLAG.forwardTo(() -> quitFlag);
        INHIBIT_QUIT.forwardTo(() -> inhibitQuit);
        INHIBIT_DEBUGGER.forwardTo(() -> inhibitDebugger);
        DEBUG_ON_ERROR.forwardTo(() -> debugOnError);
        DEBUG_IGNORED_ERRORS.forwardTo(() -> debugIgnoredErrors);
        DEBUG_ON_QUIT.forwardTo(() -> debugOnQuit);
        DEBUG_ON_NEXT_CALL.forwardTo(() -> debugOnNextCall);
        BACKTRACE_ON_REDISPLAY_ERROR.forwardTo(() -> backtraceOnRedisplayError);
        DEBUGGER_MAY_CONTINUE.forwardTo(() -> debuggerMayContinue);
        DEBUGGER_STACK_FRAME_AS_LIST.forwardTo(() -> debuggerStackFrameAsList);
        DEBUGGER.forwardTo(() -> debugger);
        SIGNAL_HOOK_FUNCTION.forwardTo(() -> signalHookFunction);
        DEBUG_ON_SIGNAL.forwardTo(() -> debugOnSignal);
        BACKTRACE_ON_ERROR_NONINTERACTIVE.forwardTo(() -> backtraceOnErrorNoninteractive);
        INTERNAL_WHEN_ENTERED_DEBUGGER.forwardTo(() -> whenEnteredDebugger);
        INTERNAL_INTERPRETER_ENVIRONMENT.forwardTo(() -> internalInterpreterEnvironment);
        INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION.forwardTo(() -> internalMakeInterpretedClosureFunction);
    }
    /* @end region="eval.c" */
    /* @generated region="fns.c" by="extract-emacs-src.py" */
    public static Object overridingPlistEnvironment = NIL;
    public static Object features = new ELispCons(EMACS);
    public static boolean useDialogBox = true;
    public static boolean useFileDialog = true;
    public static boolean useShortAnswers = false;
    public static Object yesOrNoPrompt = new ELispString("(yes or no) ");

    private static void fnsVars() {
        OVERRIDING_PLIST_ENVIRONMENT.forwardTo(() -> overridingPlistEnvironment);
        FEATURES.forwardTo(() -> features);
        USE_DIALOG_BOX.forwardTo(() -> useDialogBox);
        USE_FILE_DIALOG.forwardTo(() -> useFileDialog);
        USE_SHORT_ANSWERS.forwardTo(() -> useShortAnswers);
        YES_OR_NO_PROMPT.forwardTo(() -> yesOrNoPrompt);
    }
    /* @end region="fns.c" */
    /* @generated region="lread.c" by="extract-emacs-src.py" */
    public static Object obarray = NIL /* uninitialized */;
    public static Object values = NIL /* uninitialized */;
    public static Object standardInput = T;
    public static Object readCircle = T;
    public static Object loadPath = NIL /* uninitialized */;
    public static Object loadSuffixes = NIL /* uninitialized */;
    public static Object moduleFileSuffix = NIL /* uninitialized */;
    public static Object dynamicLibrarySuffixes = NIL /* uninitialized */;
    public static Object loadFileRepSuffixes = new ELispCons(new ELispString(""));
    public static boolean loadInProgress = false;
    public static Object afterLoadAlist = NIL;
    public static Object loadHistory = NIL;
    public static Object loadFileName = NIL;
    public static Object loadTrueFileName = NIL;
    public static Object userInitFile = NIL;
    public static Object currentLoadList = NIL;
    public static Object loadReadFunction = READ;
    public static Object loadSourceFileFunction = NIL;
    public static boolean loadForceDocStrings = false;
    public static boolean loadConvertToUnibyte = false;
    public static Object sourceDirectory = FExpandFileName.expandFileName(new ELispString("../"), new ELispString(""));
    public static Object preloadedFileList = NIL;
    public static Object byteBooleanVars = NIL;
    public static boolean loadDangerousLibraries = false;
    public static boolean forceLoadMessages = false;
    public static Object bytecompVersionRegexp = new ELispString("^;;;.\\(?:in Emacs version\\|bytecomp version FSF\\)");
    public static Object lexicalBinding = NIL;
    public static Object evalBufferList = NIL;
    public static Object lreadUnescapedCharacterLiterals = NIL;
    public static boolean loadPreferNewer = false;
    public static boolean loadNoNative = false;
    public static Object readSymbolShorthands = NIL;
    public static Object macroexpDynvars = NIL;

    private static void lreadVars() {
        OBARRAY.forwardTo(() -> obarray);
        VALUES.forwardTo(() -> values);
        STANDARD_INPUT.forwardTo(() -> standardInput);
        READ_CIRCLE.forwardTo(() -> readCircle);
        LOAD_PATH.forwardTo(() -> loadPath);
        LOAD_SUFFIXES.forwardTo(() -> loadSuffixes);
        MODULE_FILE_SUFFIX.forwardTo(() -> moduleFileSuffix);
        DYNAMIC_LIBRARY_SUFFIXES.forwardTo(() -> dynamicLibrarySuffixes);
        LOAD_FILE_REP_SUFFIXES.forwardTo(() -> loadFileRepSuffixes);
        LOAD_IN_PROGRESS.forwardTo(() -> loadInProgress);
        AFTER_LOAD_ALIST.forwardTo(() -> afterLoadAlist);
        LOAD_HISTORY.forwardTo(() -> loadHistory);
        LOAD_FILE_NAME.forwardTo(() -> loadFileName);
        LOAD_TRUE_FILE_NAME.forwardTo(() -> loadTrueFileName);
        USER_INIT_FILE.forwardTo(() -> userInitFile);
        CURRENT_LOAD_LIST.forwardTo(() -> currentLoadList);
        LOAD_READ_FUNCTION.forwardTo(() -> loadReadFunction);
        LOAD_SOURCE_FILE_FUNCTION.forwardTo(() -> loadSourceFileFunction);
        LOAD_FORCE_DOC_STRINGS.forwardTo(() -> loadForceDocStrings);
        LOAD_CONVERT_TO_UNIBYTE.forwardTo(() -> loadConvertToUnibyte);
        SOURCE_DIRECTORY.forwardTo(() -> sourceDirectory);
        PRELOADED_FILE_LIST.forwardTo(() -> preloadedFileList);
        BYTE_BOOLEAN_VARS.forwardTo(() -> byteBooleanVars);
        LOAD_DANGEROUS_LIBRARIES.forwardTo(() -> loadDangerousLibraries);
        FORCE_LOAD_MESSAGES.forwardTo(() -> forceLoadMessages);
        BYTECOMP_VERSION_REGEXP.forwardTo(() -> bytecompVersionRegexp);
        LEXICAL_BINDING.forwardTo(() -> lexicalBinding);
        EVAL_BUFFER_LIST.forwardTo(() -> evalBufferList);
        LREAD__UNESCAPED_CHARACTER_LITERALS.forwardTo(() -> lreadUnescapedCharacterLiterals);
        LOAD_PREFER_NEWER.forwardTo(() -> loadPreferNewer);
        LOAD_NO_NATIVE.forwardTo(() -> loadNoNative);
        READ_SYMBOL_SHORTHANDS.forwardTo(() -> readSymbolShorthands);
        MACROEXP__DYNVARS.forwardTo(() -> macroexpDynvars);
    }
    /* @end region="lread.c" */
    /* @generated region="process.c" by="extract-emacs-src.py" */
    public static boolean deleteExitedProcesses = true;
    public static Object processConnectionType = T;
    public static Object processAdaptiveReadBuffering = T;
    public static boolean processPrioritizeLowerFds = false;
    public static Object interruptProcessFunctions = new ELispCons(INTERNAL_DEFAULT_INTERRUPT_PROCESS);
    public static Object signalProcessFunctions = new ELispCons(INTERNAL_DEFAULT_SIGNAL_PROCESS);
    public static Object internalDaemonSockname = NIL;
    public static long readProcessOutputMax = 65536;
    public static boolean fastReadProcessOutput = true;
    public static long processErrorPauseTime = 1;

    private static void processVars() {
        DELETE_EXITED_PROCESSES.forwardTo(() -> deleteExitedProcesses);
        PROCESS_CONNECTION_TYPE.forwardTo(() -> processConnectionType);
        PROCESS_ADAPTIVE_READ_BUFFERING.forwardTo(() -> processAdaptiveReadBuffering);
        PROCESS_PRIORITIZE_LOWER_FDS.forwardTo(() -> processPrioritizeLowerFds);
        INTERRUPT_PROCESS_FUNCTIONS.forwardTo(() -> interruptProcessFunctions);
        SIGNAL_PROCESS_FUNCTIONS.forwardTo(() -> signalProcessFunctions);
        INTERNAL__DAEMON_SOCKNAME.forwardTo(() -> internalDaemonSockname);
        READ_PROCESS_OUTPUT_MAX.forwardTo(() -> readProcessOutputMax);
        FAST_READ_PROCESS_OUTPUT.forwardTo(() -> fastReadProcessOutput);
        PROCESS_ERROR_PAUSE_TIME.forwardTo(() -> processErrorPauseTime);
    }
    /* @end region="process.c" */
    /* @generated region="charset.c" by="extract-emacs-src.py" */
    public static Object charsetMapPath = NIL;
    public static boolean inhibitLoadCharsetMap = false;
    public static Object charsetList = NIL;
    public static Object currentIso639Language = NIL;

    private static void charsetVars() {
        CHARSET_MAP_PATH.forwardTo(() -> charsetMapPath);
        INHIBIT_LOAD_CHARSET_MAP.forwardTo(() -> inhibitLoadCharsetMap);
        CHARSET_LIST.forwardTo(() -> charsetList);
        CURRENT_ISO639_LANGUAGE.forwardTo(() -> currentIso639Language);
    }
    /* @end region="charset.c" */
    /* @generated region="fileio.c" by="extract-emacs-src.py" */
    public static Object fileNameCodingSystem = NIL;
    public static Object defaultFileNameCodingSystem = NIL;
    public static Object fileNameHandlerAlist = NIL;
    public static Object setAutoCodingFunction = NIL;
    public static Object afterInsertFileFunctions = NIL;
    public static Object writeRegionAnnotateFunctions = NIL;
    public static Object writeRegionPostAnnotationFunction = NIL;
    public static Object writeRegionAnnotationsSoFar = NIL;
    public static Object inhibitFileNameHandlers = NIL;
    public static Object inhibitFileNameOperation = NIL;
    public static Object autoSaveListFileName = NIL;
    public static Object autoSaveVisitedFileName = NIL;
    public static Object autoSaveIncludeBigDeletions = NIL;
    public static boolean writeRegionInhibitFsync = true;
    public static boolean deleteByMovingToTrash = false;

    private static void fileioVars() {
        FILE_NAME_CODING_SYSTEM.forwardTo(() -> fileNameCodingSystem);
        DEFAULT_FILE_NAME_CODING_SYSTEM.forwardTo(() -> defaultFileNameCodingSystem);
        FILE_NAME_HANDLER_ALIST.forwardTo(() -> fileNameHandlerAlist);
        SET_AUTO_CODING_FUNCTION.forwardTo(() -> setAutoCodingFunction);
        AFTER_INSERT_FILE_FUNCTIONS.forwardTo(() -> afterInsertFileFunctions);
        WRITE_REGION_ANNOTATE_FUNCTIONS.forwardTo(() -> writeRegionAnnotateFunctions);
        WRITE_REGION_POST_ANNOTATION_FUNCTION.forwardTo(() -> writeRegionPostAnnotationFunction);
        WRITE_REGION_ANNOTATIONS_SO_FAR.forwardTo(() -> writeRegionAnnotationsSoFar);
        INHIBIT_FILE_NAME_HANDLERS.forwardTo(() -> inhibitFileNameHandlers);
        INHIBIT_FILE_NAME_OPERATION.forwardTo(() -> inhibitFileNameOperation);
        AUTO_SAVE_LIST_FILE_NAME.forwardTo(() -> autoSaveListFileName);
        AUTO_SAVE_VISITED_FILE_NAME.forwardTo(() -> autoSaveVisitedFileName);
        AUTO_SAVE_INCLUDE_BIG_DELETIONS.forwardTo(() -> autoSaveIncludeBigDeletions);
        WRITE_REGION_INHIBIT_FSYNC.forwardTo(() -> writeRegionInhibitFsync);
        DELETE_BY_MOVING_TO_TRASH.forwardTo(() -> deleteByMovingToTrash);
    }
    /* @end region="fileio.c" */
    /* @generated region="editfns.c" by="extract-emacs-src.py" */
    public static Object inhibitFieldTextMotion = NIL;
    public static Object bufferAccessFontifyFunctions = NIL;
    public static Object bufferAccessFontifiedProperty = NIL;
    public static Object systemName = NIL;
    public static Object userFullName = NIL /* uninitialized */;
    public static Object userLoginName = NIL;
    public static Object userRealLoginName = NIL /* uninitialized */;
    public static Object operatingSystemRelease = NIL /* uninitialized */;
    public static boolean binaryAsUnsigned = false;

    private static void editfnsVars() {
        INHIBIT_FIELD_TEXT_MOTION.forwardTo(() -> inhibitFieldTextMotion);
        BUFFER_ACCESS_FONTIFY_FUNCTIONS.forwardTo(() -> bufferAccessFontifyFunctions);
        BUFFER_ACCESS_FONTIFIED_PROPERTY.forwardTo(() -> bufferAccessFontifiedProperty);
        SYSTEM_NAME.forwardTo(() -> systemName);
        USER_FULL_NAME.forwardTo(() -> userFullName);
        USER_LOGIN_NAME.forwardTo(() -> userLoginName);
        USER_REAL_LOGIN_NAME.forwardTo(() -> userRealLoginName);
        OPERATING_SYSTEM_RELEASE.forwardTo(() -> operatingSystemRelease);
        BINARY_AS_UNSIGNED.forwardTo(() -> binaryAsUnsigned);
    }
    /* @end region="editfns.c" */
}
