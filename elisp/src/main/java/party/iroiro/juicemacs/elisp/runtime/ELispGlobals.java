package party.iroiro.juicemacs.elisp.runtime;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInCharTab.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInData.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFileIO.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInKeymap.*;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispFrame;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispKboard;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.MuleString;

import java.io.File;
import java.nio.file.Paths;
import java.util.HashMap;

import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.getMiniBuffer;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCoding.FDefineCodingSystemInternal.defineCodingSystemInternal;
import static party.iroiro.juicemacs.elisp.forms.BuiltInEmacs.decodeEnvPath;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

@SuppressWarnings({
        "PMD.FieldNamingConventions",
        "PMD.UnnecessaryCast",
        "PMD.UseExplicitTypes",
        "PMD.UseUnderscoresInNumericLiterals",
        "RedundantCast",
        "UnnecessaryUnicodeEscape",
        "UnnecessaryLocalVariable"
})
public final class ELispGlobals extends ELispGlobalsBase {
    public ELispGlobals(ELispContext context) {
        super(context);
    }

    public void initSymbols() {
        internSymbols(allSymbols());
        internSymbols(variableSymbols());
        internSymbols(bufferLocalVarSymbols());
    }

    public void init(ELispLanguage language) {
        initSymbols();
        initSubroutines(language);
        initGlobalVariables();
        invocationDirectory.setValue(new ELispString(getContext().truffleEnv().getCurrentWorkingDirectory().toString()));
        postInitVariables();
    }

    private void internSymbols(ELispSymbol[] symbols) {
        HashMap<MuleString, ELispSymbol> map = globalObarray.symbols();
        for (ELispSymbol symbol : symbols) {
            map.put(symbol.name(), symbol);
        }
    }

    public ELispSymbol intern(String name) {
        return globalObarray.intern(name);
    }

    private void unintern(ELispSymbol symbol) {
        globalObarray.unintern(symbol.name());
    }

    private void initForwardTo(ELispSymbol symbol, ValueStorage.AbstractForwarded<?> value) {
        ctx.forwardTo(symbol, value);
    }

    @Override
    public void patchGlobals() {
        initBuffer();
    }

    //#region extra globals
    private static final ELispString emptyUnibyteString = new ELispString("");

    private void defineError(ELispSymbol errorType, String errorMessage, ELispSymbol parentType) {
        Object parentConditions = ctx.getStorage(parentType).getProperty(ERROR_CONDITIONS);
        ValueStorage storage = ctx.getStorage(errorType);
        storage.putProperty(ERROR_CONDITIONS, new ELispCons(errorType, parentConditions));
        storage.putProperty(ERROR_MESSAGE, new ELispString(errorMessage));
    }
    private static ELispCons loadPathDefault() {
        // TODO
        return new ELispCons(new ELispString(Paths.get("").toAbsolutePath().toString()));
    }
    private static void loadPathCheck(ELispCons cons) {
        for (Object dir : cons) {
            if (dir instanceof ELispString s) {
                if (!Paths.get(s.toString()).toFile().canRead()) {
                    // TODO: dir_warning
                    System.err.println("Warning: " + s + " is not readable");
                }
            }
        }
    }
    private Object checkLoadPath() {
        String path = ctx.getEnv("EMACSLOADPATH");
        ELispCons loadPaths;
        if (path != null) {
            loadPaths = decodeEnvPath(null, path, true);
            loadPathCheck(loadPaths);
            if (!isNil(FMemq.memq(false, loadPaths))) {
                @Nullable ELispCons defaultPaths = loadPathDefault();
                loadPathCheck(defaultPaths);
                ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
                for (Object o : loadPaths) {
                    if (isNil(o)) {
                        if (defaultPaths != null) {
                            for (Object defaultPath : defaultPaths) {
                                builder.add(defaultPath);
                            }
                            defaultPaths = null;
                        }
                    } else {
                        builder.add(o);
                    }
                }
                loadPaths = asCons(builder.build());
            }
        } else {
            loadPaths = loadPathDefault();
            loadPathCheck(loadPaths);
        }
        return loadPaths;
    }
    private static Object initDynlibSuffixes() {
        String libraryName = System.mapLibraryName("library_name");
        return ELispCons.listOf(
                new ELispString(libraryName.substring(libraryName.lastIndexOf('.'))),
                emptyUnibyteString
        );
    }

    private void initCharset() {
        ELispString path = FExpandFileName.expandFileName(new ELispString("charsets"), dataDirectory.getValue());
        File dir = Paths.get(path.toString()).toFile();
        if (!(dir.isDirectory() && dir.canRead())) {
            throw ELispSignals.fatal("cannot read charsets directory " + dir);
        }
        charsetMapPath.setValue(new ELispCons(path));
    }

    private void makeInitialWindowFrame() {
        ctx.language().currentFrame().setValue(new ELispFrame());
    }
    //#endregion extra globals

    //#region initGlobalVariables
    public void initGlobalVariables() {
        allocVars();
        bufferVars();
        bytecodeVars();
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
        compositeVars();
        dataVars();
        diredVars();
        dispnewVars();
        docVars();
        editfnsVars();
        emacsVars();
        evalVars();
        fileioVars();
        filelockVars();
        floatfnsVars();
        fnsVars();
        frameVars();
        indentVars();
        keyboardVars();
        keymapVars();
        lreadVars();
        macrosVars();
        markerVars();
        minibufVars();
        pdumperVars();
        printVars();
        processVars();
        searchVars();
        syntaxVars();
        terminalVars();
        textpropVars();
        timefnsVars();
        treesitVars();
        windowVars();
        xdispVars();
        xfacesVars();
    }
    private final ValueStorage.ForwardedLong gcConsThreshold = new ValueStorage.ForwardedLong(800000);
    private final ValueStorage.Forwarded gcConsPercentage = new ValueStorage.Forwarded(0.1);
    private final ValueStorage.ForwardedLong pureBytesUsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong consCellsConsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong floatsConsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong vectorCellsConsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong symbolsConsed = new ValueStorage.ForwardedLong(1568);
    private final ValueStorage.ForwardedLong stringCharsConsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong intervalsConsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong stringsConsed = new ValueStorage.ForwardedLong();
    private final ValueStorage.Forwarded purifyFlag = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool garbageCollectionMessages = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded postGcHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded memorySignalData = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded memoryFull = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded gcElapsed = new ValueStorage.Forwarded(0.0);
    private final ValueStorage.ForwardedLong gcsDone = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.ForwardedLong integerWidth = new ValueStorage.ForwardedLong();
    private void allocVars() {
        initForwardTo(GC_CONS_THRESHOLD, gcConsThreshold);
        initForwardTo(GC_CONS_PERCENTAGE, gcConsPercentage);
        initForwardTo(PURE_BYTES_USED, pureBytesUsed);
        initForwardTo(CONS_CELLS_CONSED, consCellsConsed);
        initForwardTo(FLOATS_CONSED, floatsConsed);
        initForwardTo(VECTOR_CELLS_CONSED, vectorCellsConsed);
        initForwardTo(SYMBOLS_CONSED, symbolsConsed);
        initForwardTo(STRING_CHARS_CONSED, stringCharsConsed);
        initForwardTo(INTERVALS_CONSED, intervalsConsed);
        initForwardTo(STRINGS_CONSED, stringsConsed);
        initForwardTo(PURIFY_FLAG, purifyFlag);
        initForwardTo(GARBAGE_COLLECTION_MESSAGES, garbageCollectionMessages);
        initForwardTo(POST_GC_HOOK, postGcHook);
        initForwardTo(MEMORY_SIGNAL_DATA, memorySignalData);
        initForwardTo(MEMORY_FULL, memoryFull);
        initForwardTo(GC_ELAPSED, gcElapsed);
        initForwardTo(GCS_DONE, gcsDone);
        initForwardTo(INTEGER_WIDTH, integerWidth);
    }
    private final ValueStorage.Forwarded beforeChangeFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded afterChangeFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded firstChangeHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded transientMarkMode = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitReadOnly = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded killBufferQueryFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded changeMajorModeHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded bufferListUpdateHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool killBufferDeleteAutoSaveFiles = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool deleteAutoSaveFiles = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded caseFoldSearch = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded cloneIndirectBufferHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded longLineThreshold = new ValueStorage.Forwarded(50000L);
    private final ValueStorage.ForwardedLong longLineOptimizationsRegionSize = new ValueStorage.ForwardedLong(500000);
    private final ValueStorage.ForwardedLong longLineOptimizationsBolSearchLimit = new ValueStorage.ForwardedLong(128);
    private final ValueStorage.ForwardedLong largeHscrollThreshold = new ValueStorage.ForwardedLong(10000);
    private void bufferVars() {
        initForwardTo(BEFORE_CHANGE_FUNCTIONS, beforeChangeFunctions);
        initForwardTo(AFTER_CHANGE_FUNCTIONS, afterChangeFunctions);
        initForwardTo(FIRST_CHANGE_HOOK, firstChangeHook);
        initForwardTo(TRANSIENT_MARK_MODE, transientMarkMode);
        initForwardTo(INHIBIT_READ_ONLY, inhibitReadOnly);
        initForwardTo(KILL_BUFFER_QUERY_FUNCTIONS, killBufferQueryFunctions);
        initForwardTo(CHANGE_MAJOR_MODE_HOOK, changeMajorModeHook);
        initForwardTo(BUFFER_LIST_UPDATE_HOOK, bufferListUpdateHook);
        initForwardTo(KILL_BUFFER_DELETE_AUTO_SAVE_FILES, killBufferDeleteAutoSaveFiles);
        initForwardTo(DELETE_AUTO_SAVE_FILES, deleteAutoSaveFiles);
        initForwardTo(CASE_FOLD_SEARCH, caseFoldSearch);
        initForwardTo(CLONE_INDIRECT_BUFFER_HOOK, cloneIndirectBufferHook);
        initForwardTo(LONG_LINE_THRESHOLD, longLineThreshold);
        initForwardTo(LONG_LINE_OPTIMIZATIONS_REGION_SIZE, longLineOptimizationsRegionSize);
        initForwardTo(LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT, longLineOptimizationsBolSearchLimit);
        initForwardTo(LARGE_HSCROLL_THRESHOLD, largeHscrollThreshold);
    }
    private final ValueStorage.Forwarded byteCodeMeter = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool byteMeteringOn = new ValueStorage.ForwardedBool();
    private void bytecodeVars() {
        initForwardTo(BYTE_CODE_METER, byteCodeMeter);
        initForwardTo(BYTE_METERING_ON, byteMeteringOn);
    }
    private final ValueStorage.Forwarded currentPrefixArg = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded commandHistory = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded commandDebugStatus = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded markEvenIfInactive = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded mouseLeaveBufferHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitMouseEventCheck = new ValueStorage.ForwardedBool(false);
    private void callintVars() {
        initForwardTo(CURRENT_PREFIX_ARG, currentPrefixArg);
        initForwardTo(COMMAND_HISTORY, commandHistory);
        initForwardTo(COMMAND_DEBUG_STATUS, commandDebugStatus);
        initForwardTo(MARK_EVEN_IF_INACTIVE, markEvenIfInactive);
        initForwardTo(MOUSE_LEAVE_BUFFER_HOOK, mouseLeaveBufferHook);
        initForwardTo(INHIBIT_MOUSE_EVENT_CHECK, inhibitMouseEventCheck);
    }
    private final ValueStorage.Forwarded shellFileName = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded execPath = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded execSuffixes = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded execDirectory = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded dataDirectory = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded docDirectory = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded configureInfoDirectory = new ValueStorage.Forwarded(new ELispString("/usr/share/info"));
    private final ValueStorage.Forwarded sharedGameScoreDirectory = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded initialEnvironment = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded processEnvironment = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded ctagsProgramName = new ValueStorage.Forwarded(new ELispString("ctags"));
    private final ValueStorage.Forwarded etagsProgramName = new ValueStorage.Forwarded(new ELispString("etags"));
    private final ValueStorage.Forwarded hexlProgramName = new ValueStorage.Forwarded(new ELispString("hexl"));
    private final ValueStorage.Forwarded emacsclientProgramName = new ValueStorage.Forwarded(new ELispString("emacsclient"));
    private final ValueStorage.Forwarded movemailProgramName = new ValueStorage.Forwarded(new ELispString("movemail"));
    private final ValueStorage.Forwarded ebrowseProgramName = new ValueStorage.Forwarded(new ELispString("ebrowse"));
    private final ValueStorage.Forwarded rcs2logProgramName = new ValueStorage.Forwarded(new ELispString("rcs2log"));
    private void callprocVars() {
        initForwardTo(SHELL_FILE_NAME, shellFileName);
        initForwardTo(EXEC_PATH, execPath);
        initForwardTo(EXEC_SUFFIXES, execSuffixes);
        initForwardTo(EXEC_DIRECTORY, execDirectory);
        initForwardTo(DATA_DIRECTORY, dataDirectory);
        initForwardTo(DOC_DIRECTORY, docDirectory);
        initForwardTo(CONFIGURE_INFO_DIRECTORY, configureInfoDirectory);
        initForwardTo(SHARED_GAME_SCORE_DIRECTORY, sharedGameScoreDirectory);
        initForwardTo(INITIAL_ENVIRONMENT, initialEnvironment);
        initForwardTo(PROCESS_ENVIRONMENT, processEnvironment);
        initForwardTo(CTAGS_PROGRAM_NAME, ctagsProgramName);
        initForwardTo(ETAGS_PROGRAM_NAME, etagsProgramName);
        initForwardTo(HEXL_PROGRAM_NAME, hexlProgramName);
        initForwardTo(EMACSCLIENT_PROGRAM_NAME, emacsclientProgramName);
        initForwardTo(MOVEMAIL_PROGRAM_NAME, movemailProgramName);
        initForwardTo(EBROWSE_PROGRAM_NAME, ebrowseProgramName);
        initForwardTo(RCS2LOG_PROGRAM_NAME, rcs2logProgramName);
    }
    private final ValueStorage.Forwarded regionExtractFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool caseSymbolsAsWords = new ValueStorage.ForwardedBool(false);
    private void casefiddleVars() {
        initForwardTo(REGION_EXTRACT_FUNCTION, regionExtractFunction);
        initForwardTo(CASE_SYMBOLS_AS_WORDS, caseSymbolsAsWords);
    }

    private void casetabVars() {

    }
    private final ValueStorage.Forwarded wordCombiningCategories = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded wordSeparatingCategories = new ValueStorage.Forwarded(false);
    private void categoryVars() {
        initForwardTo(WORD_COMBINING_CATEGORIES, wordCombiningCategories);
        initForwardTo(WORD_SEPARATING_CATEGORIES, wordSeparatingCategories);
    }
    private final ValueStorage.Forwarded codeConversionMapVector = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded fontCclEncoderAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded translationHashTableVector = new ValueStorage.Forwarded(false);
    private void cclVars() {
        initForwardTo(CODE_CONVERSION_MAP_VECTOR, codeConversionMapVector);
        initForwardTo(FONT_CCL_ENCODER_ALIST, fontCclEncoderAlist);
        initForwardTo(TRANSLATION_HASH_TABLE_VECTOR, translationHashTableVector);
    }
    private final ValueStorage.Forwarded translationTableVector = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded autoFillChars = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded charWidthTable = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded ambiguousWidthChars = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded printableChars = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded charScriptTable = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded scriptRepresentativeChars = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded unicodeCategoryTable = new ValueStorage.Forwarded(false);
    private void characterVars() {
        initForwardTo(TRANSLATION_TABLE_VECTOR, translationTableVector);
        initForwardTo(AUTO_FILL_CHARS, autoFillChars);
        initForwardTo(CHAR_WIDTH_TABLE, charWidthTable);
        initForwardTo(AMBIGUOUS_WIDTH_CHARS, ambiguousWidthChars);
        initForwardTo(PRINTABLE_CHARS, printableChars);
        initForwardTo(CHAR_SCRIPT_TABLE, charScriptTable);
        initForwardTo(SCRIPT_REPRESENTATIVE_CHARS, scriptRepresentativeChars);
        initForwardTo(UNICODE_CATEGORY_TABLE, unicodeCategoryTable);
    }
    private final ValueStorage.Forwarded charsetMapPath = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitLoadCharsetMap = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded charsetList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded currentIso639Language = new ValueStorage.Forwarded(false);
    private void charsetVars() {
        initForwardTo(CHARSET_MAP_PATH, charsetMapPath);
        initForwardTo(INHIBIT_LOAD_CHARSET_MAP, inhibitLoadCharsetMap);
        initForwardTo(CHARSET_LIST, charsetList);
        initForwardTo(CURRENT_ISO639_LANGUAGE, currentIso639Language);
    }
    private final ValueStorage.Forwarded charCodePropertyAlist = new ValueStorage.Forwarded(false);
    private void chartabVars() {
        initForwardTo(CHAR_CODE_PROPERTY_ALIST, charCodePropertyAlist);
    }
    private final ValueStorage.Forwarded postSelfInsertHook = new ValueStorage.Forwarded(false);
    private void cmdsVars() {
        initForwardTo(POST_SELF_INSERT_HOOK, postSelfInsertHook);
    }
    private final ValueStorage.Forwarded codingSystemList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded codingSystemAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded codingCategoryList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded codingSystemForRead = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded codingSystemForWrite = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded lastCodingSystemUsed = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded lastCodeConversionError = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitEolConversion = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool inheritProcessCodingSystem = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded fileCodingSystemAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded processCodingSystemAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded networkCodingSystemAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded localeCodingSystem = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded eolMnemonicUnix = new ValueStorage.Forwarded(new ELispString(":"));
    private final ValueStorage.Forwarded eolMnemonicDos = new ValueStorage.Forwarded(new ELispString("\\"));
    private final ValueStorage.Forwarded eolMnemonicMac = new ValueStorage.Forwarded(new ELispString("/"));
    private final ValueStorage.Forwarded eolMnemonicUndecided = new ValueStorage.Forwarded(new ELispString(":"));
    private final ValueStorage.Forwarded enableCharacterTranslation = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded standardTranslationTableForDecode = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded standardTranslationTableForEncode = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded charsetRevisionTable = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded defaultProcessCodingSystem = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded latinExtraCodeTable = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded selectSafeCodingSystemFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool codingSystemRequireWarning = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool inhibitIsoEscapeDetection = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool inhibitNullByteDetection = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool disableAsciiOptimization = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded translationTableForInput = new ValueStorage.Forwarded(false);
    private void codingVars() {
        initForwardTo(CODING_SYSTEM_LIST, codingSystemList);
        initForwardTo(CODING_SYSTEM_ALIST, codingSystemAlist);
        initForwardTo(CODING_CATEGORY_LIST, codingCategoryList);
        initForwardTo(CODING_SYSTEM_FOR_READ, codingSystemForRead);
        initForwardTo(CODING_SYSTEM_FOR_WRITE, codingSystemForWrite);
        initForwardTo(LAST_CODING_SYSTEM_USED, lastCodingSystemUsed);
        initForwardTo(LAST_CODE_CONVERSION_ERROR, lastCodeConversionError);
        initForwardTo(INHIBIT_EOL_CONVERSION, inhibitEolConversion);
        initForwardTo(INHERIT_PROCESS_CODING_SYSTEM, inheritProcessCodingSystem);
        initForwardTo(FILE_CODING_SYSTEM_ALIST, fileCodingSystemAlist);
        initForwardTo(PROCESS_CODING_SYSTEM_ALIST, processCodingSystemAlist);
        initForwardTo(NETWORK_CODING_SYSTEM_ALIST, networkCodingSystemAlist);
        initForwardTo(LOCALE_CODING_SYSTEM, localeCodingSystem);
        initForwardTo(EOL_MNEMONIC_UNIX, eolMnemonicUnix);
        initForwardTo(EOL_MNEMONIC_DOS, eolMnemonicDos);
        initForwardTo(EOL_MNEMONIC_MAC, eolMnemonicMac);
        initForwardTo(EOL_MNEMONIC_UNDECIDED, eolMnemonicUndecided);
        initForwardTo(ENABLE_CHARACTER_TRANSLATION, enableCharacterTranslation);
        initForwardTo(STANDARD_TRANSLATION_TABLE_FOR_DECODE, standardTranslationTableForDecode);
        initForwardTo(STANDARD_TRANSLATION_TABLE_FOR_ENCODE, standardTranslationTableForEncode);
        initForwardTo(CHARSET_REVISION_TABLE, charsetRevisionTable);
        initForwardTo(DEFAULT_PROCESS_CODING_SYSTEM, defaultProcessCodingSystem);
        initForwardTo(LATIN_EXTRA_CODE_TABLE, latinExtraCodeTable);
        initForwardTo(SELECT_SAFE_CODING_SYSTEM_FUNCTION, selectSafeCodingSystemFunction);
        initForwardTo(CODING_SYSTEM_REQUIRE_WARNING, codingSystemRequireWarning);
        initForwardTo(INHIBIT_ISO_ESCAPE_DETECTION, inhibitIsoEscapeDetection);
        initForwardTo(INHIBIT_NULL_BYTE_DETECTION, inhibitNullByteDetection);
        initForwardTo(DISABLE_ASCII_OPTIMIZATION, disableAsciiOptimization);
        initForwardTo(TRANSLATION_TABLE_FOR_INPUT, translationTableForInput);
    }
    private final ValueStorage.ForwardedBool nativeCompJitCompilation = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded compCtxt = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded compSubrList = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded compAbiHash = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded compNativeVersionDir = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded compDeferredPendingH = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded compElnToElH = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded nativeCompElnLoadPath = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded nativeCompEnableSubrTrampolines = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded compInstalledTrampolinesH = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded compNoNativeFileH = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool compFilePreloadedP = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded compLoadedCompUnitsH = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded compSubrAritiesH = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool compSanitizerActive = new ValueStorage.ForwardedBool(false);
    private void compVars() {
        initForwardTo(NATIVE_COMP_JIT_COMPILATION, nativeCompJitCompilation);
        initForwardTo(COMP_CTXT, compCtxt);
        initForwardTo(COMP_SUBR_LIST, compSubrList);
        initForwardTo(COMP_ABI_HASH, compAbiHash);
        initForwardTo(COMP_NATIVE_VERSION_DIR, compNativeVersionDir);
        initForwardTo(COMP_DEFERRED_PENDING_H, compDeferredPendingH);
        initForwardTo(COMP_ELN_TO_EL_H, compElnToElH);
        initForwardTo(NATIVE_COMP_ELN_LOAD_PATH, nativeCompElnLoadPath);
        initForwardTo(NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES, nativeCompEnableSubrTrampolines);
        initForwardTo(COMP_INSTALLED_TRAMPOLINES_H, compInstalledTrampolinesH);
        initForwardTo(COMP_NO_NATIVE_FILE_H, compNoNativeFileH);
        initForwardTo(COMP_FILE_PRELOADED_P, compFilePreloadedP);
        initForwardTo(COMP_LOADED_COMP_UNITS_H, compLoadedCompUnitsH);
        initForwardTo(COMP_SUBR_ARITIES_H, compSubrAritiesH);
        initForwardTo(COMP_SANITIZER_ACTIVE, compSanitizerActive);
    }
    private final ValueStorage.Forwarded composeCharsAfterFunction = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded autoCompositionMode = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded autoCompositionFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded compositionFunctionTable = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded autoCompositionEmojiEligibleCodepoints = new ValueStorage.Forwarded(false);
    private void compositeVars() {
        initForwardTo(COMPOSE_CHARS_AFTER_FUNCTION, composeCharsAfterFunction);
        initForwardTo(AUTO_COMPOSITION_MODE, autoCompositionMode);
        initForwardTo(AUTO_COMPOSITION_FUNCTION, autoCompositionFunction);
        initForwardTo(COMPOSITION_FUNCTION_TABLE, compositionFunctionTable);
        initForwardTo(AUTO_COMPOSITION_EMOJI_ELIGIBLE_CODEPOINTS, autoCompositionEmojiEligibleCodepoints);
    }
    private final ValueStorage.Forwarded mostPositiveFixnum = new ValueStorage.Forwarded(2147483647L);
    private final ValueStorage.Forwarded mostNegativeFixnum = new ValueStorage.Forwarded(-2147483648L);
    private final ValueStorage.ForwardedBool symbolsWithPosEnabled = new ValueStorage.ForwardedBool(false);
    private void dataVars() {
        initForwardTo(MOST_POSITIVE_FIXNUM, mostPositiveFixnum);
        initForwardTo(MOST_NEGATIVE_FIXNUM, mostNegativeFixnum);
        initForwardTo(SYMBOLS_WITH_POS_ENABLED, symbolsWithPosEnabled);
    }
    private final ValueStorage.Forwarded completionIgnoredExtensions = new ValueStorage.Forwarded(false);
    private void diredVars() {
        initForwardTo(COMPLETION_IGNORED_EXTENSIONS, completionIgnoredExtensions);
    }
    private final ValueStorage.ForwardedLong baudRate = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedBool inverseVideo = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool visibleBell = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool noRedrawOnReenter = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded initialWindowSystem = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool cursorInEchoArea = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool mousePreferClosestGlyph = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded glyphTable = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded standardDisplayTable = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool redisplayDontPause = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded xShowTooltipTimeout = new ValueStorage.Forwarded(5L);
    private final ValueStorage.Forwarded tabBarPosition = new ValueStorage.Forwarded();
    private void dispnewVars() {
        initForwardTo(BAUD_RATE, baudRate);
        initForwardTo(INVERSE_VIDEO, inverseVideo);
        initForwardTo(VISIBLE_BELL, visibleBell);
        initForwardTo(NO_REDRAW_ON_REENTER, noRedrawOnReenter);
        initForwardTo(INITIAL_WINDOW_SYSTEM, initialWindowSystem);
        initForwardTo(CURSOR_IN_ECHO_AREA, cursorInEchoArea);
        initForwardTo(MOUSE_PREFER_CLOSEST_GLYPH, mousePreferClosestGlyph);
        initForwardTo(GLYPH_TABLE, glyphTable);
        initForwardTo(STANDARD_DISPLAY_TABLE, standardDisplayTable);
        initForwardTo(REDISPLAY_DONT_PAUSE, redisplayDontPause);
        initForwardTo(X_SHOW_TOOLTIP_TIMEOUT, xShowTooltipTimeout);
        initForwardTo(TAB_BAR_POSITION, tabBarPosition);
    }
    private final ValueStorage.Forwarded internalDocFileName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded buildFiles = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded textQuotingStyle = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool internalTextQuotingFlag = new ValueStorage.ForwardedBool();
    private void docVars() {
        initForwardTo(INTERNAL_DOC_FILE_NAME, internalDocFileName);
        initForwardTo(BUILD_FILES, buildFiles);
        initForwardTo(TEXT_QUOTING_STYLE, textQuotingStyle);
        initForwardTo(INTERNAL__TEXT_QUOTING_FLAG, internalTextQuotingFlag);
    }
    private final ValueStorage.Forwarded inhibitFieldTextMotion = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded bufferAccessFontifyFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded bufferAccessFontifiedProperty = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded systemName = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded userFullName = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded userLoginName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded userRealLoginName = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded operatingSystemRelease = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool binaryAsUnsigned = new ValueStorage.ForwardedBool(false);
    private void editfnsVars() {
        initForwardTo(INHIBIT_FIELD_TEXT_MOTION, inhibitFieldTextMotion);
        initForwardTo(BUFFER_ACCESS_FONTIFY_FUNCTIONS, bufferAccessFontifyFunctions);
        initForwardTo(BUFFER_ACCESS_FONTIFIED_PROPERTY, bufferAccessFontifiedProperty);
        initForwardTo(SYSTEM_NAME, systemName);
        initForwardTo(USER_FULL_NAME, userFullName);
        initForwardTo(USER_LOGIN_NAME, userLoginName);
        initForwardTo(USER_REAL_LOGIN_NAME, userRealLoginName);
        initForwardTo(OPERATING_SYSTEM_RELEASE, operatingSystemRelease);
        initForwardTo(BINARY_AS_UNSIGNED, binaryAsUnsigned);
    }
    private final ValueStorage.Forwarded commandLineArgs = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded systemType = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded systemConfiguration = new ValueStorage.Forwarded(new ELispString("x86_64-pc-linux-gnu"));
    private final ValueStorage.Forwarded systemConfigurationOptions = new ValueStorage.Forwarded(new ELispString(""));
    private final ValueStorage.Forwarded systemConfigurationFeatures = new ValueStorage.Forwarded(new ELispString(""));
    private final ValueStorage.ForwardedBool noninteractive = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded killEmacsHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded pathSeparator = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded invocationName = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded invocationDirectory = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded installationDirectory = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded systemMessagesLocale = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded systemTimeLocale = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded beforeInitTime = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded afterInitTime = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitXResources = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded emacsCopyright = new ValueStorage.Forwarded(new ELispString("Copyright (C) 2024 Free Software Foundation, Inc."));
    private final ValueStorage.Forwarded emacsVersion = new ValueStorage.Forwarded(new ELispString("31.0.50"));
    private final ValueStorage.Forwarded reportEmacsBugAddress = new ValueStorage.Forwarded(new ELispString(""));
    private final ValueStorage.Forwarded dumpMode = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded dynamicLibraryAlist = new ValueStorage.Forwarded(false);
    private void emacsVars() {
        initForwardTo(COMMAND_LINE_ARGS, commandLineArgs);
        initForwardTo(SYSTEM_TYPE, systemType);
        initForwardTo(SYSTEM_CONFIGURATION, systemConfiguration);
        initForwardTo(SYSTEM_CONFIGURATION_OPTIONS, systemConfigurationOptions);
        initForwardTo(SYSTEM_CONFIGURATION_FEATURES, systemConfigurationFeatures);
        initForwardTo(NONINTERACTIVE, noninteractive);
        initForwardTo(KILL_EMACS_HOOK, killEmacsHook);
        initForwardTo(PATH_SEPARATOR, pathSeparator);
        initForwardTo(INVOCATION_NAME, invocationName);
        initForwardTo(INVOCATION_DIRECTORY, invocationDirectory);
        initForwardTo(INSTALLATION_DIRECTORY, installationDirectory);
        initForwardTo(SYSTEM_MESSAGES_LOCALE, systemMessagesLocale);
        initForwardTo(SYSTEM_TIME_LOCALE, systemTimeLocale);
        initForwardTo(BEFORE_INIT_TIME, beforeInitTime);
        initForwardTo(AFTER_INIT_TIME, afterInitTime);
        initForwardTo(INHIBIT_X_RESOURCES, inhibitXResources);
        initForwardTo(EMACS_COPYRIGHT, emacsCopyright);
        initForwardTo(EMACS_VERSION, emacsVersion);
        initForwardTo(REPORT_EMACS_BUG_ADDRESS, reportEmacsBugAddress);
        initForwardTo(DUMP_MODE, dumpMode);
        initForwardTo(DYNAMIC_LIBRARY_ALIST, dynamicLibraryAlist);
    }
    private final ValueStorage.ForwardedLong maxLispEvalDepth = new ValueStorage.ForwardedLong(1600);
    private final ValueStorage.ForwardedLong lispEvalDepthReserve = new ValueStorage.ForwardedLong(200);
    private final ValueStorage.Forwarded quitFlag = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitQuit = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitDebugger = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded debugOnError = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded debugIgnoredErrors = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool debugOnQuit = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool debugOnNextCall = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool backtraceOnRedisplayError = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool debuggerMayContinue = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool debuggerStackFrameAsList = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded debugger = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded signalHookFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded debugOnSignal = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool backtraceOnErrorNoninteractive = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedLong internalWhenEnteredDebugger = new ValueStorage.ForwardedLong();
    private final ValueStorage.Forwarded internalInterpreterEnvironment = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded internalMakeInterpretedClosureFunction = new ValueStorage.Forwarded(false);
    private void evalVars() {
        initForwardTo(MAX_LISP_EVAL_DEPTH, maxLispEvalDepth);
        initForwardTo(LISP_EVAL_DEPTH_RESERVE, lispEvalDepthReserve);
        initForwardTo(QUIT_FLAG, quitFlag);
        initForwardTo(INHIBIT_QUIT, inhibitQuit);
        initForwardTo(INHIBIT_DEBUGGER, inhibitDebugger);
        initForwardTo(DEBUG_ON_ERROR, debugOnError);
        initForwardTo(DEBUG_IGNORED_ERRORS, debugIgnoredErrors);
        initForwardTo(DEBUG_ON_QUIT, debugOnQuit);
        initForwardTo(DEBUG_ON_NEXT_CALL, debugOnNextCall);
        initForwardTo(BACKTRACE_ON_REDISPLAY_ERROR, backtraceOnRedisplayError);
        initForwardTo(DEBUGGER_MAY_CONTINUE, debuggerMayContinue);
        initForwardTo(DEBUGGER_STACK_FRAME_AS_LIST, debuggerStackFrameAsList);
        initForwardTo(DEBUGGER, debugger);
        initForwardTo(SIGNAL_HOOK_FUNCTION, signalHookFunction);
        initForwardTo(DEBUG_ON_SIGNAL, debugOnSignal);
        initForwardTo(BACKTRACE_ON_ERROR_NONINTERACTIVE, backtraceOnErrorNoninteractive);
        initForwardTo(INTERNAL_WHEN_ENTERED_DEBUGGER, internalWhenEnteredDebugger);
        initForwardTo(INTERNAL_INTERPRETER_ENVIRONMENT, internalInterpreterEnvironment);
        initForwardTo(INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION, internalMakeInterpretedClosureFunction);
    }
    private final ValueStorage.Forwarded fileNameCodingSystem = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded defaultFileNameCodingSystem = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded fileNameHandlerAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded setAutoCodingFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded afterInsertFileFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded writeRegionAnnotateFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded writeRegionPostAnnotationFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded writeRegionAnnotationsSoFar = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitFileNameHandlers = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitFileNameOperation = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded autoSaveListFileName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded autoSaveVisitedFileName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded autoSaveIncludeBigDeletions = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool writeRegionInhibitFsync = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool deleteByMovingToTrash = new ValueStorage.ForwardedBool(false);
    private void fileioVars() {
        initForwardTo(FILE_NAME_CODING_SYSTEM, fileNameCodingSystem);
        initForwardTo(DEFAULT_FILE_NAME_CODING_SYSTEM, defaultFileNameCodingSystem);
        initForwardTo(FILE_NAME_HANDLER_ALIST, fileNameHandlerAlist);
        initForwardTo(SET_AUTO_CODING_FUNCTION, setAutoCodingFunction);
        initForwardTo(AFTER_INSERT_FILE_FUNCTIONS, afterInsertFileFunctions);
        initForwardTo(WRITE_REGION_ANNOTATE_FUNCTIONS, writeRegionAnnotateFunctions);
        initForwardTo(WRITE_REGION_POST_ANNOTATION_FUNCTION, writeRegionPostAnnotationFunction);
        initForwardTo(WRITE_REGION_ANNOTATIONS_SO_FAR, writeRegionAnnotationsSoFar);
        initForwardTo(INHIBIT_FILE_NAME_HANDLERS, inhibitFileNameHandlers);
        initForwardTo(INHIBIT_FILE_NAME_OPERATION, inhibitFileNameOperation);
        initForwardTo(AUTO_SAVE_LIST_FILE_NAME, autoSaveListFileName);
        initForwardTo(AUTO_SAVE_VISITED_FILE_NAME, autoSaveVisitedFileName);
        initForwardTo(AUTO_SAVE_INCLUDE_BIG_DELETIONS, autoSaveIncludeBigDeletions);
        initForwardTo(WRITE_REGION_INHIBIT_FSYNC, writeRegionInhibitFsync);
        initForwardTo(DELETE_BY_MOVING_TO_TRASH, deleteByMovingToTrash);
    }
    private final ValueStorage.Forwarded temporaryFileDirectory = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool createLockfiles = new ValueStorage.ForwardedBool(true);
    private void filelockVars() {
        initForwardTo(TEMPORARY_FILE_DIRECTORY, temporaryFileDirectory);
        initForwardTo(CREATE_LOCKFILES, createLockfiles);
    }

    private void floatfnsVars() {

    }
    private final ValueStorage.Forwarded overridingPlistEnvironment = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded features = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool useDialogBox = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool useFileDialog = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool useShortAnswers = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded yesOrNoPrompt = new ValueStorage.Forwarded(new ELispString("(yes or no) "));
    private void fnsVars() {
        initForwardTo(OVERRIDING_PLIST_ENVIRONMENT, overridingPlistEnvironment);
        initForwardTo(FEATURES, features);
        initForwardTo(USE_DIALOG_BOX, useDialogBox);
        initForwardTo(USE_FILE_DIALOG, useFileDialog);
        initForwardTo(USE_SHORT_ANSWERS, useShortAnswers);
        initForwardTo(YES_OR_NO_PROMPT, yesOrNoPrompt);
    }
    private final ValueStorage.Forwarded xResourceName = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded xResourceClass = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded frameAlphaLowerLimit = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded defaultFrameAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded defaultFrameScrollBars = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool scrollBarAdjustThumbPortion = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded TerminalFrame = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded mousePositionFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded mouseHighlight = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded makePointerInvisible = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded moveFrameFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded deleteFrameFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded afterDeleteFrameFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded menuBarMode = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded tabBarMode = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded toolBarMode = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded resizeMiniFrames = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded focusFollowsMouse = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool frameResizePixelwise = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded frameInhibitImpliedResize = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded frameSizeHistory = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool tooltipReuseHiddenFrame = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool useSystemTooltips = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded iconifyChildFrame = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded frameInternalParameters = new ValueStorage.Forwarded();
    private void frameVars() {
        initForwardTo(X_RESOURCE_NAME, xResourceName);
        initForwardTo(X_RESOURCE_CLASS, xResourceClass);
        initForwardTo(FRAME_ALPHA_LOWER_LIMIT, frameAlphaLowerLimit);
        initForwardTo(DEFAULT_FRAME_ALIST, defaultFrameAlist);
        initForwardTo(DEFAULT_FRAME_SCROLL_BARS, defaultFrameScrollBars);
        initForwardTo(SCROLL_BAR_ADJUST_THUMB_PORTION, scrollBarAdjustThumbPortion);
        initForwardTo(_TERMINAL_FRAME, TerminalFrame);
        initForwardTo(MOUSE_POSITION_FUNCTION, mousePositionFunction);
        initForwardTo(MOUSE_HIGHLIGHT, mouseHighlight);
        initForwardTo(MAKE_POINTER_INVISIBLE, makePointerInvisible);
        initForwardTo(MOVE_FRAME_FUNCTIONS, moveFrameFunctions);
        initForwardTo(DELETE_FRAME_FUNCTIONS, deleteFrameFunctions);
        initForwardTo(AFTER_DELETE_FRAME_FUNCTIONS, afterDeleteFrameFunctions);
        initForwardTo(MENU_BAR_MODE, menuBarMode);
        initForwardTo(TAB_BAR_MODE, tabBarMode);
        initForwardTo(TOOL_BAR_MODE, toolBarMode);
        initForwardTo(RESIZE_MINI_FRAMES, resizeMiniFrames);
        initForwardTo(FOCUS_FOLLOWS_MOUSE, focusFollowsMouse);
        initForwardTo(FRAME_RESIZE_PIXELWISE, frameResizePixelwise);
        initForwardTo(FRAME_INHIBIT_IMPLIED_RESIZE, frameInhibitImpliedResize);
        initForwardTo(FRAME_SIZE_HISTORY, frameSizeHistory);
        initForwardTo(TOOLTIP_REUSE_HIDDEN_FRAME, tooltipReuseHiddenFrame);
        initForwardTo(USE_SYSTEM_TOOLTIPS, useSystemTooltips);
        initForwardTo(ICONIFY_CHILD_FRAME, iconifyChildFrame);
        initForwardTo(FRAME_INTERNAL_PARAMETERS, frameInternalParameters);
    }
    private final ValueStorage.ForwardedBool indentTabsMode = new ValueStorage.ForwardedBool(true);
    private void indentVars() {
        initForwardTo(INDENT_TABS_MODE, indentTabsMode);
    }
    private final ValueStorage.Forwarded internalTopLevelMessage = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded lastCommandEvent = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded lastNonmenuEvent = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded lastInputEvent = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded unreadCommandEvents = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded unreadPostInputMethodEvents = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded unreadInputMethodEvents = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded metaPrefixChar = new ValueStorage.Forwarded(27L);
    private final ValueStorage.Forwarded thisCommand = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded realThisCommand = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded currentMinibufferCommand = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded thisCommandKeysShiftTranslated = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded thisOriginalCommand = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong autoSaveInterval = new ValueStorage.ForwardedLong(300);
    private final ValueStorage.ForwardedBool autoSaveNoMessage = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded autoSaveTimeout = new ValueStorage.Forwarded(30L);
    private final ValueStorage.Forwarded echoKeystrokes = new ValueStorage.Forwarded(1L);
    private final ValueStorage.ForwardedBool echoKeystrokesHelp = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded pollingPeriod = new ValueStorage.Forwarded(2.0);
    private final ValueStorage.Forwarded doubleClickTime = new ValueStorage.Forwarded(500L);
    private final ValueStorage.ForwardedLong doubleClickFuzz = new ValueStorage.ForwardedLong(3);
    private final ValueStorage.ForwardedLong numInputKeys = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.ForwardedLong numNonmacroInputEvents = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.Forwarded lastEventFrame = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded lastEventDevice = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded ttyEraseChar = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded helpChar = new ValueStorage.Forwarded(8L);
    private final ValueStorage.Forwarded helpEventList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded helpForm = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded prefixHelpCommand = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded topLevel = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool cannotSuspend = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool menuPrompting = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded menuPromptMoreChar = new ValueStorage.Forwarded(32L);
    private final ValueStorage.ForwardedLong extraKeyboardModifiers = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.Forwarded deactivateMark = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded preCommandHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded postCommandHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool lucidMenuGrabKeyboard = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded menuBarFinalItems = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded tabBarSeparatorImageExpression = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded toolBarSeparatorImageExpression = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded overridingLocalMap = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded overridingLocalMapMenuFlag = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded specialEventMap = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded trackMouse = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded functionKeyMap = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded keyTranslationMap = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded delayedWarningsList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded timerList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded timerIdleList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inputMethodFunction = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded inputMethodPreviousMessage = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded showHelpFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded disablePointAdjustment = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded globalDisablePointAdjustment = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferMessageTimeout = new ValueStorage.Forwarded(2L);
    private final ValueStorage.Forwarded throwOnInput = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded commandErrorFunction = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded enableDisabledMenusAndButtons = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded selectActiveRegions = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded savedRegionSelection = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded selectionInhibitUpdateCommands = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded debugOnEvent = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool attemptStackOverflowRecovery = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool attemptOrderlyShutdownOnFatalSignal = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded whileNoInputIgnoreEvents = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool translateUpperCaseKeyBindings = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool inputPendingPFilterEvents = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool mwheelCoalesceScrollEvents = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded displayMonitorsChangedFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitRecordChar = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool recordAllKeys = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded postSelectRegionHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool disableInhibitTextConversion = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded currentKeyRemapSequence = new ValueStorage.Forwarded(false);
    private void keyboardVars() {
        initForwardTo(INTERNAL__TOP_LEVEL_MESSAGE, internalTopLevelMessage);
        initForwardTo(LAST_COMMAND_EVENT, lastCommandEvent);
        initForwardTo(LAST_NONMENU_EVENT, lastNonmenuEvent);
        initForwardTo(LAST_INPUT_EVENT, lastInputEvent);
        initForwardTo(UNREAD_COMMAND_EVENTS, unreadCommandEvents);
        initForwardTo(UNREAD_POST_INPUT_METHOD_EVENTS, unreadPostInputMethodEvents);
        initForwardTo(UNREAD_INPUT_METHOD_EVENTS, unreadInputMethodEvents);
        initForwardTo(META_PREFIX_CHAR, metaPrefixChar);
        initForwardTo(THIS_COMMAND, thisCommand);
        initForwardTo(REAL_THIS_COMMAND, realThisCommand);
        initForwardTo(CURRENT_MINIBUFFER_COMMAND, currentMinibufferCommand);
        initForwardTo(THIS_COMMAND_KEYS_SHIFT_TRANSLATED, thisCommandKeysShiftTranslated);
        initForwardTo(THIS_ORIGINAL_COMMAND, thisOriginalCommand);
        initForwardTo(AUTO_SAVE_INTERVAL, autoSaveInterval);
        initForwardTo(AUTO_SAVE_NO_MESSAGE, autoSaveNoMessage);
        initForwardTo(AUTO_SAVE_TIMEOUT, autoSaveTimeout);
        initForwardTo(ECHO_KEYSTROKES, echoKeystrokes);
        initForwardTo(ECHO_KEYSTROKES_HELP, echoKeystrokesHelp);
        initForwardTo(POLLING_PERIOD, pollingPeriod);
        initForwardTo(DOUBLE_CLICK_TIME, doubleClickTime);
        initForwardTo(DOUBLE_CLICK_FUZZ, doubleClickFuzz);
        initForwardTo(NUM_INPUT_KEYS, numInputKeys);
        initForwardTo(NUM_NONMACRO_INPUT_EVENTS, numNonmacroInputEvents);
        initForwardTo(LAST_EVENT_FRAME, lastEventFrame);
        initForwardTo(LAST_EVENT_DEVICE, lastEventDevice);
        initForwardTo(TTY_ERASE_CHAR, ttyEraseChar);
        initForwardTo(HELP_CHAR, helpChar);
        initForwardTo(HELP_EVENT_LIST, helpEventList);
        initForwardTo(HELP_FORM, helpForm);
        initForwardTo(PREFIX_HELP_COMMAND, prefixHelpCommand);
        initForwardTo(TOP_LEVEL, topLevel);
        initForwardTo(CANNOT_SUSPEND, cannotSuspend);
        initForwardTo(MENU_PROMPTING, menuPrompting);
        initForwardTo(MENU_PROMPT_MORE_CHAR, menuPromptMoreChar);
        initForwardTo(EXTRA_KEYBOARD_MODIFIERS, extraKeyboardModifiers);
        initForwardTo(DEACTIVATE_MARK, deactivateMark);
        initForwardTo(PRE_COMMAND_HOOK, preCommandHook);
        initForwardTo(POST_COMMAND_HOOK, postCommandHook);
        initForwardTo(LUCID__MENU_GRAB_KEYBOARD, lucidMenuGrabKeyboard);
        initForwardTo(MENU_BAR_FINAL_ITEMS, menuBarFinalItems);
        initForwardTo(TAB_BAR_SEPARATOR_IMAGE_EXPRESSION, tabBarSeparatorImageExpression);
        initForwardTo(TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION, toolBarSeparatorImageExpression);
        initForwardTo(OVERRIDING_LOCAL_MAP, overridingLocalMap);
        initForwardTo(OVERRIDING_LOCAL_MAP_MENU_FLAG, overridingLocalMapMenuFlag);
        initForwardTo(SPECIAL_EVENT_MAP, specialEventMap);
        initForwardTo(TRACK_MOUSE, trackMouse);
        initForwardTo(FUNCTION_KEY_MAP, functionKeyMap);
        initForwardTo(KEY_TRANSLATION_MAP, keyTranslationMap);
        initForwardTo(DELAYED_WARNINGS_LIST, delayedWarningsList);
        initForwardTo(TIMER_LIST, timerList);
        initForwardTo(TIMER_IDLE_LIST, timerIdleList);
        initForwardTo(INPUT_METHOD_FUNCTION, inputMethodFunction);
        initForwardTo(INPUT_METHOD_PREVIOUS_MESSAGE, inputMethodPreviousMessage);
        initForwardTo(SHOW_HELP_FUNCTION, showHelpFunction);
        initForwardTo(DISABLE_POINT_ADJUSTMENT, disablePointAdjustment);
        initForwardTo(GLOBAL_DISABLE_POINT_ADJUSTMENT, globalDisablePointAdjustment);
        initForwardTo(MINIBUFFER_MESSAGE_TIMEOUT, minibufferMessageTimeout);
        initForwardTo(THROW_ON_INPUT, throwOnInput);
        initForwardTo(COMMAND_ERROR_FUNCTION, commandErrorFunction);
        initForwardTo(ENABLE_DISABLED_MENUS_AND_BUTTONS, enableDisabledMenusAndButtons);
        initForwardTo(SELECT_ACTIVE_REGIONS, selectActiveRegions);
        initForwardTo(SAVED_REGION_SELECTION, savedRegionSelection);
        initForwardTo(SELECTION_INHIBIT_UPDATE_COMMANDS, selectionInhibitUpdateCommands);
        initForwardTo(DEBUG_ON_EVENT, debugOnEvent);
        initForwardTo(ATTEMPT_STACK_OVERFLOW_RECOVERY, attemptStackOverflowRecovery);
        initForwardTo(ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL, attemptOrderlyShutdownOnFatalSignal);
        initForwardTo(WHILE_NO_INPUT_IGNORE_EVENTS, whileNoInputIgnoreEvents);
        initForwardTo(TRANSLATE_UPPER_CASE_KEY_BINDINGS, translateUpperCaseKeyBindings);
        initForwardTo(INPUT_PENDING_P_FILTER_EVENTS, inputPendingPFilterEvents);
        initForwardTo(MWHEEL_COALESCE_SCROLL_EVENTS, mwheelCoalesceScrollEvents);
        initForwardTo(DISPLAY_MONITORS_CHANGED_FUNCTIONS, displayMonitorsChangedFunctions);
        initForwardTo(INHIBIT__RECORD_CHAR, inhibitRecordChar);
        initForwardTo(RECORD_ALL_KEYS, recordAllKeys);
        initForwardTo(POST_SELECT_REGION_HOOK, postSelectRegionHook);
        initForwardTo(DISABLE_INHIBIT_TEXT_CONVERSION, disableInhibitTextConversion);
        initForwardTo(CURRENT_KEY_REMAP_SEQUENCE, currentKeyRemapSequence);
    }
    private final ValueStorage.Forwarded minibufferLocalMap = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded minorModeMapAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minorModeOverridingMapAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded emulationModeMapAlists = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded whereIsPreferredModifier = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded describeBindingsCheckShadowingInRanges = new ValueStorage.Forwarded(false);
    private void keymapVars() {
        initForwardTo(MINIBUFFER_LOCAL_MAP, minibufferLocalMap);
        initForwardTo(MINOR_MODE_MAP_ALIST, minorModeMapAlist);
        initForwardTo(MINOR_MODE_OVERRIDING_MAP_ALIST, minorModeOverridingMapAlist);
        initForwardTo(EMULATION_MODE_MAP_ALISTS, emulationModeMapAlists);
        initForwardTo(WHERE_IS_PREFERRED_MODIFIER, whereIsPreferredModifier);
        initForwardTo(DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES, describeBindingsCheckShadowingInRanges);
    }
    private final ValueStorage.Forwarded obarray = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded values = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded standardInput = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded readCircle = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded loadPath = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded loadSuffixes = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded moduleFileSuffix = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded dynamicLibrarySuffixes = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded loadFileRepSuffixes = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool loadInProgress = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded afterLoadAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded loadHistory = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded loadFileName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded loadTrueFileName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded userInitFile = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded currentLoadList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded loadReadFunction = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded loadSourceFileFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool loadForceDocStrings = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool loadConvertToUnibyte = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded sourceDirectory = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded preloadedFileList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded byteBooleanVars = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool loadDangerousLibraries = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool forceLoadMessages = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded bytecompVersionRegexp = new ValueStorage.Forwarded(new ELispString("^;;;.\\(?:in Emacs version\\|bytecomp version FSF\\)"));
    private final ValueStorage.Forwarded lexicalBinding = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded evalBufferList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded lreadUnescapedCharacterLiterals = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool loadPreferNewer = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool loadNoNative = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded readSymbolShorthands = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded macroexpDynvars = new ValueStorage.Forwarded(false);
    private void lreadVars() {
        initForwardTo(OBARRAY, obarray);
        initForwardTo(VALUES, values);
        initForwardTo(STANDARD_INPUT, standardInput);
        initForwardTo(READ_CIRCLE, readCircle);
        initForwardTo(LOAD_PATH, loadPath);
        initForwardTo(LOAD_SUFFIXES, loadSuffixes);
        initForwardTo(MODULE_FILE_SUFFIX, moduleFileSuffix);
        initForwardTo(DYNAMIC_LIBRARY_SUFFIXES, dynamicLibrarySuffixes);
        initForwardTo(LOAD_FILE_REP_SUFFIXES, loadFileRepSuffixes);
        initForwardTo(LOAD_IN_PROGRESS, loadInProgress);
        initForwardTo(AFTER_LOAD_ALIST, afterLoadAlist);
        initForwardTo(LOAD_HISTORY, loadHistory);
        initForwardTo(LOAD_FILE_NAME, loadFileName);
        initForwardTo(LOAD_TRUE_FILE_NAME, loadTrueFileName);
        initForwardTo(USER_INIT_FILE, userInitFile);
        initForwardTo(CURRENT_LOAD_LIST, currentLoadList);
        initForwardTo(LOAD_READ_FUNCTION, loadReadFunction);
        initForwardTo(LOAD_SOURCE_FILE_FUNCTION, loadSourceFileFunction);
        initForwardTo(LOAD_FORCE_DOC_STRINGS, loadForceDocStrings);
        initForwardTo(LOAD_CONVERT_TO_UNIBYTE, loadConvertToUnibyte);
        initForwardTo(SOURCE_DIRECTORY, sourceDirectory);
        initForwardTo(PRELOADED_FILE_LIST, preloadedFileList);
        initForwardTo(BYTE_BOOLEAN_VARS, byteBooleanVars);
        initForwardTo(LOAD_DANGEROUS_LIBRARIES, loadDangerousLibraries);
        initForwardTo(FORCE_LOAD_MESSAGES, forceLoadMessages);
        initForwardTo(BYTECOMP_VERSION_REGEXP, bytecompVersionRegexp);
        initForwardTo(LEXICAL_BINDING, lexicalBinding);
        initForwardTo(EVAL_BUFFER_LIST, evalBufferList);
        initForwardTo(LREAD_UNESCAPED_CHARACTER_LITERALS, lreadUnescapedCharacterLiterals);
        initForwardTo(LOAD_PREFER_NEWER, loadPreferNewer);
        initForwardTo(LOAD_NO_NATIVE, loadNoNative);
        initForwardTo(READ_SYMBOL_SHORTHANDS, readSymbolShorthands);
        initForwardTo(MACROEXP__DYNVARS, macroexpDynvars);
    }
    private final ValueStorage.Forwarded kbdMacroTerminationHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded executingKbdMacro = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong executingKbdMacroIndex = new ValueStorage.ForwardedLong();
    private void macrosVars() {
        initForwardTo(KBD_MACRO_TERMINATION_HOOK, kbdMacroTerminationHook);
        initForwardTo(EXECUTING_KBD_MACRO, executingKbdMacro);
        initForwardTo(EXECUTING_KBD_MACRO_INDEX, executingKbdMacroIndex);
    }

    private void markerVars() {

    }
    private final ValueStorage.Forwarded readExpressionHistory = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded readBufferFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferFollowsSelectedFrame = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool readBufferCompletionIgnoreCase = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded minibufferSetupHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferExitHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded historyLength = new ValueStorage.Forwarded(100L);
    private final ValueStorage.ForwardedBool historyDeleteDuplicates = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded historyAddNewInput = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool completionIgnoreCase = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool enableRecursiveMinibuffers = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded minibufferCompletionTable = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferCompletionPredicate = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferCompletionConfirm = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferCompletingFileName = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferHelpForm = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferHistoryVariable = new ValueStorage.Forwarded(0L);
    private final ValueStorage.Forwarded minibufferHistoryPosition = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool minibufferAutoRaise = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded completionRegexpList = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool minibufferAllowTextProperties = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded minibufferPromptProperties = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded readHideChar = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitInteraction = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool readMinibufferRestoreWindows = new ValueStorage.ForwardedBool(true);
    private void minibufVars() {
        initForwardTo(READ_EXPRESSION_HISTORY, readExpressionHistory);
        initForwardTo(READ_BUFFER_FUNCTION, readBufferFunction);
        initForwardTo(MINIBUFFER_FOLLOWS_SELECTED_FRAME, minibufferFollowsSelectedFrame);
        initForwardTo(READ_BUFFER_COMPLETION_IGNORE_CASE, readBufferCompletionIgnoreCase);
        initForwardTo(MINIBUFFER_SETUP_HOOK, minibufferSetupHook);
        initForwardTo(MINIBUFFER_EXIT_HOOK, minibufferExitHook);
        initForwardTo(HISTORY_LENGTH, historyLength);
        initForwardTo(HISTORY_DELETE_DUPLICATES, historyDeleteDuplicates);
        initForwardTo(HISTORY_ADD_NEW_INPUT, historyAddNewInput);
        initForwardTo(COMPLETION_IGNORE_CASE, completionIgnoreCase);
        initForwardTo(ENABLE_RECURSIVE_MINIBUFFERS, enableRecursiveMinibuffers);
        initForwardTo(MINIBUFFER_COMPLETION_TABLE, minibufferCompletionTable);
        initForwardTo(MINIBUFFER_COMPLETION_PREDICATE, minibufferCompletionPredicate);
        initForwardTo(MINIBUFFER_COMPLETION_CONFIRM, minibufferCompletionConfirm);
        initForwardTo(MINIBUFFER_COMPLETING_FILE_NAME, minibufferCompletingFileName);
        initForwardTo(MINIBUFFER_HELP_FORM, minibufferHelpForm);
        initForwardTo(MINIBUFFER_HISTORY_VARIABLE, minibufferHistoryVariable);
        initForwardTo(MINIBUFFER_HISTORY_POSITION, minibufferHistoryPosition);
        initForwardTo(MINIBUFFER_AUTO_RAISE, minibufferAutoRaise);
        initForwardTo(COMPLETION_REGEXP_LIST, completionRegexpList);
        initForwardTo(MINIBUFFER_ALLOW_TEXT_PROPERTIES, minibufferAllowTextProperties);
        initForwardTo(MINIBUFFER_PROMPT_PROPERTIES, minibufferPromptProperties);
        initForwardTo(READ_HIDE_CHAR, readHideChar);
        initForwardTo(INHIBIT_INTERACTION, inhibitInteraction);
        initForwardTo(READ_MINIBUFFER_RESTORE_WINDOWS, readMinibufferRestoreWindows);
    }
    private final ValueStorage.Forwarded pdumperFingerprint = new ValueStorage.Forwarded();
    private void pdumperVars() {
        initForwardTo(PDUMPER_FINGERPRINT, pdumperFingerprint);
    }
    private final ValueStorage.Forwarded standardOutput = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded floatOutputFormat = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool printIntegersAsCharacters = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded printLength = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded printLevel = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool printEscapeNewlines = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool printEscapeControlCharacters = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool printEscapeNonascii = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool printEscapeMultibyte = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool printQuoted = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded printGensym = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded printCircle = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded printContinuousNumbering = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded printNumberTable = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded printCharsetTextProperty = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool printSymbolsBare = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded printUnreadableFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded printUnreadableCallbackBuffer = new ValueStorage.Forwarded(false);
    private void printVars() {
        initForwardTo(STANDARD_OUTPUT, standardOutput);
        initForwardTo(FLOAT_OUTPUT_FORMAT, floatOutputFormat);
        initForwardTo(PRINT_INTEGERS_AS_CHARACTERS, printIntegersAsCharacters);
        initForwardTo(PRINT_LENGTH, printLength);
        initForwardTo(PRINT_LEVEL, printLevel);
        initForwardTo(PRINT_ESCAPE_NEWLINES, printEscapeNewlines);
        initForwardTo(PRINT_ESCAPE_CONTROL_CHARACTERS, printEscapeControlCharacters);
        initForwardTo(PRINT_ESCAPE_NONASCII, printEscapeNonascii);
        initForwardTo(PRINT_ESCAPE_MULTIBYTE, printEscapeMultibyte);
        initForwardTo(PRINT_QUOTED, printQuoted);
        initForwardTo(PRINT_GENSYM, printGensym);
        initForwardTo(PRINT_CIRCLE, printCircle);
        initForwardTo(PRINT_CONTINUOUS_NUMBERING, printContinuousNumbering);
        initForwardTo(PRINT_NUMBER_TABLE, printNumberTable);
        initForwardTo(PRINT_CHARSET_TEXT_PROPERTY, printCharsetTextProperty);
        initForwardTo(PRINT_SYMBOLS_BARE, printSymbolsBare);
        initForwardTo(PRINT_UNREADABLE_FUNCTION, printUnreadableFunction);
        initForwardTo(PRINT__UNREADABLE_CALLBACK_BUFFER, printUnreadableCallbackBuffer);
    }
    private final ValueStorage.ForwardedBool deleteExitedProcesses = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded processConnectionType = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded processAdaptiveReadBuffering = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool processPrioritizeLowerFds = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded interruptProcessFunctions = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded signalProcessFunctions = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded internalDaemonSockname = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong readProcessOutputMax = new ValueStorage.ForwardedLong(65536);
    private final ValueStorage.ForwardedBool fastReadProcessOutput = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedLong processErrorPauseTime = new ValueStorage.ForwardedLong(1);
    private void processVars() {
        initForwardTo(DELETE_EXITED_PROCESSES, deleteExitedProcesses);
        initForwardTo(PROCESS_CONNECTION_TYPE, processConnectionType);
        initForwardTo(PROCESS_ADAPTIVE_READ_BUFFERING, processAdaptiveReadBuffering);
        initForwardTo(PROCESS_PRIORITIZE_LOWER_FDS, processPrioritizeLowerFds);
        initForwardTo(INTERRUPT_PROCESS_FUNCTIONS, interruptProcessFunctions);
        initForwardTo(SIGNAL_PROCESS_FUNCTIONS, signalProcessFunctions);
        initForwardTo(INTERNAL__DAEMON_SOCKNAME, internalDaemonSockname);
        initForwardTo(READ_PROCESS_OUTPUT_MAX, readProcessOutputMax);
        initForwardTo(FAST_READ_PROCESS_OUTPUT, fastReadProcessOutput);
        initForwardTo(PROCESS_ERROR_PAUSE_TIME, processErrorPauseTime);
    }
    private final ValueStorage.Forwarded searchSpacesRegexp = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitChangingMatchData = new ValueStorage.Forwarded(false);
    private void searchVars() {
        initForwardTo(SEARCH_SPACES_REGEXP, searchSpacesRegexp);
        initForwardTo(INHIBIT_CHANGING_MATCH_DATA, inhibitChangingMatchData);
    }
    private final ValueStorage.Forwarded commentUseSyntaxPpss = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool parseSexpIgnoreComments = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool parseSexpLookupProperties = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedLong syntaxPropertizeDone = new ValueStorage.ForwardedLong(-1);
    private final ValueStorage.ForwardedBool wordsIncludeEscapes = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool multibyteSyntaxAsSymbol = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool openParenInColumn0IsDefunStart = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded findWordBoundaryFunctionTable = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool commentEndCanBeEscaped = new ValueStorage.ForwardedBool(false);
    private void syntaxVars() {
        initForwardTo(COMMENT_USE_SYNTAX_PPSS, commentUseSyntaxPpss);
        initForwardTo(PARSE_SEXP_IGNORE_COMMENTS, parseSexpIgnoreComments);
        initForwardTo(PARSE_SEXP_LOOKUP_PROPERTIES, parseSexpLookupProperties);
        initForwardTo(SYNTAX_PROPERTIZE__DONE, syntaxPropertizeDone);
        initForwardTo(WORDS_INCLUDE_ESCAPES, wordsIncludeEscapes);
        initForwardTo(MULTIBYTE_SYNTAX_AS_SYMBOL, multibyteSyntaxAsSymbol);
        initForwardTo(OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START, openParenInColumn0IsDefunStart);
        initForwardTo(FIND_WORD_BOUNDARY_FUNCTION_TABLE, findWordBoundaryFunctionTable);
        initForwardTo(COMMENT_END_CAN_BE_ESCAPED, commentEndCanBeEscaped);
    }
    private final ValueStorage.Forwarded ringBellFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded deleteTerminalFunctions = new ValueStorage.Forwarded(false);
    private void terminalVars() {
        initForwardTo(RING_BELL_FUNCTION, ringBellFunction);
        initForwardTo(DELETE_TERMINAL_FUNCTIONS, deleteTerminalFunctions);
    }
    private final ValueStorage.Forwarded defaultTextProperties = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded charPropertyAliasAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded inhibitPointMotionHooks = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded textPropertyDefaultNonsticky = new ValueStorage.Forwarded();
    private void textpropVars() {
        initForwardTo(DEFAULT_TEXT_PROPERTIES, defaultTextProperties);
        initForwardTo(CHAR_PROPERTY_ALIAS_ALIST, charPropertyAliasAlist);
        initForwardTo(INHIBIT_POINT_MOTION_HOOKS, inhibitPointMotionHooks);
        initForwardTo(TEXT_PROPERTY_DEFAULT_NONSTICKY, textPropertyDefaultNonsticky);
    }
    private final ValueStorage.ForwardedBool currentTimeList = new ValueStorage.ForwardedBool(true);
    private void timefnsVars() {
        initForwardTo(CURRENT_TIME_LIST, currentTimeList);
    }
    private final ValueStorage.Forwarded treesitLoadNameOverrideList = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded treesitExtraLoadPath = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded treesitThingSettings = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded treesitLanguageRemapAlist = new ValueStorage.Forwarded(false);
    private void treesitVars() {
        initForwardTo(TREESIT_LOAD_NAME_OVERRIDE_LIST, treesitLoadNameOverrideList);
        initForwardTo(TREESIT_EXTRA_LOAD_PATH, treesitExtraLoadPath);
        initForwardTo(TREESIT_THING_SETTINGS, treesitThingSettings);
        initForwardTo(TREESIT_LANGUAGE_REMAP_ALIST, treesitLanguageRemapAlist);
    }
    private final ValueStorage.Forwarded tempBufferShowFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded minibufferScrollWindow = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool modeLineInNonSelectedWindows = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded otherWindowScrollBuffer = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded otherWindowScrollDefault = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool autoWindowVscroll = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedLong nextScreenContextLines = new ValueStorage.ForwardedLong(2);
    private final ValueStorage.Forwarded scrollPreserveScreenPosition = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowPointInsertionType = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowBufferChangeFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowSizeChangeFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowSelectionChangeFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowStateChangeFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowStateChangeHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowConfigurationChangeHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowRestoreKilledBufferWindows = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded recenterRedisplay = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded windowCombinationResize = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded windowCombinationLimit = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded windowPersistentParameters = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool windowResizePixelwise = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool fastButImpreciseScrolling = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded windowDeadWindowsTable = new ValueStorage.Forwarded();
    private void windowVars() {
        initForwardTo(TEMP_BUFFER_SHOW_FUNCTION, tempBufferShowFunction);
        initForwardTo(MINIBUFFER_SCROLL_WINDOW, minibufferScrollWindow);
        initForwardTo(MODE_LINE_IN_NON_SELECTED_WINDOWS, modeLineInNonSelectedWindows);
        initForwardTo(OTHER_WINDOW_SCROLL_BUFFER, otherWindowScrollBuffer);
        initForwardTo(OTHER_WINDOW_SCROLL_DEFAULT, otherWindowScrollDefault);
        initForwardTo(AUTO_WINDOW_VSCROLL, autoWindowVscroll);
        initForwardTo(NEXT_SCREEN_CONTEXT_LINES, nextScreenContextLines);
        initForwardTo(SCROLL_PRESERVE_SCREEN_POSITION, scrollPreserveScreenPosition);
        initForwardTo(WINDOW_POINT_INSERTION_TYPE, windowPointInsertionType);
        initForwardTo(WINDOW_BUFFER_CHANGE_FUNCTIONS, windowBufferChangeFunctions);
        initForwardTo(WINDOW_SIZE_CHANGE_FUNCTIONS, windowSizeChangeFunctions);
        initForwardTo(WINDOW_SELECTION_CHANGE_FUNCTIONS, windowSelectionChangeFunctions);
        initForwardTo(WINDOW_STATE_CHANGE_FUNCTIONS, windowStateChangeFunctions);
        initForwardTo(WINDOW_STATE_CHANGE_HOOK, windowStateChangeHook);
        initForwardTo(WINDOW_CONFIGURATION_CHANGE_HOOK, windowConfigurationChangeHook);
        initForwardTo(WINDOW_RESTORE_KILLED_BUFFER_WINDOWS, windowRestoreKilledBufferWindows);
        initForwardTo(RECENTER_REDISPLAY, recenterRedisplay);
        initForwardTo(WINDOW_COMBINATION_RESIZE, windowCombinationResize);
        initForwardTo(WINDOW_COMBINATION_LIMIT, windowCombinationLimit);
        initForwardTo(WINDOW_PERSISTENT_PARAMETERS, windowPersistentParameters);
        initForwardTo(WINDOW_RESIZE_PIXELWISE, windowResizePixelwise);
        initForwardTo(FAST_BUT_IMPRECISE_SCROLLING, fastButImpreciseScrolling);
        initForwardTo(WINDOW_DEAD_WINDOWS_TABLE, windowDeadWindowsTable);
    }
    private final ValueStorage.ForwardedBool scrollMinibufferConservatively = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool inhibitMessage = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded messagesBufferName = new ValueStorage.Forwarded(new ELispString("*Messages*"));
    private final ValueStorage.ForwardedBool xStretchCursor = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded showTrailingWhitespace = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded modeLineCompact = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded nobreakCharDisplay = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool nobreakCharAsciiDisplay = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded voidTextAreaPointer = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded inhibitRedisplay = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded globalModeString = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded overlayArrowPosition = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded overlayArrowString = new ValueStorage.Forwarded(new ELispString("=>"));
    private final ValueStorage.Forwarded overlayArrowVariableList = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedLong scrollStep = new ValueStorage.ForwardedLong();
    private final ValueStorage.ForwardedLong scrollConservatively = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.ForwardedLong scrollMargin = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.Forwarded maximumScrollMargin = new ValueStorage.Forwarded(0.25);
    private final ValueStorage.Forwarded displayPixelsPerInch = new ValueStorage.Forwarded(72.0);
    private final ValueStorage.ForwardedLong debugEndPos = new ValueStorage.ForwardedLong();
    private final ValueStorage.Forwarded truncatePartialWidthWindows = new ValueStorage.Forwarded(50L);
    private final ValueStorage.ForwardedBool wordWrapByCategory = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded lineNumberDisplayLimit = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong lineNumberDisplayLimitWidth = new ValueStorage.ForwardedLong(200);
    private final ValueStorage.ForwardedBool highlightNonselectedWindows = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool multipleFrames = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded frameTitleFormat = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded iconTitleFormat = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded messageLogMax = new ValueStorage.Forwarded(1000L);
    private final ValueStorage.Forwarded windowScrollFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded mouseAutoselectWindow = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded autoResizeTabBars = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool autoRaiseTabBarButtons = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded autoResizeToolBars = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool autoRaiseToolBarButtons = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded makeCursorLineFullyVisible = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool makeWindowStartVisible = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded tabBarBorder = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded tabBarButtonMargin = new ValueStorage.Forwarded(1L);
    private final ValueStorage.ForwardedLong tabBarButtonRelief = new ValueStorage.ForwardedLong(1);
    private final ValueStorage.Forwarded toolBarBorder = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded toolBarButtonMargin = new ValueStorage.Forwarded(4L);
    private final ValueStorage.ForwardedLong toolBarButtonRelief = new ValueStorage.ForwardedLong(1);
    private final ValueStorage.Forwarded toolBarStyle = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong toolBarMaxLabelSize = new ValueStorage.ForwardedLong(14);
    private final ValueStorage.Forwarded fontificationFunctions = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool unibyteDisplayViaLanguageEnvironment = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded maxMiniWindowHeight = new ValueStorage.Forwarded(0.25);
    private final ValueStorage.Forwarded resizeMiniWindows = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded blinkCursorAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded autoHscrollMode = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedLong hscrollMargin = new ValueStorage.ForwardedLong(5);
    private final ValueStorage.Forwarded hscrollStep = new ValueStorage.Forwarded(0L);
    private final ValueStorage.ForwardedBool messageTruncateLines = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded menuBarUpdateHook = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded menuUpdatingFrame = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedBool inhibitMenubarUpdate = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded wrapPrefix = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded linePrefix = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded displayLineNumbers = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded displayLineNumbersWidth = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded displayLineNumbersCurrentAbsolute = new ValueStorage.Forwarded(true);
    private final ValueStorage.ForwardedBool displayLineNumbersWiden = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedLong displayLineNumbersOffset = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.ForwardedBool displayFillColumnIndicator = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.Forwarded displayFillColumnIndicatorColumn = new ValueStorage.Forwarded(true);
    private final ValueStorage.Forwarded displayFillColumnIndicatorCharacter = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong displayLineNumbersMajorTick = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.ForwardedLong displayLineNumbersMinorTick = new ValueStorage.ForwardedLong(0);
    private final ValueStorage.ForwardedBool inhibitEvalDuringRedisplay = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool inhibitFreeRealizedFaces = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool inhibitBidiMirroring = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool bidiInhibitBpa = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool inhibitTryWindowId = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool inhibitTryWindowReusing = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedBool inhibitTryCursorMovement = new ValueStorage.ForwardedBool();
    private final ValueStorage.ForwardedLong overlineMargin = new ValueStorage.ForwardedLong(2);
    private final ValueStorage.ForwardedLong underlineMinimumOffset = new ValueStorage.ForwardedLong(1);
    private final ValueStorage.ForwardedBool displayHourglass = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.Forwarded hourglassDelay = new ValueStorage.Forwarded(1L);
    private final ValueStorage.Forwarded preRedisplayFunction = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded glyphlessCharDisplay = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded debugOnMessage = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded setMessageFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded clearMessageFunction = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded redisplayAllWindowsCause = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded redisplayModeLinesCause = new ValueStorage.Forwarded();
    private final ValueStorage.ForwardedBool redisplayInhibitBidi = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool displayRawBytesAsHex = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool mouseFineGrainedTracking = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool tabBarDraggingInProgress = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool redisplaySkipInitialFrame = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool redisplaySkipFontificationOnInput = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedBool redisplayAdhocScrollInResizeMiniWindows = new ValueStorage.ForwardedBool(true);
    private final ValueStorage.ForwardedBool compositionBreakAtPoint = new ValueStorage.ForwardedBool(false);
    private final ValueStorage.ForwardedLong maxRedisplayTicks = new ValueStorage.ForwardedLong(0);
    private void xdispVars() {
        initForwardTo(SCROLL_MINIBUFFER_CONSERVATIVELY, scrollMinibufferConservatively);
        initForwardTo(INHIBIT_MESSAGE, inhibitMessage);
        initForwardTo(MESSAGES_BUFFER_NAME, messagesBufferName);
        initForwardTo(X_STRETCH_CURSOR, xStretchCursor);
        initForwardTo(SHOW_TRAILING_WHITESPACE, showTrailingWhitespace);
        initForwardTo(MODE_LINE_COMPACT, modeLineCompact);
        initForwardTo(NOBREAK_CHAR_DISPLAY, nobreakCharDisplay);
        initForwardTo(NOBREAK_CHAR_ASCII_DISPLAY, nobreakCharAsciiDisplay);
        initForwardTo(VOID_TEXT_AREA_POINTER, voidTextAreaPointer);
        initForwardTo(INHIBIT_REDISPLAY, inhibitRedisplay);
        initForwardTo(GLOBAL_MODE_STRING, globalModeString);
        initForwardTo(OVERLAY_ARROW_POSITION, overlayArrowPosition);
        initForwardTo(OVERLAY_ARROW_STRING, overlayArrowString);
        initForwardTo(OVERLAY_ARROW_VARIABLE_LIST, overlayArrowVariableList);
        initForwardTo(SCROLL_STEP, scrollStep);
        initForwardTo(SCROLL_CONSERVATIVELY, scrollConservatively);
        initForwardTo(SCROLL_MARGIN, scrollMargin);
        initForwardTo(MAXIMUM_SCROLL_MARGIN, maximumScrollMargin);
        initForwardTo(DISPLAY_PIXELS_PER_INCH, displayPixelsPerInch);
        initForwardTo(DEBUG_END_POS, debugEndPos);
        initForwardTo(TRUNCATE_PARTIAL_WIDTH_WINDOWS, truncatePartialWidthWindows);
        initForwardTo(WORD_WRAP_BY_CATEGORY, wordWrapByCategory);
        initForwardTo(LINE_NUMBER_DISPLAY_LIMIT, lineNumberDisplayLimit);
        initForwardTo(LINE_NUMBER_DISPLAY_LIMIT_WIDTH, lineNumberDisplayLimitWidth);
        initForwardTo(HIGHLIGHT_NONSELECTED_WINDOWS, highlightNonselectedWindows);
        initForwardTo(MULTIPLE_FRAMES, multipleFrames);
        initForwardTo(FRAME_TITLE_FORMAT, frameTitleFormat);
        initForwardTo(ICON_TITLE_FORMAT, iconTitleFormat);
        initForwardTo(MESSAGE_LOG_MAX, messageLogMax);
        initForwardTo(WINDOW_SCROLL_FUNCTIONS, windowScrollFunctions);
        initForwardTo(MOUSE_AUTOSELECT_WINDOW, mouseAutoselectWindow);
        initForwardTo(AUTO_RESIZE_TAB_BARS, autoResizeTabBars);
        initForwardTo(AUTO_RAISE_TAB_BAR_BUTTONS, autoRaiseTabBarButtons);
        initForwardTo(AUTO_RESIZE_TOOL_BARS, autoResizeToolBars);
        initForwardTo(AUTO_RAISE_TOOL_BAR_BUTTONS, autoRaiseToolBarButtons);
        initForwardTo(MAKE_CURSOR_LINE_FULLY_VISIBLE, makeCursorLineFullyVisible);
        initForwardTo(MAKE_WINDOW_START_VISIBLE, makeWindowStartVisible);
        initForwardTo(TAB_BAR_BORDER, tabBarBorder);
        initForwardTo(TAB_BAR_BUTTON_MARGIN, tabBarButtonMargin);
        initForwardTo(TAB_BAR_BUTTON_RELIEF, tabBarButtonRelief);
        initForwardTo(TOOL_BAR_BORDER, toolBarBorder);
        initForwardTo(TOOL_BAR_BUTTON_MARGIN, toolBarButtonMargin);
        initForwardTo(TOOL_BAR_BUTTON_RELIEF, toolBarButtonRelief);
        initForwardTo(TOOL_BAR_STYLE, toolBarStyle);
        initForwardTo(TOOL_BAR_MAX_LABEL_SIZE, toolBarMaxLabelSize);
        initForwardTo(FONTIFICATION_FUNCTIONS, fontificationFunctions);
        initForwardTo(UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT, unibyteDisplayViaLanguageEnvironment);
        initForwardTo(MAX_MINI_WINDOW_HEIGHT, maxMiniWindowHeight);
        initForwardTo(RESIZE_MINI_WINDOWS, resizeMiniWindows);
        initForwardTo(BLINK_CURSOR_ALIST, blinkCursorAlist);
        initForwardTo(AUTO_HSCROLL_MODE, autoHscrollMode);
        initForwardTo(HSCROLL_MARGIN, hscrollMargin);
        initForwardTo(HSCROLL_STEP, hscrollStep);
        initForwardTo(MESSAGE_TRUNCATE_LINES, messageTruncateLines);
        initForwardTo(MENU_BAR_UPDATE_HOOK, menuBarUpdateHook);
        initForwardTo(MENU_UPDATING_FRAME, menuUpdatingFrame);
        initForwardTo(INHIBIT_MENUBAR_UPDATE, inhibitMenubarUpdate);
        initForwardTo(WRAP_PREFIX, wrapPrefix);
        initForwardTo(LINE_PREFIX, linePrefix);
        initForwardTo(DISPLAY_LINE_NUMBERS, displayLineNumbers);
        initForwardTo(DISPLAY_LINE_NUMBERS_WIDTH, displayLineNumbersWidth);
        initForwardTo(DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE, displayLineNumbersCurrentAbsolute);
        initForwardTo(DISPLAY_LINE_NUMBERS_WIDEN, displayLineNumbersWiden);
        initForwardTo(DISPLAY_LINE_NUMBERS_OFFSET, displayLineNumbersOffset);
        initForwardTo(DISPLAY_FILL_COLUMN_INDICATOR, displayFillColumnIndicator);
        initForwardTo(DISPLAY_FILL_COLUMN_INDICATOR_COLUMN, displayFillColumnIndicatorColumn);
        initForwardTo(DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER, displayFillColumnIndicatorCharacter);
        initForwardTo(DISPLAY_LINE_NUMBERS_MAJOR_TICK, displayLineNumbersMajorTick);
        initForwardTo(DISPLAY_LINE_NUMBERS_MINOR_TICK, displayLineNumbersMinorTick);
        initForwardTo(INHIBIT_EVAL_DURING_REDISPLAY, inhibitEvalDuringRedisplay);
        initForwardTo(INHIBIT_FREE_REALIZED_FACES, inhibitFreeRealizedFaces);
        initForwardTo(INHIBIT_BIDI_MIRRORING, inhibitBidiMirroring);
        initForwardTo(BIDI_INHIBIT_BPA, bidiInhibitBpa);
        initForwardTo(INHIBIT_TRY_WINDOW_ID, inhibitTryWindowId);
        initForwardTo(INHIBIT_TRY_WINDOW_REUSING, inhibitTryWindowReusing);
        initForwardTo(INHIBIT_TRY_CURSOR_MOVEMENT, inhibitTryCursorMovement);
        initForwardTo(OVERLINE_MARGIN, overlineMargin);
        initForwardTo(UNDERLINE_MINIMUM_OFFSET, underlineMinimumOffset);
        initForwardTo(DISPLAY_HOURGLASS, displayHourglass);
        initForwardTo(HOURGLASS_DELAY, hourglassDelay);
        initForwardTo(PRE_REDISPLAY_FUNCTION, preRedisplayFunction);
        initForwardTo(GLYPHLESS_CHAR_DISPLAY, glyphlessCharDisplay);
        initForwardTo(DEBUG_ON_MESSAGE, debugOnMessage);
        initForwardTo(SET_MESSAGE_FUNCTION, setMessageFunction);
        initForwardTo(CLEAR_MESSAGE_FUNCTION, clearMessageFunction);
        initForwardTo(REDISPLAY__ALL_WINDOWS_CAUSE, redisplayAllWindowsCause);
        initForwardTo(REDISPLAY__MODE_LINES_CAUSE, redisplayModeLinesCause);
        initForwardTo(REDISPLAY__INHIBIT_BIDI, redisplayInhibitBidi);
        initForwardTo(DISPLAY_RAW_BYTES_AS_HEX, displayRawBytesAsHex);
        initForwardTo(MOUSE_FINE_GRAINED_TRACKING, mouseFineGrainedTracking);
        initForwardTo(TAB_BAR__DRAGGING_IN_PROGRESS, tabBarDraggingInProgress);
        initForwardTo(REDISPLAY_SKIP_INITIAL_FRAME, redisplaySkipInitialFrame);
        initForwardTo(REDISPLAY_SKIP_FONTIFICATION_ON_INPUT, redisplaySkipFontificationOnInput);
        initForwardTo(REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS, redisplayAdhocScrollInResizeMiniWindows);
        initForwardTo(COMPOSITION_BREAK_AT_POINT, compositionBreakAtPoint);
        initForwardTo(MAX_REDISPLAY_TICKS, maxRedisplayTicks);
    }
    private final ValueStorage.ForwardedBool faceFiltersAlwaysMatch = new ValueStorage.ForwardedBool();
    private final ValueStorage.Forwarded faceNewFrameDefaults = new ValueStorage.Forwarded();
    private final ValueStorage.Forwarded faceDefaultStipple = new ValueStorage.Forwarded(new ELispString("gray3"));
    private final ValueStorage.Forwarded ttyDefinedColorAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded scalableFontsAllowed = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded faceIgnoredFonts = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded faceRemappingAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.Forwarded faceFontRescaleAlist = new ValueStorage.Forwarded(false);
    private final ValueStorage.ForwardedLong faceNearSameColorThreshold = new ValueStorage.ForwardedLong(30000);
    private final ValueStorage.Forwarded faceFontLaxMatchedAttributes = new ValueStorage.Forwarded(true);
    private void xfacesVars() {
        initForwardTo(FACE_FILTERS_ALWAYS_MATCH, faceFiltersAlwaysMatch);
        initForwardTo(FACE__NEW_FRAME_DEFAULTS, faceNewFrameDefaults);
        initForwardTo(FACE_DEFAULT_STIPPLE, faceDefaultStipple);
        initForwardTo(TTY_DEFINED_COLOR_ALIST, ttyDefinedColorAlist);
        initForwardTo(SCALABLE_FONTS_ALLOWED, scalableFontsAllowed);
        initForwardTo(FACE_IGNORED_FONTS, faceIgnoredFonts);
        initForwardTo(FACE_REMAPPING_ALIST, faceRemappingAlist);
        initForwardTo(FACE_FONT_RESCALE_ALIST, faceFontRescaleAlist);
        initForwardTo(FACE_NEAR_SAME_COLOR_THRESHOLD, faceNearSameColorThreshold);
        initForwardTo(FACE_FONT_LAX_MATCHED_ATTRIBUTES, faceFontLaxMatchedAttributes);
    }
    //#endregion initGlobalVariables
    //#region initializations
    public void postInitVariables() {
        initObarrayOnce();
        builtInSyntax.initSyntaxOnce(ctx);
        initCategoryOnce();
        builtInCaseTab.initCaseTabOnce(ctx);
        initBufferOnce();
        initMinibufOnce();
        symsOfXfaces();
        symsOfKeymap();
        symsOfKeyboard();
        symsOfData();
        symsOfFns();
        symsOfFileio();
        symsOfAlloc();
        symsOfCharset();
        symsOfCoding();
        initWindowOnce();
        symsOfComp();
        initBuffer();
        initCallproc1();
        initLread();
        symsOfLread();
        symsOfPrint();
        symsOfEval();
        symsOfBuffer();
        symsOfCasefiddle();
        symsOfCcl();
        symsOfCharacter();
        symsOfEditfns();
        symsOfEmacs();
        symsOfMinibuf();
        symsOfProcess();
        symsOfSearch();
        symsOfFrame();
        symsOfSyntax();
        symsOfTerminal();
        symsOfTreesit();
        symsOfTextprop();
        symsOfComposite();
        symsOfWindow();
        symsOfXdisp();
        initCharset();
        initKeyboard();
    }

    private void initObarrayOnce() {
        NIL.setValue(NIL);
        NIL.setConstant(true);
        NIL.setSpecial(true);
        T.setValue(T);
        T.setConstant(true);
        T.setSpecial(true);
    }
    private void initCategoryOnce() {
        FPut.put(CATEGORY_TABLE, CHAR_TABLE_EXTRA_SLOTS, (long) (2));
        var standardCategoryTable = FMakeCharTable.makeCharTable(CATEGORY_TABLE, NIL);
        standardCategoryTable.setDefault(FMakeBoolVector.makeBoolVector((long) (128), NIL));
        FSetCharTableExtraSlot.setCharTableExtraSlot(standardCategoryTable, (long) (0), new ELispVector(95, false));
        bufferDefaults.setCategoryTable(standardCategoryTable);
    }
    private void initBufferOnce() {
        ELispBuffer.initBufferLocalVars(ctx, bufferDefaults, builtInBuffer);
    }
    private void initMinibufOnce() {
        getMiniBuffer(0);
    }
    private void symsOfXfaces() {
        var faceNewFrameDefaultsJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
        faceNewFrameDefaults.setValue(faceNewFrameDefaultsJInit);
    }
    private void symsOfKeymap() {
        FPut.put(KEYMAP, CHAR_TABLE_EXTRA_SLOTS, (long) (0));
        var minibufferLocalMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        minibufferLocalMap.setValue(minibufferLocalMapJInit);
    }
    private void symsOfKeyboard() {
        var regularTopLevelMessage = new ELispString("Back to top level");
        var internalTopLevelMessageJInit = regularTopLevelMessage;
        internalTopLevelMessage.setValue(internalTopLevelMessageJInit);
        INPUT_METHOD_EXIT_ON_FIRST_CHAR.setValue(NIL);
        INPUT_METHOD_USE_ECHO_AREA.setValue(NIL);
        FPut.put(MOUSE_MOVEMENT, EVENT_KIND, MOUSE_MOVEMENT);
        FPut.put(MOUSE_MOVEMENT, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(MOUSE_MOVEMENT));
        FPut.put(SCROLL_BAR_MOVEMENT, EVENT_KIND, MOUSE_MOVEMENT);
        FPut.put(SCROLL_BAR_MOVEMENT, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(SCROLL_BAR_MOVEMENT));
        FPut.put(SWITCH_FRAME, EVENT_KIND, SWITCH_FRAME);
        FPut.put(SWITCH_FRAME, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(SWITCH_FRAME));
        FPut.put(FOCUS_IN, EVENT_KIND, FOCUS_IN);
        FPut.put(FOCUS_IN, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(FOCUS_IN));
        FPut.put(FOCUS_OUT, EVENT_KIND, FOCUS_OUT);
        FPut.put(FOCUS_OUT, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(FOCUS_OUT));
        FPut.put(MOVE_FRAME, EVENT_KIND, MOVE_FRAME);
        FPut.put(MOVE_FRAME, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(MOVE_FRAME));
        FPut.put(DELETE_FRAME, EVENT_KIND, DELETE_FRAME);
        FPut.put(DELETE_FRAME, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(DELETE_FRAME));
        FPut.put(ICONIFY_FRAME, EVENT_KIND, ICONIFY_FRAME);
        FPut.put(ICONIFY_FRAME, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(ICONIFY_FRAME));
        FPut.put(MAKE_FRAME_VISIBLE, EVENT_KIND, MAKE_FRAME_VISIBLE);
        FPut.put(MAKE_FRAME_VISIBLE, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(MAKE_FRAME_VISIBLE));
        FPut.put(SELECT_WINDOW, EVENT_KIND, SWITCH_FRAME);
        FPut.put(SELECT_WINDOW, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(SELECT_WINDOW));
        FPut.put(TOUCHSCREEN_BEGIN, EVENT_KIND, TOUCHSCREEN);
        FPut.put(TOUCHSCREEN_BEGIN, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(TOUCHSCREEN_BEGIN));
        FPut.put(TOUCHSCREEN_END, EVENT_KIND, TOUCHSCREEN);
        FPut.put(TOUCHSCREEN_END, EVENT_SYMBOL_ELEMENTS, ELispCons.listOf(TOUCHSCREEN_END));
        var modifierSymbols = new ELispVector(28, false);
        modifierSymbols.set(0, UP);
        modifierSymbols.set(1, DOWN);
        modifierSymbols.set(2, intern("drag"));
        modifierSymbols.set(3, intern("click"));
        modifierSymbols.set(4, intern("double"));
        modifierSymbols.set(5, intern("triple"));
        modifierSymbols.set(22, intern("alt"));
        modifierSymbols.set(23, intern("super"));
        modifierSymbols.set(24, intern("hyper"));
        modifierSymbols.set(25, intern("shift"));
        modifierSymbols.set(26, intern("control"));
        modifierSymbols.set(27, intern("meta"));
        TOP_LEVEL.setSpecial(false);
        DEACTIVATE_MARK.setBufferLocal(true);
        ECHO_AREA_CLEAR_HOOK.setValue(NIL);
        var specialEventMapJInit = ELispCons.listOf(KEYMAP);
        specialEventMap.setValue(specialEventMapJInit);
        var functionKeyMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        functionKeyMap.setValue(functionKeyMapJInit);
        var keyTranslationMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        keyTranslationMap.setValue(keyTranslationMapJInit);
        var inputMethodFunctionJInit = LIST;
        inputMethodFunction.setValue(inputMethodFunctionJInit);
        var commandErrorFunctionJInit = COMMAND_ERROR_DEFAULT_FUNCTION;
        commandErrorFunction.setValue(commandErrorFunctionJInit);
        var selectionInhibitUpdateCommandsJInit = ELispCons.listOf(HANDLE_SWITCH_FRAME, HANDLE_SELECT_WINDOW);
        selectionInhibitUpdateCommands.setValue(selectionInhibitUpdateCommandsJInit);
        var debugOnEventJInit = SIGUSR2;
        debugOnEvent.setValue(debugOnEventJInit);
        var events = ELispCons.listOf(
            SELECT_WINDOW,
            HELP_ECHO,
            MOVE_FRAME,
            ICONIFY_FRAME,
            MAKE_FRAME_VISIBLE,
            FOCUS_IN,
            FOCUS_OUT,
            CONFIG_CHANGED_EVENT,
            SELECTION_REQUEST
        );
        var whileNoInputIgnoreEventsJInit = events;
        whileNoInputIgnoreEvents.setValue(whileNoInputIgnoreEventsJInit);
        var initialKboard = false /* TODO */;
    }
    private void symsOfData() {
        var errorTail = FCons.cons(ERROR, NIL);
        FPut.put(ERROR, ERROR_CONDITIONS, errorTail);
        FPut.put(ERROR, ERROR_MESSAGE, new ELispString("error"));
        FPut.put(QUIT, ERROR_CONDITIONS, FCons.cons(QUIT, NIL));
        FPut.put(QUIT, ERROR_MESSAGE, new ELispString("Quit"));
        FPut.put(MINIBUFFER_QUIT, ERROR_CONDITIONS, FCons.cons(MINIBUFFER_QUIT, FCons.cons(QUIT, NIL)));
        FPut.put(MINIBUFFER_QUIT, ERROR_MESSAGE, new ELispString("Quit"));
        FPut.put(USER_ERROR, ERROR_CONDITIONS, FCons.cons(USER_ERROR, errorTail));
        FPut.put(USER_ERROR, ERROR_MESSAGE, new ELispString(""));
        FPut.put(WRONG_LENGTH_ARGUMENT, ERROR_CONDITIONS, FCons.cons(WRONG_LENGTH_ARGUMENT, errorTail));
        FPut.put(WRONG_LENGTH_ARGUMENT, ERROR_MESSAGE, new ELispString("Wrong length argument"));
        FPut.put(WRONG_TYPE_ARGUMENT, ERROR_CONDITIONS, FCons.cons(WRONG_TYPE_ARGUMENT, errorTail));
        FPut.put(WRONG_TYPE_ARGUMENT, ERROR_MESSAGE, new ELispString("Wrong type argument"));
        FPut.put(TYPE_MISMATCH, ERROR_CONDITIONS, FCons.cons(TYPE_MISMATCH, errorTail));
        FPut.put(TYPE_MISMATCH, ERROR_MESSAGE, new ELispString("Types do not match"));
        FPut.put(ARGS_OUT_OF_RANGE, ERROR_CONDITIONS, FCons.cons(ARGS_OUT_OF_RANGE, errorTail));
        FPut.put(ARGS_OUT_OF_RANGE, ERROR_MESSAGE, new ELispString("Args out of range"));
        FPut.put(VOID_FUNCTION, ERROR_CONDITIONS, FCons.cons(VOID_FUNCTION, errorTail));
        FPut.put(VOID_FUNCTION, ERROR_MESSAGE, new ELispString("Symbol's function definition is void"));
        FPut.put(CYCLIC_FUNCTION_INDIRECTION, ERROR_CONDITIONS, FCons.cons(CYCLIC_FUNCTION_INDIRECTION, errorTail));
        FPut.put(CYCLIC_FUNCTION_INDIRECTION, ERROR_MESSAGE, new ELispString("Symbol's chain of function indirections contains a loop"));
        FPut.put(CYCLIC_VARIABLE_INDIRECTION, ERROR_CONDITIONS, FCons.cons(CYCLIC_VARIABLE_INDIRECTION, errorTail));
        FPut.put(CYCLIC_VARIABLE_INDIRECTION, ERROR_MESSAGE, new ELispString("Symbol's chain of variable indirections contains a loop"));
        FPut.put(CIRCULAR_LIST, ERROR_CONDITIONS, FCons.cons(CIRCULAR_LIST, errorTail));
        FPut.put(CIRCULAR_LIST, ERROR_MESSAGE, new ELispString("List contains a loop"));
        FPut.put(VOID_VARIABLE, ERROR_CONDITIONS, FCons.cons(VOID_VARIABLE, errorTail));
        FPut.put(VOID_VARIABLE, ERROR_MESSAGE, new ELispString("Symbol's value as variable is void"));
        FPut.put(SETTING_CONSTANT, ERROR_CONDITIONS, FCons.cons(SETTING_CONSTANT, errorTail));
        FPut.put(SETTING_CONSTANT, ERROR_MESSAGE, new ELispString("Attempt to set a constant symbol"));
        FPut.put(TRAPPING_CONSTANT, ERROR_CONDITIONS, FCons.cons(TRAPPING_CONSTANT, errorTail));
        FPut.put(TRAPPING_CONSTANT, ERROR_MESSAGE, new ELispString("Attempt to trap writes to a constant symbol"));
        FPut.put(INVALID_READ_SYNTAX, ERROR_CONDITIONS, FCons.cons(INVALID_READ_SYNTAX, errorTail));
        FPut.put(INVALID_READ_SYNTAX, ERROR_MESSAGE, new ELispString("Invalid read syntax"));
        FPut.put(INVALID_FUNCTION, ERROR_CONDITIONS, FCons.cons(INVALID_FUNCTION, errorTail));
        FPut.put(INVALID_FUNCTION, ERROR_MESSAGE, new ELispString("Invalid function"));
        FPut.put(WRONG_NUMBER_OF_ARGUMENTS, ERROR_CONDITIONS, FCons.cons(WRONG_NUMBER_OF_ARGUMENTS, errorTail));
        FPut.put(WRONG_NUMBER_OF_ARGUMENTS, ERROR_MESSAGE, new ELispString("Wrong number of arguments"));
        FPut.put(NO_CATCH, ERROR_CONDITIONS, FCons.cons(NO_CATCH, errorTail));
        FPut.put(NO_CATCH, ERROR_MESSAGE, new ELispString("No catch for tag"));
        FPut.put(END_OF_FILE, ERROR_CONDITIONS, FCons.cons(END_OF_FILE, errorTail));
        FPut.put(END_OF_FILE, ERROR_MESSAGE, new ELispString("End of file during parsing"));
        var arithTail = FCons.cons(ARITH_ERROR, errorTail);
        FPut.put(ARITH_ERROR, ERROR_CONDITIONS, arithTail);
        FPut.put(ARITH_ERROR, ERROR_MESSAGE, new ELispString("Arithmetic error"));
        FPut.put(BEGINNING_OF_BUFFER, ERROR_CONDITIONS, FCons.cons(BEGINNING_OF_BUFFER, errorTail));
        FPut.put(BEGINNING_OF_BUFFER, ERROR_MESSAGE, new ELispString("Beginning of buffer"));
        FPut.put(END_OF_BUFFER, ERROR_CONDITIONS, FCons.cons(END_OF_BUFFER, errorTail));
        FPut.put(END_OF_BUFFER, ERROR_MESSAGE, new ELispString("End of buffer"));
        FPut.put(BUFFER_READ_ONLY, ERROR_CONDITIONS, FCons.cons(BUFFER_READ_ONLY, errorTail));
        FPut.put(BUFFER_READ_ONLY, ERROR_MESSAGE, new ELispString("Buffer is read-only"));
        FPut.put(TEXT_READ_ONLY, ERROR_CONDITIONS, FCons.cons(TEXT_READ_ONLY, FCons.cons(BUFFER_READ_ONLY, errorTail)));
        FPut.put(TEXT_READ_ONLY, ERROR_MESSAGE, new ELispString("Text is read-only"));
        FPut.put(INHIBITED_INTERACTION, ERROR_CONDITIONS, FCons.cons(INHIBITED_INTERACTION, errorTail));
        FPut.put(INHIBITED_INTERACTION, ERROR_MESSAGE, new ELispString("User interaction while inhibited"));
        FPut.put(DOMAIN_ERROR, ERROR_CONDITIONS, FCons.cons(DOMAIN_ERROR, arithTail));
        FPut.put(DOMAIN_ERROR, ERROR_MESSAGE, new ELispString("Arithmetic domain error"));
        FPut.put(RANGE_ERROR, ERROR_CONDITIONS, FCons.cons(RANGE_ERROR, arithTail));
        FPut.put(RANGE_ERROR, ERROR_MESSAGE, new ELispString("Arithmetic range error"));
        FPut.put(SINGULARITY_ERROR, ERROR_CONDITIONS, FCons.cons(SINGULARITY_ERROR, FCons.cons(DOMAIN_ERROR, arithTail)));
        FPut.put(SINGULARITY_ERROR, ERROR_MESSAGE, new ELispString("Arithmetic singularity error"));
        FPut.put(OVERFLOW_ERROR, ERROR_CONDITIONS, FCons.cons(OVERFLOW_ERROR, FCons.cons(RANGE_ERROR, arithTail)));
        FPut.put(OVERFLOW_ERROR, ERROR_MESSAGE, new ELispString("Arithmetic overflow error"));
        FPut.put(UNDERFLOW_ERROR, ERROR_CONDITIONS, FCons.cons(UNDERFLOW_ERROR, FCons.cons(RANGE_ERROR, arithTail)));
        FPut.put(UNDERFLOW_ERROR, ERROR_MESSAGE, new ELispString("Arithmetic underflow error"));
        var recursionTail = FCons.cons(RECURSION_ERROR, errorTail);
        FPut.put(RECURSION_ERROR, ERROR_CONDITIONS, recursionTail);
        FPut.put(RECURSION_ERROR, ERROR_MESSAGE, new ELispString("Excessive recursive calling error"));
        FPut.put(EXCESSIVE_LISP_NESTING, ERROR_CONDITIONS, FCons.cons(EXCESSIVE_LISP_NESTING, recursionTail));
        FPut.put(EXCESSIVE_LISP_NESTING, ERROR_MESSAGE, new ELispString("Lisp nesting exceeds `max-lisp-eval-depth'"));
        FPut.put(EXCESSIVE_VARIABLE_BINDING, ERROR_CONDITIONS, FCons.cons(EXCESSIVE_VARIABLE_BINDING, recursionTail));
        FPut.put(EXCESSIVE_VARIABLE_BINDING, ERROR_MESSAGE, new ELispString("Variable binding depth exceeds max-specpdl-size"));
        MOST_POSITIVE_FIXNUM.setConstant(true);
        MOST_NEGATIVE_FIXNUM.setConstant(true);
    }
    private void symsOfFns() {
        YES_OR_NO_P_HISTORY.setValue(NIL);
        var featuresJInit = ELispCons.listOf(EMACS);
        features.setValue(featuresJInit);
        FEATURES.setSpecial(false);
    }
    private void symsOfFileio() {
        FILE_NAME_HISTORY.setValue(NIL);
        FPut.put(FILE_ERROR, ERROR_CONDITIONS, ELispCons.listOf(FILE_ERROR, ERROR));
        FPut.put(FILE_ERROR, ERROR_MESSAGE, new ELispString("File error"));
        FPut.put(FILE_ALREADY_EXISTS, ERROR_CONDITIONS, ELispCons.listOf(FILE_ALREADY_EXISTS, FILE_ERROR, ERROR));
        FPut.put(FILE_ALREADY_EXISTS, ERROR_MESSAGE, new ELispString("File already exists"));
        FPut.put(FILE_DATE_ERROR, ERROR_CONDITIONS, ELispCons.listOf(FILE_DATE_ERROR, FILE_ERROR, ERROR));
        FPut.put(FILE_DATE_ERROR, ERROR_MESSAGE, new ELispString("Cannot set file date"));
        FPut.put(FILE_MISSING, ERROR_CONDITIONS, ELispCons.listOf(FILE_MISSING, FILE_ERROR, ERROR));
        FPut.put(FILE_MISSING, ERROR_MESSAGE, new ELispString("File is missing"));
        FPut.put(PERMISSION_DENIED, ERROR_CONDITIONS, ELispCons.listOf(PERMISSION_DENIED, FILE_ERROR, ERROR));
        FPut.put(PERMISSION_DENIED, ERROR_MESSAGE, new ELispString("Cannot access file or directory"));
        FPut.put(FILE_NOTIFY_ERROR, ERROR_CONDITIONS, ELispCons.listOf(FILE_NOTIFY_ERROR, FILE_ERROR, ERROR));
        FPut.put(FILE_NOTIFY_ERROR, ERROR_MESSAGE, new ELispString("File notification error"));
        FPut.put(REMOTE_FILE_ERROR, ERROR_CONDITIONS, ELispCons.listOf(REMOTE_FILE_ERROR, FILE_ERROR, ERROR));
        FPut.put(REMOTE_FILE_ERROR, ERROR_MESSAGE, new ELispString("Remote file error"));
    }
    private void symsOfAlloc() {
        var memorySignalDataJInit = ELispCons.listOf(ERROR, new ELispString("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs"));
        memorySignalData.setValue(memorySignalDataJInit);
    }
    private void symsOfCharset() {
        var charsetAscii = builtInCharSet.defineCharsetInternal(
            this,
            ASCII,
            1,
            "\u0000\u007f\u0000\u0000\u0000\u0000\u0000",
            0,
            127,
            66,
            -1,
            0,
            1,
            0,
            0
        );
        var charsetIso88591 = builtInCharSet.defineCharsetInternal(
            this,
            ISO_8859_1,
            1,
            "\u0000\u00ff\u0000\u0000\u0000\u0000\u0000",
            0,
            255,
            -1,
            -1,
            -1,
            1,
            0,
            0
        );
        var charsetUnicode = builtInCharSet.defineCharsetInternal(
            this,
            UNICODE,
            3,
            "\u0000\u00ff\u0000\u00ff\u0000\u0010\u0000",
            0,
            1114111,
            -1,
            0,
            -1,
            1,
            0,
            0
        );
        var charsetEmacs = builtInCharSet.defineCharsetInternal(
            this,
            EMACS,
            3,
            "\u0000\u00ff\u0000\u00ff\u0000?\u0000",
            0,
            MAX_5_BYTE_CHAR,
            -1,
            0,
            -1,
            1,
            1,
            0
        );
        var charsetEightBit = builtInCharSet.defineCharsetInternal(
            this,
            EIGHT_BIT,
            1,
            "\u0080\u00ff\u0000\u0000\u0000\u0000\u0000",
            128,
            255,
            -1,
            0,
            -1,
            0,
            1,
            4194176
        );
        var charsetUnibyte = charsetIso88591;
    }
    private void symsOfCoding() {
        CODING_SYSTEM_HISTORY.setValue(NIL);
        FPut.put(INSERT_FILE_CONTENTS, TARGET_IDX, (long) (0));
        FPut.put(WRITE_REGION, TARGET_IDX, (long) (2));
        FPut.put(CALL_PROCESS, TARGET_IDX, (long) (0));
        FPut.put(CALL_PROCESS_REGION, TARGET_IDX, (long) (2));
        FPut.put(START_PROCESS, TARGET_IDX, (long) (2));
        FPut.put(OPEN_NETWORK_STREAM, TARGET_IDX, (long) (3));
        FPut.put(CODING_SYSTEM_ERROR, ERROR_CONDITIONS, ELispCons.listOf(CODING_SYSTEM_ERROR, ERROR));
        FPut.put(CODING_SYSTEM_ERROR, ERROR_MESSAGE, new ELispString("Invalid coding system"));
        FPut.put(TRANSLATION_TABLE, CHAR_TABLE_EXTRA_SLOTS, (long) (2));
        var codingCategoryTable = new ELispVector(CODING_CATEGORY_MAX, false);
        codingCategoryTable.set(CODING_CATEGORY_ISO_7, intern("coding-category-iso-7"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_7_TIGHT, intern("coding-category-iso-7-tight"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_8_1, intern("coding-category-iso-8-1"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_8_2, intern("coding-category-iso-8-2"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_7_ELSE, intern("coding-category-iso-7-else"));
        codingCategoryTable.set(CODING_CATEGORY_ISO_8_ELSE, intern("coding-category-iso-8-else"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_8_AUTO, intern("coding-category-utf-8-auto"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_8_NOSIG, intern("coding-category-utf-8"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_8_SIG, intern("coding-category-utf-8-sig"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_BE, intern("coding-category-utf-16-be"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_AUTO, intern("coding-category-utf-16-auto"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_LE, intern("coding-category-utf-16-le"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_BE_NOSIG, intern("coding-category-utf-16-be-nosig"));
        codingCategoryTable.set(CODING_CATEGORY_UTF_16_LE_NOSIG, intern("coding-category-utf-16-le-nosig"));
        codingCategoryTable.set(CODING_CATEGORY_CHARSET, intern("coding-category-charset"));
        codingCategoryTable.set(CODING_CATEGORY_SJIS, intern("coding-category-sjis"));
        codingCategoryTable.set(CODING_CATEGORY_BIG5, intern("coding-category-big5"));
        codingCategoryTable.set(CODING_CATEGORY_CCL, intern("coding-category-ccl"));
        codingCategoryTable.set(CODING_CATEGORY_EMACS_MULE, intern("coding-category-emacs-mule"));
        codingCategoryTable.set(CODING_CATEGORY_RAW_TEXT, intern("coding-category-raw-text"));
        codingCategoryTable.set(CODING_CATEGORY_UNDECIDED, intern("coding-category-undecided"));
        var codingCategoryListJInit = FCons.cons(codingCategoryTable.get(20), codingCategoryList.getValue());
        codingCategoryList.setValue(codingCategoryListJInit);
        var codingCategoryListJInit1 = FCons.cons(codingCategoryTable.get(19), codingCategoryListJInit);
        codingCategoryList.setValue(codingCategoryListJInit1);
        var codingCategoryListJInit2 = FCons.cons(codingCategoryTable.get(18), codingCategoryListJInit1);
        codingCategoryList.setValue(codingCategoryListJInit2);
        var codingCategoryListJInit3 = FCons.cons(codingCategoryTable.get(17), codingCategoryListJInit2);
        codingCategoryList.setValue(codingCategoryListJInit3);
        var codingCategoryListJInit4 = FCons.cons(codingCategoryTable.get(16), codingCategoryListJInit3);
        codingCategoryList.setValue(codingCategoryListJInit4);
        var codingCategoryListJInit5 = FCons.cons(codingCategoryTable.get(15), codingCategoryListJInit4);
        codingCategoryList.setValue(codingCategoryListJInit5);
        var codingCategoryListJInit6 = FCons.cons(codingCategoryTable.get(14), codingCategoryListJInit5);
        codingCategoryList.setValue(codingCategoryListJInit6);
        var codingCategoryListJInit7 = FCons.cons(codingCategoryTable.get(13), codingCategoryListJInit6);
        codingCategoryList.setValue(codingCategoryListJInit7);
        var codingCategoryListJInit8 = FCons.cons(codingCategoryTable.get(12), codingCategoryListJInit7);
        codingCategoryList.setValue(codingCategoryListJInit8);
        var codingCategoryListJInit9 = FCons.cons(codingCategoryTable.get(11), codingCategoryListJInit8);
        codingCategoryList.setValue(codingCategoryListJInit9);
        var codingCategoryListJInit10 = FCons.cons(codingCategoryTable.get(10), codingCategoryListJInit9);
        codingCategoryList.setValue(codingCategoryListJInit10);
        var codingCategoryListJInit11 = FCons.cons(codingCategoryTable.get(9), codingCategoryListJInit10);
        codingCategoryList.setValue(codingCategoryListJInit11);
        var codingCategoryListJInit12 = FCons.cons(codingCategoryTable.get(8), codingCategoryListJInit11);
        codingCategoryList.setValue(codingCategoryListJInit12);
        var codingCategoryListJInit13 = FCons.cons(codingCategoryTable.get(7), codingCategoryListJInit12);
        codingCategoryList.setValue(codingCategoryListJInit13);
        var codingCategoryListJInit14 = FCons.cons(codingCategoryTable.get(6), codingCategoryListJInit13);
        codingCategoryList.setValue(codingCategoryListJInit14);
        var codingCategoryListJInit15 = FCons.cons(codingCategoryTable.get(5), codingCategoryListJInit14);
        codingCategoryList.setValue(codingCategoryListJInit15);
        var codingCategoryListJInit16 = FCons.cons(codingCategoryTable.get(4), codingCategoryListJInit15);
        codingCategoryList.setValue(codingCategoryListJInit16);
        var codingCategoryListJInit17 = FCons.cons(codingCategoryTable.get(3), codingCategoryListJInit16);
        codingCategoryList.setValue(codingCategoryListJInit17);
        var codingCategoryListJInit18 = FCons.cons(codingCategoryTable.get(2), codingCategoryListJInit17);
        codingCategoryList.setValue(codingCategoryListJInit18);
        var codingCategoryListJInit19 = FCons.cons(codingCategoryTable.get(1), codingCategoryListJInit18);
        codingCategoryList.setValue(codingCategoryListJInit19);
        var codingCategoryListJInit20 = FCons.cons(codingCategoryTable.get(0), codingCategoryListJInit19);
        codingCategoryList.setValue(codingCategoryListJInit20);
        var latinExtraCodeTableJInit = new ELispVector(256, false);
        latinExtraCodeTable.setValue(latinExtraCodeTableJInit);
        defineCodingSystemInternal(new Object[]{
            NO_CONVERSION,
            (long) (61),
            RAW_TEXT,
            false,
            T,
            false,
            false,
            false,
            false,
            (long) (0),
            T,
            ELispCons.listOf(
                CNAME,
                NO_CONVERSION,
                CMNEMONIC,
                (long) (61),
                intern(":coding-type"),
                RAW_TEXT,
                CASCII_COMPATIBLE_P,
                T,
                CDEFAULT_CHAR,
                (long) (0),
                intern(":for-unibyte"),
                T,
                intern(":docstring"),
                new ELispString("""
Do no conversion.

When you visit a file with this coding, the file is read into a
unibyte buffer as is, thus each byte of a file is treated as a
character."""),
                intern(":eol-type"),
                UNIX
            ),
            UNIX,
            false,
            false,
            false
        });
        defineCodingSystemInternal(new Object[]{
            UNDECIDED,
            (long) (45),
            UNDECIDED,
            ELispCons.listOf(ASCII),
            T,
            false,
            false,
            false,
            false,
            (long) (0),
            NIL,
            ELispCons.listOf(
                CNAME,
                UNDECIDED,
                CMNEMONIC,
                (long) (45),
                intern(":coding-type"),
                UNDECIDED,
                CASCII_COMPATIBLE_P,
                T,
                intern(":charset-list"),
                ELispCons.listOf(ASCII),
                intern(":for-unibyte"),
                NIL,
                intern(":docstring"),
                new ELispString("No conversion on encoding, automatic conversion on decoding."),
                intern(":eol-type"),
                NIL
            ),
            NIL,
            (long) (0),
            (long) (0),
            false
        });
        getCodings().safeTerminalCoding = getCodings().getCodingSystem(NO_CONVERSION);
        asSym(codingCategoryTable.get(0)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(1)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(2)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(3)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(4)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(5)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(6)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(7)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(8)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(9)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(10)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(11)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(12)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(13)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(14)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(15)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(16)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(17)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(18)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(19)).setValue(NO_CONVERSION);
        asSym(codingCategoryTable.get(20)).setValue(NO_CONVERSION);
    }
    private void initWindowOnce() {
        makeInitialWindowFrame();
    }
    private void symsOfComp() {
        FPut.put(NATIVE_COMPILER_ERROR, ERROR_CONDITIONS, ELispCons.listOf(NATIVE_COMPILER_ERROR, ERROR));
        FPut.put(NATIVE_COMPILER_ERROR, ERROR_MESSAGE, new ELispString("Native compiler error"));
        FPut.put(NATIVE_ICE, ERROR_CONDITIONS, ELispCons.listOf(NATIVE_ICE, NATIVE_COMPILER_ERROR, ERROR));
        FPut.put(NATIVE_ICE, ERROR_MESSAGE, new ELispString("Internal native compiler error"));
        FPut.put(NATIVE_LISP_LOAD_FAILED, ERROR_CONDITIONS, ELispCons.listOf(NATIVE_LISP_LOAD_FAILED, ERROR));
        FPut.put(NATIVE_LISP_LOAD_FAILED, ERROR_MESSAGE, new ELispString("Native elisp load failed"));
        FPut.put(NATIVE_LISP_WRONG_RELOC, ERROR_CONDITIONS, ELispCons.listOf(NATIVE_LISP_WRONG_RELOC, NATIVE_LISP_LOAD_FAILED, ERROR));
        FPut.put(NATIVE_LISP_WRONG_RELOC, ERROR_MESSAGE, new ELispString("Primitive redefined or wrong relocation"));
        FPut.put(WRONG_REGISTER_SUBR_CALL, ERROR_CONDITIONS, ELispCons.listOf(WRONG_REGISTER_SUBR_CALL, NATIVE_LISP_LOAD_FAILED, ERROR));
        FPut.put(WRONG_REGISTER_SUBR_CALL, ERROR_MESSAGE, new ELispString("comp--register-subr can only be called during native lisp load phase."));
        FPut.put(NATIVE_LISP_FILE_INCONSISTENT, ERROR_CONDITIONS, ELispCons.listOf(NATIVE_LISP_FILE_INCONSISTENT, NATIVE_LISP_LOAD_FAILED, ERROR));
        FPut.put(NATIVE_LISP_FILE_INCONSISTENT, ERROR_MESSAGE, new ELispString("eln file inconsistent with current runtime configuration, please recompile"));
        FPut.put(COMP_SANITIZER_ERROR, ERROR_CONDITIONS, ELispCons.listOf(COMP_SANITIZER_ERROR, ERROR));
        FPut.put(COMP_SANITIZER_ERROR, ERROR_MESSAGE, new ELispString("Native code sanitizer runtime error"));
        var compDeferredPendingHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
        compDeferredPendingH.setValue(compDeferredPendingHJInit);
        var compElnToElHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
        compElnToElH.setValue(compElnToElHJInit);
        var nativeCompElnLoadPathJInit = FCons.cons(new ELispString("../native-lisp/"), NIL);
        nativeCompElnLoadPath.setValue(nativeCompElnLoadPathJInit);
        var compInstalledTrampolinesHJInit = FMakeHashTable.makeHashTable(new Object[]{});
        compInstalledTrampolinesH.setValue(compInstalledTrampolinesHJInit);
        var compNoNativeFileHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
        compNoNativeFileH.setValue(compNoNativeFileHJInit);
        var compLoadedCompUnitsHJInit = FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, VALUE, CTEST, EQUAL});
        compLoadedCompUnitsH.setValue(compLoadedCompUnitsHJInit);
        var compSubrAritiesHJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQUAL});
        compSubrAritiesH.setValue(compSubrAritiesHJInit);
        FProvide.provide(intern("native-compile"), NIL);
    }
    private void initBuffer() {
        var scratch = new ELispString("*scratch*");
        FSetBuffer.setBuffer(FGetBufferCreate.getBufferCreate(scratch, NIL));
        ELispBuffer.initDirectory();
    }
    private void initCallproc1() {
        var dataDirectoryJInit = decodeEnvPath("EMACSDATA", "/usr/share/emacs/31.0.50/etc", false);
        dataDirectory.setValue(dataDirectoryJInit);
        var dataDirectoryJInit1 = FFileNameAsDirectory.fileNameAsDirectory(FCar.car(dataDirectoryJInit));
        dataDirectory.setValue(dataDirectoryJInit1);
        var docDirectoryJInit = decodeEnvPath("EMACSDOC", "/usr/share/emacs/31.0.50/etc", false);
        docDirectory.setValue(docDirectoryJInit);
        var docDirectoryJInit1 = FFileNameAsDirectory.fileNameAsDirectory(FCar.car(docDirectoryJInit));
        docDirectory.setValue(docDirectoryJInit1);
        var execPathJInit = decodeEnvPath("EMACSPATH", "/usr/lib/emacs/31.0.50/x86_64-pc-linux-gnu", false);
        execPath.setValue(execPathJInit);
        var execDirectoryJInit = FFileNameAsDirectory.fileNameAsDirectory(FCar.car(execPathJInit));
        execDirectory.setValue(execDirectoryJInit);
        var execPathJInit1 = FNconc.nconc(new Object[]{decodeEnvPath("PATH", "", false), execPathJInit});
        execPath.setValue(execPathJInit1);
    }
    private void initLread() {
        var loadPathJInit = checkLoadPath();
        loadPath.setValue(loadPathJInit);
    }
    private void symsOfLread() {
        VALUES.setSpecial(false);
        var loadSuffixesJInit = ELispCons.listOf(new ELispString(".elc"), new ELispString(".el"));
        loadSuffixes.setValue(loadSuffixesJInit);
        var dynamicLibrarySuffixesJInit = initDynlibSuffixes();
        dynamicLibrarySuffixes.setValue(dynamicLibrarySuffixesJInit);
        var loadFileRepSuffixesJInit = ELispCons.listOf(emptyUnibyteString);
        loadFileRepSuffixes.setValue(loadFileRepSuffixesJInit);
        var loadReadFunctionJInit = READ;
        loadReadFunction.setValue(loadReadFunctionJInit);
        var sourceDirectoryJInit = FExpandFileName.expandFileName(new ELispString("../"), FCar.car(decodeEnvPath(null, "", false)));
        sourceDirectory.setValue(sourceDirectoryJInit);
        LEXICAL_BINDING.setBufferLocal(true);
    }
    private void symsOfPrint() {
        var printCharsetTextPropertyJInit = DEFAULT;
        printCharsetTextProperty.setValue(printCharsetTextPropertyJInit);
        unintern(PRINT__UNREADABLE_CALLBACK_BUFFER);
    }
    private void symsOfEval() {
        var debuggerJInit = DEBUG_EARLY;
        debugger.setValue(debuggerJInit);
        unintern(INTERNAL_INTERPRETER_ENVIRONMENT);
    }
    private void symsOfBuffer() {
        FPut.put(VERTICAL_SCROLL_BAR, CHOICE, ELispCons.listOf(NIL, T, LEFT, RIGHT));
        FPut.put(FRACTION, RANGE, FCons.cons((double) (0.0), (double) (1.0)));
        FPut.put(OVERWRITE_MODE, CHOICE, ELispCons.listOf(NIL, intern("overwrite-mode-textual"), OVERWRITE_MODE_BINARY));
        FPut.put(PROTECTED_FIELD, ERROR_CONDITIONS, ELispCons.listOf(PROTECTED_FIELD, ERROR));
        FPut.put(PROTECTED_FIELD, ERROR_MESSAGE, new ELispString("Attempt to modify a protected field"));
        ENABLE_MULTIBYTE_CHARACTERS.setConstant(true);
        CASE_FOLD_SEARCH.setBufferLocal(true);
        FPut.put(intern("erase-buffer"), DISABLED, T);
    }
    private void symsOfCasefiddle() {
        CASE_SYMBOLS_AS_WORDS.setBufferLocal(true);
    }
    private void symsOfCcl() {
        var codeConversionMapVectorJInit = new ELispVector(16, false);
        codeConversionMapVector.setValue(codeConversionMapVectorJInit);
    }
    private void symsOfCharacter() {
        var translationTableVectorJInit = new ELispVector(16, false);
        translationTableVector.setValue(translationTableVectorJInit);
        var autoFillCharsJInit = FMakeCharTable.makeCharTable(AUTO_FILL_CHARS, NIL);
        autoFillChars.setValue(autoFillCharsJInit);
        autoFillCharsJInit.set(32, T);
        autoFillCharsJInit.set(10, T);
        var charWidthTableJInit = FMakeCharTable.makeCharTable(NIL, (long) (1));
        charWidthTable.setValue(charWidthTableJInit);
        charWidthTableJInit.setRange(128, 159, (long) (4));
        charWidthTableJInit.setRange(4194176, 4194303, (long) (4));
        var ambiguousWidthCharsJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        ambiguousWidthChars.setValue(ambiguousWidthCharsJInit);
        var printableCharsJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        printableChars.setValue(printableCharsJInit);
        FSetCharTableRange.setCharTableRange(printableCharsJInit, FCons.cons((long) (32), (long) (126)), T);
        FSetCharTableRange.setCharTableRange(printableCharsJInit, FCons.cons((long) (160), (long) (MAX_5_BYTE_CHAR)), T);
        FPut.put(CHAR_SCRIPT_TABLE, CHAR_TABLE_EXTRA_SLOTS, (long) (1));
        var charScriptTableJInit = FMakeCharTable.makeCharTable(CHAR_SCRIPT_TABLE, NIL);
        charScriptTable.setValue(charScriptTableJInit);
    }
    private void symsOfEditfns() {
        var cachedSystemName = false;
        var systemNameJInit = cachedSystemName;
        systemName.setValue(systemNameJInit);
        unintern(OUTERMOST_RESTRICTION);
    }
    private void symsOfEmacs() {
        var systemTypeJInit = intern("gnu/linux");
        systemType.setValue(systemTypeJInit);
        var c = File.pathSeparator;
        var pathSeparatorJInit = new ELispString(c);
        pathSeparator.setValue(pathSeparatorJInit);
        FPut.put(DYNAMIC_LIBRARY_ALIST, RISKY_LOCAL_VARIABLE, T);
    }
    private void symsOfMinibuf() {
        MINIBUFFER_DEFAULT.setValue(NIL);
        CUSTOM_VARIABLE_HISTORY.setValue(NIL);
        BUFFER_NAME_HISTORY.setValue(NIL);
        var minibufferPromptPropertiesJInit = ELispCons.listOf(READ_ONLY, T);
        minibufferPromptProperties.setValue(minibufferPromptPropertiesJInit);
    }
    private void symsOfProcess() {
        var interruptProcessFunctionsJInit = ELispCons.listOf(INTERNAL_DEFAULT_INTERRUPT_PROCESS);
        interruptProcessFunctions.setValue(interruptProcessFunctionsJInit);
        var signalProcessFunctionsJInit = ELispCons.listOf(INTERNAL_DEFAULT_SIGNAL_PROCESS);
        signalProcessFunctions.setValue(signalProcessFunctionsJInit);
        var subfeatures = FCons.cons(FCons.cons(CNOWAIT, FCons.cons(T, NIL)), NIL);
        subfeatures = FCons.cons(FCons.cons(CFAMILY, FCons.cons(IPV4, NIL)), subfeatures);
        subfeatures = FCons.cons(FCons.cons(CSERVER, FCons.cons(T, NIL)), subfeatures);
        FProvide.provide(intern("make-network-process"), subfeatures);
    }
    private void symsOfSearch() {
        FPut.put(SEARCH_FAILED, ERROR_CONDITIONS, ELispCons.listOf(SEARCH_FAILED, ERROR));
        FPut.put(SEARCH_FAILED, ERROR_MESSAGE, new ELispString("Search failed"));
        FPut.put(USER_SEARCH_FAILED, ERROR_CONDITIONS, ELispCons.listOf(USER_SEARCH_FAILED, USER_ERROR, SEARCH_FAILED, ERROR));
        FPut.put(USER_SEARCH_FAILED, ERROR_MESSAGE, new ELispString("Search failed"));
        FPut.put(INVALID_REGEXP, ERROR_CONDITIONS, ELispCons.listOf(INVALID_REGEXP, ERROR));
        FPut.put(INVALID_REGEXP, ERROR_MESSAGE, new ELispString("Invalid regexp"));
    }
    private void symsOfFrame() {
        FPut.put(AUTO_RAISE, X_FRAME_PARAMETER, (long) (0));
        FPut.put(AUTO_LOWER, X_FRAME_PARAMETER, (long) (1));
        FPut.put(BACKGROUND_COLOR, X_FRAME_PARAMETER, (long) (2));
        FPut.put(BORDER_COLOR, X_FRAME_PARAMETER, (long) (3));
        FPut.put(BORDER_WIDTH, X_FRAME_PARAMETER, (long) (4));
        FPut.put(CURSOR_COLOR, X_FRAME_PARAMETER, (long) (5));
        FPut.put(CURSOR_TYPE, X_FRAME_PARAMETER, (long) (6));
        FPut.put(FONT, X_FRAME_PARAMETER, (long) (7));
        FPut.put(FOREGROUND_COLOR, X_FRAME_PARAMETER, (long) (8));
        FPut.put(ICON_NAME, X_FRAME_PARAMETER, (long) (9));
        FPut.put(ICON_TYPE, X_FRAME_PARAMETER, (long) (10));
        FPut.put(CHILD_FRAME_BORDER_WIDTH, X_FRAME_PARAMETER, (long) (11));
        FPut.put(INTERNAL_BORDER_WIDTH, X_FRAME_PARAMETER, (long) (12));
        FPut.put(RIGHT_DIVIDER_WIDTH, X_FRAME_PARAMETER, (long) (13));
        FPut.put(BOTTOM_DIVIDER_WIDTH, X_FRAME_PARAMETER, (long) (14));
        FPut.put(MENU_BAR_LINES, X_FRAME_PARAMETER, (long) (15));
        FPut.put(MOUSE_COLOR, X_FRAME_PARAMETER, (long) (16));
        FPut.put(NAME, X_FRAME_PARAMETER, (long) (17));
        FPut.put(SCROLL_BAR_WIDTH, X_FRAME_PARAMETER, (long) (18));
        FPut.put(SCROLL_BAR_HEIGHT, X_FRAME_PARAMETER, (long) (19));
        FPut.put(TITLE, X_FRAME_PARAMETER, (long) (20));
        FPut.put(UNSPLITTABLE, X_FRAME_PARAMETER, (long) (21));
        FPut.put(VERTICAL_SCROLL_BARS, X_FRAME_PARAMETER, (long) (22));
        FPut.put(HORIZONTAL_SCROLL_BARS, X_FRAME_PARAMETER, (long) (23));
        FPut.put(VISIBILITY, X_FRAME_PARAMETER, (long) (24));
        FPut.put(TAB_BAR_LINES, X_FRAME_PARAMETER, (long) (25));
        FPut.put(TOOL_BAR_LINES, X_FRAME_PARAMETER, (long) (26));
        FPut.put(SCROLL_BAR_FOREGROUND, X_FRAME_PARAMETER, (long) (27));
        FPut.put(SCROLL_BAR_BACKGROUND, X_FRAME_PARAMETER, (long) (28));
        FPut.put(SCREEN_GAMMA, X_FRAME_PARAMETER, (long) (29));
        FPut.put(LINE_SPACING, X_FRAME_PARAMETER, (long) (30));
        FPut.put(LEFT_FRINGE, X_FRAME_PARAMETER, (long) (31));
        FPut.put(RIGHT_FRINGE, X_FRAME_PARAMETER, (long) (32));
        FPut.put(WAIT_FOR_WM, X_FRAME_PARAMETER, (long) (33));
        FPut.put(FULLSCREEN, X_FRAME_PARAMETER, (long) (34));
        FPut.put(FONT_BACKEND, X_FRAME_PARAMETER, (long) (35));
        FPut.put(ALPHA, X_FRAME_PARAMETER, (long) (36));
        FPut.put(STICKY, X_FRAME_PARAMETER, (long) (37));
        FPut.put(TOOL_BAR_POSITION, X_FRAME_PARAMETER, (long) (38));
        FPut.put(INHIBIT_DOUBLE_BUFFERING, X_FRAME_PARAMETER, (long) (39));
        FPut.put(UNDECORATED, X_FRAME_PARAMETER, (long) (40));
        FPut.put(PARENT_FRAME, X_FRAME_PARAMETER, (long) (41));
        FPut.put(SKIP_TASKBAR, X_FRAME_PARAMETER, (long) (42));
        FPut.put(NO_FOCUS_ON_MAP, X_FRAME_PARAMETER, (long) (43));
        FPut.put(NO_ACCEPT_FOCUS, X_FRAME_PARAMETER, (long) (44));
        FPut.put(Z_GROUP, X_FRAME_PARAMETER, (long) (45));
        FPut.put(OVERRIDE_REDIRECT, X_FRAME_PARAMETER, (long) (46));
        FPut.put(NO_SPECIAL_GLYPHS, X_FRAME_PARAMETER, (long) (47));
        FPut.put(ALPHA_BACKGROUND, X_FRAME_PARAMETER, (long) (48));
        FPut.put(USE_FRAME_SYNCHRONIZATION, X_FRAME_PARAMETER, (long) (49));
        var iconifyChildFrameJInit = ICONIFY_TOP_LEVEL;
        iconifyChildFrame.setValue(iconifyChildFrameJInit);
        var frameInternalParametersJInit = ELispCons.listOf(NAME, PARENT_ID, WINDOW_ID);
        frameInternalParameters.setValue(frameInternalParametersJInit);
    }
    private void symsOfSyntax() {
        FPut.put(SCAN_ERROR, ERROR_CONDITIONS, ELispCons.listOf(SCAN_ERROR, ERROR));
        FPut.put(SCAN_ERROR, ERROR_MESSAGE, new ELispString("Scan error"));
        SYNTAX_PROPERTIZE__DONE.setBufferLocal(true);
        var findWordBoundaryFunctionTableJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        findWordBoundaryFunctionTable.setValue(findWordBoundaryFunctionTableJInit);
        COMMENT_END_CAN_BE_ESCAPED.setBufferLocal(true);
    }
    private void symsOfTerminal() {
        FProvide.provide(intern("multi-tty"), NIL);
    }
    private void symsOfTreesit() {
        defineError(TREESIT_ERROR, "Generic tree-sitter error", ERROR);
        defineError(TREESIT_QUERY_ERROR, "Query pattern is malformed", TREESIT_ERROR);
        defineError(TREESIT_PARSE_ERROR, "Parse failed", TREESIT_ERROR);
        defineError(TREESIT_RANGE_INVALID, "RANGES are invalid: they have to be ordered and should not overlap", TREESIT_ERROR);
        defineError(TREESIT_BUFFER_TOO_LARGE, "Buffer too large (> 4GiB)", TREESIT_ERROR);
        defineError(TREESIT_LOAD_LANGUAGE_ERROR, "Cannot load language definition", TREESIT_ERROR);
        defineError(TREESIT_NODE_OUTDATED, "This node is outdated, please retrieve a new one", TREESIT_ERROR);
        defineError(TREESIT_NODE_BUFFER_KILLED, "The buffer associated with this node is killed", TREESIT_ERROR);
        defineError(TREESIT_PARSER_DELETED, "This parser is deleted and cannot be used", TREESIT_ERROR);
        defineError(TREESIT_INVALID_PREDICATE, "Invalid predicate, see `treesit-thing-settings' for valid forms for a predicate", TREESIT_ERROR);
        TREESIT_LANGUAGE_REMAP_ALIST.setBufferLocal(true);
        var treesitStrLibtreeSitter = new ELispString("libtree-sitter-");
        var treesitStrTreeSitter = new ELispString("tree-sitter-");
        var treesitStrDot0 = new ELispString(".0");
        var treesitStrDot = new ELispString(".");
        var treesitStrQuestionMark = new ELispString("?");
        var treesitStrStar = new ELispString("*");
        var treesitStrPlus = new ELispString("+");
        var treesitStrPoundEqual = new ELispString("#equal");
        var treesitStrPoundMatch = new ELispString("#match");
        var treesitStrPoundPred = new ELispString("#pred");
        var treesitStrOpenBracket = new ELispString("[");
        var treesitStrCloseBracket = new ELispString("]");
        var treesitStrOpenParen = new ELispString("(");
        var treesitStrCloseParen = new ELispString(")");
        var treesitStrSpace = new ELispString(" ");
        var treesitStrEqual = new ELispString("equal");
        var treesitStrMatch = new ELispString("match");
        var treesitStrPred = new ELispString("pred");
    }
    private void symsOfTextprop() {
        var textPropertyDefaultNonstickyJInit = ELispCons.listOf(FCons.cons(SYNTAX_TABLE, T), FCons.cons(DISPLAY, T));
        textPropertyDefaultNonsticky.setValue(textPropertyDefaultNonstickyJInit);
    }
    private void symsOfComposite() {
        var gstringWorkHeaders = new ELispVector(8, false);
        gstringWorkHeaders.set(0, new ELispVector(2, false));
        gstringWorkHeaders.set(1, new ELispVector(3, false));
        gstringWorkHeaders.set(2, new ELispVector(4, false));
        gstringWorkHeaders.set(3, new ELispVector(5, false));
        gstringWorkHeaders.set(4, new ELispVector(6, false));
        gstringWorkHeaders.set(5, new ELispVector(7, false));
        gstringWorkHeaders.set(6, new ELispVector(8, false));
        gstringWorkHeaders.set(7, new ELispVector(9, false));
        var textPropertyDefaultNonstickyJInit = FCons.cons(FCons.cons(COMPOSITION, T), textPropertyDefaultNonsticky.getValue());
        textPropertyDefaultNonsticky.setValue(textPropertyDefaultNonstickyJInit);
        var composeCharsAfterFunctionJInit = intern("compose-chars-after");
        composeCharsAfterFunction.setValue(composeCharsAfterFunctionJInit);
        var compositionFunctionTableJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        compositionFunctionTable.setValue(compositionFunctionTableJInit);
    }
    private void symsOfWindow() {
        FPut.put(SCROLL_UP, SCROLL_COMMAND, T);
        FPut.put(SCROLL_DOWN, SCROLL_COMMAND, T);
        var recenterRedisplayJInit = TTY;
        recenterRedisplay.setValue(recenterRedisplayJInit);
        var windowCombinationLimitJInit = WINDOW_SIZE;
        windowCombinationLimit.setValue(windowCombinationLimitJInit);
        var windowPersistentParametersJInit = ELispCons.listOf(FCons.cons(CLONE_OF, T));
        windowPersistentParameters.setValue(windowPersistentParametersJInit);
        var windowDeadWindowsTableJInit = FMakeHashTable.makeHashTable(new Object[]{CWEAKNESS, T});
        windowDeadWindowsTable.setValue(windowDeadWindowsTableJInit);
    }
    private void symsOfXdisp() {
        var voidTextAreaPointerJInit = ARROW;
        voidTextAreaPointer.setValue(voidTextAreaPointerJInit);
        var overlayArrowVariableListJInit = ELispCons.listOf(OVERLAY_ARROW_POSITION);
        overlayArrowVariableList.setValue(overlayArrowVariableListJInit);
        var iconTitleNameFormat = ELispCons.listOf(emptyUnibyteString, new ELispString("%b - GNU Emacs at "), SYSTEM_NAME);
        var frameTitleFormatJInit = ELispCons.listOf(MULTIPLE_FRAMES, new ELispString("%b"), iconTitleNameFormat);
        frameTitleFormat.setValue(frameTitleFormatJInit);
        var iconTitleFormatJInit = frameTitleFormatJInit;
        iconTitleFormat.setValue(iconTitleFormatJInit);
        MAKE_WINDOW_START_VISIBLE.setBufferLocal(true);
        var tabBarBorderJInit = INTERNAL_BORDER_WIDTH;
        tabBarBorder.setValue(tabBarBorderJInit);
        var toolBarBorderJInit = INTERNAL_BORDER_WIDTH;
        toolBarBorder.setValue(toolBarBorderJInit);
        FONTIFICATION_FUNCTIONS.setBufferLocal(true);
        WRAP_PREFIX.setBufferLocal(true);
        LINE_PREFIX.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS_WIDTH.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS_WIDEN.setBufferLocal(true);
        DISPLAY_LINE_NUMBERS_OFFSET.setBufferLocal(true);
        DISPLAY_FILL_COLUMN_INDICATOR.setBufferLocal(true);
        DISPLAY_FILL_COLUMN_INDICATOR_COLUMN.setBufferLocal(true);
        DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER.setBufferLocal(true);
        var preRedisplayFunctionJInit = intern("ignore");
        preRedisplayFunction.setValue(preRedisplayFunctionJInit);
        FPut.put(GLYPHLESS_CHAR_DISPLAY, CHAR_TABLE_EXTRA_SLOTS, (long) (1));
        var glyphlessCharDisplayJInit = FMakeCharTable.makeCharTable(GLYPHLESS_CHAR_DISPLAY, NIL);
        glyphlessCharDisplay.setValue(glyphlessCharDisplayJInit);
        FSetCharTableExtraSlot.setCharTableExtraSlot(glyphlessCharDisplayJInit, (long) (0), EMPTY_BOX);
        var redisplayAllWindowsCauseJInit = FMakeHashTable.makeHashTable(new Object[]{});
        redisplayAllWindowsCause.setValue(redisplayAllWindowsCauseJInit);
        var redisplayModeLinesCauseJInit = FMakeHashTable.makeHashTable(new Object[]{});
        redisplayModeLinesCause.setValue(redisplayModeLinesCauseJInit);
    }
    private void initKeyboard() {
        ELispKboard.initKboardLocalVars(ctx);
    }
    //#endregion initializations
    //#region globals.h
    public static final ELispSymbol NIL = new ELispSymbol("nil");
    public static final ELispSymbol T = new ELispSymbol("t");
    public static final ELispSymbol UNBOUND = new ELispSymbol("unbound");
    public static final ELispSymbol ERROR = new ELispSymbol("error");
    public static final ELispSymbol LAMBDA = new ELispSymbol("lambda");
    public static final ELispSymbol AEAD_CIPHERS = new ELispSymbol("AEAD-ciphers");
    public static final ELispSymbol AUTOMATIC_GC = new ELispSymbol("Automatic GC");
    public static final ELispSymbol CADSTYLE = new ELispSymbol(":adstyle");
    public static final ELispSymbol CADVERTISED_BINDING = new ELispSymbol(":advertised-binding");
    public static final ELispSymbol CALIGN_TO = new ELispSymbol(":align-to");
    public static final ELispSymbol CANCHOR = new ELispSymbol(":anchor");
    public static final ELispSymbol CANTIALIAS = new ELispSymbol(":antialias");
    public static final ELispSymbol CARRAY = new ELispSymbol(":array");
    public static final ELispSymbol CARRAY_TYPE = new ELispSymbol(":array-type");
    public static final ELispSymbol CASCII_COMPATIBLE_P = new ELispSymbol(":ascii-compatible-p");
    public static final ELispSymbol CAUTHORIZABLE = new ELispSymbol(":authorizable");
    public static final ELispSymbol CAVGWIDTH = new ELispSymbol(":avgwidth");
    public static final ELispSymbol CBACKGROUND = new ELispSymbol(":background");
    public static final ELispSymbol CBOLD = new ELispSymbol(":bold");
    public static final ELispSymbol CBOOLEAN = new ELispSymbol(":boolean");
    public static final ELispSymbol CBOX = new ELispSymbol(":box");
    public static final ELispSymbol CBUFFER = new ELispSymbol(":buffer");
    public static final ELispSymbol CBUTTON = new ELispSymbol(":button");
    public static final ELispSymbol CBYTE = new ELispSymbol(":byte");
    public static final ELispSymbol CBYTESIZE = new ELispSymbol(":bytesize");
    public static final ELispSymbol CCATEGORY = new ELispSymbol(":category");
    public static final ELispSymbol CCERTIFICATE = new ELispSymbol(":certificate");
    public static final ELispSymbol CCERTIFICATE_ID = new ELispSymbol(":certificate-id");
    public static final ELispSymbol CCERTIFICATE_SECURITY_LEVEL = new ELispSymbol(":certificate-security-level");
    public static final ELispSymbol CCERTIFICATES = new ELispSymbol(":certificates");
    public static final ELispSymbol CCIPHER = new ELispSymbol(":cipher");
    public static final ELispSymbol CCIPHER_AEAD_CAPABLE = new ELispSymbol(":cipher-aead-capable");
    public static final ELispSymbol CCIPHER_BLOCKSIZE = new ELispSymbol(":cipher-blocksize");
    public static final ELispSymbol CCIPHER_ID = new ELispSymbol(":cipher-id");
    public static final ELispSymbol CCIPHER_IVSIZE = new ELispSymbol(":cipher-ivsize");
    public static final ELispSymbol CCIPHER_KEYSIZE = new ELispSymbol(":cipher-keysize");
    public static final ELispSymbol CCIPHER_TAGSIZE = new ELispSymbol(":cipher-tagsize");
    public static final ELispSymbol CCODING = new ELispSymbol(":coding");
    public static final ELispSymbol CCOLOR = new ELispSymbol(":color");
    public static final ELispSymbol CCOMBINING_CAPABILITY = new ELispSymbol(":combining-capability");
    public static final ELispSymbol CCOMMAND = new ELispSymbol(":command");
    public static final ELispSymbol CCOMPLETE_NEGOTIATION = new ELispSymbol(":complete-negotiation");
    public static final ELispSymbol CCOMPRESSION = new ELispSymbol(":compression");
    public static final ELispSymbol CCONNECTION_TYPE = new ELispSymbol(":connection-type");
    public static final ELispSymbol CCRLFILES = new ELispSymbol(":crlfiles");
    public static final ELispSymbol CDATA = new ELispSymbol(":data");
    public static final ELispSymbol CDEBUG_ON_EXIT = new ELispSymbol(":debug-on-exit");
    public static final ELispSymbol CDECODE_TRANSLATION_TABLE = new ELispSymbol(":decode-translation-table");
    public static final ELispSymbol CDEFAULT_CHAR = new ELispSymbol(":default-char");
    public static final ELispSymbol CDEVICE = new ELispSymbol(":device");
    public static final ELispSymbol CDICT_ENTRY = new ELispSymbol(":dict-entry");
    public static final ELispSymbol CDIFFIE_HELLMAN_PRIME_BITS = new ELispSymbol(":diffie-hellman-prime-bits");
    public static final ELispSymbol CDIGEST_ALGORITHM_ID = new ELispSymbol(":digest-algorithm-id");
    public static final ELispSymbol CDIGEST_ALGORITHM_LENGTH = new ELispSymbol(":digest-algorithm-length");
    public static final ELispSymbol CDISTANT_FOREGROUND = new ELispSymbol(":distant-foreground");
    public static final ELispSymbol CDOCUMENTATION = new ELispSymbol(":documentation");
    public static final ELispSymbol CDOUBLE = new ELispSymbol(":double");
    public static final ELispSymbol CDPI = new ELispSymbol(":dpi");
    public static final ELispSymbol CEMERGENCY = new ELispSymbol(":emergency");
    public static final ELispSymbol CENABLE = new ELispSymbol(":enable");
    public static final ELispSymbol CENCODE_TRANSLATION_TABLE = new ELispSymbol(":encode-translation-table");
    public static final ELispSymbol CENCRYPT_THEN_MAC = new ELispSymbol(":encrypt-then-mac");
    public static final ELispSymbol CEQUAL = new ELispSymbol(":equal");
    public static final ELispSymbol CERROR = new ELispSymbol(":error");
    public static final ELispSymbol CEVAL = new ELispSymbol(":eval");
    public static final ELispSymbol CEXPIRED = new ELispSymbol(":expired");
    public static final ELispSymbol CEXTEND = new ELispSymbol(":extend");
    public static final ELispSymbol CFALSE = new ELispSymbol(":false");
    public static final ELispSymbol CFALSE_OBJECT = new ELispSymbol(":false-object");
    public static final ELispSymbol CFAMILY = new ELispSymbol(":family");
    public static final ELispSymbol CFILE = new ELispSymbol(":file");
    public static final ELispSymbol CFILE_HANDLER = new ELispSymbol(":file-handler");
    public static final ELispSymbol CFILTER = new ELispSymbol(":filter");
    public static final ELispSymbol CFILTERED = new ELispSymbol(":filtered");
    public static final ELispSymbol CFLAGS = new ELispSymbol(":flags");
    public static final ELispSymbol CFLOWCONTROL = new ELispSymbol(":flowcontrol");
    public static final ELispSymbol CFONT = new ELispSymbol(":font");
    public static final ELispSymbol CFONT_ENTITY = new ELispSymbol(":font-entity");
    public static final ELispSymbol CFONTSET = new ELispSymbol(":fontset");
    public static final ELispSymbol CFOREGROUND = new ELispSymbol(":foreground");
    public static final ELispSymbol CFOUNDRY = new ELispSymbol(":foundry");
    public static final ELispSymbol CHEIGHT = new ELispSymbol(":height");
    public static final ELispSymbol CHELP = new ELispSymbol(":help");
    public static final ELispSymbol CHOST = new ELispSymbol(":host");
    public static final ELispSymbol CHOSTNAME = new ELispSymbol(":hostname");
    public static final ELispSymbol CIGNORE_DEFFACE = new ELispSymbol(":ignore-defface");
    public static final ELispSymbol CIMAGE = new ELispSymbol(":image");
    public static final ELispSymbol CIN_PLACE = new ELispSymbol(":in-place");
    public static final ELispSymbol CINHERIT = new ELispSymbol(":inherit");
    public static final ELispSymbol CINSECURE = new ELispSymbol(":insecure");
    public static final ELispSymbol CINT16 = new ELispSymbol(":int16");
    public static final ELispSymbol CINT32 = new ELispSymbol(":int32");
    public static final ELispSymbol CINT64 = new ELispSymbol(":int64");
    public static final ELispSymbol CINVALID = new ELispSymbol(":invalid");
    public static final ELispSymbol CINVALID_OCSP_STATUS = new ELispSymbol(":invalid-ocsp-status");
    public static final ELispSymbol CINVERSE_VIDEO = new ELispSymbol(":inverse-video");
    public static final ELispSymbol CISSUER = new ELispSymbol(":issuer");
    public static final ELispSymbol CISSUER_UNIQUE_ID = new ELispSymbol(":issuer-unique-id");
    public static final ELispSymbol CITALIC = new ELispSymbol(":italic");
    public static final ELispSymbol CKEY = new ELispSymbol(":key");
    public static final ELispSymbol CKEY_EXCHANGE = new ELispSymbol(":key-exchange");
    public static final ELispSymbol CKEY_SEQUENCE = new ELispSymbol(":key-sequence");
    public static final ELispSymbol CKEYLIST = new ELispSymbol(":keylist");
    public static final ELispSymbol CKEYS = new ELispSymbol(":keys");
    public static final ELispSymbol CLABEL = new ELispSymbol(":label");
    public static final ELispSymbol CLANG = new ELispSymbol(":lang");
    public static final ELispSymbol CLESSP = new ELispSymbol(":lessp");
    public static final ELispSymbol CLIENTHELLO_PADDING = new ELispSymbol("ClientHello Padding");
    public static final ELispSymbol CLINE_WIDTH = new ELispSymbol(":line-width");
    public static final ELispSymbol CLOCAL = new ELispSymbol(":local");
    public static final ELispSymbol CLOG = new ELispSymbol(":log");
    public static final ELispSymbol CLOGLEVEL = new ELispSymbol(":loglevel");
    public static final ELispSymbol CMAC = new ELispSymbol(":mac");
    public static final ELispSymbol CMAC_ALGORITHM_ID = new ELispSymbol(":mac-algorithm-id");
    public static final ELispSymbol CMAC_ALGORITHM_KEYSIZE = new ELispSymbol(":mac-algorithm-keysize");
    public static final ELispSymbol CMAC_ALGORITHM_LENGTH = new ELispSymbol(":mac-algorithm-length");
    public static final ELispSymbol CMAC_ALGORITHM_NONCESIZE = new ELispSymbol(":mac-algorithm-noncesize");
    public static final ELispSymbol CMAP = new ELispSymbol(":map");
    public static final ELispSymbol CMATCH = new ELispSymbol(":match");
    public static final ELispSymbol CMETHOD = new ELispSymbol(":method");
    public static final ELispSymbol CMIN_PRIME_BITS = new ELispSymbol(":min-prime-bits");
    public static final ELispSymbol CMISSING_OCSP_STATUS = new ELispSymbol(":missing-ocsp-status");
    public static final ELispSymbol CMNEMONIC = new ELispSymbol(":mnemonic");
    public static final ELispSymbol CMONITOR = new ELispSymbol(":monitor");
    public static final ELispSymbol CNAME = new ELispSymbol(":name");
    public static final ELispSymbol CNO_HOST_MATCH = new ELispSymbol(":no-host-match");
    public static final ELispSymbol CNOQUERY = new ELispSymbol(":noquery");
    public static final ELispSymbol CNOT_ACTIVATED = new ELispSymbol(":not-activated");
    public static final ELispSymbol CNOT_CA = new ELispSymbol(":not-ca");
    public static final ELispSymbol CNOWAIT = new ELispSymbol(":nowait");
    public static final ELispSymbol CNULL = new ELispSymbol(":null");
    public static final ELispSymbol CNULL_OBJECT = new ELispSymbol(":null-object");
    public static final ELispSymbol COBJECT_PATH = new ELispSymbol(":object-path");
    public static final ELispSymbol COBJECT_TYPE = new ELispSymbol(":object-type");
    public static final ELispSymbol COTF = new ELispSymbol(":otf");
    public static final ELispSymbol COVERLINE = new ELispSymbol(":overline");
    public static final ELispSymbol CPARITY = new ELispSymbol(":parity");
    public static final ELispSymbol CPASS = new ELispSymbol(":pass");
    public static final ELispSymbol CPEM = new ELispSymbol(":pem");
    public static final ELispSymbol CPLIST = new ELispSymbol(":plist");
    public static final ELispSymbol CPLUS = new ELispSymbol(":+");
    public static final ELispSymbol CPOINTER = new ELispSymbol(":pointer");
    public static final ELispSymbol CPORT = new ELispSymbol(":port");
    public static final ELispSymbol CPOSITION = new ELispSymbol(":position");
    public static final ELispSymbol CPOST_READ_CONVERSION = new ELispSymbol(":post-read-conversion");
    public static final ELispSymbol CPRE_WRITE_CONVERSION = new ELispSymbol(":pre-write-conversion");
    public static final ELispSymbol CPRED = new ELispSymbol(":pred");
    public static final ELispSymbol CPRIORITY = new ELispSymbol(":priority");
    public static final ELispSymbol CPROCESS = new ELispSymbol(":process");
    public static final ELispSymbol CPROPERTIZE = new ELispSymbol(":propertize");
    public static final ELispSymbol CPROTOCOL = new ELispSymbol(":protocol");
    public static final ELispSymbol CPUBLIC_KEY_ALGORITHM = new ELispSymbol(":public-key-algorithm");
    public static final ELispSymbol CPUBLIC_KEY_ID = new ELispSymbol(":public-key-id");
    public static final ELispSymbol CPUBLIC_KEY_ID_SHA256 = new ELispSymbol(":public-key-id-sha256");
    public static final ELispSymbol CPURECOPY = new ELispSymbol(":purecopy");
    public static final ELispSymbol CPURPOSE_MISMATCH = new ELispSymbol(":purpose-mismatch");
    public static final ELispSymbol CQUESTION = new ELispSymbol(":?");
    public static final ELispSymbol CRADIO = new ELispSymbol(":radio");
    public static final ELispSymbol CREGISTRY = new ELispSymbol(":registry");
    public static final ELispSymbol CREHASH_SIZE = new ELispSymbol(":rehash-size");
    public static final ELispSymbol CREHASH_THRESHOLD = new ELispSymbol(":rehash-threshold");
    public static final ELispSymbol CRELATIVE_HEIGHT = new ELispSymbol(":relative-height");
    public static final ELispSymbol CRELATIVE_WIDTH = new ELispSymbol(":relative-width");
    public static final ELispSymbol CREMOTE = new ELispSymbol(":remote");
    public static final ELispSymbol CREVERSE = new ELispSymbol(":reverse");
    public static final ELispSymbol CREVOCATION_DATA_ISSUED_IN_FUTURE = new ELispSymbol(":revocation-data-issued-in-future");
    public static final ELispSymbol CREVOCATION_DATA_SUPERSEDED = new ELispSymbol(":revocation-data-superseded");
    public static final ELispSymbol CREVOKED = new ELispSymbol(":revoked");
    public static final ELispSymbol CRTL = new ELispSymbol(":rtl");
    public static final ELispSymbol CSAFE_RENEGOTIATION = new ELispSymbol(":safe-renegotiation");
    public static final ELispSymbol CSCALABLE = new ELispSymbol(":scalable");
    public static final ELispSymbol CSCRIPT = new ELispSymbol(":script");
    public static final ELispSymbol CSELF_SIGNED = new ELispSymbol(":self-signed");
    public static final ELispSymbol CSENTINEL = new ELispSymbol(":sentinel");
    public static final ELispSymbol CSERIAL = new ELispSymbol(":serial");
    public static final ELispSymbol CSERIAL_NUMBER = new ELispSymbol(":serial-number");
    public static final ELispSymbol CSERVER = new ELispSymbol(":server");
    public static final ELispSymbol CSERVICE = new ELispSymbol(":service");
    public static final ELispSymbol CSESSION = new ELispSymbol(":session");
    public static final ELispSymbol CSESSION_PRIVATE = new ELispSymbol(":session-private");
    public static final ELispSymbol CSIGNAL = new ELispSymbol(":signal");
    public static final ELispSymbol CSIGNATURE = new ELispSymbol(":signature");
    public static final ELispSymbol CSIGNATURE_ALGORITHM = new ELispSymbol(":signature-algorithm");
    public static final ELispSymbol CSIGNATURE_FAILURE = new ELispSymbol(":signature-failure");
    public static final ELispSymbol CSIGNER_CONSTRAINTS_FAILURE = new ELispSymbol(":signer-constraints-failure");
    public static final ELispSymbol CSIZE = new ELispSymbol(":size");
    public static final ELispSymbol CSLANT = new ELispSymbol(":slant");
    public static final ELispSymbol CSPACING = new ELispSymbol(":spacing");
    public static final ELispSymbol CSPEED = new ELispSymbol(":speed");
    public static final ELispSymbol CSTAR = new ELispSymbol(":*");
    public static final ELispSymbol CSTDERR = new ELispSymbol(":stderr");
    public static final ELispSymbol CSTIPPLE = new ELispSymbol(":stipple");
    public static final ELispSymbol CSTOP = new ELispSymbol(":stop");
    public static final ELispSymbol CSTOPBITS = new ELispSymbol(":stopbits");
    public static final ELispSymbol CSTRIKE_THROUGH = new ELispSymbol(":strike-through");
    public static final ELispSymbol CSTRING = new ELispSymbol(":string");
    public static final ELispSymbol CSTRUCT = new ELispSymbol(":struct");
    public static final ELispSymbol CSTYLE = new ELispSymbol(":style");
    public static final ELispSymbol CSUBJECT = new ELispSymbol(":subject");
    public static final ELispSymbol CSUBJECT_UNIQUE_ID = new ELispSymbol(":subject-unique-id");
    public static final ELispSymbol CSUCCESS = new ELispSymbol(":success");
    public static final ELispSymbol CSUMMARY = new ELispSymbol(":summary");
    public static final ELispSymbol CSYSTEM = new ELispSymbol(":system");
    public static final ELispSymbol CSYSTEM_PRIVATE = new ELispSymbol(":system-private");
    public static final ELispSymbol CTEST = new ELispSymbol(":test");
    public static final ELispSymbol CTIMEOUT = new ELispSymbol(":timeout");
    public static final ELispSymbol CTLS_PARAMETERS = new ELispSymbol(":tls-parameters");
    public static final ELispSymbol CTOGGLE = new ELispSymbol(":toggle");
    public static final ELispSymbol CTRUSTFILES = new ELispSymbol(":trustfiles");
    public static final ELispSymbol CTYPE = new ELispSymbol(":type");
    public static final ELispSymbol CUINT16 = new ELispSymbol(":uint16");
    public static final ELispSymbol CUINT32 = new ELispSymbol(":uint32");
    public static final ELispSymbol CUINT64 = new ELispSymbol(":uint64");
    public static final ELispSymbol CUNDERLINE = new ELispSymbol(":underline");
    public static final ELispSymbol CUNIX_FD = new ELispSymbol(":unix-fd");
    public static final ELispSymbol CUNKNOWN_CA = new ELispSymbol(":unknown-ca");
    public static final ELispSymbol CUSE_EXTERNAL_SOCKET = new ELispSymbol(":use-external-socket");
    public static final ELispSymbol CUSER_SPEC = new ELispSymbol(":user-spec");
    public static final ELispSymbol CVALID_FROM = new ELispSymbol(":valid-from");
    public static final ELispSymbol CVALID_TO = new ELispSymbol(":valid-to");
    public static final ELispSymbol CVARIANT = new ELispSymbol(":variant");
    public static final ELispSymbol CVERIFY_ERROR = new ELispSymbol(":verify-error");
    public static final ELispSymbol CVERIFY_FLAGS = new ELispSymbol(":verify-flags");
    public static final ELispSymbol CVERSION = new ELispSymbol(":version");
    public static final ELispSymbol CVERT_ONLY = new ELispSymbol(":vert-only");
    public static final ELispSymbol CVISIBLE = new ELispSymbol(":visible");
    public static final ELispSymbol CVOLUME = new ELispSymbol(":volume");
    public static final ELispSymbol CWARNINGS = new ELispSymbol(":warnings");
    public static final ELispSymbol CWEAKNESS = new ELispSymbol(":weakness");
    public static final ELispSymbol CWEIGHT = new ELispSymbol(":weight");
    public static final ELispSymbol CWIDTH = new ELispSymbol(":width");
    public static final ELispSymbol CWINDOW = new ELispSymbol(":window");
    public static final ELispSymbol CWRAP = new ELispSymbol(":wrap");
    public static final ELispSymbol D_BUS = new ELispSymbol("D-Bus");
    public static final ELispSymbol DISCARDED_SAMPLES = new ELispSymbol("Discarded Samples");
    public static final ELispSymbol L2R = new ELispSymbol("L2R");
    public static final ELispSymbol PRIMARY = new ELispSymbol("PRIMARY");
    public static final ELispSymbol R2L = new ELispSymbol("R2L");
    public static final ELispSymbol UNKNOWN_ERROR = new ELispSymbol("Unknown error");
    public static final ELispSymbol ABOVE = new ELispSymbol("above");
    public static final ELispSymbol ABOVE_HANDLE = new ELispSymbol("above-handle");
    public static final ELispSymbol ABOVE_SUSPENDED = new ELispSymbol("above-suspended");
    public static final ELispSymbol ACCESS = new ELispSymbol("access");
    public static final ELispSymbol ACCESS_FILE = new ELispSymbol("access-file");
    public static final ELispSymbol ACTIVATE_INPUT_METHOD = new ELispSymbol("activate-input-method");
    public static final ELispSymbol ACTIVATE_MARK_HOOK = new ELispSymbol("activate-mark-hook");
    public static final ELispSymbol ACTIVATE_MENUBAR_HOOK = new ELispSymbol("activate-menubar-hook");
    public static final ELispSymbol ADD1 = new ELispSymbol("1+");
    public static final ELispSymbol ADD_NAME_TO_FILE = new ELispSymbol("add-name-to-file");
    public static final ELispSymbol ADD_TO_HISTORY = new ELispSymbol("add-to-history");
    public static final ELispSymbol AFTER_CHANGE_FUNCTIONS = new ELispSymbol("after-change-functions");
    public static final ELispSymbol AFTER_DELETE_FRAME_FUNCTIONS = new ELispSymbol("after-delete-frame-functions");
    public static final ELispSymbol AFTER_HANDLE = new ELispSymbol("after-handle");
    public static final ELispSymbol AFTER_INSERT_FILE_SET_BUFFER_FILE_CODING_SYSTEM = new ELispSymbol("after-insert-file-set-buffer-file-coding-system");
    public static final ELispSymbol AFTER_INSERT_FILE_SET_CODING = new ELispSymbol("after-insert-file-set-coding");
    public static final ELispSymbol AFTER_PDUMP_LOAD_HOOK = new ELispSymbol("after-pdump-load-hook");
    public static final ELispSymbol AFTER_STRING = new ELispSymbol("after-string");
    public static final ELispSymbol ALIST = new ELispSymbol("alist");
    public static final ELispSymbol ALL = new ELispSymbol("all");
    public static final ELispSymbol ALL_EVENTS = new ELispSymbol("all-events");
    public static final ELispSymbol ALLOC = new ELispSymbol("alloc");
    public static final ELispSymbol ALPHA = new ELispSymbol("alpha");
    public static final ELispSymbol ALPHA_BACKGROUND = new ELispSymbol("alpha-background");
    public static final ELispSymbol AND_OPTIONAL = new ELispSymbol("&optional");
    public static final ELispSymbol AND_REST = new ELispSymbol("&rest");
    public static final ELispSymbol ANDROID = new ELispSymbol("android");
    public static final ELispSymbol APPLY = new ELispSymbol("apply");
    public static final ELispSymbol AREF = new ELispSymbol("aref");
    public static final ELispSymbol ARGS = new ELispSymbol("args");
    public static final ELispSymbol ARGS_OUT_OF_RANGE = new ELispSymbol("args-out-of-range");
    public static final ELispSymbol ARITH_ERROR = new ELispSymbol("arith-error");
    public static final ELispSymbol ARRAY = new ELispSymbol("array");
    public static final ELispSymbol ARRAYP = new ELispSymbol("arrayp");
    public static final ELispSymbol ARROW = new ELispSymbol("arrow");
    public static final ELispSymbol ASCII = new ELispSymbol("ascii");
    public static final ELispSymbol ASCII_0 = new ELispSymbol("ascii-0");
    public static final ELispSymbol ASCII_CHARACTER = new ELispSymbol("ascii-character");
    public static final ELispSymbol ASET = new ELispSymbol("aset");
    public static final ELispSymbol ASSUME = new ELispSymbol("assume");
    public static final ELispSymbol ATTRIB = new ELispSymbol("attrib");
    public static final ELispSymbol AUTO_COMPOSED = new ELispSymbol("auto-composed");
    public static final ELispSymbol AUTO_FILL_CHARS = new ELispSymbol("auto-fill-chars");
    public static final ELispSymbol AUTO_HSCROLL_MODE = new ELispSymbol("auto-hscroll-mode");
    public static final ELispSymbol AUTO_LOWER = new ELispSymbol("auto-lower");
    public static final ELispSymbol AUTO_RAISE = new ELispSymbol("auto-raise");
    public static final ELispSymbol AUTO_SAVE = new ELispSymbol("auto-save");
    public static final ELispSymbol AUTO_SAVE_CODING = new ELispSymbol("auto-save-coding");
    public static final ELispSymbol AUTO_SAVE_HOOK = new ELispSymbol("auto-save-hook");
    public static final ELispSymbol AUTOLOAD = new ELispSymbol("autoload");
    public static final ELispSymbol AUTOSAVED = new ELispSymbol("autosaved");
    public static final ELispSymbol BACKGROUND_COLOR = new ELispSymbol("background-color");
    public static final ELispSymbol BACKGROUND_MODE = new ELispSymbol("background-mode");
    public static final ELispSymbol BACKQUOTE = new ELispSymbol("`");
    public static final ELispSymbol BAR = new ELispSymbol("bar");
    public static final ELispSymbol BARE_SYMBOL_P = new ELispSymbol("bare-symbol-p");
    public static final ELispSymbol BEFORE_CHANGE_FUNCTIONS = new ELispSymbol("before-change-functions");
    public static final ELispSymbol BEFORE_HANDLE = new ELispSymbol("before-handle");
    public static final ELispSymbol BEFORE_STRING = new ELispSymbol("before-string");
    public static final ELispSymbol BEGINNING_OF_BUFFER = new ELispSymbol("beginning-of-buffer");
    public static final ELispSymbol BELOW = new ELispSymbol("below");
    public static final ELispSymbol BELOW_HANDLE = new ELispSymbol("below-handle");
    public static final ELispSymbol BIG = new ELispSymbol("big");
    public static final ELispSymbol BIG5 = new ELispSymbol("big5");
    public static final ELispSymbol BIGNUM = new ELispSymbol("bignum");
    public static final ELispSymbol BINARY = new ELispSymbol("binary");
    public static final ELispSymbol BITMAP_SPEC_P = new ELispSymbol("bitmap-spec-p");
    public static final ELispSymbol BLACK = new ELispSymbol("black");
    public static final ELispSymbol BOLD = new ELispSymbol("bold");
    public static final ELispSymbol BOOK = new ELispSymbol("book");
    public static final ELispSymbol BOOL_VECTOR = new ELispSymbol("bool-vector");
    public static final ELispSymbol BOOL_VECTOR_P = new ELispSymbol("bool-vector-p");
    public static final ELispSymbol BOOLEAN = new ELispSymbol("boolean");
    public static final ELispSymbol BOOLEANP = new ELispSymbol("booleanp");
    public static final ELispSymbol BORDER = new ELispSymbol("border");
    public static final ELispSymbol BORDER_COLOR = new ELispSymbol("border-color");
    public static final ELispSymbol BORDER_WIDTH = new ELispSymbol("border-width");
    public static final ELispSymbol BOTH = new ELispSymbol("both");
    public static final ELispSymbol BOTH_HORIZ = new ELispSymbol("both-horiz");
    public static final ELispSymbol BOTTOM = new ELispSymbol("bottom");
    public static final ELispSymbol BOTTOM_DIVIDER = new ELispSymbol("bottom-divider");
    public static final ELispSymbol BOTTOM_DIVIDER_WIDTH = new ELispSymbol("bottom-divider-width");
    public static final ELispSymbol BOTTOM_EDGE = new ELispSymbol("bottom-edge");
    public static final ELispSymbol BOTTOM_LEFT_CORNER = new ELispSymbol("bottom-left-corner");
    public static final ELispSymbol BOTTOM_RIGHT_CORNER = new ELispSymbol("bottom-right-corner");
    public static final ELispSymbol BOUNDARY = new ELispSymbol("boundary");
    public static final ELispSymbol BOUNDS = new ELispSymbol("bounds");
    public static final ELispSymbol BOX = new ELispSymbol("box");
    public static final ELispSymbol BUFFER = new ELispSymbol("buffer");
    public static final ELispSymbol BUFFER_ACCESS_FONTIFY_FUNCTIONS = new ELispSymbol("buffer-access-fontify-functions");
    public static final ELispSymbol BUFFER_FILE_CODING_SYSTEM = new ELispSymbol("buffer-file-coding-system");
    public static final ELispSymbol BUFFER_FILE_NAME = new ELispSymbol("buffer-file-name");
    public static final ELispSymbol BUFFER_FILE_NUMBER = new ELispSymbol("buffer-file-number");
    public static final ELispSymbol BUFFER_LIST = new ELispSymbol("buffer-list");
    public static final ELispSymbol BUFFER_LIST_UPDATE_HOOK = new ELispSymbol("buffer-list-update-hook");
    public static final ELispSymbol BUFFER_NAME_HISTORY = new ELispSymbol("buffer-name-history");
    public static final ELispSymbol BUFFER_OR_STRING_P = new ELispSymbol("buffer-or-string-p");
    public static final ELispSymbol BUFFER_POSITION = new ELispSymbol("buffer-position");
    public static final ELispSymbol BUFFER_PREDICATE = new ELispSymbol("buffer-predicate");
    public static final ELispSymbol BUFFER_READ_ONLY = new ELispSymbol("buffer-read-only");
    public static final ELispSymbol BUFFER_SAVE_WITHOUT_QUERY = new ELispSymbol("buffer-save-without-query");
    public static final ELispSymbol BUFFER_STALE_FUNCTION = new ELispSymbol("buffer-stale-function");
    public static final ELispSymbol BUFFER_UNDO_LIST = new ELispSymbol("buffer-undo-list");
    public static final ELispSymbol BUFFERP = new ELispSymbol("bufferp");
    public static final ELispSymbol BUFFERS = new ELispSymbol("buffers");
    public static final ELispSymbol BURIED_BUFFER_LIST = new ELispSymbol("buried-buffer-list");
    public static final ELispSymbol BYTE_CODE_FUNCTION = new ELispSymbol("byte-code-function");
    public static final ELispSymbol BYTE_CODE_FUNCTION_P = new ELispSymbol("byte-code-function-p");
    public static final ELispSymbol BYTE_CODE_METER = new ELispSymbol("byte-code-meter");
    public static final ELispSymbol BYTE_RUN_UNESCAPED_CHARACTER_LITERALS_WARNING = new ELispSymbol("byte-run--unescaped-character-literals-warning");
    public static final ELispSymbol C = new ELispSymbol("c");
    public static final ELispSymbol CALL = new ELispSymbol("call");
    public static final ELispSymbol CALL_PROCESS = new ELispSymbol("call-process");
    public static final ELispSymbol CALL_PROCESS_REGION = new ELispSymbol("call-process-region");
    public static final ELispSymbol CALLREF = new ELispSymbol("callref");
    public static final ELispSymbol CANONICAL_COMBINING_CLASS = new ELispSymbol("canonical-combining-class");
    public static final ELispSymbol CAR = new ELispSymbol("car");
    public static final ELispSymbol CAR_LESS_THAN_CAR = new ELispSymbol("car-less-than-car");
    public static final ELispSymbol CASE_FOLD_SEARCH = new ELispSymbol("case-fold-search");
    public static final ELispSymbol CASE_SYMBOLS_AS_WORDS = new ELispSymbol("case-symbols-as-words");
    public static final ELispSymbol CASE_TABLE = new ELispSymbol("case-table");
    public static final ELispSymbol CASE_TABLE_P = new ELispSymbol("case-table-p");
    public static final ELispSymbol CATCHER = new ELispSymbol("catcher");
    public static final ELispSymbol CATEGORY = new ELispSymbol("category");
    public static final ELispSymbol CATEGORY_TABLE = new ELispSymbol("category-table");
    public static final ELispSymbol CATEGORY_TABLE_P = new ELispSymbol("category-table-p");
    public static final ELispSymbol CATEGORYP = new ELispSymbol("categoryp");
    public static final ELispSymbol CATEGORYSETP = new ELispSymbol("categorysetp");
    public static final ELispSymbol CCL = new ELispSymbol("ccl");
    public static final ELispSymbol CCL_PROGRAM_IDX = new ELispSymbol("ccl-program-idx");
    public static final ELispSymbol CCLP = new ELispSymbol("cclp");
    public static final ELispSymbol CDR = new ELispSymbol("cdr");
    public static final ELispSymbol CEILING = new ELispSymbol("ceiling");
    public static final ELispSymbol CENTER = new ELispSymbol("center");
    public static final ELispSymbol CHANGE_FRAME_SIZE = new ELispSymbol("change_frame_size");
    public static final ELispSymbol CHANGE_MAJOR_MODE_HOOK = new ELispSymbol("change-major-mode-hook");
    public static final ELispSymbol CHAR_CODE_PROPERTY_TABLE = new ELispSymbol("char-code-property-table");
    public static final ELispSymbol CHAR_FROM_NAME = new ELispSymbol("char-from-name");
    public static final ELispSymbol CHAR_OR_STRING_P = new ELispSymbol("char-or-string-p");
    public static final ELispSymbol CHAR_SCRIPT_TABLE = new ELispSymbol("char-script-table");
    public static final ELispSymbol CHAR_TABLE = new ELispSymbol("char-table");
    public static final ELispSymbol CHAR_TABLE_EXTRA_SLOTS = new ELispSymbol("char-table-extra-slots");
    public static final ELispSymbol CHAR_TABLE_P = new ELispSymbol("char-table-p");
    public static final ELispSymbol CHARACTERP = new ELispSymbol("characterp");
    public static final ELispSymbol CHARSET = new ELispSymbol("charset");
    public static final ELispSymbol CHARSETP = new ELispSymbol("charsetp");
    public static final ELispSymbol CHILD_FRAME_BORDER = new ELispSymbol("child-frame-border");
    public static final ELispSymbol CHILD_FRAME_BORDER_WIDTH = new ELispSymbol("child-frame-border-width");
    public static final ELispSymbol CHOICE = new ELispSymbol("choice");
    public static final ELispSymbol CIPHERS = new ELispSymbol("ciphers");
    public static final ELispSymbol CIRCLE = new ELispSymbol("circle");
    public static final ELispSymbol CIRCULAR_LIST = new ELispSymbol("circular-list");
    public static final ELispSymbol CLONE_INDIRECT_BUFFER_HOOK = new ELispSymbol("clone-indirect-buffer-hook");
    public static final ELispSymbol CLONE_OF = new ELispSymbol("clone-of");
    public static final ELispSymbol CLOSE = new ELispSymbol("close");
    public static final ELispSymbol CLOSE_NOWRITE = new ELispSymbol("close-nowrite");
    public static final ELispSymbol CLOSE_TAB = new ELispSymbol("close-tab");
    public static final ELispSymbol CLOSE_WRITE = new ELispSymbol("close-write");
    public static final ELispSymbol CLOSED = new ELispSymbol("closed");
    public static final ELispSymbol CMAJFLT = new ELispSymbol("cmajflt");
    public static final ELispSymbol CMINFLT = new ELispSymbol("cminflt");
    public static final ELispSymbol CODE_CONVERSION_MAP = new ELispSymbol("code-conversion-map");
    public static final ELispSymbol CODE_CONVERSION_MAP_ID = new ELispSymbol("code-conversion-map-id");
    public static final ELispSymbol CODESET = new ELispSymbol("codeset");
    public static final ELispSymbol CODING = new ELispSymbol("coding");
    public static final ELispSymbol CODING_SYSTEM = new ELispSymbol("coding-system");
    public static final ELispSymbol CODING_SYSTEM_DEFINE_FORM = new ELispSymbol("coding-system-define-form");
    public static final ELispSymbol CODING_SYSTEM_ERROR = new ELispSymbol("coding-system-error");
    public static final ELispSymbol CODING_SYSTEM_FOR_WRITE = new ELispSymbol("coding-system-for-write");
    public static final ELispSymbol CODING_SYSTEM_HISTORY = new ELispSymbol("coding-system-history");
    public static final ELispSymbol CODING_SYSTEM_P = new ELispSymbol("coding-system-p");
    public static final ELispSymbol COLUMNS = new ELispSymbol("columns");
    public static final ELispSymbol COMM = new ELispSymbol("comm");
    public static final ELispSymbol COMMA = new ELispSymbol(",");
    public static final ELispSymbol COMMA_AT = new ELispSymbol(",@");
    public static final ELispSymbol COMMAND_DEBUG_STATUS = new ELispSymbol("command-debug-status");
    public static final ELispSymbol COMMAND_ERROR_DEFAULT_FUNCTION = new ELispSymbol("command-error-default-function");
    public static final ELispSymbol COMMAND_EXECUTE = new ELispSymbol("command-execute");
    public static final ELispSymbol COMMAND_HISTORY = new ELispSymbol("command-history");
    public static final ELispSymbol COMMAND_LINE_PROCESSED = new ELispSymbol("command-line-processed");
    public static final ELispSymbol COMMAND_MODES = new ELispSymbol("command-modes");
    public static final ELispSymbol COMMANDP = new ELispSymbol("commandp");
    public static final ELispSymbol COMMENT = new ELispSymbol("comment");
    public static final ELispSymbol COMMENT_END_CAN_BE_ESCAPED = new ELispSymbol("comment-end-can-be-escaped");
    public static final ELispSymbol COMP_LIBGCCJIT_REPRODUCER = new ELispSymbol("comp-libgccjit-reproducer");
    public static final ELispSymbol COMP_MAYBE_GC_OR_QUIT = new ELispSymbol("comp-maybe-gc-or-quit");
    public static final ELispSymbol COMP_MVAR = new ELispSymbol("comp-mvar");
    public static final ELispSymbol COMP_SANITIZER_ERROR = new ELispSymbol("comp-sanitizer-error");
    public static final ELispSymbol COMP_SUBR_TRAMPOLINE_INSTALL = new ELispSymbol("comp-subr-trampoline-install");
    public static final ELispSymbol COMPLETING_READ_FUNCTION = new ELispSymbol("completing-read-function");
    public static final ELispSymbol COMPLETION_IGNORE_CASE = new ELispSymbol("completion-ignore-case");
    public static final ELispSymbol COMPOSITION = new ELispSymbol("composition");
    public static final ELispSymbol CONCAT = new ELispSymbol("concat");
    public static final ELispSymbol COND_JUMP = new ELispSymbol("cond-jump");
    public static final ELispSymbol COND_JUMP_NARG_LEQ = new ELispSymbol("cond-jump-narg-leq");
    public static final ELispSymbol CONDITION_CASE = new ELispSymbol("condition-case");
    public static final ELispSymbol CONDITION_VARIABLE = new ELispSymbol("condition-variable");
    public static final ELispSymbol CONDITION_VARIABLE_P = new ELispSymbol("condition-variable-p");
    public static final ELispSymbol CONFIG_CHANGED_EVENT = new ELispSymbol("config-changed-event");
    public static final ELispSymbol CONFIGURATION = new ELispSymbol("configuration");
    public static final ELispSymbol CONNECT = new ELispSymbol("connect");
    public static final ELispSymbol CONS = new ELispSymbol("cons");
    public static final ELispSymbol CONSES = new ELispSymbol("conses");
    public static final ELispSymbol CONSP = new ELispSymbol("consp");
    public static final ELispSymbol COPY_DIRECTORY = new ELispSymbol("copy-directory");
    public static final ELispSymbol COPY_FILE = new ELispSymbol("copy-file");
    public static final ELispSymbol CREATE = new ELispSymbol("create");
    public static final ELispSymbol CSTIME = new ELispSymbol("cstime");
    public static final ELispSymbol CTIME = new ELispSymbol("ctime");
    public static final ELispSymbol CURRENT = new ELispSymbol("current");
    public static final ELispSymbol CURRENT_INPUT_METHOD = new ELispSymbol("current-input-method");
    public static final ELispSymbol CURRENT_KEY_REMAP_SEQUENCE = new ELispSymbol("current-key-remap-sequence");
    public static final ELispSymbol CURRENT_LINE = new ELispSymbol("current-line");
    public static final ELispSymbol CURRENT_LOAD_LIST = new ELispSymbol("current-load-list");
    public static final ELispSymbol CURRENT_MINIBUFFER_COMMAND = new ELispSymbol("current-minibuffer-command");
    public static final ELispSymbol CURSOR = new ELispSymbol("cursor");
    public static final ELispSymbol CURSOR_COLOR = new ELispSymbol("cursor-color");
    public static final ELispSymbol CURSOR_IN_ECHO_AREA = new ELispSymbol("cursor-in-echo-area");
    public static final ELispSymbol CURSOR_TYPE = new ELispSymbol("cursor-type");
    public static final ELispSymbol CURVE = new ELispSymbol("curve");
    public static final ELispSymbol CUSTOM_DELAYED_INIT_VARIABLES = new ELispSymbol("custom-delayed-init-variables");
    public static final ELispSymbol CUSTOM_VARIABLE_HISTORY = new ELispSymbol("custom-variable-history");
    public static final ELispSymbol CUSTOM_VARIABLE_P = new ELispSymbol("custom-variable-p");
    public static final ELispSymbol CUTIME = new ELispSymbol("cutime");
    public static final ELispSymbol CYCLE_SORT_FUNCTION = new ELispSymbol("cycle-sort-function");
    public static final ELispSymbol CYCLIC_FUNCTION_INDIRECTION = new ELispSymbol("cyclic-function-indirection");
    public static final ELispSymbol CYCLIC_VARIABLE_INDIRECTION = new ELispSymbol("cyclic-variable-indirection");
    public static final ELispSymbol D = new ELispSymbol("d");
    public static final ELispSymbol D_DEFAULT = new ELispSymbol("d-default");
    public static final ELispSymbol D_EPHEMERAL = new ELispSymbol("d-ephemeral");
    public static final ELispSymbol D_IMPURE = new ELispSymbol("d-impure");
    public static final ELispSymbol DASHES = new ELispSymbol("dashes");
    public static final ELispSymbol DATA = new ELispSymbol("data");
    public static final ELispSymbol DATAGRAM = new ELispSymbol("datagram");
    public static final ELispSymbol DAYS = new ELispSymbol("days");
    public static final ELispSymbol DBUS_ERROR = new ELispSymbol("dbus-error");
    public static final ELispSymbol DBUS_EVENT = new ELispSymbol("dbus-event");
    public static final ELispSymbol DBUS_GET_NAME_OWNER = new ELispSymbol("dbus-get-name-owner");
    public static final ELispSymbol DBUS_MESSAGE_INTERNAL = new ELispSymbol("dbus-message-internal");
    public static final ELispSymbol DEACTIVATE_MARK = new ELispSymbol("deactivate-mark");
    public static final ELispSymbol DEBUG = new ELispSymbol("debug");
    public static final ELispSymbol DEBUG_EARLY = new ELispSymbol("debug-early");
    public static final ELispSymbol DEBUG_EARLY__HANDLER = new ELispSymbol("debug-early--handler");
    public static final ELispSymbol DEBUG_EARLY__MUTED = new ELispSymbol("debug-early--muted");
    public static final ELispSymbol DEBUGGER = new ELispSymbol("debugger");
    public static final ELispSymbol DEBUGGER_MAY_CONTINUE = new ELispSymbol("debugger-may-continue");
    public static final ELispSymbol DECOMPOSED_CHARACTERS = new ELispSymbol("decomposed-characters");
    public static final ELispSymbol DEDICATED = new ELispSymbol("dedicated");
    public static final ELispSymbol DEFALIAS_FSET_FUNCTION = new ELispSymbol("defalias-fset-function");
    public static final ELispSymbol DEFAULT = new ELispSymbol("default");
    public static final ELispSymbol DEFAULT_DIRECTORY = new ELispSymbol("default-directory");
    public static final ELispSymbol DEFAULT_KEYBOARD_CODING_SYSTEM = new ELispSymbol("default-keyboard-coding-system");
    public static final ELispSymbol DEFAULT_TERMINAL_CODING_SYSTEM = new ELispSymbol("default-terminal-coding-system");
    public static final ELispSymbol DEFINE_CHARSET_INTERNAL = new ELispSymbol("define-charset-internal");
    public static final ELispSymbol DEFINE_CODING_SYSTEM_INTERNAL = new ELispSymbol("define-coding-system-internal");
    public static final ELispSymbol DEFUN = new ELispSymbol("defun");
    public static final ELispSymbol DEFVARALIAS = new ELispSymbol("defvaralias");
    public static final ELispSymbol DELAYED_WARNINGS_HOOK = new ELispSymbol("delayed-warnings-hook");
    public static final ELispSymbol DELETE = new ELispSymbol("delete");
    public static final ELispSymbol DELETE_AUTO_SAVE_FILE_IF_NECESSARY = new ELispSymbol("delete-auto-save-file-if-necessary");
    public static final ELispSymbol DELETE_BEFORE = new ELispSymbol("delete-before");
    public static final ELispSymbol DELETE_BY_MOVING_TO_TRASH = new ELispSymbol("delete-by-moving-to-trash");
    public static final ELispSymbol DELETE_DIRECTORY = new ELispSymbol("delete-directory");
    public static final ELispSymbol DELETE_FILE = new ELispSymbol("delete-file");
    public static final ELispSymbol DELETE_FILE_INTERNAL = new ELispSymbol("delete-file-internal");
    public static final ELispSymbol DELETE_FRAME = new ELispSymbol("delete-frame");
    public static final ELispSymbol DELETE_FRAME_FUNCTIONS = new ELispSymbol("delete-frame-functions");
    public static final ELispSymbol DELETE_SELF = new ELispSymbol("delete-self");
    public static final ELispSymbol DELETE_TERMINAL_FUNCTIONS = new ELispSymbol("delete-terminal-functions");
    public static final ELispSymbol DELETE_WINDOW = new ELispSymbol("delete-window");
    public static final ELispSymbol DIGESTS = new ELispSymbol("digests");
    public static final ELispSymbol DIR_OK = new ELispSymbol("dir-ok");
    public static final ELispSymbol DIRECT_CALL = new ELispSymbol("direct-call");
    public static final ELispSymbol DIRECT_CALLREF = new ELispSymbol("direct-callref");
    public static final ELispSymbol DIRECTORY_FILE_NAME = new ELispSymbol("directory-file-name");
    public static final ELispSymbol DIRECTORY_FILES = new ELispSymbol("directory-files");
    public static final ELispSymbol DIRECTORY_FILES_AND_ATTRIBUTES = new ELispSymbol("directory-files-and-attributes");
    public static final ELispSymbol DISABLE_EVAL = new ELispSymbol("disable-eval");
    public static final ELispSymbol DISABLED = new ELispSymbol("disabled");
    public static final ELispSymbol DISPLAY = new ELispSymbol("display");
    public static final ELispSymbol DISPLAY_BUFFER = new ELispSymbol("display-buffer");
    public static final ELispSymbol DISPLAY_FILL_COLUMN_INDICATOR = new ELispSymbol("display-fill-column-indicator");
    public static final ELispSymbol DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER = new ELispSymbol("display-fill-column-indicator-character");
    public static final ELispSymbol DISPLAY_FILL_COLUMN_INDICATOR_COLUMN = new ELispSymbol("display-fill-column-indicator-column");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS = new ELispSymbol("display-line-numbers");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_DISABLE = new ELispSymbol("display-line-numbers-disable");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_OFFSET = new ELispSymbol("display-line-numbers-offset");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_WIDEN = new ELispSymbol("display-line-numbers-widen");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_WIDTH = new ELispSymbol("display-line-numbers-width");
    public static final ELispSymbol DISPLAY_MONITORS_CHANGED_FUNCTIONS = new ELispSymbol("display-monitors-changed-functions");
    public static final ELispSymbol DISPLAY_TABLE = new ELispSymbol("display-table");
    public static final ELispSymbol DISPLAY_TYPE = new ELispSymbol("display-type");
    public static final ELispSymbol DISPLAY_WARNING = new ELispSymbol("display-warning");
    public static final ELispSymbol DO_AFTER_LOAD_EVALUATION = new ELispSymbol("do-after-load-evaluation");
    public static final ELispSymbol DOMAIN_ERROR = new ELispSymbol("domain-error");
    public static final ELispSymbol DONT_CLEAR_MESSAGE = new ELispSymbol("dont-clear-message");
    public static final ELispSymbol DONT_FOLLOW = new ELispSymbol("dont-follow");
    public static final ELispSymbol DOS = new ELispSymbol("dos");
    public static final ELispSymbol DOTS = new ELispSymbol("dots");
    public static final ELispSymbol DOUBLE_LINE = new ELispSymbol("double-line");
    public static final ELispSymbol DOWN = new ELispSymbol("down");
    public static final ELispSymbol DRAG_INTERNAL_BORDER = new ELispSymbol("drag-internal-border");
    public static final ELispSymbol DRAG_N_DROP = new ELispSymbol("drag-n-drop");
    public static final ELispSymbol DRAG_SOURCE = new ELispSymbol("drag-source");
    public static final ELispSymbol DRAG_WITH_HEADER_LINE = new ELispSymbol("drag-with-header-line");
    public static final ELispSymbol DRAG_WITH_MODE_LINE = new ELispSymbol("drag-with-mode-line");
    public static final ELispSymbol DRAG_WITH_TAB_LINE = new ELispSymbol("drag-with-tab-line");
    public static final ELispSymbol DRAGGING = new ELispSymbol("dragging");
    public static final ELispSymbol DROPPING = new ELispSymbol("dropping");
    public static final ELispSymbol DUMP_EMACS_PORTABLE__SORT_PREDICATE = new ELispSymbol("dump-emacs-portable--sort-predicate");
    public static final ELispSymbol DUMP_EMACS_PORTABLE__SORT_PREDICATE_COPIED = new ELispSymbol("dump-emacs-portable--sort-predicate-copied");
    public static final ELispSymbol DUMP_FILE_NAME = new ELispSymbol("dump-file-name");
    public static final ELispSymbol DUMPED_WITH_PDUMPER = new ELispSymbol("dumped-with-pdumper");
    public static final ELispSymbol ECHO_AREA_CLEAR_HOOK = new ELispSymbol("echo-area-clear-hook");
    public static final ELispSymbol ECHO_KEYSTROKES = new ELispSymbol("echo-keystrokes");
    public static final ELispSymbol EGID = new ELispSymbol("egid");
    public static final ELispSymbol EIGHT_BIT = new ELispSymbol("eight-bit");
    public static final ELispSymbol ELT = new ELispSymbol("elt");
    public static final ELispSymbol EMACS = new ELispSymbol("emacs");
    public static final ELispSymbol EMACS_MULE = new ELispSymbol("emacs-mule");
    public static final ELispSymbol EMOJI = new ELispSymbol("emoji");
    public static final ELispSymbol EMPTY_BOX = new ELispSymbol("empty-box");
    public static final ELispSymbol ENABLE_RECURSIVE_MINIBUFFERS = new ELispSymbol("enable-recursive-minibuffers");
    public static final ELispSymbol ENCODE_TIME = new ELispSymbol("encode-time");
    public static final ELispSymbol ENCODED = new ELispSymbol("encoded");
    public static final ELispSymbol END_OF_BUFFER = new ELispSymbol("end-of-buffer");
    public static final ELispSymbol END_OF_FILE = new ELispSymbol("end-of-file");
    public static final ELispSymbol END_SCROLL = new ELispSymbol("end-scroll");
    public static final ELispSymbol END_SESSION = new ELispSymbol("end-session");
    public static final ELispSymbol ENTRY = new ELispSymbol("entry");
    public static final ELispSymbol EQ = new ELispSymbol("eq");
    public static final ELispSymbol EQL = new ELispSymbol("eql");
    public static final ELispSymbol EQUAL = new ELispSymbol("equal");
    public static final ELispSymbol ERROR_CONDITIONS = new ELispSymbol("error-conditions");
    public static final ELispSymbol ERROR_MESSAGE = new ELispSymbol("error-message");
    public static final ELispSymbol ESCAPE_GLYPH = new ELispSymbol("escape-glyph");
    public static final ELispSymbol ETIME = new ELispSymbol("etime");
    public static final ELispSymbol EUID = new ELispSymbol("euid");
    public static final ELispSymbol EVAL = new ELispSymbol("eval");
    public static final ELispSymbol EVAL_BUFFER_LIST = new ELispSymbol("eval-buffer-list");
    public static final ELispSymbol EVAL_MINIBUFFER = new ELispSymbol("eval-minibuffer");
    public static final ELispSymbol EVAPORATE = new ELispSymbol("evaporate");
    public static final ELispSymbol EVEN = new ELispSymbol("even");
    public static final ELispSymbol EVENT_KIND = new ELispSymbol("event-kind");
    public static final ELispSymbol EVENT_SYMBOL_ELEMENT_MASK = new ELispSymbol("event-symbol-element-mask");
    public static final ELispSymbol EVENT_SYMBOL_ELEMENTS = new ELispSymbol("event-symbol-elements");
    public static final ELispSymbol EXCESSIVE_LISP_NESTING = new ELispSymbol("excessive-lisp-nesting");
    public static final ELispSymbol EXCESSIVE_VARIABLE_BINDING = new ELispSymbol("excessive-variable-binding");
    public static final ELispSymbol EXCL = new ELispSymbol("excl");
    public static final ELispSymbol EXIT = new ELispSymbol("exit");
    public static final ELispSymbol EXPAND_ABBREV = new ELispSymbol("expand-abbrev");
    public static final ELispSymbol EXPAND_FILE_NAME = new ELispSymbol("expand-file-name");
    public static final ELispSymbol EXPLICIT = new ELispSymbol("explicit");
    public static final ELispSymbol EXPLICIT_NAME = new ELispSymbol("explicit-name");
    public static final ELispSymbol EXTERNAL_BORDER_SIZE = new ELispSymbol("external-border-size");
    public static final ELispSymbol EXTERNAL_DEBUGGING_OUTPUT = new ELispSymbol("external-debugging-output");
    public static final ELispSymbol EXTRA = new ELispSymbol("extra");
    public static final ELispSymbol EXTRA_BOLD = new ELispSymbol("extra-bold");
    public static final ELispSymbol EXTRA_LIGHT = new ELispSymbol("extra-light");
    public static final ELispSymbol F0 = new ELispSymbol("f0");
    public static final ELispSymbol F10 = new ELispSymbol("f10");
    public static final ELispSymbol FACE = new ELispSymbol("face");
    public static final ELispSymbol FACE_ALIAS = new ELispSymbol("face-alias");
    public static final ELispSymbol FACE_NO_INHERIT = new ELispSymbol("face-no-inherit");
    public static final ELispSymbol FACE_REMAPPING_ALIST = new ELispSymbol("face-remapping-alist");
    public static final ELispSymbol FACE_SET_AFTER_FRAME_DEFAULT = new ELispSymbol("face-set-after-frame-default");
    public static final ELispSymbol FAILED = new ELispSymbol("failed");
    public static final ELispSymbol FALSE = new ELispSymbol("false");
    public static final ELispSymbol FBOUNDP = new ELispSymbol("fboundp");
    public static final ELispSymbol FEATURES = new ELispSymbol("features");
    public static final ELispSymbol FETCH_HANDLER = new ELispSymbol("fetch-handler");
    public static final ELispSymbol FIELD = new ELispSymbol("field");
    public static final ELispSymbol FILE_ACCESSIBLE_DIRECTORY_P = new ELispSymbol("file-accessible-directory-p");
    public static final ELispSymbol FILE_ACL = new ELispSymbol("file-acl");
    public static final ELispSymbol FILE_ALREADY_EXISTS = new ELispSymbol("file-already-exists");
    public static final ELispSymbol FILE_ATTRIBUTES = new ELispSymbol("file-attributes");
    public static final ELispSymbol FILE_ATTRIBUTES_LESSP = new ELispSymbol("file-attributes-lessp");
    public static final ELispSymbol FILE_DATE_ERROR = new ELispSymbol("file-date-error");
    public static final ELispSymbol FILE_DIRECTORY_P = new ELispSymbol("file-directory-p");
    public static final ELispSymbol FILE_ERROR = new ELispSymbol("file-error");
    public static final ELispSymbol FILE_EXECUTABLE_P = new ELispSymbol("file-executable-p");
    public static final ELispSymbol FILE_EXISTS_P = new ELispSymbol("file-exists-p");
    public static final ELispSymbol FILE_LOCKED_P = new ELispSymbol("file-locked-p");
    public static final ELispSymbol FILE_MISSING = new ELispSymbol("file-missing");
    public static final ELispSymbol FILE_MODES = new ELispSymbol("file-modes");
    public static final ELispSymbol FILE_NAME_ALL_COMPLETIONS = new ELispSymbol("file-name-all-completions");
    public static final ELispSymbol FILE_NAME_AS_DIRECTORY = new ELispSymbol("file-name-as-directory");
    public static final ELispSymbol FILE_NAME_CASE_INSENSITIVE_P = new ELispSymbol("file-name-case-insensitive-p");
    public static final ELispSymbol FILE_NAME_COMPLETION = new ELispSymbol("file-name-completion");
    public static final ELispSymbol FILE_NAME_DIRECTORY = new ELispSymbol("file-name-directory");
    public static final ELispSymbol FILE_NAME_HANDLER_ALIST = new ELispSymbol("file-name-handler-alist");
    public static final ELispSymbol FILE_NAME_HISTORY = new ELispSymbol("file-name-history");
    public static final ELispSymbol FILE_NAME_NONDIRECTORY = new ELispSymbol("file-name-nondirectory");
    public static final ELispSymbol FILE_NEWER_THAN_FILE_P = new ELispSymbol("file-newer-than-file-p");
    public static final ELispSymbol FILE_NOTIFY = new ELispSymbol("file-notify");
    public static final ELispSymbol FILE_NOTIFY_ERROR = new ELispSymbol("file-notify-error");
    public static final ELispSymbol FILE_OFFSET = new ELispSymbol("file-offset");
    public static final ELispSymbol FILE_READABLE_P = new ELispSymbol("file-readable-p");
    public static final ELispSymbol FILE_REGULAR_P = new ELispSymbol("file-regular-p");
    public static final ELispSymbol FILE_REMOTE_P = new ELispSymbol("file-remote-p");
    public static final ELispSymbol FILE_SELINUX_CONTEXT = new ELispSymbol("file-selinux-context");
    public static final ELispSymbol FILE_SYMLINK_P = new ELispSymbol("file-symlink-p");
    public static final ELispSymbol FILE_SYSTEM_INFO = new ELispSymbol("file-system-info");
    public static final ELispSymbol FILE_TRUENAME = new ELispSymbol("file-truename");
    public static final ELispSymbol FILE_WRITABLE_P = new ELispSymbol("file-writable-p");
    public static final ELispSymbol FILENAMEP = new ELispSymbol("filenamep");
    public static final ELispSymbol FILL_COLUMN_INDICATOR = new ELispSymbol("fill-column-indicator");
    public static final ELispSymbol FINALIZER = new ELispSymbol("finalizer");
    public static final ELispSymbol FIRST_CHANGE_HOOK = new ELispSymbol("first-change-hook");
    public static final ELispSymbol FIXNUM = new ELispSymbol("fixnum");
    public static final ELispSymbol FIXNUM_OR_SYMBOL_WITH_POS_P = new ELispSymbol("fixnum-or-symbol-with-pos-p");
    public static final ELispSymbol FIXNUMP = new ELispSymbol("fixnump");
    public static final ELispSymbol FLAT_BUTTON = new ELispSymbol("flat-button");
    public static final ELispSymbol FLOAT = new ELispSymbol("float");
    public static final ELispSymbol FLOAT_OUTPUT_FORMAT = new ELispSymbol("float-output-format");
    public static final ELispSymbol FLOATP = new ELispSymbol("floatp");
    public static final ELispSymbol FLOATS = new ELispSymbol("floats");
    public static final ELispSymbol FLOOR = new ELispSymbol("floor");
    public static final ELispSymbol FOCUS_IN = new ELispSymbol("focus-in");
    public static final ELispSymbol FOCUS_OUT = new ELispSymbol("focus-out");
    public static final ELispSymbol FONT = new ELispSymbol("font");
    public static final ELispSymbol FONT_BACKEND = new ELispSymbol("font-backend");
    public static final ELispSymbol FONT_DRIVER_SUPERSEDED_BY = new ELispSymbol("font-driver-superseded-by");
    public static final ELispSymbol FONT_ENTITY = new ELispSymbol("font-entity");
    public static final ELispSymbol FONT_EXTRA_TYPE = new ELispSymbol("font-extra-type");
    public static final ELispSymbol FONT_LOCK_FACE = new ELispSymbol("font-lock-face");
    public static final ELispSymbol FONT_OBJECT = new ELispSymbol("font-object");
    public static final ELispSymbol FONT_PARAMETER = new ELispSymbol("font-parameter");
    public static final ELispSymbol FONT_SPEC = new ELispSymbol("font-spec");
    public static final ELispSymbol FONTIFICATION_FUNCTIONS = new ELispSymbol("fontification-functions");
    public static final ELispSymbol FONTIFIED = new ELispSymbol("fontified");
    public static final ELispSymbol FOREGROUND_COLOR = new ELispSymbol("foreground-color");
    public static final ELispSymbol FORMAT_ANNOTATE_FUNCTION = new ELispSymbol("format-annotate-function");
    public static final ELispSymbol FORMAT_DECODE = new ELispSymbol("format-decode");
    public static final ELispSymbol FORMAT_PROMPT = new ELispSymbol("format-prompt");
    public static final ELispSymbol FRACTION = new ELispSymbol("fraction");
    public static final ELispSymbol FRAME = new ELispSymbol("frame");
    public static final ELispSymbol FRAME_EDGES = new ELispSymbol("frame-edges");
    public static final ELispSymbol FRAME_LIVE_P = new ELispSymbol("frame-live-p");
    public static final ELispSymbol FRAME_MONITOR_ATTRIBUTES = new ELispSymbol("frame-monitor-attributes");
    public static final ELispSymbol FRAME_SET_BACKGROUND_MODE = new ELispSymbol("frame-set-background-mode");
    public static final ELispSymbol FRAME_WINDOWS_MIN_SIZE = new ELispSymbol("frame-windows-min-size");
    public static final ELispSymbol FRAMEP = new ELispSymbol("framep");
    public static final ELispSymbol FRAMES = new ELispSymbol("frames");
    public static final ELispSymbol FRINGE = new ELispSymbol("fringe");
    public static final ELispSymbol FROM__TTY_MENU_P = new ELispSymbol("from--tty-menu-p");
    public static final ELispSymbol FRONT_STICKY = new ELispSymbol("front-sticky");
    public static final ELispSymbol FULL = new ELispSymbol("full");
    public static final ELispSymbol FULLBOTH = new ELispSymbol("fullboth");
    public static final ELispSymbol FULLHEIGHT = new ELispSymbol("fullheight");
    public static final ELispSymbol FULLSCREEN = new ELispSymbol("fullscreen");
    public static final ELispSymbol FULLWIDTH = new ELispSymbol("fullwidth");
    public static final ELispSymbol FUNCALL = new ELispSymbol("funcall");
    public static final ELispSymbol FUNCALL_INTERACTIVELY = new ELispSymbol("funcall-interactively");
    public static final ELispSymbol FUNCTION = new ELispSymbol("function");
    public static final ELispSymbol FUNCTION_DOCUMENTATION = new ELispSymbol("function-documentation");
    public static final ELispSymbol FUNCTION_HISTORY = new ELispSymbol("function-history");
    public static final ELispSymbol FUNCTION_KEY = new ELispSymbol("function-key");
    public static final ELispSymbol FUNCTIONP = new ELispSymbol("functionp");
    public static final ELispSymbol FUNDAMENTAL_MODE = new ELispSymbol("fundamental-mode");
    public static final ELispSymbol GC_CONS_PERCENTAGE = new ELispSymbol("gc-cons-percentage");
    public static final ELispSymbol GC_CONS_THRESHOLD = new ELispSymbol("gc-cons-threshold");
    public static final ELispSymbol GCCJIT = new ELispSymbol("gccjit");
    public static final ELispSymbol GEOMETRY = new ELispSymbol("geometry");
    public static final ELispSymbol GET_BUFFER_WINDOW_LIST = new ELispSymbol("get-buffer-window-list");
    public static final ELispSymbol GET_EMACS_MULE_FILE_CHAR = new ELispSymbol("get-emacs-mule-file-char");
    public static final ELispSymbol GET_FILE_BUFFER = new ELispSymbol("get-file-buffer");
    public static final ELispSymbol GET_FILE_CHAR = new ELispSymbol("get-file-char");
    public static final ELispSymbol GET_MRU_WINDOW = new ELispSymbol("get-mru-window");
    public static final ELispSymbol GET_SCRATCH_BUFFER_CREATE = new ELispSymbol("get-scratch-buffer-create");
    public static final ELispSymbol GLYPHLESS_CHAR = new ELispSymbol("glyphless-char");
    public static final ELispSymbol GLYPHLESS_CHAR_DISPLAY = new ELispSymbol("glyphless-char-display");
    public static final ELispSymbol GNUTLS = new ELispSymbol("gnutls");
    public static final ELispSymbol GNUTLS3 = new ELispSymbol("gnutls3");
    public static final ELispSymbol GNUTLS_ANON = new ELispSymbol("gnutls-anon");
    public static final ELispSymbol GNUTLS_CODE = new ELispSymbol("gnutls-code");
    public static final ELispSymbol GNUTLS_E_AGAIN = new ELispSymbol("gnutls-e-again");
    public static final ELispSymbol GNUTLS_E_INTERRUPTED = new ELispSymbol("gnutls-e-interrupted");
    public static final ELispSymbol GNUTLS_E_INVALID_SESSION = new ELispSymbol("gnutls-e-invalid-session");
    public static final ELispSymbol GNUTLS_E_NOT_READY_FOR_HANDSHAKE = new ELispSymbol("gnutls-e-not-ready-for-handshake");
    public static final ELispSymbol GNUTLS_PKCS_NULL_PASSWORD = new ELispSymbol("GNUTLS_PKCS_NULL_PASSWORD");
    public static final ELispSymbol GNUTLS_PKCS_PBES1_DES_MD5 = new ELispSymbol("GNUTLS_PKCS_PBES1_DES_MD5");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_3DES = new ELispSymbol("GNUTLS_PKCS_PBES2_3DES");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_AES_128 = new ELispSymbol("GNUTLS_PKCS_PBES2_AES_128");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_AES_192 = new ELispSymbol("GNUTLS_PKCS_PBES2_AES_192");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_AES_256 = new ELispSymbol("GNUTLS_PKCS_PBES2_AES_256");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_DES = new ELispSymbol("GNUTLS_PKCS_PBES2_DES");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_GOST_CPA = new ELispSymbol("GNUTLS_PKCS_PBES2_GOST_CPA");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_GOST_CPB = new ELispSymbol("GNUTLS_PKCS_PBES2_GOST_CPB");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_GOST_CPC = new ELispSymbol("GNUTLS_PKCS_PBES2_GOST_CPC");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_GOST_CPD = new ELispSymbol("GNUTLS_PKCS_PBES2_GOST_CPD");
    public static final ELispSymbol GNUTLS_PKCS_PBES2_GOST_TC26Z = new ELispSymbol("GNUTLS_PKCS_PBES2_GOST_TC26Z");
    public static final ELispSymbol GNUTLS_PKCS_PKCS12_3DES = new ELispSymbol("GNUTLS_PKCS_PKCS12_3DES");
    public static final ELispSymbol GNUTLS_PKCS_PKCS12_ARCFOUR = new ELispSymbol("GNUTLS_PKCS_PKCS12_ARCFOUR");
    public static final ELispSymbol GNUTLS_PKCS_PKCS12_RC2_40 = new ELispSymbol("GNUTLS_PKCS_PKCS12_RC2_40");
    public static final ELispSymbol GNUTLS_PKCS_PLAIN = new ELispSymbol("GNUTLS_PKCS_PLAIN");
    public static final ELispSymbol GNUTLS_TYPE_CIPHER = new ELispSymbol("gnutls-symmetric-cipher");
    public static final ELispSymbol GNUTLS_TYPE_DIGEST_ALGORITHM = new ELispSymbol("gnutls-digest-algorithm");
    public static final ELispSymbol GNUTLS_TYPE_MAC_ALGORITHM = new ELispSymbol("gnutls-mac-algorithm");
    public static final ELispSymbol GNUTLS_X509PKI = new ELispSymbol("gnutls-x509pki");
    public static final ELispSymbol GRAVE = new ELispSymbol("grave");
    public static final ELispSymbol GROUP = new ELispSymbol("group");
    public static final ELispSymbol GROW_ONLY = new ELispSymbol("grow-only");
    public static final ELispSymbol GUI_FIGURE_WINDOW_SIZE = new ELispSymbol("gui_figure_window_size");
    public static final ELispSymbol GUI_SET_SELECTION = new ELispSymbol("gui-set-selection");
    public static final ELispSymbol HAIKU = new ELispSymbol("haiku");
    public static final ELispSymbol HAND = new ELispSymbol("hand");
    public static final ELispSymbol HANDLE = new ELispSymbol("handle");
    public static final ELispSymbol HANDLE_SELECT_WINDOW = new ELispSymbol("handle-select-window");
    public static final ELispSymbol HANDLE_SHIFT_SELECTION = new ELispSymbol("handle-shift-selection");
    public static final ELispSymbol HANDLE_SWITCH_FRAME = new ELispSymbol("handle-switch-frame");
    public static final ELispSymbol HAS_ERROR = new ELispSymbol("has-error");
    public static final ELispSymbol HASH_TABLE = new ELispSymbol("hash-table");
    public static final ELispSymbol HASH_TABLE_P = new ELispSymbol("hash-table-p");
    public static final ELispSymbol HASH_TABLE_TEST = new ELispSymbol("hash-table-test");
    public static final ELispSymbol HBAR = new ELispSymbol("hbar");
    public static final ELispSymbol HDRAG = new ELispSymbol("hdrag");
    public static final ELispSymbol HEADER_LINE = new ELispSymbol("header-line");
    public static final ELispSymbol HEADER_LINE_FORMAT = new ELispSymbol("header-line-format");
    public static final ELispSymbol HEAP = new ELispSymbol("heap");
    public static final ELispSymbol HEAVY = new ELispSymbol("heavy");
    public static final ELispSymbol HEIGHT = new ELispSymbol("height");
    public static final ELispSymbol HEIGHT_ONLY = new ELispSymbol("height-only");
    public static final ELispSymbol HELP__APPEND_KEYSTROKES_HELP = new ELispSymbol("help--append-keystrokes-help");
    public static final ELispSymbol HELP__DESCRIBE_MAP_TREE = new ELispSymbol("help--describe-map-tree");
    public static final ELispSymbol HELP_ECHO = new ELispSymbol("help-echo");
    public static final ELispSymbol HELP_ECHO_INHIBIT_SUBSTITUTION = new ELispSymbol("help-echo-inhibit-substitution");
    public static final ELispSymbol HELP_FORM_SHOW = new ELispSymbol("help-form-show");
    public static final ELispSymbol HELP_KEY_BINDING = new ELispSymbol("help-key-binding");
    public static final ELispSymbol HELPER_SANITIZER_ASSERT = new ELispSymbol("helper_sanitizer_assert");
    public static final ELispSymbol HELPER_SAVE_RESTRICTION = new ELispSymbol("helper_save_restriction");
    public static final ELispSymbol HELPER_UNBIND_N = new ELispSymbol("helper_unbind_n");
    public static final ELispSymbol HELPER_UNWIND_PROTECT = new ELispSymbol("helper_unwind_protect");
    public static final ELispSymbol HEX_CODE = new ELispSymbol("hex-code");
    public static final ELispSymbol HIDE = new ELispSymbol("hide");
    public static final ELispSymbol HOLLOW = new ELispSymbol("hollow");
    public static final ELispSymbol HORIZONTAL_HANDLE = new ELispSymbol("horizontal-handle");
    public static final ELispSymbol HORIZONTAL_SCROLL_BAR = new ELispSymbol("horizontal-scroll-bar");
    public static final ELispSymbol HORIZONTAL_SCROLL_BARS = new ELispSymbol("horizontal-scroll-bars");
    public static final ELispSymbol HOURGLASS = new ELispSymbol("hourglass");
    public static final ELispSymbol HW = new ELispSymbol("hw");
    public static final ELispSymbol ICON = new ELispSymbol("icon");
    public static final ELispSymbol ICON_LEFT = new ELispSymbol("icon-left");
    public static final ELispSymbol ICON_NAME = new ELispSymbol("icon-name");
    public static final ELispSymbol ICON_TOP = new ELispSymbol("icon-top");
    public static final ELispSymbol ICON_TYPE = new ELispSymbol("icon-type");
    public static final ELispSymbol ICONIFY_FRAME = new ELispSymbol("iconify-frame");
    public static final ELispSymbol ICONIFY_TOP_LEVEL = new ELispSymbol("iconify-top-level");
    public static final ELispSymbol IDENTITY = new ELispSymbol("identity");
    public static final ELispSymbol IF = new ELispSymbol("if");
    public static final ELispSymbol IF_REGULAR = new ELispSymbol("if-regular");
    public static final ELispSymbol IGNORE_SELF_INSERT = new ELispSymbol("ignore-self-insert");
    public static final ELispSymbol IGNORED = new ELispSymbol("ignored");
    public static final ELispSymbol IMAGE = new ELispSymbol("image");
    public static final ELispSymbol INC_ARGS = new ELispSymbol("inc-args");
    public static final ELispSymbol INHIBIT_CHANGING_MATCH_DATA = new ELispSymbol("inhibit-changing-match-data");
    public static final ELispSymbol INHIBIT_DEBUGGER = new ELispSymbol("inhibit-debugger");
    public static final ELispSymbol INHIBIT_DOUBLE_BUFFERING = new ELispSymbol("inhibit-double-buffering");
    public static final ELispSymbol INHIBIT_EVAL_DURING_REDISPLAY = new ELispSymbol("inhibit-eval-during-redisplay");
    public static final ELispSymbol INHIBIT_FILE_NAME_OPERATION = new ELispSymbol("inhibit-file-name-operation");
    public static final ELispSymbol INHIBIT_FREE_REALIZED_FACES = new ELispSymbol("inhibit-free-realized-faces");
    public static final ELispSymbol INHIBIT_MENUBAR_UPDATE = new ELispSymbol("inhibit-menubar-update");
    public static final ELispSymbol INHIBIT_MODIFICATION_HOOKS = new ELispSymbol("inhibit-modification-hooks");
    public static final ELispSymbol INHIBIT_POINT_MOTION_HOOKS = new ELispSymbol("inhibit-point-motion-hooks");
    public static final ELispSymbol INHIBIT_QUIT = new ELispSymbol("inhibit-quit");
    public static final ELispSymbol INHIBIT_READ_ONLY = new ELispSymbol("inhibit-read-only");
    public static final ELispSymbol INHIBIT_REDISPLAY = new ELispSymbol("inhibit-redisplay");
    public static final ELispSymbol INHIBITED_INTERACTION = new ELispSymbol("inhibited-interaction");
    public static final ELispSymbol INITIAL_MAJOR_MODE = new ELispSymbol("initial-major-mode");
    public static final ELispSymbol INNER_EDGES = new ELispSymbol("inner-edges");
    public static final ELispSymbol INPUT_METHOD_EXIT_ON_FIRST_CHAR = new ELispSymbol("input-method-exit-on-first-char");
    public static final ELispSymbol INPUT_METHOD_USE_ECHO_AREA = new ELispSymbol("input-method-use-echo-area");
    public static final ELispSymbol INSERT_BEHIND_HOOKS = new ELispSymbol("insert-behind-hooks");
    public static final ELispSymbol INSERT_FILE_CONTENTS = new ELispSymbol("insert-file-contents");
    public static final ELispSymbol INSERT_IN_FRONT_HOOKS = new ELispSymbol("insert-in-front-hooks");
    public static final ELispSymbol INSERTED_CHARS = new ELispSymbol("inserted-chars");
    public static final ELispSymbol INSUFFICIENT_SOURCE = new ELispSymbol("insufficient-source");
    public static final ELispSymbol INTANGIBLE = new ELispSymbol("intangible");
    public static final ELispSymbol INTEGER = new ELispSymbol("integer");
    public static final ELispSymbol INTEGER_OR_MARKER_P = new ELispSymbol("integer-or-marker-p");
    public static final ELispSymbol INTEGERP = new ELispSymbol("integerp");
    public static final ELispSymbol INTERACTIVE = new ELispSymbol("interactive");
    public static final ELispSymbol INTERACTIVE_ARGS = new ELispSymbol("interactive-args");
    public static final ELispSymbol INTERACTIVE_FORM = new ELispSymbol("interactive-form");
    public static final ELispSymbol INTERACTIVE_P = new ELispSymbol("interactive-p");
    public static final ELispSymbol INTERNAL__SYNTAX_PROPERTIZE = new ELispSymbol("internal--syntax-propertize");
    public static final ELispSymbol INTERNAL_AUTO_FILL = new ELispSymbol("internal-auto-fill");
    public static final ELispSymbol INTERNAL_BORDER = new ELispSymbol("internal-border");
    public static final ELispSymbol INTERNAL_BORDER_WIDTH = new ELispSymbol("internal-border-width");
    public static final ELispSymbol INTERNAL_COMPLETE_BUFFER = new ELispSymbol("internal-complete-buffer");
    public static final ELispSymbol INTERNAL_DEFAULT_INTERRUPT_PROCESS = new ELispSymbol("internal-default-interrupt-process");
    public static final ELispSymbol INTERNAL_DEFAULT_PROCESS_FILTER = new ELispSymbol("internal-default-process-filter");
    public static final ELispSymbol INTERNAL_DEFAULT_PROCESS_SENTINEL = new ELispSymbol("internal-default-process-sentinel");
    public static final ELispSymbol INTERNAL_DEFAULT_SIGNAL_PROCESS = new ELispSymbol("internal-default-signal-process");
    public static final ELispSymbol INTERNAL_ECHO_KEYSTROKES_PREFIX = new ELispSymbol("internal-echo-keystrokes-prefix");
    public static final ELispSymbol INTERNAL_INTERPRETER_ENVIRONMENT = new ELispSymbol("internal-interpreter-environment");
    public static final ELispSymbol INTERNAL_MACROEXPAND_FOR_LOAD = new ELispSymbol("internal-macroexpand-for-load");
    public static final ELispSymbol INTERNAL_TIMER_START_IDLE = new ELispSymbol("internal-timer-start-idle");
    public static final ELispSymbol INTERNAL_WHEN_ENTERED_DEBUGGER = new ELispSymbol("internal-when-entered-debugger");
    public static final ELispSymbol INTERPRETED_FUNCTION = new ELispSymbol("interpreted-function");
    public static final ELispSymbol INTERRUPT_PROCESS_FUNCTIONS = new ELispSymbol("interrupt-process-functions");
    public static final ELispSymbol INTERRUPTED = new ELispSymbol("interrupted");
    public static final ELispSymbol INTERVALS = new ELispSymbol("intervals");
    public static final ELispSymbol INVALID_ARITY = new ELispSymbol("invalid-arity");
    public static final ELispSymbol INVALID_FUNCTION = new ELispSymbol("invalid-function");
    public static final ELispSymbol INVALID_READ_SYNTAX = new ELispSymbol("invalid-read-syntax");
    public static final ELispSymbol INVALID_REGEXP = new ELispSymbol("invalid-regexp");
    public static final ELispSymbol INVALID_SOURCE = new ELispSymbol("invalid-source");
    public static final ELispSymbol INVISIBLE = new ELispSymbol("invisible");
    public static final ELispSymbol IPV4 = new ELispSymbol("ipv4");
    public static final ELispSymbol IPV6 = new ELispSymbol("ipv6");
    public static final ELispSymbol ISDIR = new ELispSymbol("isdir");
    public static final ELispSymbol ISO10646_1 = new ELispSymbol("iso10646-1");
    public static final ELispSymbol ISO8859_1 = new ELispSymbol("iso8859-1");
    public static final ELispSymbol ISO_2022 = new ELispSymbol("iso-2022");
    public static final ELispSymbol ISO_8859_1 = new ELispSymbol("iso-8859-1");
    public static final ELispSymbol ITALIC = new ELispSymbol("italic");
    public static final ELispSymbol IV_AUTO = new ELispSymbol("iv-auto");
    public static final ELispSymbol JA = new ELispSymbol("ja");
    public static final ELispSymbol JSON_END_OF_FILE = new ELispSymbol("json-end-of-file");
    public static final ELispSymbol JSON_ERROR = new ELispSymbol("json-error");
    public static final ELispSymbol JSON_ESCAPE_SEQUENCE_ERROR = new ELispSymbol("json-escape-sequence-error");
    public static final ELispSymbol JSON_INVALID_SURROGATE_ERROR = new ELispSymbol("json-invalid-surrogate-error");
    public static final ELispSymbol JSON_NUMBER_OUT_OF_RANGE = new ELispSymbol("json-number-out-of-range-error");
    public static final ELispSymbol JSON_OBJECT_TOO_DEEP = new ELispSymbol("json-object-too-deep");
    public static final ELispSymbol JSON_OUT_OF_MEMORY = new ELispSymbol("json-out-of-memory");
    public static final ELispSymbol JSON_PARSE_ERROR = new ELispSymbol("json-parse-error");
    public static final ELispSymbol JSON_PARSE_STRING = new ELispSymbol("json-parse-string");
    public static final ELispSymbol JSON_SERIALIZE = new ELispSymbol("json-serialize");
    public static final ELispSymbol JSON_TRAILING_CONTENT = new ELispSymbol("json-trailing-content");
    public static final ELispSymbol JSON_UTF8_DECODE_ERROR = new ELispSymbol("json-utf8-decode-error");
    public static final ELispSymbol JSON_VALUE_P = new ELispSymbol("json-value-p");
    public static final ELispSymbol JUMP = new ELispSymbol("jump");
    public static final ELispSymbol KBD_MACRO_TERMINATION_HOOK = new ELispSymbol("kbd-macro-termination-hook");
    public static final ELispSymbol KEEP_RATIO = new ELispSymbol("keep-ratio");
    public static final ELispSymbol KEY = new ELispSymbol("key");
    public static final ELispSymbol KEY_AND_VALUE = new ELispSymbol("key-and-value");
    public static final ELispSymbol KEY_OR_VALUE = new ELispSymbol("key-or-value");
    public static final ELispSymbol KEY_PARSE = new ELispSymbol("key-parse");
    public static final ELispSymbol KEY_VALID_P = new ELispSymbol("key-valid-p");
    public static final ELispSymbol KEYMAP = new ELispSymbol("keymap");
    public static final ELispSymbol KEYMAP_CANONICALIZE = new ELispSymbol("keymap-canonicalize");
    public static final ELispSymbol KEYMAPP = new ELispSymbol("keymapp");
    public static final ELispSymbol KILL_BUFFER__POSSIBLY_SAVE = new ELispSymbol("kill-buffer--possibly-save");
    public static final ELispSymbol KILL_BUFFER_HOOK = new ELispSymbol("kill-buffer-hook");
    public static final ELispSymbol KILL_BUFFER_QUERY_FUNCTIONS = new ELispSymbol("kill-buffer-query-functions");
    public static final ELispSymbol KILL_EMACS = new ELispSymbol("kill-emacs");
    public static final ELispSymbol KILL_EMACS_HOOK = new ELispSymbol("kill-emacs-hook");
    public static final ELispSymbol KILL_FORWARD_CHARS = new ELispSymbol("kill-forward-chars");
    public static final ELispSymbol KO = new ELispSymbol("ko");
    public static final ELispSymbol LAMBDA_FIXUP = new ELispSymbol("lambda-fixup");
    public static final ELispSymbol LANGUAGE_CHANGE = new ELispSymbol("language-change");
    public static final ELispSymbol LAST_ARROW_POSITION = new ELispSymbol("last-arrow-position");
    public static final ELispSymbol LAST_ARROW_STRING = new ELispSymbol("last-arrow-string");
    public static final ELispSymbol LAST_NONMENU_EVENT = new ELispSymbol("last-nonmenu-event");
    public static final ELispSymbol LATE = new ELispSymbol("late");
    public static final ELispSymbol LEFT = new ELispSymbol("left");
    public static final ELispSymbol LEFT_EDGE = new ELispSymbol("left-edge");
    public static final ELispSymbol LEFT_FRINGE = new ELispSymbol("left-fringe");
    public static final ELispSymbol LEFT_FRINGE_HELP = new ELispSymbol("left-fringe-help");
    public static final ELispSymbol LEFT_MARGIN = new ELispSymbol("left-margin");
    public static final ELispSymbol LEFT_ONLY = new ELispSymbol("left-only");
    public static final ELispSymbol LEFT_TO_RIGHT = new ELispSymbol("left-to-right");
    public static final ELispSymbol LEFTMOST = new ELispSymbol("leftmost");
    public static final ELispSymbol LET = new ELispSymbol("let");
    public static final ELispSymbol LETX = new ELispSymbol("let*");
    public static final ELispSymbol LEXICAL_BINDING = new ELispSymbol("lexical-binding");
    public static final ELispSymbol LIGHT = new ELispSymbol("light");
    public static final ELispSymbol LINE = new ELispSymbol("line");
    public static final ELispSymbol LINE_HEIGHT = new ELispSymbol("line-height");
    public static final ELispSymbol LINE_NUMBER = new ELispSymbol("line-number");
    public static final ELispSymbol LINE_NUMBER_CURRENT_LINE = new ELispSymbol("line-number-current-line");
    public static final ELispSymbol LINE_NUMBER_MAJOR_TICK = new ELispSymbol("line-number-major-tick");
    public static final ELispSymbol LINE_NUMBER_MINOR_TICK = new ELispSymbol("line-number-minor-tick");
    public static final ELispSymbol LINE_PREFIX = new ELispSymbol("line-prefix");
    public static final ELispSymbol LINE_SPACING = new ELispSymbol("line-spacing");
    public static final ELispSymbol LISP_DIRECTORY = new ELispSymbol("lisp-directory");
    public static final ELispSymbol LIST = new ELispSymbol("list");
    public static final ELispSymbol LIST_OR_VECTOR_P = new ELispSymbol("list-or-vector-p");
    public static final ELispSymbol LIST_SYSTEM_PROCESSES = new ELispSymbol("list-system-processes");
    public static final ELispSymbol LISTEN = new ELispSymbol("listen");
    public static final ELispSymbol LISTP = new ELispSymbol("listp");
    public static final ELispSymbol LITTLE = new ELispSymbol("little");
    public static final ELispSymbol LIVE = new ELispSymbol("live");
    public static final ELispSymbol LOAD = new ELispSymbol("load");
    public static final ELispSymbol LOAD_FILE_NAME = new ELispSymbol("load-file-name");
    public static final ELispSymbol LOAD_FORCE_DOC_STRINGS = new ELispSymbol("load-force-doc-strings");
    public static final ELispSymbol LOAD_IN_PROGRESS = new ELispSymbol("load-in-progress");
    public static final ELispSymbol LOAD_TIME = new ELispSymbol("load-time");
    public static final ELispSymbol LOAD_TRUE_FILE_NAME = new ELispSymbol("load-true-file-name");
    public static final ELispSymbol LOCAL = new ELispSymbol("local");
    public static final ELispSymbol LOCAL_MAP = new ELispSymbol("local-map");
    public static final ELispSymbol LOCK_FILE = new ELispSymbol("lock-file");
    public static final ELispSymbol LONG = new ELispSymbol("long");
    public static final ELispSymbol LONG_LINE_OPTIMIZATIONS_IN_COMMAND_HOOKS = new ELispSymbol("long-line-optimizations-in-command-hooks");
    public static final ELispSymbol LONG_LINE_OPTIMIZATIONS_IN_FONTIFICATION_FUNCTIONS = new ELispSymbol("long-line-optimizations-in-fontification-functions");
    public static final ELispSymbol LOSING_VALUE = new ELispSymbol("losing-value");
    public static final ELispSymbol LOWERCASE = new ELispSymbol("lowercase");
    public static final ELispSymbol LREAD_UNESCAPED_CHARACTER_LITERALS = new ELispSymbol("lread--unescaped-character-literals");
    public static final ELispSymbol M = new ELispSymbol("m");
    public static final ELispSymbol MAC = new ELispSymbol("mac");
    public static final ELispSymbol MACRO = new ELispSymbol("macro");
    public static final ELispSymbol MACROEXP__DYNVARS = new ELispSymbol("macroexp--dynvars");
    public static final ELispSymbol MACS = new ELispSymbol("macs");
    public static final ELispSymbol MAJFLT = new ELispSymbol("majflt");
    public static final ELispSymbol MAKE_CURSOR_LINE_FULLY_VISIBLE = new ELispSymbol("make-cursor-line-fully-visible");
    public static final ELispSymbol MAKE_DIRECTORY = new ELispSymbol("make-directory");
    public static final ELispSymbol MAKE_DIRECTORY_INTERNAL = new ELispSymbol("make-directory-internal");
    public static final ELispSymbol MAKE_FRAME_VISIBLE = new ELispSymbol("make-frame-visible");
    public static final ELispSymbol MAKE_INITIAL_MINIBUFFER_FRAME = new ELispSymbol("make-initial-minibuffer-frame");
    public static final ELispSymbol MAKE_INVISIBLE = new ELispSymbol("make-invisible");
    public static final ELispSymbol MAKE_LOCK_FILE_NAME = new ELispSymbol("make-lock-file-name");
    public static final ELispSymbol MAKE_PROCESS = new ELispSymbol("make-process");
    public static final ELispSymbol MAKE_SYMBOLIC_LINK = new ELispSymbol("make-symbolic-link");
    public static final ELispSymbol MAKE_WINDOW_START_VISIBLE = new ELispSymbol("make-window-start-visible");
    public static final ELispSymbol MAKUNBOUND = new ELispSymbol("makunbound");
    public static final ELispSymbol MANY = new ELispSymbol("many");
    public static final ELispSymbol MAP_KEYMAP_SORTED = new ELispSymbol("map-keymap-sorted");
    public static final ELispSymbol MARGIN = new ELispSymbol("margin");
    public static final ELispSymbol MARK_FOR_REDISPLAY = new ELispSymbol("mark-for-redisplay");
    public static final ELispSymbol MARK_INACTIVE = new ELispSymbol("mark-inactive");
    public static final ELispSymbol MARKER = new ELispSymbol("marker");
    public static final ELispSymbol MARKERP = new ELispSymbol("markerp");
    public static final ELispSymbol MAXIMIZED = new ELispSymbol("maximized");
    public static final ELispSymbol MD5 = new ELispSymbol("md5");
    public static final ELispSymbol MEDIUM = new ELispSymbol("medium");
    public static final ELispSymbol MEMORY_INFO = new ELispSymbol("memory-info");
    public static final ELispSymbol MENU = new ELispSymbol("menu");
    public static final ELispSymbol MENU_BAR = new ELispSymbol("menu-bar");
    public static final ELispSymbol MENU_BAR_EXTERNAL = new ELispSymbol("menu-bar-external");
    public static final ELispSymbol MENU_BAR_LINES = new ELispSymbol("menu-bar-lines");
    public static final ELispSymbol MENU_BAR_SIZE = new ELispSymbol("menu-bar-size");
    public static final ELispSymbol MENU_BAR_UPDATE_HOOK = new ELispSymbol("menu-bar-update-hook");
    public static final ELispSymbol MENU_ENABLE = new ELispSymbol("menu-enable");
    public static final ELispSymbol MENU_ITEM = new ELispSymbol("menu-item");
    public static final ELispSymbol MESSAGE = new ELispSymbol("message");
    public static final ELispSymbol MESSAGES_BUFFER_MODE = new ELispSymbol("messages-buffer-mode");
    public static final ELispSymbol METADATA = new ELispSymbol("metadata");
    public static final ELispSymbol MIN_HEIGHT = new ELispSymbol("min-height");
    public static final ELispSymbol MIN_WIDTH = new ELispSymbol("min-width");
    public static final ELispSymbol MINFLT = new ELispSymbol("minflt");
    public static final ELispSymbol MINIBUFFER = new ELispSymbol("minibuffer");
    public static final ELispSymbol MINIBUFFER_COMPLETING_FILE_NAME = new ELispSymbol("minibuffer-completing-file-name");
    public static final ELispSymbol MINIBUFFER_COMPLETION_TABLE = new ELispSymbol("minibuffer-completion-table");
    public static final ELispSymbol MINIBUFFER_DEFAULT = new ELispSymbol("minibuffer-default");
    public static final ELispSymbol MINIBUFFER_EXIT = new ELispSymbol("minibuffer-exit");
    public static final ELispSymbol MINIBUFFER_EXIT_HOOK = new ELispSymbol("minibuffer-exit-hook");
    public static final ELispSymbol MINIBUFFER_FOLLOWS_SELECTED_FRAME = new ELispSymbol("minibuffer-follows-selected-frame");
    public static final ELispSymbol MINIBUFFER_HISTORY = new ELispSymbol("minibuffer-history");
    public static final ELispSymbol MINIBUFFER_INACTIVE_MODE = new ELispSymbol("minibuffer-inactive-mode");
    public static final ELispSymbol MINIBUFFER_MODE = new ELispSymbol("minibuffer-mode");
    public static final ELispSymbol MINIBUFFER_PROMPT = new ELispSymbol("minibuffer-prompt");
    public static final ELispSymbol MINIBUFFER_QUIT = new ELispSymbol("minibuffer-quit");
    public static final ELispSymbol MINIBUFFER_QUIT_RECURSIVE_EDIT = new ELispSymbol("minibuffer-quit-recursive-edit");
    public static final ELispSymbol MINIBUFFER_SETUP_HOOK = new ELispSymbol("minibuffer-setup-hook");
    public static final ELispSymbol MINUS = new ELispSymbol("-");
    public static final ELispSymbol MISSING = new ELispSymbol("missing");
    public static final ELispSymbol MISSING_MODULE_INIT_FUNCTION = new ELispSymbol("missing-module-init-function");
    public static final ELispSymbol MM_SIZE = new ELispSymbol("mm-size");
    public static final ELispSymbol MODE_CLASS = new ELispSymbol("mode-class");
    public static final ELispSymbol MODE_LINE = new ELispSymbol("mode-line");
    public static final ELispSymbol MODE_LINE_ACTIVE = new ELispSymbol("mode-line-active");
    public static final ELispSymbol MODE_LINE_DEFAULT_HELP_ECHO = new ELispSymbol("mode-line-default-help-echo");
    public static final ELispSymbol MODE_LINE_FORMAT = new ELispSymbol("mode-line-format");
    public static final ELispSymbol MODE_LINE_INACTIVE = new ELispSymbol("mode-line-inactive");
    public static final ELispSymbol MODELINE = new ELispSymbol("modeline");
    public static final ELispSymbol MODIFICATION_HOOKS = new ELispSymbol("modification-hooks");
    public static final ELispSymbol MODIFIER_CACHE = new ELispSymbol("modifier-cache");
    public static final ELispSymbol MODIFY = new ELispSymbol("modify");
    public static final ELispSymbol MODULE_FUNCTION = new ELispSymbol("module-function");
    public static final ELispSymbol MODULE_FUNCTION_P = new ELispSymbol("module-function-p");
    public static final ELispSymbol MODULE_INIT_FAILED = new ELispSymbol("module-init-failed");
    public static final ELispSymbol MODULE_LOAD_FAILED = new ELispSymbol("module-load-failed");
    public static final ELispSymbol MODULE_NOT_GPL_COMPATIBLE = new ELispSymbol("module-not-gpl-compatible");
    public static final ELispSymbol MODULE_OPEN_FAILED = new ELispSymbol("module-open-failed");
    public static final ELispSymbol MONTHS = new ELispSymbol("months");
    public static final ELispSymbol MOUSE = new ELispSymbol("mouse");
    public static final ELispSymbol MOUSE_CLICK = new ELispSymbol("mouse-click");
    public static final ELispSymbol MOUSE_COLOR = new ELispSymbol("mouse-color");
    public static final ELispSymbol MOUSE_FACE = new ELispSymbol("mouse-face");
    public static final ELispSymbol MOUSE_FIXUP_HELP_MESSAGE = new ELispSymbol("mouse-fixup-help-message");
    public static final ELispSymbol MOUSE_LEAVE_BUFFER_HOOK = new ELispSymbol("mouse-leave-buffer-hook");
    public static final ELispSymbol MOUSE_MOVEMENT = new ELispSymbol("mouse-movement");
    public static final ELispSymbol MOUSE_WHEEL_FRAME = new ELispSymbol("mouse-wheel-frame");
    public static final ELispSymbol MOVE = new ELispSymbol("move");
    public static final ELispSymbol MOVE_FRAME = new ELispSymbol("move-frame");
    public static final ELispSymbol MOVE_SELF = new ELispSymbol("move-self");
    public static final ELispSymbol MOVE_TOOLBAR = new ELispSymbol("move-toolbar");
    public static final ELispSymbol MOVED_FROM = new ELispSymbol("moved-from");
    public static final ELispSymbol MOVED_TO = new ELispSymbol("moved-to");
    public static final ELispSymbol MUTEX = new ELispSymbol("mutex");
    public static final ELispSymbol MUTEXP = new ELispSymbol("mutexp");
    public static final ELispSymbol NAME = new ELispSymbol("name");
    public static final ELispSymbol NAMED = new ELispSymbol("named");
    public static final ELispSymbol NATIVE__COMPILE_ASYNC = new ELispSymbol("native--compile-async");
    public static final ELispSymbol NATIVE_COMP_COMPILER_OPTIONS = new ELispSymbol("native-comp-compiler-options");
    public static final ELispSymbol NATIVE_COMP_DEBUG = new ELispSymbol("native-comp-debug");
    public static final ELispSymbol NATIVE_COMP_DRIVER_OPTIONS = new ELispSymbol("native-comp-driver-options");
    public static final ELispSymbol NATIVE_COMP_FUNCTION = new ELispSymbol("native-comp-function");
    public static final ELispSymbol NATIVE_COMP_SPEED = new ELispSymbol("native-comp-speed");
    public static final ELispSymbol NATIVE_COMP_UNIT = new ELispSymbol("native-comp-unit");
    public static final ELispSymbol NATIVE_COMP_WARNING_ON_MISSING_SOURCE = new ELispSymbol("native-comp-warning-on-missing-source");
    public static final ELispSymbol NATIVE_COMPILER = new ELispSymbol("native-compiler");
    public static final ELispSymbol NATIVE_COMPILER_ERROR = new ELispSymbol("native-compiler-error");
    public static final ELispSymbol NATIVE_EDGES = new ELispSymbol("native-edges");
    public static final ELispSymbol NATIVE_ICE = new ELispSymbol("native-ice");
    public static final ELispSymbol NATIVE_LISP_FILE_INCONSISTENT = new ELispSymbol("native-lisp-file-inconsistent");
    public static final ELispSymbol NATIVE_LISP_LOAD_FAILED = new ELispSymbol("native-lisp-load-failed");
    public static final ELispSymbol NATIVE_LISP_WRONG_RELOC = new ELispSymbol("native-lisp-wrong-reloc");
    public static final ELispSymbol NATNUMP = new ELispSymbol("natnump");
    public static final ELispSymbol NEGATE = new ELispSymbol("negate");
    public static final ELispSymbol NETWORK = new ELispSymbol("network");
    public static final ELispSymbol NHDRAG = new ELispSymbol("nhdrag");
    public static final ELispSymbol NICE = new ELispSymbol("nice");
    public static final ELispSymbol NO_ACCEPT_FOCUS = new ELispSymbol("no-accept-focus");
    public static final ELispSymbol NO_CATCH = new ELispSymbol("no-catch");
    public static final ELispSymbol NO_CONVERSION = new ELispSymbol("no-conversion");
    public static final ELispSymbol NO_FOCUS_ON_MAP = new ELispSymbol("no-focus-on-map");
    public static final ELispSymbol NO_OTHER_FRAME = new ELispSymbol("no-other-frame");
    public static final ELispSymbol NO_OTHER_WINDOW = new ELispSymbol("no-other-window");
    public static final ELispSymbol NO_RECORD = new ELispSymbol("no-record");
    public static final ELispSymbol NO_SELF_INSERT = new ELispSymbol("no-self-insert");
    public static final ELispSymbol NO_SPECIAL_GLYPHS = new ELispSymbol("no-special-glyphs");
    public static final ELispSymbol NOBREAK_HYPHEN = new ELispSymbol("nobreak-hyphen");
    public static final ELispSymbol NOBREAK_SPACE = new ELispSymbol("nobreak-space");
    public static final ELispSymbol NOELISP = new ELispSymbol("noelisp");
    public static final ELispSymbol NON_ASCII = new ELispSymbol("non-ascii");
    public static final ELispSymbol NON_KEY_EVENT = new ELispSymbol("non-key-event");
    public static final ELispSymbol NONE = new ELispSymbol("none");
    public static final ELispSymbol NORMAL = new ELispSymbol("normal");
    public static final ELispSymbol NOT = new ELispSymbol("not");
    public static final ELispSymbol NOT_FOUND = new ELispSymbol("not-found");
    public static final ELispSymbol NS = new ELispSymbol("ns");
    public static final ELispSymbol NS_APPEARANCE = new ELispSymbol("ns-appearance");
    public static final ELispSymbol NS_PARSE_GEOMETRY = new ELispSymbol("ns-parse-geometry");
    public static final ELispSymbol NS_TRANSPARENT_TITLEBAR = new ELispSymbol("ns-transparent-titlebar");
    public static final ELispSymbol NS_UNPUT_WORKING_TEXT = new ELispSymbol("ns-unput-working-text");
    public static final ELispSymbol NSM_VERIFY_CONNECTION = new ELispSymbol("nsm-verify-connection");
    public static final ELispSymbol NTH = new ELispSymbol("nth");
    public static final ELispSymbol NULL = new ELispSymbol("null");
    public static final ELispSymbol NUMBER_OR_MARKER_P = new ELispSymbol("number-or-marker-p");
    public static final ELispSymbol NUMBERP = new ELispSymbol("numberp");
    public static final ELispSymbol NUMERIC = new ELispSymbol("numeric");
    public static final ELispSymbol OBARRAY = new ELispSymbol("obarray");
    public static final ELispSymbol OBARRAY_CACHE = new ELispSymbol("obarray-cache");
    public static final ELispSymbol OBARRAYP = new ELispSymbol("obarrayp");
    public static final ELispSymbol OBJECT = new ELispSymbol("object");
    public static final ELispSymbol OBLIQUE = new ELispSymbol("oblique");
    public static final ELispSymbol OCLOSURE_INTERACTIVE_FORM = new ELispSymbol("oclosure-interactive-form");
    public static final ELispSymbol ODD = new ELispSymbol("odd");
    public static final ELispSymbol ONLY = new ELispSymbol("only");
    public static final ELispSymbol ONLYDIR = new ELispSymbol("onlydir");
    public static final ELispSymbol OPEN = new ELispSymbol("open");
    public static final ELispSymbol OPEN_NETWORK_STREAM = new ELispSymbol("open-network-stream");
    public static final ELispSymbol OPENTYPE = new ELispSymbol("opentype");
    public static final ELispSymbol OPERATIONS = new ELispSymbol("operations");
    public static final ELispSymbol OR = new ELispSymbol("or");
    public static final ELispSymbol OUTDATED = new ELispSymbol("outdated");
    public static final ELispSymbol OUTER_BORDER_WIDTH = new ELispSymbol("outer-border-width");
    public static final ELispSymbol OUTER_EDGES = new ELispSymbol("outer-edges");
    public static final ELispSymbol OUTER_POSITION = new ELispSymbol("outer-position");
    public static final ELispSymbol OUTER_SIZE = new ELispSymbol("outer-size");
    public static final ELispSymbol OUTER_WINDOW_ID = new ELispSymbol("outer-window-id");
    public static final ELispSymbol OUTERMOST_RESTRICTION = new ELispSymbol("outermost-restriction");
    public static final ELispSymbol OVERFLOW_ERROR = new ELispSymbol("overflow-error");
    public static final ELispSymbol OVERLAY = new ELispSymbol("overlay");
    public static final ELispSymbol OVERLAY_ARROW_BITMAP = new ELispSymbol("overlay-arrow-bitmap");
    public static final ELispSymbol OVERLAY_ARROW_STRING = new ELispSymbol("overlay-arrow-string");
    public static final ELispSymbol OVERLAYP = new ELispSymbol("overlayp");
    public static final ELispSymbol OVERRIDE_REDIRECT = new ELispSymbol("override-redirect");
    public static final ELispSymbol OVERRIDING_LOCAL_MAP = new ELispSymbol("overriding-local-map");
    public static final ELispSymbol OVERRIDING_PLIST_ENVIRONMENT = new ELispSymbol("overriding-plist-environment");
    public static final ELispSymbol OVERRIDING_TERMINAL_LOCAL_MAP = new ELispSymbol("overriding-terminal-local-map");
    public static final ELispSymbol OVERWRITE_MODE = new ELispSymbol("overwrite-mode");
    public static final ELispSymbol OVERWRITE_MODE_BINARY = new ELispSymbol("overwrite-mode-binary");
    public static final ELispSymbol P = new ELispSymbol("p");
    public static final ELispSymbol PAPER = new ELispSymbol("paper");
    public static final ELispSymbol PARENT_FRAME = new ELispSymbol("parent-frame");
    public static final ELispSymbol PARENT_ID = new ELispSymbol("parent-id");
    public static final ELispSymbol PC = new ELispSymbol("pc");
    public static final ELispSymbol PCPU = new ELispSymbol("pcpu");
    public static final ELispSymbol PERMANENT_LOCAL = new ELispSymbol("permanent-local");
    public static final ELispSymbol PERMANENT_LOCAL_HOOK = new ELispSymbol("permanent-local-hook");
    public static final ELispSymbol PERMISSION_DENIED = new ELispSymbol("permission-denied");
    public static final ELispSymbol PGRP = new ELispSymbol("pgrp");
    public static final ELispSymbol PGTK = new ELispSymbol("pgtk");
    public static final ELispSymbol PHI = new ELispSymbol("phi");
    public static final ELispSymbol PINCH = new ELispSymbol("pinch");
    public static final ELispSymbol PIPE = new ELispSymbol("pipe");
    public static final ELispSymbol PIPE_PROCESS_P = new ELispSymbol("pipe-process-p");
    public static final ELispSymbol PLAY_SOUND_FUNCTIONS = new ELispSymbol("play-sound-functions");
    public static final ELispSymbol PLIST = new ELispSymbol("plist");
    public static final ELispSymbol PLISTP = new ELispSymbol("plistp");
    public static final ELispSymbol PLUS = new ELispSymbol("+");
    public static final ELispSymbol PMEM = new ELispSymbol("pmem");
    public static final ELispSymbol POINT_ENTERED = new ELispSymbol("point-entered");
    public static final ELispSymbol POINT_LEFT = new ELispSymbol("point-left");
    public static final ELispSymbol POINTER = new ELispSymbol("pointer");
    public static final ELispSymbol POLLING_PERIOD = new ELispSymbol("polling-period");
    public static final ELispSymbol POLY = new ELispSymbol("poly");
    public static final ELispSymbol POP_HANDLER = new ELispSymbol("pop-handler");
    public static final ELispSymbol POSITION = new ELispSymbol("position");
    public static final ELispSymbol POST_COMMAND_HOOK = new ELispSymbol("post-command-hook");
    public static final ELispSymbol POST_GC_HOOK = new ELispSymbol("post-gc-hook");
    public static final ELispSymbol POST_SELECT_REGION_HOOK = new ELispSymbol("post-select-region-hook");
    public static final ELispSymbol POST_SELF_INSERT_HOOK = new ELispSymbol("post-self-insert-hook");
    public static final ELispSymbol PPID = new ELispSymbol("ppid");
    public static final ELispSymbol PRE_COMMAND_HOOK = new ELispSymbol("pre-command-hook");
    public static final ELispSymbol PREEDIT_TEXT = new ELispSymbol("preedit-text");
    public static final ELispSymbol PRESSED_BUTTON = new ELispSymbol("pressed-button");
    public static final ELispSymbol PRI = new ELispSymbol("pri");
    public static final ELispSymbol PRIMITIVE_FUNCTION = new ELispSymbol("primitive-function");
    public static final ELispSymbol PRINC = new ELispSymbol("princ");
    public static final ELispSymbol PRINT__UNREADABLE_CALLBACK_BUFFER = new ELispSymbol("print--unreadable-callback-buffer");
    public static final ELispSymbol PRINT_ESCAPE_MULTIBYTE = new ELispSymbol("print-escape-multibyte");
    public static final ELispSymbol PRINT_ESCAPE_NONASCII = new ELispSymbol("print-escape-nonascii");
    public static final ELispSymbol PRINT_SYMBOLS_BARE = new ELispSymbol("print-symbols-bare");
    public static final ELispSymbol PRINT_UNREADABLE_FUNCTION = new ELispSymbol("print-unreadable-function");
    public static final ELispSymbol PRIORITY = new ELispSymbol("priority");
    public static final ELispSymbol PROCESS = new ELispSymbol("process");
    public static final ELispSymbol PROCESS_ATTRIBUTES = new ELispSymbol("process-attributes");
    public static final ELispSymbol PROCESSP = new ELispSymbol("processp");
    public static final ELispSymbol PROGN = new ELispSymbol("progn");
    public static final ELispSymbol PROPERTIZE = new ELispSymbol("propertize");
    public static final ELispSymbol PROTECTED_FIELD = new ELispSymbol("protected-field");
    public static final ELispSymbol PROVIDE = new ELispSymbol("provide");
    public static final ELispSymbol PTY = new ELispSymbol("pty");
    public static final ELispSymbol PURE = new ELispSymbol("pure");
    public static final ELispSymbol PURECOPY = new ELispSymbol("purecopy");
    public static final ELispSymbol PUSH_HANDLER = new ELispSymbol("push-handler");
    public static final ELispSymbol PUSH_WINDOW_BUFFER_ONTO_PREV = new ELispSymbol("push-window-buffer-onto-prev");
    public static final ELispSymbol Q_OVERFLOW = new ELispSymbol("q-overflow");
    public static final ELispSymbol QUIT = new ELispSymbol("quit");
    public static final ELispSymbol QUIT_RESTORE = new ELispSymbol("quit-restore");
    public static final ELispSymbol QUIT_RESTORE_PREV = new ELispSymbol("quit-restore-prev");
    public static final ELispSymbol QUOTE = new ELispSymbol("quote");
    public static final ELispSymbol RAISE = new ELispSymbol("raise");
    public static final ELispSymbol RANGE = new ELispSymbol("range");
    public static final ELispSymbol RANGE_ERROR = new ELispSymbol("range-error");
    public static final ELispSymbol RATIO = new ELispSymbol("ratio");
    public static final ELispSymbol RAW_TEXT = new ELispSymbol("raw-text");
    public static final ELispSymbol READ = new ELispSymbol("read");
    public static final ELispSymbol READ_CHAR = new ELispSymbol("read-char");
    public static final ELispSymbol READ_FILE_NAME = new ELispSymbol("read-file-name");
    public static final ELispSymbol READ_MINIBUFFER = new ELispSymbol("read-minibuffer");
    public static final ELispSymbol READ_NUMBER = new ELispSymbol("read-number");
    public static final ELispSymbol READ_ONLY = new ELispSymbol("read-only");
    public static final ELispSymbol REAL = new ELispSymbol("real");
    public static final ELispSymbol REAL_THIS_COMMAND = new ELispSymbol("real-this-command");
    public static final ELispSymbol REAR_NONSTICKY = new ELispSymbol("rear-nonsticky");
    public static final ELispSymbol RECORD = new ELispSymbol("record");
    public static final ELispSymbol RECORD_UNWIND_CURRENT_BUFFER = new ELispSymbol("record_unwind_current_buffer");
    public static final ELispSymbol RECORD_UNWIND_PROTECT_EXCURSION = new ELispSymbol("record_unwind_protect_excursion");
    public static final ELispSymbol RECORD_WINDOW_BUFFER = new ELispSymbol("record-window-buffer");
    public static final ELispSymbol RECORDP = new ELispSymbol("recordp");
    public static final ELispSymbol RECT = new ELispSymbol("rect");
    public static final ELispSymbol RECURSION_ERROR = new ELispSymbol("recursion-error");
    public static final ELispSymbol REDISPLAY_DONT_PAUSE = new ELispSymbol("redisplay-dont-pause");
    public static final ELispSymbol REDISPLAY_INTERNAL_XC_FUNCTIONX = new ELispSymbol("redisplay_internal (C function)");
    public static final ELispSymbol RELATIVE = new ELispSymbol("relative");
    public static final ELispSymbol RELEASED_BUTTON = new ELispSymbol("released-button");
    public static final ELispSymbol REMAP = new ELispSymbol("remap");
    public static final ELispSymbol REMOTE_FILE_ERROR = new ELispSymbol("remote-file-error");
    public static final ELispSymbol RENAME_AUTO_SAVE_FILE = new ELispSymbol("rename-auto-save-file");
    public static final ELispSymbol RENAME_FILE = new ELispSymbol("rename-file");
    public static final ELispSymbol REPLACE_BUFFER_IN_WINDOWS = new ELispSymbol("replace-buffer-in-windows");
    public static final ELispSymbol REQUIRE = new ELispSymbol("require");
    public static final ELispSymbol RESET = new ELispSymbol("reset");
    public static final ELispSymbol RESUME_TTY_FUNCTIONS = new ELispSymbol("resume-tty-functions");
    public static final ELispSymbol RETURN = new ELispSymbol("return");
    public static final ELispSymbol RIGHT = new ELispSymbol("right");
    public static final ELispSymbol RIGHT_DIVIDER = new ELispSymbol("right-divider");
    public static final ELispSymbol RIGHT_DIVIDER_WIDTH = new ELispSymbol("right-divider-width");
    public static final ELispSymbol RIGHT_EDGE = new ELispSymbol("right-edge");
    public static final ELispSymbol RIGHT_FRINGE = new ELispSymbol("right-fringe");
    public static final ELispSymbol RIGHT_FRINGE_HELP = new ELispSymbol("right-fringe-help");
    public static final ELispSymbol RIGHT_MARGIN = new ELispSymbol("right-margin");
    public static final ELispSymbol RIGHT_TO_LEFT = new ELispSymbol("right-to-left");
    public static final ELispSymbol RIGHTMOST = new ELispSymbol("rightmost");
    public static final ELispSymbol RISKY_LOCAL_VARIABLE = new ELispSymbol("risky-local-variable");
    public static final ELispSymbol RSS = new ELispSymbol("rss");
    public static final ELispSymbol RUN = new ELispSymbol("run");
    public static final ELispSymbol RUN_HOOK_QUERY_ERROR_WITH_TIMEOUT = new ELispSymbol("run-hook-query-error-with-timeout");
    public static final ELispSymbol RUN_HOOK_WITH_ARGS = new ELispSymbol("run-hook-with-args");
    public static final ELispSymbol SAFE = new ELispSymbol("safe");
    public static final ELispSymbol SAFE_MAGIC = new ELispSymbol("safe-magic");
    public static final ELispSymbol SAVE_EXCURSION = new ELispSymbol("save-excursion");
    public static final ELispSymbol SAVE_SESSION = new ELispSymbol("save-session");
    public static final ELispSymbol SCALE_FACTOR = new ELispSymbol("scale-factor");
    public static final ELispSymbol SCAN_ERROR = new ELispSymbol("scan-error");
    public static final ELispSymbol SCRATCH = new ELispSymbol("scratch");
    public static final ELispSymbol SCREEN_GAMMA = new ELispSymbol("screen-gamma");
    public static final ELispSymbol SCROLL_BAR = new ELispSymbol("scroll-bar");
    public static final ELispSymbol SCROLL_BAR_BACKGROUND = new ELispSymbol("scroll-bar-background");
    public static final ELispSymbol SCROLL_BAR_FOREGROUND = new ELispSymbol("scroll-bar-foreground");
    public static final ELispSymbol SCROLL_BAR_HEIGHT = new ELispSymbol("scroll-bar-height");
    public static final ELispSymbol SCROLL_BAR_MOVEMENT = new ELispSymbol("scroll-bar-movement");
    public static final ELispSymbol SCROLL_BAR_WIDTH = new ELispSymbol("scroll-bar-width");
    public static final ELispSymbol SCROLL_COMMAND = new ELispSymbol("scroll-command");
    public static final ELispSymbol SCROLL_DOWN = new ELispSymbol("scroll-down");
    public static final ELispSymbol SCROLL_UP = new ELispSymbol("scroll-up");
    public static final ELispSymbol SEARCH_FAILED = new ELispSymbol("search-failed");
    public static final ELispSymbol SELECT_FRAME_SET_INPUT_FOCUS = new ELispSymbol("select-frame-set-input-focus");
    public static final ELispSymbol SELECT_WINDOW = new ELispSymbol("select-window");
    public static final ELispSymbol SELECTION_REQUEST = new ELispSymbol("selection-request");
    public static final ELispSymbol SELF_INSERT_COMMAND = new ELispSymbol("self-insert-command");
    public static final ELispSymbol SEMI_BOLD = new ELispSymbol("semi-bold");
    public static final ELispSymbol SEMI_LIGHT = new ELispSymbol("semi-light");
    public static final ELispSymbol SEQPACKET = new ELispSymbol("seqpacket");
    public static final ELispSymbol SEQUENCEP = new ELispSymbol("sequencep");
    public static final ELispSymbol SERIAL = new ELispSymbol("serial");
    public static final ELispSymbol SESS = new ELispSymbol("sess");
    public static final ELispSymbol SET = new ELispSymbol("set");
    public static final ELispSymbol SET_ARGS_TO_LOCAL = new ELispSymbol("set-args-to-local");
    public static final ELispSymbol SET_BUFFER_MULTIBYTE = new ELispSymbol("set-buffer-multibyte");
    public static final ELispSymbol SET_DEFAULT = new ELispSymbol("set-default");
    public static final ELispSymbol SET_FILE_ACL = new ELispSymbol("set-file-acl");
    public static final ELispSymbol SET_FILE_MODES = new ELispSymbol("set-file-modes");
    public static final ELispSymbol SET_FILE_SELINUX_CONTEXT = new ELispSymbol("set-file-selinux-context");
    public static final ELispSymbol SET_FILE_TIMES = new ELispSymbol("set-file-times");
    public static final ELispSymbol SET_INTERNAL = new ELispSymbol("set_internal");
    public static final ELispSymbol SET_PAR_TO_LOCAL = new ELispSymbol("set-par-to-local");
    public static final ELispSymbol SET_REST_ARGS_TO_LOCAL = new ELispSymbol("set-rest-args-to-local");
    public static final ELispSymbol SET_VISITED_FILE_MODTIME = new ELispSymbol("set-visited-file-modtime");
    public static final ELispSymbol SET_WINDOW_CONFIGURATION = new ELispSymbol("set_window_configuration");
    public static final ELispSymbol SETCAR = new ELispSymbol("setcar");
    public static final ELispSymbol SETCDR = new ELispSymbol("setcdr");
    public static final ELispSymbol SETIMM = new ELispSymbol("setimm");
    public static final ELispSymbol SETQ = new ELispSymbol("setq");
    public static final ELispSymbol SETTING_CONSTANT = new ELispSymbol("setting-constant");
    public static final ELispSymbol SHA1 = new ELispSymbol("sha1");
    public static final ELispSymbol SHA224 = new ELispSymbol("sha224");
    public static final ELispSymbol SHA256 = new ELispSymbol("sha256");
    public static final ELispSymbol SHA384 = new ELispSymbol("sha384");
    public static final ELispSymbol SHA512 = new ELispSymbol("sha512");
    public static final ELispSymbol SHADED = new ELispSymbol("shaded");
    public static final ELispSymbol SHIFT_JIS = new ELispSymbol("shift-jis");
    public static final ELispSymbol SIDE_EFFECT_FREE = new ELispSymbol("side-effect-free");
    public static final ELispSymbol SIGNAL = new ELispSymbol("signal");
    public static final ELispSymbol SIGNAL_PROCESS_FUNCTIONS = new ELispSymbol("signal-process-functions");
    public static final ELispSymbol SIGUSR2 = new ELispSymbol("sigusr2");
    public static final ELispSymbol SINGULARITY_ERROR = new ELispSymbol("singularity-error");
    public static final ELispSymbol SIZE = new ELispSymbol("size");
    public static final ELispSymbol SKIP_TASKBAR = new ELispSymbol("skip-taskbar");
    public static final ELispSymbol SLICE = new ELispSymbol("slice");
    public static final ELispSymbol SOUND = new ELispSymbol("sound");
    public static final ELispSymbol SOURCE = new ELispSymbol("source");
    public static final ELispSymbol SPACE = new ELispSymbol("space");
    public static final ELispSymbol SPACE_WIDTH = new ELispSymbol("space-width");
    public static final ELispSymbol SPECIAL_FORM = new ELispSymbol("special-form");
    public static final ELispSymbol SPECIAL_LOWERCASE = new ELispSymbol("special-lowercase");
    public static final ELispSymbol SPECIAL_TITLECASE = new ELispSymbol("special-titlecase");
    public static final ELispSymbol SPECIAL_UPPERCASE = new ELispSymbol("special-uppercase");
    public static final ELispSymbol SQLITE = new ELispSymbol("sqlite");
    public static final ELispSymbol SQLITE3 = new ELispSymbol("sqlite3");
    public static final ELispSymbol SQLITE_ERROR = new ELispSymbol("sqlite-error");
    public static final ELispSymbol SQLITE_LOCKED_ERROR = new ELispSymbol("sqlite-locked-error");
    public static final ELispSymbol SQLITEP = new ELispSymbol("sqlitep");
    public static final ELispSymbol STANDARD_INPUT = new ELispSymbol("standard-input");
    public static final ELispSymbol STANDARD_OUTPUT = new ELispSymbol("standard-output");
    public static final ELispSymbol START = new ELispSymbol("start");
    public static final ELispSymbol START_PROCESS = new ELispSymbol("start-process");
    public static final ELispSymbol STATE = new ELispSymbol("state");
    public static final ELispSymbol STDERR = new ELispSymbol("stderr");
    public static final ELispSymbol STDIN = new ELispSymbol("stdin");
    public static final ELispSymbol STDOUT = new ELispSymbol("stdout");
    public static final ELispSymbol STICKY = new ELispSymbol("sticky");
    public static final ELispSymbol STIME = new ELispSymbol("stime");
    public static final ELispSymbol STOP = new ELispSymbol("stop");
    public static final ELispSymbol STRAIGHT = new ELispSymbol("straight");
    public static final ELispSymbol STRING = new ELispSymbol("string");
    public static final ELispSymbol STRING_BYTES = new ELispSymbol("string-bytes");
    public static final ELispSymbol STRING_LESSP = new ELispSymbol("string-lessp");
    public static final ELispSymbol STRING_REPLACE = new ELispSymbol("string-replace");
    public static final ELispSymbol STRINGP = new ELispSymbol("stringp");
    public static final ELispSymbol STRINGS = new ELispSymbol("strings");
    public static final ELispSymbol SUB1 = new ELispSymbol("1-");
    public static final ELispSymbol SUB_CHAR_TABLE = new ELispSymbol("sub-char-table");
    public static final ELispSymbol SUBFEATURES = new ELispSymbol("subfeatures");
    public static final ELispSymbol SUBR = new ELispSymbol("subr");
    public static final ELispSymbol SUBR_NATIVE_ELISP = new ELispSymbol("subr-native-elisp");
    public static final ELispSymbol SUBRP = new ELispSymbol("subrp");
    public static final ELispSymbol SUBSTITUTE_COMMAND_KEYS = new ELispSymbol("substitute-command-keys");
    public static final ELispSymbol SUBSTITUTE_ENV_IN_FILE_NAME = new ELispSymbol("substitute-env-in-file-name");
    public static final ELispSymbol SUBSTITUTE_IN_FILE_NAME = new ELispSymbol("substitute-in-file-name");
    public static final ELispSymbol SUPPRESS_KEYMAP = new ELispSymbol("suppress-keymap");
    public static final ELispSymbol SUSPEND_HOOK = new ELispSymbol("suspend-hook");
    public static final ELispSymbol SUSPEND_RESUME_HOOK = new ELispSymbol("suspend-resume-hook");
    public static final ELispSymbol SUSPEND_TTY_FUNCTIONS = new ELispSymbol("suspend-tty-functions");
    public static final ELispSymbol SW = new ELispSymbol("sw");
    public static final ELispSymbol SWITCH_FRAME = new ELispSymbol("switch-frame");
    public static final ELispSymbol SYMBOL = new ELispSymbol("symbol");
    public static final ELispSymbol SYMBOL_ERROR = new ELispSymbol("symbol-error");
    public static final ELispSymbol SYMBOL_WITH_POS = new ELispSymbol("symbol-with-pos");
    public static final ELispSymbol SYMBOL_WITH_POS_P = new ELispSymbol("symbol-with-pos-p");
    public static final ELispSymbol SYMBOLP = new ELispSymbol("symbolp");
    public static final ELispSymbol SYMBOLS = new ELispSymbol("symbols");
    public static final ELispSymbol SYMBOLS_WITH_POS_ENABLED = new ELispSymbol("symbols-with-pos-enabled");
    public static final ELispSymbol SYNTAX_PPSS = new ELispSymbol("syntax-ppss");
    public static final ELispSymbol SYNTAX_PPSS_FLUSH_CACHE = new ELispSymbol("syntax-ppss-flush-cache");
    public static final ELispSymbol SYNTAX_TABLE = new ELispSymbol("syntax-table");
    public static final ELispSymbol SYNTAX_TABLE_P = new ELispSymbol("syntax-table-p");
    public static final ELispSymbol TAB_BAR = new ELispSymbol("tab-bar");
    public static final ELispSymbol TAB_BAR_LINES = new ELispSymbol("tab-bar-lines");
    public static final ELispSymbol TAB_BAR_SIZE = new ELispSymbol("tab-bar-size");
    public static final ELispSymbol TAB_LINE = new ELispSymbol("tab-line");
    public static final ELispSymbol TAB_LINE_FORMAT = new ELispSymbol("tab-line-format");
    public static final ELispSymbol TARGET_IDX = new ELispSymbol("target-idx");
    public static final ELispSymbol TEMP_BUFFER_SETUP_HOOK = new ELispSymbol("temp-buffer-setup-hook");
    public static final ELispSymbol TEMP_BUFFER_SHOW_HOOK = new ELispSymbol("temp-buffer-show-hook");
    public static final ELispSymbol TERMINAL = new ELispSymbol("terminal");
    public static final ELispSymbol TERMINAL_FRAME = new ELispSymbol("terminal_frame");
    public static final ELispSymbol TERMINAL_LIVE_P = new ELispSymbol("terminal-live-p");
    public static final ELispSymbol TEST = new ELispSymbol("test");
    public static final ELispSymbol TEXT = new ELispSymbol("text");
    public static final ELispSymbol TEXT_CONVERSION = new ELispSymbol("text-conversion");
    public static final ELispSymbol TEXT_IMAGE_HORIZ = new ELispSymbol("text-image-horiz");
    public static final ELispSymbol TEXT_PIXELS = new ELispSymbol("text-pixels");
    public static final ELispSymbol TEXT_READ_ONLY = new ELispSymbol("text-read-only");
    public static final ELispSymbol THCOUNT = new ELispSymbol("thcount");
    public static final ELispSymbol THIN = new ELispSymbol("thin");
    public static final ELispSymbol THIN_SPACE = new ELispSymbol("thin-space");
    public static final ELispSymbol THREAD = new ELispSymbol("thread");
    public static final ELispSymbol THREAD_EVENT = new ELispSymbol("thread-event");
    public static final ELispSymbol THREADP = new ELispSymbol("threadp");
    public static final ELispSymbol TIME = new ELispSymbol("time");
    public static final ELispSymbol TIMER_EVENT_HANDLER = new ELispSymbol("timer-event-handler");
    public static final ELispSymbol TIP_FRAME = new ELispSymbol("tip_frame");
    public static final ELispSymbol TITLE = new ELispSymbol("title");
    public static final ELispSymbol TITLE_BAR_SIZE = new ELispSymbol("title-bar-size");
    public static final ELispSymbol TITLECASE = new ELispSymbol("titlecase");
    public static final ELispSymbol TOOL_BAR = new ELispSymbol("tool-bar");
    public static final ELispSymbol TOOL_BAR_EXTERNAL = new ELispSymbol("tool-bar-external");
    public static final ELispSymbol TOOL_BAR_LINES = new ELispSymbol("tool-bar-lines");
    public static final ELispSymbol TOOL_BAR_POSITION = new ELispSymbol("tool-bar-position");
    public static final ELispSymbol TOOL_BAR_SIZE = new ELispSymbol("tool-bar-size");
    public static final ELispSymbol TOOLTIP = new ELispSymbol("tooltip");
    public static final ELispSymbol TOP = new ELispSymbol("top");
    public static final ELispSymbol TOP_EDGE = new ELispSymbol("top-edge");
    public static final ELispSymbol TOP_LEFT_CORNER = new ELispSymbol("top-left-corner");
    public static final ELispSymbol TOP_LEVEL = new ELispSymbol("top-level");
    public static final ELispSymbol TOP_ONLY = new ELispSymbol("top-only");
    public static final ELispSymbol TOP_RIGHT_CORNER = new ELispSymbol("top-right-corner");
    public static final ELispSymbol TOUCH_END = new ELispSymbol("touch-end");
    public static final ELispSymbol TOUCHSCREEN = new ELispSymbol("touchscreen");
    public static final ELispSymbol TOUCHSCREEN_BEGIN = new ELispSymbol("touchscreen-begin");
    public static final ELispSymbol TOUCHSCREEN_END = new ELispSymbol("touchscreen-end");
    public static final ELispSymbol TOUCHSCREEN_UPDATE = new ELispSymbol("touchscreen-update");
    public static final ELispSymbol TPGID = new ELispSymbol("tpgid");
    public static final ELispSymbol TRAILING_WHITESPACE = new ELispSymbol("trailing-whitespace");
    public static final ELispSymbol TRANSLATION_TABLE = new ELispSymbol("translation-table");
    public static final ELispSymbol TRANSLATION_TABLE_ID = new ELispSymbol("translation-table-id");
    public static final ELispSymbol TRAPPING_CONSTANT = new ELispSymbol("trapping-constant");
    public static final ELispSymbol TREE_SITTER = new ELispSymbol("tree-sitter");
    public static final ELispSymbol TREESIT_BUFFER_TOO_LARGE = new ELispSymbol("treesit-buffer-too-large");
    public static final ELispSymbol TREESIT_COMPILED_QUERY = new ELispSymbol("treesit-compiled-query");
    public static final ELispSymbol TREESIT_COMPILED_QUERY_P = new ELispSymbol("treesit-compiled-query-p");
    public static final ELispSymbol TREESIT_ERROR = new ELispSymbol("treesit-error");
    public static final ELispSymbol TREESIT_INVALID_PREDICATE = new ELispSymbol("treesit-invalid-predicate");
    public static final ELispSymbol TREESIT_LANGUAGE_REMAP_ALIST = new ELispSymbol("treesit-language-remap-alist");
    public static final ELispSymbol TREESIT_LOAD_LANGUAGE_ERROR = new ELispSymbol("treesit-load-language-error");
    public static final ELispSymbol TREESIT_NODE = new ELispSymbol("treesit-node");
    public static final ELispSymbol TREESIT_NODE_BUFFER_KILLED = new ELispSymbol("treesit-node-buffer-killed");
    public static final ELispSymbol TREESIT_NODE_OUTDATED = new ELispSymbol("treesit-node-outdated");
    public static final ELispSymbol TREESIT_NODE_P = new ELispSymbol("treesit-node-p");
    public static final ELispSymbol TREESIT_PARSE_ERROR = new ELispSymbol("treesit-parse-error");
    public static final ELispSymbol TREESIT_PARSER = new ELispSymbol("treesit-parser");
    public static final ELispSymbol TREESIT_PARSER_DELETED = new ELispSymbol("treesit-parser-deleted");
    public static final ELispSymbol TREESIT_PARSER_P = new ELispSymbol("treesit-parser-p");
    public static final ELispSymbol TREESIT_PATTERN_EXPAND = new ELispSymbol("treesit-pattern-expand");
    public static final ELispSymbol TREESIT_PREDICATE_NOT_FOUND = new ELispSymbol("treesit-predicate-not-found");
    public static final ELispSymbol TREESIT_QUERY_ERROR = new ELispSymbol("treesit-query-error");
    public static final ELispSymbol TREESIT_QUERY_P = new ELispSymbol("treesit-query-p");
    public static final ELispSymbol TREESIT_RANGE_INVALID = new ELispSymbol("treesit-range-invalid");
    public static final ELispSymbol TTNAME = new ELispSymbol("ttname");
    public static final ELispSymbol TTY = new ELispSymbol("tty");
    public static final ELispSymbol TTY_COLOR_ALIST = new ELispSymbol("tty-color-alist");
    public static final ELispSymbol TTY_COLOR_BY_INDEX = new ELispSymbol("tty-color-by-index");
    public static final ELispSymbol TTY_COLOR_DESC = new ELispSymbol("tty-color-desc");
    public static final ELispSymbol TTY_COLOR_MODE = new ELispSymbol("tty-color-mode");
    public static final ELispSymbol TTY_COLOR_STANDARD_VALUES = new ELispSymbol("tty-color-standard-values");
    public static final ELispSymbol TTY_DEFINED_COLOR_ALIST = new ELispSymbol("tty-defined-color-alist");
    public static final ELispSymbol TTY_MENU_DISABLED_FACE = new ELispSymbol("tty-menu-disabled-face");
    public static final ELispSymbol TTY_MENU_ENABLED_FACE = new ELispSymbol("tty-menu-enabled-face");
    public static final ELispSymbol TTY_MENU_EXIT = new ELispSymbol("tty-menu-exit");
    public static final ELispSymbol TTY_MENU_IGNORE = new ELispSymbol("tty-menu-ignore");
    public static final ELispSymbol TTY_MENU_MOUSE_MOVEMENT = new ELispSymbol("tty-menu-mouse-movement");
    public static final ELispSymbol TTY_MENU_NAVIGATION_MAP = new ELispSymbol("tty-menu-navigation-map");
    public static final ELispSymbol TTY_MENU_NEXT_ITEM = new ELispSymbol("tty-menu-next-item");
    public static final ELispSymbol TTY_MENU_NEXT_MENU = new ELispSymbol("tty-menu-next-menu");
    public static final ELispSymbol TTY_MENU_PREV_ITEM = new ELispSymbol("tty-menu-prev-item");
    public static final ELispSymbol TTY_MENU_PREV_MENU = new ELispSymbol("tty-menu-prev-menu");
    public static final ELispSymbol TTY_MENU_SELECT = new ELispSymbol("tty-menu-select");
    public static final ELispSymbol TTY_MODE_RESET_STRINGS = new ELispSymbol("tty-mode-reset-strings");
    public static final ELispSymbol TTY_MODE_SET_STRINGS = new ELispSymbol("tty-mode-set-strings");
    public static final ELispSymbol TTY_SELECT_ACTIVE_REGIONS = new ELispSymbol("tty-select-active-regions");
    public static final ELispSymbol TTY_SET_UP_INITIAL_FRAME_FACES = new ELispSymbol("tty-set-up-initial-frame-faces");
    public static final ELispSymbol TTY_TYPE = new ELispSymbol("tty-type");
    public static final ELispSymbol TYPE_MISMATCH = new ELispSymbol("type-mismatch");
    public static final ELispSymbol ULTRA_BOLD = new ELispSymbol("ultra-bold");
    public static final ELispSymbol ULTRA_HEAVY = new ELispSymbol("ultra-heavy");
    public static final ELispSymbol ULTRA_LIGHT = new ELispSymbol("ultra-light");
    public static final ELispSymbol UNDECIDED = new ELispSymbol("undecided");
    public static final ELispSymbol UNDECORATED = new ELispSymbol("undecorated");
    public static final ELispSymbol UNDEFINED = new ELispSymbol("undefined");
    public static final ELispSymbol UNDERFLOW_ERROR = new ELispSymbol("underflow-error");
    public static final ELispSymbol UNDERLINE_MINIMUM_OFFSET = new ELispSymbol("underline-minimum-offset");
    public static final ELispSymbol UNDO_AUTO__ADD_BOUNDARY = new ELispSymbol("undo-auto--add-boundary");
    public static final ELispSymbol UNDO_AUTO__LAST_BOUNDARY_CAUSE = new ELispSymbol("undo-auto--last-boundary-cause");
    public static final ELispSymbol UNDO_AUTO__THIS_COMMAND_AMALGAMATING = new ELispSymbol("undo-auto--this-command-amalgamating");
    public static final ELispSymbol UNDO_AUTO__UNDOABLE_CHANGE = new ELispSymbol("undo-auto--undoable-change");
    public static final ELispSymbol UNDO_AUTO__UNDOABLY_CHANGED_BUFFERS = new ELispSymbol("undo-auto--undoably-changed-buffers");
    public static final ELispSymbol UNDO_AUTO_AMALGAMATE = new ELispSymbol("undo-auto-amalgamate");
    public static final ELispSymbol UNEVALLED = new ELispSymbol("unevalled");
    public static final ELispSymbol UNHANDLED_FILE_NAME_DIRECTORY = new ELispSymbol("unhandled-file-name-directory");
    public static final ELispSymbol UNICODE = new ELispSymbol("unicode");
    public static final ELispSymbol UNICODE_BMP = new ELispSymbol("unicode-bmp");
    public static final ELispSymbol UNICODE_STRING_P = new ELispSymbol("unicode-string-p");
    public static final ELispSymbol UNIQUIFY__RENAME_BUFFER_ADVICE = new ELispSymbol("uniquify--rename-buffer-advice");
    public static final ELispSymbol UNIX = new ELispSymbol("unix");
    public static final ELispSymbol UNLET = new ELispSymbol("unlet");
    public static final ELispSymbol UNLOCK_FILE = new ELispSymbol("unlock-file");
    public static final ELispSymbol UNMOUNT = new ELispSymbol("unmount");
    public static final ELispSymbol UNREACHABLE = new ELispSymbol("unreachable");
    public static final ELispSymbol UNSPECIFIED = new ELispSymbol("unspecified");
    public static final ELispSymbol UNSPLITTABLE = new ELispSymbol("unsplittable");
    public static final ELispSymbol UP = new ELispSymbol("up");
    public static final ELispSymbol UPPERCASE = new ELispSymbol("uppercase");
    public static final ELispSymbol US_ASCII = new ELispSymbol("us-ascii");
    public static final ELispSymbol USE_FRAME_SYNCHRONIZATION = new ELispSymbol("use-frame-synchronization");
    public static final ELispSymbol USER = new ELispSymbol("user");
    public static final ELispSymbol USER_EMACS_DIRECTORY = new ELispSymbol("user-emacs-directory");
    public static final ELispSymbol USER_ERROR = new ELispSymbol("user-error");
    public static final ELispSymbol USER_POSITION = new ELispSymbol("user-position");
    public static final ELispSymbol USER_PTR = new ELispSymbol("user-ptr");
    public static final ELispSymbol USER_PTRP = new ELispSymbol("user-ptrp");
    public static final ELispSymbol USER_SEARCH_FAILED = new ELispSymbol("user-search-failed");
    public static final ELispSymbol USER_SIZE = new ELispSymbol("user-size");
    public static final ELispSymbol UTF_16 = new ELispSymbol("utf-16");
    public static final ELispSymbol UTF_16LE = new ELispSymbol("utf-16le");
    public static final ELispSymbol UTF_8 = new ELispSymbol("utf-8");
    public static final ELispSymbol UTF_8_EMACS = new ELispSymbol("utf-8-emacs");
    public static final ELispSymbol UTF_8_STRING_P = new ELispSymbol("utf-8-string-p");
    public static final ELispSymbol UTF_8_UNIX = new ELispSymbol("utf-8-unix");
    public static final ELispSymbol UTIME = new ELispSymbol("utime");
    public static final ELispSymbol VALUE = new ELispSymbol("value");
    public static final ELispSymbol VALUELT = new ELispSymbol("value<");
    public static final ELispSymbol VARIABLE_DOCUMENTATION = new ELispSymbol("variable-documentation");
    public static final ELispSymbol VDRAG = new ELispSymbol("vdrag");
    public static final ELispSymbol VECTOR = new ELispSymbol("vector");
    public static final ELispSymbol VECTOR_OR_CHAR_TABLE_P = new ELispSymbol("vector-or-char-table-p");
    public static final ELispSymbol VECTOR_SLOTS = new ELispSymbol("vector-slots");
    public static final ELispSymbol VECTORP = new ELispSymbol("vectorp");
    public static final ELispSymbol VECTORS = new ELispSymbol("vectors");
    public static final ELispSymbol VERIFY_VISITED_FILE_MODTIME = new ELispSymbol("verify-visited-file-modtime");
    public static final ELispSymbol VERSION_MISMATCH = new ELispSymbol("version-mismatch");
    public static final ELispSymbol VERTICAL_BORDER = new ELispSymbol("vertical-border");
    public static final ELispSymbol VERTICAL_LINE = new ELispSymbol("vertical-line");
    public static final ELispSymbol VERTICAL_SCROLL_BAR = new ELispSymbol("vertical-scroll-bar");
    public static final ELispSymbol VERTICAL_SCROLL_BARS = new ELispSymbol("vertical-scroll-bars");
    public static final ELispSymbol VISIBILITY = new ELispSymbol("visibility");
    public static final ELispSymbol VISIBLE = new ELispSymbol("visible");
    public static final ELispSymbol VISUAL = new ELispSymbol("visual");
    public static final ELispSymbol VOID_FUNCTION = new ELispSymbol("void-function");
    public static final ELispSymbol VOID_VARIABLE = new ELispSymbol("void-variable");
    public static final ELispSymbol VSIZE = new ELispSymbol("vsize");
    public static final ELispSymbol W32 = new ELispSymbol("w32");
    public static final ELispSymbol WAIT_FOR_WM = new ELispSymbol("wait-for-wm");
    public static final ELispSymbol WALL = new ELispSymbol("wall");
    public static final ELispSymbol WATCHERS = new ELispSymbol("watchers");
    public static final ELispSymbol WAVE = new ELispSymbol("wave");
    public static final ELispSymbol WEAKNESS = new ELispSymbol("weakness");
    public static final ELispSymbol WHEN = new ELispSymbol("when");
    public static final ELispSymbol WHOLENUMP = new ELispSymbol("wholenump");
    public static final ELispSymbol WIDGET_TYPE = new ELispSymbol("widget-type");
    public static final ELispSymbol WIDTH = new ELispSymbol("width");
    public static final ELispSymbol WIDTH_ONLY = new ELispSymbol("width-only");
    public static final ELispSymbol WINDOW = new ELispSymbol("window");
    public static final ELispSymbol WINDOW__PIXEL_TO_TOTAL = new ELispSymbol("window--pixel-to-total");
    public static final ELispSymbol WINDOW__RESIZE_MINI_FRAME = new ELispSymbol("window--resize-mini-frame");
    public static final ELispSymbol WINDOW__RESIZE_ROOT_WINDOW = new ELispSymbol("window--resize-root-window");
    public static final ELispSymbol WINDOW__RESIZE_ROOT_WINDOW_VERTICALLY = new ELispSymbol("window--resize-root-window-vertically");
    public static final ELispSymbol WINDOW_BUFFER_CHANGE_FUNCTIONS = new ELispSymbol("window-buffer-change-functions");
    public static final ELispSymbol WINDOW_CONFIGURATION = new ELispSymbol("window-configuration");
    public static final ELispSymbol WINDOW_CONFIGURATION_CHANGE_HOOK = new ELispSymbol("window-configuration-change-hook");
    public static final ELispSymbol WINDOW_CONFIGURATION_P = new ELispSymbol("window-configuration-p");
    public static final ELispSymbol WINDOW_DELETABLE_P = new ELispSymbol("window-deletable-p");
    public static final ELispSymbol WINDOW_DIVIDER = new ELispSymbol("window-divider");
    public static final ELispSymbol WINDOW_DIVIDER_FIRST_PIXEL = new ELispSymbol("window-divider-first-pixel");
    public static final ELispSymbol WINDOW_DIVIDER_LAST_PIXEL = new ELispSymbol("window-divider-last-pixel");
    public static final ELispSymbol WINDOW_EDGES = new ELispSymbol("window-edges");
    public static final ELispSymbol WINDOW_ID = new ELispSymbol("window-id");
    public static final ELispSymbol WINDOW_LIVE_P = new ELispSymbol("window-live-p");
    public static final ELispSymbol WINDOW_POINT_INSERTION_TYPE = new ELispSymbol("window-point-insertion-type");
    public static final ELispSymbol WINDOW_SCROLL_FUNCTIONS = new ELispSymbol("window-scroll-functions");
    public static final ELispSymbol WINDOW_SELECTION_CHANGE_FUNCTIONS = new ELispSymbol("window-selection-change-functions");
    public static final ELispSymbol WINDOW_SIZE = new ELispSymbol("window-size");
    public static final ELispSymbol WINDOW_SIZE_CHANGE_FUNCTIONS = new ELispSymbol("window-size-change-functions");
    public static final ELispSymbol WINDOW_STATE_CHANGE_FUNCTIONS = new ELispSymbol("window-state-change-functions");
    public static final ELispSymbol WINDOW_STATE_CHANGE_HOOK = new ELispSymbol("window-state-change-hook");
    public static final ELispSymbol WINDOW_VALID_P = new ELispSymbol("window-valid-p");
    public static final ELispSymbol WINDOWP = new ELispSymbol("windowp");
    public static final ELispSymbol WORKAREA = new ELispSymbol("workarea");
    public static final ELispSymbol WRAP_PREFIX = new ELispSymbol("wrap-prefix");
    public static final ELispSymbol WRITE_REGION = new ELispSymbol("write-region");
    public static final ELispSymbol WRITE_REGION_ANNOTATE_FUNCTIONS = new ELispSymbol("write-region-annotate-functions");
    public static final ELispSymbol WRONG_LENGTH_ARGUMENT = new ELispSymbol("wrong-length-argument");
    public static final ELispSymbol WRONG_NUMBER_OF_ARGUMENTS = new ELispSymbol("wrong-number-of-arguments");
    public static final ELispSymbol WRONG_REGISTER_SUBR_CALL = new ELispSymbol("wrong-register-subr-call");
    public static final ELispSymbol WRONG_TYPE_ARGUMENT = new ELispSymbol("wrong-type-argument");
    public static final ELispSymbol X = new ELispSymbol("x");
    public static final ELispSymbol X_CREATE_FRAME_1 = new ELispSymbol("x_create_frame_1");
    public static final ELispSymbol X_CREATE_FRAME_2 = new ELispSymbol("x_create_frame_2");
    public static final ELispSymbol X_FRAME_PARAMETER = new ELispSymbol("x-frame-parameter");
    public static final ELispSymbol X_PRE_POPUP_MENU_HOOK = new ELispSymbol("x-pre-popup-menu-hook");
    public static final ELispSymbol X_RESOURCE_NAME = new ELispSymbol("x-resource-name");
    public static final ELispSymbol X_SET_MENU_BAR_LINES = new ELispSymbol("x_set_menu_bar_lines");
    public static final ELispSymbol X_SET_WINDOW_SIZE_1 = new ELispSymbol("x_set_window_size_1");
    public static final ELispSymbol XG_FRAME_SET_CHAR_SIZE = new ELispSymbol("xg_frame_set_char_size");
    public static final ELispSymbol XTERM__SET_SELECTION = new ELispSymbol("xterm--set-selection");
    public static final ELispSymbol XWIDGET = new ELispSymbol("xwidget");
    public static final ELispSymbol XWIDGET_DISPLAY_EVENT = new ELispSymbol("xwidget-display-event");
    public static final ELispSymbol XWIDGET_EVENT = new ELispSymbol("xwidget-event");
    public static final ELispSymbol XWIDGET_VIEW = new ELispSymbol("xwidget-view");
    public static final ELispSymbol Y_OR_N_P = new ELispSymbol("y-or-n-p");
    public static final ELispSymbol YES_OR_NO_P = new ELispSymbol("yes-or-no-p");
    public static final ELispSymbol YES_OR_NO_P_HISTORY = new ELispSymbol("yes-or-no-p-history");
    public static final ELispSymbol Z_GROUP = new ELispSymbol("z-group");
    public static final ELispSymbol ZERO_WIDTH = new ELispSymbol("zero-width");
    private static ELispSymbol[] allSymbols() {
        return new ELispSymbol[] {
            NIL,
            T,
            UNBOUND,
            ERROR,
            LAMBDA,
            AEAD_CIPHERS,
            AUTOMATIC_GC,
            CADSTYLE,
            CADVERTISED_BINDING,
            CALIGN_TO,
            CANCHOR,
            CANTIALIAS,
            CARRAY,
            CARRAY_TYPE,
            CASCII_COMPATIBLE_P,
            CAUTHORIZABLE,
            CAVGWIDTH,
            CBACKGROUND,
            CBOLD,
            CBOOLEAN,
            CBOX,
            CBUFFER,
            CBUTTON,
            CBYTE,
            CBYTESIZE,
            CCATEGORY,
            CCERTIFICATE,
            CCERTIFICATE_ID,
            CCERTIFICATE_SECURITY_LEVEL,
            CCERTIFICATES,
            CCIPHER,
            CCIPHER_AEAD_CAPABLE,
            CCIPHER_BLOCKSIZE,
            CCIPHER_ID,
            CCIPHER_IVSIZE,
            CCIPHER_KEYSIZE,
            CCIPHER_TAGSIZE,
            CCODING,
            CCOLOR,
            CCOMBINING_CAPABILITY,
            CCOMMAND,
            CCOMPLETE_NEGOTIATION,
            CCOMPRESSION,
            CCONNECTION_TYPE,
            CCRLFILES,
            CDATA,
            CDEBUG_ON_EXIT,
            CDECODE_TRANSLATION_TABLE,
            CDEFAULT_CHAR,
            CDEVICE,
            CDICT_ENTRY,
            CDIFFIE_HELLMAN_PRIME_BITS,
            CDIGEST_ALGORITHM_ID,
            CDIGEST_ALGORITHM_LENGTH,
            CDISTANT_FOREGROUND,
            CDOCUMENTATION,
            CDOUBLE,
            CDPI,
            CEMERGENCY,
            CENABLE,
            CENCODE_TRANSLATION_TABLE,
            CENCRYPT_THEN_MAC,
            CEQUAL,
            CERROR,
            CEVAL,
            CEXPIRED,
            CEXTEND,
            CFALSE,
            CFALSE_OBJECT,
            CFAMILY,
            CFILE,
            CFILE_HANDLER,
            CFILTER,
            CFILTERED,
            CFLAGS,
            CFLOWCONTROL,
            CFONT,
            CFONT_ENTITY,
            CFONTSET,
            CFOREGROUND,
            CFOUNDRY,
            CHEIGHT,
            CHELP,
            CHOST,
            CHOSTNAME,
            CIGNORE_DEFFACE,
            CIMAGE,
            CIN_PLACE,
            CINHERIT,
            CINSECURE,
            CINT16,
            CINT32,
            CINT64,
            CINVALID,
            CINVALID_OCSP_STATUS,
            CINVERSE_VIDEO,
            CISSUER,
            CISSUER_UNIQUE_ID,
            CITALIC,
            CKEY,
            CKEY_EXCHANGE,
            CKEY_SEQUENCE,
            CKEYLIST,
            CKEYS,
            CLABEL,
            CLANG,
            CLESSP,
            CLIENTHELLO_PADDING,
            CLINE_WIDTH,
            CLOCAL,
            CLOG,
            CLOGLEVEL,
            CMAC,
            CMAC_ALGORITHM_ID,
            CMAC_ALGORITHM_KEYSIZE,
            CMAC_ALGORITHM_LENGTH,
            CMAC_ALGORITHM_NONCESIZE,
            CMAP,
            CMATCH,
            CMETHOD,
            CMIN_PRIME_BITS,
            CMISSING_OCSP_STATUS,
            CMNEMONIC,
            CMONITOR,
            CNAME,
            CNO_HOST_MATCH,
            CNOQUERY,
            CNOT_ACTIVATED,
            CNOT_CA,
            CNOWAIT,
            CNULL,
            CNULL_OBJECT,
            COBJECT_PATH,
            COBJECT_TYPE,
            COTF,
            COVERLINE,
            CPARITY,
            CPASS,
            CPEM,
            CPLIST,
            CPLUS,
            CPOINTER,
            CPORT,
            CPOSITION,
            CPOST_READ_CONVERSION,
            CPRE_WRITE_CONVERSION,
            CPRED,
            CPRIORITY,
            CPROCESS,
            CPROPERTIZE,
            CPROTOCOL,
            CPUBLIC_KEY_ALGORITHM,
            CPUBLIC_KEY_ID,
            CPUBLIC_KEY_ID_SHA256,
            CPURECOPY,
            CPURPOSE_MISMATCH,
            CQUESTION,
            CRADIO,
            CREGISTRY,
            CREHASH_SIZE,
            CREHASH_THRESHOLD,
            CRELATIVE_HEIGHT,
            CRELATIVE_WIDTH,
            CREMOTE,
            CREVERSE,
            CREVOCATION_DATA_ISSUED_IN_FUTURE,
            CREVOCATION_DATA_SUPERSEDED,
            CREVOKED,
            CRTL,
            CSAFE_RENEGOTIATION,
            CSCALABLE,
            CSCRIPT,
            CSELF_SIGNED,
            CSENTINEL,
            CSERIAL,
            CSERIAL_NUMBER,
            CSERVER,
            CSERVICE,
            CSESSION,
            CSESSION_PRIVATE,
            CSIGNAL,
            CSIGNATURE,
            CSIGNATURE_ALGORITHM,
            CSIGNATURE_FAILURE,
            CSIGNER_CONSTRAINTS_FAILURE,
            CSIZE,
            CSLANT,
            CSPACING,
            CSPEED,
            CSTAR,
            CSTDERR,
            CSTIPPLE,
            CSTOP,
            CSTOPBITS,
            CSTRIKE_THROUGH,
            CSTRING,
            CSTRUCT,
            CSTYLE,
            CSUBJECT,
            CSUBJECT_UNIQUE_ID,
            CSUCCESS,
            CSUMMARY,
            CSYSTEM,
            CSYSTEM_PRIVATE,
            CTEST,
            CTIMEOUT,
            CTLS_PARAMETERS,
            CTOGGLE,
            CTRUSTFILES,
            CTYPE,
            CUINT16,
            CUINT32,
            CUINT64,
            CUNDERLINE,
            CUNIX_FD,
            CUNKNOWN_CA,
            CUSE_EXTERNAL_SOCKET,
            CUSER_SPEC,
            CVALID_FROM,
            CVALID_TO,
            CVARIANT,
            CVERIFY_ERROR,
            CVERIFY_FLAGS,
            CVERSION,
            CVERT_ONLY,
            CVISIBLE,
            CVOLUME,
            CWARNINGS,
            CWEAKNESS,
            CWEIGHT,
            CWIDTH,
            CWINDOW,
            CWRAP,
            D_BUS,
            DISCARDED_SAMPLES,
            L2R,
            PRIMARY,
            R2L,
            UNKNOWN_ERROR,
            ABOVE,
            ABOVE_HANDLE,
            ABOVE_SUSPENDED,
            ACCESS,
            ACCESS_FILE,
            ACTIVATE_INPUT_METHOD,
            ACTIVATE_MARK_HOOK,
            ACTIVATE_MENUBAR_HOOK,
            ADD1,
            ADD_NAME_TO_FILE,
            ADD_TO_HISTORY,
            AFTER_CHANGE_FUNCTIONS,
            AFTER_DELETE_FRAME_FUNCTIONS,
            AFTER_HANDLE,
            AFTER_INSERT_FILE_SET_BUFFER_FILE_CODING_SYSTEM,
            AFTER_INSERT_FILE_SET_CODING,
            AFTER_PDUMP_LOAD_HOOK,
            AFTER_STRING,
            ALIST,
            ALL,
            ALL_EVENTS,
            ALLOC,
            ALPHA,
            ALPHA_BACKGROUND,
            AND_OPTIONAL,
            AND_REST,
            ANDROID,
            APPLY,
            AREF,
            ARGS,
            ARGS_OUT_OF_RANGE,
            ARITH_ERROR,
            ARRAY,
            ARRAYP,
            ARROW,
            ASCII,
            ASCII_0,
            ASCII_CHARACTER,
            ASET,
            ASSUME,
            ATTRIB,
            AUTO_COMPOSED,
            AUTO_FILL_CHARS,
            AUTO_HSCROLL_MODE,
            AUTO_LOWER,
            AUTO_RAISE,
            AUTO_SAVE,
            AUTO_SAVE_CODING,
            AUTO_SAVE_HOOK,
            AUTOLOAD,
            AUTOSAVED,
            BACKGROUND_COLOR,
            BACKGROUND_MODE,
            BACKQUOTE,
            BAR,
            BARE_SYMBOL_P,
            BEFORE_CHANGE_FUNCTIONS,
            BEFORE_HANDLE,
            BEFORE_STRING,
            BEGINNING_OF_BUFFER,
            BELOW,
            BELOW_HANDLE,
            BIG,
            BIG5,
            BIGNUM,
            BINARY,
            BITMAP_SPEC_P,
            BLACK,
            BOLD,
            BOOK,
            BOOL_VECTOR,
            BOOL_VECTOR_P,
            BOOLEAN,
            BOOLEANP,
            BORDER,
            BORDER_COLOR,
            BORDER_WIDTH,
            BOTH,
            BOTH_HORIZ,
            BOTTOM,
            BOTTOM_DIVIDER,
            BOTTOM_DIVIDER_WIDTH,
            BOTTOM_EDGE,
            BOTTOM_LEFT_CORNER,
            BOTTOM_RIGHT_CORNER,
            BOUNDARY,
            BOUNDS,
            BOX,
            BUFFER,
            BUFFER_ACCESS_FONTIFY_FUNCTIONS,
            BUFFER_FILE_CODING_SYSTEM,
            BUFFER_FILE_NAME,
            BUFFER_FILE_NUMBER,
            BUFFER_LIST,
            BUFFER_LIST_UPDATE_HOOK,
            BUFFER_NAME_HISTORY,
            BUFFER_OR_STRING_P,
            BUFFER_POSITION,
            BUFFER_PREDICATE,
            BUFFER_READ_ONLY,
            BUFFER_SAVE_WITHOUT_QUERY,
            BUFFER_STALE_FUNCTION,
            BUFFER_UNDO_LIST,
            BUFFERP,
            BUFFERS,
            BURIED_BUFFER_LIST,
            BYTE_CODE_FUNCTION,
            BYTE_CODE_FUNCTION_P,
            BYTE_CODE_METER,
            BYTE_RUN_UNESCAPED_CHARACTER_LITERALS_WARNING,
            C,
            CALL,
            CALL_PROCESS,
            CALL_PROCESS_REGION,
            CALLREF,
            CANONICAL_COMBINING_CLASS,
            CAR,
            CAR_LESS_THAN_CAR,
            CASE_FOLD_SEARCH,
            CASE_SYMBOLS_AS_WORDS,
            CASE_TABLE,
            CASE_TABLE_P,
            CATCHER,
            CATEGORY,
            CATEGORY_TABLE,
            CATEGORY_TABLE_P,
            CATEGORYP,
            CATEGORYSETP,
            CCL,
            CCL_PROGRAM_IDX,
            CCLP,
            CDR,
            CEILING,
            CENTER,
            CHANGE_FRAME_SIZE,
            CHANGE_MAJOR_MODE_HOOK,
            CHAR_CODE_PROPERTY_TABLE,
            CHAR_FROM_NAME,
            CHAR_OR_STRING_P,
            CHAR_SCRIPT_TABLE,
            CHAR_TABLE,
            CHAR_TABLE_EXTRA_SLOTS,
            CHAR_TABLE_P,
            CHARACTERP,
            CHARSET,
            CHARSETP,
            CHILD_FRAME_BORDER,
            CHILD_FRAME_BORDER_WIDTH,
            CHOICE,
            CIPHERS,
            CIRCLE,
            CIRCULAR_LIST,
            CLONE_INDIRECT_BUFFER_HOOK,
            CLONE_OF,
            CLOSE,
            CLOSE_NOWRITE,
            CLOSE_TAB,
            CLOSE_WRITE,
            CLOSED,
            CMAJFLT,
            CMINFLT,
            CODE_CONVERSION_MAP,
            CODE_CONVERSION_MAP_ID,
            CODESET,
            CODING,
            CODING_SYSTEM,
            CODING_SYSTEM_DEFINE_FORM,
            CODING_SYSTEM_ERROR,
            CODING_SYSTEM_FOR_WRITE,
            CODING_SYSTEM_HISTORY,
            CODING_SYSTEM_P,
            COLUMNS,
            COMM,
            COMMA,
            COMMA_AT,
            COMMAND_DEBUG_STATUS,
            COMMAND_ERROR_DEFAULT_FUNCTION,
            COMMAND_EXECUTE,
            COMMAND_HISTORY,
            COMMAND_LINE_PROCESSED,
            COMMAND_MODES,
            COMMANDP,
            COMMENT,
            COMMENT_END_CAN_BE_ESCAPED,
            COMP_LIBGCCJIT_REPRODUCER,
            COMP_MAYBE_GC_OR_QUIT,
            COMP_MVAR,
            COMP_SANITIZER_ERROR,
            COMP_SUBR_TRAMPOLINE_INSTALL,
            COMPLETING_READ_FUNCTION,
            COMPLETION_IGNORE_CASE,
            COMPOSITION,
            CONCAT,
            COND_JUMP,
            COND_JUMP_NARG_LEQ,
            CONDITION_CASE,
            CONDITION_VARIABLE,
            CONDITION_VARIABLE_P,
            CONFIG_CHANGED_EVENT,
            CONFIGURATION,
            CONNECT,
            CONS,
            CONSES,
            CONSP,
            COPY_DIRECTORY,
            COPY_FILE,
            CREATE,
            CSTIME,
            CTIME,
            CURRENT,
            CURRENT_INPUT_METHOD,
            CURRENT_KEY_REMAP_SEQUENCE,
            CURRENT_LINE,
            CURRENT_LOAD_LIST,
            CURRENT_MINIBUFFER_COMMAND,
            CURSOR,
            CURSOR_COLOR,
            CURSOR_IN_ECHO_AREA,
            CURSOR_TYPE,
            CURVE,
            CUSTOM_DELAYED_INIT_VARIABLES,
            CUSTOM_VARIABLE_HISTORY,
            CUSTOM_VARIABLE_P,
            CUTIME,
            CYCLE_SORT_FUNCTION,
            CYCLIC_FUNCTION_INDIRECTION,
            CYCLIC_VARIABLE_INDIRECTION,
            D,
            D_DEFAULT,
            D_EPHEMERAL,
            D_IMPURE,
            DASHES,
            DATA,
            DATAGRAM,
            DAYS,
            DBUS_ERROR,
            DBUS_EVENT,
            DBUS_GET_NAME_OWNER,
            DBUS_MESSAGE_INTERNAL,
            DEACTIVATE_MARK,
            DEBUG,
            DEBUG_EARLY,
            DEBUG_EARLY__HANDLER,
            DEBUG_EARLY__MUTED,
            DEBUGGER,
            DEBUGGER_MAY_CONTINUE,
            DECOMPOSED_CHARACTERS,
            DEDICATED,
            DEFALIAS_FSET_FUNCTION,
            DEFAULT,
            DEFAULT_DIRECTORY,
            DEFAULT_KEYBOARD_CODING_SYSTEM,
            DEFAULT_TERMINAL_CODING_SYSTEM,
            DEFINE_CHARSET_INTERNAL,
            DEFINE_CODING_SYSTEM_INTERNAL,
            DEFUN,
            DEFVARALIAS,
            DELAYED_WARNINGS_HOOK,
            DELETE,
            DELETE_AUTO_SAVE_FILE_IF_NECESSARY,
            DELETE_BEFORE,
            DELETE_BY_MOVING_TO_TRASH,
            DELETE_DIRECTORY,
            DELETE_FILE,
            DELETE_FILE_INTERNAL,
            DELETE_FRAME,
            DELETE_FRAME_FUNCTIONS,
            DELETE_SELF,
            DELETE_TERMINAL_FUNCTIONS,
            DELETE_WINDOW,
            DIGESTS,
            DIR_OK,
            DIRECT_CALL,
            DIRECT_CALLREF,
            DIRECTORY_FILE_NAME,
            DIRECTORY_FILES,
            DIRECTORY_FILES_AND_ATTRIBUTES,
            DISABLE_EVAL,
            DISABLED,
            DISPLAY,
            DISPLAY_BUFFER,
            DISPLAY_FILL_COLUMN_INDICATOR,
            DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER,
            DISPLAY_FILL_COLUMN_INDICATOR_COLUMN,
            DISPLAY_LINE_NUMBERS,
            DISPLAY_LINE_NUMBERS_DISABLE,
            DISPLAY_LINE_NUMBERS_OFFSET,
            DISPLAY_LINE_NUMBERS_WIDEN,
            DISPLAY_LINE_NUMBERS_WIDTH,
            DISPLAY_MONITORS_CHANGED_FUNCTIONS,
            DISPLAY_TABLE,
            DISPLAY_TYPE,
            DISPLAY_WARNING,
            DO_AFTER_LOAD_EVALUATION,
            DOMAIN_ERROR,
            DONT_CLEAR_MESSAGE,
            DONT_FOLLOW,
            DOS,
            DOTS,
            DOUBLE_LINE,
            DOWN,
            DRAG_INTERNAL_BORDER,
            DRAG_N_DROP,
            DRAG_SOURCE,
            DRAG_WITH_HEADER_LINE,
            DRAG_WITH_MODE_LINE,
            DRAG_WITH_TAB_LINE,
            DRAGGING,
            DROPPING,
            DUMP_EMACS_PORTABLE__SORT_PREDICATE,
            DUMP_EMACS_PORTABLE__SORT_PREDICATE_COPIED,
            DUMP_FILE_NAME,
            DUMPED_WITH_PDUMPER,
            ECHO_AREA_CLEAR_HOOK,
            ECHO_KEYSTROKES,
            EGID,
            EIGHT_BIT,
            ELT,
            EMACS,
            EMACS_MULE,
            EMOJI,
            EMPTY_BOX,
            ENABLE_RECURSIVE_MINIBUFFERS,
            ENCODE_TIME,
            ENCODED,
            END_OF_BUFFER,
            END_OF_FILE,
            END_SCROLL,
            END_SESSION,
            ENTRY,
            EQ,
            EQL,
            EQUAL,
            ERROR_CONDITIONS,
            ERROR_MESSAGE,
            ESCAPE_GLYPH,
            ETIME,
            EUID,
            EVAL,
            EVAL_BUFFER_LIST,
            EVAL_MINIBUFFER,
            EVAPORATE,
            EVEN,
            EVENT_KIND,
            EVENT_SYMBOL_ELEMENT_MASK,
            EVENT_SYMBOL_ELEMENTS,
            EXCESSIVE_LISP_NESTING,
            EXCESSIVE_VARIABLE_BINDING,
            EXCL,
            EXIT,
            EXPAND_ABBREV,
            EXPAND_FILE_NAME,
            EXPLICIT,
            EXPLICIT_NAME,
            EXTERNAL_BORDER_SIZE,
            EXTERNAL_DEBUGGING_OUTPUT,
            EXTRA,
            EXTRA_BOLD,
            EXTRA_LIGHT,
            F0,
            F10,
            FACE,
            FACE_ALIAS,
            FACE_NO_INHERIT,
            FACE_REMAPPING_ALIST,
            FACE_SET_AFTER_FRAME_DEFAULT,
            FAILED,
            FALSE,
            FBOUNDP,
            FEATURES,
            FETCH_HANDLER,
            FIELD,
            FILE_ACCESSIBLE_DIRECTORY_P,
            FILE_ACL,
            FILE_ALREADY_EXISTS,
            FILE_ATTRIBUTES,
            FILE_ATTRIBUTES_LESSP,
            FILE_DATE_ERROR,
            FILE_DIRECTORY_P,
            FILE_ERROR,
            FILE_EXECUTABLE_P,
            FILE_EXISTS_P,
            FILE_LOCKED_P,
            FILE_MISSING,
            FILE_MODES,
            FILE_NAME_ALL_COMPLETIONS,
            FILE_NAME_AS_DIRECTORY,
            FILE_NAME_CASE_INSENSITIVE_P,
            FILE_NAME_COMPLETION,
            FILE_NAME_DIRECTORY,
            FILE_NAME_HANDLER_ALIST,
            FILE_NAME_HISTORY,
            FILE_NAME_NONDIRECTORY,
            FILE_NEWER_THAN_FILE_P,
            FILE_NOTIFY,
            FILE_NOTIFY_ERROR,
            FILE_OFFSET,
            FILE_READABLE_P,
            FILE_REGULAR_P,
            FILE_REMOTE_P,
            FILE_SELINUX_CONTEXT,
            FILE_SYMLINK_P,
            FILE_SYSTEM_INFO,
            FILE_TRUENAME,
            FILE_WRITABLE_P,
            FILENAMEP,
            FILL_COLUMN_INDICATOR,
            FINALIZER,
            FIRST_CHANGE_HOOK,
            FIXNUM,
            FIXNUM_OR_SYMBOL_WITH_POS_P,
            FIXNUMP,
            FLAT_BUTTON,
            FLOAT,
            FLOAT_OUTPUT_FORMAT,
            FLOATP,
            FLOATS,
            FLOOR,
            FOCUS_IN,
            FOCUS_OUT,
            FONT,
            FONT_BACKEND,
            FONT_DRIVER_SUPERSEDED_BY,
            FONT_ENTITY,
            FONT_EXTRA_TYPE,
            FONT_LOCK_FACE,
            FONT_OBJECT,
            FONT_PARAMETER,
            FONT_SPEC,
            FONTIFICATION_FUNCTIONS,
            FONTIFIED,
            FOREGROUND_COLOR,
            FORMAT_ANNOTATE_FUNCTION,
            FORMAT_DECODE,
            FORMAT_PROMPT,
            FRACTION,
            FRAME,
            FRAME_EDGES,
            FRAME_LIVE_P,
            FRAME_MONITOR_ATTRIBUTES,
            FRAME_SET_BACKGROUND_MODE,
            FRAME_WINDOWS_MIN_SIZE,
            FRAMEP,
            FRAMES,
            FRINGE,
            FROM__TTY_MENU_P,
            FRONT_STICKY,
            FULL,
            FULLBOTH,
            FULLHEIGHT,
            FULLSCREEN,
            FULLWIDTH,
            FUNCALL,
            FUNCALL_INTERACTIVELY,
            FUNCTION,
            FUNCTION_DOCUMENTATION,
            FUNCTION_HISTORY,
            FUNCTION_KEY,
            FUNCTIONP,
            FUNDAMENTAL_MODE,
            GC_CONS_PERCENTAGE,
            GC_CONS_THRESHOLD,
            GCCJIT,
            GEOMETRY,
            GET_BUFFER_WINDOW_LIST,
            GET_EMACS_MULE_FILE_CHAR,
            GET_FILE_BUFFER,
            GET_FILE_CHAR,
            GET_MRU_WINDOW,
            GET_SCRATCH_BUFFER_CREATE,
            GLYPHLESS_CHAR,
            GLYPHLESS_CHAR_DISPLAY,
            GNUTLS,
            GNUTLS3,
            GNUTLS_ANON,
            GNUTLS_CODE,
            GNUTLS_E_AGAIN,
            GNUTLS_E_INTERRUPTED,
            GNUTLS_E_INVALID_SESSION,
            GNUTLS_E_NOT_READY_FOR_HANDSHAKE,
            GNUTLS_PKCS_NULL_PASSWORD,
            GNUTLS_PKCS_PBES1_DES_MD5,
            GNUTLS_PKCS_PBES2_3DES,
            GNUTLS_PKCS_PBES2_AES_128,
            GNUTLS_PKCS_PBES2_AES_192,
            GNUTLS_PKCS_PBES2_AES_256,
            GNUTLS_PKCS_PBES2_DES,
            GNUTLS_PKCS_PBES2_GOST_CPA,
            GNUTLS_PKCS_PBES2_GOST_CPB,
            GNUTLS_PKCS_PBES2_GOST_CPC,
            GNUTLS_PKCS_PBES2_GOST_CPD,
            GNUTLS_PKCS_PBES2_GOST_TC26Z,
            GNUTLS_PKCS_PKCS12_3DES,
            GNUTLS_PKCS_PKCS12_ARCFOUR,
            GNUTLS_PKCS_PKCS12_RC2_40,
            GNUTLS_PKCS_PLAIN,
            GNUTLS_TYPE_CIPHER,
            GNUTLS_TYPE_DIGEST_ALGORITHM,
            GNUTLS_TYPE_MAC_ALGORITHM,
            GNUTLS_X509PKI,
            GRAVE,
            GROUP,
            GROW_ONLY,
            GUI_FIGURE_WINDOW_SIZE,
            GUI_SET_SELECTION,
            HAIKU,
            HAND,
            HANDLE,
            HANDLE_SELECT_WINDOW,
            HANDLE_SHIFT_SELECTION,
            HANDLE_SWITCH_FRAME,
            HAS_ERROR,
            HASH_TABLE,
            HASH_TABLE_P,
            HASH_TABLE_TEST,
            HBAR,
            HDRAG,
            HEADER_LINE,
            HEADER_LINE_FORMAT,
            HEAP,
            HEAVY,
            HEIGHT,
            HEIGHT_ONLY,
            HELP__APPEND_KEYSTROKES_HELP,
            HELP__DESCRIBE_MAP_TREE,
            HELP_ECHO,
            HELP_ECHO_INHIBIT_SUBSTITUTION,
            HELP_FORM_SHOW,
            HELP_KEY_BINDING,
            HELPER_SANITIZER_ASSERT,
            HELPER_SAVE_RESTRICTION,
            HELPER_UNBIND_N,
            HELPER_UNWIND_PROTECT,
            HEX_CODE,
            HIDE,
            HOLLOW,
            HORIZONTAL_HANDLE,
            HORIZONTAL_SCROLL_BAR,
            HORIZONTAL_SCROLL_BARS,
            HOURGLASS,
            HW,
            ICON,
            ICON_LEFT,
            ICON_NAME,
            ICON_TOP,
            ICON_TYPE,
            ICONIFY_FRAME,
            ICONIFY_TOP_LEVEL,
            IDENTITY,
            IF,
            IF_REGULAR,
            IGNORE_SELF_INSERT,
            IGNORED,
            IMAGE,
            INC_ARGS,
            INHIBIT_CHANGING_MATCH_DATA,
            INHIBIT_DEBUGGER,
            INHIBIT_DOUBLE_BUFFERING,
            INHIBIT_EVAL_DURING_REDISPLAY,
            INHIBIT_FILE_NAME_OPERATION,
            INHIBIT_FREE_REALIZED_FACES,
            INHIBIT_MENUBAR_UPDATE,
            INHIBIT_MODIFICATION_HOOKS,
            INHIBIT_POINT_MOTION_HOOKS,
            INHIBIT_QUIT,
            INHIBIT_READ_ONLY,
            INHIBIT_REDISPLAY,
            INHIBITED_INTERACTION,
            INITIAL_MAJOR_MODE,
            INNER_EDGES,
            INPUT_METHOD_EXIT_ON_FIRST_CHAR,
            INPUT_METHOD_USE_ECHO_AREA,
            INSERT_BEHIND_HOOKS,
            INSERT_FILE_CONTENTS,
            INSERT_IN_FRONT_HOOKS,
            INSERTED_CHARS,
            INSUFFICIENT_SOURCE,
            INTANGIBLE,
            INTEGER,
            INTEGER_OR_MARKER_P,
            INTEGERP,
            INTERACTIVE,
            INTERACTIVE_ARGS,
            INTERACTIVE_FORM,
            INTERACTIVE_P,
            INTERNAL__SYNTAX_PROPERTIZE,
            INTERNAL_AUTO_FILL,
            INTERNAL_BORDER,
            INTERNAL_BORDER_WIDTH,
            INTERNAL_COMPLETE_BUFFER,
            INTERNAL_DEFAULT_INTERRUPT_PROCESS,
            INTERNAL_DEFAULT_PROCESS_FILTER,
            INTERNAL_DEFAULT_PROCESS_SENTINEL,
            INTERNAL_DEFAULT_SIGNAL_PROCESS,
            INTERNAL_ECHO_KEYSTROKES_PREFIX,
            INTERNAL_INTERPRETER_ENVIRONMENT,
            INTERNAL_MACROEXPAND_FOR_LOAD,
            INTERNAL_TIMER_START_IDLE,
            INTERNAL_WHEN_ENTERED_DEBUGGER,
            INTERPRETED_FUNCTION,
            INTERRUPT_PROCESS_FUNCTIONS,
            INTERRUPTED,
            INTERVALS,
            INVALID_ARITY,
            INVALID_FUNCTION,
            INVALID_READ_SYNTAX,
            INVALID_REGEXP,
            INVALID_SOURCE,
            INVISIBLE,
            IPV4,
            IPV6,
            ISDIR,
            ISO10646_1,
            ISO8859_1,
            ISO_2022,
            ISO_8859_1,
            ITALIC,
            IV_AUTO,
            JA,
            JSON_END_OF_FILE,
            JSON_ERROR,
            JSON_ESCAPE_SEQUENCE_ERROR,
            JSON_INVALID_SURROGATE_ERROR,
            JSON_NUMBER_OUT_OF_RANGE,
            JSON_OBJECT_TOO_DEEP,
            JSON_OUT_OF_MEMORY,
            JSON_PARSE_ERROR,
            JSON_PARSE_STRING,
            JSON_SERIALIZE,
            JSON_TRAILING_CONTENT,
            JSON_UTF8_DECODE_ERROR,
            JSON_VALUE_P,
            JUMP,
            KBD_MACRO_TERMINATION_HOOK,
            KEEP_RATIO,
            KEY,
            KEY_AND_VALUE,
            KEY_OR_VALUE,
            KEY_PARSE,
            KEY_VALID_P,
            KEYMAP,
            KEYMAP_CANONICALIZE,
            KEYMAPP,
            KILL_BUFFER__POSSIBLY_SAVE,
            KILL_BUFFER_HOOK,
            KILL_BUFFER_QUERY_FUNCTIONS,
            KILL_EMACS,
            KILL_EMACS_HOOK,
            KILL_FORWARD_CHARS,
            KO,
            LAMBDA_FIXUP,
            LANGUAGE_CHANGE,
            LAST_ARROW_POSITION,
            LAST_ARROW_STRING,
            LAST_NONMENU_EVENT,
            LATE,
            LEFT,
            LEFT_EDGE,
            LEFT_FRINGE,
            LEFT_FRINGE_HELP,
            LEFT_MARGIN,
            LEFT_ONLY,
            LEFT_TO_RIGHT,
            LEFTMOST,
            LET,
            LETX,
            LEXICAL_BINDING,
            LIGHT,
            LINE,
            LINE_HEIGHT,
            LINE_NUMBER,
            LINE_NUMBER_CURRENT_LINE,
            LINE_NUMBER_MAJOR_TICK,
            LINE_NUMBER_MINOR_TICK,
            LINE_PREFIX,
            LINE_SPACING,
            LISP_DIRECTORY,
            LIST,
            LIST_OR_VECTOR_P,
            LIST_SYSTEM_PROCESSES,
            LISTEN,
            LISTP,
            LITTLE,
            LIVE,
            LOAD,
            LOAD_FILE_NAME,
            LOAD_FORCE_DOC_STRINGS,
            LOAD_IN_PROGRESS,
            LOAD_TIME,
            LOAD_TRUE_FILE_NAME,
            LOCAL,
            LOCAL_MAP,
            LOCK_FILE,
            LONG,
            LONG_LINE_OPTIMIZATIONS_IN_COMMAND_HOOKS,
            LONG_LINE_OPTIMIZATIONS_IN_FONTIFICATION_FUNCTIONS,
            LOSING_VALUE,
            LOWERCASE,
            LREAD_UNESCAPED_CHARACTER_LITERALS,
            M,
            MAC,
            MACRO,
            MACROEXP__DYNVARS,
            MACS,
            MAJFLT,
            MAKE_CURSOR_LINE_FULLY_VISIBLE,
            MAKE_DIRECTORY,
            MAKE_DIRECTORY_INTERNAL,
            MAKE_FRAME_VISIBLE,
            MAKE_INITIAL_MINIBUFFER_FRAME,
            MAKE_INVISIBLE,
            MAKE_LOCK_FILE_NAME,
            MAKE_PROCESS,
            MAKE_SYMBOLIC_LINK,
            MAKE_WINDOW_START_VISIBLE,
            MAKUNBOUND,
            MANY,
            MAP_KEYMAP_SORTED,
            MARGIN,
            MARK_FOR_REDISPLAY,
            MARK_INACTIVE,
            MARKER,
            MARKERP,
            MAXIMIZED,
            MD5,
            MEDIUM,
            MEMORY_INFO,
            MENU,
            MENU_BAR,
            MENU_BAR_EXTERNAL,
            MENU_BAR_LINES,
            MENU_BAR_SIZE,
            MENU_BAR_UPDATE_HOOK,
            MENU_ENABLE,
            MENU_ITEM,
            MESSAGE,
            MESSAGES_BUFFER_MODE,
            METADATA,
            MIN_HEIGHT,
            MIN_WIDTH,
            MINFLT,
            MINIBUFFER,
            MINIBUFFER_COMPLETING_FILE_NAME,
            MINIBUFFER_COMPLETION_TABLE,
            MINIBUFFER_DEFAULT,
            MINIBUFFER_EXIT,
            MINIBUFFER_EXIT_HOOK,
            MINIBUFFER_FOLLOWS_SELECTED_FRAME,
            MINIBUFFER_HISTORY,
            MINIBUFFER_INACTIVE_MODE,
            MINIBUFFER_MODE,
            MINIBUFFER_PROMPT,
            MINIBUFFER_QUIT,
            MINIBUFFER_QUIT_RECURSIVE_EDIT,
            MINIBUFFER_SETUP_HOOK,
            MINUS,
            MISSING,
            MISSING_MODULE_INIT_FUNCTION,
            MM_SIZE,
            MODE_CLASS,
            MODE_LINE,
            MODE_LINE_ACTIVE,
            MODE_LINE_DEFAULT_HELP_ECHO,
            MODE_LINE_FORMAT,
            MODE_LINE_INACTIVE,
            MODELINE,
            MODIFICATION_HOOKS,
            MODIFIER_CACHE,
            MODIFY,
            MODULE_FUNCTION,
            MODULE_FUNCTION_P,
            MODULE_INIT_FAILED,
            MODULE_LOAD_FAILED,
            MODULE_NOT_GPL_COMPATIBLE,
            MODULE_OPEN_FAILED,
            MONTHS,
            MOUSE,
            MOUSE_CLICK,
            MOUSE_COLOR,
            MOUSE_FACE,
            MOUSE_FIXUP_HELP_MESSAGE,
            MOUSE_LEAVE_BUFFER_HOOK,
            MOUSE_MOVEMENT,
            MOUSE_WHEEL_FRAME,
            MOVE,
            MOVE_FRAME,
            MOVE_SELF,
            MOVE_TOOLBAR,
            MOVED_FROM,
            MOVED_TO,
            MUTEX,
            MUTEXP,
            NAME,
            NAMED,
            NATIVE__COMPILE_ASYNC,
            NATIVE_COMP_COMPILER_OPTIONS,
            NATIVE_COMP_DEBUG,
            NATIVE_COMP_DRIVER_OPTIONS,
            NATIVE_COMP_FUNCTION,
            NATIVE_COMP_SPEED,
            NATIVE_COMP_UNIT,
            NATIVE_COMP_WARNING_ON_MISSING_SOURCE,
            NATIVE_COMPILER,
            NATIVE_COMPILER_ERROR,
            NATIVE_EDGES,
            NATIVE_ICE,
            NATIVE_LISP_FILE_INCONSISTENT,
            NATIVE_LISP_LOAD_FAILED,
            NATIVE_LISP_WRONG_RELOC,
            NATNUMP,
            NEGATE,
            NETWORK,
            NHDRAG,
            NICE,
            NO_ACCEPT_FOCUS,
            NO_CATCH,
            NO_CONVERSION,
            NO_FOCUS_ON_MAP,
            NO_OTHER_FRAME,
            NO_OTHER_WINDOW,
            NO_RECORD,
            NO_SELF_INSERT,
            NO_SPECIAL_GLYPHS,
            NOBREAK_HYPHEN,
            NOBREAK_SPACE,
            NOELISP,
            NON_ASCII,
            NON_KEY_EVENT,
            NONE,
            NORMAL,
            NOT,
            NOT_FOUND,
            NS,
            NS_APPEARANCE,
            NS_PARSE_GEOMETRY,
            NS_TRANSPARENT_TITLEBAR,
            NS_UNPUT_WORKING_TEXT,
            NSM_VERIFY_CONNECTION,
            NTH,
            NULL,
            NUMBER_OR_MARKER_P,
            NUMBERP,
            NUMERIC,
            OBARRAY,
            OBARRAY_CACHE,
            OBARRAYP,
            OBJECT,
            OBLIQUE,
            OCLOSURE_INTERACTIVE_FORM,
            ODD,
            ONLY,
            ONLYDIR,
            OPEN,
            OPEN_NETWORK_STREAM,
            OPENTYPE,
            OPERATIONS,
            OR,
            OUTDATED,
            OUTER_BORDER_WIDTH,
            OUTER_EDGES,
            OUTER_POSITION,
            OUTER_SIZE,
            OUTER_WINDOW_ID,
            OUTERMOST_RESTRICTION,
            OVERFLOW_ERROR,
            OVERLAY,
            OVERLAY_ARROW_BITMAP,
            OVERLAY_ARROW_STRING,
            OVERLAYP,
            OVERRIDE_REDIRECT,
            OVERRIDING_LOCAL_MAP,
            OVERRIDING_PLIST_ENVIRONMENT,
            OVERRIDING_TERMINAL_LOCAL_MAP,
            OVERWRITE_MODE,
            OVERWRITE_MODE_BINARY,
            P,
            PAPER,
            PARENT_FRAME,
            PARENT_ID,
            PC,
            PCPU,
            PERMANENT_LOCAL,
            PERMANENT_LOCAL_HOOK,
            PERMISSION_DENIED,
            PGRP,
            PGTK,
            PHI,
            PINCH,
            PIPE,
            PIPE_PROCESS_P,
            PLAY_SOUND_FUNCTIONS,
            PLIST,
            PLISTP,
            PLUS,
            PMEM,
            POINT_ENTERED,
            POINT_LEFT,
            POINTER,
            POLLING_PERIOD,
            POLY,
            POP_HANDLER,
            POSITION,
            POST_COMMAND_HOOK,
            POST_GC_HOOK,
            POST_SELECT_REGION_HOOK,
            POST_SELF_INSERT_HOOK,
            PPID,
            PRE_COMMAND_HOOK,
            PREEDIT_TEXT,
            PRESSED_BUTTON,
            PRI,
            PRIMITIVE_FUNCTION,
            PRINC,
            PRINT__UNREADABLE_CALLBACK_BUFFER,
            PRINT_ESCAPE_MULTIBYTE,
            PRINT_ESCAPE_NONASCII,
            PRINT_SYMBOLS_BARE,
            PRINT_UNREADABLE_FUNCTION,
            PRIORITY,
            PROCESS,
            PROCESS_ATTRIBUTES,
            PROCESSP,
            PROGN,
            PROPERTIZE,
            PROTECTED_FIELD,
            PROVIDE,
            PTY,
            PURE,
            PURECOPY,
            PUSH_HANDLER,
            PUSH_WINDOW_BUFFER_ONTO_PREV,
            Q_OVERFLOW,
            QUIT,
            QUIT_RESTORE,
            QUIT_RESTORE_PREV,
            QUOTE,
            RAISE,
            RANGE,
            RANGE_ERROR,
            RATIO,
            RAW_TEXT,
            READ,
            READ_CHAR,
            READ_FILE_NAME,
            READ_MINIBUFFER,
            READ_NUMBER,
            READ_ONLY,
            REAL,
            REAL_THIS_COMMAND,
            REAR_NONSTICKY,
            RECORD,
            RECORD_UNWIND_CURRENT_BUFFER,
            RECORD_UNWIND_PROTECT_EXCURSION,
            RECORD_WINDOW_BUFFER,
            RECORDP,
            RECT,
            RECURSION_ERROR,
            REDISPLAY_DONT_PAUSE,
            REDISPLAY_INTERNAL_XC_FUNCTIONX,
            RELATIVE,
            RELEASED_BUTTON,
            REMAP,
            REMOTE_FILE_ERROR,
            RENAME_AUTO_SAVE_FILE,
            RENAME_FILE,
            REPLACE_BUFFER_IN_WINDOWS,
            REQUIRE,
            RESET,
            RESUME_TTY_FUNCTIONS,
            RETURN,
            RIGHT,
            RIGHT_DIVIDER,
            RIGHT_DIVIDER_WIDTH,
            RIGHT_EDGE,
            RIGHT_FRINGE,
            RIGHT_FRINGE_HELP,
            RIGHT_MARGIN,
            RIGHT_TO_LEFT,
            RIGHTMOST,
            RISKY_LOCAL_VARIABLE,
            RSS,
            RUN,
            RUN_HOOK_QUERY_ERROR_WITH_TIMEOUT,
            RUN_HOOK_WITH_ARGS,
            SAFE,
            SAFE_MAGIC,
            SAVE_EXCURSION,
            SAVE_SESSION,
            SCALE_FACTOR,
            SCAN_ERROR,
            SCRATCH,
            SCREEN_GAMMA,
            SCROLL_BAR,
            SCROLL_BAR_BACKGROUND,
            SCROLL_BAR_FOREGROUND,
            SCROLL_BAR_HEIGHT,
            SCROLL_BAR_MOVEMENT,
            SCROLL_BAR_WIDTH,
            SCROLL_COMMAND,
            SCROLL_DOWN,
            SCROLL_UP,
            SEARCH_FAILED,
            SELECT_FRAME_SET_INPUT_FOCUS,
            SELECT_WINDOW,
            SELECTION_REQUEST,
            SELF_INSERT_COMMAND,
            SEMI_BOLD,
            SEMI_LIGHT,
            SEQPACKET,
            SEQUENCEP,
            SERIAL,
            SESS,
            SET,
            SET_ARGS_TO_LOCAL,
            SET_BUFFER_MULTIBYTE,
            SET_DEFAULT,
            SET_FILE_ACL,
            SET_FILE_MODES,
            SET_FILE_SELINUX_CONTEXT,
            SET_FILE_TIMES,
            SET_INTERNAL,
            SET_PAR_TO_LOCAL,
            SET_REST_ARGS_TO_LOCAL,
            SET_VISITED_FILE_MODTIME,
            SET_WINDOW_CONFIGURATION,
            SETCAR,
            SETCDR,
            SETIMM,
            SETQ,
            SETTING_CONSTANT,
            SHA1,
            SHA224,
            SHA256,
            SHA384,
            SHA512,
            SHADED,
            SHIFT_JIS,
            SIDE_EFFECT_FREE,
            SIGNAL,
            SIGNAL_PROCESS_FUNCTIONS,
            SIGUSR2,
            SINGULARITY_ERROR,
            SIZE,
            SKIP_TASKBAR,
            SLICE,
            SOUND,
            SOURCE,
            SPACE,
            SPACE_WIDTH,
            SPECIAL_FORM,
            SPECIAL_LOWERCASE,
            SPECIAL_TITLECASE,
            SPECIAL_UPPERCASE,
            SQLITE,
            SQLITE3,
            SQLITE_ERROR,
            SQLITE_LOCKED_ERROR,
            SQLITEP,
            STANDARD_INPUT,
            STANDARD_OUTPUT,
            START,
            START_PROCESS,
            STATE,
            STDERR,
            STDIN,
            STDOUT,
            STICKY,
            STIME,
            STOP,
            STRAIGHT,
            STRING,
            STRING_BYTES,
            STRING_LESSP,
            STRING_REPLACE,
            STRINGP,
            STRINGS,
            SUB1,
            SUB_CHAR_TABLE,
            SUBFEATURES,
            SUBR,
            SUBR_NATIVE_ELISP,
            SUBRP,
            SUBSTITUTE_COMMAND_KEYS,
            SUBSTITUTE_ENV_IN_FILE_NAME,
            SUBSTITUTE_IN_FILE_NAME,
            SUPPRESS_KEYMAP,
            SUSPEND_HOOK,
            SUSPEND_RESUME_HOOK,
            SUSPEND_TTY_FUNCTIONS,
            SW,
            SWITCH_FRAME,
            SYMBOL,
            SYMBOL_ERROR,
            SYMBOL_WITH_POS,
            SYMBOL_WITH_POS_P,
            SYMBOLP,
            SYMBOLS,
            SYMBOLS_WITH_POS_ENABLED,
            SYNTAX_PPSS,
            SYNTAX_PPSS_FLUSH_CACHE,
            SYNTAX_TABLE,
            SYNTAX_TABLE_P,
            TAB_BAR,
            TAB_BAR_LINES,
            TAB_BAR_SIZE,
            TAB_LINE,
            TAB_LINE_FORMAT,
            TARGET_IDX,
            TEMP_BUFFER_SETUP_HOOK,
            TEMP_BUFFER_SHOW_HOOK,
            TERMINAL,
            TERMINAL_FRAME,
            TERMINAL_LIVE_P,
            TEST,
            TEXT,
            TEXT_CONVERSION,
            TEXT_IMAGE_HORIZ,
            TEXT_PIXELS,
            TEXT_READ_ONLY,
            THCOUNT,
            THIN,
            THIN_SPACE,
            THREAD,
            THREAD_EVENT,
            THREADP,
            TIME,
            TIMER_EVENT_HANDLER,
            TIP_FRAME,
            TITLE,
            TITLE_BAR_SIZE,
            TITLECASE,
            TOOL_BAR,
            TOOL_BAR_EXTERNAL,
            TOOL_BAR_LINES,
            TOOL_BAR_POSITION,
            TOOL_BAR_SIZE,
            TOOLTIP,
            TOP,
            TOP_EDGE,
            TOP_LEFT_CORNER,
            TOP_LEVEL,
            TOP_ONLY,
            TOP_RIGHT_CORNER,
            TOUCH_END,
            TOUCHSCREEN,
            TOUCHSCREEN_BEGIN,
            TOUCHSCREEN_END,
            TOUCHSCREEN_UPDATE,
            TPGID,
            TRAILING_WHITESPACE,
            TRANSLATION_TABLE,
            TRANSLATION_TABLE_ID,
            TRAPPING_CONSTANT,
            TREE_SITTER,
            TREESIT_BUFFER_TOO_LARGE,
            TREESIT_COMPILED_QUERY,
            TREESIT_COMPILED_QUERY_P,
            TREESIT_ERROR,
            TREESIT_INVALID_PREDICATE,
            TREESIT_LANGUAGE_REMAP_ALIST,
            TREESIT_LOAD_LANGUAGE_ERROR,
            TREESIT_NODE,
            TREESIT_NODE_BUFFER_KILLED,
            TREESIT_NODE_OUTDATED,
            TREESIT_NODE_P,
            TREESIT_PARSE_ERROR,
            TREESIT_PARSER,
            TREESIT_PARSER_DELETED,
            TREESIT_PARSER_P,
            TREESIT_PATTERN_EXPAND,
            TREESIT_PREDICATE_NOT_FOUND,
            TREESIT_QUERY_ERROR,
            TREESIT_QUERY_P,
            TREESIT_RANGE_INVALID,
            TTNAME,
            TTY,
            TTY_COLOR_ALIST,
            TTY_COLOR_BY_INDEX,
            TTY_COLOR_DESC,
            TTY_COLOR_MODE,
            TTY_COLOR_STANDARD_VALUES,
            TTY_DEFINED_COLOR_ALIST,
            TTY_MENU_DISABLED_FACE,
            TTY_MENU_ENABLED_FACE,
            TTY_MENU_EXIT,
            TTY_MENU_IGNORE,
            TTY_MENU_MOUSE_MOVEMENT,
            TTY_MENU_NAVIGATION_MAP,
            TTY_MENU_NEXT_ITEM,
            TTY_MENU_NEXT_MENU,
            TTY_MENU_PREV_ITEM,
            TTY_MENU_PREV_MENU,
            TTY_MENU_SELECT,
            TTY_MODE_RESET_STRINGS,
            TTY_MODE_SET_STRINGS,
            TTY_SELECT_ACTIVE_REGIONS,
            TTY_SET_UP_INITIAL_FRAME_FACES,
            TTY_TYPE,
            TYPE_MISMATCH,
            ULTRA_BOLD,
            ULTRA_HEAVY,
            ULTRA_LIGHT,
            UNDECIDED,
            UNDECORATED,
            UNDEFINED,
            UNDERFLOW_ERROR,
            UNDERLINE_MINIMUM_OFFSET,
            UNDO_AUTO__ADD_BOUNDARY,
            UNDO_AUTO__LAST_BOUNDARY_CAUSE,
            UNDO_AUTO__THIS_COMMAND_AMALGAMATING,
            UNDO_AUTO__UNDOABLE_CHANGE,
            UNDO_AUTO__UNDOABLY_CHANGED_BUFFERS,
            UNDO_AUTO_AMALGAMATE,
            UNEVALLED,
            UNHANDLED_FILE_NAME_DIRECTORY,
            UNICODE,
            UNICODE_BMP,
            UNICODE_STRING_P,
            UNIQUIFY__RENAME_BUFFER_ADVICE,
            UNIX,
            UNLET,
            UNLOCK_FILE,
            UNMOUNT,
            UNREACHABLE,
            UNSPECIFIED,
            UNSPLITTABLE,
            UP,
            UPPERCASE,
            US_ASCII,
            USE_FRAME_SYNCHRONIZATION,
            USER,
            USER_EMACS_DIRECTORY,
            USER_ERROR,
            USER_POSITION,
            USER_PTR,
            USER_PTRP,
            USER_SEARCH_FAILED,
            USER_SIZE,
            UTF_16,
            UTF_16LE,
            UTF_8,
            UTF_8_EMACS,
            UTF_8_STRING_P,
            UTF_8_UNIX,
            UTIME,
            VALUE,
            VALUELT,
            VARIABLE_DOCUMENTATION,
            VDRAG,
            VECTOR,
            VECTOR_OR_CHAR_TABLE_P,
            VECTOR_SLOTS,
            VECTORP,
            VECTORS,
            VERIFY_VISITED_FILE_MODTIME,
            VERSION_MISMATCH,
            VERTICAL_BORDER,
            VERTICAL_LINE,
            VERTICAL_SCROLL_BAR,
            VERTICAL_SCROLL_BARS,
            VISIBILITY,
            VISIBLE,
            VISUAL,
            VOID_FUNCTION,
            VOID_VARIABLE,
            VSIZE,
            W32,
            WAIT_FOR_WM,
            WALL,
            WATCHERS,
            WAVE,
            WEAKNESS,
            WHEN,
            WHOLENUMP,
            WIDGET_TYPE,
            WIDTH,
            WIDTH_ONLY,
            WINDOW,
            WINDOW__PIXEL_TO_TOTAL,
            WINDOW__RESIZE_MINI_FRAME,
            WINDOW__RESIZE_ROOT_WINDOW,
            WINDOW__RESIZE_ROOT_WINDOW_VERTICALLY,
            WINDOW_BUFFER_CHANGE_FUNCTIONS,
            WINDOW_CONFIGURATION,
            WINDOW_CONFIGURATION_CHANGE_HOOK,
            WINDOW_CONFIGURATION_P,
            WINDOW_DELETABLE_P,
            WINDOW_DIVIDER,
            WINDOW_DIVIDER_FIRST_PIXEL,
            WINDOW_DIVIDER_LAST_PIXEL,
            WINDOW_EDGES,
            WINDOW_ID,
            WINDOW_LIVE_P,
            WINDOW_POINT_INSERTION_TYPE,
            WINDOW_SCROLL_FUNCTIONS,
            WINDOW_SELECTION_CHANGE_FUNCTIONS,
            WINDOW_SIZE,
            WINDOW_SIZE_CHANGE_FUNCTIONS,
            WINDOW_STATE_CHANGE_FUNCTIONS,
            WINDOW_STATE_CHANGE_HOOK,
            WINDOW_VALID_P,
            WINDOWP,
            WORKAREA,
            WRAP_PREFIX,
            WRITE_REGION,
            WRITE_REGION_ANNOTATE_FUNCTIONS,
            WRONG_LENGTH_ARGUMENT,
            WRONG_NUMBER_OF_ARGUMENTS,
            WRONG_REGISTER_SUBR_CALL,
            WRONG_TYPE_ARGUMENT,
            X,
            X_CREATE_FRAME_1,
            X_CREATE_FRAME_2,
            X_FRAME_PARAMETER,
            X_PRE_POPUP_MENU_HOOK,
            X_RESOURCE_NAME,
            X_SET_MENU_BAR_LINES,
            X_SET_WINDOW_SIZE_1,
            XG_FRAME_SET_CHAR_SIZE,
            XTERM__SET_SELECTION,
            XWIDGET,
            XWIDGET_DISPLAY_EVENT,
            XWIDGET_EVENT,
            XWIDGET_VIEW,
            Y_OR_N_P,
            YES_OR_NO_P,
            YES_OR_NO_P_HISTORY,
            Z_GROUP,
            ZERO_WIDTH,
        };
    }
    //#endregion globals.h
    //#region variable symbols
    public static final ELispSymbol AFTER_INIT_TIME = new ELispSymbol("after-init-time");
    public static final ELispSymbol AFTER_INSERT_FILE_FUNCTIONS = new ELispSymbol("after-insert-file-functions");
    public static final ELispSymbol AFTER_LOAD_ALIST = new ELispSymbol("after-load-alist");
    public static final ELispSymbol AMBIGUOUS_WIDTH_CHARS = new ELispSymbol("ambiguous-width-chars");
    public static final ELispSymbol ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL = new ELispSymbol("attempt-orderly-shutdown-on-fatal-signal");
    public static final ELispSymbol ATTEMPT_STACK_OVERFLOW_RECOVERY = new ELispSymbol("attempt-stack-overflow-recovery");
    public static final ELispSymbol AUTO_COMPOSITION_EMOJI_ELIGIBLE_CODEPOINTS = new ELispSymbol("auto-composition-emoji-eligible-codepoints");
    public static final ELispSymbol AUTO_COMPOSITION_FUNCTION = new ELispSymbol("auto-composition-function");
    public static final ELispSymbol AUTO_COMPOSITION_MODE = new ELispSymbol("auto-composition-mode");
    public static final ELispSymbol AUTO_RAISE_TAB_BAR_BUTTONS = new ELispSymbol("auto-raise-tab-bar-buttons");
    public static final ELispSymbol AUTO_RAISE_TOOL_BAR_BUTTONS = new ELispSymbol("auto-raise-tool-bar-buttons");
    public static final ELispSymbol AUTO_RESIZE_TAB_BARS = new ELispSymbol("auto-resize-tab-bars");
    public static final ELispSymbol AUTO_RESIZE_TOOL_BARS = new ELispSymbol("auto-resize-tool-bars");
    public static final ELispSymbol AUTO_SAVE_INCLUDE_BIG_DELETIONS = new ELispSymbol("auto-save-include-big-deletions");
    public static final ELispSymbol AUTO_SAVE_INTERVAL = new ELispSymbol("auto-save-interval");
    public static final ELispSymbol AUTO_SAVE_LIST_FILE_NAME = new ELispSymbol("auto-save-list-file-name");
    public static final ELispSymbol AUTO_SAVE_NO_MESSAGE = new ELispSymbol("auto-save-no-message");
    public static final ELispSymbol AUTO_SAVE_TIMEOUT = new ELispSymbol("auto-save-timeout");
    public static final ELispSymbol AUTO_SAVE_VISITED_FILE_NAME = new ELispSymbol("auto-save-visited-file-name");
    public static final ELispSymbol AUTO_WINDOW_VSCROLL = new ELispSymbol("auto-window-vscroll");
    public static final ELispSymbol BACKTRACE_ON_ERROR_NONINTERACTIVE = new ELispSymbol("backtrace-on-error-noninteractive");
    public static final ELispSymbol BACKTRACE_ON_REDISPLAY_ERROR = new ELispSymbol("backtrace-on-redisplay-error");
    public static final ELispSymbol BAUD_RATE = new ELispSymbol("baud-rate");
    public static final ELispSymbol BEFORE_INIT_TIME = new ELispSymbol("before-init-time");
    public static final ELispSymbol BIDI_INHIBIT_BPA = new ELispSymbol("bidi-inhibit-bpa");
    public static final ELispSymbol BINARY_AS_UNSIGNED = new ELispSymbol("binary-as-unsigned");
    public static final ELispSymbol BLINK_CURSOR_ALIST = new ELispSymbol("blink-cursor-alist");
    public static final ELispSymbol BUFFER_ACCESS_FONTIFIED_PROPERTY = new ELispSymbol("buffer-access-fontified-property");
    public static final ELispSymbol BUILD_FILES = new ELispSymbol("build-files");
    public static final ELispSymbol BYTECOMP_VERSION_REGEXP = new ELispSymbol("bytecomp-version-regexp");
    public static final ELispSymbol BYTE_BOOLEAN_VARS = new ELispSymbol("byte-boolean-vars");
    public static final ELispSymbol BYTE_METERING_ON = new ELispSymbol("byte-metering-on");
    public static final ELispSymbol CANNOT_SUSPEND = new ELispSymbol("cannot-suspend");
    public static final ELispSymbol CHARSET_LIST = new ELispSymbol("charset-list");
    public static final ELispSymbol CHARSET_MAP_PATH = new ELispSymbol("charset-map-path");
    public static final ELispSymbol CHARSET_REVISION_TABLE = new ELispSymbol("charset-revision-table");
    public static final ELispSymbol CHAR_CODE_PROPERTY_ALIST = new ELispSymbol("char-code-property-alist");
    public static final ELispSymbol CHAR_PROPERTY_ALIAS_ALIST = new ELispSymbol("char-property-alias-alist");
    public static final ELispSymbol CHAR_WIDTH_TABLE = new ELispSymbol("char-width-table");
    public static final ELispSymbol CLEAR_MESSAGE_FUNCTION = new ELispSymbol("clear-message-function");
    public static final ELispSymbol CODE_CONVERSION_MAP_VECTOR = new ELispSymbol("code-conversion-map-vector");
    public static final ELispSymbol CODING_CATEGORY_LIST = new ELispSymbol("coding-category-list");
    public static final ELispSymbol CODING_SYSTEM_ALIST = new ELispSymbol("coding-system-alist");
    public static final ELispSymbol CODING_SYSTEM_FOR_READ = new ELispSymbol("coding-system-for-read");
    public static final ELispSymbol CODING_SYSTEM_LIST = new ELispSymbol("coding-system-list");
    public static final ELispSymbol CODING_SYSTEM_REQUIRE_WARNING = new ELispSymbol("coding-system-require-warning");
    public static final ELispSymbol COMMAND_ERROR_FUNCTION = new ELispSymbol("command-error-function");
    public static final ELispSymbol COMMAND_LINE_ARGS = new ELispSymbol("command-line-args");
    public static final ELispSymbol COMMENT_USE_SYNTAX_PPSS = new ELispSymbol("comment-use-syntax-ppss");
    public static final ELispSymbol COMPLETION_IGNORED_EXTENSIONS = new ELispSymbol("completion-ignored-extensions");
    public static final ELispSymbol COMPLETION_REGEXP_LIST = new ELispSymbol("completion-regexp-list");
    public static final ELispSymbol COMPOSE_CHARS_AFTER_FUNCTION = new ELispSymbol("compose-chars-after-function");
    public static final ELispSymbol COMPOSITION_BREAK_AT_POINT = new ELispSymbol("composition-break-at-point");
    public static final ELispSymbol COMPOSITION_FUNCTION_TABLE = new ELispSymbol("composition-function-table");
    public static final ELispSymbol COMP_ABI_HASH = new ELispSymbol("comp-abi-hash");
    public static final ELispSymbol COMP_CTXT = new ELispSymbol("comp-ctxt");
    public static final ELispSymbol COMP_DEFERRED_PENDING_H = new ELispSymbol("comp-deferred-pending-h");
    public static final ELispSymbol COMP_ELN_TO_EL_H = new ELispSymbol("comp-eln-to-el-h");
    public static final ELispSymbol COMP_FILE_PRELOADED_P = new ELispSymbol("comp-file-preloaded-p");
    public static final ELispSymbol COMP_INSTALLED_TRAMPOLINES_H = new ELispSymbol("comp-installed-trampolines-h");
    public static final ELispSymbol COMP_LOADED_COMP_UNITS_H = new ELispSymbol("comp-loaded-comp-units-h");
    public static final ELispSymbol COMP_NATIVE_VERSION_DIR = new ELispSymbol("comp-native-version-dir");
    public static final ELispSymbol COMP_NO_NATIVE_FILE_H = new ELispSymbol("comp-no-native-file-h");
    public static final ELispSymbol COMP_SANITIZER_ACTIVE = new ELispSymbol("comp-sanitizer-active");
    public static final ELispSymbol COMP_SUBR_ARITIES_H = new ELispSymbol("comp-subr-arities-h");
    public static final ELispSymbol COMP_SUBR_LIST = new ELispSymbol("comp-subr-list");
    public static final ELispSymbol CONFIGURE_INFO_DIRECTORY = new ELispSymbol("configure-info-directory");
    public static final ELispSymbol CONS_CELLS_CONSED = new ELispSymbol("cons-cells-consed");
    public static final ELispSymbol CREATE_LOCKFILES = new ELispSymbol("create-lockfiles");
    public static final ELispSymbol CTAGS_PROGRAM_NAME = new ELispSymbol("ctags-program-name");
    public static final ELispSymbol CURRENT_ISO639_LANGUAGE = new ELispSymbol("current-iso639-language");
    public static final ELispSymbol CURRENT_PREFIX_ARG = new ELispSymbol("current-prefix-arg");
    public static final ELispSymbol CURRENT_TIME_LIST = new ELispSymbol("current-time-list");
    public static final ELispSymbol DATA_DIRECTORY = new ELispSymbol("data-directory");
    public static final ELispSymbol DEBUGGER_STACK_FRAME_AS_LIST = new ELispSymbol("debugger-stack-frame-as-list");
    public static final ELispSymbol DEBUG_END_POS = new ELispSymbol("debug-end-pos");
    public static final ELispSymbol DEBUG_IGNORED_ERRORS = new ELispSymbol("debug-ignored-errors");
    public static final ELispSymbol DEBUG_ON_ERROR = new ELispSymbol("debug-on-error");
    public static final ELispSymbol DEBUG_ON_EVENT = new ELispSymbol("debug-on-event");
    public static final ELispSymbol DEBUG_ON_MESSAGE = new ELispSymbol("debug-on-message");
    public static final ELispSymbol DEBUG_ON_NEXT_CALL = new ELispSymbol("debug-on-next-call");
    public static final ELispSymbol DEBUG_ON_QUIT = new ELispSymbol("debug-on-quit");
    public static final ELispSymbol DEBUG_ON_SIGNAL = new ELispSymbol("debug-on-signal");
    public static final ELispSymbol DEFAULT_FILE_NAME_CODING_SYSTEM = new ELispSymbol("default-file-name-coding-system");
    public static final ELispSymbol DEFAULT_FRAME_ALIST = new ELispSymbol("default-frame-alist");
    public static final ELispSymbol DEFAULT_FRAME_SCROLL_BARS = new ELispSymbol("default-frame-scroll-bars");
    public static final ELispSymbol DEFAULT_MINIBUFFER_FRAME = new ELispSymbol("default-minibuffer-frame");
    public static final ELispSymbol DEFAULT_PROCESS_CODING_SYSTEM = new ELispSymbol("default-process-coding-system");
    public static final ELispSymbol DEFAULT_TEXT_PROPERTIES = new ELispSymbol("default-text-properties");
    public static final ELispSymbol DEFINING_KBD_MACRO = new ELispSymbol("defining-kbd-macro");
    public static final ELispSymbol DELAYED_WARNINGS_LIST = new ELispSymbol("delayed-warnings-list");
    public static final ELispSymbol DELETE_AUTO_SAVE_FILES = new ELispSymbol("delete-auto-save-files");
    public static final ELispSymbol DELETE_EXITED_PROCESSES = new ELispSymbol("delete-exited-processes");
    public static final ELispSymbol DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES = new ELispSymbol("describe-bindings-check-shadowing-in-ranges");
    public static final ELispSymbol DISABLE_ASCII_OPTIMIZATION = new ELispSymbol("disable-ascii-optimization");
    public static final ELispSymbol DISABLE_INHIBIT_TEXT_CONVERSION = new ELispSymbol("disable-inhibit-text-conversion");
    public static final ELispSymbol DISABLE_POINT_ADJUSTMENT = new ELispSymbol("disable-point-adjustment");
    public static final ELispSymbol DISPLAY_HOURGLASS = new ELispSymbol("display-hourglass");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE = new ELispSymbol("display-line-numbers-current-absolute");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_MAJOR_TICK = new ELispSymbol("display-line-numbers-major-tick");
    public static final ELispSymbol DISPLAY_LINE_NUMBERS_MINOR_TICK = new ELispSymbol("display-line-numbers-minor-tick");
    public static final ELispSymbol DISPLAY_PIXELS_PER_INCH = new ELispSymbol("display-pixels-per-inch");
    public static final ELispSymbol DISPLAY_RAW_BYTES_AS_HEX = new ELispSymbol("display-raw-bytes-as-hex");
    public static final ELispSymbol DOC_DIRECTORY = new ELispSymbol("doc-directory");
    public static final ELispSymbol DOUBLE_CLICK_FUZZ = new ELispSymbol("double-click-fuzz");
    public static final ELispSymbol DOUBLE_CLICK_TIME = new ELispSymbol("double-click-time");
    public static final ELispSymbol DUMP_MODE = new ELispSymbol("dump-mode");
    public static final ELispSymbol DYNAMIC_LIBRARY_ALIST = new ELispSymbol("dynamic-library-alist");
    public static final ELispSymbol DYNAMIC_LIBRARY_SUFFIXES = new ELispSymbol("dynamic-library-suffixes");
    public static final ELispSymbol EBROWSE_PROGRAM_NAME = new ELispSymbol("ebrowse-program-name");
    public static final ELispSymbol ECHO_KEYSTROKES_HELP = new ELispSymbol("echo-keystrokes-help");
    public static final ELispSymbol EMACSCLIENT_PROGRAM_NAME = new ELispSymbol("emacsclient-program-name");
    public static final ELispSymbol EMACS_COPYRIGHT = new ELispSymbol("emacs-copyright");
    public static final ELispSymbol EMACS_VERSION = new ELispSymbol("emacs-version");
    public static final ELispSymbol EMULATION_MODE_MAP_ALISTS = new ELispSymbol("emulation-mode-map-alists");
    public static final ELispSymbol ENABLE_CHARACTER_TRANSLATION = new ELispSymbol("enable-character-translation");
    public static final ELispSymbol ENABLE_DISABLED_MENUS_AND_BUTTONS = new ELispSymbol("enable-disabled-menus-and-buttons");
    public static final ELispSymbol EOL_MNEMONIC_DOS = new ELispSymbol("eol-mnemonic-dos");
    public static final ELispSymbol EOL_MNEMONIC_MAC = new ELispSymbol("eol-mnemonic-mac");
    public static final ELispSymbol EOL_MNEMONIC_UNDECIDED = new ELispSymbol("eol-mnemonic-undecided");
    public static final ELispSymbol EOL_MNEMONIC_UNIX = new ELispSymbol("eol-mnemonic-unix");
    public static final ELispSymbol ETAGS_PROGRAM_NAME = new ELispSymbol("etags-program-name");
    public static final ELispSymbol EXECUTING_KBD_MACRO = new ELispSymbol("executing-kbd-macro");
    public static final ELispSymbol EXECUTING_KBD_MACRO_INDEX = new ELispSymbol("executing-kbd-macro-index");
    public static final ELispSymbol EXEC_DIRECTORY = new ELispSymbol("exec-directory");
    public static final ELispSymbol EXEC_PATH = new ELispSymbol("exec-path");
    public static final ELispSymbol EXEC_SUFFIXES = new ELispSymbol("exec-suffixes");
    public static final ELispSymbol EXTRA_KEYBOARD_MODIFIERS = new ELispSymbol("extra-keyboard-modifiers");
    public static final ELispSymbol FACE_DEFAULT_STIPPLE = new ELispSymbol("face-default-stipple");
    public static final ELispSymbol FACE_FILTERS_ALWAYS_MATCH = new ELispSymbol("face-filters-always-match");
    public static final ELispSymbol FACE_FONT_LAX_MATCHED_ATTRIBUTES = new ELispSymbol("face-font-lax-matched-attributes");
    public static final ELispSymbol FACE_FONT_RESCALE_ALIST = new ELispSymbol("face-font-rescale-alist");
    public static final ELispSymbol FACE_IGNORED_FONTS = new ELispSymbol("face-ignored-fonts");
    public static final ELispSymbol FACE_NEAR_SAME_COLOR_THRESHOLD = new ELispSymbol("face-near-same-color-threshold");
    public static final ELispSymbol FACE__NEW_FRAME_DEFAULTS = new ELispSymbol("face--new-frame-defaults");
    public static final ELispSymbol FAST_BUT_IMPRECISE_SCROLLING = new ELispSymbol("fast-but-imprecise-scrolling");
    public static final ELispSymbol FAST_READ_PROCESS_OUTPUT = new ELispSymbol("fast-read-process-output");
    public static final ELispSymbol FILE_CODING_SYSTEM_ALIST = new ELispSymbol("file-coding-system-alist");
    public static final ELispSymbol FILE_NAME_CODING_SYSTEM = new ELispSymbol("file-name-coding-system");
    public static final ELispSymbol FIND_WORD_BOUNDARY_FUNCTION_TABLE = new ELispSymbol("find-word-boundary-function-table");
    public static final ELispSymbol FLOATS_CONSED = new ELispSymbol("floats-consed");
    public static final ELispSymbol FOCUS_FOLLOWS_MOUSE = new ELispSymbol("focus-follows-mouse");
    public static final ELispSymbol FONT_CCL_ENCODER_ALIST = new ELispSymbol("font-ccl-encoder-alist");
    public static final ELispSymbol FORCE_LOAD_MESSAGES = new ELispSymbol("force-load-messages");
    public static final ELispSymbol FRAME_ALPHA_LOWER_LIMIT = new ELispSymbol("frame-alpha-lower-limit");
    public static final ELispSymbol FRAME_INHIBIT_IMPLIED_RESIZE = new ELispSymbol("frame-inhibit-implied-resize");
    public static final ELispSymbol FRAME_INTERNAL_PARAMETERS = new ELispSymbol("frame-internal-parameters");
    public static final ELispSymbol FRAME_RESIZE_PIXELWISE = new ELispSymbol("frame-resize-pixelwise");
    public static final ELispSymbol FRAME_SIZE_HISTORY = new ELispSymbol("frame-size-history");
    public static final ELispSymbol FRAME_TITLE_FORMAT = new ELispSymbol("frame-title-format");
    public static final ELispSymbol FUNCTION_KEY_MAP = new ELispSymbol("function-key-map");
    public static final ELispSymbol GARBAGE_COLLECTION_MESSAGES = new ELispSymbol("garbage-collection-messages");
    public static final ELispSymbol GCS_DONE = new ELispSymbol("gcs-done");
    public static final ELispSymbol GC_ELAPSED = new ELispSymbol("gc-elapsed");
    public static final ELispSymbol GLOBAL_DISABLE_POINT_ADJUSTMENT = new ELispSymbol("global-disable-point-adjustment");
    public static final ELispSymbol GLOBAL_MODE_STRING = new ELispSymbol("global-mode-string");
    public static final ELispSymbol GLYPH_TABLE = new ELispSymbol("glyph-table");
    public static final ELispSymbol HELP_CHAR = new ELispSymbol("help-char");
    public static final ELispSymbol HELP_EVENT_LIST = new ELispSymbol("help-event-list");
    public static final ELispSymbol HELP_FORM = new ELispSymbol("help-form");
    public static final ELispSymbol HEXL_PROGRAM_NAME = new ELispSymbol("hexl-program-name");
    public static final ELispSymbol HIGHLIGHT_NONSELECTED_WINDOWS = new ELispSymbol("highlight-nonselected-windows");
    public static final ELispSymbol HISTORY_ADD_NEW_INPUT = new ELispSymbol("history-add-new-input");
    public static final ELispSymbol HISTORY_DELETE_DUPLICATES = new ELispSymbol("history-delete-duplicates");
    public static final ELispSymbol HISTORY_LENGTH = new ELispSymbol("history-length");
    public static final ELispSymbol HOURGLASS_DELAY = new ELispSymbol("hourglass-delay");
    public static final ELispSymbol HSCROLL_MARGIN = new ELispSymbol("hscroll-margin");
    public static final ELispSymbol HSCROLL_STEP = new ELispSymbol("hscroll-step");
    public static final ELispSymbol ICONIFY_CHILD_FRAME = new ELispSymbol("iconify-child-frame");
    public static final ELispSymbol ICON_TITLE_FORMAT = new ELispSymbol("icon-title-format");
    public static final ELispSymbol INDENT_TABS_MODE = new ELispSymbol("indent-tabs-mode");
    public static final ELispSymbol INHERIT_PROCESS_CODING_SYSTEM = new ELispSymbol("inherit-process-coding-system");
    public static final ELispSymbol INHIBIT_BIDI_MIRRORING = new ELispSymbol("inhibit-bidi-mirroring");
    public static final ELispSymbol INHIBIT_EOL_CONVERSION = new ELispSymbol("inhibit-eol-conversion");
    public static final ELispSymbol INHIBIT_FIELD_TEXT_MOTION = new ELispSymbol("inhibit-field-text-motion");
    public static final ELispSymbol INHIBIT_FILE_NAME_HANDLERS = new ELispSymbol("inhibit-file-name-handlers");
    public static final ELispSymbol INHIBIT_INTERACTION = new ELispSymbol("inhibit-interaction");
    public static final ELispSymbol INHIBIT_ISO_ESCAPE_DETECTION = new ELispSymbol("inhibit-iso-escape-detection");
    public static final ELispSymbol INHIBIT_LOAD_CHARSET_MAP = new ELispSymbol("inhibit-load-charset-map");
    public static final ELispSymbol INHIBIT_MESSAGE = new ELispSymbol("inhibit-message");
    public static final ELispSymbol INHIBIT_MOUSE_EVENT_CHECK = new ELispSymbol("inhibit-mouse-event-check");
    public static final ELispSymbol INHIBIT_NULL_BYTE_DETECTION = new ELispSymbol("inhibit-null-byte-detection");
    public static final ELispSymbol INHIBIT_TRY_CURSOR_MOVEMENT = new ELispSymbol("inhibit-try-cursor-movement");
    public static final ELispSymbol INHIBIT_TRY_WINDOW_ID = new ELispSymbol("inhibit-try-window-id");
    public static final ELispSymbol INHIBIT_TRY_WINDOW_REUSING = new ELispSymbol("inhibit-try-window-reusing");
    public static final ELispSymbol INHIBIT_X_RESOURCES = new ELispSymbol("inhibit-x-resources");
    public static final ELispSymbol INHIBIT__RECORD_CHAR = new ELispSymbol("inhibit--record-char");
    public static final ELispSymbol INITIAL_ENVIRONMENT = new ELispSymbol("initial-environment");
    public static final ELispSymbol INITIAL_WINDOW_SYSTEM = new ELispSymbol("initial-window-system");
    public static final ELispSymbol INPUT_DECODE_MAP = new ELispSymbol("input-decode-map");
    public static final ELispSymbol INPUT_METHOD_FUNCTION = new ELispSymbol("input-method-function");
    public static final ELispSymbol INPUT_METHOD_PREVIOUS_MESSAGE = new ELispSymbol("input-method-previous-message");
    public static final ELispSymbol INPUT_PENDING_P_FILTER_EVENTS = new ELispSymbol("input-pending-p-filter-events");
    public static final ELispSymbol INSTALLATION_DIRECTORY = new ELispSymbol("installation-directory");
    public static final ELispSymbol INTEGER_WIDTH = new ELispSymbol("integer-width");
    public static final ELispSymbol INTERNAL_DOC_FILE_NAME = new ELispSymbol("internal-doc-file-name");
    public static final ELispSymbol INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION = new ELispSymbol("internal-make-interpreted-closure-function");
    public static final ELispSymbol INTERNAL__DAEMON_SOCKNAME = new ELispSymbol("internal--daemon-sockname");
    public static final ELispSymbol INTERNAL__TEXT_QUOTING_FLAG = new ELispSymbol("internal--text-quoting-flag");
    public static final ELispSymbol INTERNAL__TOP_LEVEL_MESSAGE = new ELispSymbol("internal--top-level-message");
    public static final ELispSymbol INTERVALS_CONSED = new ELispSymbol("intervals-consed");
    public static final ELispSymbol INVERSE_VIDEO = new ELispSymbol("inverse-video");
    public static final ELispSymbol INVOCATION_DIRECTORY = new ELispSymbol("invocation-directory");
    public static final ELispSymbol INVOCATION_NAME = new ELispSymbol("invocation-name");
    public static final ELispSymbol KEYBOARD_TRANSLATE_TABLE = new ELispSymbol("keyboard-translate-table");
    public static final ELispSymbol KEY_TRANSLATION_MAP = new ELispSymbol("key-translation-map");
    public static final ELispSymbol KILL_BUFFER_DELETE_AUTO_SAVE_FILES = new ELispSymbol("kill-buffer-delete-auto-save-files");
    public static final ELispSymbol LARGE_HSCROLL_THRESHOLD = new ELispSymbol("large-hscroll-threshold");
    public static final ELispSymbol LAST_CODE_CONVERSION_ERROR = new ELispSymbol("last-code-conversion-error");
    public static final ELispSymbol LAST_CODING_SYSTEM_USED = new ELispSymbol("last-coding-system-used");
    public static final ELispSymbol LAST_COMMAND = new ELispSymbol("last-command");
    public static final ELispSymbol LAST_COMMAND_EVENT = new ELispSymbol("last-command-event");
    public static final ELispSymbol LAST_EVENT_DEVICE = new ELispSymbol("last-event-device");
    public static final ELispSymbol LAST_EVENT_FRAME = new ELispSymbol("last-event-frame");
    public static final ELispSymbol LAST_INPUT_EVENT = new ELispSymbol("last-input-event");
    public static final ELispSymbol LAST_KBD_MACRO = new ELispSymbol("last-kbd-macro");
    public static final ELispSymbol LAST_PREFIX_ARG = new ELispSymbol("last-prefix-arg");
    public static final ELispSymbol LAST_REPEATABLE_COMMAND = new ELispSymbol("last-repeatable-command");
    public static final ELispSymbol LATIN_EXTRA_CODE_TABLE = new ELispSymbol("latin-extra-code-table");
    public static final ELispSymbol LINE_NUMBER_DISPLAY_LIMIT = new ELispSymbol("line-number-display-limit");
    public static final ELispSymbol LINE_NUMBER_DISPLAY_LIMIT_WIDTH = new ELispSymbol("line-number-display-limit-width");
    public static final ELispSymbol LISP_EVAL_DEPTH_RESERVE = new ELispSymbol("lisp-eval-depth-reserve");
    public static final ELispSymbol LOAD_CONVERT_TO_UNIBYTE = new ELispSymbol("load-convert-to-unibyte");
    public static final ELispSymbol LOAD_DANGEROUS_LIBRARIES = new ELispSymbol("load-dangerous-libraries");
    public static final ELispSymbol LOAD_FILE_REP_SUFFIXES = new ELispSymbol("load-file-rep-suffixes");
    public static final ELispSymbol LOAD_HISTORY = new ELispSymbol("load-history");
    public static final ELispSymbol LOAD_NO_NATIVE = new ELispSymbol("load-no-native");
    public static final ELispSymbol LOAD_PATH = new ELispSymbol("load-path");
    public static final ELispSymbol LOAD_PREFER_NEWER = new ELispSymbol("load-prefer-newer");
    public static final ELispSymbol LOAD_READ_FUNCTION = new ELispSymbol("load-read-function");
    public static final ELispSymbol LOAD_SOURCE_FILE_FUNCTION = new ELispSymbol("load-source-file-function");
    public static final ELispSymbol LOAD_SUFFIXES = new ELispSymbol("load-suffixes");
    public static final ELispSymbol LOCALE_CODING_SYSTEM = new ELispSymbol("locale-coding-system");
    public static final ELispSymbol LOCAL_FUNCTION_KEY_MAP = new ELispSymbol("local-function-key-map");
    public static final ELispSymbol LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT = new ELispSymbol("long-line-optimizations-bol-search-limit");
    public static final ELispSymbol LONG_LINE_OPTIMIZATIONS_REGION_SIZE = new ELispSymbol("long-line-optimizations-region-size");
    public static final ELispSymbol LONG_LINE_THRESHOLD = new ELispSymbol("long-line-threshold");
    public static final ELispSymbol LUCID__MENU_GRAB_KEYBOARD = new ELispSymbol("lucid--menu-grab-keyboard");
    public static final ELispSymbol MAKE_POINTER_INVISIBLE = new ELispSymbol("make-pointer-invisible");
    public static final ELispSymbol MARK_EVEN_IF_INACTIVE = new ELispSymbol("mark-even-if-inactive");
    public static final ELispSymbol MAXIMUM_SCROLL_MARGIN = new ELispSymbol("maximum-scroll-margin");
    public static final ELispSymbol MAX_LISP_EVAL_DEPTH = new ELispSymbol("max-lisp-eval-depth");
    public static final ELispSymbol MAX_MINI_WINDOW_HEIGHT = new ELispSymbol("max-mini-window-height");
    public static final ELispSymbol MAX_REDISPLAY_TICKS = new ELispSymbol("max-redisplay-ticks");
    public static final ELispSymbol MEMORY_FULL = new ELispSymbol("memory-full");
    public static final ELispSymbol MEMORY_SIGNAL_DATA = new ELispSymbol("memory-signal-data");
    public static final ELispSymbol MENU_BAR_FINAL_ITEMS = new ELispSymbol("menu-bar-final-items");
    public static final ELispSymbol MENU_BAR_MODE = new ELispSymbol("menu-bar-mode");
    public static final ELispSymbol MENU_PROMPTING = new ELispSymbol("menu-prompting");
    public static final ELispSymbol MENU_PROMPT_MORE_CHAR = new ELispSymbol("menu-prompt-more-char");
    public static final ELispSymbol MENU_UPDATING_FRAME = new ELispSymbol("menu-updating-frame");
    public static final ELispSymbol MESSAGES_BUFFER_NAME = new ELispSymbol("messages-buffer-name");
    public static final ELispSymbol MESSAGE_LOG_MAX = new ELispSymbol("message-log-max");
    public static final ELispSymbol MESSAGE_TRUNCATE_LINES = new ELispSymbol("message-truncate-lines");
    public static final ELispSymbol META_PREFIX_CHAR = new ELispSymbol("meta-prefix-char");
    public static final ELispSymbol MINIBUFFER_ALLOW_TEXT_PROPERTIES = new ELispSymbol("minibuffer-allow-text-properties");
    public static final ELispSymbol MINIBUFFER_AUTO_RAISE = new ELispSymbol("minibuffer-auto-raise");
    public static final ELispSymbol MINIBUFFER_COMPLETION_CONFIRM = new ELispSymbol("minibuffer-completion-confirm");
    public static final ELispSymbol MINIBUFFER_COMPLETION_PREDICATE = new ELispSymbol("minibuffer-completion-predicate");
    public static final ELispSymbol MINIBUFFER_HELP_FORM = new ELispSymbol("minibuffer-help-form");
    public static final ELispSymbol MINIBUFFER_HISTORY_POSITION = new ELispSymbol("minibuffer-history-position");
    public static final ELispSymbol MINIBUFFER_HISTORY_VARIABLE = new ELispSymbol("minibuffer-history-variable");
    public static final ELispSymbol MINIBUFFER_LOCAL_MAP = new ELispSymbol("minibuffer-local-map");
    public static final ELispSymbol MINIBUFFER_MESSAGE_TIMEOUT = new ELispSymbol("minibuffer-message-timeout");
    public static final ELispSymbol MINIBUFFER_PROMPT_PROPERTIES = new ELispSymbol("minibuffer-prompt-properties");
    public static final ELispSymbol MINIBUFFER_SCROLL_WINDOW = new ELispSymbol("minibuffer-scroll-window");
    public static final ELispSymbol MINOR_MODE_MAP_ALIST = new ELispSymbol("minor-mode-map-alist");
    public static final ELispSymbol MINOR_MODE_OVERRIDING_MAP_ALIST = new ELispSymbol("minor-mode-overriding-map-alist");
    public static final ELispSymbol MODE_LINE_COMPACT = new ELispSymbol("mode-line-compact");
    public static final ELispSymbol MODE_LINE_IN_NON_SELECTED_WINDOWS = new ELispSymbol("mode-line-in-non-selected-windows");
    public static final ELispSymbol MODULE_FILE_SUFFIX = new ELispSymbol("module-file-suffix");
    public static final ELispSymbol MOST_NEGATIVE_FIXNUM = new ELispSymbol("most-negative-fixnum");
    public static final ELispSymbol MOST_POSITIVE_FIXNUM = new ELispSymbol("most-positive-fixnum");
    public static final ELispSymbol MOUSE_AUTOSELECT_WINDOW = new ELispSymbol("mouse-autoselect-window");
    public static final ELispSymbol MOUSE_FINE_GRAINED_TRACKING = new ELispSymbol("mouse-fine-grained-tracking");
    public static final ELispSymbol MOUSE_HIGHLIGHT = new ELispSymbol("mouse-highlight");
    public static final ELispSymbol MOUSE_POSITION_FUNCTION = new ELispSymbol("mouse-position-function");
    public static final ELispSymbol MOUSE_PREFER_CLOSEST_GLYPH = new ELispSymbol("mouse-prefer-closest-glyph");
    public static final ELispSymbol MOVEMAIL_PROGRAM_NAME = new ELispSymbol("movemail-program-name");
    public static final ELispSymbol MOVE_FRAME_FUNCTIONS = new ELispSymbol("move-frame-functions");
    public static final ELispSymbol MULTIBYTE_SYNTAX_AS_SYMBOL = new ELispSymbol("multibyte-syntax-as-symbol");
    public static final ELispSymbol MULTIPLE_FRAMES = new ELispSymbol("multiple-frames");
    public static final ELispSymbol MWHEEL_COALESCE_SCROLL_EVENTS = new ELispSymbol("mwheel-coalesce-scroll-events");
    public static final ELispSymbol NATIVE_COMP_ELN_LOAD_PATH = new ELispSymbol("native-comp-eln-load-path");
    public static final ELispSymbol NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES = new ELispSymbol("native-comp-enable-subr-trampolines");
    public static final ELispSymbol NATIVE_COMP_JIT_COMPILATION = new ELispSymbol("native-comp-jit-compilation");
    public static final ELispSymbol NETWORK_CODING_SYSTEM_ALIST = new ELispSymbol("network-coding-system-alist");
    public static final ELispSymbol NEXT_SCREEN_CONTEXT_LINES = new ELispSymbol("next-screen-context-lines");
    public static final ELispSymbol NOBREAK_CHAR_ASCII_DISPLAY = new ELispSymbol("nobreak-char-ascii-display");
    public static final ELispSymbol NOBREAK_CHAR_DISPLAY = new ELispSymbol("nobreak-char-display");
    public static final ELispSymbol NONINTERACTIVE = new ELispSymbol("noninteractive");
    public static final ELispSymbol NO_REDRAW_ON_REENTER = new ELispSymbol("no-redraw-on-reenter");
    public static final ELispSymbol NUM_INPUT_KEYS = new ELispSymbol("num-input-keys");
    public static final ELispSymbol NUM_NONMACRO_INPUT_EVENTS = new ELispSymbol("num-nonmacro-input-events");
    public static final ELispSymbol OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START = new ELispSymbol("open-paren-in-column-0-is-defun-start");
    public static final ELispSymbol OPERATING_SYSTEM_RELEASE = new ELispSymbol("operating-system-release");
    public static final ELispSymbol OTHER_WINDOW_SCROLL_BUFFER = new ELispSymbol("other-window-scroll-buffer");
    public static final ELispSymbol OTHER_WINDOW_SCROLL_DEFAULT = new ELispSymbol("other-window-scroll-default");
    public static final ELispSymbol OVERLAY_ARROW_POSITION = new ELispSymbol("overlay-arrow-position");
    public static final ELispSymbol OVERLAY_ARROW_VARIABLE_LIST = new ELispSymbol("overlay-arrow-variable-list");
    public static final ELispSymbol OVERLINE_MARGIN = new ELispSymbol("overline-margin");
    public static final ELispSymbol OVERRIDING_LOCAL_MAP_MENU_FLAG = new ELispSymbol("overriding-local-map-menu-flag");
    public static final ELispSymbol PARSE_SEXP_IGNORE_COMMENTS = new ELispSymbol("parse-sexp-ignore-comments");
    public static final ELispSymbol PARSE_SEXP_LOOKUP_PROPERTIES = new ELispSymbol("parse-sexp-lookup-properties");
    public static final ELispSymbol PATH_SEPARATOR = new ELispSymbol("path-separator");
    public static final ELispSymbol PDUMPER_FINGERPRINT = new ELispSymbol("pdumper-fingerprint");
    public static final ELispSymbol PREFIX_ARG = new ELispSymbol("prefix-arg");
    public static final ELispSymbol PREFIX_HELP_COMMAND = new ELispSymbol("prefix-help-command");
    public static final ELispSymbol PRELOADED_FILE_LIST = new ELispSymbol("preloaded-file-list");
    public static final ELispSymbol PRE_REDISPLAY_FUNCTION = new ELispSymbol("pre-redisplay-function");
    public static final ELispSymbol PRINTABLE_CHARS = new ELispSymbol("printable-chars");
    public static final ELispSymbol PRINT_CHARSET_TEXT_PROPERTY = new ELispSymbol("print-charset-text-property");
    public static final ELispSymbol PRINT_CIRCLE = new ELispSymbol("print-circle");
    public static final ELispSymbol PRINT_CONTINUOUS_NUMBERING = new ELispSymbol("print-continuous-numbering");
    public static final ELispSymbol PRINT_ESCAPE_CONTROL_CHARACTERS = new ELispSymbol("print-escape-control-characters");
    public static final ELispSymbol PRINT_ESCAPE_NEWLINES = new ELispSymbol("print-escape-newlines");
    public static final ELispSymbol PRINT_GENSYM = new ELispSymbol("print-gensym");
    public static final ELispSymbol PRINT_INTEGERS_AS_CHARACTERS = new ELispSymbol("print-integers-as-characters");
    public static final ELispSymbol PRINT_LENGTH = new ELispSymbol("print-length");
    public static final ELispSymbol PRINT_LEVEL = new ELispSymbol("print-level");
    public static final ELispSymbol PRINT_NUMBER_TABLE = new ELispSymbol("print-number-table");
    public static final ELispSymbol PRINT_QUOTED = new ELispSymbol("print-quoted");
    public static final ELispSymbol PROCESS_ADAPTIVE_READ_BUFFERING = new ELispSymbol("process-adaptive-read-buffering");
    public static final ELispSymbol PROCESS_CODING_SYSTEM_ALIST = new ELispSymbol("process-coding-system-alist");
    public static final ELispSymbol PROCESS_CONNECTION_TYPE = new ELispSymbol("process-connection-type");
    public static final ELispSymbol PROCESS_ENVIRONMENT = new ELispSymbol("process-environment");
    public static final ELispSymbol PROCESS_ERROR_PAUSE_TIME = new ELispSymbol("process-error-pause-time");
    public static final ELispSymbol PROCESS_PRIORITIZE_LOWER_FDS = new ELispSymbol("process-prioritize-lower-fds");
    public static final ELispSymbol PURE_BYTES_USED = new ELispSymbol("pure-bytes-used");
    public static final ELispSymbol PURIFY_FLAG = new ELispSymbol("purify-flag");
    public static final ELispSymbol QUIT_FLAG = new ELispSymbol("quit-flag");
    public static final ELispSymbol RCS2LOG_PROGRAM_NAME = new ELispSymbol("rcs2log-program-name");
    public static final ELispSymbol READ_BUFFER_COMPLETION_IGNORE_CASE = new ELispSymbol("read-buffer-completion-ignore-case");
    public static final ELispSymbol READ_BUFFER_FUNCTION = new ELispSymbol("read-buffer-function");
    public static final ELispSymbol READ_CIRCLE = new ELispSymbol("read-circle");
    public static final ELispSymbol READ_EXPRESSION_HISTORY = new ELispSymbol("read-expression-history");
    public static final ELispSymbol READ_HIDE_CHAR = new ELispSymbol("read-hide-char");
    public static final ELispSymbol READ_MINIBUFFER_RESTORE_WINDOWS = new ELispSymbol("read-minibuffer-restore-windows");
    public static final ELispSymbol READ_PROCESS_OUTPUT_MAX = new ELispSymbol("read-process-output-max");
    public static final ELispSymbol READ_SYMBOL_SHORTHANDS = new ELispSymbol("read-symbol-shorthands");
    public static final ELispSymbol REAL_LAST_COMMAND = new ELispSymbol("real-last-command");
    public static final ELispSymbol RECENTER_REDISPLAY = new ELispSymbol("recenter-redisplay");
    public static final ELispSymbol RECORD_ALL_KEYS = new ELispSymbol("record-all-keys");
    public static final ELispSymbol REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS = new ELispSymbol("redisplay-adhoc-scroll-in-resize-mini-windows");
    public static final ELispSymbol REDISPLAY_SKIP_FONTIFICATION_ON_INPUT = new ELispSymbol("redisplay-skip-fontification-on-input");
    public static final ELispSymbol REDISPLAY_SKIP_INITIAL_FRAME = new ELispSymbol("redisplay-skip-initial-frame");
    public static final ELispSymbol REDISPLAY__ALL_WINDOWS_CAUSE = new ELispSymbol("redisplay--all-windows-cause");
    public static final ELispSymbol REDISPLAY__INHIBIT_BIDI = new ELispSymbol("redisplay--inhibit-bidi");
    public static final ELispSymbol REDISPLAY__MODE_LINES_CAUSE = new ELispSymbol("redisplay--mode-lines-cause");
    public static final ELispSymbol REGION_EXTRACT_FUNCTION = new ELispSymbol("region-extract-function");
    public static final ELispSymbol REPORT_EMACS_BUG_ADDRESS = new ELispSymbol("report-emacs-bug-address");
    public static final ELispSymbol RESIZE_MINI_FRAMES = new ELispSymbol("resize-mini-frames");
    public static final ELispSymbol RESIZE_MINI_WINDOWS = new ELispSymbol("resize-mini-windows");
    public static final ELispSymbol RING_BELL_FUNCTION = new ELispSymbol("ring-bell-function");
    public static final ELispSymbol SAVED_REGION_SELECTION = new ELispSymbol("saved-region-selection");
    public static final ELispSymbol SCALABLE_FONTS_ALLOWED = new ELispSymbol("scalable-fonts-allowed");
    public static final ELispSymbol SCRIPT_REPRESENTATIVE_CHARS = new ELispSymbol("script-representative-chars");
    public static final ELispSymbol SCROLL_BAR_ADJUST_THUMB_PORTION = new ELispSymbol("scroll-bar-adjust-thumb-portion");
    public static final ELispSymbol SCROLL_CONSERVATIVELY = new ELispSymbol("scroll-conservatively");
    public static final ELispSymbol SCROLL_MARGIN = new ELispSymbol("scroll-margin");
    public static final ELispSymbol SCROLL_MINIBUFFER_CONSERVATIVELY = new ELispSymbol("scroll-minibuffer-conservatively");
    public static final ELispSymbol SCROLL_PRESERVE_SCREEN_POSITION = new ELispSymbol("scroll-preserve-screen-position");
    public static final ELispSymbol SCROLL_STEP = new ELispSymbol("scroll-step");
    public static final ELispSymbol SEARCH_SPACES_REGEXP = new ELispSymbol("search-spaces-regexp");
    public static final ELispSymbol SELECTION_INHIBIT_UPDATE_COMMANDS = new ELispSymbol("selection-inhibit-update-commands");
    public static final ELispSymbol SELECT_ACTIVE_REGIONS = new ELispSymbol("select-active-regions");
    public static final ELispSymbol SELECT_SAFE_CODING_SYSTEM_FUNCTION = new ELispSymbol("select-safe-coding-system-function");
    public static final ELispSymbol SET_AUTO_CODING_FUNCTION = new ELispSymbol("set-auto-coding-function");
    public static final ELispSymbol SET_MESSAGE_FUNCTION = new ELispSymbol("set-message-function");
    public static final ELispSymbol SHARED_GAME_SCORE_DIRECTORY = new ELispSymbol("shared-game-score-directory");
    public static final ELispSymbol SHELL_FILE_NAME = new ELispSymbol("shell-file-name");
    public static final ELispSymbol SHOW_HELP_FUNCTION = new ELispSymbol("show-help-function");
    public static final ELispSymbol SHOW_TRAILING_WHITESPACE = new ELispSymbol("show-trailing-whitespace");
    public static final ELispSymbol SIGNAL_HOOK_FUNCTION = new ELispSymbol("signal-hook-function");
    public static final ELispSymbol SOURCE_DIRECTORY = new ELispSymbol("source-directory");
    public static final ELispSymbol SPECIAL_EVENT_MAP = new ELispSymbol("special-event-map");
    public static final ELispSymbol STANDARD_DISPLAY_TABLE = new ELispSymbol("standard-display-table");
    public static final ELispSymbol STANDARD_TRANSLATION_TABLE_FOR_DECODE = new ELispSymbol("standard-translation-table-for-decode");
    public static final ELispSymbol STANDARD_TRANSLATION_TABLE_FOR_ENCODE = new ELispSymbol("standard-translation-table-for-encode");
    public static final ELispSymbol STRINGS_CONSED = new ELispSymbol("strings-consed");
    public static final ELispSymbol STRING_CHARS_CONSED = new ELispSymbol("string-chars-consed");
    public static final ELispSymbol SYMBOLS_CONSED = new ELispSymbol("symbols-consed");
    public static final ELispSymbol SYNTAX_PROPERTIZE__DONE = new ELispSymbol("syntax-propertize--done");
    public static final ELispSymbol SYSTEM_CONFIGURATION = new ELispSymbol("system-configuration");
    public static final ELispSymbol SYSTEM_CONFIGURATION_FEATURES = new ELispSymbol("system-configuration-features");
    public static final ELispSymbol SYSTEM_CONFIGURATION_OPTIONS = new ELispSymbol("system-configuration-options");
    public static final ELispSymbol SYSTEM_KEY_ALIST = new ELispSymbol("system-key-alist");
    public static final ELispSymbol SYSTEM_MESSAGES_LOCALE = new ELispSymbol("system-messages-locale");
    public static final ELispSymbol SYSTEM_NAME = new ELispSymbol("system-name");
    public static final ELispSymbol SYSTEM_TIME_LOCALE = new ELispSymbol("system-time-locale");
    public static final ELispSymbol SYSTEM_TYPE = new ELispSymbol("system-type");
    public static final ELispSymbol TAB_BAR_BORDER = new ELispSymbol("tab-bar-border");
    public static final ELispSymbol TAB_BAR_BUTTON_MARGIN = new ELispSymbol("tab-bar-button-margin");
    public static final ELispSymbol TAB_BAR_BUTTON_RELIEF = new ELispSymbol("tab-bar-button-relief");
    public static final ELispSymbol TAB_BAR_MODE = new ELispSymbol("tab-bar-mode");
    public static final ELispSymbol TAB_BAR_POSITION = new ELispSymbol("tab-bar-position");
    public static final ELispSymbol TAB_BAR_SEPARATOR_IMAGE_EXPRESSION = new ELispSymbol("tab-bar-separator-image-expression");
    public static final ELispSymbol TAB_BAR__DRAGGING_IN_PROGRESS = new ELispSymbol("tab-bar--dragging-in-progress");
    public static final ELispSymbol TEMPORARY_FILE_DIRECTORY = new ELispSymbol("temporary-file-directory");
    public static final ELispSymbol TEMP_BUFFER_SHOW_FUNCTION = new ELispSymbol("temp-buffer-show-function");
    public static final ELispSymbol TEXT_PROPERTY_DEFAULT_NONSTICKY = new ELispSymbol("text-property-default-nonsticky");
    public static final ELispSymbol TEXT_QUOTING_STYLE = new ELispSymbol("text-quoting-style");
    public static final ELispSymbol THIS_COMMAND = new ELispSymbol("this-command");
    public static final ELispSymbol THIS_COMMAND_KEYS_SHIFT_TRANSLATED = new ELispSymbol("this-command-keys-shift-translated");
    public static final ELispSymbol THIS_ORIGINAL_COMMAND = new ELispSymbol("this-original-command");
    public static final ELispSymbol THROW_ON_INPUT = new ELispSymbol("throw-on-input");
    public static final ELispSymbol TIMER_IDLE_LIST = new ELispSymbol("timer-idle-list");
    public static final ELispSymbol TIMER_LIST = new ELispSymbol("timer-list");
    public static final ELispSymbol TOOLTIP_REUSE_HIDDEN_FRAME = new ELispSymbol("tooltip-reuse-hidden-frame");
    public static final ELispSymbol TOOL_BAR_BORDER = new ELispSymbol("tool-bar-border");
    public static final ELispSymbol TOOL_BAR_BUTTON_MARGIN = new ELispSymbol("tool-bar-button-margin");
    public static final ELispSymbol TOOL_BAR_BUTTON_RELIEF = new ELispSymbol("tool-bar-button-relief");
    public static final ELispSymbol TOOL_BAR_MAX_LABEL_SIZE = new ELispSymbol("tool-bar-max-label-size");
    public static final ELispSymbol TOOL_BAR_MODE = new ELispSymbol("tool-bar-mode");
    public static final ELispSymbol TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION = new ELispSymbol("tool-bar-separator-image-expression");
    public static final ELispSymbol TOOL_BAR_STYLE = new ELispSymbol("tool-bar-style");
    public static final ELispSymbol TRACK_MOUSE = new ELispSymbol("track-mouse");
    public static final ELispSymbol TRANSIENT_MARK_MODE = new ELispSymbol("transient-mark-mode");
    public static final ELispSymbol TRANSLATE_UPPER_CASE_KEY_BINDINGS = new ELispSymbol("translate-upper-case-key-bindings");
    public static final ELispSymbol TRANSLATION_HASH_TABLE_VECTOR = new ELispSymbol("translation-hash-table-vector");
    public static final ELispSymbol TRANSLATION_TABLE_FOR_INPUT = new ELispSymbol("translation-table-for-input");
    public static final ELispSymbol TRANSLATION_TABLE_VECTOR = new ELispSymbol("translation-table-vector");
    public static final ELispSymbol TREESIT_EXTRA_LOAD_PATH = new ELispSymbol("treesit-extra-load-path");
    public static final ELispSymbol TREESIT_LOAD_NAME_OVERRIDE_LIST = new ELispSymbol("treesit-load-name-override-list");
    public static final ELispSymbol TREESIT_THING_SETTINGS = new ELispSymbol("treesit-thing-settings");
    public static final ELispSymbol TRUNCATE_PARTIAL_WIDTH_WINDOWS = new ELispSymbol("truncate-partial-width-windows");
    public static final ELispSymbol TTY_ERASE_CHAR = new ELispSymbol("tty-erase-char");
    public static final ELispSymbol UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT = new ELispSymbol("unibyte-display-via-language-environment");
    public static final ELispSymbol UNICODE_CATEGORY_TABLE = new ELispSymbol("unicode-category-table");
    public static final ELispSymbol UNREAD_COMMAND_EVENTS = new ELispSymbol("unread-command-events");
    public static final ELispSymbol UNREAD_INPUT_METHOD_EVENTS = new ELispSymbol("unread-input-method-events");
    public static final ELispSymbol UNREAD_POST_INPUT_METHOD_EVENTS = new ELispSymbol("unread-post-input-method-events");
    public static final ELispSymbol USER_FULL_NAME = new ELispSymbol("user-full-name");
    public static final ELispSymbol USER_INIT_FILE = new ELispSymbol("user-init-file");
    public static final ELispSymbol USER_LOGIN_NAME = new ELispSymbol("user-login-name");
    public static final ELispSymbol USER_REAL_LOGIN_NAME = new ELispSymbol("user-real-login-name");
    public static final ELispSymbol USE_DIALOG_BOX = new ELispSymbol("use-dialog-box");
    public static final ELispSymbol USE_FILE_DIALOG = new ELispSymbol("use-file-dialog");
    public static final ELispSymbol USE_SHORT_ANSWERS = new ELispSymbol("use-short-answers");
    public static final ELispSymbol USE_SYSTEM_TOOLTIPS = new ELispSymbol("use-system-tooltips");
    public static final ELispSymbol VALUES = new ELispSymbol("values");
    public static final ELispSymbol VECTOR_CELLS_CONSED = new ELispSymbol("vector-cells-consed");
    public static final ELispSymbol VISIBLE_BELL = new ELispSymbol("visible-bell");
    public static final ELispSymbol VOID_TEXT_AREA_POINTER = new ELispSymbol("void-text-area-pointer");
    public static final ELispSymbol WHERE_IS_PREFERRED_MODIFIER = new ELispSymbol("where-is-preferred-modifier");
    public static final ELispSymbol WHILE_NO_INPUT_IGNORE_EVENTS = new ELispSymbol("while-no-input-ignore-events");
    public static final ELispSymbol WINDOW_COMBINATION_LIMIT = new ELispSymbol("window-combination-limit");
    public static final ELispSymbol WINDOW_COMBINATION_RESIZE = new ELispSymbol("window-combination-resize");
    public static final ELispSymbol WINDOW_DEAD_WINDOWS_TABLE = new ELispSymbol("window-dead-windows-table");
    public static final ELispSymbol WINDOW_PERSISTENT_PARAMETERS = new ELispSymbol("window-persistent-parameters");
    public static final ELispSymbol WINDOW_RESIZE_PIXELWISE = new ELispSymbol("window-resize-pixelwise");
    public static final ELispSymbol WINDOW_RESTORE_KILLED_BUFFER_WINDOWS = new ELispSymbol("window-restore-killed-buffer-windows");
    public static final ELispSymbol WINDOW_SYSTEM = new ELispSymbol("window-system");
    public static final ELispSymbol WORDS_INCLUDE_ESCAPES = new ELispSymbol("words-include-escapes");
    public static final ELispSymbol WORD_COMBINING_CATEGORIES = new ELispSymbol("word-combining-categories");
    public static final ELispSymbol WORD_SEPARATING_CATEGORIES = new ELispSymbol("word-separating-categories");
    public static final ELispSymbol WORD_WRAP_BY_CATEGORY = new ELispSymbol("word-wrap-by-category");
    public static final ELispSymbol WRITE_REGION_ANNOTATIONS_SO_FAR = new ELispSymbol("write-region-annotations-so-far");
    public static final ELispSymbol WRITE_REGION_INHIBIT_FSYNC = new ELispSymbol("write-region-inhibit-fsync");
    public static final ELispSymbol WRITE_REGION_POST_ANNOTATION_FUNCTION = new ELispSymbol("write-region-post-annotation-function");
    public static final ELispSymbol X_RESOURCE_CLASS = new ELispSymbol("x-resource-class");
    public static final ELispSymbol X_SHOW_TOOLTIP_TIMEOUT = new ELispSymbol("x-show-tooltip-timeout");
    public static final ELispSymbol X_STRETCH_CURSOR = new ELispSymbol("x-stretch-cursor");
    public static final ELispSymbol YES_OR_NO_PROMPT = new ELispSymbol("yes-or-no-prompt");
    public static final ELispSymbol _TERMINAL_FRAME = new ELispSymbol("terminal-frame");
    private static ELispSymbol[] variableSymbols() {
        return new ELispSymbol[] {
            AFTER_INIT_TIME,
            AFTER_INSERT_FILE_FUNCTIONS,
            AFTER_LOAD_ALIST,
            AMBIGUOUS_WIDTH_CHARS,
            ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL,
            ATTEMPT_STACK_OVERFLOW_RECOVERY,
            AUTO_COMPOSITION_EMOJI_ELIGIBLE_CODEPOINTS,
            AUTO_COMPOSITION_FUNCTION,
            AUTO_COMPOSITION_MODE,
            AUTO_RAISE_TAB_BAR_BUTTONS,
            AUTO_RAISE_TOOL_BAR_BUTTONS,
            AUTO_RESIZE_TAB_BARS,
            AUTO_RESIZE_TOOL_BARS,
            AUTO_SAVE_INCLUDE_BIG_DELETIONS,
            AUTO_SAVE_INTERVAL,
            AUTO_SAVE_LIST_FILE_NAME,
            AUTO_SAVE_NO_MESSAGE,
            AUTO_SAVE_TIMEOUT,
            AUTO_SAVE_VISITED_FILE_NAME,
            AUTO_WINDOW_VSCROLL,
            BACKTRACE_ON_ERROR_NONINTERACTIVE,
            BACKTRACE_ON_REDISPLAY_ERROR,
            BAUD_RATE,
            BEFORE_INIT_TIME,
            BIDI_INHIBIT_BPA,
            BINARY_AS_UNSIGNED,
            BLINK_CURSOR_ALIST,
            BUFFER_ACCESS_FONTIFIED_PROPERTY,
            BUILD_FILES,
            BYTECOMP_VERSION_REGEXP,
            BYTE_BOOLEAN_VARS,
            BYTE_METERING_ON,
            CANNOT_SUSPEND,
            CHARSET_LIST,
            CHARSET_MAP_PATH,
            CHARSET_REVISION_TABLE,
            CHAR_CODE_PROPERTY_ALIST,
            CHAR_PROPERTY_ALIAS_ALIST,
            CHAR_WIDTH_TABLE,
            CLEAR_MESSAGE_FUNCTION,
            CODE_CONVERSION_MAP_VECTOR,
            CODING_CATEGORY_LIST,
            CODING_SYSTEM_ALIST,
            CODING_SYSTEM_FOR_READ,
            CODING_SYSTEM_LIST,
            CODING_SYSTEM_REQUIRE_WARNING,
            COMMAND_ERROR_FUNCTION,
            COMMAND_LINE_ARGS,
            COMMENT_USE_SYNTAX_PPSS,
            COMPLETION_IGNORED_EXTENSIONS,
            COMPLETION_REGEXP_LIST,
            COMPOSE_CHARS_AFTER_FUNCTION,
            COMPOSITION_BREAK_AT_POINT,
            COMPOSITION_FUNCTION_TABLE,
            COMP_ABI_HASH,
            COMP_CTXT,
            COMP_DEFERRED_PENDING_H,
            COMP_ELN_TO_EL_H,
            COMP_FILE_PRELOADED_P,
            COMP_INSTALLED_TRAMPOLINES_H,
            COMP_LOADED_COMP_UNITS_H,
            COMP_NATIVE_VERSION_DIR,
            COMP_NO_NATIVE_FILE_H,
            COMP_SANITIZER_ACTIVE,
            COMP_SUBR_ARITIES_H,
            COMP_SUBR_LIST,
            CONFIGURE_INFO_DIRECTORY,
            CONS_CELLS_CONSED,
            CREATE_LOCKFILES,
            CTAGS_PROGRAM_NAME,
            CURRENT_ISO639_LANGUAGE,
            CURRENT_PREFIX_ARG,
            CURRENT_TIME_LIST,
            DATA_DIRECTORY,
            DEBUGGER_STACK_FRAME_AS_LIST,
            DEBUG_END_POS,
            DEBUG_IGNORED_ERRORS,
            DEBUG_ON_ERROR,
            DEBUG_ON_EVENT,
            DEBUG_ON_MESSAGE,
            DEBUG_ON_NEXT_CALL,
            DEBUG_ON_QUIT,
            DEBUG_ON_SIGNAL,
            DEFAULT_FILE_NAME_CODING_SYSTEM,
            DEFAULT_FRAME_ALIST,
            DEFAULT_FRAME_SCROLL_BARS,
            DEFAULT_MINIBUFFER_FRAME,
            DEFAULT_PROCESS_CODING_SYSTEM,
            DEFAULT_TEXT_PROPERTIES,
            DEFINING_KBD_MACRO,
            DELAYED_WARNINGS_LIST,
            DELETE_AUTO_SAVE_FILES,
            DELETE_EXITED_PROCESSES,
            DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES,
            DISABLE_ASCII_OPTIMIZATION,
            DISABLE_INHIBIT_TEXT_CONVERSION,
            DISABLE_POINT_ADJUSTMENT,
            DISPLAY_HOURGLASS,
            DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE,
            DISPLAY_LINE_NUMBERS_MAJOR_TICK,
            DISPLAY_LINE_NUMBERS_MINOR_TICK,
            DISPLAY_PIXELS_PER_INCH,
            DISPLAY_RAW_BYTES_AS_HEX,
            DOC_DIRECTORY,
            DOUBLE_CLICK_FUZZ,
            DOUBLE_CLICK_TIME,
            DUMP_MODE,
            DYNAMIC_LIBRARY_ALIST,
            DYNAMIC_LIBRARY_SUFFIXES,
            EBROWSE_PROGRAM_NAME,
            ECHO_KEYSTROKES_HELP,
            EMACSCLIENT_PROGRAM_NAME,
            EMACS_COPYRIGHT,
            EMACS_VERSION,
            EMULATION_MODE_MAP_ALISTS,
            ENABLE_CHARACTER_TRANSLATION,
            ENABLE_DISABLED_MENUS_AND_BUTTONS,
            EOL_MNEMONIC_DOS,
            EOL_MNEMONIC_MAC,
            EOL_MNEMONIC_UNDECIDED,
            EOL_MNEMONIC_UNIX,
            ETAGS_PROGRAM_NAME,
            EXECUTING_KBD_MACRO,
            EXECUTING_KBD_MACRO_INDEX,
            EXEC_DIRECTORY,
            EXEC_PATH,
            EXEC_SUFFIXES,
            EXTRA_KEYBOARD_MODIFIERS,
            FACE_DEFAULT_STIPPLE,
            FACE_FILTERS_ALWAYS_MATCH,
            FACE_FONT_LAX_MATCHED_ATTRIBUTES,
            FACE_FONT_RESCALE_ALIST,
            FACE_IGNORED_FONTS,
            FACE_NEAR_SAME_COLOR_THRESHOLD,
            FACE__NEW_FRAME_DEFAULTS,
            FAST_BUT_IMPRECISE_SCROLLING,
            FAST_READ_PROCESS_OUTPUT,
            FILE_CODING_SYSTEM_ALIST,
            FILE_NAME_CODING_SYSTEM,
            FIND_WORD_BOUNDARY_FUNCTION_TABLE,
            FLOATS_CONSED,
            FOCUS_FOLLOWS_MOUSE,
            FONT_CCL_ENCODER_ALIST,
            FORCE_LOAD_MESSAGES,
            FRAME_ALPHA_LOWER_LIMIT,
            FRAME_INHIBIT_IMPLIED_RESIZE,
            FRAME_INTERNAL_PARAMETERS,
            FRAME_RESIZE_PIXELWISE,
            FRAME_SIZE_HISTORY,
            FRAME_TITLE_FORMAT,
            FUNCTION_KEY_MAP,
            GARBAGE_COLLECTION_MESSAGES,
            GCS_DONE,
            GC_ELAPSED,
            GLOBAL_DISABLE_POINT_ADJUSTMENT,
            GLOBAL_MODE_STRING,
            GLYPH_TABLE,
            HELP_CHAR,
            HELP_EVENT_LIST,
            HELP_FORM,
            HEXL_PROGRAM_NAME,
            HIGHLIGHT_NONSELECTED_WINDOWS,
            HISTORY_ADD_NEW_INPUT,
            HISTORY_DELETE_DUPLICATES,
            HISTORY_LENGTH,
            HOURGLASS_DELAY,
            HSCROLL_MARGIN,
            HSCROLL_STEP,
            ICONIFY_CHILD_FRAME,
            ICON_TITLE_FORMAT,
            INDENT_TABS_MODE,
            INHERIT_PROCESS_CODING_SYSTEM,
            INHIBIT_BIDI_MIRRORING,
            INHIBIT_EOL_CONVERSION,
            INHIBIT_FIELD_TEXT_MOTION,
            INHIBIT_FILE_NAME_HANDLERS,
            INHIBIT_INTERACTION,
            INHIBIT_ISO_ESCAPE_DETECTION,
            INHIBIT_LOAD_CHARSET_MAP,
            INHIBIT_MESSAGE,
            INHIBIT_MOUSE_EVENT_CHECK,
            INHIBIT_NULL_BYTE_DETECTION,
            INHIBIT_TRY_CURSOR_MOVEMENT,
            INHIBIT_TRY_WINDOW_ID,
            INHIBIT_TRY_WINDOW_REUSING,
            INHIBIT_X_RESOURCES,
            INHIBIT__RECORD_CHAR,
            INITIAL_ENVIRONMENT,
            INITIAL_WINDOW_SYSTEM,
            INPUT_DECODE_MAP,
            INPUT_METHOD_FUNCTION,
            INPUT_METHOD_PREVIOUS_MESSAGE,
            INPUT_PENDING_P_FILTER_EVENTS,
            INSTALLATION_DIRECTORY,
            INTEGER_WIDTH,
            INTERNAL_DOC_FILE_NAME,
            INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION,
            INTERNAL__DAEMON_SOCKNAME,
            INTERNAL__TEXT_QUOTING_FLAG,
            INTERNAL__TOP_LEVEL_MESSAGE,
            INTERVALS_CONSED,
            INVERSE_VIDEO,
            INVOCATION_DIRECTORY,
            INVOCATION_NAME,
            KEYBOARD_TRANSLATE_TABLE,
            KEY_TRANSLATION_MAP,
            KILL_BUFFER_DELETE_AUTO_SAVE_FILES,
            LARGE_HSCROLL_THRESHOLD,
            LAST_CODE_CONVERSION_ERROR,
            LAST_CODING_SYSTEM_USED,
            LAST_COMMAND,
            LAST_COMMAND_EVENT,
            LAST_EVENT_DEVICE,
            LAST_EVENT_FRAME,
            LAST_INPUT_EVENT,
            LAST_KBD_MACRO,
            LAST_PREFIX_ARG,
            LAST_REPEATABLE_COMMAND,
            LATIN_EXTRA_CODE_TABLE,
            LINE_NUMBER_DISPLAY_LIMIT,
            LINE_NUMBER_DISPLAY_LIMIT_WIDTH,
            LISP_EVAL_DEPTH_RESERVE,
            LOAD_CONVERT_TO_UNIBYTE,
            LOAD_DANGEROUS_LIBRARIES,
            LOAD_FILE_REP_SUFFIXES,
            LOAD_HISTORY,
            LOAD_NO_NATIVE,
            LOAD_PATH,
            LOAD_PREFER_NEWER,
            LOAD_READ_FUNCTION,
            LOAD_SOURCE_FILE_FUNCTION,
            LOAD_SUFFIXES,
            LOCALE_CODING_SYSTEM,
            LOCAL_FUNCTION_KEY_MAP,
            LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT,
            LONG_LINE_OPTIMIZATIONS_REGION_SIZE,
            LONG_LINE_THRESHOLD,
            LUCID__MENU_GRAB_KEYBOARD,
            MAKE_POINTER_INVISIBLE,
            MARK_EVEN_IF_INACTIVE,
            MAXIMUM_SCROLL_MARGIN,
            MAX_LISP_EVAL_DEPTH,
            MAX_MINI_WINDOW_HEIGHT,
            MAX_REDISPLAY_TICKS,
            MEMORY_FULL,
            MEMORY_SIGNAL_DATA,
            MENU_BAR_FINAL_ITEMS,
            MENU_BAR_MODE,
            MENU_PROMPTING,
            MENU_PROMPT_MORE_CHAR,
            MENU_UPDATING_FRAME,
            MESSAGES_BUFFER_NAME,
            MESSAGE_LOG_MAX,
            MESSAGE_TRUNCATE_LINES,
            META_PREFIX_CHAR,
            MINIBUFFER_ALLOW_TEXT_PROPERTIES,
            MINIBUFFER_AUTO_RAISE,
            MINIBUFFER_COMPLETION_CONFIRM,
            MINIBUFFER_COMPLETION_PREDICATE,
            MINIBUFFER_HELP_FORM,
            MINIBUFFER_HISTORY_POSITION,
            MINIBUFFER_HISTORY_VARIABLE,
            MINIBUFFER_LOCAL_MAP,
            MINIBUFFER_MESSAGE_TIMEOUT,
            MINIBUFFER_PROMPT_PROPERTIES,
            MINIBUFFER_SCROLL_WINDOW,
            MINOR_MODE_MAP_ALIST,
            MINOR_MODE_OVERRIDING_MAP_ALIST,
            MODE_LINE_COMPACT,
            MODE_LINE_IN_NON_SELECTED_WINDOWS,
            MODULE_FILE_SUFFIX,
            MOST_NEGATIVE_FIXNUM,
            MOST_POSITIVE_FIXNUM,
            MOUSE_AUTOSELECT_WINDOW,
            MOUSE_FINE_GRAINED_TRACKING,
            MOUSE_HIGHLIGHT,
            MOUSE_POSITION_FUNCTION,
            MOUSE_PREFER_CLOSEST_GLYPH,
            MOVEMAIL_PROGRAM_NAME,
            MOVE_FRAME_FUNCTIONS,
            MULTIBYTE_SYNTAX_AS_SYMBOL,
            MULTIPLE_FRAMES,
            MWHEEL_COALESCE_SCROLL_EVENTS,
            NATIVE_COMP_ELN_LOAD_PATH,
            NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES,
            NATIVE_COMP_JIT_COMPILATION,
            NETWORK_CODING_SYSTEM_ALIST,
            NEXT_SCREEN_CONTEXT_LINES,
            NOBREAK_CHAR_ASCII_DISPLAY,
            NOBREAK_CHAR_DISPLAY,
            NONINTERACTIVE,
            NO_REDRAW_ON_REENTER,
            NUM_INPUT_KEYS,
            NUM_NONMACRO_INPUT_EVENTS,
            OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START,
            OPERATING_SYSTEM_RELEASE,
            OTHER_WINDOW_SCROLL_BUFFER,
            OTHER_WINDOW_SCROLL_DEFAULT,
            OVERLAY_ARROW_POSITION,
            OVERLAY_ARROW_VARIABLE_LIST,
            OVERLINE_MARGIN,
            OVERRIDING_LOCAL_MAP_MENU_FLAG,
            PARSE_SEXP_IGNORE_COMMENTS,
            PARSE_SEXP_LOOKUP_PROPERTIES,
            PATH_SEPARATOR,
            PDUMPER_FINGERPRINT,
            PREFIX_ARG,
            PREFIX_HELP_COMMAND,
            PRELOADED_FILE_LIST,
            PRE_REDISPLAY_FUNCTION,
            PRINTABLE_CHARS,
            PRINT_CHARSET_TEXT_PROPERTY,
            PRINT_CIRCLE,
            PRINT_CONTINUOUS_NUMBERING,
            PRINT_ESCAPE_CONTROL_CHARACTERS,
            PRINT_ESCAPE_NEWLINES,
            PRINT_GENSYM,
            PRINT_INTEGERS_AS_CHARACTERS,
            PRINT_LENGTH,
            PRINT_LEVEL,
            PRINT_NUMBER_TABLE,
            PRINT_QUOTED,
            PROCESS_ADAPTIVE_READ_BUFFERING,
            PROCESS_CODING_SYSTEM_ALIST,
            PROCESS_CONNECTION_TYPE,
            PROCESS_ENVIRONMENT,
            PROCESS_ERROR_PAUSE_TIME,
            PROCESS_PRIORITIZE_LOWER_FDS,
            PURE_BYTES_USED,
            PURIFY_FLAG,
            QUIT_FLAG,
            RCS2LOG_PROGRAM_NAME,
            READ_BUFFER_COMPLETION_IGNORE_CASE,
            READ_BUFFER_FUNCTION,
            READ_CIRCLE,
            READ_EXPRESSION_HISTORY,
            READ_HIDE_CHAR,
            READ_MINIBUFFER_RESTORE_WINDOWS,
            READ_PROCESS_OUTPUT_MAX,
            READ_SYMBOL_SHORTHANDS,
            REAL_LAST_COMMAND,
            RECENTER_REDISPLAY,
            RECORD_ALL_KEYS,
            REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS,
            REDISPLAY_SKIP_FONTIFICATION_ON_INPUT,
            REDISPLAY_SKIP_INITIAL_FRAME,
            REDISPLAY__ALL_WINDOWS_CAUSE,
            REDISPLAY__INHIBIT_BIDI,
            REDISPLAY__MODE_LINES_CAUSE,
            REGION_EXTRACT_FUNCTION,
            REPORT_EMACS_BUG_ADDRESS,
            RESIZE_MINI_FRAMES,
            RESIZE_MINI_WINDOWS,
            RING_BELL_FUNCTION,
            SAVED_REGION_SELECTION,
            SCALABLE_FONTS_ALLOWED,
            SCRIPT_REPRESENTATIVE_CHARS,
            SCROLL_BAR_ADJUST_THUMB_PORTION,
            SCROLL_CONSERVATIVELY,
            SCROLL_MARGIN,
            SCROLL_MINIBUFFER_CONSERVATIVELY,
            SCROLL_PRESERVE_SCREEN_POSITION,
            SCROLL_STEP,
            SEARCH_SPACES_REGEXP,
            SELECTION_INHIBIT_UPDATE_COMMANDS,
            SELECT_ACTIVE_REGIONS,
            SELECT_SAFE_CODING_SYSTEM_FUNCTION,
            SET_AUTO_CODING_FUNCTION,
            SET_MESSAGE_FUNCTION,
            SHARED_GAME_SCORE_DIRECTORY,
            SHELL_FILE_NAME,
            SHOW_HELP_FUNCTION,
            SHOW_TRAILING_WHITESPACE,
            SIGNAL_HOOK_FUNCTION,
            SOURCE_DIRECTORY,
            SPECIAL_EVENT_MAP,
            STANDARD_DISPLAY_TABLE,
            STANDARD_TRANSLATION_TABLE_FOR_DECODE,
            STANDARD_TRANSLATION_TABLE_FOR_ENCODE,
            STRINGS_CONSED,
            STRING_CHARS_CONSED,
            SYMBOLS_CONSED,
            SYNTAX_PROPERTIZE__DONE,
            SYSTEM_CONFIGURATION,
            SYSTEM_CONFIGURATION_FEATURES,
            SYSTEM_CONFIGURATION_OPTIONS,
            SYSTEM_KEY_ALIST,
            SYSTEM_MESSAGES_LOCALE,
            SYSTEM_NAME,
            SYSTEM_TIME_LOCALE,
            SYSTEM_TYPE,
            TAB_BAR_BORDER,
            TAB_BAR_BUTTON_MARGIN,
            TAB_BAR_BUTTON_RELIEF,
            TAB_BAR_MODE,
            TAB_BAR_POSITION,
            TAB_BAR_SEPARATOR_IMAGE_EXPRESSION,
            TAB_BAR__DRAGGING_IN_PROGRESS,
            TEMPORARY_FILE_DIRECTORY,
            TEMP_BUFFER_SHOW_FUNCTION,
            TEXT_PROPERTY_DEFAULT_NONSTICKY,
            TEXT_QUOTING_STYLE,
            THIS_COMMAND,
            THIS_COMMAND_KEYS_SHIFT_TRANSLATED,
            THIS_ORIGINAL_COMMAND,
            THROW_ON_INPUT,
            TIMER_IDLE_LIST,
            TIMER_LIST,
            TOOLTIP_REUSE_HIDDEN_FRAME,
            TOOL_BAR_BORDER,
            TOOL_BAR_BUTTON_MARGIN,
            TOOL_BAR_BUTTON_RELIEF,
            TOOL_BAR_MAX_LABEL_SIZE,
            TOOL_BAR_MODE,
            TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION,
            TOOL_BAR_STYLE,
            TRACK_MOUSE,
            TRANSIENT_MARK_MODE,
            TRANSLATE_UPPER_CASE_KEY_BINDINGS,
            TRANSLATION_HASH_TABLE_VECTOR,
            TRANSLATION_TABLE_FOR_INPUT,
            TRANSLATION_TABLE_VECTOR,
            TREESIT_EXTRA_LOAD_PATH,
            TREESIT_LOAD_NAME_OVERRIDE_LIST,
            TREESIT_THING_SETTINGS,
            TRUNCATE_PARTIAL_WIDTH_WINDOWS,
            TTY_ERASE_CHAR,
            UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT,
            UNICODE_CATEGORY_TABLE,
            UNREAD_COMMAND_EVENTS,
            UNREAD_INPUT_METHOD_EVENTS,
            UNREAD_POST_INPUT_METHOD_EVENTS,
            USER_FULL_NAME,
            USER_INIT_FILE,
            USER_LOGIN_NAME,
            USER_REAL_LOGIN_NAME,
            USE_DIALOG_BOX,
            USE_FILE_DIALOG,
            USE_SHORT_ANSWERS,
            USE_SYSTEM_TOOLTIPS,
            VALUES,
            VECTOR_CELLS_CONSED,
            VISIBLE_BELL,
            VOID_TEXT_AREA_POINTER,
            WHERE_IS_PREFERRED_MODIFIER,
            WHILE_NO_INPUT_IGNORE_EVENTS,
            WINDOW_COMBINATION_LIMIT,
            WINDOW_COMBINATION_RESIZE,
            WINDOW_DEAD_WINDOWS_TABLE,
            WINDOW_PERSISTENT_PARAMETERS,
            WINDOW_RESIZE_PIXELWISE,
            WINDOW_RESTORE_KILLED_BUFFER_WINDOWS,
            WINDOW_SYSTEM,
            WORDS_INCLUDE_ESCAPES,
            WORD_COMBINING_CATEGORIES,
            WORD_SEPARATING_CATEGORIES,
            WORD_WRAP_BY_CATEGORY,
            WRITE_REGION_ANNOTATIONS_SO_FAR,
            WRITE_REGION_INHIBIT_FSYNC,
            WRITE_REGION_POST_ANNOTATION_FUNCTION,
            X_RESOURCE_CLASS,
            X_SHOW_TOOLTIP_TIMEOUT,
            X_STRETCH_CURSOR,
            YES_OR_NO_PROMPT,
            _TERMINAL_FRAME,
        };
    }
    //#endregion variable symbols
    //#region buffer.c
    public static final ELispSymbol ABBREV_MODE = new ELispSymbol("abbrev-mode");
    public static final ELispSymbol AUTO_FILL_FUNCTION = new ELispSymbol("auto-fill-function");
    public static final ELispSymbol BIDI_DISPLAY_REORDERING = new ELispSymbol("bidi-display-reordering");
    public static final ELispSymbol BIDI_PARAGRAPH_DIRECTION = new ELispSymbol("bidi-paragraph-direction");
    public static final ELispSymbol BIDI_PARAGRAPH_SEPARATE_RE = new ELispSymbol("bidi-paragraph-separate-re");
    public static final ELispSymbol BIDI_PARAGRAPH_START_RE = new ELispSymbol("bidi-paragraph-start-re");
    public static final ELispSymbol BUFFER_AUTO_SAVE_FILE_FORMAT = new ELispSymbol("buffer-auto-save-file-format");
    public static final ELispSymbol BUFFER_AUTO_SAVE_FILE_NAME = new ELispSymbol("buffer-auto-save-file-name");
    public static final ELispSymbol BUFFER_BACKED_UP = new ELispSymbol("buffer-backed-up");
    public static final ELispSymbol BUFFER_DISPLAY_COUNT = new ELispSymbol("buffer-display-count");
    public static final ELispSymbol BUFFER_DISPLAY_TABLE = new ELispSymbol("buffer-display-table");
    public static final ELispSymbol BUFFER_DISPLAY_TIME = new ELispSymbol("buffer-display-time");
    public static final ELispSymbol BUFFER_FILE_FORMAT = new ELispSymbol("buffer-file-format");
    public static final ELispSymbol BUFFER_FILE_TRUENAME = new ELispSymbol("buffer-file-truename");
    public static final ELispSymbol BUFFER_INVISIBILITY_SPEC = new ELispSymbol("buffer-invisibility-spec");
    public static final ELispSymbol BUFFER_SAVED_SIZE = new ELispSymbol("buffer-saved-size");
    public static final ELispSymbol CACHE_LONG_SCANS = new ELispSymbol("cache-long-scans");
    public static final ELispSymbol CTL_ARROW = new ELispSymbol("ctl-arrow");
    public static final ELispSymbol CURSOR_IN_NON_SELECTED_WINDOWS = new ELispSymbol("cursor-in-non-selected-windows");
    public static final ELispSymbol ENABLE_MULTIBYTE_CHARACTERS = new ELispSymbol("enable-multibyte-characters");
    public static final ELispSymbol FILL_COLUMN = new ELispSymbol("fill-column");
    public static final ELispSymbol FRINGES_OUTSIDE_MARGINS = new ELispSymbol("fringes-outside-margins");
    public static final ELispSymbol FRINGE_CURSOR_ALIST = new ELispSymbol("fringe-cursor-alist");
    public static final ELispSymbol FRINGE_INDICATOR_ALIST = new ELispSymbol("fringe-indicator-alist");
    public static final ELispSymbol INDICATE_BUFFER_BOUNDARIES = new ELispSymbol("indicate-buffer-boundaries");
    public static final ELispSymbol INDICATE_EMPTY_LINES = new ELispSymbol("indicate-empty-lines");
    public static final ELispSymbol LEFT_FRINGE_WIDTH = new ELispSymbol("left-fringe-width");
    public static final ELispSymbol LEFT_MARGIN_WIDTH = new ELispSymbol("left-margin-width");
    public static final ELispSymbol LOCAL_ABBREV_TABLE = new ELispSymbol("local-abbrev-table");
    public static final ELispSymbol LOCAL_MINOR_MODES = new ELispSymbol("local-minor-modes");
    public static final ELispSymbol MAJOR_MODE = new ELispSymbol("major-mode");
    public static final ELispSymbol MARK_ACTIVE = new ELispSymbol("mark-active");
    public static final ELispSymbol MODE_NAME = new ELispSymbol("mode-name");
    public static final ELispSymbol POINT_BEFORE_SCROLL = new ELispSymbol("point-before-scroll");
    public static final ELispSymbol RIGHT_FRINGE_WIDTH = new ELispSymbol("right-fringe-width");
    public static final ELispSymbol RIGHT_MARGIN_WIDTH = new ELispSymbol("right-margin-width");
    public static final ELispSymbol SCROLL_DOWN_AGGRESSIVELY = new ELispSymbol("scroll-down-aggressively");
    public static final ELispSymbol SCROLL_UP_AGGRESSIVELY = new ELispSymbol("scroll-up-aggressively");
    public static final ELispSymbol SELECTIVE_DISPLAY = new ELispSymbol("selective-display");
    public static final ELispSymbol SELECTIVE_DISPLAY_ELLIPSES = new ELispSymbol("selective-display-ellipses");
    public static final ELispSymbol TAB_WIDTH = new ELispSymbol("tab-width");
    public static final ELispSymbol TEXT_CONVERSION_STYLE = new ELispSymbol("text-conversion-style");
    public static final ELispSymbol TRUNCATE_LINES = new ELispSymbol("truncate-lines");
    public static final ELispSymbol WORD_WRAP = new ELispSymbol("word-wrap");
    private static ELispSymbol[] bufferLocalVarSymbols() {
        return new ELispSymbol[] {
            ABBREV_MODE,
            AUTO_FILL_FUNCTION,
            BIDI_DISPLAY_REORDERING,
            BIDI_PARAGRAPH_DIRECTION,
            BIDI_PARAGRAPH_SEPARATE_RE,
            BIDI_PARAGRAPH_START_RE,
            BUFFER_AUTO_SAVE_FILE_FORMAT,
            BUFFER_AUTO_SAVE_FILE_NAME,
            BUFFER_BACKED_UP,
            BUFFER_DISPLAY_COUNT,
            BUFFER_DISPLAY_TABLE,
            BUFFER_DISPLAY_TIME,
            BUFFER_FILE_FORMAT,
            BUFFER_FILE_TRUENAME,
            BUFFER_INVISIBILITY_SPEC,
            BUFFER_SAVED_SIZE,
            CACHE_LONG_SCANS,
            CTL_ARROW,
            CURSOR_IN_NON_SELECTED_WINDOWS,
            ENABLE_MULTIBYTE_CHARACTERS,
            FILL_COLUMN,
            FRINGES_OUTSIDE_MARGINS,
            FRINGE_CURSOR_ALIST,
            FRINGE_INDICATOR_ALIST,
            INDICATE_BUFFER_BOUNDARIES,
            INDICATE_EMPTY_LINES,
            LEFT_FRINGE_WIDTH,
            LEFT_MARGIN_WIDTH,
            LOCAL_ABBREV_TABLE,
            LOCAL_MINOR_MODES,
            MAJOR_MODE,
            MARK_ACTIVE,
            MODE_NAME,
            POINT_BEFORE_SCROLL,
            RIGHT_FRINGE_WIDTH,
            RIGHT_MARGIN_WIDTH,
            SCROLL_DOWN_AGGRESSIVELY,
            SCROLL_UP_AGGRESSIVELY,
            SELECTIVE_DISPLAY,
            SELECTIVE_DISPLAY_ELLIPSES,
            TAB_WIDTH,
            TEXT_CONVERSION_STYLE,
            TRUNCATE_LINES,
            WORD_WRAP,
        };
    }
    //#endregion buffer.c
    //#region getAllSymbols
    public static ELispSymbol[][] getAllSymbols() {
        return new ELispSymbol[][]{
                allSymbols(),
                variableSymbols(),
                bufferLocalVarSymbols(),
        };
    }
    //#endregion getAllSymbols
}
