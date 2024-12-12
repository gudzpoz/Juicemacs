package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInCharTab.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInData.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFileIO.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInKeymap.*;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.io.File;
import java.nio.file.Paths;

import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.getMiniBuffer;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCaseTab.initCasetabOnce;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCharSet.defineCharsetInternal;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCoding.*;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCoding.FDefineCodingSystemInternal.defineCodingSystemInternal;
import static party.iroiro.juicemacs.elisp.forms.BuiltInEmacs.decodeEnvPath;
import static party.iroiro.juicemacs.elisp.forms.BuiltInSyntax.initSyntaxOnce;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;
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
public class ELispGlobals {
    private static ELispString emptyUnibyteString = new ELispString("");

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
    private static Object setAndCheckLoadPath() {
        String path = ELispLanguage.getEnv().get("EMACSLOADPATH");
        ELispCons loadPaths;
        if (path != null) {
            loadPaths = decodeEnvPath(null, path, true);
            loadPathCheck(loadPaths);
            if (!isNil(FMemq.memq(false, loadPaths))) {
                ELispCons defaultPaths = loadPathDefault();
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

    private static void initCharset() {
        ELispString path = FExpandFileName.expandFileName(new ELispString("charsets"), dataDirectory.getValue());
        File dir = Paths.get(path.toString()).toFile();
        if (!(dir.isDirectory() && dir.canRead())) {
            throw ELispSignals.fatal();
        }
        charsetMapPath.setValue(new ELispCons(path));
    }

    //#region initGlobalVariables
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
        compositeVars();
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
    private static final ELispSymbol.Value.ForwardedLong gcConsThreshold = new ELispSymbol.Value.ForwardedLong(800000);
    private static final ELispSymbol.Value.Forwarded gcConsPercentage = new ELispSymbol.Value.Forwarded(0.1);
    private static final ELispSymbol.Value.ForwardedLong pureBytesUsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong consCellsConsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong floatsConsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong vectorCellsConsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong symbolsConsed = new ELispSymbol.Value.ForwardedLong(1568);
    private static final ELispSymbol.Value.ForwardedLong stringCharsConsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong intervalsConsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong stringsConsed = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.Forwarded purifyFlag = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool garbageCollectionMessages = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded postGcHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded memorySignalData = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded memoryFull = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded gcElapsed = new ELispSymbol.Value.Forwarded(0.0);
    private static final ELispSymbol.Value.ForwardedLong gcsDone = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.ForwardedLong integerWidth = new ELispSymbol.Value.ForwardedLong();
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
    private static final ELispSymbol.Value.Forwarded beforeChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded afterChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded firstChangeHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded transientMarkMode = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitReadOnly = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded killBufferQueryFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded changeMajorModeHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded bufferListUpdateHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool killBufferDeleteAutoSaveFiles = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool deleteAutoSaveFiles = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded caseFoldSearch = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded cloneIndirectBufferHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded longLineThreshold = new ELispSymbol.Value.Forwarded(50000L);
    private static final ELispSymbol.Value.ForwardedLong longLineOptimizationsRegionSize = new ELispSymbol.Value.ForwardedLong(500000);
    private static final ELispSymbol.Value.ForwardedLong longLineOptimizationsBolSearchLimit = new ELispSymbol.Value.ForwardedLong(128);
    private static final ELispSymbol.Value.ForwardedLong largeHscrollThreshold = new ELispSymbol.Value.ForwardedLong(10000);
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
    }
    private static final ELispSymbol.Value.Forwarded prefixArg = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded lastPrefixArg = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded currentPrefixArg = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded commandHistory = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded commandDebugStatus = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded markEvenIfInactive = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded mouseLeaveBufferHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitMouseEventCheck = new ELispSymbol.Value.ForwardedBool(false);
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
    private static final ELispSymbol.Value.Forwarded shellFileName = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded execPath = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded execSuffixes = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded execDirectory = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded dataDirectory = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded docDirectory = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded configureInfoDirectory = new ELispSymbol.Value.Forwarded(new ELispString("/usr/share/info"));
    private static final ELispSymbol.Value.Forwarded sharedGameScoreDirectory = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded initialEnvironment = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded processEnvironment = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded ctagsProgramName = new ELispSymbol.Value.Forwarded(new ELispString("ctags"));
    private static final ELispSymbol.Value.Forwarded etagsProgramName = new ELispSymbol.Value.Forwarded(new ELispString("etags"));
    private static final ELispSymbol.Value.Forwarded hexlProgramName = new ELispSymbol.Value.Forwarded(new ELispString("hexl"));
    private static final ELispSymbol.Value.Forwarded emacsclientProgramName = new ELispSymbol.Value.Forwarded(new ELispString("emacsclient"));
    private static final ELispSymbol.Value.Forwarded movemailProgramName = new ELispSymbol.Value.Forwarded(new ELispString("movemail"));
    private static final ELispSymbol.Value.Forwarded ebrowseProgramName = new ELispSymbol.Value.Forwarded(new ELispString("ebrowse"));
    private static final ELispSymbol.Value.Forwarded rcs2logProgramName = new ELispSymbol.Value.Forwarded(new ELispString("rcs2log"));
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
    private static final ELispSymbol.Value.Forwarded regionExtractFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool caseSymbolsAsWords = new ELispSymbol.Value.ForwardedBool(false);
    private static void casefiddleVars() {
        REGION_EXTRACT_FUNCTION.initForwardTo(regionExtractFunction);
        CASE_SYMBOLS_AS_WORDS.initForwardTo(caseSymbolsAsWords);
    }

    private static void casetabVars() {

    }
    private static final ELispSymbol.Value.Forwarded wordCombiningCategories = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded wordSeparatingCategories = new ELispSymbol.Value.Forwarded(false);
    private static void categoryVars() {
        WORD_COMBINING_CATEGORIES.initForwardTo(wordCombiningCategories);
        WORD_SEPARATING_CATEGORIES.initForwardTo(wordSeparatingCategories);
    }
    private static final ELispSymbol.Value.Forwarded codeConversionMapVector = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded fontCclEncoderAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded translationHashTableVector = new ELispSymbol.Value.Forwarded(false);
    private static void cclVars() {
        CODE_CONVERSION_MAP_VECTOR.initForwardTo(codeConversionMapVector);
        FONT_CCL_ENCODER_ALIST.initForwardTo(fontCclEncoderAlist);
        TRANSLATION_HASH_TABLE_VECTOR.initForwardTo(translationHashTableVector);
    }
    private static final ELispSymbol.Value.Forwarded translationTableVector = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded autoFillChars = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded charWidthTable = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded ambiguousWidthChars = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded printableChars = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded charScriptTable = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded scriptRepresentativeChars = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded unicodeCategoryTable = new ELispSymbol.Value.Forwarded(false);
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
    private static final ELispSymbol.Value.Forwarded charsetMapPath = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitLoadCharsetMap = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded charsetList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded currentIso639Language = new ELispSymbol.Value.Forwarded(false);
    private static void charsetVars() {
        CHARSET_MAP_PATH.initForwardTo(charsetMapPath);
        INHIBIT_LOAD_CHARSET_MAP.initForwardTo(inhibitLoadCharsetMap);
        CHARSET_LIST.initForwardTo(charsetList);
        CURRENT_ISO639_LANGUAGE.initForwardTo(currentIso639Language);
    }
    private static final ELispSymbol.Value.Forwarded charCodePropertyAlist = new ELispSymbol.Value.Forwarded(false);
    private static void chartabVars() {
        CHAR_CODE_PROPERTY_ALIST.initForwardTo(charCodePropertyAlist);
    }
    private static final ELispSymbol.Value.Forwarded postSelfInsertHook = new ELispSymbol.Value.Forwarded(false);
    private static void cmdsVars() {
        POST_SELF_INSERT_HOOK.initForwardTo(postSelfInsertHook);
    }
    private static final ELispSymbol.Value.Forwarded codingSystemList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded codingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded codingCategoryList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded codingSystemForRead = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded codingSystemForWrite = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded lastCodingSystemUsed = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded lastCodeConversionError = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitEolConversion = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool inheritProcessCodingSystem = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded fileCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded processCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded networkCodingSystemAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded localeCodingSystem = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded eolMnemonicUnix = new ELispSymbol.Value.Forwarded(new ELispString(":"));
    private static final ELispSymbol.Value.Forwarded eolMnemonicDos = new ELispSymbol.Value.Forwarded(new ELispString("\\"));
    private static final ELispSymbol.Value.Forwarded eolMnemonicMac = new ELispSymbol.Value.Forwarded(new ELispString("/"));
    private static final ELispSymbol.Value.Forwarded eolMnemonicUndecided = new ELispSymbol.Value.Forwarded(new ELispString(":"));
    private static final ELispSymbol.Value.Forwarded enableCharacterTranslation = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded standardTranslationTableForDecode = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded standardTranslationTableForEncode = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded charsetRevisionTable = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded defaultProcessCodingSystem = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded latinExtraCodeTable = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded selectSafeCodingSystemFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool codingSystemRequireWarning = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitIsoEscapeDetection = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitNullByteDetection = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool disableAsciiOptimization = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded translationTableForInput = new ELispSymbol.Value.Forwarded(false);
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
    private static final ELispSymbol.Value.ForwardedBool nativeCompJitCompilation = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded compCtxt = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded compSubrList = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded compAbiHash = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded compNativeVersionDir = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded compDeferredPendingH = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded compElnToElH = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded nativeCompElnLoadPath = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded nativeCompEnableSubrTrampolines = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded compInstalledTrampolinesH = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded compNoNativeFileH = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool compFilePreloadedP = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.Forwarded compLoadedCompUnitsH = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded compSubrAritiesH = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool compSanitizerActive = new ELispSymbol.Value.ForwardedBool(false);
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
    private static final ELispSymbol.Value.Forwarded composeCharsAfterFunction = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded autoCompositionMode = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded autoCompositionFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded compositionFunctionTable = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded autoCompositionEmojiEligibleCodepoints = new ELispSymbol.Value.Forwarded(false);
    private static void compositeVars() {
        COMPOSE_CHARS_AFTER_FUNCTION.initForwardTo(composeCharsAfterFunction);
        AUTO_COMPOSITION_MODE.initForwardTo(autoCompositionMode);
        AUTO_COMPOSITION_FUNCTION.initForwardTo(autoCompositionFunction);
        COMPOSITION_FUNCTION_TABLE.initForwardTo(compositionFunctionTable);
        AUTO_COMPOSITION_EMOJI_ELIGIBLE_CODEPOINTS.initForwardTo(autoCompositionEmojiEligibleCodepoints);
    }
    private static final ELispSymbol.Value.Forwarded mostPositiveFixnum = new ELispSymbol.Value.Forwarded(2147483647L);
    private static final ELispSymbol.Value.Forwarded mostNegativeFixnum = new ELispSymbol.Value.Forwarded(-2147483648L);
    private static final ELispSymbol.Value.ForwardedBool symbolsWithPosEnabled = new ELispSymbol.Value.ForwardedBool(false);
    private static void dataVars() {
        MOST_POSITIVE_FIXNUM.initForwardTo(mostPositiveFixnum);
        MOST_NEGATIVE_FIXNUM.initForwardTo(mostNegativeFixnum);
        SYMBOLS_WITH_POS_ENABLED.initForwardTo(symbolsWithPosEnabled);
    }
    private static final ELispSymbol.Value.Forwarded internalDocFileName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded buildFiles = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded textQuotingStyle = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool internalTextQuotingFlag = new ELispSymbol.Value.ForwardedBool();
    private static void docVars() {
        INTERNAL_DOC_FILE_NAME.initForwardTo(internalDocFileName);
        BUILD_FILES.initForwardTo(buildFiles);
        TEXT_QUOTING_STYLE.initForwardTo(textQuotingStyle);
        INTERNAL__TEXT_QUOTING_FLAG.initForwardTo(internalTextQuotingFlag);
    }
    private static final ELispSymbol.Value.Forwarded inhibitFieldTextMotion = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded bufferAccessFontifyFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded bufferAccessFontifiedProperty = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded systemName = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded userFullName = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded userLoginName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded userRealLoginName = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded operatingSystemRelease = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool binaryAsUnsigned = new ELispSymbol.Value.ForwardedBool(false);
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
    private static final ELispSymbol.Value.Forwarded commandLineArgs = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded systemType = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded systemConfiguration = new ELispSymbol.Value.Forwarded(new ELispString("x86_64-pc-linux-gnu"));
    private static final ELispSymbol.Value.Forwarded systemConfigurationOptions = new ELispSymbol.Value.Forwarded(new ELispString(""));
    private static final ELispSymbol.Value.Forwarded systemConfigurationFeatures = new ELispSymbol.Value.Forwarded(new ELispString(""));
    private static final ELispSymbol.Value.ForwardedBool noninteractive = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.Forwarded killEmacsHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded pathSeparator = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded invocationName = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded invocationDirectory = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded installationDirectory = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded systemMessagesLocale = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded systemTimeLocale = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded beforeInitTime = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded afterInitTime = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitXResources = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded emacsCopyright = new ELispSymbol.Value.Forwarded(new ELispString("Copyright (C) 2024 Free Software Foundation, Inc."));
    private static final ELispSymbol.Value.Forwarded emacsVersion = new ELispSymbol.Value.Forwarded(new ELispString("31.0.50"));
    private static final ELispSymbol.Value.Forwarded reportEmacsBugAddress = new ELispSymbol.Value.Forwarded(new ELispString(""));
    private static final ELispSymbol.Value.Forwarded dumpMode = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded dynamicLibraryAlist = new ELispSymbol.Value.Forwarded(false);
    private static void emacsVars() {
        COMMAND_LINE_ARGS.initForwardTo(commandLineArgs);
        SYSTEM_TYPE.initForwardTo(systemType);
        SYSTEM_CONFIGURATION.initForwardTo(systemConfiguration);
        SYSTEM_CONFIGURATION_OPTIONS.initForwardTo(systemConfigurationOptions);
        SYSTEM_CONFIGURATION_FEATURES.initForwardTo(systemConfigurationFeatures);
        NONINTERACTIVE.initForwardTo(noninteractive);
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
    private static final ELispSymbol.Value.ForwardedLong maxLispEvalDepth = new ELispSymbol.Value.ForwardedLong(1600);
    private static final ELispSymbol.Value.ForwardedLong lispEvalDepthReserve = new ELispSymbol.Value.ForwardedLong(200);
    private static final ELispSymbol.Value.Forwarded quitFlag = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitQuit = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitDebugger = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded debugOnError = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded debugIgnoredErrors = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool debugOnQuit = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool debugOnNextCall = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.ForwardedBool backtraceOnRedisplayError = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool debuggerMayContinue = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool debuggerStackFrameAsList = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded debugger = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded signalHookFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded debugOnSignal = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool backtraceOnErrorNoninteractive = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedLong internalWhenEnteredDebugger = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.Forwarded internalInterpreterEnvironment = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded internalMakeInterpretedClosureFunction = new ELispSymbol.Value.Forwarded(false);
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
        INTERNAL_WHEN_ENTERED_DEBUGGER.initForwardTo(internalWhenEnteredDebugger);
        INTERNAL_INTERPRETER_ENVIRONMENT.initForwardTo(internalInterpreterEnvironment);
        INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION.initForwardTo(internalMakeInterpretedClosureFunction);
    }
    private static final ELispSymbol.Value.Forwarded fileNameCodingSystem = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded defaultFileNameCodingSystem = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded fileNameHandlerAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded setAutoCodingFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded afterInsertFileFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded writeRegionAnnotateFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded writeRegionPostAnnotationFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded writeRegionAnnotationsSoFar = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitFileNameHandlers = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitFileNameOperation = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded autoSaveListFileName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded autoSaveVisitedFileName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded autoSaveIncludeBigDeletions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool writeRegionInhibitFsync = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool deleteByMovingToTrash = new ELispSymbol.Value.ForwardedBool(false);
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

    private static void floatfnsVars() {

    }
    private static final ELispSymbol.Value.Forwarded overridingPlistEnvironment = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded features = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool useDialogBox = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool useFileDialog = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool useShortAnswers = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded yesOrNoPrompt = new ELispSymbol.Value.Forwarded(new ELispString("(yes or no) "));
    private static void fnsVars() {
        OVERRIDING_PLIST_ENVIRONMENT.initForwardTo(overridingPlistEnvironment);
        FEATURES.initForwardTo(features);
        USE_DIALOG_BOX.initForwardTo(useDialogBox);
        USE_FILE_DIALOG.initForwardTo(useFileDialog);
        USE_SHORT_ANSWERS.initForwardTo(useShortAnswers);
        YES_OR_NO_PROMPT.initForwardTo(yesOrNoPrompt);
    }
    private static final ELispSymbol.Value.Forwarded xResourceName = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded xResourceClass = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded frameAlphaLowerLimit = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded defaultFrameAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded defaultFrameScrollBars = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool scrollBarAdjustThumbPortion = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded TerminalFrame = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded mousePositionFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded mouseHighlight = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded makePointerInvisible = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded moveFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded deleteFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded afterDeleteFrameFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded menuBarMode = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded tabBarMode = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded toolBarMode = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded defaultMinibufferFrame = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded resizeMiniFrames = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded focusFollowsMouse = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool frameResizePixelwise = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded frameInhibitImpliedResize = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded frameSizeHistory = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool tooltipReuseHiddenFrame = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool useSystemTooltips = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded iconifyChildFrame = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded frameInternalParameters = new ELispSymbol.Value.Forwarded();
    private static void frameVars() {
        X_RESOURCE_NAME.initForwardTo(xResourceName);
        X_RESOURCE_CLASS.initForwardTo(xResourceClass);
        FRAME_ALPHA_LOWER_LIMIT.initForwardTo(frameAlphaLowerLimit);
        DEFAULT_FRAME_ALIST.initForwardTo(defaultFrameAlist);
        DEFAULT_FRAME_SCROLL_BARS.initForwardTo(defaultFrameScrollBars);
        SCROLL_BAR_ADJUST_THUMB_PORTION.initForwardTo(scrollBarAdjustThumbPortion);
        _TERMINAL_FRAME.initForwardTo(TerminalFrame);
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
    private static final ELispSymbol.Value.Forwarded internalTopLevelMessage = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded lastCommandEvent = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded lastNonmenuEvent = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded lastInputEvent = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded unreadCommandEvents = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded unreadPostInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded unreadInputMethodEvents = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded metaPrefixChar = new ELispSymbol.Value.Forwarded(27L);
    private static final ELispSymbol.Value.Forwarded lastCommand = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded realLastCommand = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded lastRepeatableCommand = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded thisCommand = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded realThisCommand = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded currentMinibufferCommand = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded thisCommandKeysShiftTranslated = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded thisOriginalCommand = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong autoSaveInterval = new ELispSymbol.Value.ForwardedLong(300);
    private static final ELispSymbol.Value.ForwardedBool autoSaveNoMessage = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded autoSaveTimeout = new ELispSymbol.Value.Forwarded(30L);
    private static final ELispSymbol.Value.Forwarded echoKeystrokes = new ELispSymbol.Value.Forwarded(1L);
    private static final ELispSymbol.Value.ForwardedBool echoKeystrokesHelp = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded pollingPeriod = new ELispSymbol.Value.Forwarded(2.0);
    private static final ELispSymbol.Value.Forwarded doubleClickTime = new ELispSymbol.Value.Forwarded(500L);
    private static final ELispSymbol.Value.ForwardedLong doubleClickFuzz = new ELispSymbol.Value.ForwardedLong(3);
    private static final ELispSymbol.Value.ForwardedLong numInputKeys = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.ForwardedLong numNonmacroInputEvents = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.Forwarded lastEventFrame = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded lastEventDevice = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded ttyEraseChar = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded helpChar = new ELispSymbol.Value.Forwarded(8L);
    private static final ELispSymbol.Value.Forwarded helpEventList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded helpForm = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded prefixHelpCommand = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded topLevel = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded keyboardTranslateTable = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.ForwardedBool cannotSuspend = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool menuPrompting = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded menuPromptMoreChar = new ELispSymbol.Value.Forwarded(32L);
    private static final ELispSymbol.Value.ForwardedLong extraKeyboardModifiers = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.Forwarded deactivateMark = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded preCommandHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded postCommandHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool lucidMenuGrabKeyboard = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.Forwarded menuBarFinalItems = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded tabBarSeparatorImageExpression = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded toolBarSeparatorImageExpression = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded overridingTerminalLocalMap = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded overridingLocalMap = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded overridingLocalMapMenuFlag = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded specialEventMap = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded trackMouse = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded systemKeyAlist = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded localFunctionKeyMap = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded inputDecodeMap = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded functionKeyMap = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded keyTranslationMap = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded delayedWarningsList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded timerList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded timerIdleList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inputMethodFunction = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded inputMethodPreviousMessage = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded showHelpFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded disablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded globalDisablePointAdjustment = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferMessageTimeout = new ELispSymbol.Value.Forwarded(2L);
    private static final ELispSymbol.Value.Forwarded throwOnInput = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded commandErrorFunction = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded enableDisabledMenusAndButtons = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded selectActiveRegions = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded savedRegionSelection = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded selectionInhibitUpdateCommands = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded debugOnEvent = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool attemptStackOverflowRecovery = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool attemptOrderlyShutdownOnFatalSignal = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded whileNoInputIgnoreEvents = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool translateUpperCaseKeyBindings = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool inputPendingPFilterEvents = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool mwheelCoalesceScrollEvents = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded displayMonitorsChangedFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitRecordChar = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool recordAllKeys = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded postSelectRegionHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool disableInhibitTextConversion = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded currentKeyRemapSequence = new ELispSymbol.Value.Forwarded(false);
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
    private static final ELispSymbol.Value.Forwarded minibufferLocalMap = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded minorModeMapAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minorModeOverridingMapAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded emulationModeMapAlists = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded whereIsPreferredModifier = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded describeBindingsCheckShadowingInRanges = new ELispSymbol.Value.Forwarded(false);
    private static void keymapVars() {
        MINIBUFFER_LOCAL_MAP.initForwardTo(minibufferLocalMap);
        MINOR_MODE_MAP_ALIST.initForwardTo(minorModeMapAlist);
        MINOR_MODE_OVERRIDING_MAP_ALIST.initForwardTo(minorModeOverridingMapAlist);
        EMULATION_MODE_MAP_ALISTS.initForwardTo(emulationModeMapAlists);
        WHERE_IS_PREFERRED_MODIFIER.initForwardTo(whereIsPreferredModifier);
        DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES.initForwardTo(describeBindingsCheckShadowingInRanges);
    }
    private static final ELispSymbol.Value.Forwarded obarray = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded values = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded standardInput = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded readCircle = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded loadPath = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded loadSuffixes = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded moduleFileSuffix = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded dynamicLibrarySuffixes = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded loadFileRepSuffixes = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool loadInProgress = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded afterLoadAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded loadHistory = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded loadFileName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded loadTrueFileName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded userInitFile = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded currentLoadList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded loadReadFunction = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded loadSourceFileFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool loadForceDocStrings = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool loadConvertToUnibyte = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded sourceDirectory = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded preloadedFileList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded byteBooleanVars = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool loadDangerousLibraries = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool forceLoadMessages = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded bytecompVersionRegexp = new ELispSymbol.Value.Forwarded(new ELispString("^;;;.\\(?:in Emacs version\\|bytecomp version FSF\\)"));
    private static final ELispSymbol.Value.Forwarded lexicalBinding = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded evalBufferList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded lreadUnescapedCharacterLiterals = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool loadPreferNewer = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool loadNoNative = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded readSymbolShorthands = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded macroexpDynvars = new ELispSymbol.Value.Forwarded(false);
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
        LREAD_UNESCAPED_CHARACTER_LITERALS.initForwardTo(lreadUnescapedCharacterLiterals);
        LOAD_PREFER_NEWER.initForwardTo(loadPreferNewer);
        LOAD_NO_NATIVE.initForwardTo(loadNoNative);
        READ_SYMBOL_SHORTHANDS.initForwardTo(readSymbolShorthands);
        MACROEXP__DYNVARS.initForwardTo(macroexpDynvars);
    }
    private static final ELispSymbol.Value.Forwarded kbdMacroTerminationHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded definingKbdMacro = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static final ELispSymbol.Value.Forwarded executingKbdMacro = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong executingKbdMacroIndex = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.Forwarded lastKbdMacro = new ELispSymbol.Value.Forwarded(); /* TODO */
    private static void macrosVars() {
        KBD_MACRO_TERMINATION_HOOK.initForwardTo(kbdMacroTerminationHook);
        DEFINING_KBD_MACRO.initForwardTo(definingKbdMacro);
        EXECUTING_KBD_MACRO.initForwardTo(executingKbdMacro);
        EXECUTING_KBD_MACRO_INDEX.initForwardTo(executingKbdMacroIndex);
        LAST_KBD_MACRO.initForwardTo(lastKbdMacro);
    }
    private static final ELispSymbol.Value.Forwarded readExpressionHistory = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded readBufferFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferFollowsSelectedFrame = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool readBufferCompletionIgnoreCase = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded minibufferSetupHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferExitHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded historyLength = new ELispSymbol.Value.Forwarded(100L);
    private static final ELispSymbol.Value.ForwardedBool historyDeleteDuplicates = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded historyAddNewInput = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool completionIgnoreCase = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool enableRecursiveMinibuffers = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded minibufferCompletionTable = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferCompletionPredicate = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferCompletionConfirm = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferCompletingFileName = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferHelpForm = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferHistoryVariable = new ELispSymbol.Value.Forwarded(0L);
    private static final ELispSymbol.Value.Forwarded minibufferHistoryPosition = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool minibufferAutoRaise = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded completionRegexpList = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool minibufferAllowTextProperties = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded minibufferPromptProperties = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded readHideChar = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitInteraction = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool readMinibufferRestoreWindows = new ELispSymbol.Value.ForwardedBool(true);
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
    private static final ELispSymbol.Value.Forwarded standardOutput = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded floatOutputFormat = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool printIntegersAsCharacters = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded printLength = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded printLevel = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool printEscapeNewlines = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool printEscapeControlCharacters = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool printEscapeNonascii = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool printEscapeMultibyte = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool printQuoted = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded printGensym = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded printCircle = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded printContinuousNumbering = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded printNumberTable = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded printCharsetTextProperty = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool printSymbolsBare = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded printUnreadableFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded printUnreadableCallbackBuffer = new ELispSymbol.Value.Forwarded(false);
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
    private static final ELispSymbol.Value.ForwardedBool deleteExitedProcesses = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded processConnectionType = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded processAdaptiveReadBuffering = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool processPrioritizeLowerFds = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded interruptProcessFunctions = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded signalProcessFunctions = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded internalDaemonSockname = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong readProcessOutputMax = new ELispSymbol.Value.ForwardedLong(65536);
    private static final ELispSymbol.Value.ForwardedBool fastReadProcessOutput = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedLong processErrorPauseTime = new ELispSymbol.Value.ForwardedLong(1);
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
    private static final ELispSymbol.Value.Forwarded searchSpacesRegexp = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitChangingMatchData = new ELispSymbol.Value.Forwarded(false);
    private static void searchVars() {
        SEARCH_SPACES_REGEXP.initForwardTo(searchSpacesRegexp);
        INHIBIT_CHANGING_MATCH_DATA.initForwardTo(inhibitChangingMatchData);
    }
    private static final ELispSymbol.Value.Forwarded commentUseSyntaxPpss = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool parseSexpIgnoreComments = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.ForwardedBool parseSexpLookupProperties = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.ForwardedLong syntaxPropertizeDone = new ELispSymbol.Value.ForwardedLong(-1);
    private static final ELispSymbol.Value.ForwardedBool wordsIncludeEscapes = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool multibyteSyntaxAsSymbol = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool openParenInColumn0IsDefunStart = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded findWordBoundaryFunctionTable = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool commentEndCanBeEscaped = new ELispSymbol.Value.ForwardedBool(false);
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
    private static final ELispSymbol.Value.Forwarded defaultTextProperties = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded charPropertyAliasAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded inhibitPointMotionHooks = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded textPropertyDefaultNonsticky = new ELispSymbol.Value.Forwarded();
    private static void textpropVars() {
        DEFAULT_TEXT_PROPERTIES.initForwardTo(defaultTextProperties);
        CHAR_PROPERTY_ALIAS_ALIST.initForwardTo(charPropertyAliasAlist);
        INHIBIT_POINT_MOTION_HOOKS.initForwardTo(inhibitPointMotionHooks);
        TEXT_PROPERTY_DEFAULT_NONSTICKY.initForwardTo(textPropertyDefaultNonsticky);
    }
    private static final ELispSymbol.Value.ForwardedBool currentTimeList = new ELispSymbol.Value.ForwardedBool(true);
    private static void timefnsVars() {
        CURRENT_TIME_LIST.initForwardTo(currentTimeList);
    }
    private static final ELispSymbol.Value.Forwarded tempBufferShowFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded minibufferScrollWindow = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool modeLineInNonSelectedWindows = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded otherWindowScrollBuffer = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded otherWindowScrollDefault = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool autoWindowVscroll = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedLong nextScreenContextLines = new ELispSymbol.Value.ForwardedLong(2);
    private static final ELispSymbol.Value.Forwarded scrollPreserveScreenPosition = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowPointInsertionType = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowBufferChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowSizeChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowSelectionChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowStateChangeFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowStateChangeHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowConfigurationChangeHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowRestoreKilledBufferWindows = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded recenterRedisplay = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded windowCombinationResize = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded windowCombinationLimit = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded windowPersistentParameters = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool windowResizePixelwise = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool fastButImpreciseScrolling = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded windowDeadWindowsTable = new ELispSymbol.Value.Forwarded();
    private static void windowVars() {
        TEMP_BUFFER_SHOW_FUNCTION.initForwardTo(tempBufferShowFunction);
        MINIBUFFER_SCROLL_WINDOW.initForwardTo(minibufferScrollWindow);
        MODE_LINE_IN_NON_SELECTED_WINDOWS.initForwardTo(modeLineInNonSelectedWindows);
        OTHER_WINDOW_SCROLL_BUFFER.initForwardTo(otherWindowScrollBuffer);
        OTHER_WINDOW_SCROLL_DEFAULT.initForwardTo(otherWindowScrollDefault);
        AUTO_WINDOW_VSCROLL.initForwardTo(autoWindowVscroll);
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
    private static final ELispSymbol.Value.ForwardedBool scrollMinibufferConservatively = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool inhibitMessage = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded messagesBufferName = new ELispSymbol.Value.Forwarded(new ELispString("*Messages*"));
    private static final ELispSymbol.Value.ForwardedBool xStretchCursor = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.Forwarded showTrailingWhitespace = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded modeLineCompact = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded nobreakCharDisplay = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool nobreakCharAsciiDisplay = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded voidTextAreaPointer = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded inhibitRedisplay = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded globalModeString = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded overlayArrowPosition = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded overlayArrowString = new ELispSymbol.Value.Forwarded(new ELispString("=>"));
    private static final ELispSymbol.Value.Forwarded overlayArrowVariableList = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedLong scrollStep = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.ForwardedLong scrollConservatively = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.ForwardedLong scrollMargin = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.Forwarded maximumScrollMargin = new ELispSymbol.Value.Forwarded(0.25);
    private static final ELispSymbol.Value.Forwarded displayPixelsPerInch = new ELispSymbol.Value.Forwarded(72.0);
    private static final ELispSymbol.Value.ForwardedLong debugEndPos = new ELispSymbol.Value.ForwardedLong();
    private static final ELispSymbol.Value.Forwarded truncatePartialWidthWindows = new ELispSymbol.Value.Forwarded(50L);
    private static final ELispSymbol.Value.ForwardedBool wordWrapByCategory = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded lineNumberDisplayLimit = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong lineNumberDisplayLimitWidth = new ELispSymbol.Value.ForwardedLong(200);
    private static final ELispSymbol.Value.ForwardedBool highlightNonselectedWindows = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool multipleFrames = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.Forwarded frameTitleFormat = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded iconTitleFormat = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded messageLogMax = new ELispSymbol.Value.Forwarded(1000L);
    private static final ELispSymbol.Value.Forwarded windowScrollFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded mouseAutoselectWindow = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded autoResizeTabBars = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool autoRaiseTabBarButtons = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded autoResizeToolBars = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool autoRaiseToolBarButtons = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded makeCursorLineFullyVisible = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool makeWindowStartVisible = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded tabBarBorder = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded tabBarButtonMargin = new ELispSymbol.Value.Forwarded(1L);
    private static final ELispSymbol.Value.ForwardedLong tabBarButtonRelief = new ELispSymbol.Value.ForwardedLong(1);
    private static final ELispSymbol.Value.Forwarded toolBarBorder = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded toolBarButtonMargin = new ELispSymbol.Value.Forwarded(4L);
    private static final ELispSymbol.Value.ForwardedLong toolBarButtonRelief = new ELispSymbol.Value.ForwardedLong(1);
    private static final ELispSymbol.Value.Forwarded toolBarStyle = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong toolBarMaxLabelSize = new ELispSymbol.Value.ForwardedLong(14);
    private static final ELispSymbol.Value.Forwarded fontificationFunctions = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool unibyteDisplayViaLanguageEnvironment = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded maxMiniWindowHeight = new ELispSymbol.Value.Forwarded(0.25);
    private static final ELispSymbol.Value.Forwarded resizeMiniWindows = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded blinkCursorAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded autoHscrollMode = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedLong hscrollMargin = new ELispSymbol.Value.ForwardedLong(5);
    private static final ELispSymbol.Value.Forwarded hscrollStep = new ELispSymbol.Value.Forwarded(0L);
    private static final ELispSymbol.Value.ForwardedBool messageTruncateLines = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded menuBarUpdateHook = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded menuUpdatingFrame = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitMenubarUpdate = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded wrapPrefix = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded linePrefix = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded displayLineNumbers = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded displayLineNumbersWidth = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded displayLineNumbersCurrentAbsolute = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.ForwardedBool displayLineNumbersWiden = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedLong displayLineNumbersOffset = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.ForwardedBool displayFillColumnIndicator = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.Forwarded displayFillColumnIndicatorColumn = new ELispSymbol.Value.Forwarded(true);
    private static final ELispSymbol.Value.Forwarded displayFillColumnIndicatorCharacter = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong displayLineNumbersMajorTick = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.ForwardedLong displayLineNumbersMinorTick = new ELispSymbol.Value.ForwardedLong(0);
    private static final ELispSymbol.Value.ForwardedBool inhibitEvalDuringRedisplay = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitFreeRealizedFaces = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitBidiMirroring = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool bidiInhibitBpa = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool inhibitTryWindowId = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.ForwardedBool inhibitTryWindowReusing = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.ForwardedBool inhibitTryCursorMovement = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.ForwardedLong overlineMargin = new ELispSymbol.Value.ForwardedLong(2);
    private static final ELispSymbol.Value.ForwardedLong underlineMinimumOffset = new ELispSymbol.Value.ForwardedLong(1);
    private static final ELispSymbol.Value.ForwardedBool displayHourglass = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.Forwarded hourglassDelay = new ELispSymbol.Value.Forwarded(1L);
    private static final ELispSymbol.Value.Forwarded preRedisplayFunction = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded glyphlessCharDisplay = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded debugOnMessage = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded setMessageFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded clearMessageFunction = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded redisplayAllWindowsCause = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded redisplayModeLinesCause = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.ForwardedBool redisplayInhibitBidi = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool displayRawBytesAsHex = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool mouseFineGrainedTracking = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool tabBarDraggingInProgress = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool redisplaySkipInitialFrame = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool redisplaySkipFontificationOnInput = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedBool redisplayAdhocScrollInResizeMiniWindows = new ELispSymbol.Value.ForwardedBool(true);
    private static final ELispSymbol.Value.ForwardedBool compositionBreakAtPoint = new ELispSymbol.Value.ForwardedBool(false);
    private static final ELispSymbol.Value.ForwardedLong maxRedisplayTicks = new ELispSymbol.Value.ForwardedLong(0);
    private static void xdispVars() {
        SCROLL_MINIBUFFER_CONSERVATIVELY.initForwardTo(scrollMinibufferConservatively);
        INHIBIT_MESSAGE.initForwardTo(inhibitMessage);
        MESSAGES_BUFFER_NAME.initForwardTo(messagesBufferName);
        X_STRETCH_CURSOR.initForwardTo(xStretchCursor);
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
        SCROLL_STEP.initForwardTo(scrollStep);
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
        AUTO_RAISE_TAB_BAR_BUTTONS.initForwardTo(autoRaiseTabBarButtons);
        AUTO_RESIZE_TOOL_BARS.initForwardTo(autoResizeToolBars);
        AUTO_RAISE_TOOL_BAR_BUTTONS.initForwardTo(autoRaiseToolBarButtons);
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
        AUTO_HSCROLL_MODE.initForwardTo(autoHscrollMode);
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
        DISPLAY_HOURGLASS.initForwardTo(displayHourglass);
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
    private static final ELispSymbol.Value.ForwardedBool faceFiltersAlwaysMatch = new ELispSymbol.Value.ForwardedBool();
    private static final ELispSymbol.Value.Forwarded faceNewFrameDefaults = new ELispSymbol.Value.Forwarded();
    private static final ELispSymbol.Value.Forwarded faceDefaultStipple = new ELispSymbol.Value.Forwarded(new ELispString("gray3"));
    private static final ELispSymbol.Value.Forwarded ttyDefinedColorAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded scalableFontsAllowed = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded faceIgnoredFonts = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded faceRemappingAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.Forwarded faceFontRescaleAlist = new ELispSymbol.Value.Forwarded(false);
    private static final ELispSymbol.Value.ForwardedLong faceNearSameColorThreshold = new ELispSymbol.Value.ForwardedLong(30000);
    private static final ELispSymbol.Value.Forwarded faceFontLaxMatchedAttributes = new ELispSymbol.Value.Forwarded(true);
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
    //#endregion initGlobalVariables
    //#region initializations
    public static void postInitVariables() {
        initObarrayOnce();
        initSyntaxOnce();
        initCategoryOnce();
        initCasetabOnce();
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
        symsOfTextprop();
        symsOfComposite();
        symsOfWindow();
        symsOfXdisp();
        initCharset();
    }

    private static void initObarrayOnce() {
        NIL.setValue(NIL);
        NIL.setConstant(true);
        NIL.setSpecial(true);
        T.setValue(T);
        T.setConstant(true);
        T.setSpecial(true);
    }
    private static void initCategoryOnce() {
        FPut.put(CATEGORY_TABLE, CHAR_TABLE_EXTRA_SLOTS, (long) (2));
        var standardCategoryTable = FMakeCharTable.makeCharTable(CATEGORY_TABLE, NIL);
        standardCategoryTable.setDefault(FMakeBoolVector.makeBoolVector((long) (128), NIL));
        FSetCharTableExtraSlot.setCharTableExtraSlot(standardCategoryTable, (long) (0), new ELispVector(95, false));
    }
    private static void initBufferOnce() {
        ELispBuffer.initBufferLocalVars();
    }
    private static void initMinibufOnce() {
        getMiniBuffer(0);
    }
    private static void symsOfXfaces() {
        var faceNewFrameDefaultsJInit = FMakeHashTable.makeHashTable(new Object[]{CTEST, EQ});
        faceNewFrameDefaults.setValue(faceNewFrameDefaultsJInit);
    }
    private static void symsOfKeymap() {
        FPut.put(KEYMAP, CHAR_TABLE_EXTRA_SLOTS, (long) (0));
        var minibufferLocalMapJInit = FMakeSparseKeymap.makeSparseKeymap(NIL);
        minibufferLocalMap.setValue(minibufferLocalMapJInit);
    }
    private static void symsOfKeyboard() {
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
    private static void symsOfData() {
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
    private static void symsOfFns() {
        YES_OR_NO_P_HISTORY.setValue(NIL);
        var featuresJInit = ELispCons.listOf(EMACS);
        features.setValue(featuresJInit);
        FEATURES.setSpecial(false);
    }
    private static void symsOfFileio() {
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
    private static void symsOfAlloc() {
        var memorySignalDataJInit = ELispCons.listOf(ERROR, new ELispString("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs"));
        memorySignalData.setValue(memorySignalDataJInit);
    }
    private static void symsOfCharset() {
        var charsetAscii = defineCharsetInternal(
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
        var charsetIso88591 = defineCharsetInternal(
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
        var charsetUnicode = defineCharsetInternal(
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
        var charsetEmacs = defineCharsetInternal(
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
        var charsetEightBit = defineCharsetInternal(
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
    private static void symsOfCoding() {
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
        setupCodingSystem(NO_CONVERSION, safeTerminalCoding);
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
    private static void symsOfComp() {
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
    private static void initBuffer() {
        var scratch = new ELispString("*scratch*");
        FSetBuffer.setBuffer(FGetBufferCreate.getBufferCreate(scratch, NIL));
        ELispBuffer.initDirectory();
    }
    private static void initCallproc1() {
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
    private static void initLread() {
        var loadPathJInit = setAndCheckLoadPath();
        loadPath.setValue(loadPathJInit);
    }
    private static void symsOfLread() {
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
    private static void symsOfPrint() {
        var printCharsetTextPropertyJInit = DEFAULT;
        printCharsetTextProperty.setValue(printCharsetTextPropertyJInit);
        unintern(PRINT__UNREADABLE_CALLBACK_BUFFER);
    }
    private static void symsOfEval() {
        var debuggerJInit = DEBUG_EARLY;
        debugger.setValue(debuggerJInit);
        unintern(INTERNAL_INTERPRETER_ENVIRONMENT);
    }
    private static void symsOfBuffer() {
        FPut.put(VERTICAL_SCROLL_BAR, CHOICE, ELispCons.listOf(NIL, T, LEFT, RIGHT));
        FPut.put(FRACTION, RANGE, FCons.cons((double) (0.0), (double) (1.0)));
        FPut.put(OVERWRITE_MODE, CHOICE, ELispCons.listOf(NIL, intern("overwrite-mode-textual"), OVERWRITE_MODE_BINARY));
        FPut.put(PROTECTED_FIELD, ERROR_CONDITIONS, ELispCons.listOf(PROTECTED_FIELD, ERROR));
        FPut.put(PROTECTED_FIELD, ERROR_MESSAGE, new ELispString("Attempt to modify a protected field"));
        ENABLE_MULTIBYTE_CHARACTERS.setConstant(true);
        CASE_FOLD_SEARCH.setBufferLocal(true);
        FPut.put(intern("erase-buffer"), DISABLED, T);
    }
    private static void symsOfCasefiddle() {
        CASE_SYMBOLS_AS_WORDS.setBufferLocal(true);
    }
    private static void symsOfCcl() {
        var codeConversionMapVectorJInit = new ELispVector(16, false);
        codeConversionMapVector.setValue(codeConversionMapVectorJInit);
    }
    private static void symsOfCharacter() {
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
    private static void symsOfEditfns() {
        var cachedSystemName = false;
        var systemNameJInit = cachedSystemName;
        systemName.setValue(systemNameJInit);
        unintern(OUTERMOST_RESTRICTION);
    }
    private static void symsOfEmacs() {
        var systemTypeJInit = intern("gnu/linux");
        systemType.setValue(systemTypeJInit);
        var c = File.pathSeparator;
        var pathSeparatorJInit = new ELispString(c);
        pathSeparator.setValue(pathSeparatorJInit);
        FPut.put(DYNAMIC_LIBRARY_ALIST, RISKY_LOCAL_VARIABLE, T);
    }
    private static void symsOfMinibuf() {
        MINIBUFFER_DEFAULT.setValue(NIL);
        CUSTOM_VARIABLE_HISTORY.setValue(NIL);
        BUFFER_NAME_HISTORY.setValue(NIL);
        var minibufferPromptPropertiesJInit = ELispCons.listOf(READ_ONLY, T);
        minibufferPromptProperties.setValue(minibufferPromptPropertiesJInit);
    }
    private static void symsOfProcess() {
        var interruptProcessFunctionsJInit = ELispCons.listOf(INTERNAL_DEFAULT_INTERRUPT_PROCESS);
        interruptProcessFunctions.setValue(interruptProcessFunctionsJInit);
        var signalProcessFunctionsJInit = ELispCons.listOf(INTERNAL_DEFAULT_SIGNAL_PROCESS);
        signalProcessFunctions.setValue(signalProcessFunctionsJInit);
        var subfeatures = FCons.cons(FCons.cons(CNOWAIT, FCons.cons(T, NIL)), NIL);
        subfeatures = FCons.cons(FCons.cons(CFAMILY, FCons.cons(IPV4, NIL)), subfeatures);
        subfeatures = FCons.cons(FCons.cons(CSERVER, FCons.cons(T, NIL)), subfeatures);
        FProvide.provide(intern("make-network-process"), subfeatures);
    }
    private static void symsOfSearch() {
        FPut.put(SEARCH_FAILED, ERROR_CONDITIONS, ELispCons.listOf(SEARCH_FAILED, ERROR));
        FPut.put(SEARCH_FAILED, ERROR_MESSAGE, new ELispString("Search failed"));
        FPut.put(USER_SEARCH_FAILED, ERROR_CONDITIONS, ELispCons.listOf(USER_SEARCH_FAILED, USER_ERROR, SEARCH_FAILED, ERROR));
        FPut.put(USER_SEARCH_FAILED, ERROR_MESSAGE, new ELispString("Search failed"));
        FPut.put(INVALID_REGEXP, ERROR_CONDITIONS, ELispCons.listOf(INVALID_REGEXP, ERROR));
        FPut.put(INVALID_REGEXP, ERROR_MESSAGE, new ELispString("Invalid regexp"));
    }
    private static void symsOfFrame() {
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
    private static void symsOfSyntax() {
        FPut.put(SCAN_ERROR, ERROR_CONDITIONS, ELispCons.listOf(SCAN_ERROR, ERROR));
        FPut.put(SCAN_ERROR, ERROR_MESSAGE, new ELispString("Scan error"));
        SYNTAX_PROPERTIZE__DONE.setBufferLocal(true);
        var findWordBoundaryFunctionTableJInit = FMakeCharTable.makeCharTable(NIL, NIL);
        findWordBoundaryFunctionTable.setValue(findWordBoundaryFunctionTableJInit);
        COMMENT_END_CAN_BE_ESCAPED.setBufferLocal(true);
    }
    private static void symsOfTextprop() {
        var textPropertyDefaultNonstickyJInit = ELispCons.listOf(FCons.cons(SYNTAX_TABLE, T), FCons.cons(DISPLAY, T));
        textPropertyDefaultNonsticky.setValue(textPropertyDefaultNonstickyJInit);
    }
    private static void symsOfComposite() {
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
    private static void symsOfWindow() {
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
    private static void symsOfXdisp() {
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
    //#endregion initializations
}
