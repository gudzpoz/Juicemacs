package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * A context holding static everything
 *
 * <p>
 * Basically, things here corresponds to those global C variables in Emacs
 * C code. These could be made context-local, but it is really hard to keep a
 * localized context around when Emacs is design around a global one, and
 * almost every argument needs to get checked against {@link #NIL}.
 * </p>
 */
public final class ELispContext {
    private final static ELispContext ELISP_CONTEXT_INSTANCE = new ELispContext();

    public static ELispContext getInstance() {
        return ELISP_CONTEXT_INSTANCE;
    }

    public final static ELispSymbol.ThreadLocalValue CURRENT_BUFFER = new ELispSymbol.ThreadLocalValue();

    // TODO: Replace this with obarray
    private final static HashMap<String, ELispSymbol> INTERN_MAP = new HashMap<>();

    @CompilerDirectives.TruffleBoundary
    public static ELispSymbol intern(String symbol) {
        return INTERN_MAP.computeIfAbsent(symbol, ELispSymbol::new);
    }

    @CompilerDirectives.TruffleBoundary
    public static void unintern(ELispSymbol symbol) {
        ELispSymbol old = INTERN_MAP.get(symbol.name());
        if (old == symbol) {
            INTERN_MAP.remove(symbol.name());
        }
    }

    public static List<ELispSymbol> internedSymbols() {
        return new ArrayList<>(INTERN_MAP.values());
    }

    public static String applyShorthands(String symbol) {
        // TODO: Implementation
        return symbol;
    }

    public static ELispExpressionNode valueToExpression(Object[] expressions, boolean lexicalBinding) {
        return ELispInterpretedNode.create(expressions, lexicalBinding);
    }

    public void registerFunction(ELispSymbol symbol, ELispValue function) {
        symbol.setFunction(function);
    }

    public void initGlobal(ELispLanguage language) {
        initSymbols(allocSymbols());
        initSymbols(bufferSymbols());
        initSymbols(callintSymbols());
        initSymbols(callprocSymbols());
        initSymbols(casefiddleSymbols());
        initSymbols(casetabSymbols());
        initSymbols(categorySymbols());
        initSymbols(characterSymbols());
        initSymbols(charsetSymbols());
        initSymbols(chartabSymbols());
        initSymbols(cmdsSymbols());
        initSymbols(codingSymbols());
        initSymbols(compSymbols());
        initSymbols(dataSymbols());
        initSymbols(docSymbols());
        initSymbols(editfnsSymbols());
        initSymbols(emacsSymbols());
        initSymbols(evalSymbols());
        initSymbols(fileioSymbols());
        initSymbols(fnsSymbols());
        initSymbols(frameSymbols());
        initSymbols(keyboardSymbols());
        initSymbols(keymapSymbols());
        initSymbols(lreadSymbols());
        initSymbols(macrosSymbols());
        initSymbols(minibufSymbols());
        initSymbols(printSymbols());
        initSymbols(processSymbols());
        initSymbols(searchSymbols());
        initSymbols(syntaxSymbols());
        initSymbols(textpropSymbols());
        initSymbols(timefnsSymbols());
        initSymbols(windowSymbols());
        initSymbols(xdispSymbols());
        initSymbols(xfacesSymbols());

        T.setConstant(true);
        NIL.setConstant(true);

        // Built-in functions must be re-initialized every time.
        // Otherwise, Truffle will complain that we are trying to use functions across contexts.
        initBuiltIns(language, new BuiltInAlloc());
        initBuiltIns(language, new BuiltInBuffer());
        initBuiltIns(language, new BuiltInCallInt());
        initBuiltIns(language, new BuiltInCallProc());
        initBuiltIns(language, new BuiltInCaseFiddle());
        initBuiltIns(language, new BuiltInCaseTab());
        initBuiltIns(language, new BuiltInCategory());
        initBuiltIns(language, new BuiltInCharacter());
        initBuiltIns(language, new BuiltInCharSet());
        initBuiltIns(language, new BuiltInCharTab());
        initBuiltIns(language, new BuiltInCmds());
        initBuiltIns(language, new BuiltInCoding());
        initBuiltIns(language, new BuiltInComp());
        initBuiltIns(language, new BuiltInData());
        initBuiltIns(language, new BuiltInDoc());
        initBuiltIns(language, new BuiltInEditFns());
        initBuiltIns(language, new BuiltInEmacs());
        initBuiltIns(language, new BuiltInEval());
        initBuiltIns(language, new BuiltInFileIO());
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

        ELispGlobals.initGlobalVariables();
        ELispGlobals.postInitVariables();
    }

    private void initBuiltIns(ELispLanguage language, ELispBuiltIns builtIns) {
        builtIns.initialize(language, this);
    }

    private void initSymbols(ELispSymbol[] symbols) {
        for (ELispSymbol symbol : symbols) {
            INTERN_MAP.put(symbol.name(), symbol);
            // Clear any possible function values to avoid Truffle cross-context calls.
            if (!symbol.isConstant()) {
                if (symbol.isBound() && symbol.getValue() instanceof ELispValue) {
                    symbol.setValue(false);
                }
                symbol.setFunction(false);
            }
            symbol.clearProperties();
            symbol.setInterned(ELispSymbol.Interned.INTERNED_IN_INITIAL_OBARRAY);
        }
    }

    //#region data.c
    public final static ELispSymbol AREF = new ELispSymbol("aref");
    public final static ELispSymbol ARGS_OUT_OF_RANGE = new ELispSymbol("args-out-of-range");
    public final static ELispSymbol ARITH_ERROR = new ELispSymbol("arith-error");
    public final static ELispSymbol ARRAYP = new ELispSymbol("arrayp");
    public final static ELispSymbol ASET = new ELispSymbol("aset");
    public final static ELispSymbol BARE_SYMBOL_P = new ELispSymbol("bare-symbol-p");
    public final static ELispSymbol BEGINNING_OF_BUFFER = new ELispSymbol("beginning-of-buffer");
    public final static ELispSymbol BIGNUM = new ELispSymbol("bignum");
    public final static ELispSymbol BOOLEAN = new ELispSymbol("boolean");
    public final static ELispSymbol BOOLEANP = new ELispSymbol("booleanp");
    public final static ELispSymbol BOOL_VECTOR = new ELispSymbol("bool-vector");
    public final static ELispSymbol BOOL_VECTOR_P = new ELispSymbol("bool-vector-p");
    public final static ELispSymbol BUFFER = new ELispSymbol("buffer");
    public final static ELispSymbol BUFFERP = new ELispSymbol("bufferp");
    public final static ELispSymbol BUFFER_OR_STRING_P = new ELispSymbol("buffer-or-string-p");
    public final static ELispSymbol BUFFER_READ_ONLY = new ELispSymbol("buffer-read-only");
    public final static ELispSymbol BYTE_CODE_FUNCTION = new ELispSymbol("byte-code-function");
    public final static ELispSymbol BYTE_CODE_FUNCTION_P = new ELispSymbol("byte-code-function-p");
    public final static ELispSymbol CAR = new ELispSymbol("car");
    public final static ELispSymbol CDR = new ELispSymbol("cdr");
    public final static ELispSymbol CHAR_OR_STRING_P = new ELispSymbol("char-or-string-p");
    public final static ELispSymbol CHAR_TABLE = new ELispSymbol("char-table");
    public final static ELispSymbol CHAR_TABLE_P = new ELispSymbol("char-table-p");
    public final static ELispSymbol CIRCULAR_LIST = new ELispSymbol("circular-list");
    public final static ELispSymbol COMMAND_MODES = new ELispSymbol("command-modes");
    public final static ELispSymbol CONDITION_VARIABLE = new ELispSymbol("condition-variable");
    public final static ELispSymbol CONS = new ELispSymbol("cons");
    public final static ELispSymbol CONSP = new ELispSymbol("consp");
    public final static ELispSymbol CYCLIC_FUNCTION_INDIRECTION = new ELispSymbol("cyclic-function-indirection");
    public final static ELispSymbol CYCLIC_VARIABLE_INDIRECTION = new ELispSymbol("cyclic-variable-indirection");
    public final static ELispSymbol DEFALIAS_FSET_FUNCTION = new ELispSymbol("defalias-fset-function");
    public final static ELispSymbol DEFUN = new ELispSymbol("defun");
    public final static ELispSymbol DOMAIN_ERROR = new ELispSymbol("domain-error");
    public final static ELispSymbol ELT = new ELispSymbol("elt");
    public final static ELispSymbol END_OF_BUFFER = new ELispSymbol("end-of-buffer");
    public final static ELispSymbol END_OF_FILE = new ELispSymbol("end-of-file");
    public final static ELispSymbol ERROR = new ELispSymbol("error");
    public final static ELispSymbol ERROR_CONDITIONS = new ELispSymbol("error-conditions");
    public final static ELispSymbol ERROR_MESSAGE = new ELispSymbol("error-message");
    public final static ELispSymbol EXCESSIVE_LISP_NESTING = new ELispSymbol("excessive-lisp-nesting");
    public final static ELispSymbol EXCESSIVE_VARIABLE_BINDING = new ELispSymbol("excessive-variable-binding");
    public final static ELispSymbol FBOUNDP = new ELispSymbol("fboundp");
    public final static ELispSymbol FINALIZER = new ELispSymbol("finalizer");
    public final static ELispSymbol FIXNUMP = new ELispSymbol("fixnump");
    public final static ELispSymbol FIXNUM_OR_SYMBOL_WITH_POS_P = new ELispSymbol("fixnum-or-symbol-with-pos-p");
    public final static ELispSymbol FLOAT = new ELispSymbol("float");
    public final static ELispSymbol FLOATP = new ELispSymbol("floatp");
    public final static ELispSymbol FONT_ENTITY = new ELispSymbol("font-entity");
    public final static ELispSymbol FONT_OBJECT = new ELispSymbol("font-object");
    public final static ELispSymbol FONT_SPEC = new ELispSymbol("font-spec");
    public final static ELispSymbol FRAME = new ELispSymbol("frame");
    public final static ELispSymbol FUNCTION_HISTORY = new ELispSymbol("function-history");
    public final static ELispSymbol INHIBITED_INTERACTION = new ELispSymbol("inhibited-interaction");
    public final static ELispSymbol INTEGER = new ELispSymbol("integer");
    public final static ELispSymbol INTEGERP = new ELispSymbol("integerp");
    public final static ELispSymbol INTEGER_OR_MARKER_P = new ELispSymbol("integer-or-marker-p");
    public final static ELispSymbol INTERACTIVE_FORM = new ELispSymbol("interactive-form");
    public final static ELispSymbol INTERPRETED_FUNCTION = new ELispSymbol("interpreted-function");
    public final static ELispSymbol INVALID_FUNCTION = new ELispSymbol("invalid-function");
    public final static ELispSymbol INVALID_READ_SYNTAX = new ELispSymbol("invalid-read-syntax");
    public final static ELispSymbol LAMBDA = new ELispSymbol("lambda");
    public final static ELispSymbol LISTP = new ELispSymbol("listp");
    public final static ELispSymbol MAKUNBOUND = new ELispSymbol("makunbound");
    public final static ELispSymbol MANY = new ELispSymbol("many");
    public final static ELispSymbol MARKER = new ELispSymbol("marker");
    public final static ELispSymbol MARKERP = new ELispSymbol("markerp");
    public final static ELispSymbol MARK_INACTIVE = new ELispSymbol("mark-inactive");
    public final static ELispSymbol MINIBUFFER_QUIT = new ELispSymbol("minibuffer-quit");
    public final static ELispSymbol MODULE_FUNCTION = new ELispSymbol("module-function");
    public final static ELispSymbol MOST_NEGATIVE_FIXNUM = new ELispSymbol("most-negative-fixnum");
    public final static ELispSymbol MOST_POSITIVE_FIXNUM = new ELispSymbol("most-positive-fixnum");
    public final static ELispSymbol MUTEX = new ELispSymbol("mutex");
    public final static ELispSymbol NATIVE_COMP_FUNCTION = new ELispSymbol("native-comp-function");
    public final static ELispSymbol NATIVE_COMP_UNIT = new ELispSymbol("native-comp-unit");
    public final static ELispSymbol NATNUMP = new ELispSymbol("natnump");
    public final static ELispSymbol NO_CATCH = new ELispSymbol("no-catch");
    public final static ELispSymbol NTH = new ELispSymbol("nth");
    public final static ELispSymbol NUMBERP = new ELispSymbol("numberp");
    public final static ELispSymbol NUMBER_OR_MARKER_P = new ELispSymbol("number-or-marker-p");
    public final static ELispSymbol OBARRAY = new ELispSymbol("obarray");
    public final static ELispSymbol OCLOSURE_INTERACTIVE_FORM = new ELispSymbol("oclosure-interactive-form");
    public final static ELispSymbol OVERFLOW_ERROR = new ELispSymbol("overflow-error");
    public final static ELispSymbol OVERLAY = new ELispSymbol("overlay");
    public final static ELispSymbol PRIMITIVE_FUNCTION = new ELispSymbol("primitive-function");
    public final static ELispSymbol PROCESS = new ELispSymbol("process");
    public final static ELispSymbol QUIT = new ELispSymbol("quit");
    public final static ELispSymbol QUOTE = new ELispSymbol("quote");
    public final static ELispSymbol RANGE_ERROR = new ELispSymbol("range-error");
    public final static ELispSymbol RECORD = new ELispSymbol("record");
    public final static ELispSymbol RECORDP = new ELispSymbol("recordp");
    public final static ELispSymbol RECURSION_ERROR = new ELispSymbol("recursion-error");
    public final static ELispSymbol SEQUENCEP = new ELispSymbol("sequencep");
    public final static ELispSymbol SET = new ELispSymbol("set");
    public final static ELispSymbol SETCAR = new ELispSymbol("setcar");
    public final static ELispSymbol SETCDR = new ELispSymbol("setcdr");
    public final static ELispSymbol SETTING_CONSTANT = new ELispSymbol("setting-constant");
    public final static ELispSymbol SET_DEFAULT = new ELispSymbol("set-default");
    public final static ELispSymbol SINGULARITY_ERROR = new ELispSymbol("singularity-error");
    public final static ELispSymbol SPECIAL_FORM = new ELispSymbol("special-form");
    public final static ELispSymbol STRING = new ELispSymbol("string");
    public final static ELispSymbol STRINGP = new ELispSymbol("stringp");
    public final static ELispSymbol SUBR = new ELispSymbol("subr");
    public final static ELispSymbol SUBRP = new ELispSymbol("subrp");
    public final static ELispSymbol SUBR_NATIVE_ELISP = new ELispSymbol("subr-native-elisp");
    public final static ELispSymbol SUB_CHAR_TABLE = new ELispSymbol("sub-char-table");
    public final static ELispSymbol SYMBOL = new ELispSymbol("symbol");
    public final static ELispSymbol SYMBOLP = new ELispSymbol("symbolp");
    public final static ELispSymbol SYMBOLS_WITH_POS_ENABLED = new ELispSymbol("symbols-with-pos-enabled");
    public final static ELispSymbol SYMBOL_WITH_POS = new ELispSymbol("symbol-with-pos");
    public final static ELispSymbol SYMBOL_WITH_POS_P = new ELispSymbol("symbol-with-pos-p");
    public final static ELispSymbol TERMINAL = new ELispSymbol("terminal");
    public final static ELispSymbol TEXT_READ_ONLY = new ELispSymbol("text-read-only");
    public final static ELispSymbol THREAD = new ELispSymbol("thread");
    public final static ELispSymbol TOP_LEVEL = new ELispSymbol("top-level");
    public final static ELispSymbol TRAPPING_CONSTANT = new ELispSymbol("trapping-constant");
    public final static ELispSymbol TREESIT_COMPILED_QUERY = new ELispSymbol("treesit-compiled-query");
    public final static ELispSymbol TREESIT_NODE = new ELispSymbol("treesit-node");
    public final static ELispSymbol TREESIT_PARSER = new ELispSymbol("treesit-parser");
    public final static ELispSymbol TYPE_MISMATCH = new ELispSymbol("type-mismatch");
    public final static ELispSymbol UNDERFLOW_ERROR = new ELispSymbol("underflow-error");
    public final static ELispSymbol UNEVALLED = new ELispSymbol("unevalled");
    public final static ELispSymbol UNLET = new ELispSymbol("unlet");
    public final static ELispSymbol USER_ERROR = new ELispSymbol("user-error");
    public final static ELispSymbol USER_PTR = new ELispSymbol("user-ptr");
    public final static ELispSymbol USER_PTRP = new ELispSymbol("user-ptrp");
    public final static ELispSymbol VECTOR = new ELispSymbol("vector");
    public final static ELispSymbol VECTORP = new ELispSymbol("vectorp");
    public final static ELispSymbol VECTOR_OR_CHAR_TABLE_P = new ELispSymbol("vector-or-char-table-p");
    public final static ELispSymbol VOID_FUNCTION = new ELispSymbol("void-function");
    public final static ELispSymbol VOID_VARIABLE = new ELispSymbol("void-variable");
    public final static ELispSymbol WATCHERS = new ELispSymbol("watchers");
    public final static ELispSymbol WHOLENUMP = new ELispSymbol("wholenump");
    public final static ELispSymbol WINDOW = new ELispSymbol("window");
    public final static ELispSymbol WINDOW_CONFIGURATION = new ELispSymbol("window-configuration");
    public final static ELispSymbol WRONG_LENGTH_ARGUMENT = new ELispSymbol("wrong-length-argument");
    public final static ELispSymbol WRONG_NUMBER_OF_ARGUMENTS = new ELispSymbol("wrong-number-of-arguments");
    public final static ELispSymbol WRONG_TYPE_ARGUMENT = new ELispSymbol("wrong-type-argument");
    public final static ELispSymbol XWIDGET = new ELispSymbol("xwidget");
    public final static ELispSymbol XWIDGET_VIEW = new ELispSymbol("xwidget-view");
    private ELispSymbol[] dataSymbols() {
        return new ELispSymbol[]{
                AREF,
                ARGS_OUT_OF_RANGE,
                ARITH_ERROR,
                ARRAYP,
                ASET,
                BARE_SYMBOL_P,
                BEGINNING_OF_BUFFER,
                BIGNUM,
                BOOLEAN,
                BOOLEANP,
                BOOL_VECTOR,
                BOOL_VECTOR_P,
                BUFFER,
                BUFFERP,
                BUFFER_OR_STRING_P,
                BUFFER_READ_ONLY,
                BYTE_CODE_FUNCTION,
                BYTE_CODE_FUNCTION_P,
                CAR,
                CDR,
                CHAR_OR_STRING_P,
                CHAR_TABLE,
                CHAR_TABLE_P,
                CIRCULAR_LIST,
                COMMAND_MODES,
                CONDITION_VARIABLE,
                CONS,
                CONSP,
                CYCLIC_FUNCTION_INDIRECTION,
                CYCLIC_VARIABLE_INDIRECTION,
                DEFALIAS_FSET_FUNCTION,
                DEFUN,
                DOMAIN_ERROR,
                ELT,
                END_OF_BUFFER,
                END_OF_FILE,
                ERROR,
                ERROR_CONDITIONS,
                ERROR_MESSAGE,
                EXCESSIVE_LISP_NESTING,
                EXCESSIVE_VARIABLE_BINDING,
                FBOUNDP,
                FINALIZER,
                FIXNUMP,
                FIXNUM_OR_SYMBOL_WITH_POS_P,
                FLOAT,
                FLOATP,
                FONT_ENTITY,
                FONT_OBJECT,
                FONT_SPEC,
                FRAME,
                FUNCTION_HISTORY,
                HASH_TABLE,
                INHIBITED_INTERACTION,
                INTEGER,
                INTEGERP,
                INTEGER_OR_MARKER_P,
                INTERACTIVE_FORM,
                INTERPRETED_FUNCTION,
                INVALID_FUNCTION,
                INVALID_READ_SYNTAX,
                LAMBDA,
                LISTP,
                MAKUNBOUND,
                MANY,
                MARKER,
                MARKERP,
                MARK_INACTIVE,
                MINIBUFFER_QUIT,
                MODULE_FUNCTION,
                MOST_NEGATIVE_FIXNUM,
                MOST_POSITIVE_FIXNUM,
                MUTEX,
                NATIVE_COMP_FUNCTION,
                NATIVE_COMP_UNIT,
                NATNUMP,
                NO_CATCH,
                NTH,
                NUMBERP,
                NUMBER_OR_MARKER_P,
                OBARRAY,
                OCLOSURE_INTERACTIVE_FORM,
                OVERFLOW_ERROR,
                OVERLAY,
                PRIMITIVE_FUNCTION,
                PROCESS,
                QUIT,
                QUOTE,
                RANGE_ERROR,
                RECORD,
                RECORDP,
                RECURSION_ERROR,
                SEQUENCEP,
                SET,
                SETCAR,
                SETCDR,
                SETTING_CONSTANT,
                SET_DEFAULT,
                SINGULARITY_ERROR,
                SPECIAL_FORM,
                STRING,
                STRINGP,
                SUBR,
                SUBRP,
                SUBR_NATIVE_ELISP,
                SUB_CHAR_TABLE,
                SYMBOL,
                SYMBOLP,
                SYMBOLS_WITH_POS_ENABLED,
                SYMBOL_WITH_POS,
                SYMBOL_WITH_POS_P,
                TERMINAL,
                TEXT_READ_ONLY,
                THREAD,
                TOP_LEVEL,
                TRAPPING_CONSTANT,
                TREESIT_COMPILED_QUERY,
                TREESIT_NODE,
                TREESIT_PARSER,
                TYPE_MISMATCH,
                UNDERFLOW_ERROR,
                UNEVALLED,
                UNLET,
                USER_ERROR,
                USER_PTR,
                USER_PTRP,
                VECTOR,
                VECTORP,
                VECTOR_OR_CHAR_TABLE_P,
                VOID_FUNCTION,
                VOID_VARIABLE,
                WATCHERS,
                WHOLENUMP,
                WINDOW,
                WINDOW_CONFIGURATION,
                WRONG_LENGTH_ARGUMENT,
                WRONG_NUMBER_OF_ARGUMENTS,
                WRONG_TYPE_ARGUMENT,
                XWIDGET,
                XWIDGET_VIEW,
        };
    }
    //#endregion data.c
    //#region lread.c
    public final static ELispSymbol AFTER_LOAD_ALIST = new ELispSymbol("after-load-alist");
    public final static ELispSymbol ASCII_CHARACTER = new ELispSymbol("ascii-character");
    public final static ELispSymbol BACKQUOTE = new ELispSymbol("`");
    public final static ELispSymbol BYTECOMP_VERSION_REGEXP = new ELispSymbol("bytecomp-version-regexp");
    public final static ELispSymbol BYTE_BOOLEAN_VARS = new ELispSymbol("byte-boolean-vars");
    public final static ELispSymbol BYTE_RUN__UNESCAPED_CHARACTER_LITERALS_WARNING = new ELispSymbol("byte-run--unescaped-character-literals-warning");
    public final static ELispSymbol CHAR_FROM_NAME = new ELispSymbol("char-from-name");
    public final static ELispSymbol COMMA = new ELispSymbol(",");
    public final static ELispSymbol COMMA_AT = new ELispSymbol(",@");
    public final static ELispSymbol CURRENT_LOAD_LIST = new ELispSymbol("current-load-list");
    public final static ELispSymbol DATA = new ELispSymbol("data");
    public final static ELispSymbol DIR_OK = new ELispSymbol("dir-ok");
    public final static ELispSymbol DO_AFTER_LOAD_EVALUATION = new ELispSymbol("do-after-load-evaluation");
    public final static ELispSymbol DYNAMIC_LIBRARY_SUFFIXES = new ELispSymbol("dynamic-library-suffixes");
    public final static ELispSymbol EVAL_BUFFER_LIST = new ELispSymbol("eval-buffer-list");
    public final static ELispSymbol FORCE_LOAD_MESSAGES = new ELispSymbol("force-load-messages");
    public final static ELispSymbol FUNCTION = new ELispSymbol("function");
    public final static ELispSymbol GET_EMACS_MULE_FILE_CHAR = new ELispSymbol("get-emacs-mule-file-char");
    public final static ELispSymbol GET_FILE_CHAR = new ELispSymbol("get-file-char");
    public final static ELispSymbol HASH_TABLE = new ELispSymbol("hash-table");
    public final static ELispSymbol INHIBIT_FILE_NAME_OPERATION = new ELispSymbol("inhibit-file-name-operation");
    public final static ELispSymbol INTERNAL_MACROEXPAND_FOR_LOAD = new ELispSymbol("internal-macroexpand-for-load");
    public final static ELispSymbol LEXICAL_BINDING = new ELispSymbol("lexical-binding");
    public final static ELispSymbol LOAD = new ELispSymbol("load");
    public final static ELispSymbol LOAD_CONVERT_TO_UNIBYTE = new ELispSymbol("load-convert-to-unibyte");
    public final static ELispSymbol LOAD_DANGEROUS_LIBRARIES = new ELispSymbol("load-dangerous-libraries");
    public final static ELispSymbol LOAD_FILE_NAME = new ELispSymbol("load-file-name");
    public final static ELispSymbol LOAD_FILE_REP_SUFFIXES = new ELispSymbol("load-file-rep-suffixes");
    public final static ELispSymbol LOAD_FORCE_DOC_STRINGS = new ELispSymbol("load-force-doc-strings");
    public final static ELispSymbol LOAD_HISTORY = new ELispSymbol("load-history");
    public final static ELispSymbol LOAD_IN_PROGRESS = new ELispSymbol("load-in-progress");
    public final static ELispSymbol LOAD_NO_NATIVE = new ELispSymbol("load-no-native");
    public final static ELispSymbol LOAD_PATH = new ELispSymbol("load-path");
    public final static ELispSymbol LOAD_PREFER_NEWER = new ELispSymbol("load-prefer-newer");
    public final static ELispSymbol LOAD_READ_FUNCTION = new ELispSymbol("load-read-function");
    public final static ELispSymbol LOAD_SOURCE_FILE_FUNCTION = new ELispSymbol("load-source-file-function");
    public final static ELispSymbol LOAD_SUFFIXES = new ELispSymbol("load-suffixes");
    public final static ELispSymbol LOAD_TRUE_FILE_NAME = new ELispSymbol("load-true-file-name");
    public final static ELispSymbol LREAD__UNESCAPED_CHARACTER_LITERALS = new ELispSymbol("lread--unescaped-character-literals");
    public final static ELispSymbol MACROEXP__DYNVARS = new ELispSymbol("macroexp--dynvars");
    public final static ELispSymbol MODULE_FILE_SUFFIX = new ELispSymbol("module-file-suffix");
    public final static ELispSymbol NIL = new ELispSymbol("nil");
    public final static ELispSymbol OBARRAYP = new ELispSymbol("obarrayp");
    public final static ELispSymbol OBARRAY_CACHE = new ELispSymbol("obarray-cache");
    public final static ELispSymbol PRELOADED_FILE_LIST = new ELispSymbol("preloaded-file-list");
    public final static ELispSymbol PURECOPY = new ELispSymbol("purecopy");
    public final static ELispSymbol READ = new ELispSymbol("read");
    public final static ELispSymbol READ_CHAR = new ELispSymbol("read-char");
    public final static ELispSymbol READ_CIRCLE = new ELispSymbol("read-circle");
    public final static ELispSymbol READ_MINIBUFFER = new ELispSymbol("read-minibuffer");
    public final static ELispSymbol READ_SYMBOL_SHORTHANDS = new ELispSymbol("read-symbol-shorthands");
    public final static ELispSymbol SIZE = new ELispSymbol("size");
    public final static ELispSymbol SOURCE_DIRECTORY = new ELispSymbol("source-directory");
    public final static ELispSymbol STANDARD_INPUT = new ELispSymbol("standard-input");
    public final static ELispSymbol T = new ELispSymbol("t");
    public final static ELispSymbol TEST = new ELispSymbol("test");
    public final static ELispSymbol UNBOUND = new ELispSymbol("unbound");
    public final static ELispSymbol USER_INIT_FILE = new ELispSymbol("user-init-file");
    public final static ELispSymbol VALUES = new ELispSymbol("values");
    public final static ELispSymbol VARIABLE_DOCUMENTATION = new ELispSymbol("variable-documentation");
    public final static ELispSymbol WEAKNESS = new ELispSymbol("weakness");
    private ELispSymbol[] lreadSymbols() {
        return new ELispSymbol[]{
                AFTER_LOAD_ALIST,
                ASCII_CHARACTER,
                BACKQUOTE,
                BYTECOMP_VERSION_REGEXP,
                BYTE_BOOLEAN_VARS,
                BYTE_RUN__UNESCAPED_CHARACTER_LITERALS_WARNING,
                CHAR_FROM_NAME,
                COMMA,
                COMMA_AT,
                CURRENT_LOAD_LIST,
                DATA,
                DIR_OK,
                DO_AFTER_LOAD_EVALUATION,
                DYNAMIC_LIBRARY_SUFFIXES,
                EVAL_BUFFER_LIST,
                FORCE_LOAD_MESSAGES,
                FUNCTION,
                GET_EMACS_MULE_FILE_CHAR,
                GET_FILE_CHAR,
                HASH_TABLE,
                INHIBIT_FILE_NAME_OPERATION,
                INTERNAL_MACROEXPAND_FOR_LOAD,
                LEXICAL_BINDING,
                LOAD,
                LOAD_CONVERT_TO_UNIBYTE,
                LOAD_DANGEROUS_LIBRARIES,
                LOAD_FILE_NAME,
                LOAD_FILE_REP_SUFFIXES,
                LOAD_FORCE_DOC_STRINGS,
                LOAD_HISTORY,
                LOAD_IN_PROGRESS,
                LOAD_NO_NATIVE,
                LOAD_PATH,
                LOAD_PREFER_NEWER,
                LOAD_READ_FUNCTION,
                LOAD_SOURCE_FILE_FUNCTION,
                LOAD_SUFFIXES,
                LOAD_TRUE_FILE_NAME,
                LREAD__UNESCAPED_CHARACTER_LITERALS,
                MACROEXP__DYNVARS,
                MODULE_FILE_SUFFIX,
                NIL,
                OBARRAY,
                OBARRAYP,
                OBARRAY_CACHE,
                PRELOADED_FILE_LIST,
                PURECOPY,
                READ,
                READ_CHAR,
                READ_CIRCLE,
                READ_MINIBUFFER,
                READ_SYMBOL_SHORTHANDS,
                SIZE,
                SOURCE_DIRECTORY,
                STANDARD_INPUT,
                T,
                TEST,
                UNBOUND,
                USER_INIT_FILE,
                VALUES,
                VARIABLE_DOCUMENTATION,
                WEAKNESS,
        };
    }
    //#endregion lread.c
    //#region comp.c
    public final static ELispSymbol ADD1 = new ELispSymbol("1+");
    public final static ELispSymbol ASSUME = new ELispSymbol("assume");
    public final static ELispSymbol CALL = new ELispSymbol("call");
    public final static ELispSymbol CALLREF = new ELispSymbol("callref");
    public final static ELispSymbol CATCHER = new ELispSymbol("catcher");
    public final static ELispSymbol COMMENT = new ELispSymbol("comment");
    public final static ELispSymbol COMP_ABI_HASH = new ELispSymbol("comp-abi-hash");
    public final static ELispSymbol COMP_CTXT = new ELispSymbol("comp-ctxt");
    public final static ELispSymbol COMP_DEFERRED_PENDING_H = new ELispSymbol("comp-deferred-pending-h");
    public final static ELispSymbol COMP_ELN_TO_EL_H = new ELispSymbol("comp-eln-to-el-h");
    public final static ELispSymbol COMP_FILE_PRELOADED_P = new ELispSymbol("comp-file-preloaded-p");
    public final static ELispSymbol COMP_INSTALLED_TRAMPOLINES_H = new ELispSymbol("comp-installed-trampolines-h");
    public final static ELispSymbol COMP_LIBGCCJIT_REPRODUCER = new ELispSymbol("comp-libgccjit-reproducer");
    public final static ELispSymbol COMP_LOADED_COMP_UNITS_H = new ELispSymbol("comp-loaded-comp-units-h");
    public final static ELispSymbol COMP_MAYBE_GC_OR_QUIT = new ELispSymbol("comp-maybe-gc-or-quit");
    public final static ELispSymbol COMP_MVAR = new ELispSymbol("comp-mvar");
    public final static ELispSymbol COMP_NATIVE_VERSION_DIR = new ELispSymbol("comp-native-version-dir");
    public final static ELispSymbol COMP_NO_NATIVE_FILE_H = new ELispSymbol("comp-no-native-file-h");
    public final static ELispSymbol COMP_SANITIZER_ACTIVE = new ELispSymbol("comp-sanitizer-active");
    public final static ELispSymbol COMP_SANITIZER_ERROR = new ELispSymbol("comp-sanitizer-error");
    public final static ELispSymbol COMP_SUBR_ARITIES_H = new ELispSymbol("comp-subr-arities-h");
    public final static ELispSymbol COMP_SUBR_LIST = new ELispSymbol("comp-subr-list");
    public final static ELispSymbol COMP_SUBR_TRAMPOLINE_INSTALL = new ELispSymbol("comp-subr-trampoline-install");
    public final static ELispSymbol CONDITION_CASE = new ELispSymbol("condition-case");
    public final static ELispSymbol COND_JUMP = new ELispSymbol("cond-jump");
    public final static ELispSymbol COND_JUMP_NARG_LEQ = new ELispSymbol("cond-jump-narg-leq");
    public final static ELispSymbol DIRECT_CALL = new ELispSymbol("direct-call");
    public final static ELispSymbol DIRECT_CALLREF = new ELispSymbol("direct-callref");
    public final static ELispSymbol D_DEFAULT = new ELispSymbol("d-default");
    public final static ELispSymbol D_EPHEMERAL = new ELispSymbol("d-ephemeral");
    public final static ELispSymbol D_IMPURE = new ELispSymbol("d-impure");
    public final static ELispSymbol ENTRY = new ELispSymbol("entry");
    public final static ELispSymbol FETCH_HANDLER = new ELispSymbol("fetch-handler");
    public final static ELispSymbol FIXNUM = new ELispSymbol("fixnum");
    public final static ELispSymbol GCCJIT = new ELispSymbol("gccjit");
    public final static ELispSymbol HELPER_SANITIZER_ASSERT = new ELispSymbol("helper_sanitizer_assert");
    public final static ELispSymbol HELPER_SAVE_RESTRICTION = new ELispSymbol("helper_save_restriction");
    public final static ELispSymbol HELPER_UNBIND_N = new ELispSymbol("helper_unbind_n");
    public final static ELispSymbol HELPER_UNWIND_PROTECT = new ELispSymbol("helper_unwind_protect");
    public final static ELispSymbol INC_ARGS = new ELispSymbol("inc-args");
    public final static ELispSymbol JUMP = new ELispSymbol("jump");
    public final static ELispSymbol LAMBDA_FIXUP = new ELispSymbol("lambda-fixup");
    public final static ELispSymbol LATE = new ELispSymbol("late");
    public final static ELispSymbol NATIVE_COMPILER = new ELispSymbol("native-compiler");
    public final static ELispSymbol NATIVE_COMPILER_ERROR = new ELispSymbol("native-compiler-error");
    public final static ELispSymbol NATIVE_COMP_COMPILER_OPTIONS = new ELispSymbol("native-comp-compiler-options");
    public final static ELispSymbol NATIVE_COMP_DEBUG = new ELispSymbol("native-comp-debug");
    public final static ELispSymbol NATIVE_COMP_DRIVER_OPTIONS = new ELispSymbol("native-comp-driver-options");
    public final static ELispSymbol NATIVE_COMP_ELN_LOAD_PATH = new ELispSymbol("native-comp-eln-load-path");
    public final static ELispSymbol NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES = new ELispSymbol("native-comp-enable-subr-trampolines");
    public final static ELispSymbol NATIVE_COMP_JIT_COMPILATION = new ELispSymbol("native-comp-jit-compilation");
    public final static ELispSymbol NATIVE_COMP_SPEED = new ELispSymbol("native-comp-speed");
    public final static ELispSymbol NATIVE_COMP_WARNING_ON_MISSING_SOURCE = new ELispSymbol("native-comp-warning-on-missing-source");
    public final static ELispSymbol NATIVE_ICE = new ELispSymbol("native-ice");
    public final static ELispSymbol NATIVE_LISP_FILE_INCONSISTENT = new ELispSymbol("native-lisp-file-inconsistent");
    public final static ELispSymbol NATIVE_LISP_LOAD_FAILED = new ELispSymbol("native-lisp-load-failed");
    public final static ELispSymbol NATIVE_LISP_WRONG_RELOC = new ELispSymbol("native-lisp-wrong-reloc");
    public final static ELispSymbol NATIVE__COMPILE_ASYNC = new ELispSymbol("native--compile-async");
    public final static ELispSymbol NEGATE = new ELispSymbol("negate");
    public final static ELispSymbol PHI = new ELispSymbol("phi");
    public final static ELispSymbol POP_HANDLER = new ELispSymbol("pop-handler");
    public final static ELispSymbol PUSH_HANDLER = new ELispSymbol("push-handler");
    public final static ELispSymbol RECORD_UNWIND_CURRENT_BUFFER = new ELispSymbol("record_unwind_current_buffer");
    public final static ELispSymbol RECORD_UNWIND_PROTECT_EXCURSION = new ELispSymbol("record_unwind_protect_excursion");
    public final static ELispSymbol RETURN = new ELispSymbol("return");
    public final static ELispSymbol SCRATCH = new ELispSymbol("scratch");
    public final static ELispSymbol SETIMM = new ELispSymbol("setimm");
    public final static ELispSymbol SET_ARGS_TO_LOCAL = new ELispSymbol("set-args-to-local");
    public final static ELispSymbol SET_INTERNAL = new ELispSymbol("set_internal");
    public final static ELispSymbol SET_PAR_TO_LOCAL = new ELispSymbol("set-par-to-local");
    public final static ELispSymbol SET_REST_ARGS_TO_LOCAL = new ELispSymbol("set-rest-args-to-local");
    public final static ELispSymbol SUB1 = new ELispSymbol("1-");
    public final static ELispSymbol UNREACHABLE = new ELispSymbol("unreachable");
    public final static ELispSymbol WRONG_REGISTER_SUBR_CALL = new ELispSymbol("wrong-register-subr-call");
    private ELispSymbol[] compSymbols() {
        return new ELispSymbol[]{
                ADD1,
                ASSUME,
                CALL,
                CALLREF,
                CAR,
                CATCHER,
                CDR,
                COMMENT,
                COMP_ABI_HASH,
                COMP_CTXT,
                COMP_DEFERRED_PENDING_H,
                COMP_ELN_TO_EL_H,
                COMP_FILE_PRELOADED_P,
                COMP_INSTALLED_TRAMPOLINES_H,
                COMP_LIBGCCJIT_REPRODUCER,
                COMP_LOADED_COMP_UNITS_H,
                COMP_MAYBE_GC_OR_QUIT,
                COMP_MVAR,
                COMP_NATIVE_VERSION_DIR,
                COMP_NO_NATIVE_FILE_H,
                COMP_SANITIZER_ACTIVE,
                COMP_SANITIZER_ERROR,
                COMP_SUBR_ARITIES_H,
                COMP_SUBR_LIST,
                COMP_SUBR_TRAMPOLINE_INSTALL,
                CONDITION_CASE,
                COND_JUMP,
                COND_JUMP_NARG_LEQ,
                CONSP,
                DIRECT_CALL,
                DIRECT_CALLREF,
                D_DEFAULT,
                D_EPHEMERAL,
                D_IMPURE,
                ENTRY,
                FETCH_HANDLER,
                FIXNUM,
                GCCJIT,
                HELPER_SANITIZER_ASSERT,
                HELPER_SAVE_RESTRICTION,
                HELPER_UNBIND_N,
                HELPER_UNWIND_PROTECT,
                INC_ARGS,
                INTEGERP,
                JUMP,
                LAMBDA_FIXUP,
                LATE,
                NATIVE_COMPILER,
                NATIVE_COMPILER_ERROR,
                NATIVE_COMP_COMPILER_OPTIONS,
                NATIVE_COMP_DEBUG,
                NATIVE_COMP_DRIVER_OPTIONS,
                NATIVE_COMP_ELN_LOAD_PATH,
                NATIVE_COMP_ENABLE_SUBR_TRAMPOLINES,
                NATIVE_COMP_JIT_COMPILATION,
                NATIVE_COMP_SPEED,
                NATIVE_COMP_WARNING_ON_MISSING_SOURCE,
                NATIVE_ICE,
                NATIVE_LISP_FILE_INCONSISTENT,
                NATIVE_LISP_LOAD_FAILED,
                NATIVE_LISP_WRONG_RELOC,
                NATIVE__COMPILE_ASYNC,
                NEGATE,
                NUMBERP,
                PHI,
                POP_HANDLER,
                PUSH_HANDLER,
                RECORD_UNWIND_CURRENT_BUFFER,
                RECORD_UNWIND_PROTECT_EXCURSION,
                RETURN,
                SCRATCH,
                SETCAR,
                SETCDR,
                SETIMM,
                SET_ARGS_TO_LOCAL,
                SET_INTERNAL,
                SET_PAR_TO_LOCAL,
                SET_REST_ARGS_TO_LOCAL,
                SUB1,
                SYMBOL_WITH_POS_P,
                UNREACHABLE,
                WRONG_REGISTER_SUBR_CALL,
        };
    }
    //#endregion comp.c
    //#region process.c
    public final static ELispSymbol ALL = new ELispSymbol("all");
    public final static ELispSymbol ARGS = new ELispSymbol("args");
    public final static ELispSymbol CBUFFER = new ELispSymbol(":buffer");
    public final static ELispSymbol CBYTESIZE = new ELispSymbol(":bytesize");
    public final static ELispSymbol CCODING = new ELispSymbol(":coding");
    public final static ELispSymbol CCOMMAND = new ELispSymbol(":command");
    public final static ELispSymbol CCONNECTION_TYPE = new ELispSymbol(":connection-type");
    public final static ELispSymbol CFILE_HANDLER = new ELispSymbol(":file-handler");
    public final static ELispSymbol CFLOWCONTROL = new ELispSymbol(":flowcontrol");
    public final static ELispSymbol CHOST = new ELispSymbol(":host");
    public final static ELispSymbol CLOCAL = new ELispSymbol(":local");
    public final static ELispSymbol CLOG = new ELispSymbol(":log");
    public final static ELispSymbol CLOSED = new ELispSymbol("closed");
    public final static ELispSymbol CMAJFLT = new ELispSymbol("cmajflt");
    public final static ELispSymbol CMINFLT = new ELispSymbol("cminflt");
    public final static ELispSymbol CNAME = new ELispSymbol(":name");
    public final static ELispSymbol CNOQUERY = new ELispSymbol(":noquery");
    public final static ELispSymbol CNOWAIT = new ELispSymbol(":nowait");
    public final static ELispSymbol COMM = new ELispSymbol("comm");
    public final static ELispSymbol CONNECT = new ELispSymbol("connect");
    public final static ELispSymbol CPARITY = new ELispSymbol(":parity");
    public final static ELispSymbol CPLIST = new ELispSymbol(":plist");
    public final static ELispSymbol CPORT = new ELispSymbol(":port");
    public final static ELispSymbol CPROCESS = new ELispSymbol(":process");
    public final static ELispSymbol CREMOTE = new ELispSymbol(":remote");
    public final static ELispSymbol CSENTINEL = new ELispSymbol(":sentinel");
    public final static ELispSymbol CSERVER = new ELispSymbol(":server");
    public final static ELispSymbol CSERVICE = new ELispSymbol(":service");
    public final static ELispSymbol CSPEED = new ELispSymbol(":speed");
    public final static ELispSymbol CSTDERR = new ELispSymbol(":stderr");
    public final static ELispSymbol CSTIME = new ELispSymbol("cstime");
    public final static ELispSymbol CSTOP = new ELispSymbol(":stop");
    public final static ELispSymbol CSTOPBITS = new ELispSymbol(":stopbits");
    public final static ELispSymbol CSUMMARY = new ELispSymbol(":summary");
    public final static ELispSymbol CTIME = new ELispSymbol("ctime");
    public final static ELispSymbol CTLS_PARAMETERS = new ELispSymbol(":tls-parameters");
    public final static ELispSymbol CTYPE = new ELispSymbol(":type");
    public final static ELispSymbol CURRENT = new ELispSymbol("current");
    public final static ELispSymbol CUSE_EXTERNAL_SOCKET = new ELispSymbol(":use-external-socket");
    public final static ELispSymbol CUTIME = new ELispSymbol("cutime");
    public final static ELispSymbol DATAGRAM = new ELispSymbol("datagram");
    public final static ELispSymbol DELETE_EXITED_PROCESSES = new ELispSymbol("delete-exited-processes");
    public final static ELispSymbol EGID = new ELispSymbol("egid");
    public final static ELispSymbol ETIME = new ELispSymbol("etime");
    public final static ELispSymbol EUID = new ELispSymbol("euid");
    public final static ELispSymbol EVEN = new ELispSymbol("even");
    public final static ELispSymbol FAILED = new ELispSymbol("failed");
    public final static ELispSymbol FAST_READ_PROCESS_OUTPUT = new ELispSymbol("fast-read-process-output");
    public final static ELispSymbol GROUP = new ELispSymbol("group");
    public final static ELispSymbol HW = new ELispSymbol("hw");
    public final static ELispSymbol INTERNAL_DEFAULT_INTERRUPT_PROCESS = new ELispSymbol("internal-default-interrupt-process");
    public final static ELispSymbol INTERNAL_DEFAULT_PROCESS_FILTER = new ELispSymbol("internal-default-process-filter");
    public final static ELispSymbol INTERNAL_DEFAULT_PROCESS_SENTINEL = new ELispSymbol("internal-default-process-sentinel");
    public final static ELispSymbol INTERNAL_DEFAULT_SIGNAL_PROCESS = new ELispSymbol("internal-default-signal-process");
    public final static ELispSymbol INTERNAL__DAEMON_SOCKNAME = new ELispSymbol("internal--daemon-sockname");
    public final static ELispSymbol INTERRUPT_PROCESS_FUNCTIONS = new ELispSymbol("interrupt-process-functions");
    public final static ELispSymbol IPV4 = new ELispSymbol("ipv4");
    public final static ELispSymbol IPV6 = new ELispSymbol("ipv6");
    public final static ELispSymbol LAST_NONMENU_EVENT = new ELispSymbol("last-nonmenu-event");
    public final static ELispSymbol LISTEN = new ELispSymbol("listen");
    public final static ELispSymbol LIST_SYSTEM_PROCESSES = new ELispSymbol("list-system-processes");
    public final static ELispSymbol LOCAL = new ELispSymbol("local");
    public final static ELispSymbol MAJFLT = new ELispSymbol("majflt");
    public final static ELispSymbol MAKE_PROCESS = new ELispSymbol("make-process");
    public final static ELispSymbol MESSAGE = new ELispSymbol("message");
    public final static ELispSymbol MINFLT = new ELispSymbol("minflt");
    public final static ELispSymbol NETWORK = new ELispSymbol("network");
    public final static ELispSymbol NICE = new ELispSymbol("nice");
    public final static ELispSymbol NSM_VERIFY_CONNECTION = new ELispSymbol("nsm-verify-connection");
    public final static ELispSymbol NULL = new ELispSymbol("null");
    public final static ELispSymbol NUMERIC = new ELispSymbol("numeric");
    public final static ELispSymbol ODD = new ELispSymbol("odd");
    public final static ELispSymbol OPEN = new ELispSymbol("open");
    public final static ELispSymbol PCPU = new ELispSymbol("pcpu");
    public final static ELispSymbol PGRP = new ELispSymbol("pgrp");
    public final static ELispSymbol PIPE = new ELispSymbol("pipe");
    public final static ELispSymbol PIPE_PROCESS_P = new ELispSymbol("pipe-process-p");
    public final static ELispSymbol PMEM = new ELispSymbol("pmem");
    public final static ELispSymbol PPID = new ELispSymbol("ppid");
    public final static ELispSymbol PRI = new ELispSymbol("pri");
    public final static ELispSymbol PROCESSP = new ELispSymbol("processp");
    public final static ELispSymbol PROCESS_ADAPTIVE_READ_BUFFERING = new ELispSymbol("process-adaptive-read-buffering");
    public final static ELispSymbol PROCESS_ATTRIBUTES = new ELispSymbol("process-attributes");
    public final static ELispSymbol PROCESS_CONNECTION_TYPE = new ELispSymbol("process-connection-type");
    public final static ELispSymbol PROCESS_ERROR_PAUSE_TIME = new ELispSymbol("process-error-pause-time");
    public final static ELispSymbol PROCESS_PRIORITIZE_LOWER_FDS = new ELispSymbol("process-prioritize-lower-fds");
    public final static ELispSymbol PTY = new ELispSymbol("pty");
    public final static ELispSymbol READ_PROCESS_OUTPUT_MAX = new ELispSymbol("read-process-output-max");
    public final static ELispSymbol REAL = new ELispSymbol("real");
    public final static ELispSymbol RSS = new ELispSymbol("rss");
    public final static ELispSymbol RUN = new ELispSymbol("run");
    public final static ELispSymbol SEQPACKET = new ELispSymbol("seqpacket");
    public final static ELispSymbol SERIAL = new ELispSymbol("serial");
    public final static ELispSymbol SESS = new ELispSymbol("sess");
    public final static ELispSymbol SIGNAL = new ELispSymbol("signal");
    public final static ELispSymbol SIGNAL_PROCESS_FUNCTIONS = new ELispSymbol("signal-process-functions");
    public final static ELispSymbol START = new ELispSymbol("start");
    public final static ELispSymbol STATE = new ELispSymbol("state");
    public final static ELispSymbol STIME = new ELispSymbol("stime");
    public final static ELispSymbol STOP = new ELispSymbol("stop");
    public final static ELispSymbol SW = new ELispSymbol("sw");
    public final static ELispSymbol THCOUNT = new ELispSymbol("thcount");
    public final static ELispSymbol TIME = new ELispSymbol("time");
    public final static ELispSymbol TPGID = new ELispSymbol("tpgid");
    public final static ELispSymbol TTNAME = new ELispSymbol("ttname");
    public final static ELispSymbol USER = new ELispSymbol("user");
    public final static ELispSymbol UTIME = new ELispSymbol("utime");
    public final static ELispSymbol VSIZE = new ELispSymbol("vsize");
    private ELispSymbol[] processSymbols() {
        return new ELispSymbol[]{
                ALL,
                ARGS,
                CBUFFER,
                CBYTESIZE,
                CCODING,
                CCOMMAND,
                CCONNECTION_TYPE,
                CFILE_HANDLER,
                CFLOWCONTROL,
                CHOST,
                CLOCAL,
                CLOG,
                CLOSED,
                CMAJFLT,
                CMINFLT,
                CNAME,
                CNOQUERY,
                CNOWAIT,
                COMM,
                CONNECT,
                CPARITY,
                CPLIST,
                CPORT,
                CPROCESS,
                CREMOTE,
                CSENTINEL,
                CSERVER,
                CSERVICE,
                CSPEED,
                CSTDERR,
                CSTIME,
                CSTOP,
                CSTOPBITS,
                CSUMMARY,
                CTIME,
                CTLS_PARAMETERS,
                CTYPE,
                CURRENT,
                CUSE_EXTERNAL_SOCKET,
                CUTIME,
                DATAGRAM,
                DELETE_EXITED_PROCESSES,
                EGID,
                ETIME,
                EUID,
                EVEN,
                FAILED,
                FAST_READ_PROCESS_OUTPUT,
                GROUP,
                HW,
                INTERNAL_DEFAULT_INTERRUPT_PROCESS,
                INTERNAL_DEFAULT_PROCESS_FILTER,
                INTERNAL_DEFAULT_PROCESS_SENTINEL,
                INTERNAL_DEFAULT_SIGNAL_PROCESS,
                INTERNAL__DAEMON_SOCKNAME,
                INTERRUPT_PROCESS_FUNCTIONS,
                IPV4,
                IPV6,
                LAST_NONMENU_EVENT,
                LISTEN,
                LIST_SYSTEM_PROCESSES,
                LOCAL,
                MAJFLT,
                MAKE_PROCESS,
                MESSAGE,
                MINFLT,
                NETWORK,
                NICE,
                NSM_VERIFY_CONNECTION,
                NULL,
                NUMERIC,
                ODD,
                OPEN,
                PCPU,
                PGRP,
                PIPE,
                PIPE_PROCESS_P,
                PMEM,
                PPID,
                PRI,
                PROCESSP,
                PROCESS_ADAPTIVE_READ_BUFFERING,
                PROCESS_ATTRIBUTES,
                PROCESS_CONNECTION_TYPE,
                PROCESS_ERROR_PAUSE_TIME,
                PROCESS_PRIORITIZE_LOWER_FDS,
                PTY,
                READ_PROCESS_OUTPUT_MAX,
                REAL,
                RSS,
                RUN,
                SEQPACKET,
                SERIAL,
                SESS,
                SIGNAL,
                SIGNAL_PROCESS_FUNCTIONS,
                START,
                STATE,
                STIME,
                STOP,
                SW,
                THCOUNT,
                TIME,
                TPGID,
                TTNAME,
                USER,
                UTIME,
                VSIZE,
        };
    }
    //#endregion process.c
    //#region eval.c
    public final static ELispSymbol AND_OPTIONAL = new ELispSymbol("&optional");
    public final static ELispSymbol AND_REST = new ELispSymbol("&rest");
    public final static ELispSymbol AUTOLOAD = new ELispSymbol("autoload");
    public final static ELispSymbol BACKTRACE_ON_ERROR_NONINTERACTIVE = new ELispSymbol("backtrace-on-error-noninteractive");
    public final static ELispSymbol BACKTRACE_ON_REDISPLAY_ERROR = new ELispSymbol("backtrace-on-redisplay-error");
    public final static ELispSymbol CDEBUG_ON_EXIT = new ELispSymbol(":debug-on-exit");
    public final static ELispSymbol CDOCUMENTATION = new ELispSymbol(":documentation");
    public final static ELispSymbol COMMANDP = new ELispSymbol("commandp");
    public final static ELispSymbol CSUCCESS = new ELispSymbol(":success");
    public final static ELispSymbol DEBUG = new ELispSymbol("debug");
    public final static ELispSymbol DEBUGGER = new ELispSymbol("debugger");
    public final static ELispSymbol DEBUGGER_MAY_CONTINUE = new ELispSymbol("debugger-may-continue");
    public final static ELispSymbol DEBUGGER_STACK_FRAME_AS_LIST = new ELispSymbol("debugger-stack-frame-as-list");
    public final static ELispSymbol DEBUG_EARLY = new ELispSymbol("debug-early");
    public final static ELispSymbol DEBUG_EARLY__HANDLER = new ELispSymbol("debug-early--handler");
    public final static ELispSymbol DEBUG_IGNORED_ERRORS = new ELispSymbol("debug-ignored-errors");
    public final static ELispSymbol DEBUG_ON_ERROR = new ELispSymbol("debug-on-error");
    public final static ELispSymbol DEBUG_ON_NEXT_CALL = new ELispSymbol("debug-on-next-call");
    public final static ELispSymbol DEBUG_ON_QUIT = new ELispSymbol("debug-on-quit");
    public final static ELispSymbol DEBUG_ON_SIGNAL = new ELispSymbol("debug-on-signal");
    public final static ELispSymbol DEFVARALIAS = new ELispSymbol("defvaralias");
    public final static ELispSymbol DISPLAY_WARNING = new ELispSymbol("display-warning");
    public final static ELispSymbol EXIT = new ELispSymbol("exit");
    public final static ELispSymbol FUNCTIONP = new ELispSymbol("functionp");
    public final static ELispSymbol INHIBIT_DEBUGGER = new ELispSymbol("inhibit-debugger");
    public final static ELispSymbol INHIBIT_QUIT = new ELispSymbol("inhibit-quit");
    public final static ELispSymbol INTERACTIVE = new ELispSymbol("interactive");
    public final static ELispSymbol INTERNAL_INTERPRETER_ENVIRONMENT = new ELispSymbol("internal-interpreter-environment");
    public final static ELispSymbol INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION = new ELispSymbol("internal-make-interpreted-closure-function");
    public final static ELispSymbol INTERNAL_WHEN_ENTERED_DEBUGGER = new ELispSymbol("internal-when-entered-debugger");
    public final static ELispSymbol LISP_EVAL_DEPTH_RESERVE = new ELispSymbol("lisp-eval-depth-reserve");
    public final static ELispSymbol LOSING_VALUE = new ELispSymbol("losing-value");
    public final static ELispSymbol MACRO = new ELispSymbol("macro");
    public final static ELispSymbol MAX_LISP_EVAL_DEPTH = new ELispSymbol("max-lisp-eval-depth");
    public final static ELispSymbol QUIT_FLAG = new ELispSymbol("quit-flag");
    public final static ELispSymbol SETQ = new ELispSymbol("setq");
    public final static ELispSymbol SIGNAL_HOOK_FUNCTION = new ELispSymbol("signal-hook-function");
    private ELispSymbol[] evalSymbols() {
        return new ELispSymbol[]{
                AND_OPTIONAL,
                AND_REST,
                AUTOLOAD,
                BACKTRACE_ON_ERROR_NONINTERACTIVE,
                BACKTRACE_ON_REDISPLAY_ERROR,
                CDEBUG_ON_EXIT,
                CDOCUMENTATION,
                COMMANDP,
                CSUCCESS,
                DEBUG,
                DEBUGGER,
                DEBUGGER_MAY_CONTINUE,
                DEBUGGER_STACK_FRAME_AS_LIST,
                DEBUG_EARLY,
                DEBUG_EARLY__HANDLER,
                DEBUG_IGNORED_ERRORS,
                DEBUG_ON_ERROR,
                DEBUG_ON_NEXT_CALL,
                DEBUG_ON_QUIT,
                DEBUG_ON_SIGNAL,
                DEFVARALIAS,
                DISPLAY_WARNING,
                EXIT,
                FUNCTIONP,
                INHIBIT_DEBUGGER,
                INHIBIT_QUIT,
                INTERACTIVE,
                INTERNAL_INTERPRETER_ENVIRONMENT,
                INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION,
                INTERNAL_WHEN_ENTERED_DEBUGGER,
                LISP_EVAL_DEPTH_RESERVE,
                LOSING_VALUE,
                MACRO,
                MAX_LISP_EVAL_DEPTH,
                QUIT_FLAG,
                SETQ,
                SIGNAL_HOOK_FUNCTION,
        };
    }
    //#endregion eval.c
    //#region fns.c
    public final static ELispSymbol CIN_PLACE = new ELispSymbol(":in-place");
    public final static ELispSymbol CKEY = new ELispSymbol(":key");
    public final static ELispSymbol CLESSP = new ELispSymbol(":lessp");
    public final static ELispSymbol CODESET = new ELispSymbol("codeset");
    public final static ELispSymbol CPURECOPY = new ELispSymbol(":purecopy");
    public final static ELispSymbol CREHASH_SIZE = new ELispSymbol(":rehash-size");
    public final static ELispSymbol CREHASH_THRESHOLD = new ELispSymbol(":rehash-threshold");
    public final static ELispSymbol CREVERSE = new ELispSymbol(":reverse");
    public final static ELispSymbol CSIZE = new ELispSymbol(":size");
    public final static ELispSymbol CTEST = new ELispSymbol(":test");
    public final static ELispSymbol CURSOR_IN_ECHO_AREA = new ELispSymbol("cursor-in-echo-area");
    public final static ELispSymbol CWEAKNESS = new ELispSymbol(":weakness");
    public final static ELispSymbol DAYS = new ELispSymbol("days");
    public final static ELispSymbol EQ = new ELispSymbol("eq");
    public final static ELispSymbol EQL = new ELispSymbol("eql");
    public final static ELispSymbol EQUAL = new ELispSymbol("equal");
    public final static ELispSymbol FEATURES = new ELispSymbol("features");
    public final static ELispSymbol FROM__TTY_MENU_P = new ELispSymbol("from--tty-menu-p");
    public final static ELispSymbol FUNCALL = new ELispSymbol("funcall");
    public final static ELispSymbol HASH_TABLE_P = new ELispSymbol("hash-table-p");
    public final static ELispSymbol HASH_TABLE_TEST = new ELispSymbol("hash-table-test");
    public final static ELispSymbol IV_AUTO = new ELispSymbol("iv-auto");
    public final static ELispSymbol KEY = new ELispSymbol("key");
    public final static ELispSymbol KEY_AND_VALUE = new ELispSymbol("key-and-value");
    public final static ELispSymbol KEY_OR_VALUE = new ELispSymbol("key-or-value");
    public final static ELispSymbol LIST_OR_VECTOR_P = new ELispSymbol("list-or-vector-p");
    public final static ELispSymbol MD5 = new ELispSymbol("md5");
    public final static ELispSymbol MONTHS = new ELispSymbol("months");
    public final static ELispSymbol OVERRIDING_PLIST_ENVIRONMENT = new ELispSymbol("overriding-plist-environment");
    public final static ELispSymbol PAPER = new ELispSymbol("paper");
    public final static ELispSymbol PLISTP = new ELispSymbol("plistp");
    public final static ELispSymbol PROVIDE = new ELispSymbol("provide");
    public final static ELispSymbol REAL_THIS_COMMAND = new ELispSymbol("real-this-command");
    public final static ELispSymbol REQUIRE = new ELispSymbol("require");
    public final static ELispSymbol SHA1 = new ELispSymbol("sha1");
    public final static ELispSymbol SHA224 = new ELispSymbol("sha224");
    public final static ELispSymbol SHA256 = new ELispSymbol("sha256");
    public final static ELispSymbol SHA384 = new ELispSymbol("sha384");
    public final static ELispSymbol SHA512 = new ELispSymbol("sha512");
    public final static ELispSymbol STRING_LESSP = new ELispSymbol("string-lessp");
    public final static ELispSymbol SUBFEATURES = new ELispSymbol("subfeatures");
    public final static ELispSymbol USE_DIALOG_BOX = new ELispSymbol("use-dialog-box");
    public final static ELispSymbol USE_FILE_DIALOG = new ELispSymbol("use-file-dialog");
    public final static ELispSymbol USE_SHORT_ANSWERS = new ELispSymbol("use-short-answers");
    public final static ELispSymbol VALUE = new ELispSymbol("value");
    public final static ELispSymbol VALUELT = new ELispSymbol("value<");
    public final static ELispSymbol WIDGET_TYPE = new ELispSymbol("widget-type");
    public final static ELispSymbol YES_OR_NO_P = new ELispSymbol("yes-or-no-p");
    public final static ELispSymbol YES_OR_NO_PROMPT = new ELispSymbol("yes-or-no-prompt");
    public final static ELispSymbol YES_OR_NO_P_HISTORY = new ELispSymbol("yes-or-no-p-history");
    public final static ELispSymbol Y_OR_N_P = new ELispSymbol("y-or-n-p");
    private ELispSymbol[] fnsSymbols() {
        return new ELispSymbol[]{
                CIN_PLACE,
                CKEY,
                CLESSP,
                CODESET,
                CPURECOPY,
                CREHASH_SIZE,
                CREHASH_THRESHOLD,
                CREVERSE,
                CSIZE,
                CTEST,
                CURSOR_IN_ECHO_AREA,
                CWEAKNESS,
                DAYS,
                EQ,
                EQL,
                EQUAL,
                FEATURES,
                FROM__TTY_MENU_P,
                FUNCALL,
                HASH_TABLE_P,
                HASH_TABLE_TEST,
                IV_AUTO,
                KEY,
                KEY_AND_VALUE,
                KEY_OR_VALUE,
                LIST_OR_VECTOR_P,
                MD5,
                MONTHS,
                OVERRIDING_PLIST_ENVIRONMENT,
                PAPER,
                PLISTP,
                PROVIDE,
                REAL_THIS_COMMAND,
                REQUIRE,
                SHA1,
                SHA224,
                SHA256,
                SHA384,
                SHA512,
                STRING_LESSP,
                SUBFEATURES,
                USE_DIALOG_BOX,
                USE_FILE_DIALOG,
                USE_SHORT_ANSWERS,
                VALUE,
                VALUELT,
                WIDGET_TYPE,
                YES_OR_NO_P,
                YES_OR_NO_PROMPT,
                YES_OR_NO_P_HISTORY,
                Y_OR_N_P,
        };
    }
    //#endregion fns.c
    //#region chartab.c
    public final static ELispSymbol CHAR_CODE_PROPERTY_ALIST = new ELispSymbol("char-code-property-alist");
    public final static ELispSymbol CHAR_CODE_PROPERTY_TABLE = new ELispSymbol("char-code-property-table");
    private ELispSymbol[] chartabSymbols() {
        return new ELispSymbol[]{
                CHAR_CODE_PROPERTY_ALIST,
                CHAR_CODE_PROPERTY_TABLE,
        };
    }
    //#endregion chartab.c
    //#region alloc.c
    public final static ELispSymbol ALLOC = new ELispSymbol("alloc");
    public final static ELispSymbol AUTOMATIC_GC = new ELispSymbol("Automatic GC");
    public final static ELispSymbol BUFFERS = new ELispSymbol("buffers");
    public final static ELispSymbol CEMERGENCY = new ELispSymbol(":emergency");
    public final static ELispSymbol CHAR_TABLE_EXTRA_SLOTS = new ELispSymbol("char-table-extra-slots");
    public final static ELispSymbol CONSES = new ELispSymbol("conses");
    public final static ELispSymbol CONS_CELLS_CONSED = new ELispSymbol("cons-cells-consed");
    public final static ELispSymbol FLOATS = new ELispSymbol("floats");
    public final static ELispSymbol FLOATS_CONSED = new ELispSymbol("floats-consed");
    public final static ELispSymbol GARBAGE_COLLECTION_MESSAGES = new ELispSymbol("garbage-collection-messages");
    public final static ELispSymbol GCS_DONE = new ELispSymbol("gcs-done");
    public final static ELispSymbol GC_CONS_PERCENTAGE = new ELispSymbol("gc-cons-percentage");
    public final static ELispSymbol GC_CONS_THRESHOLD = new ELispSymbol("gc-cons-threshold");
    public final static ELispSymbol GC_ELAPSED = new ELispSymbol("gc-elapsed");
    public final static ELispSymbol HEAP = new ELispSymbol("heap");
    public final static ELispSymbol INTEGER_WIDTH = new ELispSymbol("integer-width");
    public final static ELispSymbol INTERVALS = new ELispSymbol("intervals");
    public final static ELispSymbol INTERVALS_CONSED = new ELispSymbol("intervals-consed");
    public final static ELispSymbol MEMORY_FULL = new ELispSymbol("memory-full");
    public final static ELispSymbol MEMORY_INFO = new ELispSymbol("memory-info");
    public final static ELispSymbol MEMORY_SIGNAL_DATA = new ELispSymbol("memory-signal-data");
    public final static ELispSymbol POST_GC_HOOK = new ELispSymbol("post-gc-hook");
    public final static ELispSymbol PURE_BYTES_USED = new ELispSymbol("pure-bytes-used");
    public final static ELispSymbol PURIFY_FLAG = new ELispSymbol("purify-flag");
    public final static ELispSymbol STRINGS = new ELispSymbol("strings");
    public final static ELispSymbol STRINGS_CONSED = new ELispSymbol("strings-consed");
    public final static ELispSymbol STRING_BYTES = new ELispSymbol("string-bytes");
    public final static ELispSymbol STRING_CHARS_CONSED = new ELispSymbol("string-chars-consed");
    public final static ELispSymbol SYMBOLS = new ELispSymbol("symbols");
    public final static ELispSymbol SYMBOLS_CONSED = new ELispSymbol("symbols-consed");
    public final static ELispSymbol VECTORS = new ELispSymbol("vectors");
    public final static ELispSymbol VECTOR_CELLS_CONSED = new ELispSymbol("vector-cells-consed");
    public final static ELispSymbol VECTOR_SLOTS = new ELispSymbol("vector-slots");
    private ELispSymbol[] allocSymbols() {
        return new ELispSymbol[]{
                ALLOC,
                AUTOMATIC_GC,
                BUFFERS,
                CEMERGENCY,
                CHAR_TABLE_EXTRA_SLOTS,
                CONSES,
                CONS_CELLS_CONSED,
                FLOATS,
                FLOATS_CONSED,
                GARBAGE_COLLECTION_MESSAGES,
                GCS_DONE,
                GC_CONS_PERCENTAGE,
                GC_CONS_THRESHOLD,
                GC_ELAPSED,
                HEAP,
                INTEGER_WIDTH,
                INTERVALS,
                INTERVALS_CONSED,
                MEMORY_FULL,
                MEMORY_INFO,
                MEMORY_SIGNAL_DATA,
                POST_GC_HOOK,
                PURE_BYTES_USED,
                PURIFY_FLAG,
                STRINGS,
                STRINGS_CONSED,
                STRING_BYTES,
                STRING_CHARS_CONSED,
                SYMBOLS,
                SYMBOLS_CONSED,
                VECTORS,
                VECTOR_CELLS_CONSED,
                VECTOR_SLOTS,
        };
    }
    //#endregion alloc.c
    //#region charset.c
    public final static ELispSymbol ASCII = new ELispSymbol("ascii");
    public final static ELispSymbol CHARSETP = new ELispSymbol("charsetp");
    public final static ELispSymbol CHARSET_LIST = new ELispSymbol("charset-list");
    public final static ELispSymbol CHARSET_MAP_PATH = new ELispSymbol("charset-map-path");
    public final static ELispSymbol CURRENT_ISO639_LANGUAGE = new ELispSymbol("current-iso639-language");
    public final static ELispSymbol DEFINE_CHARSET_INTERNAL = new ELispSymbol("define-charset-internal");
    public final static ELispSymbol EIGHT_BIT = new ELispSymbol("eight-bit");
    public final static ELispSymbol EMACS = new ELispSymbol("emacs");
    public final static ELispSymbol INHIBIT_LOAD_CHARSET_MAP = new ELispSymbol("inhibit-load-charset-map");
    public final static ELispSymbol ISO_8859_1 = new ELispSymbol("iso-8859-1");
    public final static ELispSymbol UNICODE = new ELispSymbol("unicode");
    private ELispSymbol[] charsetSymbols() {
        return new ELispSymbol[]{
                ASCII,
                CHARSETP,
                CHARSET_LIST,
                CHARSET_MAP_PATH,
                CURRENT_ISO639_LANGUAGE,
                DEFINE_CHARSET_INTERNAL,
                EIGHT_BIT,
                EMACS,
                INHIBIT_LOAD_CHARSET_MAP,
                ISO_8859_1,
                UNICODE,
        };
    }
    //#endregion charset.c
    //#region fileio.c
    public final static ELispSymbol ACCESS_FILE = new ELispSymbol("access-file");
    public final static ELispSymbol ADD_NAME_TO_FILE = new ELispSymbol("add-name-to-file");
    public final static ELispSymbol AFTER_INSERT_FILE_FUNCTIONS = new ELispSymbol("after-insert-file-functions");
    public final static ELispSymbol AFTER_INSERT_FILE_SET_CODING = new ELispSymbol("after-insert-file-set-coding");
    public final static ELispSymbol AUTO_SAVE = new ELispSymbol("auto-save");
    public final static ELispSymbol AUTO_SAVE_CODING = new ELispSymbol("auto-save-coding");
    public final static ELispSymbol AUTO_SAVE_HOOK = new ELispSymbol("auto-save-hook");
    public final static ELispSymbol AUTO_SAVE_INCLUDE_BIG_DELETIONS = new ELispSymbol("auto-save-include-big-deletions");
    public final static ELispSymbol AUTO_SAVE_LIST_FILE_NAME = new ELispSymbol("auto-save-list-file-name");
    public final static ELispSymbol AUTO_SAVE_VISITED_FILE_NAME = new ELispSymbol("auto-save-visited-file-name");
    public final static ELispSymbol BUFFER_FILE_NAME = new ELispSymbol("buffer-file-name");
    public final static ELispSymbol CAR_LESS_THAN_CAR = new ELispSymbol("car-less-than-car");
    public final static ELispSymbol CERROR = new ELispSymbol(":error");
    public final static ELispSymbol COPY_DIRECTORY = new ELispSymbol("copy-directory");
    public final static ELispSymbol COPY_FILE = new ELispSymbol("copy-file");
    public final static ELispSymbol DEFAULT_FILE_NAME_CODING_SYSTEM = new ELispSymbol("default-file-name-coding-system");
    public final static ELispSymbol DELETE_BY_MOVING_TO_TRASH = new ELispSymbol("delete-by-moving-to-trash");
    public final static ELispSymbol DELETE_DIRECTORY = new ELispSymbol("delete-directory");
    public final static ELispSymbol DELETE_FILE = new ELispSymbol("delete-file");
    public final static ELispSymbol DELETE_FILE_INTERNAL = new ELispSymbol("delete-file-internal");
    public final static ELispSymbol DIRECTORY_FILE_NAME = new ELispSymbol("directory-file-name");
    public final static ELispSymbol EXCL = new ELispSymbol("excl");
    public final static ELispSymbol EXPAND_FILE_NAME = new ELispSymbol("expand-file-name");
    public final static ELispSymbol FILE_ACCESSIBLE_DIRECTORY_P = new ELispSymbol("file-accessible-directory-p");
    public final static ELispSymbol FILE_ACL = new ELispSymbol("file-acl");
    public final static ELispSymbol FILE_ALREADY_EXISTS = new ELispSymbol("file-already-exists");
    public final static ELispSymbol FILE_DATE_ERROR = new ELispSymbol("file-date-error");
    public final static ELispSymbol FILE_DIRECTORY_P = new ELispSymbol("file-directory-p");
    public final static ELispSymbol FILE_ERROR = new ELispSymbol("file-error");
    public final static ELispSymbol FILE_EXECUTABLE_P = new ELispSymbol("file-executable-p");
    public final static ELispSymbol FILE_EXISTS_P = new ELispSymbol("file-exists-p");
    public final static ELispSymbol FILE_MISSING = new ELispSymbol("file-missing");
    public final static ELispSymbol FILE_MODES = new ELispSymbol("file-modes");
    public final static ELispSymbol FILE_NAME_AS_DIRECTORY = new ELispSymbol("file-name-as-directory");
    public final static ELispSymbol FILE_NAME_CASE_INSENSITIVE_P = new ELispSymbol("file-name-case-insensitive-p");
    public final static ELispSymbol FILE_NAME_CODING_SYSTEM = new ELispSymbol("file-name-coding-system");
    public final static ELispSymbol FILE_NAME_DIRECTORY = new ELispSymbol("file-name-directory");
    public final static ELispSymbol FILE_NAME_HANDLER_ALIST = new ELispSymbol("file-name-handler-alist");
    public final static ELispSymbol FILE_NAME_HISTORY = new ELispSymbol("file-name-history");
    public final static ELispSymbol FILE_NAME_NONDIRECTORY = new ELispSymbol("file-name-nondirectory");
    public final static ELispSymbol FILE_NEWER_THAN_FILE_P = new ELispSymbol("file-newer-than-file-p");
    public final static ELispSymbol FILE_NOTIFY_ERROR = new ELispSymbol("file-notify-error");
    public final static ELispSymbol FILE_OFFSET = new ELispSymbol("file-offset");
    public final static ELispSymbol FILE_READABLE_P = new ELispSymbol("file-readable-p");
    public final static ELispSymbol FILE_REGULAR_P = new ELispSymbol("file-regular-p");
    public final static ELispSymbol FILE_SELINUX_CONTEXT = new ELispSymbol("file-selinux-context");
    public final static ELispSymbol FILE_SYMLINK_P = new ELispSymbol("file-symlink-p");
    public final static ELispSymbol FILE_SYSTEM_INFO = new ELispSymbol("file-system-info");
    public final static ELispSymbol FILE_WRITABLE_P = new ELispSymbol("file-writable-p");
    public final static ELispSymbol FORMAT_ANNOTATE_FUNCTION = new ELispSymbol("format-annotate-function");
    public final static ELispSymbol FORMAT_DECODE = new ELispSymbol("format-decode");
    public final static ELispSymbol GET_BUFFER_WINDOW_LIST = new ELispSymbol("get-buffer-window-list");
    public final static ELispSymbol IF_REGULAR = new ELispSymbol("if-regular");
    public final static ELispSymbol INHIBIT_FILE_NAME_HANDLERS = new ELispSymbol("inhibit-file-name-handlers");
    public final static ELispSymbol INSERTED_CHARS = new ELispSymbol("inserted-chars");
    public final static ELispSymbol INSERT_FILE_CONTENTS = new ELispSymbol("insert-file-contents");
    public final static ELispSymbol MAKE_DIRECTORY = new ELispSymbol("make-directory");
    public final static ELispSymbol MAKE_DIRECTORY_INTERNAL = new ELispSymbol("make-directory-internal");
    public final static ELispSymbol MAKE_SYMBOLIC_LINK = new ELispSymbol("make-symbolic-link");
    public final static ELispSymbol OPERATIONS = new ELispSymbol("operations");
    public final static ELispSymbol PERMISSION_DENIED = new ELispSymbol("permission-denied");
    public final static ELispSymbol REMOTE_FILE_ERROR = new ELispSymbol("remote-file-error");
    public final static ELispSymbol RENAME_FILE = new ELispSymbol("rename-file");
    public final static ELispSymbol SET_AUTO_CODING_FUNCTION = new ELispSymbol("set-auto-coding-function");
    public final static ELispSymbol SET_FILE_ACL = new ELispSymbol("set-file-acl");
    public final static ELispSymbol SET_FILE_MODES = new ELispSymbol("set-file-modes");
    public final static ELispSymbol SET_FILE_SELINUX_CONTEXT = new ELispSymbol("set-file-selinux-context");
    public final static ELispSymbol SET_FILE_TIMES = new ELispSymbol("set-file-times");
    public final static ELispSymbol SET_VISITED_FILE_MODTIME = new ELispSymbol("set-visited-file-modtime");
    public final static ELispSymbol STDERR = new ELispSymbol("stderr");
    public final static ELispSymbol STDIN = new ELispSymbol("stdin");
    public final static ELispSymbol STDOUT = new ELispSymbol("stdout");
    public final static ELispSymbol SUBSTITUTE_ENV_IN_FILE_NAME = new ELispSymbol("substitute-env-in-file-name");
    public final static ELispSymbol SUBSTITUTE_IN_FILE_NAME = new ELispSymbol("substitute-in-file-name");
    public final static ELispSymbol UNHANDLED_FILE_NAME_DIRECTORY = new ELispSymbol("unhandled-file-name-directory");
    public final static ELispSymbol VERIFY_VISITED_FILE_MODTIME = new ELispSymbol("verify-visited-file-modtime");
    public final static ELispSymbol WRITE_REGION = new ELispSymbol("write-region");
    public final static ELispSymbol WRITE_REGION_ANNOTATE_FUNCTIONS = new ELispSymbol("write-region-annotate-functions");
    public final static ELispSymbol WRITE_REGION_ANNOTATIONS_SO_FAR = new ELispSymbol("write-region-annotations-so-far");
    public final static ELispSymbol WRITE_REGION_INHIBIT_FSYNC = new ELispSymbol("write-region-inhibit-fsync");
    public final static ELispSymbol WRITE_REGION_POST_ANNOTATION_FUNCTION = new ELispSymbol("write-region-post-annotation-function");
    private ELispSymbol[] fileioSymbols() {
        return new ELispSymbol[]{
                ACCESS_FILE,
                ADD_NAME_TO_FILE,
                AFTER_INSERT_FILE_FUNCTIONS,
                AFTER_INSERT_FILE_SET_CODING,
                AUTO_SAVE,
                AUTO_SAVE_CODING,
                AUTO_SAVE_HOOK,
                AUTO_SAVE_INCLUDE_BIG_DELETIONS,
                AUTO_SAVE_LIST_FILE_NAME,
                AUTO_SAVE_VISITED_FILE_NAME,
                BUFFER_FILE_NAME,
                CAR_LESS_THAN_CAR,
                CERROR,
                COPY_DIRECTORY,
                COPY_FILE,
                DEFAULT_FILE_NAME_CODING_SYSTEM,
                DELETE_BY_MOVING_TO_TRASH,
                DELETE_DIRECTORY,
                DELETE_FILE,
                DELETE_FILE_INTERNAL,
                DIRECTORY_FILE_NAME,
                EXCL,
                EXPAND_FILE_NAME,
                FILE_ACCESSIBLE_DIRECTORY_P,
                FILE_ACL,
                FILE_ALREADY_EXISTS,
                FILE_DATE_ERROR,
                FILE_DIRECTORY_P,
                FILE_ERROR,
                FILE_EXECUTABLE_P,
                FILE_EXISTS_P,
                FILE_MISSING,
                FILE_MODES,
                FILE_NAME_AS_DIRECTORY,
                FILE_NAME_CASE_INSENSITIVE_P,
                FILE_NAME_CODING_SYSTEM,
                FILE_NAME_DIRECTORY,
                FILE_NAME_HANDLER_ALIST,
                FILE_NAME_HISTORY,
                FILE_NAME_NONDIRECTORY,
                FILE_NEWER_THAN_FILE_P,
                FILE_NOTIFY_ERROR,
                FILE_OFFSET,
                FILE_READABLE_P,
                FILE_REGULAR_P,
                FILE_SELINUX_CONTEXT,
                FILE_SYMLINK_P,
                FILE_SYSTEM_INFO,
                FILE_WRITABLE_P,
                FORMAT_ANNOTATE_FUNCTION,
                FORMAT_DECODE,
                GET_BUFFER_WINDOW_LIST,
                IF_REGULAR,
                INHIBIT_FILE_NAME_HANDLERS,
                INHIBIT_FILE_NAME_OPERATION,
                INSERTED_CHARS,
                INSERT_FILE_CONTENTS,
                MAKE_DIRECTORY,
                MAKE_DIRECTORY_INTERNAL,
                MAKE_SYMBOLIC_LINK,
                OPERATIONS,
                PERMISSION_DENIED,
                REMOTE_FILE_ERROR,
                RENAME_FILE,
                SET_AUTO_CODING_FUNCTION,
                SET_FILE_ACL,
                SET_FILE_MODES,
                SET_FILE_SELINUX_CONTEXT,
                SET_FILE_TIMES,
                SET_VISITED_FILE_MODTIME,
                STDERR,
                STDIN,
                STDOUT,
                SUBSTITUTE_ENV_IN_FILE_NAME,
                SUBSTITUTE_IN_FILE_NAME,
                UNHANDLED_FILE_NAME_DIRECTORY,
                VERIFY_VISITED_FILE_MODTIME,
                WRITE_REGION,
                WRITE_REGION_ANNOTATE_FUNCTIONS,
                WRITE_REGION_ANNOTATIONS_SO_FAR,
                WRITE_REGION_INHIBIT_FSYNC,
                WRITE_REGION_POST_ANNOTATION_FUNCTION,
        };
    }
    //#endregion fileio.c
    //#region editfns.c
    public final static ELispSymbol BINARY_AS_UNSIGNED = new ELispSymbol("binary-as-unsigned");
    public final static ELispSymbol BOUNDARY = new ELispSymbol("boundary");
    public final static ELispSymbol BUFFER_ACCESS_FONTIFIED_PROPERTY = new ELispSymbol("buffer-access-fontified-property");
    public final static ELispSymbol BUFFER_ACCESS_FONTIFY_FUNCTIONS = new ELispSymbol("buffer-access-fontify-functions");
    public final static ELispSymbol FIELD = new ELispSymbol("field");
    public final static ELispSymbol INHIBIT_FIELD_TEXT_MOTION = new ELispSymbol("inhibit-field-text-motion");
    public final static ELispSymbol OPERATING_SYSTEM_RELEASE = new ELispSymbol("operating-system-release");
    public final static ELispSymbol OUTERMOST_RESTRICTION = new ELispSymbol("outermost-restriction");
    public final static ELispSymbol PROPERTIZE = new ELispSymbol("propertize");
    public final static ELispSymbol SYSTEM_NAME = new ELispSymbol("system-name");
    public final static ELispSymbol USER_FULL_NAME = new ELispSymbol("user-full-name");
    public final static ELispSymbol USER_LOGIN_NAME = new ELispSymbol("user-login-name");
    public final static ELispSymbol USER_REAL_LOGIN_NAME = new ELispSymbol("user-real-login-name");
    public final static ELispSymbol WALL = new ELispSymbol("wall");
    private ELispSymbol[] editfnsSymbols() {
        return new ELispSymbol[]{
                BINARY_AS_UNSIGNED,
                BOUNDARY,
                BUFFER_ACCESS_FONTIFIED_PROPERTY,
                BUFFER_ACCESS_FONTIFY_FUNCTIONS,
                FIELD,
                INHIBIT_FIELD_TEXT_MOTION,
                OPERATING_SYSTEM_RELEASE,
                OUTERMOST_RESTRICTION,
                PROPERTIZE,
                SYSTEM_NAME,
                USER_FULL_NAME,
                USER_LOGIN_NAME,
                USER_REAL_LOGIN_NAME,
                WALL,
        };
    }
    //#endregion editfns.c
    //#region emacs.c
    public final static ELispSymbol AFTER_INIT_TIME = new ELispSymbol("after-init-time");
    public final static ELispSymbol BEFORE_INIT_TIME = new ELispSymbol("before-init-time");
    public final static ELispSymbol COMMAND_LINE_ARGS = new ELispSymbol("command-line-args");
    public final static ELispSymbol COMMAND_LINE_PROCESSED = new ELispSymbol("command-line-processed");
    public final static ELispSymbol DUMP_MODE = new ELispSymbol("dump-mode");
    public final static ELispSymbol DYNAMIC_LIBRARY_ALIST = new ELispSymbol("dynamic-library-alist");
    public final static ELispSymbol EMACS_COPYRIGHT = new ELispSymbol("emacs-copyright");
    public final static ELispSymbol EMACS_VERSION = new ELispSymbol("emacs-version");
    public final static ELispSymbol FILE_TRUENAME = new ELispSymbol("file-truename");
    public final static ELispSymbol INHIBIT_X_RESOURCES = new ELispSymbol("inhibit-x-resources");
    public final static ELispSymbol INSTALLATION_DIRECTORY = new ELispSymbol("installation-directory");
    public final static ELispSymbol INVOCATION_DIRECTORY = new ELispSymbol("invocation-directory");
    public final static ELispSymbol INVOCATION_NAME = new ELispSymbol("invocation-name");
    public final static ELispSymbol KILL_EMACS = new ELispSymbol("kill-emacs");
    public final static ELispSymbol KILL_EMACS_HOOK = new ELispSymbol("kill-emacs-hook");
    public final static ELispSymbol NONINTERACTIVE = new ELispSymbol("noninteractive");
    public final static ELispSymbol PATH_SEPARATOR = new ELispSymbol("path-separator");
    public final static ELispSymbol REPORT_EMACS_BUG_ADDRESS = new ELispSymbol("report-emacs-bug-address");
    public final static ELispSymbol RISKY_LOCAL_VARIABLE = new ELispSymbol("risky-local-variable");
    public final static ELispSymbol RUN_HOOK_QUERY_ERROR_WITH_TIMEOUT = new ELispSymbol("run-hook-query-error-with-timeout");
    public final static ELispSymbol SAFE_MAGIC = new ELispSymbol("safe-magic");
    public final static ELispSymbol SYSTEM_CONFIGURATION = new ELispSymbol("system-configuration");
    public final static ELispSymbol SYSTEM_CONFIGURATION_FEATURES = new ELispSymbol("system-configuration-features");
    public final static ELispSymbol SYSTEM_CONFIGURATION_OPTIONS = new ELispSymbol("system-configuration-options");
    public final static ELispSymbol SYSTEM_MESSAGES_LOCALE = new ELispSymbol("system-messages-locale");
    public final static ELispSymbol SYSTEM_TIME_LOCALE = new ELispSymbol("system-time-locale");
    public final static ELispSymbol SYSTEM_TYPE = new ELispSymbol("system-type");
    private ELispSymbol[] emacsSymbols() {
        return new ELispSymbol[]{
                AFTER_INIT_TIME,
                BEFORE_INIT_TIME,
                COMMAND_LINE_ARGS,
                COMMAND_LINE_PROCESSED,
                DUMP_MODE,
                DYNAMIC_LIBRARY_ALIST,
                EMACS_COPYRIGHT,
                EMACS_VERSION,
                FILE_NAME_HANDLER_ALIST,
                FILE_TRUENAME,
                INHIBIT_X_RESOURCES,
                INSTALLATION_DIRECTORY,
                INVOCATION_DIRECTORY,
                INVOCATION_NAME,
                KILL_EMACS,
                KILL_EMACS_HOOK,
                NONINTERACTIVE,
                PATH_SEPARATOR,
                REPORT_EMACS_BUG_ADDRESS,
                RISKY_LOCAL_VARIABLE,
                RUN_HOOK_QUERY_ERROR_WITH_TIMEOUT,
                SAFE_MAGIC,
                SYSTEM_CONFIGURATION,
                SYSTEM_CONFIGURATION_FEATURES,
                SYSTEM_CONFIGURATION_OPTIONS,
                SYSTEM_MESSAGES_LOCALE,
                SYSTEM_TIME_LOCALE,
                SYSTEM_TYPE,
        };
    }
    //#endregion emacs.c
    //#region search.c
    public final static ELispSymbol INHIBIT_CHANGING_MATCH_DATA = new ELispSymbol("inhibit-changing-match-data");
    public final static ELispSymbol INVALID_REGEXP = new ELispSymbol("invalid-regexp");
    public final static ELispSymbol SEARCH_FAILED = new ELispSymbol("search-failed");
    public final static ELispSymbol SEARCH_SPACES_REGEXP = new ELispSymbol("search-spaces-regexp");
    public final static ELispSymbol USER_SEARCH_FAILED = new ELispSymbol("user-search-failed");
    private ELispSymbol[] searchSymbols() {
        return new ELispSymbol[]{
                INHIBIT_CHANGING_MATCH_DATA,
                INVALID_REGEXP,
                SEARCH_FAILED,
                SEARCH_SPACES_REGEXP,
                USER_SEARCH_FAILED,
        };
    }
    //#endregion search.c
    //#region buffer.c
    public final static ELispSymbol ABBREV_MODE = new ELispSymbol("abbrev-mode");
    public final static ELispSymbol AFTER_CHANGE_FUNCTIONS = new ELispSymbol("after-change-functions");
    public final static ELispSymbol AFTER_STRING = new ELispSymbol("after-string");
    public final static ELispSymbol AUTOSAVED = new ELispSymbol("autosaved");
    public final static ELispSymbol AUTO_FILL_FUNCTION = new ELispSymbol("auto-fill-function");
    public final static ELispSymbol BEFORE_CHANGE_FUNCTIONS = new ELispSymbol("before-change-functions");
    public final static ELispSymbol BEFORE_STRING = new ELispSymbol("before-string");
    public final static ELispSymbol BIDI_DISPLAY_REORDERING = new ELispSymbol("bidi-display-reordering");
    public final static ELispSymbol BIDI_PARAGRAPH_DIRECTION = new ELispSymbol("bidi-paragraph-direction");
    public final static ELispSymbol BIDI_PARAGRAPH_SEPARATE_RE = new ELispSymbol("bidi-paragraph-separate-re");
    public final static ELispSymbol BIDI_PARAGRAPH_START_RE = new ELispSymbol("bidi-paragraph-start-re");
    public final static ELispSymbol BUFFER_AUTO_SAVE_FILE_FORMAT = new ELispSymbol("buffer-auto-save-file-format");
    public final static ELispSymbol BUFFER_AUTO_SAVE_FILE_NAME = new ELispSymbol("buffer-auto-save-file-name");
    public final static ELispSymbol BUFFER_BACKED_UP = new ELispSymbol("buffer-backed-up");
    public final static ELispSymbol BUFFER_DISPLAY_COUNT = new ELispSymbol("buffer-display-count");
    public final static ELispSymbol BUFFER_DISPLAY_TABLE = new ELispSymbol("buffer-display-table");
    public final static ELispSymbol BUFFER_DISPLAY_TIME = new ELispSymbol("buffer-display-time");
    public final static ELispSymbol BUFFER_FILE_FORMAT = new ELispSymbol("buffer-file-format");
    public final static ELispSymbol BUFFER_FILE_NUMBER = new ELispSymbol("buffer-file-number");
    public final static ELispSymbol BUFFER_FILE_TRUENAME = new ELispSymbol("buffer-file-truename");
    public final static ELispSymbol BUFFER_INVISIBILITY_SPEC = new ELispSymbol("buffer-invisibility-spec");
    public final static ELispSymbol BUFFER_LIST_UPDATE_HOOK = new ELispSymbol("buffer-list-update-hook");
    public final static ELispSymbol BUFFER_SAVED_SIZE = new ELispSymbol("buffer-saved-size");
    public final static ELispSymbol BUFFER_SAVE_WITHOUT_QUERY = new ELispSymbol("buffer-save-without-query");
    public final static ELispSymbol BUFFER_STALE_FUNCTION = new ELispSymbol("buffer-stale-function");
    public final static ELispSymbol BUFFER_UNDO_LIST = new ELispSymbol("buffer-undo-list");
    public final static ELispSymbol CACHE_LONG_SCANS = new ELispSymbol("cache-long-scans");
    public final static ELispSymbol CASE_FOLD_SEARCH = new ELispSymbol("case-fold-search");
    public final static ELispSymbol CHANGE_MAJOR_MODE_HOOK = new ELispSymbol("change-major-mode-hook");
    public final static ELispSymbol CHOICE = new ELispSymbol("choice");
    public final static ELispSymbol CLONE_INDIRECT_BUFFER_HOOK = new ELispSymbol("clone-indirect-buffer-hook");
    public final static ELispSymbol CTL_ARROW = new ELispSymbol("ctl-arrow");
    public final static ELispSymbol CURSOR_IN_NON_SELECTED_WINDOWS = new ELispSymbol("cursor-in-non-selected-windows");
    public final static ELispSymbol DEFAULT_DIRECTORY = new ELispSymbol("default-directory");
    public final static ELispSymbol DELETE_AUTO_SAVE_FILES = new ELispSymbol("delete-auto-save-files");
    public final static ELispSymbol DELETE_AUTO_SAVE_FILE_IF_NECESSARY = new ELispSymbol("delete-auto-save-file-if-necessary");
    public final static ELispSymbol ENABLE_MULTIBYTE_CHARACTERS = new ELispSymbol("enable-multibyte-characters");
    public final static ELispSymbol EVAPORATE = new ELispSymbol("evaporate");
    public final static ELispSymbol FILL_COLUMN = new ELispSymbol("fill-column");
    public final static ELispSymbol FIRST_CHANGE_HOOK = new ELispSymbol("first-change-hook");
    public final static ELispSymbol FRACTION = new ELispSymbol("fraction");
    public final static ELispSymbol FRINGES_OUTSIDE_MARGINS = new ELispSymbol("fringes-outside-margins");
    public final static ELispSymbol FRINGE_CURSOR_ALIST = new ELispSymbol("fringe-cursor-alist");
    public final static ELispSymbol FRINGE_INDICATOR_ALIST = new ELispSymbol("fringe-indicator-alist");
    public final static ELispSymbol FUNDAMENTAL_MODE = new ELispSymbol("fundamental-mode");
    public final static ELispSymbol GET_FILE_BUFFER = new ELispSymbol("get-file-buffer");
    public final static ELispSymbol GET_SCRATCH_BUFFER_CREATE = new ELispSymbol("get-scratch-buffer-create");
    public final static ELispSymbol HORIZONTAL_SCROLL_BAR = new ELispSymbol("horizontal-scroll-bar");
    public final static ELispSymbol INDICATE_BUFFER_BOUNDARIES = new ELispSymbol("indicate-buffer-boundaries");
    public final static ELispSymbol INDICATE_EMPTY_LINES = new ELispSymbol("indicate-empty-lines");
    public final static ELispSymbol INHIBIT_READ_ONLY = new ELispSymbol("inhibit-read-only");
    public final static ELispSymbol INITIAL_MAJOR_MODE = new ELispSymbol("initial-major-mode");
    public final static ELispSymbol INSERT_BEHIND_HOOKS = new ELispSymbol("insert-behind-hooks");
    public final static ELispSymbol INSERT_IN_FRONT_HOOKS = new ELispSymbol("insert-in-front-hooks");
    public final static ELispSymbol KILL_BUFFER_DELETE_AUTO_SAVE_FILES = new ELispSymbol("kill-buffer-delete-auto-save-files");
    public final static ELispSymbol KILL_BUFFER_HOOK = new ELispSymbol("kill-buffer-hook");
    public final static ELispSymbol KILL_BUFFER_QUERY_FUNCTIONS = new ELispSymbol("kill-buffer-query-functions");
    public final static ELispSymbol KILL_BUFFER__POSSIBLY_SAVE = new ELispSymbol("kill-buffer--possibly-save");
    public final static ELispSymbol LARGE_HSCROLL_THRESHOLD = new ELispSymbol("large-hscroll-threshold");
    public final static ELispSymbol LEFT = new ELispSymbol("left");
    public final static ELispSymbol LEFT_FRINGE_WIDTH = new ELispSymbol("left-fringe-width");
    public final static ELispSymbol LEFT_MARGIN_WIDTH = new ELispSymbol("left-margin-width");
    public final static ELispSymbol LOCAL_ABBREV_TABLE = new ELispSymbol("local-abbrev-table");
    public final static ELispSymbol LOCAL_MINOR_MODES = new ELispSymbol("local-minor-modes");
    public final static ELispSymbol LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT = new ELispSymbol("long-line-optimizations-bol-search-limit");
    public final static ELispSymbol LONG_LINE_OPTIMIZATIONS_REGION_SIZE = new ELispSymbol("long-line-optimizations-region-size");
    public final static ELispSymbol LONG_LINE_THRESHOLD = new ELispSymbol("long-line-threshold");
    public final static ELispSymbol MAJOR_MODE = new ELispSymbol("major-mode");
    public final static ELispSymbol MARK_ACTIVE = new ELispSymbol("mark-active");
    public final static ELispSymbol MODE_CLASS = new ELispSymbol("mode-class");
    public final static ELispSymbol MODE_NAME = new ELispSymbol("mode-name");
    public final static ELispSymbol MODIFICATION_HOOKS = new ELispSymbol("modification-hooks");
    public final static ELispSymbol OVERLAYP = new ELispSymbol("overlayp");
    public final static ELispSymbol OVERWRITE_MODE = new ELispSymbol("overwrite-mode");
    public final static ELispSymbol PERMANENT_LOCAL = new ELispSymbol("permanent-local");
    public final static ELispSymbol PERMANENT_LOCAL_HOOK = new ELispSymbol("permanent-local-hook");
    public final static ELispSymbol POINT_BEFORE_SCROLL = new ELispSymbol("point-before-scroll");
    public final static ELispSymbol PRIORITY = new ELispSymbol("priority");
    public final static ELispSymbol PROTECTED_FIELD = new ELispSymbol("protected-field");
    public final static ELispSymbol RANGE = new ELispSymbol("range");
    public final static ELispSymbol RENAME_AUTO_SAVE_FILE = new ELispSymbol("rename-auto-save-file");
    public final static ELispSymbol RIGHT = new ELispSymbol("right");
    public final static ELispSymbol RIGHT_FRINGE_WIDTH = new ELispSymbol("right-fringe-width");
    public final static ELispSymbol RIGHT_MARGIN_WIDTH = new ELispSymbol("right-margin-width");
    public final static ELispSymbol SCROLL_DOWN_AGGRESSIVELY = new ELispSymbol("scroll-down-aggressively");
    public final static ELispSymbol SCROLL_UP_AGGRESSIVELY = new ELispSymbol("scroll-up-aggressively");
    public final static ELispSymbol SELECTIVE_DISPLAY = new ELispSymbol("selective-display");
    public final static ELispSymbol SELECTIVE_DISPLAY_ELLIPSES = new ELispSymbol("selective-display-ellipses");
    public final static ELispSymbol SET_BUFFER_MULTIBYTE = new ELispSymbol("set-buffer-multibyte");
    public final static ELispSymbol TAB_WIDTH = new ELispSymbol("tab-width");
    public final static ELispSymbol TEXT_CONVERSION_STYLE = new ELispSymbol("text-conversion-style");
    public final static ELispSymbol TRANSIENT_MARK_MODE = new ELispSymbol("transient-mark-mode");
    public final static ELispSymbol TRUNCATE_LINES = new ELispSymbol("truncate-lines");
    public final static ELispSymbol UNIQUIFY__RENAME_BUFFER_ADVICE = new ELispSymbol("uniquify--rename-buffer-advice");
    public final static ELispSymbol VERTICAL_SCROLL_BAR = new ELispSymbol("vertical-scroll-bar");
    public final static ELispSymbol WORD_WRAP = new ELispSymbol("word-wrap");
    private ELispSymbol[] bufferSymbols() {
        return new ELispSymbol[]{
                ABBREV_MODE,
                AFTER_CHANGE_FUNCTIONS,
                AFTER_STRING,
                AUTOSAVED,
                AUTO_FILL_FUNCTION,
                BEFORE_CHANGE_FUNCTIONS,
                BEFORE_STRING,
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
                BUFFER_FILE_CODING_SYSTEM,
                BUFFER_FILE_FORMAT,
                BUFFER_FILE_NAME,
                BUFFER_FILE_NUMBER,
                BUFFER_FILE_TRUENAME,
                BUFFER_INVISIBILITY_SPEC,
                BUFFER_LIST_UPDATE_HOOK,
                BUFFER_READ_ONLY,
                BUFFER_SAVED_SIZE,
                BUFFER_SAVE_WITHOUT_QUERY,
                BUFFER_STALE_FUNCTION,
                BUFFER_UNDO_LIST,
                CACHE_LONG_SCANS,
                CASE_FOLD_SEARCH,
                CHANGE_MAJOR_MODE_HOOK,
                CHOICE,
                CLONE_INDIRECT_BUFFER_HOOK,
                CTL_ARROW,
                CURSOR_IN_NON_SELECTED_WINDOWS,
                CURSOR_TYPE,
                DEFAULT_DIRECTORY,
                DELETE_AUTO_SAVE_FILES,
                DELETE_AUTO_SAVE_FILE_IF_NECESSARY,
                ENABLE_MULTIBYTE_CHARACTERS,
                EVAPORATE,
                FILL_COLUMN,
                FIRST_CHANGE_HOOK,
                FRACTION,
                FRINGES_OUTSIDE_MARGINS,
                FRINGE_CURSOR_ALIST,
                FRINGE_INDICATOR_ALIST,
                FUNDAMENTAL_MODE,
                GET_FILE_BUFFER,
                GET_SCRATCH_BUFFER_CREATE,
                HEADER_LINE_FORMAT,
                HORIZONTAL_SCROLL_BAR,
                INDICATE_BUFFER_BOUNDARIES,
                INDICATE_EMPTY_LINES,
                INHIBIT_READ_ONLY,
                INITIAL_MAJOR_MODE,
                INSERT_BEHIND_HOOKS,
                INSERT_IN_FRONT_HOOKS,
                KILL_BUFFER_DELETE_AUTO_SAVE_FILES,
                KILL_BUFFER_HOOK,
                KILL_BUFFER_QUERY_FUNCTIONS,
                KILL_BUFFER__POSSIBLY_SAVE,
                LARGE_HSCROLL_THRESHOLD,
                LEFT,
                LEFT_FRINGE_WIDTH,
                LEFT_MARGIN,
                LEFT_MARGIN_WIDTH,
                LINE_SPACING,
                LOCAL_ABBREV_TABLE,
                LOCAL_MINOR_MODES,
                LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT,
                LONG_LINE_OPTIMIZATIONS_REGION_SIZE,
                LONG_LINE_THRESHOLD,
                MAJOR_MODE,
                MARK_ACTIVE,
                MODE_CLASS,
                MODE_LINE_FORMAT,
                MODE_NAME,
                MODIFICATION_HOOKS,
                OVERLAYP,
                OVERWRITE_MODE,
                PERMANENT_LOCAL,
                PERMANENT_LOCAL_HOOK,
                POINT_BEFORE_SCROLL,
                PRIORITY,
                PROTECTED_FIELD,
                RANGE,
                RENAME_AUTO_SAVE_FILE,
                RIGHT,
                RIGHT_FRINGE_WIDTH,
                RIGHT_MARGIN_WIDTH,
                SCROLL_BAR_HEIGHT,
                SCROLL_BAR_WIDTH,
                SCROLL_DOWN_AGGRESSIVELY,
                SCROLL_UP_AGGRESSIVELY,
                SELECTIVE_DISPLAY,
                SELECTIVE_DISPLAY_ELLIPSES,
                SET_BUFFER_MULTIBYTE,
                TAB_LINE_FORMAT,
                TAB_WIDTH,
                TEXT_CONVERSION_STYLE,
                TRANSIENT_MARK_MODE,
                TRUNCATE_LINES,
                UNIQUIFY__RENAME_BUFFER_ADVICE,
                VERTICAL_SCROLL_BAR,
                WORD_WRAP,
        };
    }
    //#endregion buffer.c
    //#region keymap.c
    public final static ELispSymbol CADVERTISED_BINDING = new ELispSymbol(":advertised-binding");
    public final static ELispSymbol DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES = new ELispSymbol("describe-bindings-check-shadowing-in-ranges");
    public final static ELispSymbol EMULATION_MODE_MAP_ALISTS = new ELispSymbol("emulation-mode-map-alists");
    public final static ELispSymbol FONT_LOCK_FACE = new ELispSymbol("font-lock-face");
    public final static ELispSymbol HELP_KEY_BINDING = new ELispSymbol("help-key-binding");
    public final static ELispSymbol HELP__DESCRIBE_MAP_TREE = new ELispSymbol("help--describe-map-tree");
    public final static ELispSymbol IGNORE_SELF_INSERT = new ELispSymbol("ignore-self-insert");
    public final static ELispSymbol KEYMAP = new ELispSymbol("keymap");
    public final static ELispSymbol KEYMAPP = new ELispSymbol("keymapp");
    public final static ELispSymbol KEYMAP_CANONICALIZE = new ELispSymbol("keymap-canonicalize");
    public final static ELispSymbol KEY_PARSE = new ELispSymbol("key-parse");
    public final static ELispSymbol KEY_VALID_P = new ELispSymbol("key-valid-p");
    public final static ELispSymbol MAP_KEYMAP_SORTED = new ELispSymbol("map-keymap-sorted");
    public final static ELispSymbol MENU_BAR = new ELispSymbol("menu-bar");
    public final static ELispSymbol MENU_ITEM = new ELispSymbol("menu-item");
    public final static ELispSymbol MINIBUFFER_LOCAL_MAP = new ELispSymbol("minibuffer-local-map");
    public final static ELispSymbol MINOR_MODE_MAP_ALIST = new ELispSymbol("minor-mode-map-alist");
    public final static ELispSymbol MINOR_MODE_OVERRIDING_MAP_ALIST = new ELispSymbol("minor-mode-overriding-map-alist");
    public final static ELispSymbol MODE_LINE = new ELispSymbol("mode-line");
    public final static ELispSymbol NON_ASCII = new ELispSymbol("non-ascii");
    public final static ELispSymbol NON_KEY_EVENT = new ELispSymbol("non-key-event");
    public final static ELispSymbol PRINC = new ELispSymbol("princ");
    public final static ELispSymbol REMAP = new ELispSymbol("remap");
    public final static ELispSymbol SELF_INSERT_COMMAND = new ELispSymbol("self-insert-command");
    public final static ELispSymbol SUPPRESS_KEYMAP = new ELispSymbol("suppress-keymap");
    public final static ELispSymbol WHERE_IS_PREFERRED_MODIFIER = new ELispSymbol("where-is-preferred-modifier");
    private ELispSymbol[] keymapSymbols() {
        return new ELispSymbol[]{
                CADVERTISED_BINDING,
                DESCRIBE_BINDINGS_CHECK_SHADOWING_IN_RANGES,
                EMULATION_MODE_MAP_ALISTS,
                FONT_LOCK_FACE,
                HELP_KEY_BINDING,
                HELP__DESCRIBE_MAP_TREE,
                IGNORE_SELF_INSERT,
                KEYMAP,
                KEYMAPP,
                KEYMAP_CANONICALIZE,
                KEY_PARSE,
                KEY_VALID_P,
                MAP_KEYMAP_SORTED,
                MENU_BAR,
                MENU_ITEM,
                MINIBUFFER_LOCAL_MAP,
                MINOR_MODE_MAP_ALIST,
                MINOR_MODE_OVERRIDING_MAP_ALIST,
                MODE_LINE,
                NON_ASCII,
                NON_KEY_EVENT,
                PRINC,
                REMAP,
                SELF_INSERT_COMMAND,
                SUPPRESS_KEYMAP,
                WHERE_IS_PREFERRED_MODIFIER,
        };
    }
    //#endregion keymap.c
    //#region print.c
    public final static ELispSymbol EXTERNAL_DEBUGGING_OUTPUT = new ELispSymbol("external-debugging-output");
    public final static ELispSymbol FLOAT_OUTPUT_FORMAT = new ELispSymbol("float-output-format");
    public final static ELispSymbol PRINT_CHARSET_TEXT_PROPERTY = new ELispSymbol("print-charset-text-property");
    public final static ELispSymbol PRINT_CIRCLE = new ELispSymbol("print-circle");
    public final static ELispSymbol PRINT_CONTINUOUS_NUMBERING = new ELispSymbol("print-continuous-numbering");
    public final static ELispSymbol PRINT_ESCAPE_CONTROL_CHARACTERS = new ELispSymbol("print-escape-control-characters");
    public final static ELispSymbol PRINT_ESCAPE_MULTIBYTE = new ELispSymbol("print-escape-multibyte");
    public final static ELispSymbol PRINT_ESCAPE_NEWLINES = new ELispSymbol("print-escape-newlines");
    public final static ELispSymbol PRINT_ESCAPE_NONASCII = new ELispSymbol("print-escape-nonascii");
    public final static ELispSymbol PRINT_GENSYM = new ELispSymbol("print-gensym");
    public final static ELispSymbol PRINT_INTEGERS_AS_CHARACTERS = new ELispSymbol("print-integers-as-characters");
    public final static ELispSymbol PRINT_LENGTH = new ELispSymbol("print-length");
    public final static ELispSymbol PRINT_LEVEL = new ELispSymbol("print-level");
    public final static ELispSymbol PRINT_NUMBER_TABLE = new ELispSymbol("print-number-table");
    public final static ELispSymbol PRINT_QUOTED = new ELispSymbol("print-quoted");
    public final static ELispSymbol PRINT_SYMBOLS_BARE = new ELispSymbol("print-symbols-bare");
    public final static ELispSymbol PRINT_UNREADABLE_FUNCTION = new ELispSymbol("print-unreadable-function");
    public final static ELispSymbol PRINT__UNREADABLE_CALLBACK_BUFFER = new ELispSymbol("print--unreadable-callback-buffer");
    public final static ELispSymbol STANDARD_OUTPUT = new ELispSymbol("standard-output");
    public final static ELispSymbol TEMP_BUFFER_SETUP_HOOK = new ELispSymbol("temp-buffer-setup-hook");
    private ELispSymbol[] printSymbols() {
        return new ELispSymbol[]{
                EXTERNAL_DEBUGGING_OUTPUT,
                FLOAT_OUTPUT_FORMAT,
                PRINT_CHARSET_TEXT_PROPERTY,
                PRINT_CIRCLE,
                PRINT_CONTINUOUS_NUMBERING,
                PRINT_ESCAPE_CONTROL_CHARACTERS,
                PRINT_ESCAPE_MULTIBYTE,
                PRINT_ESCAPE_NEWLINES,
                PRINT_ESCAPE_NONASCII,
                PRINT_GENSYM,
                PRINT_INTEGERS_AS_CHARACTERS,
                PRINT_LENGTH,
                PRINT_LEVEL,
                PRINT_NUMBER_TABLE,
                PRINT_QUOTED,
                PRINT_SYMBOLS_BARE,
                PRINT_UNREADABLE_FUNCTION,
                PRINT__UNREADABLE_CALLBACK_BUFFER,
                STANDARD_OUTPUT,
                TEMP_BUFFER_SETUP_HOOK,
        };
    }
    //#endregion print.c
    //#region xfaces.c
    public final static ELispSymbol BACKGROUND_COLOR = new ELispSymbol("background-color");
    public final static ELispSymbol BITMAP_SPEC_P = new ELispSymbol("bitmap-spec-p");
    public final static ELispSymbol BLACK = new ELispSymbol("black");
    public final static ELispSymbol BOLD = new ELispSymbol("bold");
    public final static ELispSymbol BOOK = new ELispSymbol("book");
    public final static ELispSymbol BORDER = new ELispSymbol("border");
    public final static ELispSymbol CBACKGROUND = new ELispSymbol(":background");
    public final static ELispSymbol CBOLD = new ELispSymbol(":bold");
    public final static ELispSymbol CBOX = new ELispSymbol(":box");
    public final static ELispSymbol CCOLOR = new ELispSymbol(":color");
    public final static ELispSymbol CDISTANT_FOREGROUND = new ELispSymbol(":distant-foreground");
    public final static ELispSymbol CEXTEND = new ELispSymbol(":extend");
    public final static ELispSymbol CFAMILY = new ELispSymbol(":family");
    public final static ELispSymbol CFILTERED = new ELispSymbol(":filtered");
    public final static ELispSymbol CFONT = new ELispSymbol(":font");
    public final static ELispSymbol CFONTSET = new ELispSymbol(":fontset");
    public final static ELispSymbol CFOREGROUND = new ELispSymbol(":foreground");
    public final static ELispSymbol CHEIGHT = new ELispSymbol(":height");
    public final static ELispSymbol CHILD_FRAME_BORDER = new ELispSymbol("child-frame-border");
    public final static ELispSymbol CIGNORE_DEFFACE = new ELispSymbol(":ignore-defface");
    public final static ELispSymbol CINHERIT = new ELispSymbol(":inherit");
    public final static ELispSymbol CINVERSE_VIDEO = new ELispSymbol(":inverse-video");
    public final static ELispSymbol CITALIC = new ELispSymbol(":italic");
    public final static ELispSymbol CLINE_WIDTH = new ELispSymbol(":line-width");
    public final static ELispSymbol COVERLINE = new ELispSymbol(":overline");
    public final static ELispSymbol CPOSITION = new ELispSymbol(":position");
    public final static ELispSymbol CSLANT = new ELispSymbol(":slant");
    public final static ELispSymbol CSTIPPLE = new ELispSymbol(":stipple");
    public final static ELispSymbol CSTRIKE_THROUGH = new ELispSymbol(":strike-through");
    public final static ELispSymbol CSTYLE = new ELispSymbol(":style");
    public final static ELispSymbol CUNDERLINE = new ELispSymbol(":underline");
    public final static ELispSymbol CURSOR = new ELispSymbol("cursor");
    public final static ELispSymbol CWEIGHT = new ELispSymbol(":weight");
    public final static ELispSymbol CWIDTH = new ELispSymbol(":width");
    public final static ELispSymbol CWINDOW = new ELispSymbol(":window");
    public final static ELispSymbol DASHES = new ELispSymbol("dashes");
    public final static ELispSymbol DEFAULT = new ELispSymbol("default");
    public final static ELispSymbol DOTS = new ELispSymbol("dots");
    public final static ELispSymbol DOUBLE_LINE = new ELispSymbol("double-line");
    public final static ELispSymbol EXTRA_BOLD = new ELispSymbol("extra-bold");
    public final static ELispSymbol EXTRA_LIGHT = new ELispSymbol("extra-light");
    public final static ELispSymbol FACE = new ELispSymbol("face");
    public final static ELispSymbol FACE_ALIAS = new ELispSymbol("face-alias");
    public final static ELispSymbol FACE_DEFAULT_STIPPLE = new ELispSymbol("face-default-stipple");
    public final static ELispSymbol FACE_FILTERS_ALWAYS_MATCH = new ELispSymbol("face-filters-always-match");
    public final static ELispSymbol FACE_FONT_LAX_MATCHED_ATTRIBUTES = new ELispSymbol("face-font-lax-matched-attributes");
    public final static ELispSymbol FACE_FONT_RESCALE_ALIST = new ELispSymbol("face-font-rescale-alist");
    public final static ELispSymbol FACE_IGNORED_FONTS = new ELispSymbol("face-ignored-fonts");
    public final static ELispSymbol FACE_NEAR_SAME_COLOR_THRESHOLD = new ELispSymbol("face-near-same-color-threshold");
    public final static ELispSymbol FACE_NO_INHERIT = new ELispSymbol("face-no-inherit");
    public final static ELispSymbol FACE_REMAPPING_ALIST = new ELispSymbol("face-remapping-alist");
    public final static ELispSymbol FACE__NEW_FRAME_DEFAULTS = new ELispSymbol("face--new-frame-defaults");
    public final static ELispSymbol FLAT_BUTTON = new ELispSymbol("flat-button");
    public final static ELispSymbol FOREGROUND_COLOR = new ELispSymbol("foreground-color");
    public final static ELispSymbol FRAME_SET_BACKGROUND_MODE = new ELispSymbol("frame-set-background-mode");
    public final static ELispSymbol FRINGE = new ELispSymbol("fringe");
    public final static ELispSymbol HEADER_LINE = new ELispSymbol("header-line");
    public final static ELispSymbol HEAVY = new ELispSymbol("heavy");
    public final static ELispSymbol INTERNAL_BORDER = new ELispSymbol("internal-border");
    public final static ELispSymbol ITALIC = new ELispSymbol("italic");
    public final static ELispSymbol LIGHT = new ELispSymbol("light");
    public final static ELispSymbol LINE = new ELispSymbol("line");
    public final static ELispSymbol MEDIUM = new ELispSymbol("medium");
    public final static ELispSymbol MENU = new ELispSymbol("menu");
    public final static ELispSymbol MODE_LINE_ACTIVE = new ELispSymbol("mode-line-active");
    public final static ELispSymbol MODE_LINE_INACTIVE = new ELispSymbol("mode-line-inactive");
    public final static ELispSymbol MOUSE = new ELispSymbol("mouse");
    public final static ELispSymbol NORMAL = new ELispSymbol("normal");
    public final static ELispSymbol OBLIQUE = new ELispSymbol("oblique");
    public final static ELispSymbol PRESSED_BUTTON = new ELispSymbol("pressed-button");
    public final static ELispSymbol RELEASED_BUTTON = new ELispSymbol("released-button");
    public final static ELispSymbol RESET = new ELispSymbol("reset");
    public final static ELispSymbol SCALABLE_FONTS_ALLOWED = new ELispSymbol("scalable-fonts-allowed");
    public final static ELispSymbol SCROLL_BAR = new ELispSymbol("scroll-bar");
    public final static ELispSymbol SEMI_BOLD = new ELispSymbol("semi-bold");
    public final static ELispSymbol SEMI_LIGHT = new ELispSymbol("semi-light");
    public final static ELispSymbol TAB_BAR = new ELispSymbol("tab-bar");
    public final static ELispSymbol TAB_LINE = new ELispSymbol("tab-line");
    public final static ELispSymbol THIN = new ELispSymbol("thin");
    public final static ELispSymbol TOOL_BAR = new ELispSymbol("tool-bar");
    public final static ELispSymbol TTY_COLOR_ALIST = new ELispSymbol("tty-color-alist");
    public final static ELispSymbol TTY_COLOR_BY_INDEX = new ELispSymbol("tty-color-by-index");
    public final static ELispSymbol TTY_COLOR_DESC = new ELispSymbol("tty-color-desc");
    public final static ELispSymbol TTY_COLOR_STANDARD_VALUES = new ELispSymbol("tty-color-standard-values");
    public final static ELispSymbol TTY_DEFINED_COLOR_ALIST = new ELispSymbol("tty-defined-color-alist");
    public final static ELispSymbol ULTRA_BOLD = new ELispSymbol("ultra-bold");
    public final static ELispSymbol ULTRA_HEAVY = new ELispSymbol("ultra-heavy");
    public final static ELispSymbol ULTRA_LIGHT = new ELispSymbol("ultra-light");
    public final static ELispSymbol UNSPECIFIED = new ELispSymbol("unspecified");
    public final static ELispSymbol VERTICAL_BORDER = new ELispSymbol("vertical-border");
    public final static ELispSymbol WAVE = new ELispSymbol("wave");
    public final static ELispSymbol WINDOW_DIVIDER = new ELispSymbol("window-divider");
    public final static ELispSymbol WINDOW_DIVIDER_FIRST_PIXEL = new ELispSymbol("window-divider-first-pixel");
    public final static ELispSymbol WINDOW_DIVIDER_LAST_PIXEL = new ELispSymbol("window-divider-last-pixel");
    private ELispSymbol[] xfacesSymbols() {
        return new ELispSymbol[]{
                BACKGROUND_COLOR,
                BITMAP_SPEC_P,
                BLACK,
                BOLD,
                BOOK,
                BORDER,
                CBACKGROUND,
                CBOLD,
                CBOX,
                CCOLOR,
                CDISTANT_FOREGROUND,
                CEXTEND,
                CFAMILY,
                CFILTERED,
                CFONT,
                CFONTSET,
                CFOREGROUND,
                CHEIGHT,
                CHILD_FRAME_BORDER,
                CIGNORE_DEFFACE,
                CINHERIT,
                CINVERSE_VIDEO,
                CITALIC,
                CLINE_WIDTH,
                COVERLINE,
                CPOSITION,
                CSLANT,
                CSTIPPLE,
                CSTRIKE_THROUGH,
                CSTYLE,
                CUNDERLINE,
                CURSOR,
                CWEIGHT,
                CWIDTH,
                CWINDOW,
                DASHES,
                DEFAULT,
                DOTS,
                DOUBLE_LINE,
                EXTRA_BOLD,
                EXTRA_LIGHT,
                FACE,
                FACE_ALIAS,
                FACE_DEFAULT_STIPPLE,
                FACE_FILTERS_ALWAYS_MATCH,
                FACE_FONT_LAX_MATCHED_ATTRIBUTES,
                FACE_FONT_RESCALE_ALIST,
                FACE_IGNORED_FONTS,
                FACE_NEAR_SAME_COLOR_THRESHOLD,
                FACE_NO_INHERIT,
                FACE_REMAPPING_ALIST,
                FACE__NEW_FRAME_DEFAULTS,
                FLAT_BUTTON,
                FOREGROUND_COLOR,
                FRAME_SET_BACKGROUND_MODE,
                FRINGE,
                HEADER_LINE,
                HEAVY,
                INTERNAL_BORDER,
                ITALIC,
                LIGHT,
                LINE,
                MEDIUM,
                MENU,
                MODE_LINE_ACTIVE,
                MODE_LINE_INACTIVE,
                MOUSE,
                NORMAL,
                OBLIQUE,
                PRESSED_BUTTON,
                RELEASED_BUTTON,
                RESET,
                SCALABLE_FONTS_ALLOWED,
                SCROLL_BAR,
                SEMI_BOLD,
                SEMI_LIGHT,
                TAB_BAR,
                TAB_LINE,
                THIN,
                TOOL_BAR,
                TTY_COLOR_ALIST,
                TTY_COLOR_BY_INDEX,
                TTY_COLOR_DESC,
                TTY_COLOR_STANDARD_VALUES,
                TTY_DEFINED_COLOR_ALIST,
                ULTRA_BOLD,
                ULTRA_HEAVY,
                ULTRA_LIGHT,
                UNSPECIFIED,
                VERTICAL_BORDER,
                WAVE,
                WINDOW_DIVIDER,
                WINDOW_DIVIDER_FIRST_PIXEL,
                WINDOW_DIVIDER_LAST_PIXEL,
        };
    }
    //#endregion xfaces.c
    //#region timefns.c
    public final static ELispSymbol CURRENT_TIME_LIST = new ELispSymbol("current-time-list");
    public final static ELispSymbol ENCODE_TIME = new ELispSymbol("encode-time");
    private ELispSymbol[] timefnsSymbols() {
        return new ELispSymbol[]{
                CURRENT_TIME_LIST,
                ENCODE_TIME,
        };
    }
    //#endregion timefns.c
    //#region casetab.c
    public final static ELispSymbol CASE_TABLE = new ELispSymbol("case-table");
    public final static ELispSymbol CASE_TABLE_P = new ELispSymbol("case-table-p");
    private ELispSymbol[] casetabSymbols() {
        return new ELispSymbol[]{
                CASE_TABLE,
                CASE_TABLE_P,
        };
    }
    //#endregion casetab.c
    //#region cmds.c
    public final static ELispSymbol EXPAND_ABBREV = new ELispSymbol("expand-abbrev");
    public final static ELispSymbol INTERNAL_AUTO_FILL = new ELispSymbol("internal-auto-fill");
    public final static ELispSymbol KILL_FORWARD_CHARS = new ELispSymbol("kill-forward-chars");
    public final static ELispSymbol NO_SELF_INSERT = new ELispSymbol("no-self-insert");
    public final static ELispSymbol OVERWRITE_MODE_BINARY = new ELispSymbol("overwrite-mode-binary");
    public final static ELispSymbol POST_SELF_INSERT_HOOK = new ELispSymbol("post-self-insert-hook");
    public final static ELispSymbol UNDO_AUTO_AMALGAMATE = new ELispSymbol("undo-auto-amalgamate");
    public final static ELispSymbol UNDO_AUTO__THIS_COMMAND_AMALGAMATING = new ELispSymbol("undo-auto--this-command-amalgamating");
    private ELispSymbol[] cmdsSymbols() {
        return new ELispSymbol[]{
                EXPAND_ABBREV,
                INTERNAL_AUTO_FILL,
                KILL_FORWARD_CHARS,
                NO_SELF_INSERT,
                OVERWRITE_MODE_BINARY,
                POST_SELF_INSERT_HOOK,
                UNDO_AUTO_AMALGAMATE,
                UNDO_AUTO__THIS_COMMAND_AMALGAMATING,
        };
    }
    //#endregion cmds.c
    //#region keyboard.c
    public final static ELispSymbol ABOVE_HANDLE = new ELispSymbol("above-handle");
    public final static ELispSymbol ACTIVATE_MARK_HOOK = new ELispSymbol("activate-mark-hook");
    public final static ELispSymbol ACTIVATE_MENUBAR_HOOK = new ELispSymbol("activate-menubar-hook");
    public final static ELispSymbol AFTER_HANDLE = new ELispSymbol("after-handle");
    public final static ELispSymbol ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL = new ELispSymbol("attempt-orderly-shutdown-on-fatal-signal");
    public final static ELispSymbol ATTEMPT_STACK_OVERFLOW_RECOVERY = new ELispSymbol("attempt-stack-overflow-recovery");
    public final static ELispSymbol AUTO_SAVE_INTERVAL = new ELispSymbol("auto-save-interval");
    public final static ELispSymbol AUTO_SAVE_NO_MESSAGE = new ELispSymbol("auto-save-no-message");
    public final static ELispSymbol AUTO_SAVE_TIMEOUT = new ELispSymbol("auto-save-timeout");
    public final static ELispSymbol BEFORE_HANDLE = new ELispSymbol("before-handle");
    public final static ELispSymbol BELOW_HANDLE = new ELispSymbol("below-handle");
    public final static ELispSymbol BOTTOM = new ELispSymbol("bottom");
    public final static ELispSymbol BOTTOM_DIVIDER = new ELispSymbol("bottom-divider");
    public final static ELispSymbol BOTTOM_EDGE = new ELispSymbol("bottom-edge");
    public final static ELispSymbol BOTTOM_LEFT_CORNER = new ELispSymbol("bottom-left-corner");
    public final static ELispSymbol BOTTOM_RIGHT_CORNER = new ELispSymbol("bottom-right-corner");
    public final static ELispSymbol CANNOT_SUSPEND = new ELispSymbol("cannot-suspend");
    public final static ELispSymbol CBUTTON = new ELispSymbol(":button");
    public final static ELispSymbol CENABLE = new ELispSymbol(":enable");
    public final static ELispSymbol CFILTER = new ELispSymbol(":filter");
    public final static ELispSymbol CHELP = new ELispSymbol(":help");
    public final static ELispSymbol CIMAGE = new ELispSymbol(":image");
    public final static ELispSymbol CKEYS = new ELispSymbol(":keys");
    public final static ELispSymbol CKEY_SEQUENCE = new ELispSymbol(":key-sequence");
    public final static ELispSymbol CLABEL = new ELispSymbol(":label");
    public final static ELispSymbol CODING = new ELispSymbol("coding");
    public final static ELispSymbol COMMAND_ERROR_DEFAULT_FUNCTION = new ELispSymbol("command-error-default-function");
    public final static ELispSymbol COMMAND_ERROR_FUNCTION = new ELispSymbol("command-error-function");
    public final static ELispSymbol COMMAND_EXECUTE = new ELispSymbol("command-execute");
    public final static ELispSymbol CONCAT = new ELispSymbol("concat");
    public final static ELispSymbol CONFIG_CHANGED_EVENT = new ELispSymbol("config-changed-event");
    public final static ELispSymbol CRADIO = new ELispSymbol(":radio");
    public final static ELispSymbol CRTL = new ELispSymbol(":rtl");
    public final static ELispSymbol CTOGGLE = new ELispSymbol(":toggle");
    public final static ELispSymbol CURRENT_KEY_REMAP_SEQUENCE = new ELispSymbol("current-key-remap-sequence");
    public final static ELispSymbol CURRENT_MINIBUFFER_COMMAND = new ELispSymbol("current-minibuffer-command");
    public final static ELispSymbol CVERT_ONLY = new ELispSymbol(":vert-only");
    public final static ELispSymbol CVISIBLE = new ELispSymbol(":visible");
    public final static ELispSymbol CWRAP = new ELispSymbol(":wrap");
    public final static ELispSymbol DBUS_EVENT = new ELispSymbol("dbus-event");
    public final static ELispSymbol DEACTIVATE_MARK = new ELispSymbol("deactivate-mark");
    public final static ELispSymbol DEBUG_ON_EVENT = new ELispSymbol("debug-on-event");
    public final static ELispSymbol DELAYED_WARNINGS_HOOK = new ELispSymbol("delayed-warnings-hook");
    public final static ELispSymbol DELAYED_WARNINGS_LIST = new ELispSymbol("delayed-warnings-list");
    public final static ELispSymbol DELETE_FRAME = new ELispSymbol("delete-frame");
    public final static ELispSymbol DISABLED = new ELispSymbol("disabled");
    public final static ELispSymbol DISABLE_INHIBIT_TEXT_CONVERSION = new ELispSymbol("disable-inhibit-text-conversion");
    public final static ELispSymbol DISABLE_POINT_ADJUSTMENT = new ELispSymbol("disable-point-adjustment");
    public final static ELispSymbol DISPLAY_MONITORS_CHANGED_FUNCTIONS = new ELispSymbol("display-monitors-changed-functions");
    public final static ELispSymbol DOUBLE_CLICK_FUZZ = new ELispSymbol("double-click-fuzz");
    public final static ELispSymbol DOUBLE_CLICK_TIME = new ELispSymbol("double-click-time");
    public final static ELispSymbol DOWN = new ELispSymbol("down");
    public final static ELispSymbol DRAG_INTERNAL_BORDER = new ELispSymbol("drag-internal-border");
    public final static ELispSymbol DRAG_N_DROP = new ELispSymbol("drag-n-drop");
    public final static ELispSymbol ECHO_AREA_CLEAR_HOOK = new ELispSymbol("echo-area-clear-hook");
    public final static ELispSymbol ECHO_KEYSTROKES = new ELispSymbol("echo-keystrokes");
    public final static ELispSymbol ECHO_KEYSTROKES_HELP = new ELispSymbol("echo-keystrokes-help");
    public final static ELispSymbol ENABLE_DISABLED_MENUS_AND_BUTTONS = new ELispSymbol("enable-disabled-menus-and-buttons");
    public final static ELispSymbol ENCODED = new ELispSymbol("encoded");
    public final static ELispSymbol END_SCROLL = new ELispSymbol("end-scroll");
    public final static ELispSymbol END_SESSION = new ELispSymbol("end-session");
    public final static ELispSymbol EVENT_KIND = new ELispSymbol("event-kind");
    public final static ELispSymbol EVENT_SYMBOL_ELEMENTS = new ELispSymbol("event-symbol-elements");
    public final static ELispSymbol EVENT_SYMBOL_ELEMENT_MASK = new ELispSymbol("event-symbol-element-mask");
    public final static ELispSymbol EXTRA_KEYBOARD_MODIFIERS = new ELispSymbol("extra-keyboard-modifiers");
    public final static ELispSymbol FILE_NOTIFY = new ELispSymbol("file-notify");
    public final static ELispSymbol FOCUS_IN = new ELispSymbol("focus-in");
    public final static ELispSymbol FOCUS_OUT = new ELispSymbol("focus-out");
    public final static ELispSymbol FUNCTION_KEY = new ELispSymbol("function-key");
    public final static ELispSymbol FUNCTION_KEY_MAP = new ELispSymbol("function-key-map");
    public final static ELispSymbol GLOBAL_DISABLE_POINT_ADJUSTMENT = new ELispSymbol("global-disable-point-adjustment");
    public final static ELispSymbol GUI_SET_SELECTION = new ELispSymbol("gui-set-selection");
    public final static ELispSymbol HANDLE = new ELispSymbol("handle");
    public final static ELispSymbol HANDLE_SELECT_WINDOW = new ELispSymbol("handle-select-window");
    public final static ELispSymbol HANDLE_SWITCH_FRAME = new ELispSymbol("handle-switch-frame");
    public final static ELispSymbol HELP_CHAR = new ELispSymbol("help-char");
    public final static ELispSymbol HELP_ECHO = new ELispSymbol("help-echo");
    public final static ELispSymbol HELP_ECHO_INHIBIT_SUBSTITUTION = new ELispSymbol("help-echo-inhibit-substitution");
    public final static ELispSymbol HELP_EVENT_LIST = new ELispSymbol("help-event-list");
    public final static ELispSymbol HELP_FORM = new ELispSymbol("help-form");
    public final static ELispSymbol HELP_FORM_SHOW = new ELispSymbol("help-form-show");
    public final static ELispSymbol HELP__APPEND_KEYSTROKES_HELP = new ELispSymbol("help--append-keystrokes-help");
    public final static ELispSymbol HORIZONTAL_HANDLE = new ELispSymbol("horizontal-handle");
    public final static ELispSymbol ICONIFY_FRAME = new ELispSymbol("iconify-frame");
    public final static ELispSymbol INHIBIT__RECORD_CHAR = new ELispSymbol("inhibit--record-char");
    public final static ELispSymbol INPUT_DECODE_MAP = new ELispSymbol("input-decode-map");
    public final static ELispSymbol INPUT_METHOD_EXIT_ON_FIRST_CHAR = new ELispSymbol("input-method-exit-on-first-char");
    public final static ELispSymbol INPUT_METHOD_FUNCTION = new ELispSymbol("input-method-function");
    public final static ELispSymbol INPUT_METHOD_PREVIOUS_MESSAGE = new ELispSymbol("input-method-previous-message");
    public final static ELispSymbol INPUT_METHOD_USE_ECHO_AREA = new ELispSymbol("input-method-use-echo-area");
    public final static ELispSymbol INPUT_PENDING_P_FILTER_EVENTS = new ELispSymbol("input-pending-p-filter-events");
    public final static ELispSymbol INTERNAL_ECHO_KEYSTROKES_PREFIX = new ELispSymbol("internal-echo-keystrokes-prefix");
    public final static ELispSymbol INTERNAL_TIMER_START_IDLE = new ELispSymbol("internal-timer-start-idle");
    public final static ELispSymbol INTERNAL__TOP_LEVEL_MESSAGE = new ELispSymbol("internal--top-level-message");
    public final static ELispSymbol KEYBOARD_TRANSLATE_TABLE = new ELispSymbol("keyboard-translate-table");
    public final static ELispSymbol KEY_TRANSLATION_MAP = new ELispSymbol("key-translation-map");
    public final static ELispSymbol LANGUAGE_CHANGE = new ELispSymbol("language-change");
    public final static ELispSymbol LAST_COMMAND = new ELispSymbol("last-command");
    public final static ELispSymbol LAST_COMMAND_EVENT = new ELispSymbol("last-command-event");
    public final static ELispSymbol LAST_EVENT_DEVICE = new ELispSymbol("last-event-device");
    public final static ELispSymbol LAST_EVENT_FRAME = new ELispSymbol("last-event-frame");
    public final static ELispSymbol LAST_INPUT_EVENT = new ELispSymbol("last-input-event");
    public final static ELispSymbol LAST_REPEATABLE_COMMAND = new ELispSymbol("last-repeatable-command");
    public final static ELispSymbol LEFTMOST = new ELispSymbol("leftmost");
    public final static ELispSymbol LEFT_EDGE = new ELispSymbol("left-edge");
    public final static ELispSymbol LOCAL_FUNCTION_KEY_MAP = new ELispSymbol("local-function-key-map");
    public final static ELispSymbol LONG_LINE_OPTIMIZATIONS_IN_COMMAND_HOOKS = new ELispSymbol("long-line-optimizations-in-command-hooks");
    public final static ELispSymbol LUCID__MENU_GRAB_KEYBOARD = new ELispSymbol("lucid--menu-grab-keyboard");
    public final static ELispSymbol MAKE_FRAME_VISIBLE = new ELispSymbol("make-frame-visible");
    public final static ELispSymbol MENU_BAR_FINAL_ITEMS = new ELispSymbol("menu-bar-final-items");
    public final static ELispSymbol MENU_ENABLE = new ELispSymbol("menu-enable");
    public final static ELispSymbol MENU_PROMPTING = new ELispSymbol("menu-prompting");
    public final static ELispSymbol MENU_PROMPT_MORE_CHAR = new ELispSymbol("menu-prompt-more-char");
    public final static ELispSymbol META_PREFIX_CHAR = new ELispSymbol("meta-prefix-char");
    public final static ELispSymbol MINIBUFFER_MESSAGE_TIMEOUT = new ELispSymbol("minibuffer-message-timeout");
    public final static ELispSymbol MODIFIER_CACHE = new ELispSymbol("modifier-cache");
    public final static ELispSymbol MOUSE_CLICK = new ELispSymbol("mouse-click");
    public final static ELispSymbol MOUSE_FIXUP_HELP_MESSAGE = new ELispSymbol("mouse-fixup-help-message");
    public final static ELispSymbol MOUSE_MOVEMENT = new ELispSymbol("mouse-movement");
    public final static ELispSymbol MOVE_FRAME = new ELispSymbol("move-frame");
    public final static ELispSymbol MWHEEL_COALESCE_SCROLL_EVENTS = new ELispSymbol("mwheel-coalesce-scroll-events");
    public final static ELispSymbol NO_RECORD = new ELispSymbol("no-record");
    public final static ELispSymbol NS_UNPUT_WORKING_TEXT = new ELispSymbol("ns-unput-working-text");
    public final static ELispSymbol NUM_INPUT_KEYS = new ELispSymbol("num-input-keys");
    public final static ELispSymbol NUM_NONMACRO_INPUT_EVENTS = new ELispSymbol("num-nonmacro-input-events");
    public final static ELispSymbol OVERRIDING_LOCAL_MAP = new ELispSymbol("overriding-local-map");
    public final static ELispSymbol OVERRIDING_LOCAL_MAP_MENU_FLAG = new ELispSymbol("overriding-local-map-menu-flag");
    public final static ELispSymbol OVERRIDING_TERMINAL_LOCAL_MAP = new ELispSymbol("overriding-terminal-local-map");
    public final static ELispSymbol PINCH = new ELispSymbol("pinch");
    public final static ELispSymbol POLLING_PERIOD = new ELispSymbol("polling-period");
    public final static ELispSymbol POST_COMMAND_HOOK = new ELispSymbol("post-command-hook");
    public final static ELispSymbol POST_SELECT_REGION_HOOK = new ELispSymbol("post-select-region-hook");
    public final static ELispSymbol PREEDIT_TEXT = new ELispSymbol("preedit-text");
    public final static ELispSymbol PREFIX_HELP_COMMAND = new ELispSymbol("prefix-help-command");
    public final static ELispSymbol PRE_COMMAND_HOOK = new ELispSymbol("pre-command-hook");
    public final static ELispSymbol PRIMARY = new ELispSymbol("PRIMARY");
    public final static ELispSymbol RATIO = new ELispSymbol("ratio");
    public final static ELispSymbol REAL_LAST_COMMAND = new ELispSymbol("real-last-command");
    public final static ELispSymbol RECORD_ALL_KEYS = new ELispSymbol("record-all-keys");
    public final static ELispSymbol RIGHTMOST = new ELispSymbol("rightmost");
    public final static ELispSymbol RIGHT_DIVIDER = new ELispSymbol("right-divider");
    public final static ELispSymbol RIGHT_EDGE = new ELispSymbol("right-edge");
    public final static ELispSymbol SAVED_REGION_SELECTION = new ELispSymbol("saved-region-selection");
    public final static ELispSymbol SAVE_SESSION = new ELispSymbol("save-session");
    public final static ELispSymbol SCROLL_BAR_MOVEMENT = new ELispSymbol("scroll-bar-movement");
    public final static ELispSymbol SELECTION_INHIBIT_UPDATE_COMMANDS = new ELispSymbol("selection-inhibit-update-commands");
    public final static ELispSymbol SELECTION_REQUEST = new ELispSymbol("selection-request");
    public final static ELispSymbol SELECT_ACTIVE_REGIONS = new ELispSymbol("select-active-regions");
    public final static ELispSymbol SELECT_WINDOW = new ELispSymbol("select-window");
    public final static ELispSymbol SHOW_HELP_FUNCTION = new ELispSymbol("show-help-function");
    public final static ELispSymbol SIGUSR2 = new ELispSymbol("sigusr2");
    public final static ELispSymbol SPECIAL_EVENT_MAP = new ELispSymbol("special-event-map");
    public final static ELispSymbol SUSPEND_HOOK = new ELispSymbol("suspend-hook");
    public final static ELispSymbol SUSPEND_RESUME_HOOK = new ELispSymbol("suspend-resume-hook");
    public final static ELispSymbol SWITCH_FRAME = new ELispSymbol("switch-frame");
    public final static ELispSymbol SYSTEM_KEY_ALIST = new ELispSymbol("system-key-alist");
    public final static ELispSymbol TAB_BAR_SEPARATOR_IMAGE_EXPRESSION = new ELispSymbol("tab-bar-separator-image-expression");
    public final static ELispSymbol TEXT_CONVERSION = new ELispSymbol("text-conversion");
    public final static ELispSymbol THIS_COMMAND = new ELispSymbol("this-command");
    public final static ELispSymbol THIS_COMMAND_KEYS_SHIFT_TRANSLATED = new ELispSymbol("this-command-keys-shift-translated");
    public final static ELispSymbol THIS_ORIGINAL_COMMAND = new ELispSymbol("this-original-command");
    public final static ELispSymbol THREAD_EVENT = new ELispSymbol("thread-event");
    public final static ELispSymbol THROW_ON_INPUT = new ELispSymbol("throw-on-input");
    public final static ELispSymbol TIMER_EVENT_HANDLER = new ELispSymbol("timer-event-handler");
    public final static ELispSymbol TIMER_IDLE_LIST = new ELispSymbol("timer-idle-list");
    public final static ELispSymbol TIMER_LIST = new ELispSymbol("timer-list");
    public final static ELispSymbol TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION = new ELispSymbol("tool-bar-separator-image-expression");
    public final static ELispSymbol TOP = new ELispSymbol("top");
    public final static ELispSymbol TOP_EDGE = new ELispSymbol("top-edge");
    public final static ELispSymbol TOP_LEFT_CORNER = new ELispSymbol("top-left-corner");
    public final static ELispSymbol TOP_RIGHT_CORNER = new ELispSymbol("top-right-corner");
    public final static ELispSymbol TOUCHSCREEN = new ELispSymbol("touchscreen");
    public final static ELispSymbol TOUCHSCREEN_BEGIN = new ELispSymbol("touchscreen-begin");
    public final static ELispSymbol TOUCHSCREEN_END = new ELispSymbol("touchscreen-end");
    public final static ELispSymbol TOUCHSCREEN_UPDATE = new ELispSymbol("touchscreen-update");
    public final static ELispSymbol TOUCH_END = new ELispSymbol("touch-end");
    public final static ELispSymbol TRACK_MOUSE = new ELispSymbol("track-mouse");
    public final static ELispSymbol TRANSLATE_UPPER_CASE_KEY_BINDINGS = new ELispSymbol("translate-upper-case-key-bindings");
    public final static ELispSymbol TTY_ERASE_CHAR = new ELispSymbol("tty-erase-char");
    public final static ELispSymbol TTY_SELECT_ACTIVE_REGIONS = new ELispSymbol("tty-select-active-regions");
    public final static ELispSymbol UNDEFINED = new ELispSymbol("undefined");
    public final static ELispSymbol UNDO_AUTO__ADD_BOUNDARY = new ELispSymbol("undo-auto--add-boundary");
    public final static ELispSymbol UNDO_AUTO__UNDOABLY_CHANGED_BUFFERS = new ELispSymbol("undo-auto--undoably-changed-buffers");
    public final static ELispSymbol UNREAD_COMMAND_EVENTS = new ELispSymbol("unread-command-events");
    public final static ELispSymbol UNREAD_INPUT_METHOD_EVENTS = new ELispSymbol("unread-input-method-events");
    public final static ELispSymbol UNREAD_POST_INPUT_METHOD_EVENTS = new ELispSymbol("unread-post-input-method-events");
    public final static ELispSymbol UP = new ELispSymbol("up");
    public final static ELispSymbol VERTICAL_LINE = new ELispSymbol("vertical-line");
    public final static ELispSymbol WHILE_NO_INPUT_IGNORE_EVENTS = new ELispSymbol("while-no-input-ignore-events");
    public final static ELispSymbol WINDOW_EDGES = new ELispSymbol("window-edges");
    public final static ELispSymbol XTERM__SET_SELECTION = new ELispSymbol("xterm--set-selection");
    public final static ELispSymbol XWIDGET_DISPLAY_EVENT = new ELispSymbol("xwidget-display-event");
    public final static ELispSymbol XWIDGET_EVENT = new ELispSymbol("xwidget-event");
    private ELispSymbol[] keyboardSymbols() {
        return new ELispSymbol[]{
                ABOVE_HANDLE,
                ACTIVATE_MARK_HOOK,
                ACTIVATE_MENUBAR_HOOK,
                AFTER_HANDLE,
                ATTEMPT_ORDERLY_SHUTDOWN_ON_FATAL_SIGNAL,
                ATTEMPT_STACK_OVERFLOW_RECOVERY,
                AUTO_SAVE_INTERVAL,
                AUTO_SAVE_NO_MESSAGE,
                AUTO_SAVE_TIMEOUT,
                BEFORE_HANDLE,
                BELOW_HANDLE,
                BOTTOM,
                BOTTOM_DIVIDER,
                BOTTOM_EDGE,
                BOTTOM_LEFT_CORNER,
                BOTTOM_RIGHT_CORNER,
                CANNOT_SUSPEND,
                CBUTTON,
                CENABLE,
                CFILTER,
                CHELP,
                CIMAGE,
                CKEYS,
                CKEY_SEQUENCE,
                CLABEL,
                CODING,
                COMMAND_ERROR_DEFAULT_FUNCTION,
                COMMAND_ERROR_FUNCTION,
                COMMAND_EXECUTE,
                CONCAT,
                CONFIG_CHANGED_EVENT,
                CRADIO,
                CRTL,
                CTOGGLE,
                CURRENT_KEY_REMAP_SEQUENCE,
                CURRENT_MINIBUFFER_COMMAND,
                CVERT_ONLY,
                CVISIBLE,
                CWRAP,
                DBUS_EVENT,
                DEACTIVATE_MARK,
                DEBUG_ON_EVENT,
                DELAYED_WARNINGS_HOOK,
                DELAYED_WARNINGS_LIST,
                DELETE_FRAME,
                DISABLED,
                DISABLE_INHIBIT_TEXT_CONVERSION,
                DISABLE_POINT_ADJUSTMENT,
                DISPLAY_MONITORS_CHANGED_FUNCTIONS,
                DOUBLE_CLICK_FUZZ,
                DOUBLE_CLICK_TIME,
                DOWN,
                DRAG_INTERNAL_BORDER,
                DRAG_N_DROP,
                ECHO_AREA_CLEAR_HOOK,
                ECHO_KEYSTROKES,
                ECHO_KEYSTROKES_HELP,
                ENABLE_DISABLED_MENUS_AND_BUTTONS,
                ENCODED,
                END_SCROLL,
                END_SESSION,
                EVENT_KIND,
                EVENT_SYMBOL_ELEMENTS,
                EVENT_SYMBOL_ELEMENT_MASK,
                EXTRA_KEYBOARD_MODIFIERS,
                FILE_NOTIFY,
                FOCUS_IN,
                FOCUS_OUT,
                FUNCTION_KEY,
                FUNCTION_KEY_MAP,
                GLOBAL_DISABLE_POINT_ADJUSTMENT,
                GUI_SET_SELECTION,
                HANDLE,
                HANDLE_SELECT_WINDOW,
                HANDLE_SWITCH_FRAME,
                HELP_CHAR,
                HELP_ECHO,
                HELP_ECHO_INHIBIT_SUBSTITUTION,
                HELP_EVENT_LIST,
                HELP_FORM,
                HELP_FORM_SHOW,
                HELP_KEY_BINDING,
                HELP__APPEND_KEYSTROKES_HELP,
                HORIZONTAL_HANDLE,
                ICONIFY_FRAME,
                INHIBIT__RECORD_CHAR,
                INPUT_DECODE_MAP,
                INPUT_METHOD_EXIT_ON_FIRST_CHAR,
                INPUT_METHOD_FUNCTION,
                INPUT_METHOD_PREVIOUS_MESSAGE,
                INPUT_METHOD_USE_ECHO_AREA,
                INPUT_PENDING_P_FILTER_EVENTS,
                INTERNAL_ECHO_KEYSTROKES_PREFIX,
                INTERNAL_TIMER_START_IDLE,
                INTERNAL__TOP_LEVEL_MESSAGE,
                KEYBOARD_TRANSLATE_TABLE,
                KEY_TRANSLATION_MAP,
                LANGUAGE_CHANGE,
                LAST_COMMAND,
                LAST_COMMAND_EVENT,
                LAST_EVENT_DEVICE,
                LAST_EVENT_FRAME,
                LAST_INPUT_EVENT,
                LAST_NONMENU_EVENT,
                LAST_REPEATABLE_COMMAND,
                LEFT,
                LEFTMOST,
                LEFT_EDGE,
                LOCAL_FUNCTION_KEY_MAP,
                LONG_LINE_OPTIMIZATIONS_IN_COMMAND_HOOKS,
                LUCID__MENU_GRAB_KEYBOARD,
                MAKE_FRAME_VISIBLE,
                MENU_BAR_FINAL_ITEMS,
                MENU_ENABLE,
                MENU_PROMPTING,
                MENU_PROMPT_MORE_CHAR,
                META_PREFIX_CHAR,
                MINIBUFFER_MESSAGE_TIMEOUT,
                MODIFIER_CACHE,
                MOUSE_CLICK,
                MOUSE_FIXUP_HELP_MESSAGE,
                MOUSE_MOVEMENT,
                MOVE_FRAME,
                MWHEEL_COALESCE_SCROLL_EVENTS,
                NO_RECORD,
                NS_UNPUT_WORKING_TEXT,
                NUM_INPUT_KEYS,
                NUM_NONMACRO_INPUT_EVENTS,
                OVERRIDING_LOCAL_MAP,
                OVERRIDING_LOCAL_MAP_MENU_FLAG,
                OVERRIDING_TERMINAL_LOCAL_MAP,
                PINCH,
                POLLING_PERIOD,
                POST_COMMAND_HOOK,
                POST_SELECT_REGION_HOOK,
                PREEDIT_TEXT,
                PREFIX_HELP_COMMAND,
                PRE_COMMAND_HOOK,
                PRIMARY,
                RATIO,
                REAL_LAST_COMMAND,
                REAL_THIS_COMMAND,
                RECORD_ALL_KEYS,
                RIGHT,
                RIGHTMOST,
                RIGHT_DIVIDER,
                RIGHT_EDGE,
                SAVED_REGION_SELECTION,
                SAVE_SESSION,
                SCROLL_BAR_MOVEMENT,
                SELECTION_INHIBIT_UPDATE_COMMANDS,
                SELECTION_REQUEST,
                SELECT_ACTIVE_REGIONS,
                SELECT_WINDOW,
                SHOW_HELP_FUNCTION,
                SIGUSR2,
                SPECIAL_EVENT_MAP,
                SUSPEND_HOOK,
                SUSPEND_RESUME_HOOK,
                SWITCH_FRAME,
                SYSTEM_KEY_ALIST,
                TAB_BAR_SEPARATOR_IMAGE_EXPRESSION,
                TEXT_CONVERSION,
                THIS_COMMAND,
                THIS_COMMAND_KEYS_SHIFT_TRANSLATED,
                THIS_ORIGINAL_COMMAND,
                THREAD_EVENT,
                THROW_ON_INPUT,
                TIMER_EVENT_HANDLER,
                TIMER_IDLE_LIST,
                TIMER_LIST,
                TOOL_BAR_SEPARATOR_IMAGE_EXPRESSION,
                TOP,
                TOP_EDGE,
                TOP_LEFT_CORNER,
                TOP_LEVEL,
                TOP_RIGHT_CORNER,
                TOUCHSCREEN,
                TOUCHSCREEN_BEGIN,
                TOUCHSCREEN_END,
                TOUCHSCREEN_UPDATE,
                TOUCH_END,
                TRACK_MOUSE,
                TRANSLATE_UPPER_CASE_KEY_BINDINGS,
                TTY_ERASE_CHAR,
                TTY_SELECT_ACTIVE_REGIONS,
                UNDEFINED,
                UNDO_AUTO__ADD_BOUNDARY,
                UNDO_AUTO__UNDOABLY_CHANGED_BUFFERS,
                UNREAD_COMMAND_EVENTS,
                UNREAD_INPUT_METHOD_EVENTS,
                UNREAD_POST_INPUT_METHOD_EVENTS,
                UP,
                VERTICAL_LINE,
                WHILE_NO_INPUT_IGNORE_EVENTS,
                WINDOW_EDGES,
                XTERM__SET_SELECTION,
                XWIDGET_DISPLAY_EVENT,
                XWIDGET_EVENT,
        };
    }
    //#endregion keyboard.c
    //#region callint.c
    public final static ELispSymbol COMMAND_DEBUG_STATUS = new ELispSymbol("command-debug-status");
    public final static ELispSymbol COMMAND_HISTORY = new ELispSymbol("command-history");
    public final static ELispSymbol CURRENT_PREFIX_ARG = new ELispSymbol("current-prefix-arg");
    public final static ELispSymbol ENABLE_RECURSIVE_MINIBUFFERS = new ELispSymbol("enable-recursive-minibuffers");
    public final static ELispSymbol EVAL_MINIBUFFER = new ELispSymbol("eval-minibuffer");
    public final static ELispSymbol FUNCALL_INTERACTIVELY = new ELispSymbol("funcall-interactively");
    public final static ELispSymbol HANDLE_SHIFT_SELECTION = new ELispSymbol("handle-shift-selection");
    public final static ELispSymbol IF = new ELispSymbol("if");
    public final static ELispSymbol INHIBIT_MOUSE_EVENT_CHECK = new ELispSymbol("inhibit-mouse-event-check");
    public final static ELispSymbol INTERACTIVE_ARGS = new ELispSymbol("interactive-args");
    public final static ELispSymbol LAST_PREFIX_ARG = new ELispSymbol("last-prefix-arg");
    public final static ELispSymbol LET = new ELispSymbol("let");
    public final static ELispSymbol LETX = new ELispSymbol("let*");
    public final static ELispSymbol LIST = new ELispSymbol("list");
    public final static ELispSymbol MARK_EVEN_IF_INACTIVE = new ELispSymbol("mark-even-if-inactive");
    public final static ELispSymbol MINUS = new ELispSymbol("-");
    public final static ELispSymbol MOUSE_LEAVE_BUFFER_HOOK = new ELispSymbol("mouse-leave-buffer-hook");
    public final static ELispSymbol PLUS = new ELispSymbol("+");
    public final static ELispSymbol PREFIX_ARG = new ELispSymbol("prefix-arg");
    public final static ELispSymbol PROGN = new ELispSymbol("progn");
    public final static ELispSymbol READ_FILE_NAME = new ELispSymbol("read-file-name");
    public final static ELispSymbol READ_NUMBER = new ELispSymbol("read-number");
    public final static ELispSymbol SAVE_EXCURSION = new ELispSymbol("save-excursion");
    public final static ELispSymbol WHEN = new ELispSymbol("when");
    private ELispSymbol[] callintSymbols() {
        return new ELispSymbol[]{
                COMMAND_DEBUG_STATUS,
                COMMAND_HISTORY,
                CURRENT_PREFIX_ARG,
                ENABLE_RECURSIVE_MINIBUFFERS,
                EVAL_MINIBUFFER,
                FUNCALL_INTERACTIVELY,
                HANDLE_SHIFT_SELECTION,
                IF,
                INHIBIT_MOUSE_EVENT_CHECK,
                INTERACTIVE_ARGS,
                LAST_PREFIX_ARG,
                LET,
                LETX,
                LIST,
                MARK_EVEN_IF_INACTIVE,
                MINUS,
                MOUSE_LEAVE_BUFFER_HOOK,
                PLUS,
                PREFIX_ARG,
                PROGN,
                READ_FILE_NAME,
                READ_NUMBER,
                SAVE_EXCURSION,
                WHEN,
        };
    }
    //#endregion callint.c
    //#region casefiddle.c
    public final static ELispSymbol BOUNDS = new ELispSymbol("bounds");
    public final static ELispSymbol CASE_SYMBOLS_AS_WORDS = new ELispSymbol("case-symbols-as-words");
    public final static ELispSymbol IDENTITY = new ELispSymbol("identity");
    public final static ELispSymbol LOWERCASE = new ELispSymbol("lowercase");
    public final static ELispSymbol REGION_EXTRACT_FUNCTION = new ELispSymbol("region-extract-function");
    public final static ELispSymbol SPECIAL_LOWERCASE = new ELispSymbol("special-lowercase");
    public final static ELispSymbol SPECIAL_TITLECASE = new ELispSymbol("special-titlecase");
    public final static ELispSymbol SPECIAL_UPPERCASE = new ELispSymbol("special-uppercase");
    public final static ELispSymbol TITLECASE = new ELispSymbol("titlecase");
    public final static ELispSymbol UPPERCASE = new ELispSymbol("uppercase");
    private ELispSymbol[] casefiddleSymbols() {
        return new ELispSymbol[]{
                BOUNDS,
                CASE_SYMBOLS_AS_WORDS,
                IDENTITY,
                LOWERCASE,
                REGION_EXTRACT_FUNCTION,
                SPECIAL_LOWERCASE,
                SPECIAL_TITLECASE,
                SPECIAL_UPPERCASE,
                TITLECASE,
                UPPERCASE,
        };
    }
    //#endregion casefiddle.c
    //#region coding.c
    public final static ELispSymbol BIG = new ELispSymbol("big");
    public final static ELispSymbol BIG5 = new ELispSymbol("big5");
    public final static ELispSymbol BUFFER_FILE_CODING_SYSTEM = new ELispSymbol("buffer-file-coding-system");
    public final static ELispSymbol CALL_PROCESS = new ELispSymbol("call-process");
    public final static ELispSymbol CALL_PROCESS_REGION = new ELispSymbol("call-process-region");
    public final static ELispSymbol CASCII_COMPATIBLE_P = new ELispSymbol(":ascii-compatible-p");
    public final static ELispSymbol CCATEGORY = new ELispSymbol(":category");
    public final static ELispSymbol CDECODE_TRANSLATION_TABLE = new ELispSymbol(":decode-translation-table");
    public final static ELispSymbol CDEFAULT_CHAR = new ELispSymbol(":default-char");
    public final static ELispSymbol CENCODE_TRANSLATION_TABLE = new ELispSymbol(":encode-translation-table");
    public final static ELispSymbol CHARSET = new ELispSymbol("charset");
    public final static ELispSymbol CHARSET_REVISION_TABLE = new ELispSymbol("charset-revision-table");
    public final static ELispSymbol CMNEMONIC = new ELispSymbol(":mnemonic");
    public final static ELispSymbol CODING_CATEGORY_LIST = new ELispSymbol("coding-category-list");
    public final static ELispSymbol CODING_SYSTEM_ALIST = new ELispSymbol("coding-system-alist");
    public final static ELispSymbol CODING_SYSTEM_DEFINE_FORM = new ELispSymbol("coding-system-define-form");
    public final static ELispSymbol CODING_SYSTEM_ERROR = new ELispSymbol("coding-system-error");
    public final static ELispSymbol CODING_SYSTEM_FOR_READ = new ELispSymbol("coding-system-for-read");
    public final static ELispSymbol CODING_SYSTEM_FOR_WRITE = new ELispSymbol("coding-system-for-write");
    public final static ELispSymbol CODING_SYSTEM_HISTORY = new ELispSymbol("coding-system-history");
    public final static ELispSymbol CODING_SYSTEM_LIST = new ELispSymbol("coding-system-list");
    public final static ELispSymbol CODING_SYSTEM_P = new ELispSymbol("coding-system-p");
    public final static ELispSymbol CODING_SYSTEM_REQUIRE_WARNING = new ELispSymbol("coding-system-require-warning");
    public final static ELispSymbol CPOST_READ_CONVERSION = new ELispSymbol(":post-read-conversion");
    public final static ELispSymbol CPRE_WRITE_CONVERSION = new ELispSymbol(":pre-write-conversion");
    public final static ELispSymbol DEFAULT_PROCESS_CODING_SYSTEM = new ELispSymbol("default-process-coding-system");
    public final static ELispSymbol DEFINE_CODING_SYSTEM_INTERNAL = new ELispSymbol("define-coding-system-internal");
    public final static ELispSymbol DISABLE_ASCII_OPTIMIZATION = new ELispSymbol("disable-ascii-optimization");
    public final static ELispSymbol DOS = new ELispSymbol("dos");
    public final static ELispSymbol EMACS_MULE = new ELispSymbol("emacs-mule");
    public final static ELispSymbol ENABLE_CHARACTER_TRANSLATION = new ELispSymbol("enable-character-translation");
    public final static ELispSymbol EOL_MNEMONIC_DOS = new ELispSymbol("eol-mnemonic-dos");
    public final static ELispSymbol EOL_MNEMONIC_MAC = new ELispSymbol("eol-mnemonic-mac");
    public final static ELispSymbol EOL_MNEMONIC_UNDECIDED = new ELispSymbol("eol-mnemonic-undecided");
    public final static ELispSymbol EOL_MNEMONIC_UNIX = new ELispSymbol("eol-mnemonic-unix");
    public final static ELispSymbol FILENAMEP = new ELispSymbol("filenamep");
    public final static ELispSymbol FILE_CODING_SYSTEM_ALIST = new ELispSymbol("file-coding-system-alist");
    public final static ELispSymbol IGNORED = new ELispSymbol("ignored");
    public final static ELispSymbol INHERIT_PROCESS_CODING_SYSTEM = new ELispSymbol("inherit-process-coding-system");
    public final static ELispSymbol INHIBIT_EOL_CONVERSION = new ELispSymbol("inhibit-eol-conversion");
    public final static ELispSymbol INHIBIT_ISO_ESCAPE_DETECTION = new ELispSymbol("inhibit-iso-escape-detection");
    public final static ELispSymbol INHIBIT_NULL_BYTE_DETECTION = new ELispSymbol("inhibit-null-byte-detection");
    public final static ELispSymbol INSUFFICIENT_SOURCE = new ELispSymbol("insufficient-source");
    public final static ELispSymbol INTERRUPTED = new ELispSymbol("interrupted");
    public final static ELispSymbol INVALID_SOURCE = new ELispSymbol("invalid-source");
    public final static ELispSymbol ISO_2022 = new ELispSymbol("iso-2022");
    public final static ELispSymbol LAST_CODE_CONVERSION_ERROR = new ELispSymbol("last-code-conversion-error");
    public final static ELispSymbol LAST_CODING_SYSTEM_USED = new ELispSymbol("last-coding-system-used");
    public final static ELispSymbol LATIN_EXTRA_CODE_TABLE = new ELispSymbol("latin-extra-code-table");
    public final static ELispSymbol LITTLE = new ELispSymbol("little");
    public final static ELispSymbol LOCALE_CODING_SYSTEM = new ELispSymbol("locale-coding-system");
    public final static ELispSymbol MAC = new ELispSymbol("mac");
    public final static ELispSymbol NETWORK_CODING_SYSTEM_ALIST = new ELispSymbol("network-coding-system-alist");
    public final static ELispSymbol NO_CONVERSION = new ELispSymbol("no-conversion");
    public final static ELispSymbol OPEN_NETWORK_STREAM = new ELispSymbol("open-network-stream");
    public final static ELispSymbol PROCESS_CODING_SYSTEM_ALIST = new ELispSymbol("process-coding-system-alist");
    public final static ELispSymbol RAW_TEXT = new ELispSymbol("raw-text");
    public final static ELispSymbol SELECT_SAFE_CODING_SYSTEM_FUNCTION = new ELispSymbol("select-safe-coding-system-function");
    public final static ELispSymbol SHIFT_JIS = new ELispSymbol("shift-jis");
    public final static ELispSymbol STANDARD_TRANSLATION_TABLE_FOR_DECODE = new ELispSymbol("standard-translation-table-for-decode");
    public final static ELispSymbol STANDARD_TRANSLATION_TABLE_FOR_ENCODE = new ELispSymbol("standard-translation-table-for-encode");
    public final static ELispSymbol START_PROCESS = new ELispSymbol("start-process");
    public final static ELispSymbol TARGET_IDX = new ELispSymbol("target-idx");
    public final static ELispSymbol TRANSLATION_TABLE = new ELispSymbol("translation-table");
    public final static ELispSymbol TRANSLATION_TABLE_FOR_INPUT = new ELispSymbol("translation-table-for-input");
    public final static ELispSymbol TRANSLATION_TABLE_ID = new ELispSymbol("translation-table-id");
    public final static ELispSymbol UNDECIDED = new ELispSymbol("undecided");
    public final static ELispSymbol UNIX = new ELispSymbol("unix");
    public final static ELispSymbol UNKNOWN_ERROR = new ELispSymbol("Unknown error");
    public final static ELispSymbol US_ASCII = new ELispSymbol("us-ascii");
    public final static ELispSymbol UTF_16 = new ELispSymbol("utf-16");
    public final static ELispSymbol UTF_16LE = new ELispSymbol("utf-16le");
    public final static ELispSymbol UTF_8 = new ELispSymbol("utf-8");
    public final static ELispSymbol UTF_8_EMACS = new ELispSymbol("utf-8-emacs");
    public final static ELispSymbol UTF_8_STRING_P = new ELispSymbol("utf-8-string-p");
    public final static ELispSymbol UTF_8_UNIX = new ELispSymbol("utf-8-unix");
    private ELispSymbol[] codingSymbols() {
        return new ELispSymbol[]{
                BIG,
                BIG5,
                BUFFER_FILE_CODING_SYSTEM,
                CALL_PROCESS,
                CALL_PROCESS_REGION,
                CASCII_COMPATIBLE_P,
                CCATEGORY,
                CDECODE_TRANSLATION_TABLE,
                CDEFAULT_CHAR,
                CENCODE_TRANSLATION_TABLE,
                CHARSET,
                CHARSET_REVISION_TABLE,
                CMNEMONIC,
                CODING_CATEGORY_LIST,
                CODING_SYSTEM_ALIST,
                CODING_SYSTEM_DEFINE_FORM,
                CODING_SYSTEM_ERROR,
                CODING_SYSTEM_FOR_READ,
                CODING_SYSTEM_FOR_WRITE,
                CODING_SYSTEM_HISTORY,
                CODING_SYSTEM_LIST,
                CODING_SYSTEM_P,
                CODING_SYSTEM_REQUIRE_WARNING,
                CPOST_READ_CONVERSION,
                CPRE_WRITE_CONVERSION,
                DEFAULT_PROCESS_CODING_SYSTEM,
                DEFINE_CODING_SYSTEM_INTERNAL,
                DISABLE_ASCII_OPTIMIZATION,
                DOS,
                EMACS_MULE,
                ENABLE_CHARACTER_TRANSLATION,
                EOL_MNEMONIC_DOS,
                EOL_MNEMONIC_MAC,
                EOL_MNEMONIC_UNDECIDED,
                EOL_MNEMONIC_UNIX,
                FILENAMEP,
                FILE_CODING_SYSTEM_ALIST,
                IGNORED,
                INHERIT_PROCESS_CODING_SYSTEM,
                INHIBIT_EOL_CONVERSION,
                INHIBIT_ISO_ESCAPE_DETECTION,
                INHIBIT_NULL_BYTE_DETECTION,
                INSUFFICIENT_SOURCE,
                INTERRUPTED,
                INVALID_SOURCE,
                ISO_2022,
                LAST_CODE_CONVERSION_ERROR,
                LAST_CODING_SYSTEM_USED,
                LATIN_EXTRA_CODE_TABLE,
                LITTLE,
                LOCALE_CODING_SYSTEM,
                MAC,
                NETWORK_CODING_SYSTEM_ALIST,
                NO_CONVERSION,
                OPEN_NETWORK_STREAM,
                PROCESS_CODING_SYSTEM_ALIST,
                RAW_TEXT,
                SELECT_SAFE_CODING_SYSTEM_FUNCTION,
                SHIFT_JIS,
                STANDARD_TRANSLATION_TABLE_FOR_DECODE,
                STANDARD_TRANSLATION_TABLE_FOR_ENCODE,
                START_PROCESS,
                TARGET_IDX,
                TRANSLATION_TABLE,
                TRANSLATION_TABLE_FOR_INPUT,
                TRANSLATION_TABLE_ID,
                UNDECIDED,
                UNIX,
                UNKNOWN_ERROR,
                US_ASCII,
                UTF_16,
                UTF_16LE,
                UTF_8,
                UTF_8_EMACS,
                UTF_8_STRING_P,
                UTF_8_UNIX,
        };
    }
    //#endregion coding.c
    //#region character.c
    public final static ELispSymbol AMBIGUOUS_WIDTH_CHARS = new ELispSymbol("ambiguous-width-chars");
    public final static ELispSymbol AUTO_FILL_CHARS = new ELispSymbol("auto-fill-chars");
    public final static ELispSymbol CHARACTERP = new ELispSymbol("characterp");
    public final static ELispSymbol CHAR_SCRIPT_TABLE = new ELispSymbol("char-script-table");
    public final static ELispSymbol CHAR_WIDTH_TABLE = new ELispSymbol("char-width-table");
    public final static ELispSymbol PRINTABLE_CHARS = new ELispSymbol("printable-chars");
    public final static ELispSymbol SCRIPT_REPRESENTATIVE_CHARS = new ELispSymbol("script-representative-chars");
    public final static ELispSymbol TRANSLATION_TABLE_VECTOR = new ELispSymbol("translation-table-vector");
    public final static ELispSymbol UNICODE_CATEGORY_TABLE = new ELispSymbol("unicode-category-table");
    private ELispSymbol[] characterSymbols() {
        return new ELispSymbol[]{
                AMBIGUOUS_WIDTH_CHARS,
                AUTO_FILL_CHARS,
                CHARACTERP,
                CHAR_SCRIPT_TABLE,
                CHAR_WIDTH_TABLE,
                PRINTABLE_CHARS,
                SCRIPT_REPRESENTATIVE_CHARS,
                TRANSLATION_TABLE_VECTOR,
                UNICODE_CATEGORY_TABLE,
        };
    }
    //#endregion character.c
    //#region window.c
    public final static ELispSymbol ABOVE = new ELispSymbol("above");
    public final static ELispSymbol AUTO_WINDOW_VSCROLL = new ELispSymbol("auto-window-vscroll");
    public final static ELispSymbol CEILING = new ELispSymbol("ceiling");
    public final static ELispSymbol CLONE_OF = new ELispSymbol("clone-of");
    public final static ELispSymbol CONFIGURATION = new ELispSymbol("configuration");
    public final static ELispSymbol DEDICATED = new ELispSymbol("dedicated");
    public final static ELispSymbol DELETE = new ELispSymbol("delete");
    public final static ELispSymbol DELETE_WINDOW = new ELispSymbol("delete-window");
    public final static ELispSymbol DISPLAY_BUFFER = new ELispSymbol("display-buffer");
    public final static ELispSymbol FAST_BUT_IMPRECISE_SCROLLING = new ELispSymbol("fast-but-imprecise-scrolling");
    public final static ELispSymbol FLOOR = new ELispSymbol("floor");
    public final static ELispSymbol GET_MRU_WINDOW = new ELispSymbol("get-mru-window");
    public final static ELispSymbol HEADER_LINE_FORMAT = new ELispSymbol("header-line-format");
    public final static ELispSymbol MARK_FOR_REDISPLAY = new ELispSymbol("mark-for-redisplay");
    public final static ELispSymbol MINIBUFFER_SCROLL_WINDOW = new ELispSymbol("minibuffer-scroll-window");
    public final static ELispSymbol MODE_LINE_FORMAT = new ELispSymbol("mode-line-format");
    public final static ELispSymbol MODE_LINE_IN_NON_SELECTED_WINDOWS = new ELispSymbol("mode-line-in-non-selected-windows");
    public final static ELispSymbol NEXT_SCREEN_CONTEXT_LINES = new ELispSymbol("next-screen-context-lines");
    public final static ELispSymbol NO_OTHER_WINDOW = new ELispSymbol("no-other-window");
    public final static ELispSymbol OTHER_WINDOW_SCROLL_BUFFER = new ELispSymbol("other-window-scroll-buffer");
    public final static ELispSymbol OTHER_WINDOW_SCROLL_DEFAULT = new ELispSymbol("other-window-scroll-default");
    public final static ELispSymbol QUIT_RESTORE = new ELispSymbol("quit-restore");
    public final static ELispSymbol QUIT_RESTORE_PREV = new ELispSymbol("quit-restore-prev");
    public final static ELispSymbol RECENTER_REDISPLAY = new ELispSymbol("recenter-redisplay");
    public final static ELispSymbol RECORD_WINDOW_BUFFER = new ELispSymbol("record-window-buffer");
    public final static ELispSymbol REPLACE_BUFFER_IN_WINDOWS = new ELispSymbol("replace-buffer-in-windows");
    public final static ELispSymbol SAFE = new ELispSymbol("safe");
    public final static ELispSymbol SCROLL_COMMAND = new ELispSymbol("scroll-command");
    public final static ELispSymbol SCROLL_DOWN = new ELispSymbol("scroll-down");
    public final static ELispSymbol SCROLL_PRESERVE_SCREEN_POSITION = new ELispSymbol("scroll-preserve-screen-position");
    public final static ELispSymbol SCROLL_UP = new ELispSymbol("scroll-up");
    public final static ELispSymbol TAB_LINE_FORMAT = new ELispSymbol("tab-line-format");
    public final static ELispSymbol TEMP_BUFFER_SHOW_FUNCTION = new ELispSymbol("temp-buffer-show-function");
    public final static ELispSymbol TEMP_BUFFER_SHOW_HOOK = new ELispSymbol("temp-buffer-show-hook");
    public final static ELispSymbol WINDOWP = new ELispSymbol("windowp");
    public final static ELispSymbol WINDOW_BUFFER_CHANGE_FUNCTIONS = new ELispSymbol("window-buffer-change-functions");
    public final static ELispSymbol WINDOW_COMBINATION_LIMIT = new ELispSymbol("window-combination-limit");
    public final static ELispSymbol WINDOW_COMBINATION_RESIZE = new ELispSymbol("window-combination-resize");
    public final static ELispSymbol WINDOW_CONFIGURATION_CHANGE_HOOK = new ELispSymbol("window-configuration-change-hook");
    public final static ELispSymbol WINDOW_CONFIGURATION_P = new ELispSymbol("window-configuration-p");
    public final static ELispSymbol WINDOW_DEAD_WINDOWS_TABLE = new ELispSymbol("window-dead-windows-table");
    public final static ELispSymbol WINDOW_DELETABLE_P = new ELispSymbol("window-deletable-p");
    public final static ELispSymbol WINDOW_LIVE_P = new ELispSymbol("window-live-p");
    public final static ELispSymbol WINDOW_PERSISTENT_PARAMETERS = new ELispSymbol("window-persistent-parameters");
    public final static ELispSymbol WINDOW_POINT_INSERTION_TYPE = new ELispSymbol("window-point-insertion-type");
    public final static ELispSymbol WINDOW_RESIZE_PIXELWISE = new ELispSymbol("window-resize-pixelwise");
    public final static ELispSymbol WINDOW_RESTORE_KILLED_BUFFER_WINDOWS = new ELispSymbol("window-restore-killed-buffer-windows");
    public final static ELispSymbol WINDOW_SELECTION_CHANGE_FUNCTIONS = new ELispSymbol("window-selection-change-functions");
    public final static ELispSymbol WINDOW_SIZE = new ELispSymbol("window-size");
    public final static ELispSymbol WINDOW_SIZE_CHANGE_FUNCTIONS = new ELispSymbol("window-size-change-functions");
    public final static ELispSymbol WINDOW_STATE_CHANGE_FUNCTIONS = new ELispSymbol("window-state-change-functions");
    public final static ELispSymbol WINDOW_STATE_CHANGE_HOOK = new ELispSymbol("window-state-change-hook");
    public final static ELispSymbol WINDOW_VALID_P = new ELispSymbol("window-valid-p");
    public final static ELispSymbol WINDOW__PIXEL_TO_TOTAL = new ELispSymbol("window--pixel-to-total");
    public final static ELispSymbol WINDOW__RESIZE_MINI_FRAME = new ELispSymbol("window--resize-mini-frame");
    public final static ELispSymbol WINDOW__RESIZE_ROOT_WINDOW = new ELispSymbol("window--resize-root-window");
    public final static ELispSymbol WINDOW__RESIZE_ROOT_WINDOW_VERTICALLY = new ELispSymbol("window--resize-root-window-vertically");
    private ELispSymbol[] windowSymbols() {
        return new ELispSymbol[]{
                ABOVE,
                AUTO_WINDOW_VSCROLL,
                CEILING,
                CLONE_OF,
                CONFIGURATION,
                DEDICATED,
                DELETE,
                DELETE_WINDOW,
                DISPLAY_BUFFER,
                FAST_BUT_IMPRECISE_SCROLLING,
                FLOOR,
                GET_MRU_WINDOW,
                HEADER_LINE_FORMAT,
                MARK_FOR_REDISPLAY,
                MINIBUFFER_SCROLL_WINDOW,
                MODE_LINE_FORMAT,
                MODE_LINE_IN_NON_SELECTED_WINDOWS,
                NEXT_SCREEN_CONTEXT_LINES,
                NO_OTHER_WINDOW,
                OTHER_WINDOW_SCROLL_BUFFER,
                OTHER_WINDOW_SCROLL_DEFAULT,
                QUIT_RESTORE,
                QUIT_RESTORE_PREV,
                RECENTER_REDISPLAY,
                RECORD_WINDOW_BUFFER,
                REPLACE_BUFFER_IN_WINDOWS,
                SAFE,
                SCROLL_COMMAND,
                SCROLL_DOWN,
                SCROLL_PRESERVE_SCREEN_POSITION,
                SCROLL_UP,
                TAB_LINE_FORMAT,
                TEMP_BUFFER_SHOW_FUNCTION,
                TEMP_BUFFER_SHOW_HOOK,
                WINDOWP,
                WINDOW_BUFFER_CHANGE_FUNCTIONS,
                WINDOW_COMBINATION_LIMIT,
                WINDOW_COMBINATION_RESIZE,
                WINDOW_CONFIGURATION_CHANGE_HOOK,
                WINDOW_CONFIGURATION_P,
                WINDOW_DEAD_WINDOWS_TABLE,
                WINDOW_DELETABLE_P,
                WINDOW_LIVE_P,
                WINDOW_PERSISTENT_PARAMETERS,
                WINDOW_POINT_INSERTION_TYPE,
                WINDOW_RESIZE_PIXELWISE,
                WINDOW_RESTORE_KILLED_BUFFER_WINDOWS,
                WINDOW_SELECTION_CHANGE_FUNCTIONS,
                WINDOW_SIZE,
                WINDOW_SIZE_CHANGE_FUNCTIONS,
                WINDOW_STATE_CHANGE_FUNCTIONS,
                WINDOW_STATE_CHANGE_HOOK,
                WINDOW_VALID_P,
                WINDOW__PIXEL_TO_TOTAL,
                WINDOW__RESIZE_MINI_FRAME,
                WINDOW__RESIZE_ROOT_WINDOW,
                WINDOW__RESIZE_ROOT_WINDOW_VERTICALLY,
        };
    }
    //#endregion window.c
    //#region frame.c
    public final static ELispSymbol ABOVE_SUSPENDED = new ELispSymbol("above-suspended");
    public final static ELispSymbol AFTER_DELETE_FRAME_FUNCTIONS = new ELispSymbol("after-delete-frame-functions");
    public final static ELispSymbol ALPHA = new ELispSymbol("alpha");
    public final static ELispSymbol ALPHA_BACKGROUND = new ELispSymbol("alpha-background");
    public final static ELispSymbol ANDROID = new ELispSymbol("android");
    public final static ELispSymbol AUTO_LOWER = new ELispSymbol("auto-lower");
    public final static ELispSymbol AUTO_RAISE = new ELispSymbol("auto-raise");
    public final static ELispSymbol BACKGROUND_MODE = new ELispSymbol("background-mode");
    public final static ELispSymbol BELOW = new ELispSymbol("below");
    public final static ELispSymbol BORDER_COLOR = new ELispSymbol("border-color");
    public final static ELispSymbol BORDER_WIDTH = new ELispSymbol("border-width");
    public final static ELispSymbol BOTTOM_DIVIDER_WIDTH = new ELispSymbol("bottom-divider-width");
    public final static ELispSymbol BUFFER_LIST = new ELispSymbol("buffer-list");
    public final static ELispSymbol BUFFER_PREDICATE = new ELispSymbol("buffer-predicate");
    public final static ELispSymbol BURIED_BUFFER_LIST = new ELispSymbol("buried-buffer-list");
    public final static ELispSymbol CHANGE_FRAME_SIZE = new ELispSymbol("change_frame_size");
    public final static ELispSymbol CHILD_FRAME_BORDER_WIDTH = new ELispSymbol("child-frame-border-width");
    public final static ELispSymbol CURSOR_COLOR = new ELispSymbol("cursor-color");
    public final static ELispSymbol CURSOR_TYPE = new ELispSymbol("cursor-type");
    public final static ELispSymbol DEFAULT_FRAME_ALIST = new ELispSymbol("default-frame-alist");
    public final static ELispSymbol DEFAULT_FRAME_SCROLL_BARS = new ELispSymbol("default-frame-scroll-bars");
    public final static ELispSymbol DEFAULT_MINIBUFFER_FRAME = new ELispSymbol("default-minibuffer-frame");
    public final static ELispSymbol DELETE_BEFORE = new ELispSymbol("delete-before");
    public final static ELispSymbol DELETE_FRAME_FUNCTIONS = new ELispSymbol("delete-frame-functions");
    public final static ELispSymbol DISPLAY_TYPE = new ELispSymbol("display-type");
    public final static ELispSymbol EXPLICIT_NAME = new ELispSymbol("explicit-name");
    public final static ELispSymbol EXTERNAL_BORDER_SIZE = new ELispSymbol("external-border-size");
    public final static ELispSymbol FACE_SET_AFTER_FRAME_DEFAULT = new ELispSymbol("face-set-after-frame-default");
    public final static ELispSymbol FOCUS_FOLLOWS_MOUSE = new ELispSymbol("focus-follows-mouse");
    public final static ELispSymbol FONT_BACKEND = new ELispSymbol("font-backend");
    public final static ELispSymbol FONT_PARAMETER = new ELispSymbol("font-parameter");
    public final static ELispSymbol FRAMEP = new ELispSymbol("framep");
    public final static ELispSymbol FRAMES = new ELispSymbol("frames");
    public final static ELispSymbol FRAME_ALPHA_LOWER_LIMIT = new ELispSymbol("frame-alpha-lower-limit");
    public final static ELispSymbol FRAME_EDGES = new ELispSymbol("frame-edges");
    public final static ELispSymbol FRAME_INHIBIT_IMPLIED_RESIZE = new ELispSymbol("frame-inhibit-implied-resize");
    public final static ELispSymbol FRAME_INTERNAL_PARAMETERS = new ELispSymbol("frame-internal-parameters");
    public final static ELispSymbol FRAME_LIVE_P = new ELispSymbol("frame-live-p");
    public final static ELispSymbol FRAME_MONITOR_ATTRIBUTES = new ELispSymbol("frame-monitor-attributes");
    public final static ELispSymbol FRAME_RESIZE_PIXELWISE = new ELispSymbol("frame-resize-pixelwise");
    public final static ELispSymbol FRAME_SIZE_HISTORY = new ELispSymbol("frame-size-history");
    public final static ELispSymbol FRAME_WINDOWS_MIN_SIZE = new ELispSymbol("frame-windows-min-size");
    public final static ELispSymbol FULLBOTH = new ELispSymbol("fullboth");
    public final static ELispSymbol FULLHEIGHT = new ELispSymbol("fullheight");
    public final static ELispSymbol FULLSCREEN = new ELispSymbol("fullscreen");
    public final static ELispSymbol FULLWIDTH = new ELispSymbol("fullwidth");
    public final static ELispSymbol GEOMETRY = new ELispSymbol("geometry");
    public final static ELispSymbol GUI_FIGURE_WINDOW_SIZE = new ELispSymbol("gui_figure_window_size");
    public final static ELispSymbol HAIKU = new ELispSymbol("haiku");
    public final static ELispSymbol HEIGHT = new ELispSymbol("height");
    public final static ELispSymbol HEIGHT_ONLY = new ELispSymbol("height-only");
    public final static ELispSymbol HORIZONTAL_SCROLL_BARS = new ELispSymbol("horizontal-scroll-bars");
    public final static ELispSymbol ICON = new ELispSymbol("icon");
    public final static ELispSymbol ICONIFY_CHILD_FRAME = new ELispSymbol("iconify-child-frame");
    public final static ELispSymbol ICONIFY_TOP_LEVEL = new ELispSymbol("iconify-top-level");
    public final static ELispSymbol ICON_LEFT = new ELispSymbol("icon-left");
    public final static ELispSymbol ICON_NAME = new ELispSymbol("icon-name");
    public final static ELispSymbol ICON_TOP = new ELispSymbol("icon-top");
    public final static ELispSymbol ICON_TYPE = new ELispSymbol("icon-type");
    public final static ELispSymbol INHIBIT_DOUBLE_BUFFERING = new ELispSymbol("inhibit-double-buffering");
    public final static ELispSymbol INNER_EDGES = new ELispSymbol("inner-edges");
    public final static ELispSymbol INTERNAL_BORDER_WIDTH = new ELispSymbol("internal-border-width");
    public final static ELispSymbol KEEP_RATIO = new ELispSymbol("keep-ratio");
    public final static ELispSymbol LEFT_FRINGE = new ELispSymbol("left-fringe");
    public final static ELispSymbol LEFT_FRINGE_HELP = new ELispSymbol("left-fringe-help");
    public final static ELispSymbol LEFT_ONLY = new ELispSymbol("left-only");
    public final static ELispSymbol LINE_SPACING = new ELispSymbol("line-spacing");
    public final static ELispSymbol MAKE_INITIAL_MINIBUFFER_FRAME = new ELispSymbol("make-initial-minibuffer-frame");
    public final static ELispSymbol MAKE_INVISIBLE = new ELispSymbol("make-invisible");
    public final static ELispSymbol MAKE_POINTER_INVISIBLE = new ELispSymbol("make-pointer-invisible");
    public final static ELispSymbol MAXIMIZED = new ELispSymbol("maximized");
    public final static ELispSymbol MENU_BAR_EXTERNAL = new ELispSymbol("menu-bar-external");
    public final static ELispSymbol MENU_BAR_LINES = new ELispSymbol("menu-bar-lines");
    public final static ELispSymbol MENU_BAR_MODE = new ELispSymbol("menu-bar-mode");
    public final static ELispSymbol MENU_BAR_SIZE = new ELispSymbol("menu-bar-size");
    public final static ELispSymbol MINIBUFFER = new ELispSymbol("minibuffer");
    public final static ELispSymbol MIN_HEIGHT = new ELispSymbol("min-height");
    public final static ELispSymbol MIN_WIDTH = new ELispSymbol("min-width");
    public final static ELispSymbol MM_SIZE = new ELispSymbol("mm-size");
    public final static ELispSymbol MODELINE = new ELispSymbol("modeline");
    public final static ELispSymbol MOUSE_COLOR = new ELispSymbol("mouse-color");
    public final static ELispSymbol MOUSE_HIGHLIGHT = new ELispSymbol("mouse-highlight");
    public final static ELispSymbol MOUSE_POSITION_FUNCTION = new ELispSymbol("mouse-position-function");
    public final static ELispSymbol MOUSE_WHEEL_FRAME = new ELispSymbol("mouse-wheel-frame");
    public final static ELispSymbol MOVE_FRAME_FUNCTIONS = new ELispSymbol("move-frame-functions");
    public final static ELispSymbol MOVE_TOOLBAR = new ELispSymbol("move-toolbar");
    public final static ELispSymbol NAME = new ELispSymbol("name");
    public final static ELispSymbol NATIVE_EDGES = new ELispSymbol("native-edges");
    public final static ELispSymbol NOELISP = new ELispSymbol("noelisp");
    public final static ELispSymbol NONE = new ELispSymbol("none");
    public final static ELispSymbol NO_ACCEPT_FOCUS = new ELispSymbol("no-accept-focus");
    public final static ELispSymbol NO_FOCUS_ON_MAP = new ELispSymbol("no-focus-on-map");
    public final static ELispSymbol NO_OTHER_FRAME = new ELispSymbol("no-other-frame");
    public final static ELispSymbol NO_SPECIAL_GLYPHS = new ELispSymbol("no-special-glyphs");
    public final static ELispSymbol NS = new ELispSymbol("ns");
    public final static ELispSymbol NS_APPEARANCE = new ELispSymbol("ns-appearance");
    public final static ELispSymbol NS_PARSE_GEOMETRY = new ELispSymbol("ns-parse-geometry");
    public final static ELispSymbol NS_TRANSPARENT_TITLEBAR = new ELispSymbol("ns-transparent-titlebar");
    public final static ELispSymbol ONLY = new ELispSymbol("only");
    public final static ELispSymbol OUTER_BORDER_WIDTH = new ELispSymbol("outer-border-width");
    public final static ELispSymbol OUTER_EDGES = new ELispSymbol("outer-edges");
    public final static ELispSymbol OUTER_POSITION = new ELispSymbol("outer-position");
    public final static ELispSymbol OUTER_SIZE = new ELispSymbol("outer-size");
    public final static ELispSymbol OUTER_WINDOW_ID = new ELispSymbol("outer-window-id");
    public final static ELispSymbol OVERRIDE_REDIRECT = new ELispSymbol("override-redirect");
    public final static ELispSymbol PARENT_FRAME = new ELispSymbol("parent-frame");
    public final static ELispSymbol PARENT_ID = new ELispSymbol("parent-id");
    public final static ELispSymbol PC = new ELispSymbol("pc");
    public final static ELispSymbol PGTK = new ELispSymbol("pgtk");
    public final static ELispSymbol RESIZE_MINI_FRAMES = new ELispSymbol("resize-mini-frames");
    public final static ELispSymbol RIGHT_DIVIDER_WIDTH = new ELispSymbol("right-divider-width");
    public final static ELispSymbol RIGHT_FRINGE = new ELispSymbol("right-fringe");
    public final static ELispSymbol RIGHT_FRINGE_HELP = new ELispSymbol("right-fringe-help");
    public final static ELispSymbol SCALE_FACTOR = new ELispSymbol("scale-factor");
    public final static ELispSymbol SCREEN_GAMMA = new ELispSymbol("screen-gamma");
    public final static ELispSymbol SCROLL_BAR_ADJUST_THUMB_PORTION = new ELispSymbol("scroll-bar-adjust-thumb-portion");
    public final static ELispSymbol SCROLL_BAR_BACKGROUND = new ELispSymbol("scroll-bar-background");
    public final static ELispSymbol SCROLL_BAR_FOREGROUND = new ELispSymbol("scroll-bar-foreground");
    public final static ELispSymbol SCROLL_BAR_HEIGHT = new ELispSymbol("scroll-bar-height");
    public final static ELispSymbol SCROLL_BAR_WIDTH = new ELispSymbol("scroll-bar-width");
    public final static ELispSymbol SET_WINDOW_CONFIGURATION = new ELispSymbol("set_window_configuration");
    public final static ELispSymbol SHADED = new ELispSymbol("shaded");
    public final static ELispSymbol SKIP_TASKBAR = new ELispSymbol("skip-taskbar");
    public final static ELispSymbol SOURCE = new ELispSymbol("source");
    public final static ELispSymbol STICKY = new ELispSymbol("sticky");
    public final static ELispSymbol TAB_BAR_LINES = new ELispSymbol("tab-bar-lines");
    public final static ELispSymbol TAB_BAR_MODE = new ELispSymbol("tab-bar-mode");
    public final static ELispSymbol TAB_BAR_SIZE = new ELispSymbol("tab-bar-size");
    public final static ELispSymbol TERMINAL_FRAME = new ELispSymbol("terminal_frame");
    public final static ELispSymbol TEXT_PIXELS = new ELispSymbol("text-pixels");
    public final static ELispSymbol TIP_FRAME = new ELispSymbol("tip_frame");
    public final static ELispSymbol TITLE = new ELispSymbol("title");
    public final static ELispSymbol TITLE_BAR_SIZE = new ELispSymbol("title-bar-size");
    public final static ELispSymbol TOOLTIP = new ELispSymbol("tooltip");
    public final static ELispSymbol TOOLTIP_REUSE_HIDDEN_FRAME = new ELispSymbol("tooltip-reuse-hidden-frame");
    public final static ELispSymbol TOOL_BAR_EXTERNAL = new ELispSymbol("tool-bar-external");
    public final static ELispSymbol TOOL_BAR_LINES = new ELispSymbol("tool-bar-lines");
    public final static ELispSymbol TOOL_BAR_MODE = new ELispSymbol("tool-bar-mode");
    public final static ELispSymbol TOOL_BAR_POSITION = new ELispSymbol("tool-bar-position");
    public final static ELispSymbol TOOL_BAR_SIZE = new ELispSymbol("tool-bar-size");
    public final static ELispSymbol TOP_ONLY = new ELispSymbol("top-only");
    public final static ELispSymbol TTY = new ELispSymbol("tty");
    public final static ELispSymbol TTY_COLOR_MODE = new ELispSymbol("tty-color-mode");
    public final static ELispSymbol TTY_TYPE = new ELispSymbol("tty-type");
    public final static ELispSymbol UNDECORATED = new ELispSymbol("undecorated");
    public final static ELispSymbol UNSPLITTABLE = new ELispSymbol("unsplittable");
    public final static ELispSymbol USER_POSITION = new ELispSymbol("user-position");
    public final static ELispSymbol USER_SIZE = new ELispSymbol("user-size");
    public final static ELispSymbol USE_FRAME_SYNCHRONIZATION = new ELispSymbol("use-frame-synchronization");
    public final static ELispSymbol USE_SYSTEM_TOOLTIPS = new ELispSymbol("use-system-tooltips");
    public final static ELispSymbol VERTICAL_SCROLL_BARS = new ELispSymbol("vertical-scroll-bars");
    public final static ELispSymbol VISIBILITY = new ELispSymbol("visibility");
    public final static ELispSymbol VISIBLE = new ELispSymbol("visible");
    public final static ELispSymbol W32 = new ELispSymbol("w32");
    public final static ELispSymbol WAIT_FOR_WM = new ELispSymbol("wait-for-wm");
    public final static ELispSymbol WIDTH = new ELispSymbol("width");
    public final static ELispSymbol WIDTH_ONLY = new ELispSymbol("width-only");
    public final static ELispSymbol WINDOW_ID = new ELispSymbol("window-id");
    public final static ELispSymbol WORKAREA = new ELispSymbol("workarea");
    public final static ELispSymbol X = new ELispSymbol("x");
    public final static ELispSymbol XG_FRAME_SET_CHAR_SIZE = new ELispSymbol("xg_frame_set_char_size");
    public final static ELispSymbol X_CREATE_FRAME_1 = new ELispSymbol("x_create_frame_1");
    public final static ELispSymbol X_CREATE_FRAME_2 = new ELispSymbol("x_create_frame_2");
    public final static ELispSymbol X_FRAME_PARAMETER = new ELispSymbol("x-frame-parameter");
    public final static ELispSymbol X_RESOURCE_CLASS = new ELispSymbol("x-resource-class");
    public final static ELispSymbol X_RESOURCE_NAME = new ELispSymbol("x-resource-name");
    public final static ELispSymbol X_SET_MENU_BAR_LINES = new ELispSymbol("x_set_menu_bar_lines");
    public final static ELispSymbol X_SET_WINDOW_SIZE_1 = new ELispSymbol("x_set_window_size_1");
    public final static ELispSymbol Z_GROUP = new ELispSymbol("z-group");
    private ELispSymbol[] frameSymbols() {
        return new ELispSymbol[]{
                ABOVE_SUSPENDED,
                AFTER_DELETE_FRAME_FUNCTIONS,
                ALPHA,
                ALPHA_BACKGROUND,
                ANDROID,
                AUTO_LOWER,
                AUTO_RAISE,
                BACKGROUND_MODE,
                BELOW,
                BORDER_COLOR,
                BORDER_WIDTH,
                BOTTOM_DIVIDER_WIDTH,
                BUFFER_LIST,
                BUFFER_PREDICATE,
                BURIED_BUFFER_LIST,
                CHANGE_FRAME_SIZE,
                CHILD_FRAME_BORDER_WIDTH,
                CURSOR_COLOR,
                CURSOR_TYPE,
                DEFAULT_FRAME_ALIST,
                DEFAULT_FRAME_SCROLL_BARS,
                DEFAULT_MINIBUFFER_FRAME,
                DELETE_BEFORE,
                DELETE_FRAME_FUNCTIONS,
                DISPLAY_TYPE,
                EXPLICIT_NAME,
                EXTERNAL_BORDER_SIZE,
                FACE_SET_AFTER_FRAME_DEFAULT,
                FOCUS_FOLLOWS_MOUSE,
                FONT_BACKEND,
                FONT_PARAMETER,
                FRAMEP,
                FRAMES,
                FRAME_ALPHA_LOWER_LIMIT,
                FRAME_EDGES,
                FRAME_INHIBIT_IMPLIED_RESIZE,
                FRAME_INTERNAL_PARAMETERS,
                FRAME_LIVE_P,
                FRAME_MONITOR_ATTRIBUTES,
                FRAME_RESIZE_PIXELWISE,
                FRAME_SIZE_HISTORY,
                FRAME_WINDOWS_MIN_SIZE,
                FULLBOTH,
                FULLHEIGHT,
                FULLSCREEN,
                FULLWIDTH,
                GEOMETRY,
                GUI_FIGURE_WINDOW_SIZE,
                HAIKU,
                HEIGHT,
                HEIGHT_ONLY,
                HORIZONTAL_SCROLL_BARS,
                ICON,
                ICONIFY_CHILD_FRAME,
                ICONIFY_TOP_LEVEL,
                ICON_LEFT,
                ICON_NAME,
                ICON_TOP,
                ICON_TYPE,
                INHIBIT_DOUBLE_BUFFERING,
                INNER_EDGES,
                INTERNAL_BORDER_WIDTH,
                KEEP_RATIO,
                LEFT_FRINGE,
                LEFT_FRINGE_HELP,
                LEFT_ONLY,
                LINE_SPACING,
                MAKE_INITIAL_MINIBUFFER_FRAME,
                MAKE_INVISIBLE,
                MAKE_POINTER_INVISIBLE,
                MAXIMIZED,
                MENU_BAR_EXTERNAL,
                MENU_BAR_LINES,
                MENU_BAR_MODE,
                MENU_BAR_SIZE,
                MINIBUFFER,
                MIN_HEIGHT,
                MIN_WIDTH,
                MM_SIZE,
                MODELINE,
                MOUSE_COLOR,
                MOUSE_HIGHLIGHT,
                MOUSE_POSITION_FUNCTION,
                MOUSE_WHEEL_FRAME,
                MOVE_FRAME_FUNCTIONS,
                MOVE_TOOLBAR,
                NAME,
                NATIVE_EDGES,
                NOELISP,
                NONE,
                NO_ACCEPT_FOCUS,
                NO_FOCUS_ON_MAP,
                NO_OTHER_FRAME,
                NO_SPECIAL_GLYPHS,
                NS,
                NS_APPEARANCE,
                NS_PARSE_GEOMETRY,
                NS_TRANSPARENT_TITLEBAR,
                ONLY,
                OUTER_BORDER_WIDTH,
                OUTER_EDGES,
                OUTER_POSITION,
                OUTER_SIZE,
                OUTER_WINDOW_ID,
                OVERRIDE_REDIRECT,
                PARENT_FRAME,
                PARENT_ID,
                PC,
                PGTK,
                RESIZE_MINI_FRAMES,
                RIGHT_DIVIDER_WIDTH,
                RIGHT_FRINGE,
                RIGHT_FRINGE_HELP,
                SCALE_FACTOR,
                SCREEN_GAMMA,
                SCROLL_BAR_ADJUST_THUMB_PORTION,
                SCROLL_BAR_BACKGROUND,
                SCROLL_BAR_FOREGROUND,
                SCROLL_BAR_HEIGHT,
                SCROLL_BAR_WIDTH,
                SET_WINDOW_CONFIGURATION,
                SHADED,
                SKIP_TASKBAR,
                SOURCE,
                STICKY,
                TAB_BAR_LINES,
                TAB_BAR_MODE,
                TAB_BAR_SIZE,
                TERMINAL_FRAME,
                TEXT_PIXELS,
                TIP_FRAME,
                TITLE,
                TITLE_BAR_SIZE,
                TOOLTIP,
                TOOLTIP_REUSE_HIDDEN_FRAME,
                TOOL_BAR_EXTERNAL,
                TOOL_BAR_LINES,
                TOOL_BAR_MODE,
                TOOL_BAR_POSITION,
                TOOL_BAR_SIZE,
                TOP_ONLY,
                TTY,
                TTY_COLOR_MODE,
                TTY_TYPE,
                UNDECORATED,
                UNSPLITTABLE,
                USER_POSITION,
                USER_SIZE,
                USE_FRAME_SYNCHRONIZATION,
                USE_SYSTEM_TOOLTIPS,
                VERTICAL_SCROLL_BARS,
                VISIBILITY,
                VISIBLE,
                W32,
                WAIT_FOR_WM,
                WIDTH,
                WIDTH_ONLY,
                WINDOW_ID,
                WINDOW__PIXEL_TO_TOTAL,
                WORKAREA,
                X,
                XG_FRAME_SET_CHAR_SIZE,
                X_CREATE_FRAME_1,
                X_CREATE_FRAME_2,
                X_FRAME_PARAMETER,
                X_RESOURCE_CLASS,
                X_RESOURCE_NAME,
                X_SET_MENU_BAR_LINES,
                X_SET_WINDOW_SIZE_1,
                Z_GROUP,
        };
    }
    //#endregion frame.c
    //#region textprop.c
    public final static ELispSymbol CATEGORY = new ELispSymbol("category");
    public final static ELispSymbol CHAR_PROPERTY_ALIAS_ALIST = new ELispSymbol("char-property-alias-alist");
    public final static ELispSymbol DEFAULT_TEXT_PROPERTIES = new ELispSymbol("default-text-properties");
    public final static ELispSymbol FONT = new ELispSymbol("font");
    public final static ELispSymbol FRONT_STICKY = new ELispSymbol("front-sticky");
    public final static ELispSymbol INHIBIT_POINT_MOTION_HOOKS = new ELispSymbol("inhibit-point-motion-hooks");
    public final static ELispSymbol INTANGIBLE = new ELispSymbol("intangible");
    public final static ELispSymbol INVISIBLE = new ELispSymbol("invisible");
    public final static ELispSymbol LOCAL_MAP = new ELispSymbol("local-map");
    public final static ELispSymbol MINIBUFFER_PROMPT = new ELispSymbol("minibuffer-prompt");
    public final static ELispSymbol MOUSE_FACE = new ELispSymbol("mouse-face");
    public final static ELispSymbol POINT_ENTERED = new ELispSymbol("point-entered");
    public final static ELispSymbol POINT_LEFT = new ELispSymbol("point-left");
    public final static ELispSymbol READ_ONLY = new ELispSymbol("read-only");
    public final static ELispSymbol REAR_NONSTICKY = new ELispSymbol("rear-nonsticky");
    public final static ELispSymbol TEXT_PROPERTY_DEFAULT_NONSTICKY = new ELispSymbol("text-property-default-nonsticky");
    private ELispSymbol[] textpropSymbols() {
        return new ELispSymbol[]{
                CATEGORY,
                CHAR_PROPERTY_ALIAS_ALIST,
                DEFAULT_TEXT_PROPERTIES,
                FACE,
                FONT,
                FRONT_STICKY,
                INHIBIT_POINT_MOTION_HOOKS,
                INTANGIBLE,
                INVISIBLE,
                LOCAL_MAP,
                MINIBUFFER_PROMPT,
                MOUSE_FACE,
                POINT_ENTERED,
                POINT_LEFT,
                READ_ONLY,
                REAR_NONSTICKY,
                TEXT_PROPERTY_DEFAULT_NONSTICKY,
        };
    }
    //#endregion textprop.c
    //#region syntax.c
    public final static ELispSymbol COMMENT_END_CAN_BE_ESCAPED = new ELispSymbol("comment-end-can-be-escaped");
    public final static ELispSymbol COMMENT_USE_SYNTAX_PPSS = new ELispSymbol("comment-use-syntax-ppss");
    public final static ELispSymbol FIND_WORD_BOUNDARY_FUNCTION_TABLE = new ELispSymbol("find-word-boundary-function-table");
    public final static ELispSymbol INTERNAL__SYNTAX_PROPERTIZE = new ELispSymbol("internal--syntax-propertize");
    public final static ELispSymbol MULTIBYTE_SYNTAX_AS_SYMBOL = new ELispSymbol("multibyte-syntax-as-symbol");
    public final static ELispSymbol OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START = new ELispSymbol("open-paren-in-column-0-is-defun-start");
    public final static ELispSymbol PARSE_SEXP_IGNORE_COMMENTS = new ELispSymbol("parse-sexp-ignore-comments");
    public final static ELispSymbol PARSE_SEXP_LOOKUP_PROPERTIES = new ELispSymbol("parse-sexp-lookup-properties");
    public final static ELispSymbol SCAN_ERROR = new ELispSymbol("scan-error");
    public final static ELispSymbol SYNTAX_PPSS = new ELispSymbol("syntax-ppss");
    public final static ELispSymbol SYNTAX_PROPERTIZE__DONE = new ELispSymbol("syntax-propertize--done");
    public final static ELispSymbol SYNTAX_TABLE = new ELispSymbol("syntax-table");
    public final static ELispSymbol SYNTAX_TABLE_P = new ELispSymbol("syntax-table-p");
    public final static ELispSymbol WORDS_INCLUDE_ESCAPES = new ELispSymbol("words-include-escapes");
    private ELispSymbol[] syntaxSymbols() {
        return new ELispSymbol[]{
                COMMENT_END_CAN_BE_ESCAPED,
                COMMENT_USE_SYNTAX_PPSS,
                FIND_WORD_BOUNDARY_FUNCTION_TABLE,
                INTERNAL__SYNTAX_PROPERTIZE,
                MULTIBYTE_SYNTAX_AS_SYMBOL,
                OPEN_PAREN_IN_COLUMN_0_IS_DEFUN_START,
                PARSE_SEXP_IGNORE_COMMENTS,
                PARSE_SEXP_LOOKUP_PROPERTIES,
                SCAN_ERROR,
                SYNTAX_PPSS,
                SYNTAX_PROPERTIZE__DONE,
                SYNTAX_TABLE,
                SYNTAX_TABLE_P,
                WORDS_INCLUDE_ESCAPES,
        };
    }
    //#endregion syntax.c
    //#region xdisp.c
    public final static ELispSymbol ARROW = new ELispSymbol("arrow");
    public final static ELispSymbol AUTO_HSCROLL_MODE = new ELispSymbol("auto-hscroll-mode");
    public final static ELispSymbol AUTO_RAISE_TAB_BAR_BUTTONS = new ELispSymbol("auto-raise-tab-bar-buttons");
    public final static ELispSymbol AUTO_RAISE_TOOL_BAR_BUTTONS = new ELispSymbol("auto-raise-tool-bar-buttons");
    public final static ELispSymbol AUTO_RESIZE_TAB_BARS = new ELispSymbol("auto-resize-tab-bars");
    public final static ELispSymbol AUTO_RESIZE_TOOL_BARS = new ELispSymbol("auto-resize-tool-bars");
    public final static ELispSymbol BAR = new ELispSymbol("bar");
    public final static ELispSymbol BIDI_INHIBIT_BPA = new ELispSymbol("bidi-inhibit-bpa");
    public final static ELispSymbol BLINK_CURSOR_ALIST = new ELispSymbol("blink-cursor-alist");
    public final static ELispSymbol BOTH = new ELispSymbol("both");
    public final static ELispSymbol BOTH_HORIZ = new ELispSymbol("both-horiz");
    public final static ELispSymbol BOX = new ELispSymbol("box");
    public final static ELispSymbol BUFFER_POSITION = new ELispSymbol("buffer-position");
    public final static ELispSymbol CALIGN_TO = new ELispSymbol(":align-to");
    public final static ELispSymbol CDATA = new ELispSymbol(":data");
    public final static ELispSymbol CENTER = new ELispSymbol("center");
    public final static ELispSymbol CEVAL = new ELispSymbol(":eval");
    public final static ELispSymbol CFILE = new ELispSymbol(":file");
    public final static ELispSymbol CIRCLE = new ELispSymbol("circle");
    public final static ELispSymbol CLEAR_MESSAGE_FUNCTION = new ELispSymbol("clear-message-function");
    public final static ELispSymbol CLOSE_TAB = new ELispSymbol("close-tab");
    public final static ELispSymbol CMAP = new ELispSymbol(":map");
    public final static ELispSymbol COMPOSITION_BREAK_AT_POINT = new ELispSymbol("composition-break-at-point");
    public final static ELispSymbol CPOINTER = new ELispSymbol(":pointer");
    public final static ELispSymbol CPROPERTIZE = new ELispSymbol(":propertize");
    public final static ELispSymbol CRELATIVE_HEIGHT = new ELispSymbol(":relative-height");
    public final static ELispSymbol CRELATIVE_WIDTH = new ELispSymbol(":relative-width");
    public final static ELispSymbol CURRENT_LINE = new ELispSymbol("current-line");
    public final static ELispSymbol DEBUG_EARLY__MUTED = new ELispSymbol("debug-early--muted");
    public final static ELispSymbol DEBUG_END_POS = new ELispSymbol("debug-end-pos");
    public final static ELispSymbol DEBUG_ON_MESSAGE = new ELispSymbol("debug-on-message");
    public final static ELispSymbol DISABLE_EVAL = new ELispSymbol("disable-eval");
    public final static ELispSymbol DISPLAY = new ELispSymbol("display");
    public final static ELispSymbol DISPLAY_FILL_COLUMN_INDICATOR = new ELispSymbol("display-fill-column-indicator");
    public final static ELispSymbol DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER = new ELispSymbol("display-fill-column-indicator-character");
    public final static ELispSymbol DISPLAY_FILL_COLUMN_INDICATOR_COLUMN = new ELispSymbol("display-fill-column-indicator-column");
    public final static ELispSymbol DISPLAY_HOURGLASS = new ELispSymbol("display-hourglass");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS = new ELispSymbol("display-line-numbers");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE = new ELispSymbol("display-line-numbers-current-absolute");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_DISABLE = new ELispSymbol("display-line-numbers-disable");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_MAJOR_TICK = new ELispSymbol("display-line-numbers-major-tick");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_MINOR_TICK = new ELispSymbol("display-line-numbers-minor-tick");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_OFFSET = new ELispSymbol("display-line-numbers-offset");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_WIDEN = new ELispSymbol("display-line-numbers-widen");
    public final static ELispSymbol DISPLAY_LINE_NUMBERS_WIDTH = new ELispSymbol("display-line-numbers-width");
    public final static ELispSymbol DISPLAY_PIXELS_PER_INCH = new ELispSymbol("display-pixels-per-inch");
    public final static ELispSymbol DISPLAY_RAW_BYTES_AS_HEX = new ELispSymbol("display-raw-bytes-as-hex");
    public final static ELispSymbol DONT_CLEAR_MESSAGE = new ELispSymbol("dont-clear-message");
    public final static ELispSymbol DRAGGING = new ELispSymbol("dragging");
    public final static ELispSymbol DRAG_SOURCE = new ELispSymbol("drag-source");
    public final static ELispSymbol DRAG_WITH_HEADER_LINE = new ELispSymbol("drag-with-header-line");
    public final static ELispSymbol DRAG_WITH_MODE_LINE = new ELispSymbol("drag-with-mode-line");
    public final static ELispSymbol DRAG_WITH_TAB_LINE = new ELispSymbol("drag-with-tab-line");
    public final static ELispSymbol DROPPING = new ELispSymbol("dropping");
    public final static ELispSymbol EMPTY_BOX = new ELispSymbol("empty-box");
    public final static ELispSymbol ESCAPE_GLYPH = new ELispSymbol("escape-glyph");
    public final static ELispSymbol EVAL = new ELispSymbol("eval");
    public final static ELispSymbol FILE_REMOTE_P = new ELispSymbol("file-remote-p");
    public final static ELispSymbol FILL_COLUMN_INDICATOR = new ELispSymbol("fill-column-indicator");
    public final static ELispSymbol FONTIFICATION_FUNCTIONS = new ELispSymbol("fontification-functions");
    public final static ELispSymbol FONTIFIED = new ELispSymbol("fontified");
    public final static ELispSymbol FRAME_TITLE_FORMAT = new ELispSymbol("frame-title-format");
    public final static ELispSymbol GLOBAL_MODE_STRING = new ELispSymbol("global-mode-string");
    public final static ELispSymbol GLYPHLESS_CHAR = new ELispSymbol("glyphless-char");
    public final static ELispSymbol GLYPHLESS_CHAR_DISPLAY = new ELispSymbol("glyphless-char-display");
    public final static ELispSymbol GROW_ONLY = new ELispSymbol("grow-only");
    public final static ELispSymbol HAND = new ELispSymbol("hand");
    public final static ELispSymbol HBAR = new ELispSymbol("hbar");
    public final static ELispSymbol HDRAG = new ELispSymbol("hdrag");
    public final static ELispSymbol HEX_CODE = new ELispSymbol("hex-code");
    public final static ELispSymbol HIGHLIGHT_NONSELECTED_WINDOWS = new ELispSymbol("highlight-nonselected-windows");
    public final static ELispSymbol HOLLOW = new ELispSymbol("hollow");
    public final static ELispSymbol HOURGLASS = new ELispSymbol("hourglass");
    public final static ELispSymbol HOURGLASS_DELAY = new ELispSymbol("hourglass-delay");
    public final static ELispSymbol HSCROLL_MARGIN = new ELispSymbol("hscroll-margin");
    public final static ELispSymbol HSCROLL_STEP = new ELispSymbol("hscroll-step");
    public final static ELispSymbol ICON_TITLE_FORMAT = new ELispSymbol("icon-title-format");
    public final static ELispSymbol IMAGE = new ELispSymbol("image");
    public final static ELispSymbol INHIBIT_BIDI_MIRRORING = new ELispSymbol("inhibit-bidi-mirroring");
    public final static ELispSymbol INHIBIT_EVAL_DURING_REDISPLAY = new ELispSymbol("inhibit-eval-during-redisplay");
    public final static ELispSymbol INHIBIT_FREE_REALIZED_FACES = new ELispSymbol("inhibit-free-realized-faces");
    public final static ELispSymbol INHIBIT_MENUBAR_UPDATE = new ELispSymbol("inhibit-menubar-update");
    public final static ELispSymbol INHIBIT_MESSAGE = new ELispSymbol("inhibit-message");
    public final static ELispSymbol INHIBIT_REDISPLAY = new ELispSymbol("inhibit-redisplay");
    public final static ELispSymbol INHIBIT_TRY_CURSOR_MOVEMENT = new ELispSymbol("inhibit-try-cursor-movement");
    public final static ELispSymbol INHIBIT_TRY_WINDOW_ID = new ELispSymbol("inhibit-try-window-id");
    public final static ELispSymbol INHIBIT_TRY_WINDOW_REUSING = new ELispSymbol("inhibit-try-window-reusing");
    public final static ELispSymbol LAST_ARROW_POSITION = new ELispSymbol("last-arrow-position");
    public final static ELispSymbol LAST_ARROW_STRING = new ELispSymbol("last-arrow-string");
    public final static ELispSymbol LEFT_MARGIN = new ELispSymbol("left-margin");
    public final static ELispSymbol LEFT_TO_RIGHT = new ELispSymbol("left-to-right");
    public final static ELispSymbol LINE_HEIGHT = new ELispSymbol("line-height");
    public final static ELispSymbol LINE_NUMBER = new ELispSymbol("line-number");
    public final static ELispSymbol LINE_NUMBER_CURRENT_LINE = new ELispSymbol("line-number-current-line");
    public final static ELispSymbol LINE_NUMBER_DISPLAY_LIMIT = new ELispSymbol("line-number-display-limit");
    public final static ELispSymbol LINE_NUMBER_DISPLAY_LIMIT_WIDTH = new ELispSymbol("line-number-display-limit-width");
    public final static ELispSymbol LINE_NUMBER_MAJOR_TICK = new ELispSymbol("line-number-major-tick");
    public final static ELispSymbol LINE_NUMBER_MINOR_TICK = new ELispSymbol("line-number-minor-tick");
    public final static ELispSymbol LINE_PREFIX = new ELispSymbol("line-prefix");
    public final static ELispSymbol LONG = new ELispSymbol("long");
    public final static ELispSymbol LONG_LINE_OPTIMIZATIONS_IN_FONTIFICATION_FUNCTIONS = new ELispSymbol("long-line-optimizations-in-fontification-functions");
    public final static ELispSymbol MAKE_CURSOR_LINE_FULLY_VISIBLE = new ELispSymbol("make-cursor-line-fully-visible");
    public final static ELispSymbol MAKE_WINDOW_START_VISIBLE = new ELispSymbol("make-window-start-visible");
    public final static ELispSymbol MARGIN = new ELispSymbol("margin");
    public final static ELispSymbol MAXIMUM_SCROLL_MARGIN = new ELispSymbol("maximum-scroll-margin");
    public final static ELispSymbol MAX_MINI_WINDOW_HEIGHT = new ELispSymbol("max-mini-window-height");
    public final static ELispSymbol MAX_REDISPLAY_TICKS = new ELispSymbol("max-redisplay-ticks");
    public final static ELispSymbol MENU_BAR_UPDATE_HOOK = new ELispSymbol("menu-bar-update-hook");
    public final static ELispSymbol MENU_UPDATING_FRAME = new ELispSymbol("menu-updating-frame");
    public final static ELispSymbol MESSAGES_BUFFER_MODE = new ELispSymbol("messages-buffer-mode");
    public final static ELispSymbol MESSAGES_BUFFER_NAME = new ELispSymbol("messages-buffer-name");
    public final static ELispSymbol MESSAGE_LOG_MAX = new ELispSymbol("message-log-max");
    public final static ELispSymbol MESSAGE_TRUNCATE_LINES = new ELispSymbol("message-truncate-lines");
    public final static ELispSymbol MODE_LINE_COMPACT = new ELispSymbol("mode-line-compact");
    public final static ELispSymbol MODE_LINE_DEFAULT_HELP_ECHO = new ELispSymbol("mode-line-default-help-echo");
    public final static ELispSymbol MOUSE_AUTOSELECT_WINDOW = new ELispSymbol("mouse-autoselect-window");
    public final static ELispSymbol MOUSE_FINE_GRAINED_TRACKING = new ELispSymbol("mouse-fine-grained-tracking");
    public final static ELispSymbol MULTIPLE_FRAMES = new ELispSymbol("multiple-frames");
    public final static ELispSymbol NHDRAG = new ELispSymbol("nhdrag");
    public final static ELispSymbol NOBREAK_CHAR_ASCII_DISPLAY = new ELispSymbol("nobreak-char-ascii-display");
    public final static ELispSymbol NOBREAK_CHAR_DISPLAY = new ELispSymbol("nobreak-char-display");
    public final static ELispSymbol NOBREAK_HYPHEN = new ELispSymbol("nobreak-hyphen");
    public final static ELispSymbol NOBREAK_SPACE = new ELispSymbol("nobreak-space");
    public final static ELispSymbol OBJECT = new ELispSymbol("object");
    public final static ELispSymbol OVERLAY_ARROW_BITMAP = new ELispSymbol("overlay-arrow-bitmap");
    public final static ELispSymbol OVERLAY_ARROW_POSITION = new ELispSymbol("overlay-arrow-position");
    public final static ELispSymbol OVERLAY_ARROW_STRING = new ELispSymbol("overlay-arrow-string");
    public final static ELispSymbol OVERLAY_ARROW_VARIABLE_LIST = new ELispSymbol("overlay-arrow-variable-list");
    public final static ELispSymbol OVERLINE_MARGIN = new ELispSymbol("overline-margin");
    public final static ELispSymbol POINTER = new ELispSymbol("pointer");
    public final static ELispSymbol POLY = new ELispSymbol("poly");
    public final static ELispSymbol POSITION = new ELispSymbol("position");
    public final static ELispSymbol PRE_REDISPLAY_FUNCTION = new ELispSymbol("pre-redisplay-function");
    public final static ELispSymbol RAISE = new ELispSymbol("raise");
    public final static ELispSymbol RECT = new ELispSymbol("rect");
    public final static ELispSymbol REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS = new ELispSymbol("redisplay-adhoc-scroll-in-resize-mini-windows");
    public final static ELispSymbol REDISPLAY_INTERNAL_XC_FUNCTIONX = new ELispSymbol("redisplay_internal (C function)");
    public final static ELispSymbol REDISPLAY_SKIP_FONTIFICATION_ON_INPUT = new ELispSymbol("redisplay-skip-fontification-on-input");
    public final static ELispSymbol REDISPLAY_SKIP_INITIAL_FRAME = new ELispSymbol("redisplay-skip-initial-frame");
    public final static ELispSymbol REDISPLAY__ALL_WINDOWS_CAUSE = new ELispSymbol("redisplay--all-windows-cause");
    public final static ELispSymbol REDISPLAY__INHIBIT_BIDI = new ELispSymbol("redisplay--inhibit-bidi");
    public final static ELispSymbol REDISPLAY__MODE_LINES_CAUSE = new ELispSymbol("redisplay--mode-lines-cause");
    public final static ELispSymbol RELATIVE = new ELispSymbol("relative");
    public final static ELispSymbol RESIZE_MINI_WINDOWS = new ELispSymbol("resize-mini-windows");
    public final static ELispSymbol RIGHT_MARGIN = new ELispSymbol("right-margin");
    public final static ELispSymbol RIGHT_TO_LEFT = new ELispSymbol("right-to-left");
    public final static ELispSymbol SCROLL_CONSERVATIVELY = new ELispSymbol("scroll-conservatively");
    public final static ELispSymbol SCROLL_MARGIN = new ELispSymbol("scroll-margin");
    public final static ELispSymbol SCROLL_MINIBUFFER_CONSERVATIVELY = new ELispSymbol("scroll-minibuffer-conservatively");
    public final static ELispSymbol SCROLL_STEP = new ELispSymbol("scroll-step");
    public final static ELispSymbol SET_MESSAGE_FUNCTION = new ELispSymbol("set-message-function");
    public final static ELispSymbol SHOW_TRAILING_WHITESPACE = new ELispSymbol("show-trailing-whitespace");
    public final static ELispSymbol SLICE = new ELispSymbol("slice");
    public final static ELispSymbol SPACE = new ELispSymbol("space");
    public final static ELispSymbol SPACE_WIDTH = new ELispSymbol("space-width");
    public final static ELispSymbol TAB_BAR_BORDER = new ELispSymbol("tab-bar-border");
    public final static ELispSymbol TAB_BAR_BUTTON_MARGIN = new ELispSymbol("tab-bar-button-margin");
    public final static ELispSymbol TAB_BAR_BUTTON_RELIEF = new ELispSymbol("tab-bar-button-relief");
    public final static ELispSymbol TAB_BAR__DRAGGING_IN_PROGRESS = new ELispSymbol("tab-bar--dragging-in-progress");
    public final static ELispSymbol TEXT = new ELispSymbol("text");
    public final static ELispSymbol TEXT_IMAGE_HORIZ = new ELispSymbol("text-image-horiz");
    public final static ELispSymbol THIN_SPACE = new ELispSymbol("thin-space");
    public final static ELispSymbol TOOL_BAR_BORDER = new ELispSymbol("tool-bar-border");
    public final static ELispSymbol TOOL_BAR_BUTTON_MARGIN = new ELispSymbol("tool-bar-button-margin");
    public final static ELispSymbol TOOL_BAR_BUTTON_RELIEF = new ELispSymbol("tool-bar-button-relief");
    public final static ELispSymbol TOOL_BAR_MAX_LABEL_SIZE = new ELispSymbol("tool-bar-max-label-size");
    public final static ELispSymbol TOOL_BAR_STYLE = new ELispSymbol("tool-bar-style");
    public final static ELispSymbol TRAILING_WHITESPACE = new ELispSymbol("trailing-whitespace");
    public final static ELispSymbol TRUNCATE_PARTIAL_WIDTH_WINDOWS = new ELispSymbol("truncate-partial-width-windows");
    public final static ELispSymbol UNDERLINE_MINIMUM_OFFSET = new ELispSymbol("underline-minimum-offset");
    public final static ELispSymbol UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT = new ELispSymbol("unibyte-display-via-language-environment");
    public final static ELispSymbol VDRAG = new ELispSymbol("vdrag");
    public final static ELispSymbol VISUAL = new ELispSymbol("visual");
    public final static ELispSymbol VOID_TEXT_AREA_POINTER = new ELispSymbol("void-text-area-pointer");
    public final static ELispSymbol WINDOW_SCROLL_FUNCTIONS = new ELispSymbol("window-scroll-functions");
    public final static ELispSymbol WORD_WRAP_BY_CATEGORY = new ELispSymbol("word-wrap-by-category");
    public final static ELispSymbol WRAP_PREFIX = new ELispSymbol("wrap-prefix");
    public final static ELispSymbol X_STRETCH_CURSOR = new ELispSymbol("x-stretch-cursor");
    public final static ELispSymbol ZERO_WIDTH = new ELispSymbol("zero-width");
    private ELispSymbol[] xdispSymbols() {
        return new ELispSymbol[]{
                ARROW,
                AUTO_HSCROLL_MODE,
                AUTO_RAISE_TAB_BAR_BUTTONS,
                AUTO_RAISE_TOOL_BAR_BUTTONS,
                AUTO_RESIZE_TAB_BARS,
                AUTO_RESIZE_TOOL_BARS,
                BAR,
                BIDI_INHIBIT_BPA,
                BLINK_CURSOR_ALIST,
                BOTH,
                BOTH_HORIZ,
                BOX,
                BUFFER_POSITION,
                CALIGN_TO,
                CDATA,
                CENTER,
                CEVAL,
                CFILE,
                CIRCLE,
                CLEAR_MESSAGE_FUNCTION,
                CLOSE_TAB,
                CMAP,
                COMPOSITION_BREAK_AT_POINT,
                CPOINTER,
                CPROPERTIZE,
                CRELATIVE_HEIGHT,
                CRELATIVE_WIDTH,
                CURRENT_LINE,
                DEBUG_EARLY__MUTED,
                DEBUG_END_POS,
                DEBUG_ON_MESSAGE,
                DISABLE_EVAL,
                DISPLAY,
                DISPLAY_FILL_COLUMN_INDICATOR,
                DISPLAY_FILL_COLUMN_INDICATOR_CHARACTER,
                DISPLAY_FILL_COLUMN_INDICATOR_COLUMN,
                DISPLAY_HOURGLASS,
                DISPLAY_LINE_NUMBERS,
                DISPLAY_LINE_NUMBERS_CURRENT_ABSOLUTE,
                DISPLAY_LINE_NUMBERS_DISABLE,
                DISPLAY_LINE_NUMBERS_MAJOR_TICK,
                DISPLAY_LINE_NUMBERS_MINOR_TICK,
                DISPLAY_LINE_NUMBERS_OFFSET,
                DISPLAY_LINE_NUMBERS_WIDEN,
                DISPLAY_LINE_NUMBERS_WIDTH,
                DISPLAY_PIXELS_PER_INCH,
                DISPLAY_RAW_BYTES_AS_HEX,
                DONT_CLEAR_MESSAGE,
                DRAGGING,
                DRAG_SOURCE,
                DRAG_WITH_HEADER_LINE,
                DRAG_WITH_MODE_LINE,
                DRAG_WITH_TAB_LINE,
                DROPPING,
                EMPTY_BOX,
                ESCAPE_GLYPH,
                EVAL,
                FILE_REMOTE_P,
                FILL_COLUMN_INDICATOR,
                FONTIFICATION_FUNCTIONS,
                FONTIFIED,
                FRAME_TITLE_FORMAT,
                GLOBAL_MODE_STRING,
                GLYPHLESS_CHAR,
                GLYPHLESS_CHAR_DISPLAY,
                GROW_ONLY,
                HAND,
                HBAR,
                HDRAG,
                HEX_CODE,
                HIGHLIGHT_NONSELECTED_WINDOWS,
                HOLLOW,
                HOURGLASS,
                HOURGLASS_DELAY,
                HSCROLL_MARGIN,
                HSCROLL_STEP,
                ICON_TITLE_FORMAT,
                IMAGE,
                INHIBIT_BIDI_MIRRORING,
                INHIBIT_EVAL_DURING_REDISPLAY,
                INHIBIT_FREE_REALIZED_FACES,
                INHIBIT_MENUBAR_UPDATE,
                INHIBIT_MESSAGE,
                INHIBIT_POINT_MOTION_HOOKS,
                INHIBIT_REDISPLAY,
                INHIBIT_TRY_CURSOR_MOVEMENT,
                INHIBIT_TRY_WINDOW_ID,
                INHIBIT_TRY_WINDOW_REUSING,
                LAST_ARROW_POSITION,
                LAST_ARROW_STRING,
                LEFT_MARGIN,
                LEFT_TO_RIGHT,
                LINE_HEIGHT,
                LINE_NUMBER,
                LINE_NUMBER_CURRENT_LINE,
                LINE_NUMBER_DISPLAY_LIMIT,
                LINE_NUMBER_DISPLAY_LIMIT_WIDTH,
                LINE_NUMBER_MAJOR_TICK,
                LINE_NUMBER_MINOR_TICK,
                LINE_PREFIX,
                LONG,
                LONG_LINE_OPTIMIZATIONS_IN_FONTIFICATION_FUNCTIONS,
                MAKE_CURSOR_LINE_FULLY_VISIBLE,
                MAKE_WINDOW_START_VISIBLE,
                MARGIN,
                MAXIMUM_SCROLL_MARGIN,
                MAX_MINI_WINDOW_HEIGHT,
                MAX_REDISPLAY_TICKS,
                MENU_BAR_UPDATE_HOOK,
                MENU_UPDATING_FRAME,
                MESSAGES_BUFFER_MODE,
                MESSAGES_BUFFER_NAME,
                MESSAGE_LOG_MAX,
                MESSAGE_TRUNCATE_LINES,
                MODE_LINE_COMPACT,
                MODE_LINE_DEFAULT_HELP_ECHO,
                MOUSE_AUTOSELECT_WINDOW,
                MOUSE_FINE_GRAINED_TRACKING,
                MULTIPLE_FRAMES,
                NHDRAG,
                NOBREAK_CHAR_ASCII_DISPLAY,
                NOBREAK_CHAR_DISPLAY,
                NOBREAK_HYPHEN,
                NOBREAK_SPACE,
                OBJECT,
                OVERLAY_ARROW_BITMAP,
                OVERLAY_ARROW_POSITION,
                OVERLAY_ARROW_STRING,
                OVERLAY_ARROW_VARIABLE_LIST,
                OVERLINE_MARGIN,
                OVERRIDING_LOCAL_MAP,
                OVERRIDING_TERMINAL_LOCAL_MAP,
                POINTER,
                POLY,
                POSITION,
                PRE_REDISPLAY_FUNCTION,
                RAISE,
                RECT,
                REDISPLAY_ADHOC_SCROLL_IN_RESIZE_MINI_WINDOWS,
                REDISPLAY_INTERNAL_XC_FUNCTIONX,
                REDISPLAY_SKIP_FONTIFICATION_ON_INPUT,
                REDISPLAY_SKIP_INITIAL_FRAME,
                REDISPLAY__ALL_WINDOWS_CAUSE,
                REDISPLAY__INHIBIT_BIDI,
                REDISPLAY__MODE_LINES_CAUSE,
                RELATIVE,
                RESIZE_MINI_WINDOWS,
                RIGHT_MARGIN,
                RIGHT_TO_LEFT,
                SCROLL_CONSERVATIVELY,
                SCROLL_MARGIN,
                SCROLL_MINIBUFFER_CONSERVATIVELY,
                SCROLL_STEP,
                SET_MESSAGE_FUNCTION,
                SHOW_TRAILING_WHITESPACE,
                SLICE,
                SPACE,
                SPACE_WIDTH,
                TAB_BAR_BORDER,
                TAB_BAR_BUTTON_MARGIN,
                TAB_BAR_BUTTON_RELIEF,
                TAB_BAR__DRAGGING_IN_PROGRESS,
                TEXT,
                TEXT_IMAGE_HORIZ,
                THIN_SPACE,
                TOOL_BAR_BORDER,
                TOOL_BAR_BUTTON_MARGIN,
                TOOL_BAR_BUTTON_RELIEF,
                TOOL_BAR_MAX_LABEL_SIZE,
                TOOL_BAR_STYLE,
                TRAILING_WHITESPACE,
                TRUNCATE_PARTIAL_WIDTH_WINDOWS,
                UNDERLINE_MINIMUM_OFFSET,
                UNIBYTE_DISPLAY_VIA_LANGUAGE_ENVIRONMENT,
                VDRAG,
                VISUAL,
                VOID_TEXT_AREA_POINTER,
                WINDOW_SCROLL_FUNCTIONS,
                WORD_WRAP_BY_CATEGORY,
                WRAP_PREFIX,
                X_STRETCH_CURSOR,
                ZERO_WIDTH,
        };
    }
    //#endregion xdisp.c
    //#region category.c
    public final static ELispSymbol CATEGORYP = new ELispSymbol("categoryp");
    public final static ELispSymbol CATEGORYSETP = new ELispSymbol("categorysetp");
    public final static ELispSymbol CATEGORY_TABLE = new ELispSymbol("category-table");
    public final static ELispSymbol CATEGORY_TABLE_P = new ELispSymbol("category-table-p");
    public final static ELispSymbol WORD_COMBINING_CATEGORIES = new ELispSymbol("word-combining-categories");
    public final static ELispSymbol WORD_SEPARATING_CATEGORIES = new ELispSymbol("word-separating-categories");
    private ELispSymbol[] categorySymbols() {
        return new ELispSymbol[]{
                CATEGORYP,
                CATEGORYSETP,
                CATEGORY_TABLE,
                CATEGORY_TABLE_P,
                WORD_COMBINING_CATEGORIES,
                WORD_SEPARATING_CATEGORIES,
        };
    }
    //#endregion category.c
    //#region doc.c
    public final static ELispSymbol BUILD_FILES = new ELispSymbol("build-files");
    public final static ELispSymbol CURVE = new ELispSymbol("curve");
    public final static ELispSymbol CUSTOM_DELAYED_INIT_VARIABLES = new ELispSymbol("custom-delayed-init-variables");
    public final static ELispSymbol FUNCTION_DOCUMENTATION = new ELispSymbol("function-documentation");
    public final static ELispSymbol GRAVE = new ELispSymbol("grave");
    public final static ELispSymbol INTERNAL_DOC_FILE_NAME = new ELispSymbol("internal-doc-file-name");
    public final static ELispSymbol INTERNAL__TEXT_QUOTING_FLAG = new ELispSymbol("internal--text-quoting-flag");
    public final static ELispSymbol LISP_DIRECTORY = new ELispSymbol("lisp-directory");
    public final static ELispSymbol STRAIGHT = new ELispSymbol("straight");
    public final static ELispSymbol SUBSTITUTE_COMMAND_KEYS = new ELispSymbol("substitute-command-keys");
    public final static ELispSymbol TEXT_QUOTING_STYLE = new ELispSymbol("text-quoting-style");
    private ELispSymbol[] docSymbols() {
        return new ELispSymbol[]{
                BUILD_FILES,
                CURVE,
                CUSTOM_DELAYED_INIT_VARIABLES,
                FUNCTION_DOCUMENTATION,
                GRAVE,
                INTERNAL_DOC_FILE_NAME,
                INTERNAL__TEXT_QUOTING_FLAG,
                LISP_DIRECTORY,
                STRAIGHT,
                SUBSTITUTE_COMMAND_KEYS,
                TEXT_QUOTING_STYLE,
        };
    }
    //#endregion doc.c
    //#region minibuf.c
    public final static ELispSymbol ACTIVATE_INPUT_METHOD = new ELispSymbol("activate-input-method");
    public final static ELispSymbol ADD_TO_HISTORY = new ELispSymbol("add-to-history");
    public final static ELispSymbol BUFFER_NAME_HISTORY = new ELispSymbol("buffer-name-history");
    public final static ELispSymbol COMPLETING_READ_FUNCTION = new ELispSymbol("completing-read-function");
    public final static ELispSymbol COMPLETION_IGNORE_CASE = new ELispSymbol("completion-ignore-case");
    public final static ELispSymbol COMPLETION_REGEXP_LIST = new ELispSymbol("completion-regexp-list");
    public final static ELispSymbol CURRENT_INPUT_METHOD = new ELispSymbol("current-input-method");
    public final static ELispSymbol CUSTOM_VARIABLE_HISTORY = new ELispSymbol("custom-variable-history");
    public final static ELispSymbol CUSTOM_VARIABLE_P = new ELispSymbol("custom-variable-p");
    public final static ELispSymbol CYCLE_SORT_FUNCTION = new ELispSymbol("cycle-sort-function");
    public final static ELispSymbol FORMAT_PROMPT = new ELispSymbol("format-prompt");
    public final static ELispSymbol HISTORY_ADD_NEW_INPUT = new ELispSymbol("history-add-new-input");
    public final static ELispSymbol HISTORY_DELETE_DUPLICATES = new ELispSymbol("history-delete-duplicates");
    public final static ELispSymbol HISTORY_LENGTH = new ELispSymbol("history-length");
    public final static ELispSymbol INHIBIT_INTERACTION = new ELispSymbol("inhibit-interaction");
    public final static ELispSymbol INTERNAL_COMPLETE_BUFFER = new ELispSymbol("internal-complete-buffer");
    public final static ELispSymbol METADATA = new ELispSymbol("metadata");
    public final static ELispSymbol MINIBUFFER_ALLOW_TEXT_PROPERTIES = new ELispSymbol("minibuffer-allow-text-properties");
    public final static ELispSymbol MINIBUFFER_AUTO_RAISE = new ELispSymbol("minibuffer-auto-raise");
    public final static ELispSymbol MINIBUFFER_COMPLETING_FILE_NAME = new ELispSymbol("minibuffer-completing-file-name");
    public final static ELispSymbol MINIBUFFER_COMPLETION_CONFIRM = new ELispSymbol("minibuffer-completion-confirm");
    public final static ELispSymbol MINIBUFFER_COMPLETION_PREDICATE = new ELispSymbol("minibuffer-completion-predicate");
    public final static ELispSymbol MINIBUFFER_COMPLETION_TABLE = new ELispSymbol("minibuffer-completion-table");
    public final static ELispSymbol MINIBUFFER_DEFAULT = new ELispSymbol("minibuffer-default");
    public final static ELispSymbol MINIBUFFER_EXIT = new ELispSymbol("minibuffer-exit");
    public final static ELispSymbol MINIBUFFER_EXIT_HOOK = new ELispSymbol("minibuffer-exit-hook");
    public final static ELispSymbol MINIBUFFER_FOLLOWS_SELECTED_FRAME = new ELispSymbol("minibuffer-follows-selected-frame");
    public final static ELispSymbol MINIBUFFER_HELP_FORM = new ELispSymbol("minibuffer-help-form");
    public final static ELispSymbol MINIBUFFER_HISTORY = new ELispSymbol("minibuffer-history");
    public final static ELispSymbol MINIBUFFER_HISTORY_POSITION = new ELispSymbol("minibuffer-history-position");
    public final static ELispSymbol MINIBUFFER_HISTORY_VARIABLE = new ELispSymbol("minibuffer-history-variable");
    public final static ELispSymbol MINIBUFFER_INACTIVE_MODE = new ELispSymbol("minibuffer-inactive-mode");
    public final static ELispSymbol MINIBUFFER_MODE = new ELispSymbol("minibuffer-mode");
    public final static ELispSymbol MINIBUFFER_PROMPT_PROPERTIES = new ELispSymbol("minibuffer-prompt-properties");
    public final static ELispSymbol MINIBUFFER_QUIT_RECURSIVE_EDIT = new ELispSymbol("minibuffer-quit-recursive-edit");
    public final static ELispSymbol MINIBUFFER_SETUP_HOOK = new ELispSymbol("minibuffer-setup-hook");
    public final static ELispSymbol PUSH_WINDOW_BUFFER_ONTO_PREV = new ELispSymbol("push-window-buffer-onto-prev");
    public final static ELispSymbol READ_BUFFER_COMPLETION_IGNORE_CASE = new ELispSymbol("read-buffer-completion-ignore-case");
    public final static ELispSymbol READ_BUFFER_FUNCTION = new ELispSymbol("read-buffer-function");
    public final static ELispSymbol READ_EXPRESSION_HISTORY = new ELispSymbol("read-expression-history");
    public final static ELispSymbol READ_HIDE_CHAR = new ELispSymbol("read-hide-char");
    public final static ELispSymbol READ_MINIBUFFER_RESTORE_WINDOWS = new ELispSymbol("read-minibuffer-restore-windows");
    public final static ELispSymbol SELECT_FRAME_SET_INPUT_FOCUS = new ELispSymbol("select-frame-set-input-focus");
    private ELispSymbol[] minibufSymbols() {
        return new ELispSymbol[]{
                ACTIVATE_INPUT_METHOD,
                ADD_TO_HISTORY,
                BUFFER_NAME_HISTORY,
                COMPLETING_READ_FUNCTION,
                COMPLETION_IGNORE_CASE,
                COMPLETION_REGEXP_LIST,
                CURRENT_INPUT_METHOD,
                CUSTOM_VARIABLE_HISTORY,
                CUSTOM_VARIABLE_P,
                CYCLE_SORT_FUNCTION,
                ENABLE_RECURSIVE_MINIBUFFERS,
                FORMAT_PROMPT,
                HISTORY_ADD_NEW_INPUT,
                HISTORY_DELETE_DUPLICATES,
                HISTORY_LENGTH,
                INHIBIT_INTERACTION,
                INTERNAL_COMPLETE_BUFFER,
                METADATA,
                MINIBUFFER_ALLOW_TEXT_PROPERTIES,
                MINIBUFFER_AUTO_RAISE,
                MINIBUFFER_COMPLETING_FILE_NAME,
                MINIBUFFER_COMPLETION_CONFIRM,
                MINIBUFFER_COMPLETION_PREDICATE,
                MINIBUFFER_COMPLETION_TABLE,
                MINIBUFFER_DEFAULT,
                MINIBUFFER_EXIT,
                MINIBUFFER_EXIT_HOOK,
                MINIBUFFER_FOLLOWS_SELECTED_FRAME,
                MINIBUFFER_HELP_FORM,
                MINIBUFFER_HISTORY,
                MINIBUFFER_HISTORY_POSITION,
                MINIBUFFER_HISTORY_VARIABLE,
                MINIBUFFER_INACTIVE_MODE,
                MINIBUFFER_MODE,
                MINIBUFFER_PROMPT_PROPERTIES,
                MINIBUFFER_QUIT_RECURSIVE_EDIT,
                MINIBUFFER_SETUP_HOOK,
                PUSH_WINDOW_BUFFER_ONTO_PREV,
                READ_BUFFER_COMPLETION_IGNORE_CASE,
                READ_BUFFER_FUNCTION,
                READ_EXPRESSION_HISTORY,
                READ_HIDE_CHAR,
                READ_MINIBUFFER_RESTORE_WINDOWS,
                SELECT_FRAME_SET_INPUT_FOCUS,
        };
    }
    //#endregion minibuf.c
    //#region callproc.c
    public final static ELispSymbol AFTER_INSERT_FILE_SET_BUFFER_FILE_CODING_SYSTEM = new ELispSymbol("after-insert-file-set-buffer-file-coding-system");
    public final static ELispSymbol CONFIGURE_INFO_DIRECTORY = new ELispSymbol("configure-info-directory");
    public final static ELispSymbol CTAGS_PROGRAM_NAME = new ELispSymbol("ctags-program-name");
    public final static ELispSymbol DATA_DIRECTORY = new ELispSymbol("data-directory");
    public final static ELispSymbol DOC_DIRECTORY = new ELispSymbol("doc-directory");
    public final static ELispSymbol EBROWSE_PROGRAM_NAME = new ELispSymbol("ebrowse-program-name");
    public final static ELispSymbol EMACSCLIENT_PROGRAM_NAME = new ELispSymbol("emacsclient-program-name");
    public final static ELispSymbol ETAGS_PROGRAM_NAME = new ELispSymbol("etags-program-name");
    public final static ELispSymbol EXEC_DIRECTORY = new ELispSymbol("exec-directory");
    public final static ELispSymbol EXEC_PATH = new ELispSymbol("exec-path");
    public final static ELispSymbol EXEC_SUFFIXES = new ELispSymbol("exec-suffixes");
    public final static ELispSymbol HEXL_PROGRAM_NAME = new ELispSymbol("hexl-program-name");
    public final static ELispSymbol INITIAL_ENVIRONMENT = new ELispSymbol("initial-environment");
    public final static ELispSymbol MOVEMAIL_PROGRAM_NAME = new ELispSymbol("movemail-program-name");
    public final static ELispSymbol PROCESS_ENVIRONMENT = new ELispSymbol("process-environment");
    public final static ELispSymbol RCS2LOG_PROGRAM_NAME = new ELispSymbol("rcs2log-program-name");
    public final static ELispSymbol SHARED_GAME_SCORE_DIRECTORY = new ELispSymbol("shared-game-score-directory");
    public final static ELispSymbol SHELL_FILE_NAME = new ELispSymbol("shell-file-name");
    private ELispSymbol[] callprocSymbols() {
        return new ELispSymbol[]{
                AFTER_INSERT_FILE_SET_BUFFER_FILE_CODING_SYSTEM,
                CODING_SYSTEM_FOR_WRITE,
                CONFIGURE_INFO_DIRECTORY,
                CTAGS_PROGRAM_NAME,
                DATA_DIRECTORY,
                DOC_DIRECTORY,
                EBROWSE_PROGRAM_NAME,
                EMACSCLIENT_PROGRAM_NAME,
                ETAGS_PROGRAM_NAME,
                EXEC_DIRECTORY,
                EXEC_PATH,
                EXEC_SUFFIXES,
                HEXL_PROGRAM_NAME,
                INITIAL_ENVIRONMENT,
                MOVEMAIL_PROGRAM_NAME,
                PROCESS_ENVIRONMENT,
                RCS2LOG_PROGRAM_NAME,
                SHARED_GAME_SCORE_DIRECTORY,
                SHELL_FILE_NAME,
        };
    }
    //#endregion callproc.c
    //#region macros.c
    public final static ELispSymbol DEFINING_KBD_MACRO = new ELispSymbol("defining-kbd-macro");
    public final static ELispSymbol EXECUTING_KBD_MACRO = new ELispSymbol("executing-kbd-macro");
    public final static ELispSymbol EXECUTING_KBD_MACRO_INDEX = new ELispSymbol("executing-kbd-macro-index");
    public final static ELispSymbol KBD_MACRO_TERMINATION_HOOK = new ELispSymbol("kbd-macro-termination-hook");
    public final static ELispSymbol LAST_KBD_MACRO = new ELispSymbol("last-kbd-macro");
    private ELispSymbol[] macrosSymbols() {
        return new ELispSymbol[]{
                DEFINING_KBD_MACRO,
                EXECUTING_KBD_MACRO,
                EXECUTING_KBD_MACRO_INDEX,
                KBD_MACRO_TERMINATION_HOOK,
                LAST_KBD_MACRO,
        };
    }
    //#endregion macros.c
}
