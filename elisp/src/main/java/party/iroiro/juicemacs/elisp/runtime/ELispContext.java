package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;

import java.util.HashMap;

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

    public static ELispBuffer CURRENT_BUFFER = new ELispBuffer();

    // TODO: Replace this with obarray
    private final static HashMap<String, ELispSymbol> internMap = new HashMap<>();

    public static ELispSymbol intern(String symbol) {
        return internMap.computeIfAbsent(symbol, ELispSymbol::new);
    }

    public static String applyShorthands(String symbol) {
        // TODO: Implementation
        return symbol;
    }

    public static ELispExpressionNode valueToExpression(Object[] expressions, boolean lexicalBinding) {
        return ELispInterpretedNode.create(expressions, lexicalBinding);
    }

    public void registerFunction(String name, ELispValue function) {
        ELispSymbol symbol = intern(name);
        symbol.setFunction(function);
    }

    public void initGlobal(ELispLanguage language) {
        initSymbols(allocSymbols());
        initSymbols(bufferSymbols());
        initSymbols(charsetSymbols());
        initSymbols(chartabSymbols());
        initSymbols(compSymbols());
        initSymbols(dataSymbols());
        initSymbols(editfnsSymbols());
        initSymbols(emacsSymbols());
        initSymbols(evalSymbols());
        initSymbols(fileioSymbols());
        initSymbols(fnsSymbols());
        initSymbols(keymapSymbols());
        initSymbols(lreadSymbols());
        initSymbols(printSymbols());
        initSymbols(processSymbols());
        initSymbols(searchSymbols());
        initSymbols(timefnsSymbols());
        initSymbols(xfacesSymbols());

        initBuiltIns(language, new BuiltInAlloc());
        initBuiltIns(language, new BuiltInBuffer());
        initBuiltIns(language, new BuiltInCharSet());
        initBuiltIns(language, new BuiltInCharTab());
        initBuiltIns(language, new BuiltInComp());
        initBuiltIns(language, new BuiltInData());
        initBuiltIns(language, new BuiltInEditFns());
        initBuiltIns(language, new BuiltInEmacs());
        initBuiltIns(language, new BuiltInEval());
        initBuiltIns(language, new BuiltInFileIO());
        initBuiltIns(language, new BuiltInFns());
        initBuiltIns(language, new BuiltInKeymap());
        initBuiltIns(language, new BuiltInLRead());
        initBuiltIns(language, new BuiltInPrint());
        initBuiltIns(language, new BuiltInProcess());
        initBuiltIns(language, new BuiltInSearch());
        initBuiltIns(language, new BuiltInTimeFns());
        initBuiltIns(language, new BuiltInXFaces());

        ELispGlobals.initGlobalVariables();
    }

    private void initBuiltIns(ELispLanguage language, ELispBuiltIns builtIns) {
        builtIns.initialize(language, this);
    }

    private void initSymbols(ELispSymbol[] symbols) {
        for (ELispSymbol symbol : symbols) {
            internMap.put(symbol.name(), symbol);
            symbol.setInterned(ELispSymbol.Interned.INTERNED_IN_INITIAL_OBARRAY);
        }
    }

    /* @generated region="data.c" by="extract-emacs-src.py" */
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
    /* @end region="data.c" */
    /* @generated region="lread.c" by="extract-emacs-src.py" */
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
    /* @end region="lread.c" */
    /* @generated region="comp.c" by="extract-emacs-src.py" */
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
    /* @end region="comp.c" */
    /* @generated region="process.c" by="extract-emacs-src.py" */
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
    /* @end region="process.c" */
    /* @generated region="eval.c" by="extract-emacs-src.py" */
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
    /* @end region="eval.c" */
    /* @generated region="fns.c" by="extract-emacs-src.py" */
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
    /* @end region="fns.c" */
    /* @generated region="chartab.c" by="extract-emacs-src.py" */
    public final static ELispSymbol CHAR_CODE_PROPERTY_ALIST = new ELispSymbol("char-code-property-alist");
    public final static ELispSymbol CHAR_CODE_PROPERTY_TABLE = new ELispSymbol("char-code-property-table");
    private ELispSymbol[] chartabSymbols() {
        return new ELispSymbol[]{
                CHAR_CODE_PROPERTY_ALIST,
                CHAR_CODE_PROPERTY_TABLE,
        };
    }
    /* @end region="chartab.c" */
    /* @generated region="alloc.c" by="extract-emacs-src.py" */
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
    /* @end region="alloc.c" */
    /* @generated region="charset.c" by="extract-emacs-src.py" */
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
    /* @end region="charset.c" */
    /* @generated region="fileio.c" by="extract-emacs-src.py" */
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
    /* @end region="fileio.c" */
    /* @generated region="editfns.c" by="extract-emacs-src.py" */
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
    /* @end region="editfns.c" */
    /* @generated region="emacs.c" by="extract-emacs-src.py" */
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
    /* @end region="emacs.c" */
    /* @generated region="search.c" by="extract-emacs-src.py" */
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
    /* @end region="search.c" */
    /* @generated region="buffer.c" by="extract-emacs-src.py" */
    public final static ELispSymbol AFTER_CHANGE_FUNCTIONS = new ELispSymbol("after-change-functions");
    public final static ELispSymbol AFTER_STRING = new ELispSymbol("after-string");
    public final static ELispSymbol AUTOSAVED = new ELispSymbol("autosaved");
    public final static ELispSymbol BEFORE_CHANGE_FUNCTIONS = new ELispSymbol("before-change-functions");
    public final static ELispSymbol BEFORE_STRING = new ELispSymbol("before-string");
    public final static ELispSymbol BUFFER_FILE_NUMBER = new ELispSymbol("buffer-file-number");
    public final static ELispSymbol BUFFER_LIST_UPDATE_HOOK = new ELispSymbol("buffer-list-update-hook");
    public final static ELispSymbol BUFFER_SAVE_WITHOUT_QUERY = new ELispSymbol("buffer-save-without-query");
    public final static ELispSymbol BUFFER_STALE_FUNCTION = new ELispSymbol("buffer-stale-function");
    public final static ELispSymbol BUFFER_UNDO_LIST = new ELispSymbol("buffer-undo-list");
    public final static ELispSymbol CASE_FOLD_SEARCH = new ELispSymbol("case-fold-search");
    public final static ELispSymbol CHANGE_MAJOR_MODE_HOOK = new ELispSymbol("change-major-mode-hook");
    public final static ELispSymbol CHOICE = new ELispSymbol("choice");
    public final static ELispSymbol CLONE_INDIRECT_BUFFER_HOOK = new ELispSymbol("clone-indirect-buffer-hook");
    public final static ELispSymbol DELETE_AUTO_SAVE_FILES = new ELispSymbol("delete-auto-save-files");
    public final static ELispSymbol DELETE_AUTO_SAVE_FILE_IF_NECESSARY = new ELispSymbol("delete-auto-save-file-if-necessary");
    public final static ELispSymbol EVAPORATE = new ELispSymbol("evaporate");
    public final static ELispSymbol FIRST_CHANGE_HOOK = new ELispSymbol("first-change-hook");
    public final static ELispSymbol FRACTION = new ELispSymbol("fraction");
    public final static ELispSymbol FUNDAMENTAL_MODE = new ELispSymbol("fundamental-mode");
    public final static ELispSymbol GET_FILE_BUFFER = new ELispSymbol("get-file-buffer");
    public final static ELispSymbol GET_SCRATCH_BUFFER_CREATE = new ELispSymbol("get-scratch-buffer-create");
    public final static ELispSymbol HORIZONTAL_SCROLL_BAR = new ELispSymbol("horizontal-scroll-bar");
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
    public final static ELispSymbol LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT = new ELispSymbol("long-line-optimizations-bol-search-limit");
    public final static ELispSymbol LONG_LINE_OPTIMIZATIONS_REGION_SIZE = new ELispSymbol("long-line-optimizations-region-size");
    public final static ELispSymbol LONG_LINE_THRESHOLD = new ELispSymbol("long-line-threshold");
    public final static ELispSymbol MODE_CLASS = new ELispSymbol("mode-class");
    public final static ELispSymbol MODIFICATION_HOOKS = new ELispSymbol("modification-hooks");
    public final static ELispSymbol OVERLAYP = new ELispSymbol("overlayp");
    public final static ELispSymbol OVERWRITE_MODE = new ELispSymbol("overwrite-mode");
    public final static ELispSymbol PERMANENT_LOCAL = new ELispSymbol("permanent-local");
    public final static ELispSymbol PERMANENT_LOCAL_HOOK = new ELispSymbol("permanent-local-hook");
    public final static ELispSymbol PRIORITY = new ELispSymbol("priority");
    public final static ELispSymbol PROTECTED_FIELD = new ELispSymbol("protected-field");
    public final static ELispSymbol RANGE = new ELispSymbol("range");
    public final static ELispSymbol RENAME_AUTO_SAVE_FILE = new ELispSymbol("rename-auto-save-file");
    public final static ELispSymbol RIGHT = new ELispSymbol("right");
    public final static ELispSymbol SET_BUFFER_MULTIBYTE = new ELispSymbol("set-buffer-multibyte");
    public final static ELispSymbol TRANSIENT_MARK_MODE = new ELispSymbol("transient-mark-mode");
    public final static ELispSymbol UNIQUIFY__RENAME_BUFFER_ADVICE = new ELispSymbol("uniquify--rename-buffer-advice");
    public final static ELispSymbol VERTICAL_SCROLL_BAR = new ELispSymbol("vertical-scroll-bar");
    private ELispSymbol[] bufferSymbols() {
        return new ELispSymbol[]{
                AFTER_CHANGE_FUNCTIONS,
                AFTER_STRING,
                AUTOSAVED,
                BEFORE_CHANGE_FUNCTIONS,
                BEFORE_STRING,
                BUFFER_FILE_NUMBER,
                BUFFER_LIST_UPDATE_HOOK,
                BUFFER_SAVE_WITHOUT_QUERY,
                BUFFER_STALE_FUNCTION,
                BUFFER_UNDO_LIST,
                CASE_FOLD_SEARCH,
                CHANGE_MAJOR_MODE_HOOK,
                CHOICE,
                CLONE_INDIRECT_BUFFER_HOOK,
                DELETE_AUTO_SAVE_FILES,
                DELETE_AUTO_SAVE_FILE_IF_NECESSARY,
                EVAPORATE,
                FIRST_CHANGE_HOOK,
                FRACTION,
                FUNDAMENTAL_MODE,
                GET_FILE_BUFFER,
                GET_SCRATCH_BUFFER_CREATE,
                HORIZONTAL_SCROLL_BAR,
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
                LONG_LINE_OPTIMIZATIONS_BOL_SEARCH_LIMIT,
                LONG_LINE_OPTIMIZATIONS_REGION_SIZE,
                LONG_LINE_THRESHOLD,
                MODE_CLASS,
                MODIFICATION_HOOKS,
                OVERLAYP,
                OVERWRITE_MODE,
                PERMANENT_LOCAL,
                PERMANENT_LOCAL_HOOK,
                PRIORITY,
                PROTECTED_FIELD,
                RANGE,
                RENAME_AUTO_SAVE_FILE,
                RIGHT,
                SET_BUFFER_MULTIBYTE,
                TRANSIENT_MARK_MODE,
                UNIQUIFY__RENAME_BUFFER_ADVICE,
                VERTICAL_SCROLL_BAR,
        };
    }
    /* @end region="buffer.c" */
    /* @generated region="keymap.c" by="extract-emacs-src.py" */
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
    /* @end region="keymap.c" */
    /* @generated region="print.c" by="extract-emacs-src.py" */
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
    /* @end region="print.c" */
    /* @generated region="xfaces.c" by="extract-emacs-src.py" */
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
    public final static ELispSymbol CREVERSE_VIDEO = new ELispSymbol(":reverse-video");
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
                CREVERSE_VIDEO,
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
    /* @end region="xfaces.c" */
    /* @generated region="timefns.c" by="extract-emacs-src.py" */
    public final static ELispSymbol CURRENT_TIME_LIST = new ELispSymbol("current-time-list");
    public final static ELispSymbol ENCODE_TIME = new ELispSymbol("encode-time");
    private ELispSymbol[] timefnsSymbols() {
        return new ELispSymbol[]{
                CURRENT_TIME_LIST,
                ENCODE_TIME,
        };
    }
    /* @end region="timefns.c" */
}
