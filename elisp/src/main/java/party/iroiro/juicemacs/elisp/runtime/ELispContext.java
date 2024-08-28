package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.frame.VirtualFrame;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
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

    // TODO: Replace this with obarray
    private final static HashMap<String, ELispSymbol> internMap = new HashMap<>();
    public static ELispSymbol intern(String symbol) {
        return internMap.computeIfAbsent(symbol, ELispSymbol::new);
    }
    public static String applyShorthands(String symbol) {
        // TODO: Implementation
        return symbol;
    }

    public static ELispExpressionNode valueToExpression(Object value) {
        return new ELispExpressionNode() {
            @Override
            public Object executeGeneric(VirtualFrame frame) {
                return BuiltInEval.evalSub(value);
            }
        };
    }

    public void registerFunction(String name, ELispValue function) {
        ELispSymbol symbol = intern(name);
        symbol.setFunction(function);
    }

    public void initGlobal(ELispLanguage language) {
        initSymbols(allocSymbols());
        initSymbols(chartabSymbols());
        initSymbols(compSymbols());
        initSymbols(dataSymbols());
        initSymbols(evalSymbols());
        initSymbols(fnsSymbols());
        initSymbols(lreadSymbols());
        initSymbols(processSymbols());

        initBuiltIns(language, new BuiltInAlloc());
        initBuiltIns(language, new BuiltInCharTab());
        initBuiltIns(language, new BuiltInComp());
        initBuiltIns(language, new BuiltInData());
        initBuiltIns(language, new BuiltInEval());
        initBuiltIns(language, new BuiltInFns());
        initBuiltIns(language, new BuiltInLRead());
        initBuiltIns(language, new BuiltInProcess());
    }

    private void initBuiltIns(ELispLanguage language, ELispBuiltIns builtIns) {
        builtIns.initialize(language, this);
    }

    private void initSymbols(ELispSymbol[] symbols) {
        for (ELispSymbol symbol : symbols) {
            internMap.put(symbol.name(), symbol);
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
    private ELispSymbol[] dataSymbols(){
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
    public final static ELispSymbol ASCII_CHARACTER = new ELispSymbol("ascii-character");
    public final static ELispSymbol BACKQUOTE = new ELispSymbol("`");
    public final static ELispSymbol BYTE_RUN_UNESCAPED_CHARACTER_LITERALS_WARNING = new ELispSymbol("byte-run--unescaped-character-literals-warning");
    public final static ELispSymbol CHAR_FROM_NAME = new ELispSymbol("char-from-name");
    public final static ELispSymbol COMMA = new ELispSymbol(",");
    public final static ELispSymbol COMMA_AT = new ELispSymbol(",@");
    public final static ELispSymbol CURRENT_LOAD_LIST = new ELispSymbol("current-load-list");
    public final static ELispSymbol DATA = new ELispSymbol("data");
    public final static ELispSymbol DIR_OK = new ELispSymbol("dir-ok");
    public final static ELispSymbol DO_AFTER_LOAD_EVALUATION = new ELispSymbol("do-after-load-evaluation");
    public final static ELispSymbol EVAL_BUFFER_LIST = new ELispSymbol("eval-buffer-list");
    public final static ELispSymbol FUNCTION = new ELispSymbol("function");
    public final static ELispSymbol GET_EMACS_MULE_FILE_CHAR = new ELispSymbol("get-emacs-mule-file-char");
    public final static ELispSymbol GET_FILE_CHAR = new ELispSymbol("get-file-char");
    public final static ELispSymbol HASH_TABLE = new ELispSymbol("hash-table");
    public final static ELispSymbol INHIBIT_FILE_NAME_OPERATION = new ELispSymbol("inhibit-file-name-operation");
    public final static ELispSymbol INTERNAL_MACROEXPAND_FOR_LOAD = new ELispSymbol("internal-macroexpand-for-load");
    public final static ELispSymbol LEXICAL_BINDING = new ELispSymbol("lexical-binding");
    public final static ELispSymbol LOAD = new ELispSymbol("load");
    public final static ELispSymbol LOAD_FILE_NAME = new ELispSymbol("load-file-name");
    public final static ELispSymbol LOAD_FORCE_DOC_STRINGS = new ELispSymbol("load-force-doc-strings");
    public final static ELispSymbol LOAD_IN_PROGRESS = new ELispSymbol("load-in-progress");
    public final static ELispSymbol LOAD_TRUE_FILE_NAME = new ELispSymbol("load-true-file-name");
    public final static ELispSymbol LREAD_UNESCAPED_CHARACTER_LITERALS = new ELispSymbol("lread--unescaped-character-literals");
    public final static ELispSymbol MACROEXP__DYNVARS = new ELispSymbol("macroexp--dynvars");
    public final static ELispSymbol NIL = new ELispSymbol("nil");
    public final static ELispSymbol OBARRAYP = new ELispSymbol("obarrayp");
    public final static ELispSymbol OBARRAY_CACHE = new ELispSymbol("obarray-cache");
    public final static ELispSymbol PURECOPY = new ELispSymbol("purecopy");
    public final static ELispSymbol READ = new ELispSymbol("read");
    public final static ELispSymbol READ_CHAR = new ELispSymbol("read-char");
    public final static ELispSymbol READ_MINIBUFFER = new ELispSymbol("read-minibuffer");
    public final static ELispSymbol SIZE = new ELispSymbol("size");
    public final static ELispSymbol STANDARD_INPUT = new ELispSymbol("standard-input");
    public final static ELispSymbol T = new ELispSymbol("t");
    public final static ELispSymbol TEST = new ELispSymbol("test");
    public final static ELispSymbol UNBOUND = new ELispSymbol("unbound");
    public final static ELispSymbol VARIABLE_DOCUMENTATION = new ELispSymbol("variable-documentation");
    public final static ELispSymbol WEAKNESS = new ELispSymbol("weakness");
    private ELispSymbol[] lreadSymbols(){
        return new ELispSymbol[]{
                ASCII_CHARACTER,
                BACKQUOTE,
                BYTE_RUN_UNESCAPED_CHARACTER_LITERALS_WARNING,
                CHAR_FROM_NAME,
                COMMA,
                COMMA_AT,
                CURRENT_LOAD_LIST,
                DATA,
                DIR_OK,
                DO_AFTER_LOAD_EVALUATION,
                EVAL_BUFFER_LIST,
                FUNCTION,
                GET_EMACS_MULE_FILE_CHAR,
                GET_FILE_CHAR,
                HASH_TABLE,
                INHIBIT_FILE_NAME_OPERATION,
                INTERNAL_MACROEXPAND_FOR_LOAD,
                LEXICAL_BINDING,
                LOAD,
                LOAD_FILE_NAME,
                LOAD_FORCE_DOC_STRINGS,
                LOAD_IN_PROGRESS,
                LOAD_TRUE_FILE_NAME,
                LREAD_UNESCAPED_CHARACTER_LITERALS,
                MACROEXP__DYNVARS,
                NIL,
                OBARRAYP,
                OBARRAY_CACHE,
                PURECOPY,
                READ,
                READ_CHAR,
                READ_MINIBUFFER,
                SIZE,
                STANDARD_INPUT,
                T,
                TEST,
                UNBOUND,
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
    public final static ELispSymbol COMP_LIBGCCJIT_REPRODUCER = new ELispSymbol("comp-libgccjit-reproducer");
    public final static ELispSymbol COMP_MAYBE_GC_OR_QUIT = new ELispSymbol("comp-maybe-gc-or-quit");
    public final static ELispSymbol COMP_MVAR = new ELispSymbol("comp-mvar");
    public final static ELispSymbol COMP_SANITIZER_ERROR = new ELispSymbol("comp-sanitizer-error");
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
    private ELispSymbol[] compSymbols(){
        return new ELispSymbol[]{
                ADD1,
                ASSUME,
                CALL,
                CALLREF,
                CAR,
                CATCHER,
                CDR,
                COMMENT,
                COMP_LIBGCCJIT_REPRODUCER,
                COMP_MAYBE_GC_OR_QUIT,
                COMP_MVAR,
                COMP_SANITIZER_ERROR,
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
    public final static ELispSymbol EGID = new ELispSymbol("egid");
    public final static ELispSymbol ETIME = new ELispSymbol("etime");
    public final static ELispSymbol EUID = new ELispSymbol("euid");
    public final static ELispSymbol EVEN = new ELispSymbol("even");
    public final static ELispSymbol FAILED = new ELispSymbol("failed");
    public final static ELispSymbol GROUP = new ELispSymbol("group");
    public final static ELispSymbol HW = new ELispSymbol("hw");
    public final static ELispSymbol INTERNAL_DEFAULT_INTERRUPT_PROCESS = new ELispSymbol("internal-default-interrupt-process");
    public final static ELispSymbol INTERNAL_DEFAULT_PROCESS_FILTER = new ELispSymbol("internal-default-process-filter");
    public final static ELispSymbol INTERNAL_DEFAULT_PROCESS_SENTINEL = new ELispSymbol("internal-default-process-sentinel");
    public final static ELispSymbol INTERNAL_DEFAULT_SIGNAL_PROCESS = new ELispSymbol("internal-default-signal-process");
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
    public final static ELispSymbol PROCESS_ATTRIBUTES = new ELispSymbol("process-attributes");
    public final static ELispSymbol PTY = new ELispSymbol("pty");
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
    private ELispSymbol[] processSymbols(){
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
                EGID,
                ETIME,
                EUID,
                EVEN,
                FAILED,
                GROUP,
                HW,
                INTERNAL_DEFAULT_INTERRUPT_PROCESS,
                INTERNAL_DEFAULT_PROCESS_FILTER,
                INTERNAL_DEFAULT_PROCESS_SENTINEL,
                INTERNAL_DEFAULT_SIGNAL_PROCESS,
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
                PROCESS_ATTRIBUTES,
                PTY,
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
    public final static ELispSymbol CDEBUG_ON_EXIT = new ELispSymbol(":debug-on-exit");
    public final static ELispSymbol CDOCUMENTATION = new ELispSymbol(":documentation");
    public final static ELispSymbol COMMANDP = new ELispSymbol("commandp");
    public final static ELispSymbol CSUCCESS = new ELispSymbol(":success");
    public final static ELispSymbol DEBUG = new ELispSymbol("debug");
    public final static ELispSymbol DEBUGGER = new ELispSymbol("debugger");
    public final static ELispSymbol DEBUGGER_MAY_CONTINUE = new ELispSymbol("debugger-may-continue");
    public final static ELispSymbol DEBUG_EARLY = new ELispSymbol("debug-early");
    public final static ELispSymbol DEBUG_EARLY__HANDLER = new ELispSymbol("debug-early--handler");
    public final static ELispSymbol DEFVARALIAS = new ELispSymbol("defvaralias");
    public final static ELispSymbol DISPLAY_WARNING = new ELispSymbol("display-warning");
    public final static ELispSymbol EXIT = new ELispSymbol("exit");
    public final static ELispSymbol FUNCTIONP = new ELispSymbol("functionp");
    public final static ELispSymbol INHIBIT_DEBUGGER = new ELispSymbol("inhibit-debugger");
    public final static ELispSymbol INHIBIT_QUIT = new ELispSymbol("inhibit-quit");
    public final static ELispSymbol INTERACTIVE = new ELispSymbol("interactive");
    public final static ELispSymbol INTERNAL_INTERPRETER_ENVIRONMENT = new ELispSymbol("internal-interpreter-environment");
    public final static ELispSymbol INTERNAL_WHEN_ENTERED_DEBUGGER = new ELispSymbol("internal-when-entered-debugger");
    public final static ELispSymbol LOSING_VALUE = new ELispSymbol("losing-value");
    public final static ELispSymbol MACRO = new ELispSymbol("macro");
    public final static ELispSymbol SETQ = new ELispSymbol("setq");
    private ELispSymbol[] evalSymbols(){
        return new ELispSymbol[]{
                AND_OPTIONAL,
                AND_REST,
                AUTOLOAD,
                CDEBUG_ON_EXIT,
                CDOCUMENTATION,
                COMMANDP,
                CSUCCESS,
                DEBUG,
                DEBUGGER,
                DEBUGGER_MAY_CONTINUE,
                DEBUG_EARLY,
                DEBUG_EARLY__HANDLER,
                DEFVARALIAS,
                DISPLAY_WARNING,
                EXIT,
                FUNCTIONP,
                INHIBIT_DEBUGGER,
                INHIBIT_QUIT,
                INTERACTIVE,
                INTERNAL_INTERPRETER_ENVIRONMENT,
                INTERNAL_WHEN_ENTERED_DEBUGGER,
                LOSING_VALUE,
                MACRO,
                SETQ,
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
    public final static ELispSymbol VALUE = new ELispSymbol("value");
    public final static ELispSymbol VALUELT = new ELispSymbol("value<");
    public final static ELispSymbol WIDGET_TYPE = new ELispSymbol("widget-type");
    public final static ELispSymbol YES_OR_NO_P = new ELispSymbol("yes-or-no-p");
    public final static ELispSymbol YES_OR_NO_P_HISTORY = new ELispSymbol("yes-or-no-p-history");
    public final static ELispSymbol Y_OR_N_P = new ELispSymbol("y-or-n-p");
    private ELispSymbol[] fnsSymbols(){
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
                VALUE,
                VALUELT,
                WIDGET_TYPE,
                YES_OR_NO_P,
                YES_OR_NO_P_HISTORY,
                Y_OR_N_P,
        };
    }
    /* @end region="fns.c" */
    /* @generated region="chartab.c" by="extract-emacs-src.py" */
    public final static ELispSymbol CHAR_CODE_PROPERTY_TABLE = new ELispSymbol("char-code-property-table");
    private ELispSymbol[] chartabSymbols(){
        return new ELispSymbol[]{
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
    public final static ELispSymbol FLOATS = new ELispSymbol("floats");
    public final static ELispSymbol GC_CONS_PERCENTAGE = new ELispSymbol("gc-cons-percentage");
    public final static ELispSymbol GC_CONS_THRESHOLD = new ELispSymbol("gc-cons-threshold");
    public final static ELispSymbol HEAP = new ELispSymbol("heap");
    public final static ELispSymbol INTERVALS = new ELispSymbol("intervals");
    public final static ELispSymbol MEMORY_INFO = new ELispSymbol("memory-info");
    public final static ELispSymbol POST_GC_HOOK = new ELispSymbol("post-gc-hook");
    public final static ELispSymbol STRINGS = new ELispSymbol("strings");
    public final static ELispSymbol STRING_BYTES = new ELispSymbol("string-bytes");
    public final static ELispSymbol SYMBOLS = new ELispSymbol("symbols");
    public final static ELispSymbol VECTORS = new ELispSymbol("vectors");
    public final static ELispSymbol VECTOR_SLOTS = new ELispSymbol("vector-slots");
    private ELispSymbol[] allocSymbols(){
        return new ELispSymbol[]{
                ALLOC,
                AUTOMATIC_GC,
                BUFFERS,
                CEMERGENCY,
                CHAR_TABLE_EXTRA_SLOTS,
                CONSES,
                FLOATS,
                GC_CONS_PERCENTAGE,
                GC_CONS_THRESHOLD,
                HEAP,
                INTERVALS,
                MEMORY_INFO,
                POST_GC_HOOK,
                STRINGS,
                STRING_BYTES,
                SYMBOLS,
                VECTORS,
                VECTOR_SLOTS,
        };
    }
    /* @end region="alloc.c" */
}
