package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispFixNumLiteralNode;
import party.iroiro.juicemacs.elisp.nodes.ELispFloatLiteralNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;

import java.util.HashMap;

public class ELispContext {

    private final static ContextReference<ELispContext> REFERENCE = ContextReference.create(ELispLanguage.class);

    public static ELispContext get(Node node) {
        return REFERENCE.get(node);
    }

    // TODO: Replace this with obarray
    private final HashMap<String, ELispSymbol> internMap = new HashMap<>();

    public Object intern(String symbol) {
        return switch (symbol) {
            case "nil" -> Boolean.FALSE;
            case "t" -> Boolean.TRUE; // TODO: Emacs allows (defun t () ...)
            default -> internMap.computeIfAbsent(symbol, ELispSymbol::new);
        };
    }

    public String applyShorthands(String symbol) {
        // TODO: Implementation
        return symbol;
    }

    public ELispExpressionNode valueToExpression(Object value) {
        return switch (value) {
            case Long l -> new ELispFixNumLiteralNode(l);
            case Double d -> new ELispFloatLiteralNode(d);
            case ELispValue v -> v.eval(this);
            default -> throw new IllegalArgumentException();
        };
    }

    public void registerFunction(String name, ELispValue function) {
        ELispSymbol symbol = name.equals("t") ? T : (ELispSymbol) intern(name);
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

    public boolean isT(Object o) {
        return o == T || (o instanceof Boolean b && b);
    }

    /* @generated region="data.c" by="extract-emacs-src.py" */
    public final ELispSymbol AREF = new ELispSymbol("aref");
    public final ELispSymbol ARGS_OUT_OF_RANGE = new ELispSymbol("args-out-of-range");
    public final ELispSymbol ARITH_ERROR = new ELispSymbol("arith-error");
    public final ELispSymbol ARRAYP = new ELispSymbol("arrayp");
    public final ELispSymbol ASET = new ELispSymbol("aset");
    public final ELispSymbol BARE_SYMBOL_P = new ELispSymbol("bare-symbol-p");
    public final ELispSymbol BEGINNING_OF_BUFFER = new ELispSymbol("beginning-of-buffer");
    public final ELispSymbol BIGNUM = new ELispSymbol("bignum");
    public final ELispSymbol BOOLEAN = new ELispSymbol("boolean");
    public final ELispSymbol BOOLEANP = new ELispSymbol("booleanp");
    public final ELispSymbol BOOL_VECTOR = new ELispSymbol("bool-vector");
    public final ELispSymbol BOOL_VECTOR_P = new ELispSymbol("bool-vector-p");
    public final ELispSymbol BUFFER = new ELispSymbol("buffer");
    public final ELispSymbol BUFFERP = new ELispSymbol("bufferp");
    public final ELispSymbol BUFFER_OR_STRING_P = new ELispSymbol("buffer-or-string-p");
    public final ELispSymbol BUFFER_READ_ONLY = new ELispSymbol("buffer-read-only");
    public final ELispSymbol BYTE_CODE_FUNCTION = new ELispSymbol("byte-code-function");
    public final ELispSymbol BYTE_CODE_FUNCTION_P = new ELispSymbol("byte-code-function-p");
    public final ELispSymbol CAR = new ELispSymbol("car");
    public final ELispSymbol CDR = new ELispSymbol("cdr");
    public final ELispSymbol CHAR_OR_STRING_P = new ELispSymbol("char-or-string-p");
    public final ELispSymbol CHAR_TABLE = new ELispSymbol("char-table");
    public final ELispSymbol CHAR_TABLE_P = new ELispSymbol("char-table-p");
    public final ELispSymbol CIRCULAR_LIST = new ELispSymbol("circular-list");
    public final ELispSymbol COMMAND_MODES = new ELispSymbol("command-modes");
    public final ELispSymbol CONDITION_VARIABLE = new ELispSymbol("condition-variable");
    public final ELispSymbol CONS = new ELispSymbol("cons");
    public final ELispSymbol CONSP = new ELispSymbol("consp");
    public final ELispSymbol CYCLIC_FUNCTION_INDIRECTION = new ELispSymbol("cyclic-function-indirection");
    public final ELispSymbol CYCLIC_VARIABLE_INDIRECTION = new ELispSymbol("cyclic-variable-indirection");
    public final ELispSymbol DEFALIAS_FSET_FUNCTION = new ELispSymbol("defalias-fset-function");
    public final ELispSymbol DEFUN = new ELispSymbol("defun");
    public final ELispSymbol DOMAIN_ERROR = new ELispSymbol("domain-error");
    public final ELispSymbol ELT = new ELispSymbol("elt");
    public final ELispSymbol END_OF_BUFFER = new ELispSymbol("end-of-buffer");
    public final ELispSymbol END_OF_FILE = new ELispSymbol("end-of-file");
    public final ELispSymbol ERROR = new ELispSymbol("error");
    public final ELispSymbol ERROR_CONDITIONS = new ELispSymbol("error-conditions");
    public final ELispSymbol ERROR_MESSAGE = new ELispSymbol("error-message");
    public final ELispSymbol EXCESSIVE_LISP_NESTING = new ELispSymbol("excessive-lisp-nesting");
    public final ELispSymbol EXCESSIVE_VARIABLE_BINDING = new ELispSymbol("excessive-variable-binding");
    public final ELispSymbol FBOUNDP = new ELispSymbol("fboundp");
    public final ELispSymbol FINALIZER = new ELispSymbol("finalizer");
    public final ELispSymbol FIXNUMP = new ELispSymbol("fixnump");
    public final ELispSymbol FIXNUM_OR_SYMBOL_WITH_POS_P = new ELispSymbol("fixnum-or-symbol-with-pos-p");
    public final ELispSymbol FLOAT = new ELispSymbol("float");
    public final ELispSymbol FLOATP = new ELispSymbol("floatp");
    public final ELispSymbol FONT_ENTITY = new ELispSymbol("font-entity");
    public final ELispSymbol FONT_OBJECT = new ELispSymbol("font-object");
    public final ELispSymbol FONT_SPEC = new ELispSymbol("font-spec");
    public final ELispSymbol FRAME = new ELispSymbol("frame");
    public final ELispSymbol FUNCTION_HISTORY = new ELispSymbol("function-history");
    public final ELispSymbol HASH_TABLE = new ELispSymbol("hash-table");
    public final ELispSymbol INHIBITED_INTERACTION = new ELispSymbol("inhibited-interaction");
    public final ELispSymbol INTEGER = new ELispSymbol("integer");
    public final ELispSymbol INTEGERP = new ELispSymbol("integerp");
    public final ELispSymbol INTEGER_OR_MARKER_P = new ELispSymbol("integer-or-marker-p");
    public final ELispSymbol INTERACTIVE_FORM = new ELispSymbol("interactive-form");
    public final ELispSymbol INTERPRETED_FUNCTION = new ELispSymbol("interpreted-function");
    public final ELispSymbol INVALID_FUNCTION = new ELispSymbol("invalid-function");
    public final ELispSymbol INVALID_READ_SYNTAX = new ELispSymbol("invalid-read-syntax");
    public final ELispSymbol LAMBDA = new ELispSymbol("lambda");
    public final ELispSymbol LISTP = new ELispSymbol("listp");
    public final ELispSymbol MAKUNBOUND = new ELispSymbol("makunbound");
    public final ELispSymbol MANY = new ELispSymbol("many");
    public final ELispSymbol MARKER = new ELispSymbol("marker");
    public final ELispSymbol MARKERP = new ELispSymbol("markerp");
    public final ELispSymbol MARK_INACTIVE = new ELispSymbol("mark-inactive");
    public final ELispSymbol MINIBUFFER_QUIT = new ELispSymbol("minibuffer-quit");
    public final ELispSymbol MODULE_FUNCTION = new ELispSymbol("module-function");
    public final ELispSymbol MUTEX = new ELispSymbol("mutex");
    public final ELispSymbol NATIVE_COMP_FUNCTION = new ELispSymbol("native-comp-function");
    public final ELispSymbol NATIVE_COMP_UNIT = new ELispSymbol("native-comp-unit");
    public final ELispSymbol NATNUMP = new ELispSymbol("natnump");
    public final ELispSymbol NO_CATCH = new ELispSymbol("no-catch");
    public final ELispSymbol NTH = new ELispSymbol("nth");
    public final ELispSymbol NUMBERP = new ELispSymbol("numberp");
    public final ELispSymbol NUMBER_OR_MARKER_P = new ELispSymbol("number-or-marker-p");
    public final ELispSymbol OBARRAY = new ELispSymbol("obarray");
    public final ELispSymbol OCLOSURE_INTERACTIVE_FORM = new ELispSymbol("oclosure-interactive-form");
    public final ELispSymbol OVERFLOW_ERROR = new ELispSymbol("overflow-error");
    public final ELispSymbol OVERLAY = new ELispSymbol("overlay");
    public final ELispSymbol PRIMITIVE_FUNCTION = new ELispSymbol("primitive-function");
    public final ELispSymbol PROCESS = new ELispSymbol("process");
    public final ELispSymbol QUIT = new ELispSymbol("quit");
    public final ELispSymbol QUOTE = new ELispSymbol("quote");
    public final ELispSymbol RANGE_ERROR = new ELispSymbol("range-error");
    public final ELispSymbol RECORD = new ELispSymbol("record");
    public final ELispSymbol RECORDP = new ELispSymbol("recordp");
    public final ELispSymbol RECURSION_ERROR = new ELispSymbol("recursion-error");
    public final ELispSymbol SEQUENCEP = new ELispSymbol("sequencep");
    public final ELispSymbol SET = new ELispSymbol("set");
    public final ELispSymbol SETCAR = new ELispSymbol("setcar");
    public final ELispSymbol SETCDR = new ELispSymbol("setcdr");
    public final ELispSymbol SETTING_CONSTANT = new ELispSymbol("setting-constant");
    public final ELispSymbol SET_DEFAULT = new ELispSymbol("set-default");
    public final ELispSymbol SINGULARITY_ERROR = new ELispSymbol("singularity-error");
    public final ELispSymbol SPECIAL_FORM = new ELispSymbol("special-form");
    public final ELispSymbol STRING = new ELispSymbol("string");
    public final ELispSymbol STRINGP = new ELispSymbol("stringp");
    public final ELispSymbol SUBR = new ELispSymbol("subr");
    public final ELispSymbol SUBRP = new ELispSymbol("subrp");
    public final ELispSymbol SUBR_NATIVE_ELISP = new ELispSymbol("subr-native-elisp");
    public final ELispSymbol SUB_CHAR_TABLE = new ELispSymbol("sub-char-table");
    public final ELispSymbol SYMBOL = new ELispSymbol("symbol");
    public final ELispSymbol SYMBOLP = new ELispSymbol("symbolp");
    public final ELispSymbol SYMBOLS_WITH_POS_ENABLED = new ELispSymbol("symbols-with-pos-enabled");
    public final ELispSymbol SYMBOL_WITH_POS = new ELispSymbol("symbol-with-pos");
    public final ELispSymbol SYMBOL_WITH_POS_P = new ELispSymbol("symbol-with-pos-p");
    public final ELispSymbol TERMINAL = new ELispSymbol("terminal");
    public final ELispSymbol TEXT_READ_ONLY = new ELispSymbol("text-read-only");
    public final ELispSymbol THREAD = new ELispSymbol("thread");
    public final ELispSymbol TOP_LEVEL = new ELispSymbol("top-level");
    public final ELispSymbol TRAPPING_CONSTANT = new ELispSymbol("trapping-constant");
    public final ELispSymbol TREESIT_COMPILED_QUERY = new ELispSymbol("treesit-compiled-query");
    public final ELispSymbol TREESIT_NODE = new ELispSymbol("treesit-node");
    public final ELispSymbol TREESIT_PARSER = new ELispSymbol("treesit-parser");
    public final ELispSymbol TYPE_MISMATCH = new ELispSymbol("type-mismatch");
    public final ELispSymbol UNDERFLOW_ERROR = new ELispSymbol("underflow-error");
    public final ELispSymbol UNEVALLED = new ELispSymbol("unevalled");
    public final ELispSymbol UNLET = new ELispSymbol("unlet");
    public final ELispSymbol USER_ERROR = new ELispSymbol("user-error");
    public final ELispSymbol USER_PTR = new ELispSymbol("user-ptr");
    public final ELispSymbol USER_PTRP = new ELispSymbol("user-ptrp");
    public final ELispSymbol VECTOR = new ELispSymbol("vector");
    public final ELispSymbol VECTORP = new ELispSymbol("vectorp");
    public final ELispSymbol VECTOR_OR_CHAR_TABLE_P = new ELispSymbol("vector-or-char-table-p");
    public final ELispSymbol VOID_FUNCTION = new ELispSymbol("void-function");
    public final ELispSymbol VOID_VARIABLE = new ELispSymbol("void-variable");
    public final ELispSymbol WATCHERS = new ELispSymbol("watchers");
    public final ELispSymbol WHOLENUMP = new ELispSymbol("wholenump");
    public final ELispSymbol WINDOW = new ELispSymbol("window");
    public final ELispSymbol WINDOW_CONFIGURATION = new ELispSymbol("window-configuration");
    public final ELispSymbol WRONG_LENGTH_ARGUMENT = new ELispSymbol("wrong-length-argument");
    public final ELispSymbol WRONG_NUMBER_OF_ARGUMENTS = new ELispSymbol("wrong-number-of-arguments");
    public final ELispSymbol WRONG_TYPE_ARGUMENT = new ELispSymbol("wrong-type-argument");
    public final ELispSymbol XWIDGET = new ELispSymbol("xwidget");
    public final ELispSymbol XWIDGET_VIEW = new ELispSymbol("xwidget-view");
    private ELispSymbol[] dataSymbols() {
        return new ELispSymbol[] {
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
    public final ELispSymbol ASCII_CHARACTER = new ELispSymbol("ascii-character");
    public final ELispSymbol BACKQUOTE = new ELispSymbol("`");
    public final ELispSymbol BYTE_RUN_UNESCAPED_CHARACTER_LITERALS_WARNING = new ELispSymbol("byte-run--unescaped-character-literals-warning");
    public final ELispSymbol CHAR_FROM_NAME = new ELispSymbol("char-from-name");
    public final ELispSymbol COMMA = new ELispSymbol(",");
    public final ELispSymbol COMMA_AT = new ELispSymbol(",@");
    public final ELispSymbol CURRENT_LOAD_LIST = new ELispSymbol("current-load-list");
    public final ELispSymbol DATA = new ELispSymbol("data");
    public final ELispSymbol DIR_OK = new ELispSymbol("dir-ok");
    public final ELispSymbol DO_AFTER_LOAD_EVALUATION = new ELispSymbol("do-after-load-evaluation");
    public final ELispSymbol EVAL_BUFFER_LIST = new ELispSymbol("eval-buffer-list");
    public final ELispSymbol FUNCTION = new ELispSymbol("function");
    public final ELispSymbol GET_EMACS_MULE_FILE_CHAR = new ELispSymbol("get-emacs-mule-file-char");
    public final ELispSymbol GET_FILE_CHAR = new ELispSymbol("get-file-char");
    public final ELispSymbol INHIBIT_FILE_NAME_OPERATION = new ELispSymbol("inhibit-file-name-operation");
    public final ELispSymbol INTERNAL_MACROEXPAND_FOR_LOAD = new ELispSymbol("internal-macroexpand-for-load");
    public final ELispSymbol LEXICAL_BINDING = new ELispSymbol("lexical-binding");
    public final ELispSymbol LOAD = new ELispSymbol("load");
    public final ELispSymbol LOAD_FILE_NAME = new ELispSymbol("load-file-name");
    public final ELispSymbol LOAD_FORCE_DOC_STRINGS = new ELispSymbol("load-force-doc-strings");
    public final ELispSymbol LOAD_IN_PROGRESS = new ELispSymbol("load-in-progress");
    public final ELispSymbol LOAD_TRUE_FILE_NAME = new ELispSymbol("load-true-file-name");
    public final ELispSymbol LREAD_UNESCAPED_CHARACTER_LITERALS = new ELispSymbol("lread--unescaped-character-literals");
    public final ELispSymbol MACROEXP__DYNVARS = new ELispSymbol("macroexp--dynvars");
    /**
     * Special symbol: {@code t / nil} mapped to {@code true / false}.
     */
    private final ELispSymbol NIL = new ELispSymbol("nil");
    public final ELispSymbol OBARRAYP = new ELispSymbol("obarrayp");
    public final ELispSymbol OBARRAY_CACHE = new ELispSymbol("obarray-cache");
    public final ELispSymbol PURECOPY = new ELispSymbol("purecopy");
    public final ELispSymbol READ = new ELispSymbol("read");
    public final ELispSymbol READ_CHAR = new ELispSymbol("read-char");
    public final ELispSymbol READ_MINIBUFFER = new ELispSymbol("read-minibuffer");
    public final ELispSymbol SIZE = new ELispSymbol("size");
    public final ELispSymbol STANDARD_INPUT = new ELispSymbol("standard-input");
    /**
     * Special symbol: {@code t / nil} mapped to {@code true / false}.
     */
    private final ELispSymbol T = new ELispSymbol("t");
    public final ELispSymbol TEST = new ELispSymbol("test");
    public final ELispSymbol UNBOUND = new ELispSymbol("unbound");
    public final ELispSymbol VARIABLE_DOCUMENTATION = new ELispSymbol("variable-documentation");
    public final ELispSymbol WEAKNESS = new ELispSymbol("weakness");
    private ELispSymbol[] lreadSymbols() {
        return new ELispSymbol[] {
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
    public final ELispSymbol ADD1 = new ELispSymbol("1+");
    public final ELispSymbol ASSUME = new ELispSymbol("assume");
    public final ELispSymbol CALL = new ELispSymbol("call");
    public final ELispSymbol CALLREF = new ELispSymbol("callref");
    public final ELispSymbol CATCHER = new ELispSymbol("catcher");
    public final ELispSymbol COMMENT = new ELispSymbol("comment");
    public final ELispSymbol COMP_LIBGCCJIT_REPRODUCER = new ELispSymbol("comp-libgccjit-reproducer");
    public final ELispSymbol COMP_MAYBE_GC_OR_QUIT = new ELispSymbol("comp-maybe-gc-or-quit");
    public final ELispSymbol COMP_MVAR = new ELispSymbol("comp-mvar");
    public final ELispSymbol COMP_SANITIZER_ERROR = new ELispSymbol("comp-sanitizer-error");
    public final ELispSymbol COMP_SUBR_TRAMPOLINE_INSTALL = new ELispSymbol("comp-subr-trampoline-install");
    public final ELispSymbol CONDITION_CASE = new ELispSymbol("condition-case");
    public final ELispSymbol COND_JUMP = new ELispSymbol("cond-jump");
    public final ELispSymbol COND_JUMP_NARG_LEQ = new ELispSymbol("cond-jump-narg-leq");
    public final ELispSymbol DIRECT_CALL = new ELispSymbol("direct-call");
    public final ELispSymbol DIRECT_CALLREF = new ELispSymbol("direct-callref");
    public final ELispSymbol D_DEFAULT = new ELispSymbol("d-default");
    public final ELispSymbol D_EPHEMERAL = new ELispSymbol("d-ephemeral");
    public final ELispSymbol D_IMPURE = new ELispSymbol("d-impure");
    public final ELispSymbol ENTRY = new ELispSymbol("entry");
    public final ELispSymbol FETCH_HANDLER = new ELispSymbol("fetch-handler");
    public final ELispSymbol FIXNUM = new ELispSymbol("fixnum");
    public final ELispSymbol GCCJIT = new ELispSymbol("gccjit");
    public final ELispSymbol HELPER_SANITIZER_ASSERT = new ELispSymbol("helper_sanitizer_assert");
    public final ELispSymbol HELPER_SAVE_RESTRICTION = new ELispSymbol("helper_save_restriction");
    public final ELispSymbol HELPER_UNBIND_N = new ELispSymbol("helper_unbind_n");
    public final ELispSymbol HELPER_UNWIND_PROTECT = new ELispSymbol("helper_unwind_protect");
    public final ELispSymbol INC_ARGS = new ELispSymbol("inc-args");
    public final ELispSymbol JUMP = new ELispSymbol("jump");
    public final ELispSymbol LAMBDA_FIXUP = new ELispSymbol("lambda-fixup");
    public final ELispSymbol LATE = new ELispSymbol("late");
    public final ELispSymbol NATIVE_COMPILER = new ELispSymbol("native-compiler");
    public final ELispSymbol NATIVE_COMPILER_ERROR = new ELispSymbol("native-compiler-error");
    public final ELispSymbol NATIVE_COMP_COMPILER_OPTIONS = new ELispSymbol("native-comp-compiler-options");
    public final ELispSymbol NATIVE_COMP_DEBUG = new ELispSymbol("native-comp-debug");
    public final ELispSymbol NATIVE_COMP_DRIVER_OPTIONS = new ELispSymbol("native-comp-driver-options");
    public final ELispSymbol NATIVE_COMP_SPEED = new ELispSymbol("native-comp-speed");
    public final ELispSymbol NATIVE_COMP_WARNING_ON_MISSING_SOURCE = new ELispSymbol("native-comp-warning-on-missing-source");
    public final ELispSymbol NATIVE_ICE = new ELispSymbol("native-ice");
    public final ELispSymbol NATIVE_LISP_FILE_INCONSISTENT = new ELispSymbol("native-lisp-file-inconsistent");
    public final ELispSymbol NATIVE_LISP_LOAD_FAILED = new ELispSymbol("native-lisp-load-failed");
    public final ELispSymbol NATIVE_LISP_WRONG_RELOC = new ELispSymbol("native-lisp-wrong-reloc");
    public final ELispSymbol NATIVE__COMPILE_ASYNC = new ELispSymbol("native--compile-async");
    public final ELispSymbol NEGATE = new ELispSymbol("negate");
    public final ELispSymbol PHI = new ELispSymbol("phi");
    public final ELispSymbol POP_HANDLER = new ELispSymbol("pop-handler");
    public final ELispSymbol PUSH_HANDLER = new ELispSymbol("push-handler");
    public final ELispSymbol RECORD_UNWIND_CURRENT_BUFFER = new ELispSymbol("record_unwind_current_buffer");
    public final ELispSymbol RECORD_UNWIND_PROTECT_EXCURSION = new ELispSymbol("record_unwind_protect_excursion");
    public final ELispSymbol RETURN = new ELispSymbol("return");
    public final ELispSymbol SCRATCH = new ELispSymbol("scratch");
    public final ELispSymbol SETIMM = new ELispSymbol("setimm");
    public final ELispSymbol SET_ARGS_TO_LOCAL = new ELispSymbol("set-args-to-local");
    public final ELispSymbol SET_INTERNAL = new ELispSymbol("set_internal");
    public final ELispSymbol SET_PAR_TO_LOCAL = new ELispSymbol("set-par-to-local");
    public final ELispSymbol SET_REST_ARGS_TO_LOCAL = new ELispSymbol("set-rest-args-to-local");
    public final ELispSymbol SUB1 = new ELispSymbol("1-");
    public final ELispSymbol UNREACHABLE = new ELispSymbol("unreachable");
    public final ELispSymbol WRONG_REGISTER_SUBR_CALL = new ELispSymbol("wrong-register-subr-call");
    private ELispSymbol[] compSymbols() {
        return new ELispSymbol[] {
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
    public final ELispSymbol ALL = new ELispSymbol("all");
    public final ELispSymbol ARGS = new ELispSymbol("args");
    public final ELispSymbol CBUFFER = new ELispSymbol(":buffer");
    public final ELispSymbol CBYTESIZE = new ELispSymbol(":bytesize");
    public final ELispSymbol CCODING = new ELispSymbol(":coding");
    public final ELispSymbol CCOMMAND = new ELispSymbol(":command");
    public final ELispSymbol CCONNECTION_TYPE = new ELispSymbol(":connection-type");
    public final ELispSymbol CFILE_HANDLER = new ELispSymbol(":file-handler");
    public final ELispSymbol CFLOWCONTROL = new ELispSymbol(":flowcontrol");
    public final ELispSymbol CHOST = new ELispSymbol(":host");
    public final ELispSymbol CLOCAL = new ELispSymbol(":local");
    public final ELispSymbol CLOG = new ELispSymbol(":log");
    public final ELispSymbol CLOSED = new ELispSymbol("closed");
    public final ELispSymbol CMAJFLT = new ELispSymbol("cmajflt");
    public final ELispSymbol CMINFLT = new ELispSymbol("cminflt");
    public final ELispSymbol CNAME = new ELispSymbol(":name");
    public final ELispSymbol CNOQUERY = new ELispSymbol(":noquery");
    public final ELispSymbol CNOWAIT = new ELispSymbol(":nowait");
    public final ELispSymbol COMM = new ELispSymbol("comm");
    public final ELispSymbol CONNECT = new ELispSymbol("connect");
    public final ELispSymbol CPARITY = new ELispSymbol(":parity");
    public final ELispSymbol CPLIST = new ELispSymbol(":plist");
    public final ELispSymbol CPORT = new ELispSymbol(":port");
    public final ELispSymbol CPROCESS = new ELispSymbol(":process");
    public final ELispSymbol CREMOTE = new ELispSymbol(":remote");
    public final ELispSymbol CSENTINEL = new ELispSymbol(":sentinel");
    public final ELispSymbol CSERVER = new ELispSymbol(":server");
    public final ELispSymbol CSERVICE = new ELispSymbol(":service");
    public final ELispSymbol CSPEED = new ELispSymbol(":speed");
    public final ELispSymbol CSTDERR = new ELispSymbol(":stderr");
    public final ELispSymbol CSTIME = new ELispSymbol("cstime");
    public final ELispSymbol CSTOP = new ELispSymbol(":stop");
    public final ELispSymbol CSTOPBITS = new ELispSymbol(":stopbits");
    public final ELispSymbol CSUMMARY = new ELispSymbol(":summary");
    public final ELispSymbol CTIME = new ELispSymbol("ctime");
    public final ELispSymbol CTLS_PARAMETERS = new ELispSymbol(":tls-parameters");
    public final ELispSymbol CTYPE = new ELispSymbol(":type");
    public final ELispSymbol CURRENT = new ELispSymbol("current");
    public final ELispSymbol CUSE_EXTERNAL_SOCKET = new ELispSymbol(":use-external-socket");
    public final ELispSymbol CUTIME = new ELispSymbol("cutime");
    public final ELispSymbol DATAGRAM = new ELispSymbol("datagram");
    public final ELispSymbol EGID = new ELispSymbol("egid");
    public final ELispSymbol ETIME = new ELispSymbol("etime");
    public final ELispSymbol EUID = new ELispSymbol("euid");
    public final ELispSymbol EVEN = new ELispSymbol("even");
    public final ELispSymbol FAILED = new ELispSymbol("failed");
    public final ELispSymbol GROUP = new ELispSymbol("group");
    public final ELispSymbol HW = new ELispSymbol("hw");
    public final ELispSymbol INTERNAL_DEFAULT_INTERRUPT_PROCESS = new ELispSymbol("internal-default-interrupt-process");
    public final ELispSymbol INTERNAL_DEFAULT_PROCESS_FILTER = new ELispSymbol("internal-default-process-filter");
    public final ELispSymbol INTERNAL_DEFAULT_PROCESS_SENTINEL = new ELispSymbol("internal-default-process-sentinel");
    public final ELispSymbol INTERNAL_DEFAULT_SIGNAL_PROCESS = new ELispSymbol("internal-default-signal-process");
    public final ELispSymbol INTERRUPT_PROCESS_FUNCTIONS = new ELispSymbol("interrupt-process-functions");
    public final ELispSymbol IPV4 = new ELispSymbol("ipv4");
    public final ELispSymbol IPV6 = new ELispSymbol("ipv6");
    public final ELispSymbol LAST_NONMENU_EVENT = new ELispSymbol("last-nonmenu-event");
    public final ELispSymbol LISTEN = new ELispSymbol("listen");
    public final ELispSymbol LIST_SYSTEM_PROCESSES = new ELispSymbol("list-system-processes");
    public final ELispSymbol LOCAL = new ELispSymbol("local");
    public final ELispSymbol MAJFLT = new ELispSymbol("majflt");
    public final ELispSymbol MAKE_PROCESS = new ELispSymbol("make-process");
    public final ELispSymbol MESSAGE = new ELispSymbol("message");
    public final ELispSymbol MINFLT = new ELispSymbol("minflt");
    public final ELispSymbol NETWORK = new ELispSymbol("network");
    public final ELispSymbol NICE = new ELispSymbol("nice");
    public final ELispSymbol NSM_VERIFY_CONNECTION = new ELispSymbol("nsm-verify-connection");
    public final ELispSymbol NULL = new ELispSymbol("null");
    public final ELispSymbol NUMERIC = new ELispSymbol("numeric");
    public final ELispSymbol ODD = new ELispSymbol("odd");
    public final ELispSymbol OPEN = new ELispSymbol("open");
    public final ELispSymbol PCPU = new ELispSymbol("pcpu");
    public final ELispSymbol PGRP = new ELispSymbol("pgrp");
    public final ELispSymbol PIPE = new ELispSymbol("pipe");
    public final ELispSymbol PIPE_PROCESS_P = new ELispSymbol("pipe-process-p");
    public final ELispSymbol PMEM = new ELispSymbol("pmem");
    public final ELispSymbol PPID = new ELispSymbol("ppid");
    public final ELispSymbol PRI = new ELispSymbol("pri");
    public final ELispSymbol PROCESSP = new ELispSymbol("processp");
    public final ELispSymbol PROCESS_ATTRIBUTES = new ELispSymbol("process-attributes");
    public final ELispSymbol PTY = new ELispSymbol("pty");
    public final ELispSymbol REAL = new ELispSymbol("real");
    public final ELispSymbol RSS = new ELispSymbol("rss");
    public final ELispSymbol RUN = new ELispSymbol("run");
    public final ELispSymbol SEQPACKET = new ELispSymbol("seqpacket");
    public final ELispSymbol SERIAL = new ELispSymbol("serial");
    public final ELispSymbol SESS = new ELispSymbol("sess");
    public final ELispSymbol SIGNAL = new ELispSymbol("signal");
    public final ELispSymbol SIGNAL_PROCESS_FUNCTIONS = new ELispSymbol("signal-process-functions");
    public final ELispSymbol START = new ELispSymbol("start");
    public final ELispSymbol STATE = new ELispSymbol("state");
    public final ELispSymbol STIME = new ELispSymbol("stime");
    public final ELispSymbol STOP = new ELispSymbol("stop");
    public final ELispSymbol SW = new ELispSymbol("sw");
    public final ELispSymbol THCOUNT = new ELispSymbol("thcount");
    public final ELispSymbol TIME = new ELispSymbol("time");
    public final ELispSymbol TPGID = new ELispSymbol("tpgid");
    public final ELispSymbol TTNAME = new ELispSymbol("ttname");
    public final ELispSymbol USER = new ELispSymbol("user");
    public final ELispSymbol UTIME = new ELispSymbol("utime");
    public final ELispSymbol VSIZE = new ELispSymbol("vsize");
    private ELispSymbol[] processSymbols() {
        return new ELispSymbol[] {
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
    public final ELispSymbol AND_OPTIONAL = new ELispSymbol("&optional");
    public final ELispSymbol AND_REST = new ELispSymbol("&rest");
    public final ELispSymbol AUTOLOAD = new ELispSymbol("autoload");
    public final ELispSymbol CDEBUG_ON_EXIT = new ELispSymbol(":debug-on-exit");
    public final ELispSymbol CDOCUMENTATION = new ELispSymbol(":documentation");
    public final ELispSymbol COMMANDP = new ELispSymbol("commandp");
    public final ELispSymbol CSUCCESS = new ELispSymbol(":success");
    public final ELispSymbol DEBUG = new ELispSymbol("debug");
    public final ELispSymbol DEBUGGER = new ELispSymbol("debugger");
    public final ELispSymbol DEBUGGER_MAY_CONTINUE = new ELispSymbol("debugger-may-continue");
    public final ELispSymbol DEBUG_EARLY = new ELispSymbol("debug-early");
    public final ELispSymbol DEBUG_EARLY__HANDLER = new ELispSymbol("debug-early--handler");
    public final ELispSymbol DEFVARALIAS = new ELispSymbol("defvaralias");
    public final ELispSymbol DISPLAY_WARNING = new ELispSymbol("display-warning");
    public final ELispSymbol EXIT = new ELispSymbol("exit");
    public final ELispSymbol FUNCTIONP = new ELispSymbol("functionp");
    public final ELispSymbol INHIBIT_DEBUGGER = new ELispSymbol("inhibit-debugger");
    public final ELispSymbol INHIBIT_QUIT = new ELispSymbol("inhibit-quit");
    public final ELispSymbol INTERACTIVE = new ELispSymbol("interactive");
    public final ELispSymbol INTERNAL_INTERPRETER_ENVIRONMENT = new ELispSymbol("internal-interpreter-environment");
    public final ELispSymbol INTERNAL_WHEN_ENTERED_DEBUGGER = new ELispSymbol("internal-when-entered-debugger");
    public final ELispSymbol LOSING_VALUE = new ELispSymbol("losing-value");
    public final ELispSymbol MACRO = new ELispSymbol("macro");
    public final ELispSymbol SETQ = new ELispSymbol("setq");
    private ELispSymbol[] evalSymbols() {
        return new ELispSymbol[] {
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
    public final ELispSymbol CIN_PLACE = new ELispSymbol(":in-place");
    public final ELispSymbol CKEY = new ELispSymbol(":key");
    public final ELispSymbol CLESSP = new ELispSymbol(":lessp");
    public final ELispSymbol CODESET = new ELispSymbol("codeset");
    public final ELispSymbol CPURECOPY = new ELispSymbol(":purecopy");
    public final ELispSymbol CREHASH_SIZE = new ELispSymbol(":rehash-size");
    public final ELispSymbol CREHASH_THRESHOLD = new ELispSymbol(":rehash-threshold");
    public final ELispSymbol CREVERSE = new ELispSymbol(":reverse");
    public final ELispSymbol CSIZE = new ELispSymbol(":size");
    public final ELispSymbol CTEST = new ELispSymbol(":test");
    public final ELispSymbol CURSOR_IN_ECHO_AREA = new ELispSymbol("cursor-in-echo-area");
    public final ELispSymbol CWEAKNESS = new ELispSymbol(":weakness");
    public final ELispSymbol DAYS = new ELispSymbol("days");
    public final ELispSymbol EQ = new ELispSymbol("eq");
    public final ELispSymbol EQL = new ELispSymbol("eql");
    public final ELispSymbol EQUAL = new ELispSymbol("equal");
    public final ELispSymbol FEATURES = new ELispSymbol("features");
    public final ELispSymbol FROM__TTY_MENU_P = new ELispSymbol("from--tty-menu-p");
    public final ELispSymbol FUNCALL = new ELispSymbol("funcall");
    public final ELispSymbol HASH_TABLE_P = new ELispSymbol("hash-table-p");
    public final ELispSymbol HASH_TABLE_TEST = new ELispSymbol("hash-table-test");
    public final ELispSymbol IV_AUTO = new ELispSymbol("iv-auto");
    public final ELispSymbol KEY = new ELispSymbol("key");
    public final ELispSymbol KEY_AND_VALUE = new ELispSymbol("key-and-value");
    public final ELispSymbol KEY_OR_VALUE = new ELispSymbol("key-or-value");
    public final ELispSymbol LIST_OR_VECTOR_P = new ELispSymbol("list-or-vector-p");
    public final ELispSymbol MD5 = new ELispSymbol("md5");
    public final ELispSymbol MONTHS = new ELispSymbol("months");
    public final ELispSymbol OVERRIDING_PLIST_ENVIRONMENT = new ELispSymbol("overriding-plist-environment");
    public final ELispSymbol PAPER = new ELispSymbol("paper");
    public final ELispSymbol PLISTP = new ELispSymbol("plistp");
    public final ELispSymbol PROVIDE = new ELispSymbol("provide");
    public final ELispSymbol REAL_THIS_COMMAND = new ELispSymbol("real-this-command");
    public final ELispSymbol REQUIRE = new ELispSymbol("require");
    public final ELispSymbol SHA1 = new ELispSymbol("sha1");
    public final ELispSymbol SHA224 = new ELispSymbol("sha224");
    public final ELispSymbol SHA256 = new ELispSymbol("sha256");
    public final ELispSymbol SHA384 = new ELispSymbol("sha384");
    public final ELispSymbol SHA512 = new ELispSymbol("sha512");
    public final ELispSymbol STRING_LESSP = new ELispSymbol("string-lessp");
    public final ELispSymbol SUBFEATURES = new ELispSymbol("subfeatures");
    public final ELispSymbol VALUE = new ELispSymbol("value");
    public final ELispSymbol VALUELT = new ELispSymbol("value<");
    public final ELispSymbol WIDGET_TYPE = new ELispSymbol("widget-type");
    public final ELispSymbol YES_OR_NO_P = new ELispSymbol("yes-or-no-p");
    public final ELispSymbol YES_OR_NO_P_HISTORY = new ELispSymbol("yes-or-no-p-history");
    public final ELispSymbol Y_OR_N_P = new ELispSymbol("y-or-n-p");
    private ELispSymbol[] fnsSymbols() {
        return new ELispSymbol[] {
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
    public final ELispSymbol CHAR_CODE_PROPERTY_TABLE = new ELispSymbol("char-code-property-table");
    private ELispSymbol[] chartabSymbols() {
        return new ELispSymbol[] {
            CHAR_CODE_PROPERTY_TABLE,
        };
    }
    /* @end region="chartab.c" */
    /* @generated region="alloc.c" by="extract-emacs-src.py" */
    public final ELispSymbol ALLOC = new ELispSymbol("alloc");
    public final ELispSymbol AUTOMATIC_GC = new ELispSymbol("Automatic GC");
    public final ELispSymbol BUFFERS = new ELispSymbol("buffers");
    public final ELispSymbol CEMERGENCY = new ELispSymbol(":emergency");
    public final ELispSymbol CHAR_TABLE_EXTRA_SLOTS = new ELispSymbol("char-table-extra-slots");
    public final ELispSymbol CONSES = new ELispSymbol("conses");
    public final ELispSymbol FLOATS = new ELispSymbol("floats");
    public final ELispSymbol GC_CONS_PERCENTAGE = new ELispSymbol("gc-cons-percentage");
    public final ELispSymbol GC_CONS_THRESHOLD = new ELispSymbol("gc-cons-threshold");
    public final ELispSymbol HEAP = new ELispSymbol("heap");
    public final ELispSymbol INTERVALS = new ELispSymbol("intervals");
    public final ELispSymbol MEMORY_INFO = new ELispSymbol("memory-info");
    public final ELispSymbol POST_GC_HOOK = new ELispSymbol("post-gc-hook");
    public final ELispSymbol STRINGS = new ELispSymbol("strings");
    public final ELispSymbol STRING_BYTES = new ELispSymbol("string-bytes");
    public final ELispSymbol SYMBOLS = new ELispSymbol("symbols");
    public final ELispSymbol VECTORS = new ELispSymbol("vectors");
    public final ELispSymbol VECTOR_SLOTS = new ELispSymbol("vector-slots");
    private ELispSymbol[] allocSymbols() {
        return new ELispSymbol[] {
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
