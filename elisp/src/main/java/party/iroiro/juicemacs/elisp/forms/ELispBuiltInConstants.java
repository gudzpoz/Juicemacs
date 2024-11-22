package party.iroiro.juicemacs.elisp.forms;

import party.iroiro.juicemacs.elisp.runtime.ELispContext;

/// Emacs C constants extracted by `extract_emacs_fields.py`
public abstract class ELispBuiltInConstants {
    //#region enum define_charset_arg_index
    // The following documentation is partly copied from `define-charset` (defined in `mule.el`) in GNU Emacs
    /// the name of the charset (e.g., [ELispContext#ASCII] or [ELispContext#UNICODE])
    public final static int CHARSET_ARG_NAME = 0;
    /// `0`, `1`, `2`, or `3`, the dimension (a.k.a., max byte length) of code-points of the charset
    public final static int CHARSET_ARG_DIMENSION = 1;
    /// the byte code range of each dimension (read: byte) of the charset: `[min-1 max-1 ...]`
    public final static int CHARSET_ARG_CODE_SPACE = 2;
    /// the minimum code-point of the charset
    public final static int CHARSET_ARG_MIN_CODE = 3;
    /// the maximum code-point of the charset
    public final static int CHARSET_ARG_MAX_CODE = 4;
    /// the final char of the charset for ISO-2022 encoding
    public final static int CHARSET_ARG_ISO_FINAL = 5;
    /// the revision number of the charset for ISO-2022 encoding
    public final static int CHARSET_ARG_ISO_REVISION = 6;
    /// 0, or 129..255
    public final static int CHARSET_ARG_EMACS_MULE_ID = 7;
    /// true if the first 128 code points map to ASCII
    public final static int CHARSET_ARG_ASCII_COMPATIBLE_P = 8;
    /// true if used only as a parent or a subset of some other charset
    public final static int CHARSET_ARG_SUPPLEMENTARY_P = 9;
    /// a nonnegative integer that can be used as an invalid code point of the charset.
    public final static int CHARSET_ARG_INVALID_CODE = 10;
    /// an integer added to the index number of a character to get the corresponding character code
    public final static int CHARSET_ARG_CODE_OFFSET = 11;
    /// codepoint to character byte-sequence mapping, vector or name of a file
    public final static int CHARSET_ARG_MAP = 12;
    /// a list: `( PARENT MIN-CODE MAX-CODE OFFSET )`
    public final static int CHARSET_ARG_SUBSET = 13;
    /// a list of parent charsets
    public final static int CHARSET_ARG_SUPERSET = 14;
    /// similar to `CHARSET_ARG_MAP`, but maps to Unicode
    public final static int CHARSET_ARG_UNIFY_MAP = 15;
    /// the property list of the charset
    public final static int CHARSET_ARG_PLIST = 16;
    /// the maximum number of arguments
    public final static int CHARSET_ARG_MAX = 17;
    //#endregion enum define_charset_arg_index
    //#region enum charset_attr_index
    public final static int CHARSET_ID = 0;
    public final static int CHARSET_NAME = 1;
    public final static int CHARSET_PLIST = 2;
    public final static int CHARSET_MAP = 3;
    public final static int CHARSET_DECODER = 4;
    public final static int CHARSET_ENCODER = 5;
    public final static int CHARSET_SUBSET = 6;
    public final static int CHARSET_SUPERSET = 7;
    public final static int CHARSET_UNIFY_MAP = 8;
    public final static int CHARSET_DEUNIFIER = 9;
    public final static int CHARSET_ATTR_MAX = 10;
    //#endregion enum charset_attr_index
    //#region enum coding_category
    public final static int CODING_CATEGORY_ISO_7 = 0;
    public final static int CODING_CATEGORY_ISO_7_TIGHT = 1;
    public final static int CODING_CATEGORY_ISO_8_1 = 2;
    public final static int CODING_CATEGORY_ISO_8_2 = 3;
    public final static int CODING_CATEGORY_ISO_7_ELSE = 4;
    public final static int CODING_CATEGORY_ISO_8_ELSE = 5;
    public final static int CODING_CATEGORY_UTF_8_AUTO = 6;
    public final static int CODING_CATEGORY_UTF_8_NOSIG = 7;
    public final static int CODING_CATEGORY_UTF_8_SIG = 8;
    public final static int CODING_CATEGORY_UTF_16_AUTO = 9;
    public final static int CODING_CATEGORY_UTF_16_BE = 10;
    public final static int CODING_CATEGORY_UTF_16_LE = 11;
    public final static int CODING_CATEGORY_UTF_16_BE_NOSIG = 12;
    public final static int CODING_CATEGORY_UTF_16_LE_NOSIG = 13;
    public final static int CODING_CATEGORY_CHARSET = 14;
    public final static int CODING_CATEGORY_SJIS = 15;
    public final static int CODING_CATEGORY_BIG5 = 16;
    public final static int CODING_CATEGORY_CCL = 17;
    public final static int CODING_CATEGORY_EMACS_MULE = 18;
    public final static int CODING_CATEGORY_RAW_TEXT = 19;
    public final static int CODING_CATEGORY_UNDECIDED = 20;
    public final static int CODING_CATEGORY_MAX = 21;
    //#endregion enum coding_category
    //#region enum syntaxcode
    public final static int SWHITESPACE = 0;
    public final static int SPUNCT = 1;
    public final static int SWORD = 2;
    public final static int SSYMBOL = 3;
    public final static int SOPEN = 4;
    public final static int SCLOSE = 5;
    public final static int SQUOTE = 6;
    public final static int SSTRING = 7;
    public final static int SMATH = 8;
    public final static int SESCAPE = 9;
    public final static int SCHARQUOTE = 10;
    public final static int SCOMMENT = 11;
    public final static int SENDCOMMENT = 12;
    public final static int SINHERIT = 13;
    public final static int SCOMMENT_FENCE = 14;
    public final static int SSTRING_FENCE = 15;
    public final static int SMAX = 16;
    //#endregion enum syntaxcode
    //#region enum Lisp_Closure
    public final static int CLOSURE_ARGLIST = 0;
    public final static int CLOSURE_CODE = 1;
    public final static int CLOSURE_CONSTANTS = 2;
    public final static int CLOSURE_STACK_DEPTH = 3;
    public final static int CLOSURE_DOC_STRING = 4;
    public final static int CLOSURE_INTERACTIVE = 5;
    //#endregion enum Lisp_Closure
    //#region enum coding_attr_index
    public final static int CODING_ATTR_BASE_NAME = 0;
    public final static int CODING_ATTR_DOCSTRING = 1;
    public final static int CODING_ATTR_MNEMONIC = 2;
    public final static int CODING_ATTR_TYPE = 3;
    public final static int CODING_ATTR_CHARSET_LIST = 4;
    public final static int CODING_ATTR_ASCII_COMPAT = 5;
    public final static int CODING_ATTR_DECODE_TBL = 6;
    public final static int CODING_ATTR_ENCODE_TBL = 7;
    public final static int CODING_ATTR_TRANS_TBL = 8;
    public final static int CODING_ATTR_POST_READ = 9;
    public final static int CODING_ATTR_PRE_WRITE = 10;
    public final static int CODING_ATTR_DEFAULT_CHAR = 11;
    public final static int CODING_ATTR_FOR_UNIBYTE = 12;
    public final static int CODING_ATTR_PLIST = 13;
    public final static int CODING_ATTR_CATEGORY = 14;
    public final static int CODING_ATTR_SAFE_CHARSETS = 15;
    public final static int CODING_ATTR_CHARSET_VALIDS = 16;
    public final static int CODING_ATTR_CCL_DECODER = 17;
    public final static int CODING_ATTR_CCL_ENCODER = 18;
    public final static int CODING_ATTR_CCL_VALIDS = 19;
    public final static int CODING_ATTR_ISO_INITIAL = 20;
    public final static int CODING_ATTR_ISO_USAGE = 21;
    public final static int CODING_ATTR_ISO_REQUEST = 22;
    public final static int CODING_ATTR_ISO_FLAGS = 23;
    public final static int CODING_ATTR_UTF_BOM = 24;
    public final static int CODING_ATTR_UTF_16_ENDIAN = 25;
    public final static int CODING_ATTR_EMACS_MULE_FULL = 26;
    public final static int CODING_ATTR_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION = 27;
    public final static int CODING_ATTR_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION = 28;
    public final static int CODING_ATTR_UNDECIDED_PREFER_UTF_8 = 29;
    public final static int CODING_ATTR_LAST_INDEX = 30;
    //#endregion enum coding_attr_index
    //#region enum define_coding_system_arg_index
    public final static int CODING_ARG_NAME = 0;
    public final static int CODING_ARG_MNEMONIC = 1;
    public final static int CODING_ARG_CODING_TYPE = 2;
    public final static int CODING_ARG_CHARSET_LIST = 3;
    public final static int CODING_ARG_ASCII_COMPATIBLE_P = 4;
    public final static int CODING_ARG_DECODE_TRANSLATION_TABLE = 5;
    public final static int CODING_ARG_ENCODE_TRANSLATION_TABLE = 6;
    public final static int CODING_ARG_POST_READ_CONVERSION = 7;
    public final static int CODING_ARG_PRE_WRITE_CONVERSION = 8;
    public final static int CODING_ARG_DEFAULT_CHAR = 9;
    public final static int CODING_ARG_FOR_UNIBYTE = 10;
    public final static int CODING_ARG_PLIST = 11;
    public final static int CODING_ARG_EOL_TYPE = 12;
    public final static int CODING_ARG_MAX = 13;
    //#endregion enum define_coding_system_arg_index
    //#region enum define_coding_iso2022_arg_index
    public final static int CODING_ARG_ISO2022_INITIAL = 13;
    public final static int CODING_ARG_ISO2022_REG_USAGE = 14;
    public final static int CODING_ARG_ISO2022_REQUEST = 15;
    public final static int CODING_ARG_ISO2022_FLAGS = 16;
    public final static int CODING_ARG_ISO2022_MAX = 17;
    //#endregion enum define_coding_iso2022_arg_index
    //#region enum define_coding_utf8_arg_index
    public final static int CODING_ARG_UTF8_BOM = 13;
    public final static int CODING_ARG_UTF8_MAX = 14;
    //#endregion enum define_coding_utf8_arg_index
    //#region enum define_coding_utf16_arg_index
    public final static int CODING_ARG_UTF16_BOM = 13;
    public final static int CODING_ARG_UTF16_ENDIAN = 14;
    public final static int CODING_ARG_UTF16_MAX = 15;
    //#endregion enum define_coding_utf16_arg_index
    //#region enum define_coding_ccl_arg_index
    public final static int CODING_ARG_CCL_DECODER = 13;
    public final static int CODING_ARG_CCL_ENCODER = 14;
    public final static int CODING_ARG_CCL_VALIDS = 15;
    public final static int CODING_ARG_CCL_MAX = 16;
    //#endregion enum define_coding_ccl_arg_index
    //#region enum define_coding_undecided_arg_index
    public final static int CODING_ARG_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION = 13;
    public final static int CODING_ARG_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION = 14;
    public final static int CODING_ARG_UNDECIDED_PREFER_UTF_8 = 15;
    public final static int CODING_ARG_UNDECIDED_MAX = 16;
    //#endregion enum define_coding_undecided_arg_index
    //#region define CODING_ISO_FLAG_\w+
    public final static int CODING_ISO_FLAG_LONG_FORM = 0x0001;
    public final static int CODING_ISO_FLAG_RESET_AT_EOL = 0x0002;
    public final static int CODING_ISO_FLAG_RESET_AT_CNTL = 0x0004;
    public final static int CODING_ISO_FLAG_SEVEN_BITS = 0x0008;
    public final static int CODING_ISO_FLAG_LOCKING_SHIFT = 0x0010;
    public final static int CODING_ISO_FLAG_SINGLE_SHIFT = 0x0020;
    public final static int CODING_ISO_FLAG_DESIGNATION = 0x0040;
    public final static int CODING_ISO_FLAG_REVISION = 0x0080;
    public final static int CODING_ISO_FLAG_DIRECTION = 0x0100;
    public final static int CODING_ISO_FLAG_INIT_AT_BOL = 0x0200;
    public final static int CODING_ISO_FLAG_DESIGNATE_AT_BOL = 0x0400;
    public final static int CODING_ISO_FLAG_SAFE = 0x0800;
    public final static int CODING_ISO_FLAG_LATIN_EXTRA = 0x1000;
    public final static int CODING_ISO_FLAG_COMPOSITION = 0x2000;
    public final static int CODING_ISO_FLAG_USE_ROMAN = 0x8000;
    public final static int CODING_ISO_FLAG_USE_OLDJIS = 0x10000;
    public final static int CODING_ISO_FLAG_LEVEL_4 = 0x20000;
    public final static int CODING_ISO_FLAG_FULL_SUPPORT = 0x100000;
    //#endregion define CODING_ISO_FLAG_\w+
}
