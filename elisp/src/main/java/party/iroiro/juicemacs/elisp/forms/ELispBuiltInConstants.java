package party.iroiro.juicemacs.elisp.forms;

import party.iroiro.juicemacs.elisp.runtime.ELispContext;

/// Emacs C constants extracted by `extract_emacs_fields.py`
public abstract class ELispBuiltInConstants {
    //#region CHARTAB_SIZE_BITS
    public static final int CHARTAB_SIZE_BITS_0 = 6;
    public static final int CHARTAB_SIZE_BITS_1 = 4;
    public static final int CHARTAB_SIZE_BITS_2 = 5;
    public static final int CHARTAB_SIZE_BITS_3 = 7;
    //#endregion CHARTAB_SIZE_BITS
    //#region CHAR_TABLE_STANDARD_SLOTS
    public static final int CHAR_TABLE_STANDARD_SLOTS = 68;
    //#endregion CHAR_TABLE_STANDARD_SLOTS
    //#region Lisp_Closure
    public static final int CLOSURE_ARGLIST = 0;
    public static final int CLOSURE_CODE = 1;
    public static final int CLOSURE_CONSTANTS = 2;
    public static final int CLOSURE_STACK_DEPTH = 3;
    public static final int CLOSURE_DOC_STRING = 4;
    public static final int CLOSURE_INTERACTIVE = 5;
    //#endregion Lisp_Closure
    //#region char_bits
    public static final int CHAR_ALT = 0x0400000;
    public static final int CHAR_SUPER = 0x0800000;
    public static final int CHAR_HYPER = 0x1000000;
    public static final int CHAR_SHIFT = 0x2000000;
    public static final int CHAR_CTL = 0x4000000;
    public static final int CHAR_META = 0x8000000;
    public static final int CHAR_MODIFIER_MASK = CHAR_ALT | CHAR_SUPER | CHAR_HYPER | CHAR_SHIFT | CHAR_CTL | CHAR_META;
    public static final int CHARACTERBITS = 22;
    //#endregion char_bits
    //#region MAX_CHAR
    public static final int MAX_CHAR = 0x3FFFFF;
    //#endregion MAX_CHAR
    //#region MAX_UNICODE_CHAR
    public static final int MAX_UNICODE_CHAR = 0x10FFFF;
    //#endregion MAX_UNICODE_CHAR
    //#region MAX_._BYTE_CHAR
    public static final int MAX_1_BYTE_CHAR = 0x7F;
    public static final int MAX_2_BYTE_CHAR = 0x7FF;
    public static final int MAX_3_BYTE_CHAR = 0xFFFF;
    public static final int MAX_4_BYTE_CHAR = 0x1FFFFF;
    public static final int MAX_5_BYTE_CHAR = 0x3FFF7F;
    //#endregion MAX_._BYTE_CHAR
    //#region NO_BREAK_SPACE
    public static final int NO_BREAK_SPACE = 0x00A0;
    public static final int SOFT_HYPHEN = 0x00AD;
    public static final int ZERO_WIDTH_NON_JOINER = 0x200C;
    public static final int ZERO_WIDTH_JOINER = 0x200D;
    public static final int HYPHEN = 0x2010;
    public static final int NON_BREAKING_HYPHEN = 0x2011;
    public static final int LEFT_SINGLE_QUOTATION_MARK = 0x2018;
    public static final int RIGHT_SINGLE_QUOTATION_MARK = 0x2019;
    public static final int PARAGRAPH_SEPARATOR = 0x2029;
    public static final int LEFT_POINTING_ANGLE_BRACKET = 0x2329;
    public static final int RIGHT_POINTING_ANGLE_BRACKET = 0x232A;
    public static final int LEFT_ANGLE_BRACKET = 0x3008;
    public static final int RIGHT_ANGLE_BRACKET = 0x3009;
    public static final int OBJECT_REPLACEMENT_CHARACTER = 0xFFFC;
    public static final int TAG_SPACE = 0xE0020;
    public static final int CANCEL_TAG = 0xE007F;
    //#endregion NO_BREAK_SPACE
    //#region UNICODE_CATEGORY_UNKNOWN
    public static final int UNICODE_CATEGORY_UNKNOWN = 0;
    public static final int UNICODE_CATEGORY_LU = 1;
    public static final int UNICODE_CATEGORY_LL = 2;
    public static final int UNICODE_CATEGORY_LT = 3;
    public static final int UNICODE_CATEGORY_LM = 4;
    public static final int UNICODE_CATEGORY_LO = 5;
    public static final int UNICODE_CATEGORY_MN = 6;
    public static final int UNICODE_CATEGORY_MC = 7;
    public static final int UNICODE_CATEGORY_ME = 8;
    public static final int UNICODE_CATEGORY_ND = 9;
    public static final int UNICODE_CATEGORY_NL = 10;
    public static final int UNICODE_CATEGORY_NO = 11;
    public static final int UNICODE_CATEGORY_PC = 12;
    public static final int UNICODE_CATEGORY_PD = 13;
    public static final int UNICODE_CATEGORY_PS = 14;
    public static final int UNICODE_CATEGORY_PE = 15;
    public static final int UNICODE_CATEGORY_PI = 16;
    public static final int UNICODE_CATEGORY_PF = 17;
    public static final int UNICODE_CATEGORY_PO = 18;
    public static final int UNICODE_CATEGORY_SM = 19;
    public static final int UNICODE_CATEGORY_SC = 20;
    public static final int UNICODE_CATEGORY_SK = 21;
    public static final int UNICODE_CATEGORY_SO = 22;
    public static final int UNICODE_CATEGORY_ZS = 23;
    public static final int UNICODE_CATEGORY_ZL = 24;
    public static final int UNICODE_CATEGORY_ZP = 25;
    public static final int UNICODE_CATEGORY_CC = 26;
    public static final int UNICODE_CATEGORY_CF = 27;
    public static final int UNICODE_CATEGORY_CS = 28;
    public static final int UNICODE_CATEGORY_CO = 29;
    public static final int UNICODE_CATEGORY_CN = 30;
    //#endregion UNICODE_CATEGORY_UNKNOWN
    //#region define_charset_arg_index
    // The following documentation is partly copied from `define-charset` (defined in `mule.el`) in GNU Emacs
    /// the name of the charset (e.g., [ELispContext#ASCII] or [ELispContext#UNICODE])
    public static final int CHARSET_ARG_NAME = 0;
    /// `0`, `1`, `2`, or `3`, the dimension (a.k.a., max byte length) of code-points of the charset
    public static final int CHARSET_ARG_DIMENSION = 1;
    /// the byte code range of each dimension (read: byte) of the charset: `[min-1 max-1 ...]`
    public static final int CHARSET_ARG_CODE_SPACE = 2;
    /// the minimum code-point of the charset
    public static final int CHARSET_ARG_MIN_CODE = 3;
    /// the maximum code-point of the charset
    public static final int CHARSET_ARG_MAX_CODE = 4;
    /// the final char of the charset for ISO-2022 encoding
    public static final int CHARSET_ARG_ISO_FINAL = 5;
    /// the revision number of the charset for ISO-2022 encoding
    public static final int CHARSET_ARG_ISO_REVISION = 6;
    /// 0, or 129..255
    public static final int CHARSET_ARG_EMACS_MULE_ID = 7;
    /// true if the first 128 code points map to ASCII
    public static final int CHARSET_ARG_ASCII_COMPATIBLE_P = 8;
    /// true if used only as a parent or a subset of some other charset
    public static final int CHARSET_ARG_SUPPLEMENTARY_P = 9;
    /// a nonnegative integer that can be used as an invalid code point of the charset.
    public static final int CHARSET_ARG_INVALID_CODE = 10;
    /// an integer added to the index number of a character to get the corresponding character code
    public static final int CHARSET_ARG_CODE_OFFSET = 11;
    /// codepoint to character byte-sequence mapping, vector or name of a file
    public static final int CHARSET_ARG_MAP = 12;
    /// a list: `( PARENT MIN-CODE MAX-CODE OFFSET )`
    public static final int CHARSET_ARG_SUBSET = 13;
    /// a list of parent charsets
    public static final int CHARSET_ARG_SUPERSET = 14;
    /// similar to `CHARSET_ARG_MAP`, but maps to Unicode
    public static final int CHARSET_ARG_UNIFY_MAP = 15;
    /// the property list of the charset
    public static final int CHARSET_ARG_PLIST = 16;
    /// the maximum number of arguments
    public static final int CHARSET_ARG_MAX = 17;
    //#endregion define_charset_arg_index
    //#region charset_attr_index
    public static final int CHARSET_ID = 0;
    public static final int CHARSET_NAME = 1;
    public static final int CHARSET_PLIST = 2;
    public static final int CHARSET_MAP = 3;
    public static final int CHARSET_DECODER = 4;
    public static final int CHARSET_ENCODER = 5;
    public static final int CHARSET_SUBSET = 6;
    public static final int CHARSET_SUPERSET = 7;
    public static final int CHARSET_UNIFY_MAP = 8;
    public static final int CHARSET_DEUNIFIER = 9;
    public static final int CHARSET_ATTR_MAX = 10;
    //#endregion charset_attr_index
    //#region define_coding_system_arg_index
    public static final int CODING_ARG_NAME = 0;
    public static final int CODING_ARG_MNEMONIC = 1;
    public static final int CODING_ARG_CODING_TYPE = 2;
    public static final int CODING_ARG_CHARSET_LIST = 3;
    public static final int CODING_ARG_ASCII_COMPATIBLE_P = 4;
    public static final int CODING_ARG_DECODE_TRANSLATION_TABLE = 5;
    public static final int CODING_ARG_ENCODE_TRANSLATION_TABLE = 6;
    public static final int CODING_ARG_POST_READ_CONVERSION = 7;
    public static final int CODING_ARG_PRE_WRITE_CONVERSION = 8;
    public static final int CODING_ARG_DEFAULT_CHAR = 9;
    public static final int CODING_ARG_FOR_UNIBYTE = 10;
    public static final int CODING_ARG_PLIST = 11;
    public static final int CODING_ARG_EOL_TYPE = 12;
    public static final int CODING_ARG_MAX = 13;
    //#endregion define_coding_system_arg_index
    //#region define_coding_iso2022_arg_index
    public static final int CODING_ARG_ISO2022_INITIAL = CODING_ARG_MAX;
    public static final int CODING_ARG_ISO2022_REG_USAGE = CODING_ARG_MAX + 1;
    public static final int CODING_ARG_ISO2022_REQUEST = CODING_ARG_MAX + 2;
    public static final int CODING_ARG_ISO2022_FLAGS = CODING_ARG_MAX + 3;
    public static final int CODING_ARG_ISO2022_MAX = CODING_ARG_MAX + 4;
    //#endregion define_coding_iso2022_arg_index
    //#region define_coding_utf8_arg_index
    public static final int CODING_ARG_UTF8_BOM = CODING_ARG_MAX;
    public static final int CODING_ARG_UTF8_MAX = CODING_ARG_MAX + 1;
    //#endregion define_coding_utf8_arg_index
    //#region define_coding_utf16_arg_index
    public static final int CODING_ARG_UTF16_BOM = CODING_ARG_MAX;
    public static final int CODING_ARG_UTF16_ENDIAN = CODING_ARG_MAX + 1;
    public static final int CODING_ARG_UTF16_MAX = CODING_ARG_MAX + 2;
    //#endregion define_coding_utf16_arg_index
    //#region define_coding_ccl_arg_index
    public static final int CODING_ARG_CCL_DECODER = CODING_ARG_MAX;
    public static final int CODING_ARG_CCL_ENCODER = CODING_ARG_MAX + 1;
    public static final int CODING_ARG_CCL_VALIDS = CODING_ARG_MAX + 2;
    public static final int CODING_ARG_CCL_MAX = CODING_ARG_MAX + 3;
    //#endregion define_coding_ccl_arg_index
    //#region define_coding_undecided_arg_index
    public static final int CODING_ARG_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION = CODING_ARG_MAX;
    public static final int CODING_ARG_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION = CODING_ARG_MAX + 1;
    public static final int CODING_ARG_UNDECIDED_PREFER_UTF_8 = CODING_ARG_MAX + 2;
    public static final int CODING_ARG_UNDECIDED_MAX = CODING_ARG_MAX + 3;
    //#endregion define_coding_undecided_arg_index
    //#region coding_attr_index
    public static final int CODING_ATTR_BASE_NAME = 0;
    public static final int CODING_ATTR_DOCSTRING = 1;
    public static final int CODING_ATTR_MNEMONIC = 2;
    public static final int CODING_ATTR_TYPE = 3;
    public static final int CODING_ATTR_CHARSET_LIST = 4;
    public static final int CODING_ATTR_ASCII_COMPAT = 5;
    public static final int CODING_ATTR_DECODE_TBL = 6;
    public static final int CODING_ATTR_ENCODE_TBL = 7;
    public static final int CODING_ATTR_TRANS_TBL = 8;
    public static final int CODING_ATTR_POST_READ = 9;
    public static final int CODING_ATTR_PRE_WRITE = 10;
    public static final int CODING_ATTR_DEFAULT_CHAR = 11;
    public static final int CODING_ATTR_FOR_UNIBYTE = 12;
    public static final int CODING_ATTR_PLIST = 13;
    public static final int CODING_ATTR_CATEGORY = 14;
    public static final int CODING_ATTR_SAFE_CHARSETS = 15;
    public static final int CODING_ATTR_CHARSET_VALIDS = 16;
    public static final int CODING_ATTR_CCL_DECODER = 17;
    public static final int CODING_ATTR_CCL_ENCODER = 18;
    public static final int CODING_ATTR_CCL_VALIDS = 19;
    public static final int CODING_ATTR_ISO_INITIAL = 20;
    public static final int CODING_ATTR_ISO_USAGE = 21;
    public static final int CODING_ATTR_ISO_REQUEST = 22;
    public static final int CODING_ATTR_ISO_FLAGS = 23;
    public static final int CODING_ATTR_UTF_BOM = 24;
    public static final int CODING_ATTR_UTF_16_ENDIAN = 25;
    public static final int CODING_ATTR_EMACS_MULE_FULL = 26;
    public static final int CODING_ATTR_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION = 27;
    public static final int CODING_ATTR_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION = 28;
    public static final int CODING_ATTR_UNDECIDED_PREFER_UTF_8 = 29;
    public static final int CODING_ATTR_LAST_INDEX = 30;
    //#endregion coding_attr_index
    //#region coding_result_code
    public static final int CODING_RESULT_SUCCESS = 0;
    public static final int CODING_RESULT_INSUFFICIENT_SRC = 1;
    public static final int CODING_RESULT_INSUFFICIENT_DST = 2;
    public static final int CODING_RESULT_INVALID_SRC = 3;
    public static final int CODING_RESULT_INTERRUPT = 4;
    //#endregion coding_result_code
    //#region syntaxcode
    public static final int SWHITESPACE = 0;
    public static final int SPUNCT = 1;
    public static final int SWORD = 2;
    public static final int SSYMBOL = 3;
    public static final int SOPEN = 4;
    public static final int SCLOSE = 5;
    public static final int SQUOTE = 6;
    public static final int SSTRING = 7;
    public static final int SMATH = 8;
    public static final int SESCAPE = 9;
    public static final int SCHARQUOTE = 10;
    public static final int SCOMMENT = 11;
    public static final int SENDCOMMENT = 12;
    public static final int SINHERIT = 13;
    public static final int SCOMMENT_FENCE = 14;
    public static final int SSTRING_FENCE = 15;
    public static final int SMAX = 16;
    //#endregion syntaxcode
    //#region coding_category
    public static final int CODING_CATEGORY_ISO_7 = 0;
    public static final int CODING_CATEGORY_ISO_7_TIGHT = 1;
    public static final int CODING_CATEGORY_ISO_8_1 = 2;
    public static final int CODING_CATEGORY_ISO_8_2 = 3;
    public static final int CODING_CATEGORY_ISO_7_ELSE = 4;
    public static final int CODING_CATEGORY_ISO_8_ELSE = 5;
    public static final int CODING_CATEGORY_UTF_8_AUTO = 6;
    public static final int CODING_CATEGORY_UTF_8_NOSIG = 7;
    public static final int CODING_CATEGORY_UTF_8_SIG = 8;
    public static final int CODING_CATEGORY_UTF_16_AUTO = 9;
    public static final int CODING_CATEGORY_UTF_16_BE = 10;
    public static final int CODING_CATEGORY_UTF_16_LE = 11;
    public static final int CODING_CATEGORY_UTF_16_BE_NOSIG = 12;
    public static final int CODING_CATEGORY_UTF_16_LE_NOSIG = 13;
    public static final int CODING_CATEGORY_CHARSET = 14;
    public static final int CODING_CATEGORY_SJIS = 15;
    public static final int CODING_CATEGORY_BIG5 = 16;
    public static final int CODING_CATEGORY_CCL = 17;
    public static final int CODING_CATEGORY_EMACS_MULE = 18;
    public static final int CODING_CATEGORY_RAW_TEXT = 19;
    public static final int CODING_CATEGORY_UNDECIDED = 20;
    public static final int CODING_CATEGORY_MAX = 21;
    //#endregion coding_category
    //#region CODING_ISO_FLAG_\w+
    public static final int CODING_ISO_FLAG_LONG_FORM = 0x0001;
    public static final int CODING_ISO_FLAG_RESET_AT_EOL = 0x0002;
    public static final int CODING_ISO_FLAG_RESET_AT_CNTL = 0x0004;
    public static final int CODING_ISO_FLAG_SEVEN_BITS = 0x0008;
    public static final int CODING_ISO_FLAG_LOCKING_SHIFT = 0x0010;
    public static final int CODING_ISO_FLAG_SINGLE_SHIFT = 0x0020;
    public static final int CODING_ISO_FLAG_DESIGNATION = 0x0040;
    public static final int CODING_ISO_FLAG_REVISION = 0x0080;
    public static final int CODING_ISO_FLAG_DIRECTION = 0x0100;
    public static final int CODING_ISO_FLAG_INIT_AT_BOL = 0x0200;
    public static final int CODING_ISO_FLAG_DESIGNATE_AT_BOL = 0x0400;
    public static final int CODING_ISO_FLAG_SAFE = 0x0800;
    public static final int CODING_ISO_FLAG_LATIN_EXTRA = 0x1000;
    public static final int CODING_ISO_FLAG_COMPOSITION = 0x2000;
    public static final int CODING_ISO_FLAG_USE_ROMAN = 0x8000;
    public static final int CODING_ISO_FLAG_USE_OLDJIS = 0x10000;
    public static final int CODING_ISO_FLAG_LEVEL_4 = 0x20000;
    public static final int CODING_ISO_FLAG_FULL_SUPPORT = 0x100000;
    //#endregion CODING_ISO_FLAG_\w+
    //#region CODING_\w+_MASK
    public static final int CODING_ANNOTATION_MASK = 0x00FF;
    public static final int CODING_ANNOTATE_COMPOSITION_MASK = 0x0001;
    public static final int CODING_ANNOTATE_DIRECTION_MASK = 0x0002;
    public static final int CODING_ANNOTATE_CHARSET_MASK = 0x0003;
    public static final int CODING_FOR_UNIBYTE_MASK = 0x0100;
    public static final int CODING_REQUIRE_FLUSHING_MASK = 0x0200;
    public static final int CODING_REQUIRE_DECODING_MASK = 0x0400;
    public static final int CODING_REQUIRE_ENCODING_MASK = 0x0800;
    public static final int CODING_REQUIRE_DETECTION_MASK = 0x1000;
    public static final int CODING_RESET_AT_BOL_MASK = 0x2000;
    //#endregion CODING_\w+_MASK
}
