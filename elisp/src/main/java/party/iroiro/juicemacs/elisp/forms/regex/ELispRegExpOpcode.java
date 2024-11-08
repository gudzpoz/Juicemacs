package party.iroiro.juicemacs.elisp.forms.regex;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

class ELispRegExpOpcode {
    /// Bit flag marking other opcodes
    ///
    /// ```text
    ///       /------- Opcode
    ///       |||||||
    /// MSB |00000000| signed 24-bit in place argument | LSB
    ///      |
    ///      \- OP_FLAG_OTHER/JMP
    ///```
    ///
    /// @see #OP_FLAG_JMP
    @SuppressWarnings("unused")
    public static final int OP_FLAG_OTHER = 0b0_0000000;

    //#region Opcode: Others
    public static final int OP_MATCH         = 0;
    public static final int OP_COUNTER_RESET = 1;
    public static final int OP_COUNTER_INC   = 2;
    public static final int OP_COUNTER_CMP   = 3;
    public static final int OP_PROGRESS_REC  = 4;
    public static final int OP_PROGRESS_CMP  = 5;
    public static final int OP_JUMP_TABLE    = 6;
    public static final int OP_SPLIT         = 7;
    public static final int OP_LOOKAHEAD_CMP = 8;
    public static final int OP$STR_START     = 9;
    public static final int OP$STR_END       = 10;
    public static final int OP$LINE_START    = 11;
    public static final int OP$LINE_END      = 12;
    public static final int OP$BUFFER_POINT  = 13;
    public static final int OP$WORD_START    = 14;
    public static final int OP$WORD_END      = 15;
    public static final int OP$WORD_BOUND    = 16;
    public static final int OP$SYMBOL_START  = 17;
    public static final int OP$SYMBOL_END    = 18;
    public static final int OP$SYNTAX_CHAR   = 19;
    public static final int OP$CATEGORY_CHAR = 20;
    public static final int OP$BACKREF       = 21;
    public static final int OP$CHAR_CLASS    = 22;
    public static final int OP$CHAR_CLASS_32 = 23;
    public static final int OP$CHAR          = 24;
    public static final int OP$ANY           = 25;
    //#endregion Opcode: Others

    public static final int IN_PLACE_ARG_MASK = 0xFF_FF_FF;
    public static final int MAX_IN_PLACE_ARG  = 0x7F_FF_FF;
    public static final int MIN_IN_PLACE_ARG  = (0x80_00_00 << 8) >> 8;

    public static final int ARG_BIT_FLAG = 1 << 23;
    public static final int ARG_BIT_MASK = ARG_BIT_FLAG - 1;

    static int packSingleArgOpcode(int code, int arg) {
        if (arg < MIN_IN_PLACE_ARG || MAX_IN_PLACE_ARG < arg) {
            throw ELispSignals.error("Argument out of range: " + arg);
        }
        return (code << 24) | (arg & IN_PLACE_ARG_MASK);
    }

    static int packNoArgOpcode(int code) {
        return code << 24;
    }

    /// Bit flag marking jump opcodes
    ///
    /// ```text
    ///       BCD
    ///       |||
    /// MSB |00000000| signed 24-bit in place argument | LSB
    ///      |   ||||
    ///      A   EFGH
    ///```
    ///
    /// - `A`: `OP_FLAG_JMP`
    /// - `BCD`: `OP_FLAG_JMP_UNCOND/LE/LT/GT/GE/EQ/NE`
    ///   - `B` is used to negate the comparison result.
    /// - `EFGH`: Reserved
    ///
    /// @see #OP_FLAG_OTHER
    public static final int OP_FLAG_JMP            = 0b1_000_0_0_00;
    //#region Opcode: Jump
    public static final int OP_FLAG_JMP_UNCOND     = 0b0_000;
    public static final int OP_FLAG_JMP_NO_JUMP    = 0b0_100;
    public static final int OP_FLAG_JMP_LE         = 0b0_001;
    public static final int OP_FLAG_JMP_GT         = 0b0_101;
    public static final int OP_FLAG_JMP_LT         = 0b0_010;
    public static final int OP_FLAG_JMP_GE         = 0b0_110;
    public static final int OP_FLAG_JMP_EQ         = 0b0_011;
    public static final int OP_FLAG_JMP_NE         = 0b0_111;
    public static final int OP_FLAG_JMP_COND_MASK  = 0b0_111;
    public static final int OP_FLAG_JMP_COND_SHIFT = 4;
    //#endregion Opcode: Jump

    private static int packJmp(int cond, int rel) {
        int opcode = (cond << OP_FLAG_JMP_COND_SHIFT)
                | OP_FLAG_JMP;
        return packSingleArgOpcode(
                opcode,
                rel
        );
    }

    static int uncondJmp(int rel) {
        return packJmp(OP_FLAG_JMP_UNCOND, rel);
    }

    static int condJumpSingle(int comparison, int rel) {
        return packJmp(comparison, rel);
    }
}
