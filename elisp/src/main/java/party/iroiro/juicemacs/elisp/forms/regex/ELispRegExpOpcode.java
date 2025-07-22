package party.iroiro.juicemacs.elisp.forms.regex;

import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

/// Opcodes for the regexp VM
///
/// Opcodes are encoded in the 8 most significant bits of an integer.
/// The remaining 24 bits store the first argument, if any, denoted as
/// "inline arg" in the Javadoc.
///
/// The opcodes are named quite arbitrarily. Basically,
///
/// - `OP$XXX` are assertions.
/// - `OP_XXX` are control sequences, with `OP_XXX$YYY` serving as
///   backtracking branches.
@SuppressWarnings({"PMD.AvoidDollarSigns", "PMD.FieldNamingConventions"})
abstract class ELispRegExpOpcode {
    //#region Opcode: Others
    /// Finalize and report a match. No arg.
    public static final int OP_MATCH         = 1;
    /// Extract args from [com.oracle.truffle.api.frame.VirtualFrame]. No arg.
    public static final int OP_PROLOG_SEARCH = 2;
    /// Backtracking instruction for [#OP_PROLOG_SEARCH].
    ///
    /// When searching, it serves as `.*?`. Otherwise, it reports failure.
    public static final int OP_PROLOG$SEARCH = 3;
    /// Unconditional jump. Single inline arg (relative pc).
    ///
    /// It is only used by the branches [OP_UNION_*](#OP_UNION_PRE) to
    /// jump to the end of the union (after [#OP_UNION$POST]).
    /// Any other jumps are always implemented within the instructions.
    public static final int OP_JUMP          = 4;
    /// Union entry instruction. Always followed by [#OP_UNION$PRE].
    ///
    /// - Inline arg: frame slot number
    /// - Required frame slot: 1
    ///
    /// ## Flow
    ///
    /// - Frame slot usage: current branch index (0 or 1)
    /// - Entry: backup frame slot into overflow stack; set the slot to 0;
    ///   jump to the first branch.
    /// - Entry backtracking: increment frame slot and try another branch;
    ///   when branches are exhausted, restore the previous slot and backtrack.
    /// - Exit backtracking: jump to the backtracking instruction for the last
    ///   executed branch.
    public static final int OP_UNION_PRE     = 5;
    /// Union entry backtracking instruction.
    ///
    /// - Inline arg: backtrack relative pc.
    /// - Extra args: 1 integers (branch2 entry)
    ///   - In absolute pc, since they are shared by [#OP_UNION_PRE] and would be
    ///     ambiguous otherwise.
    public static final int OP_UNION$PRE     = 6;
    /// Union exit backtracking instruction.
    ///
    /// - Inline args: slot number
    /// - Extra args: 2 integers
    ///   - `[branch1 backtrack, branch2 backtrack]`
    ///   - Also absolute pc
    public static final int OP_UNION$POST    = 7;
    /// Capture group instruction. Always followed by [#OP_CAPTURE$WRITE].
    ///
    /// - Inline arg: frame slot
    /// - Required frame slot: 1
    ///
    /// ## Flow
    ///
    /// - Frame slot usage: group start position
    /// - Entry: backup frame slot and write the current sp to it.
    /// - Entry backtracking: restore and backtrack.
    ///
    /// Note that a whole capture group requires two pairs of [#OP_CAPTURE_WRITE]
    /// and [#OP_CAPTURE$WRITE].
    public static final int OP_CAPTURE_WRITE = 8;
    /// Capture group backtracking instruction.
    ///
    /// - Inline arg: backtrack relative pc.
    public static final int OP_CAPTURE$WRITE = 9;
    /// Quantifier instruction (for `+` `*` or `{10,12}`).
    ///
    /// - Inline arg: frame slot number
    ///
    /// ## Flow
    ///
    /// - Frame slot usage: repeat count
    /// - Entry:
    ///   - Backup and init frame slot (0) and sp.
    /// - Exit: ([#OP_QUANT_POST])
    ///   - if sp did not move: exit.
    ///   - if lazy:
    ///     - if repeat count is ok: exit
    ///     - else: increment loop count and jump to start
    ///   - if greedy:
    ///     - if next repeat count is over: exit
    ///     - else: increment loop count and jump to start
    /// - Exit backtracking:
    ///   - if lazy:
    ///     - if next repeat count is ok: loop again
    ///   - otherwise: backtrack via entry backtracking
    /// - Entry backtracking: if counter is 0, backtrack;
    ///   otherwise decrement counter and backtrack loop
    public static final int OP_QUANT_PRE     = 10;
    /// Quantifier entry backtracking instruction.
    ///
    /// - Inline arg: backtrack relative pc
    /// - Extra args: 2
    ///   - Backtrack absolute pc
    ///   - min repeats (inclusive) (>= 1)
    ///     - use [#OP_UNION_PRE] if min repeats is 0
    public static final int OP_QUANT$PRE     = 11;
    /// Quantifier exit instruction.
    ///
    /// - Inline arg: slot number
    /// - Extra args: 2
    ///   - min repeats (inclusive)
    ///   - max repeats (inclusive)
    public static final int OP_QUANT_POST    = 12;
    /// Quantifier exit backtracking instruction.
    ///
    /// - Inline arg: relative pc to [#OP_QUANT$PRE]
    public static final int OP_QUANT$POST    = 13;

    public static final int OP_QUANT$PRE_LAZY  = 14;
    public static final int OP_QUANT_POST_LAZY = 15;
    public static final int OP_QUANT$POST_LAZY = 16;

    public static final int OP$STR_START     = 64;
    public static final int OP$STR_END       = 65;
    public static final int OP$LINE_START    = 66;
    public static final int OP$LINE_END      = 67;
    public static final int OP$BUFFER_POINT  = 68;
    public static final int OP$WORD_START    = 69;
    public static final int OP$WORD_END      = 70;
    public static final int OP$WORD_BOUND    = 71;
    public static final int OP$SYMBOL_START  = 72;
    public static final int OP$SYMBOL_END    = 73;
    public static final int OP$SYNTAX_CHAR   = 74;
    public static final int OP$CATEGORY_CHAR = 75;
    public static final int OP$BACKREF       = 76;
    public static final int OP$CHAR_CLASS    = 77;
    public static final int OP$CHAR_CLASS_32 = 78;
    public static final int OP$CHAR          = 79;
    public static final int OP$ANY_BUT       = 80;
    public static final int OP$ANY           = 81;
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

    static int packSingleInvertibleArgOpcode(int code, int arg, boolean flag) {
        if (arg < 0 || MAX_IN_PLACE_ARG < arg) {
            throw ELispSignals.error("Argument out of range: " + arg);
        }
        return packSingleArgOpcode(code, arg) | (flag ? ARG_BIT_FLAG : 0);
    }

    static int packNoArgOpcode(int code) {
        return code << 24;
    }
}
