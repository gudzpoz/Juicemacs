package party.iroiro.juicemacs.elisp.runtime.internal;

import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.util.Arrays;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;

public final class ELispKboard {
    public ELispKboard() {
        kboardLocalFields = new Object[18];
        Arrays.fill(kboardLocalFields, false);
    }

    public Object getSlot(int index) {
        return kboardLocalFields[index];
    }

    public void setSlot(int index, Object value) {
        kboardLocalFields[index] = value;
    }

    //#region struct kboard
    private final Object[] kboardLocalFields;
    /**
     * If non-nil, a keymap that overrides all others but applies only to
     * this KBOARD.  Lisp code that uses this instead of calling read-char
     * can effectively wait for input in the any-kboard state, and hence
     * avoid blocking out the other KBOARDs.  See universal-argument in
     * lisp/simple.el for an example.
     */
    public final static int KVAR_VOVERRIDING_TERMINAL_LOCAL_MAP = 0;
    /**
     * Last command executed by the editor command loop, not counting
     * commands that set the prefix argument.
     */
    public final static int KVAR_VLAST_COMMAND = 1;
    /**
     * Normally same as last-command, but never modified by other commands.
     */
    public final static int KVAR_VREAL_LAST_COMMAND = 2;
    /**
     * User-supplied table to translate input characters through.
     */
    public final static int KVAR_VKEYBOARD_TRANSLATE_TABLE = 3;
    /**
     * Last command that may be repeated by `repeat'.
     */
    public final static int KVAR_VLAST_REPEATABLE_COMMAND = 4;
    /**
     * The prefix argument for the next command, in raw form.
     */
    public final static int KVAR_VPREFIX_ARG = 5;
    /**
     * Saved prefix argument for the last command, in raw form.
     */
    public final static int KVAR_VLAST_PREFIX_ARG = 6;
    /**
     * Unread events specific to this kboard.
     */
    public final static int KVAR_KBD_QUEUE = 7;
    /**
     * Non-nil while a kbd macro is being defined.
     */
    public final static int KVAR_DEFINING_KBD_MACRO = 8;
    /**
     * Last anonymous kbd macro defined.
     */
    public final static int KVAR_VLAST_KBD_MACRO = 9;
    /**
     * Alist of system-specific X windows key symbols.
     */
    public final static int KVAR_VSYSTEM_KEY_ALIST = 10;
    /**
     * Cache for modify_event_symbol.
     */
    public final static int KVAR_SYSTEM_KEY_SYMS = 11;
    /**
     * The kind of display: x, w32, ...
     */
    public final static int KVAR_VWINDOW_SYSTEM = 12;
    /**
     * Keymap mapping keys to alternative preferred forms.
     * See the DEFVAR for more documentation.
     */
    public final static int KVAR_VLOCAL_FUNCTION_KEY_MAP = 13;
    /**
     * Keymap mapping ASCII function key sequences onto their preferred
     * forms.  Initialized by the terminal-specific lisp files.  See the
     * DEFVAR for more documentation.
     */
    public final static int KVAR_VINPUT_DECODE_MAP = 14;
    /**
     * Minibufferless frames on this display use this frame's minibuffer.
     */
    public final static int KVAR_VDEFAULT_MINIBUFFER_FRAME = 15;
    /**
     * The text we're echoing in the modeline - partial key sequences,
     * usually.  This is nil when not echoing.
     */
    public final static int KVAR_ECHO_STRING = 16;
    /**
     * If we have a prompt string specified by the user, this is it.
     */
    public final static int KVAR_ECHO_PROMPT = 17;
    public Object getVoverridingTerminalLocalMap() { return kboardLocalFields[KVAR_VOVERRIDING_TERMINAL_LOCAL_MAP]; }
    public void setVoverridingTerminalLocalMap(Object value) { kboardLocalFields[KVAR_VOVERRIDING_TERMINAL_LOCAL_MAP] = value; }
    public Object getVlastCommand() { return kboardLocalFields[KVAR_VLAST_COMMAND]; }
    public void setVlastCommand(Object value) { kboardLocalFields[KVAR_VLAST_COMMAND] = value; }
    public Object getVrealLastCommand() { return kboardLocalFields[KVAR_VREAL_LAST_COMMAND]; }
    public void setVrealLastCommand(Object value) { kboardLocalFields[KVAR_VREAL_LAST_COMMAND] = value; }
    public Object getVkeyboardTranslateTable() { return kboardLocalFields[KVAR_VKEYBOARD_TRANSLATE_TABLE]; }
    public void setVkeyboardTranslateTable(Object value) { kboardLocalFields[KVAR_VKEYBOARD_TRANSLATE_TABLE] = value; }
    public Object getVlastRepeatableCommand() { return kboardLocalFields[KVAR_VLAST_REPEATABLE_COMMAND]; }
    public void setVlastRepeatableCommand(Object value) { kboardLocalFields[KVAR_VLAST_REPEATABLE_COMMAND] = value; }
    public Object getVprefixArg() { return kboardLocalFields[KVAR_VPREFIX_ARG]; }
    public void setVprefixArg(Object value) { kboardLocalFields[KVAR_VPREFIX_ARG] = value; }
    public Object getVlastPrefixArg() { return kboardLocalFields[KVAR_VLAST_PREFIX_ARG]; }
    public void setVlastPrefixArg(Object value) { kboardLocalFields[KVAR_VLAST_PREFIX_ARG] = value; }
    public Object getKbdQueue() { return kboardLocalFields[KVAR_KBD_QUEUE]; }
    public void setKbdQueue(Object value) { kboardLocalFields[KVAR_KBD_QUEUE] = value; }
    public Object getDefiningKbdMacro() { return kboardLocalFields[KVAR_DEFINING_KBD_MACRO]; }
    public void setDefiningKbdMacro(Object value) { kboardLocalFields[KVAR_DEFINING_KBD_MACRO] = value; }
    public Object getVlastKbdMacro() { return kboardLocalFields[KVAR_VLAST_KBD_MACRO]; }
    public void setVlastKbdMacro(Object value) { kboardLocalFields[KVAR_VLAST_KBD_MACRO] = value; }
    public Object getVsystemKeyAlist() { return kboardLocalFields[KVAR_VSYSTEM_KEY_ALIST]; }
    public void setVsystemKeyAlist(Object value) { kboardLocalFields[KVAR_VSYSTEM_KEY_ALIST] = value; }
    public Object getSystemKeySyms() { return kboardLocalFields[KVAR_SYSTEM_KEY_SYMS]; }
    public void setSystemKeySyms(Object value) { kboardLocalFields[KVAR_SYSTEM_KEY_SYMS] = value; }
    public Object getVwindowSystem() { return kboardLocalFields[KVAR_VWINDOW_SYSTEM]; }
    public void setVwindowSystem(Object value) { kboardLocalFields[KVAR_VWINDOW_SYSTEM] = value; }
    public Object getVlocalFunctionKeyMap() { return kboardLocalFields[KVAR_VLOCAL_FUNCTION_KEY_MAP]; }
    public void setVlocalFunctionKeyMap(Object value) { kboardLocalFields[KVAR_VLOCAL_FUNCTION_KEY_MAP] = value; }
    public Object getVinputDecodeMap() { return kboardLocalFields[KVAR_VINPUT_DECODE_MAP]; }
    public void setVinputDecodeMap(Object value) { kboardLocalFields[KVAR_VINPUT_DECODE_MAP] = value; }
    public Object getVdefaultMinibufferFrame() { return kboardLocalFields[KVAR_VDEFAULT_MINIBUFFER_FRAME]; }
    public void setVdefaultMinibufferFrame(Object value) { kboardLocalFields[KVAR_VDEFAULT_MINIBUFFER_FRAME] = value; }
    public Object getEchoString() { return kboardLocalFields[KVAR_ECHO_STRING]; }
    public void setEchoString(Object value) { kboardLocalFields[KVAR_ECHO_STRING] = value; }
    public Object getEchoPrompt() { return kboardLocalFields[KVAR_ECHO_PROMPT]; }
    public void setEchoPrompt(Object value) { kboardLocalFields[KVAR_ECHO_PROMPT] = value; }
    //#endregion struct kboard

    @SuppressWarnings("DuplicatedCode")
    public static void initKboardLocalVars(ELispContext context) {
        //#region init_kboard_once
        context.forwardTo(OVERRIDING_TERMINAL_LOCAL_MAP, new ValueStorage.ForwardedPerKboard(KVAR_VOVERRIDING_TERMINAL_LOCAL_MAP));
        context.forwardTo(LAST_COMMAND, new ValueStorage.ForwardedPerKboard(KVAR_VLAST_COMMAND));
        context.forwardTo(REAL_LAST_COMMAND, new ValueStorage.ForwardedPerKboard(KVAR_VREAL_LAST_COMMAND));
        context.forwardTo(KEYBOARD_TRANSLATE_TABLE, new ValueStorage.ForwardedPerKboard(KVAR_VKEYBOARD_TRANSLATE_TABLE));
        context.forwardTo(LAST_REPEATABLE_COMMAND, new ValueStorage.ForwardedPerKboard(KVAR_VLAST_REPEATABLE_COMMAND));
        context.forwardTo(PREFIX_ARG, new ValueStorage.ForwardedPerKboard(KVAR_VPREFIX_ARG));
        context.forwardTo(LAST_PREFIX_ARG, new ValueStorage.ForwardedPerKboard(KVAR_VLAST_PREFIX_ARG));
        context.forwardTo(DEFINING_KBD_MACRO, new ValueStorage.ForwardedPerKboard(KVAR_DEFINING_KBD_MACRO));
        context.forwardTo(LAST_KBD_MACRO, new ValueStorage.ForwardedPerKboard(KVAR_VLAST_KBD_MACRO));
        context.forwardTo(SYSTEM_KEY_ALIST, new ValueStorage.ForwardedPerKboard(KVAR_VSYSTEM_KEY_ALIST));
        context.forwardTo(WINDOW_SYSTEM, new ValueStorage.ForwardedPerKboard(KVAR_VWINDOW_SYSTEM));
        context.forwardTo(LOCAL_FUNCTION_KEY_MAP, new ValueStorage.ForwardedPerKboard(KVAR_VLOCAL_FUNCTION_KEY_MAP));
        context.forwardTo(INPUT_DECODE_MAP, new ValueStorage.ForwardedPerKboard(KVAR_VINPUT_DECODE_MAP));
        context.forwardTo(DEFAULT_MINIBUFFER_FRAME, new ValueStorage.ForwardedPerKboard(KVAR_VDEFAULT_MINIBUFFER_FRAME));
        //#endregion init_kboard_once
    }
}
