package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

public class BuiltInBuffer extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInBufferFactory.getFactories();
    }

    @ELispBuiltIn(name = "buffer-live-p", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a buffer which has not been killed.\nValue is nil if OBJECT is not a buffer or if it has been killed.")
    @GenerateNodeFactory
    public abstract static class FBufferLiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLiveP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-list", minArgs = 0, maxArgs = 1, doc = "Return a list of all live buffers.\nIf the optional arg FRAME is a frame, return the buffer list in the\nproper order for that frame: the buffers shown in FRAME come first,\nfollowed by the rest of the buffers.")
    @GenerateNodeFactory
    public abstract static class FBufferList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferList(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-buffer", minArgs = 1, maxArgs = 1, doc = "Return the buffer named BUFFER-OR-NAME.\nBUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME\nis a string and there is no buffer with that name, return nil.  If\nBUFFER-OR-NAME is a buffer, return it as given.")
    @GenerateNodeFactory
    public abstract static class FGetBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-file-buffer", minArgs = 1, maxArgs = 1, doc = "Return the buffer visiting file FILENAME (a string).\nThe buffer's `buffer-file-name' must match exactly the expansion of FILENAME.\nIf there is no such live buffer, return nil.\nSee also `find-buffer-visiting'.")
    @GenerateNodeFactory
    public abstract static class FGetFileBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getFileBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-truename-buffer", minArgs = 1, maxArgs = 1, doc = "Return the buffer with `file-truename' equal to FILENAME (a string).\nIf there is no such live buffer, return nil.\nSee also `find-buffer-visiting'.")
    @GenerateNodeFactory
    public abstract static class FGetTruenameBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getTruenameBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "find-buffer", minArgs = 2, maxArgs = 2, doc = "Return the buffer with buffer-local VARIABLE `equal' to VALUE.\nIf there is no such live buffer, return nil.\nSee also `find-buffer-visiting'.")
    @GenerateNodeFactory
    public abstract static class FFindBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findBuffer(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-buffer-create", minArgs = 1, maxArgs = 2, doc = "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.\nIf BUFFER-OR-NAME is a string and a live buffer with that name exists,\nreturn that buffer.  If no such buffer exists, create a new buffer with\nthat name and return it.\n\nIf BUFFER-OR-NAME starts with a space, the new buffer does not keep undo\ninformation.  If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the\nnew buffer does not run the hooks `kill-buffer-hook',\n`kill-buffer-query-functions', and `buffer-list-update-hook'.  This\navoids slowing down internal or temporary buffers that are never\npresented to users or passed on to other applications.\n\nIf BUFFER-OR-NAME is a buffer instead of a string, return it as given,\neven if it is dead.  The return value is never nil.")
    @GenerateNodeFactory
    public abstract static class FGetBufferCreate extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBufferCreate(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-indirect-buffer", minArgs = 2, maxArgs = 4, doc = "Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.\nBASE-BUFFER should be a live buffer, or the name of an existing buffer.\n\nNAME should be a string which is not the name of an existing buffer.\n\nInteractively, prompt for BASE-BUFFER (offering the current buffer as\nthe default), and for NAME (offering as default the name of a recently\nused buffer).\n\nOptional argument CLONE non-nil means preserve BASE-BUFFER's state,\nsuch as major and minor modes, in the indirect buffer.\nCLONE nil means the indirect buffer's state is reset to default values.\n\nIf optional argument INHIBIT-BUFFER-HOOKS is non-nil, the new buffer\ndoes not run the hooks `kill-buffer-hook',\n`kill-buffer-query-functions', and `buffer-list-update-hook'.\n\nInteractively, CLONE and INHIBIT-BUFFER-HOOKS are nil.")
    @GenerateNodeFactory
    public abstract static class FMakeIndirectBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeIndirectBuffer(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "generate-new-buffer-name", minArgs = 1, maxArgs = 2, doc = "Return a string that is the name of no existing buffer based on NAME.\nIf there is no live buffer named NAME, then return NAME.\nOtherwise modify name by appending `<NUMBER>', incrementing NUMBER\n\\(starting at 2) until an unused name is found, and then return that name.\nOptional second argument IGNORE specifies a name that is okay to use (if\nit is in the sequence to be tried) even if a buffer with that name exists.\n\nIf NAME begins with a space (i.e., a buffer that is not normally\nvisible to users), then if buffer NAME already exists a random number\nis first appended to NAME, to speed up finding a non-existent buffer.")
    @GenerateNodeFactory
    public abstract static class FGenerateNewBufferName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object generateNewBufferName(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-name", minArgs = 0, maxArgs = 1, doc = "Return the name of BUFFER, as a string.\nBUFFER defaults to the current buffer.\nReturn nil if BUFFER has been killed.")
    @GenerateNodeFactory
    public abstract static class FBufferName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-last-name", minArgs = 0, maxArgs = 1, doc = "Return last name of BUFFER, as a string.\nBUFFER defaults to the current buffer.\n\nThis is the name BUFFER had before the last time it was renamed or\nimmediately before it was killed.")
    @GenerateNodeFactory
    public abstract static class FBufferLastName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLastName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-file-name", minArgs = 0, maxArgs = 1, doc = "Return name of file BUFFER is visiting, or nil if none.\nNo argument or nil as argument means use the current buffer.")
    @GenerateNodeFactory
    public abstract static class FBufferFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferFileName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-base-buffer", minArgs = 0, maxArgs = 1, doc = "Return the base buffer of indirect buffer BUFFER.\nIf BUFFER is not indirect, return nil.\nBUFFER defaults to the current buffer.")
    @GenerateNodeFactory
    public abstract static class FBufferBaseBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferBaseBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-local-value", minArgs = 2, maxArgs = 2, doc = "Return the value of VARIABLE in BUFFER.\nIf VARIABLE does not have a buffer-local binding in BUFFER, the value\nis the default binding of the variable.")
    @GenerateNodeFactory
    public abstract static class FBufferLocalValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLocalValue(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-local-variables", minArgs = 0, maxArgs = 1, doc = "Return an alist of variables that are buffer-local in BUFFER.\nMost elements look like (SYMBOL . VALUE), describing one variable.\nFor a symbol that is locally unbound, just the symbol appears in the value.\nNote that storing new VALUEs in these elements doesn't change the variables.\nNo argument or nil as argument means use current buffer as BUFFER.")
    @GenerateNodeFactory
    public abstract static class FBufferLocalVariables extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLocalVariables(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-modified-p", minArgs = 0, maxArgs = 1, doc = "Return non-nil if BUFFER was modified since its file was last read or saved.\nNo argument or nil as argument means use current buffer as BUFFER.\n\nIf BUFFER was autosaved since it was last modified, this function\nreturns the symbol `autosaved'.")
    @GenerateNodeFactory
    public abstract static class FBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferModifiedP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "force-mode-line-update", minArgs = 0, maxArgs = 1, doc = "Force redisplay of the current buffer's mode line and header line.\nWith optional non-nil ALL, force redisplay of all mode lines, tab lines and\nheader lines.  This function also forces recomputation of the\nmenu bar menus and the frame title.")
    @GenerateNodeFactory
    public abstract static class FForceModeLineUpdate extends ELispBuiltInBaseNode {
        @Specialization
        public static Object forceModeLineUpdate(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-modified-p", minArgs = 1, maxArgs = 1, doc = "Mark current buffer as modified or unmodified according to FLAG.\nA non-nil FLAG means mark the buffer modified.\nIn addition, this function unconditionally forces redisplay of the\nmode lines of the windows that display the current buffer, and also\nlocks or unlocks the file visited by the buffer, depending on whether\nthe function's argument is non-nil, but only if both `buffer-file-name'\nand `buffer-file-truename' are non-nil.")
    @GenerateNodeFactory
    public abstract static class FSetBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferModifiedP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "restore-buffer-modified-p", minArgs = 1, maxArgs = 1, doc = "Like `set-buffer-modified-p', but doesn't redisplay buffer's mode line.\nA nil FLAG means to mark the buffer as unmodified.  A non-nil FLAG\nmeans mark the buffer as modified.  A special value of `autosaved'\nwill mark the buffer as modified and also as autosaved since it was\nlast modified.\n\nThis function also locks or unlocks the file visited by the buffer,\nif both `buffer-file-truename' and `buffer-file-name' are non-nil.\n\nIt is not ensured that mode lines will be updated to show the modified\nstate of the current buffer.  Use with care.")
    @GenerateNodeFactory
    public abstract static class FRestoreBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object restoreBufferModifiedP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-modified-tick", minArgs = 0, maxArgs = 1, doc = "Return BUFFER's tick counter, incremented for each change in text.\nEach buffer has a tick counter which is incremented each time the\ntext in that buffer is changed.  No argument or nil as argument means\nuse current buffer as BUFFER.")
    @GenerateNodeFactory
    public abstract static class FBufferModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferModifiedTick(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--set-buffer-modified-tick", minArgs = 1, maxArgs = 2, doc = "Set BUFFER's tick counter to TICK.\nNo argument or nil as argument means use current buffer as BUFFER.")
    @GenerateNodeFactory
    public abstract static class FInternalSetBufferModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalSetBufferModifiedTick(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-chars-modified-tick", minArgs = 0, maxArgs = 1, doc = "Return BUFFER's character-change tick counter.\nEach buffer has a character-change tick counter, which is set to the\nvalue of the buffer's tick counter (see `buffer-modified-tick'), each\ntime text in that buffer is inserted or deleted.  By comparing the\nvalues returned by two individual calls of `buffer-chars-modified-tick',\nyou can tell whether a character change occurred in that buffer in\nbetween these calls.  No argument or nil as argument means use current\nbuffer as BUFFER.")
    @GenerateNodeFactory
    public abstract static class FBufferCharsModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferCharsModifiedTick(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rename-buffer", minArgs = 1, maxArgs = 2, doc = "Change current buffer's name to NEWNAME (a string).\nIf second arg UNIQUE is nil or omitted, it is an error if a\nbuffer named NEWNAME already exists.\nIf UNIQUE is non-nil, come up with a new name using\n`generate-new-buffer-name'.\nInteractively, you can set UNIQUE with a prefix argument.\nWe return the name we actually gave the buffer.\nThis does not change the name of the visited file (if any).")
    @GenerateNodeFactory
    public abstract static class FRenameBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object renameBuffer(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "other-buffer", minArgs = 0, maxArgs = 3, doc = "Return most recently selected buffer other than BUFFER.\nBuffers not visible in windows are preferred to visible buffers, unless\noptional second argument VISIBLE-OK is non-nil.  Ignore the argument\nBUFFER unless it denotes a live buffer.  If the optional third argument\nFRAME specifies a live frame, then use that frame's buffer list instead\nof the selected frame's buffer list.\n\nThe buffer is found by scanning the selected or specified frame's buffer\nlist first, followed by the list of all buffers.  If no other buffer\nexists, return the buffer `*scratch*' (creating it if necessary).")
    @GenerateNodeFactory
    public abstract static class FOtherBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object otherBuffer(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-enable-undo", minArgs = 0, maxArgs = 1, doc = "Start keeping undo information for buffer BUFFER.\nNo argument or nil as argument means do this for the current buffer.")
    @GenerateNodeFactory
    public abstract static class FBufferEnableUndo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferEnableUndo(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-buffer", minArgs = 0, maxArgs = 1, doc = "Kill the buffer specified by BUFFER-OR-NAME.\nThe argument may be a buffer or the name of an existing buffer.\nArgument nil or omitted means kill the current buffer.  Return t if the\nbuffer is actually killed, nil otherwise.\n\nThe functions in `kill-buffer-query-functions' are called with the\nbuffer to be killed as the current buffer.  If any of them returns nil,\nthe buffer is not killed.  The hook `kill-buffer-hook' is run before the\nbuffer is actually killed.  The buffer being killed will be current\nwhile the hook is running.  Functions called by any of these hooks are\nsupposed to not change the current buffer.  Neither hook is run for\ninternal or temporary buffers created by `get-buffer-create' or\n`generate-new-buffer' with argument INHIBIT-BUFFER-HOOKS non-nil.\n\nAny processes that have this buffer as the `process-buffer' are killed\nwith SIGHUP.  This function calls `replace-buffer-in-windows' for\ncleaning up all windows currently displaying the buffer to be killed.")
    @GenerateNodeFactory
    public abstract static class FKillBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bury-buffer-internal", minArgs = 1, maxArgs = 1, doc = "Move BUFFER to the end of the buffer list.")
    @GenerateNodeFactory
    public abstract static class FBuryBufferInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object buryBufferInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-major-mode", minArgs = 1, maxArgs = 1, doc = "Set an appropriate major mode for BUFFER.\nFor the *scratch* buffer, use `initial-major-mode', otherwise choose a mode\naccording to the default value of `major-mode'.\nUse this function before selecting the buffer, since it may need to inspect\nthe current buffer's major mode.")
    @GenerateNodeFactory
    public abstract static class FSetBufferMajorMode extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferMajorMode(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "current-buffer", minArgs = 0, maxArgs = 0, doc = "Return the current buffer as a Lisp object.")
    @GenerateNodeFactory
    public abstract static class FCurrentBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object currentBuffer() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer", minArgs = 1, maxArgs = 1, doc = "Make buffer BUFFER-OR-NAME current for editing operations.\nBUFFER-OR-NAME may be a buffer or the name of an existing buffer.\nSee also `with-current-buffer' when you want to make a buffer current\ntemporarily.  This function does not display the buffer, so its effect\nends when the current command terminates.  Use `switch-to-buffer' or\n`pop-to-buffer' to switch buffers permanently.\nThe return value is the buffer made current.")
    @GenerateNodeFactory
    public abstract static class FSetBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBuffer(ELispString a) {
            // TODO: Real buffers
            return NIL;
        }
    }

    @ELispBuiltIn(name = "barf-if-buffer-read-only", minArgs = 0, maxArgs = 1, doc = "Signal a `buffer-read-only' error if the current buffer is read-only.\nIf the text under POSITION (which defaults to point) has the\n`inhibit-read-only' text property set, the error will not be raised.")
    @GenerateNodeFactory
    public abstract static class FBarfIfBufferReadOnly extends ELispBuiltInBaseNode {
        @Specialization
        public static Object barfIfBufferReadOnly(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "erase-buffer", minArgs = 0, maxArgs = 0, doc = "Delete the entire contents of the current buffer.\nAny narrowing restriction in effect (see `narrow-to-region') is removed,\nso the buffer is truly empty after this.")
    @GenerateNodeFactory
    public abstract static class FEraseBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eraseBuffer() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-swap-text", minArgs = 1, maxArgs = 1, doc = "Swap the text between current buffer and BUFFER.\nUsing this function from `save-excursion' might produce surprising\nresults, see Info node `(elisp)Swapping Text'.")
    @GenerateNodeFactory
    public abstract static class FBufferSwapText extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSwapText(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-multibyte", minArgs = 1, maxArgs = 1, doc = "Set the multibyte flag of the current buffer to FLAG.\nIf FLAG is t, this makes the buffer a multibyte buffer.\nIf FLAG is nil, this makes the buffer a single-byte buffer.\nIn these cases, the buffer contents remain unchanged as a sequence of\nbytes but the contents viewed as characters do change.\nIf FLAG is `to', this makes the buffer a multibyte buffer by changing\nall eight-bit bytes to eight-bit characters.\nIf the multibyte flag was really changed, undo information of the\ncurrent buffer is cleared.")
    @GenerateNodeFactory
    public abstract static class FSetBufferMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-all-local-variables", minArgs = 0, maxArgs = 1, doc = "Switch to Fundamental mode by killing current buffer's local variables.\nMost local variable bindings are eliminated so that the default values\nbecome effective once more.  Also, the syntax table is set from\n`standard-syntax-table', the local keymap is set to nil,\nand the abbrev table from `fundamental-mode-abbrev-table'.\nThis function also forces redisplay of the mode line.\n\nEvery function to select a new major mode starts by\ncalling this function.\n\nAs a special exception, local variables whose names have a non-nil\n`permanent-local' property are not eliminated by this function.  If\nthe optional KILL-PERMANENT argument is non-nil, clear out these local\nvariables, too.\n\nThe first thing this function does is run\nthe normal hook `change-major-mode-hook'.")
    @GenerateNodeFactory
    public abstract static class FKillAllLocalVariables extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killAllLocalVariables(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlayp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is an overlay.")
    @GenerateNodeFactory
    public abstract static class FOverlayp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-overlay", minArgs = 2, maxArgs = 5, doc = "Create a new overlay with range BEG to END in BUFFER and return it.\nIf omitted, BUFFER defaults to the current buffer.\nBEG and END may be integers or markers.\nThe fourth arg FRONT-ADVANCE, if non-nil, makes the marker\nfor the front of the overlay advance when text is inserted there\n\\(which means the text *is not* included in the overlay).\nThe fifth arg REAR-ADVANCE, if non-nil, makes the marker\nfor the rear of the overlay advance when text is inserted there\n\\(which means the text *is* included in the overlay).")
    @GenerateNodeFactory
    public abstract static class FMakeOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeOverlay(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "move-overlay", minArgs = 3, maxArgs = 4, doc = "Set the endpoints of OVERLAY to BEG and END in BUFFER.\nIf BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.\nIf BUFFER is omitted, and OVERLAY is in no buffer, put it in the current\nbuffer.")
    @GenerateNodeFactory
    public abstract static class FMoveOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object moveOverlay(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-overlay", minArgs = 1, maxArgs = 1, doc = "Delete the overlay OVERLAY from its buffer.")
    @GenerateNodeFactory
    public abstract static class FDeleteOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteOverlay(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-all-overlays", minArgs = 0, maxArgs = 1, doc = "Delete all overlays of BUFFER.\nBUFFER omitted or nil means delete all overlays of the current\nbuffer.")
    @GenerateNodeFactory
    public abstract static class FDeleteAllOverlays extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteAllOverlays(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-start", minArgs = 1, maxArgs = 1, doc = "Return the position at which OVERLAY starts.")
    @GenerateNodeFactory
    public abstract static class FOverlayStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayStart(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-end", minArgs = 1, maxArgs = 1, doc = "Return the position at which OVERLAY ends.")
    @GenerateNodeFactory
    public abstract static class FOverlayEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayEnd(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-buffer", minArgs = 1, maxArgs = 1, doc = "Return the buffer OVERLAY belongs to.\nReturn nil if OVERLAY has been deleted.")
    @GenerateNodeFactory
    public abstract static class FOverlayBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-properties", minArgs = 1, maxArgs = 1, doc = "Return a list of the properties on OVERLAY.\nThis is a copy of OVERLAY's plist; modifying its conses has no effect on\nOVERLAY.")
    @GenerateNodeFactory
    public abstract static class FOverlayProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayProperties(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlays-at", minArgs = 1, maxArgs = 2, doc = "Return a list of the overlays that contain the character at POS.\nIf SORTED is non-nil, then sort them by decreasing priority.\n\nZero-length overlays that start and stop at POS are not included in\nthe return value.  Instead use `overlays-in' if those overlays are of\ninterest.")
    @GenerateNodeFactory
    public abstract static class FOverlaysAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlaysAt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlays-in", minArgs = 2, maxArgs = 2, doc = "Return a list of the overlays that overlap the region BEG ... END.\nOverlap means that at least one character is contained within the overlay\nand also contained within the specified region.\n\nEmpty overlays are included in the result if they are located at BEG,\nbetween BEG and END, or at END provided END denotes the position at the\nend of the accessible part of the buffer.\n\nThe resulting list of overlays is in an arbitrary unpredictable order.")
    @GenerateNodeFactory
    public abstract static class FOverlaysIn extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlaysIn(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "next-overlay-change", minArgs = 1, maxArgs = 1, doc = "Return the next position after POS where an overlay starts or ends.\nIf there are no overlay boundaries from POS to (point-max),\nthe value is (point-max).")
    @GenerateNodeFactory
    public abstract static class FNextOverlayChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nextOverlayChange(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "previous-overlay-change", minArgs = 1, maxArgs = 1, doc = "Return the previous position before POS where an overlay starts or ends.\nIf there are no overlay boundaries from (point-min) to POS,\nthe value is (point-min).")
    @GenerateNodeFactory
    public abstract static class FPreviousOverlayChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object previousOverlayChange(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-lists", minArgs = 0, maxArgs = 0, doc = "Return a list giving all the overlays of the current buffer.\n\nFor backward compatibility, the value is actually a list that\nholds another list; the overlays are in the inner list.\nThe list you get is a copy, so that changing it has no effect.\nHowever, the overlays you get are the real objects that the buffer uses.")
    @GenerateNodeFactory
    public abstract static class FOverlayLists extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayLists() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-recenter", minArgs = 1, maxArgs = 1, doc = "Recenter the overlays of the current buffer around position POS.\nThat makes overlay lookup faster for positions near POS (but perhaps slower\nfor positions far away from POS).\n\nSince Emacs 29.1, this function is a no-op, because the implementation\nof overlays changed and their lookup is now fast regardless of their\nposition in the buffer.  In particular, this function no longer affects\nthe value returned by `overlay-lists'.")
    @GenerateNodeFactory
    public abstract static class FOverlayRecenter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayRecenter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-get", minArgs = 2, maxArgs = 2, doc = "Get the property of overlay OVERLAY with property name PROP.")
    @GenerateNodeFactory
    public abstract static class FOverlayGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayGet(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-put", minArgs = 3, maxArgs = 3, doc = "Set one property of overlay OVERLAY: give property PROP value VALUE.\nVALUE will be returned.")
    @GenerateNodeFactory
    public abstract static class FOverlayPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayPut(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-tree", minArgs = 0, maxArgs = 1, doc = "Get the overlay tree for BUFFER.")
    @GenerateNodeFactory
    public abstract static class FOverlayTree extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayTree(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
