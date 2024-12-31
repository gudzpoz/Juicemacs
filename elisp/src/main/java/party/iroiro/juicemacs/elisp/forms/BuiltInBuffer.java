package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.HashMap;
import java.util.List;
import java.util.Random;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInBuffer extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInBufferFactory.getFactories();
    }

    private final HashMap<MuleString, ELispBuffer> buffers = new HashMap<>();
    private final ValueStorage.Forwarded minibufferList = new ValueStorage.Forwarded();

    @CompilerDirectives.TruffleBoundary
    @Nullable
    private static ELispBuffer getBuffer(MuleString name) {
        // TODO: Handle name changes?
        return ELispContext.get(null).globals().builtInBuffer.buffers.get(name);
    }
    @CompilerDirectives.TruffleBoundary
    private static void putBuffer(MuleString name, ELispBuffer buffer) {
        ELispContext.get(null).globals().builtInBuffer.buffers.put(name, buffer);
    }
    @CompilerDirectives.TruffleBoundary
    private static Object getBufferList() {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (ELispBuffer buffer : ELispContext.get(null).globals().builtInBuffer.buffers.values()) {
            builder.add(buffer);
        }
        return builder.build();
    }

    public static int downCase(int c, ELispBuffer buffer) {
        Object down = asCharTable(buffer.getDowncaseTable()).getChar(c);
        return down instanceof Long l ? l.intValue() : c;
    }
    public static int upCase(int c, ELispBuffer buffer) {
        Object up = asCharTable(buffer.getUpcaseTable()).getChar(c);
        return up instanceof Long l ? l.intValue() : c;
    }
    public static boolean upperCaseP(int c, ELispBuffer buffer) {
        return downCase(c, buffer) != c;
    }
    public static boolean lowerCaseP(int c, ELispBuffer buffer) {
        return !upperCaseP(c, buffer) && upCase(c, buffer) != c;
    }

    public static ELispBuffer getMiniBuffer(int depth) {
        ValueStorage.Forwarded miniBuffers = ELispContext.get(null).globals().builtInBuffer.minibufferList;
        Object tail = BuiltInFns.FNthcdr.nthcdr(depth, miniBuffers.getValue());
        if (isNil(tail)) {
            tail = new ELispCons(false);
            miniBuffers.setValue(BuiltInFns.FNconc.nconc(new Object[]{miniBuffers.getValue(), tail}));
        }
        Object buffer = BuiltInData.FCar.car(tail);
        if (buffer instanceof ELispBuffer buf && buf.isLive()) {
            // TODO: reset
            buf.resetLocalVariables(true);
            return buf;
        } else {
            ELispBuffer newBuffer = FGetBufferCreate.getBufferCreate(new ELispString(" *Minibuf-" + depth + "*"), false);
            BuiltInData.FSetcar.setcar(asCons(tail), newBuffer);
            FBufferEnableUndo.bufferEnableUndo(newBuffer);
            return newBuffer;
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a buffer which has not been killed.
     * Value is nil if OBJECT is not a buffer or if it has been killed.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-live-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferLiveP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of all live buffers.
     * If the optional arg FRAME is a frame, return the buffer list in the
     * proper order for that frame: the buffers shown in FRAME come first,
     * followed by the rest of the buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-list", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferList(Object frame) {
            // TODO: Util we have frames
            return getBufferList();
        }
    }

    /**
     * <pre>
     * Return the buffer named BUFFER-OR-NAME.
     * BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
     * is a string and there is no buffer with that name, return nil.  If
     * BUFFER-OR-NAME is a buffer, return it as given.
     * </pre>
     */
    @ELispBuiltIn(name = "get-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBuffer(Object bufferOrName) {
            if (bufferOrName instanceof ELispBuffer) {
                return bufferOrName;
            }
            ELispBuffer buffer = BuiltInBuffer.getBuffer(asStr(bufferOrName).value());
            return buffer == null ? false : buffer;
        }
    }

    /**
     * <pre>
     * Return the buffer visiting file FILENAME (a string).
     * The buffer's `buffer-file-name' must match exactly the expansion of FILENAME.
     * If there is no such live buffer, return nil.
     * See also `find-buffer-visiting'.
     * </pre>
     */
    @ELispBuiltIn(name = "get-file-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetFileBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getFileBuffer(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer with `file-truename' equal to FILENAME (a string).
     * If there is no such live buffer, return nil.
     * See also `find-buffer-visiting'.
     * </pre>
     */
    @ELispBuiltIn(name = "get-truename-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetTruenameBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getTruenameBuffer(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer with buffer-local VARIABLE `equal' to VALUE.
     * If there is no such live buffer, return nil.
     * See also `find-buffer-visiting'.
     * </pre>
     */
    @ELispBuiltIn(name = "find-buffer", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFindBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void findBuffer(Object variable, Object value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
     * If BUFFER-OR-NAME is a string and a live buffer with that name exists,
     * return that buffer.  If no such buffer exists, create a new buffer with
     * that name and return it.
     *
     * If BUFFER-OR-NAME starts with a space, the new buffer does not keep undo
     * information.  If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the
     * new buffer does not run the hooks `kill-buffer-hook',
     * `kill-buffer-query-functions', and `buffer-list-update-hook'.  This
     * avoids slowing down internal or temporary buffers that are never
     * presented to users or passed on to other applications.
     *
     * If BUFFER-OR-NAME is a buffer instead of a string, return it as given,
     * even if it is dead.  The return value is never nil.
     * </pre>
     */
    @ELispBuiltIn(name = "get-buffer-create", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetBufferCreate extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispBuffer getBufferCreate(Object bufferOrName, Object inhibitBufferHooks) {
            Object object = FGetBuffer.getBuffer(bufferOrName);
            if (object instanceof ELispBuffer buffer) {
                return buffer;
            }

            ELispContext context = ELispContext.get(null);
            ELispBuffer bufferDefaults = context.globals().getBufferDefaults();

            MuleString name = asStr(bufferOrName).value();
            ELispBuffer buffer = new ELispBuffer(bufferDefaults, !isNil(inhibitBufferHooks));
            buffer.setWidthTable(false);
            // TODO: Texts
            buffer.setName(new ELispString(name));
            buffer.setLastName(buffer.getName());
            buffer.setUndoList(name.startsWith(" "));
            buffer.setMark(BuiltInAlloc.FMakeMarker.makeMarker());
            putBuffer(name, buffer);
            // TODO: run_buffer_list_update_hook
            return buffer;
        }
    }

    /**
     * <pre>
     * Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.
     * BASE-BUFFER should be a live buffer, or the name of an existing buffer.
     *
     * NAME should be a string which is not the name of an existing buffer.
     *
     * Interactively, prompt for BASE-BUFFER (offering the current buffer as
     * the default), and for NAME (offering as default the name of a recently
     * used buffer).
     *
     * Optional argument CLONE non-nil means preserve BASE-BUFFER's state,
     * such as major and minor modes, in the indirect buffer.
     * CLONE nil means the indirect buffer's state is reset to default values.
     *
     * If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the new buffer
     * does not run the hooks `kill-buffer-hook',
     * `kill-buffer-query-functions', and `buffer-list-update-hook'.
     *
     * Interactively, CLONE and INHIBIT-BUFFER-HOOKS are nil.
     * </pre>
     */
    @ELispBuiltIn(name = "make-indirect-buffer", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FMakeIndirectBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeIndirectBuffer(Object baseBuffer, Object name, Object clone, Object inhibitBufferHooks) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a string that is the name of no existing buffer based on NAME.
     * If there is no live buffer named NAME, then return NAME.
     * Otherwise modify name by appending `&lt;NUMBER&gt;', incrementing NUMBER
     * \(starting at 2) until an unused name is found, and then return that name.
     * Optional second argument IGNORE specifies a name that is okay to use (if
     * it is in the sequence to be tried) even if a buffer with that name exists.
     *
     * If NAME begins with a space (i.e., a buffer that is not normally
     * visible to users), then if buffer NAME already exists a random number
     * is first appended to NAME, to speed up finding a non-existent buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "generate-new-buffer-name", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGenerateNewBufferName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString generateNewBufferName(ELispString name, Object ignore) {
            if (!isNil(FGetBuffer.getBuffer(name))) {
                return name;
            }
            if (BuiltInFns.FStringEqual.stringEqual(name, ignore)) {
                return name;
            }
            MuleString base = name.value();
            if (base.startsWith(" ")) {
                int i = new Random().nextInt(1_000_000);
                base = new MuleStringBuffer().append(base)
                        .append('-').append(MuleString.fromString(Integer.toString(i)))
                        .build();
                if (getBuffer(base) != null) {
                    return new ELispString(base);
                }
            }
            for (int i = 2; i < Integer.MAX_VALUE; i++) {
                MuleString gen = new MuleStringBuffer()
                        .append(base)
                        .append('<')
                        .append(MuleString.fromString(Integer.toString(i)))
                        .append('>')
                        .build();
                ELispString wrap = new ELispString(gen);
                if (BuiltInFns.FStringEqual.stringEqual(wrap, ignore) || getBuffer(gen) == null) {
                    return wrap;
                }
            }
            throw ELispSignals.error("Unable to find a new buffer name");
        }
    }

    /**
     * <pre>
     * Return the name of BUFFER, as a string.
     * BUFFER defaults to the current buffer.
     * Return nil if BUFFER has been killed.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferName extends ELispBuiltInBaseNode {
        @Specialization
        public Object bufferName(Object buffer) {
            ELispBuffer b;
            if (buffer instanceof ELispBuffer supplied) {
                b = supplied;
            } else {
                b = getContext().currentBuffer();
            }
            return b.getName();
        }
    }

    /**
     * <pre>
     * Return last name of BUFFER, as a string.
     * BUFFER defaults to the current buffer.
     *
     * This is the name BUFFER had before the last time it was renamed or
     * immediately before it was killed.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-last-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLastName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferLastName(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return name of file BUFFER is visiting, or nil if none.
     * No argument or nil as argument means use the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-file-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferFileName(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the base buffer of indirect buffer BUFFER.
     * If BUFFER is not indirect, return nil.
     * BUFFER defaults to the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-base-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferBaseBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferBaseBuffer(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the value of VARIABLE in BUFFER.
     * If VARIABLE does not have a buffer-local binding in BUFFER, the value
     * is the default binding of the variable.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-local-value", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBufferLocalValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferLocalValue(Object variable, Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an alist of variables that are buffer-local in BUFFER.
     * Most elements look like (SYMBOL . VALUE), describing one variable.
     * For a symbol that is locally unbound, just the symbol appears in the value.
     * Note that storing new VALUEs in these elements doesn't change the variables.
     * No argument or nil as argument means use current buffer as BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-local-variables", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLocalVariables extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferLocalVariables(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if BUFFER was modified since its file was last read or saved.
     * No argument or nil as argument means use current buffer as BUFFER.
     *
     * If BUFFER was autosaved since it was last modified, this function
     * returns the symbol `autosaved'.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-modified-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferModifiedP(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Force redisplay of the current buffer's mode line and header line.
     * With optional non-nil ALL, force redisplay of all mode lines, tab lines and
     * header lines.  This function also forces recomputation of the
     * menu bar menus and the frame title.
     * </pre>
     */
    @ELispBuiltIn(name = "force-mode-line-update", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForceModeLineUpdate extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean forceModeLineUpdate(Object all) {
            // TODO: Until we get a rendering engine
            return false;
        }
    }

    /**
     * <pre>
     * Mark current buffer as modified or unmodified according to FLAG.
     * A non-nil FLAG means mark the buffer modified.
     * In addition, this function unconditionally forces redisplay of the
     * mode lines of the windows that display the current buffer, and also
     * locks or unlocks the file visited by the buffer, depending on whether
     * the function's argument is non-nil, but only if both `buffer-file-name'
     * and `buffer-file-truename' are non-nil.
     * </pre>
     */
    @ELispBuiltIn(name = "set-buffer-modified-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean setBufferModifiedP(Object flag) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Like `set-buffer-modified-p', but doesn't redisplay buffer's mode line.
     * A nil FLAG means to mark the buffer as unmodified.  A non-nil FLAG
     * means mark the buffer as modified.  A special value of `autosaved'
     * will mark the buffer as modified and also as autosaved since it was
     * last modified.
     *
     * This function also locks or unlocks the file visited by the buffer,
     * if both `buffer-file-truename' and `buffer-file-name' are non-nil.
     *
     * It is not ensured that mode lines will be updated to show the modified
     * state of the current buffer.  Use with care.
     * </pre>
     */
    @ELispBuiltIn(name = "restore-buffer-modified-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRestoreBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void restoreBufferModifiedP(Object flag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return BUFFER's tick counter, incremented for each change in text.
     * Each buffer has a tick counter which is incremented each time the
     * text in that buffer is changed.  No argument or nil as argument means
     * use current buffer as BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-modified-tick", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferModifiedTick(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set BUFFER's tick counter to TICK.
     * No argument or nil as argument means use current buffer as BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--set-buffer-modified-tick", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalSetBufferModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalSetBufferModifiedTick(Object tick, Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return BUFFER's character-change tick counter.
     * Each buffer has a character-change tick counter, which is set to the
     * value of the buffer's tick counter (see `buffer-modified-tick'), each
     * time text in that buffer is inserted or deleted.  By comparing the
     * values returned by two individual calls of `buffer-chars-modified-tick',
     * you can tell whether a character change occurred in that buffer in
     * between these calls.  No argument or nil as argument means use current
     * buffer as BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-chars-modified-tick", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferCharsModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferCharsModifiedTick(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Change current buffer's name to NEWNAME (a string).
     * If second arg UNIQUE is nil or omitted, it is an error if a
     * buffer named NEWNAME already exists.
     * If UNIQUE is non-nil, come up with a new name using
     * `generate-new-buffer-name'.
     * Interactively, you can set UNIQUE with a prefix argument.
     * We return the name we actually gave the buffer.
     * This does not change the name of the visited file (if any).
     * </pre>
     */
    @ELispBuiltIn(name = "rename-buffer", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRenameBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void renameBuffer(Object newname, Object unique) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return most recently selected buffer other than BUFFER.
     * Buffers not visible in windows are preferred to visible buffers, unless
     * optional second argument VISIBLE-OK is non-nil.  Ignore the argument
     * BUFFER unless it denotes a live buffer.  If the optional third argument
     * FRAME specifies a live frame, then use that frame's buffer list instead
     * of the selected frame's buffer list.
     *
     * The buffer is found by scanning the selected or specified frame's buffer
     * list first, followed by the list of all buffers.  If no other buffer
     * exists, return the buffer `*scratch*' (creating it if necessary).
     * </pre>
     */
    @ELispBuiltIn(name = "other-buffer", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FOtherBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void otherBuffer(Object buffer, Object visibleOk, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Start keeping undo information for buffer BUFFER.
     * No argument or nil as argument means do this for the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-enable-undo", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferEnableUndo extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean bufferEnableUndo(Object buffer) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Kill the buffer specified by BUFFER-OR-NAME.
     * The argument may be a buffer or the name of an existing buffer.
     * Argument nil or omitted means kill the current buffer.  Return t if the
     * buffer is actually killed, nil otherwise.
     *
     * The functions in `kill-buffer-query-functions' are called with the
     * buffer to be killed as the current buffer.  If any of them returns nil,
     * the buffer is not killed.  The hook `kill-buffer-hook' is run before the
     * buffer is actually killed.  The buffer being killed will be current
     * while the hook is running.  Functions called by any of these hooks are
     * supposed to not change the current buffer.  Neither hook is run for
     * internal or temporary buffers created by `get-buffer-create' or
     * `generate-new-buffer' with argument INHIBIT-BUFFER-HOOKS non-nil.
     *
     * Any processes that have this buffer as the `process-buffer' are killed
     * with SIGHUP.  This function calls `replace-buffer-in-windows' for
     * cleaning up all windows currently displaying the buffer to be killed.
     * </pre>
     */
    @ELispBuiltIn(name = "kill-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKillBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean killBuffer(Object bufferOrName) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Move BUFFER to the end of the buffer list.
     * </pre>
     */
    @ELispBuiltIn(name = "bury-buffer-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBuryBufferInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void buryBufferInternal(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set an appropriate major mode for BUFFER.
     * For the *scratch* buffer, use `initial-major-mode', otherwise choose a mode
     * according to the default value of `major-mode'.
     * Use this function before selecting the buffer, since it may need to inspect
     * the current buffer's major mode.
     * </pre>
     */
    @ELispBuiltIn(name = "set-buffer-major-mode", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBufferMajorMode extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setBufferMajorMode(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current buffer as a Lisp object.
     * </pre>
     */
    @ELispBuiltIn(name = "current-buffer", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public ELispBuffer getCurrentBuffer() {
            return getContext().currentBuffer();
        }
    }

    /**
     * <pre>
     * Make buffer BUFFER-OR-NAME current for editing operations.
     * BUFFER-OR-NAME may be a buffer or the name of an existing buffer.
     * See also `with-current-buffer' when you want to make a buffer current
     * temporarily.  This function does not display the buffer, so its effect
     * ends when the current command terminates.  Use `switch-to-buffer' or
     * `pop-to-buffer' to switch buffers permanently.
     * The return value is the buffer made current.
     * </pre>
     */
    @ELispBuiltIn(name = "set-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBuffer(Object bufferOrName) {
            // TODO: Real buffers
            ValueStorage.Forwarded storage = ELispLanguage.get(null).currentBuffer();
            if (bufferOrName instanceof ELispBuffer buffer) {
                storage.setValue(buffer);
            }
            return storage.getValue();
        }
    }

    /**
     * <pre>
     * Signal a `buffer-read-only' error if the current buffer is read-only.
     * If the text under POSITION (which defaults to point) has the
     * `inhibit-read-only' text property set, the error will not be raised.
     * </pre>
     */
    @ELispBuiltIn(name = "barf-if-buffer-read-only", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBarfIfBufferReadOnly extends ELispBuiltInBaseNode {
        @Specialization
        public static Void barfIfBufferReadOnly(Object position) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete the entire contents of the current buffer.
     * Any narrowing restriction in effect (see `narrow-to-region') is removed,
     * so the buffer is truly empty after this.
     * </pre>
     */
    @ELispBuiltIn(name = "erase-buffer", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEraseBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eraseBuffer() {
            // TODO
            currentBuffer().erase();
            return false;
        }
    }

    /**
     * <pre>
     * Swap the text between current buffer and BUFFER.
     * Using this function from `save-excursion' might produce surprising
     * results, see Info node `(elisp)Swapping Text'.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-swap-text", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferSwapText extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferSwapText(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the multibyte flag of the current buffer to FLAG.
     * If FLAG is t, this makes the buffer a multibyte buffer.
     * If FLAG is nil, this makes the buffer a single-byte buffer.
     * In these cases, the buffer contents remain unchanged as a sequence of
     * bytes but the contents viewed as characters do change.
     * If FLAG is `to', this makes the buffer a multibyte buffer by changing
     * all eight-bit bytes to eight-bit characters.
     * If the multibyte flag was really changed, undo information of the
     * current buffer is cleared.
     * </pre>
     */
    @ELispBuiltIn(name = "set-buffer-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBufferMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean setBufferMultibyte(Object flag) {
            currentBuffer().setMultibyte(flag);
            return false;
        }
    }

    /**
     * <pre>
     * Switch to Fundamental mode by killing current buffer's local variables.
     * Most local variable bindings are eliminated so that the default values
     * become effective once more.  Also, the syntax table is set from
     * `standard-syntax-table', the local keymap is set to nil,
     * and the abbrev table from `fundamental-mode-abbrev-table'.
     * This function also forces redisplay of the mode line.
     *
     * Every function to select a new major mode starts by
     * calling this function.
     *
     * As a special exception, local variables whose names have a non-nil
     * `permanent-local' property are not eliminated by this function.  If
     * the optional KILL-PERMANENT argument is non-nil, clear out these local
     * variables, too.
     *
     * The first thing this function does is run
     * the normal hook `change-major-mode-hook'.
     * </pre>
     */
    @ELispBuiltIn(name = "kill-all-local-variables", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKillAllLocalVariables extends ELispBuiltInBaseNode {
        @Specialization
        public static Void killAllLocalVariables(Object killPermanent) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is an overlay.
     * </pre>
     */
    @ELispBuiltIn(name = "overlayp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create a new overlay with range BEG to END in BUFFER and return it.
     * If omitted, BUFFER defaults to the current buffer.
     * BEG and END may be integers or markers.
     * The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
     * for the front of the overlay advance when text is inserted there
     * \(which means the text *is not* included in the overlay).
     * The fifth arg REAR-ADVANCE, if non-nil, makes the marker
     * for the rear of the overlay advance when text is inserted there
     * \(which means the text *is* included in the overlay).
     * </pre>
     */
    @ELispBuiltIn(name = "make-overlay", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMakeOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeOverlay(Object beg, Object end, Object buffer, Object frontAdvance, Object rearAdvance) {
            // TODO
            return new Object();
        }
    }

    /**
     * <pre>
     * Set the endpoints of OVERLAY to BEG and END in BUFFER.
     * If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
     * If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
     * buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "move-overlay", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FMoveOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Void moveOverlay(Object overlay, Object beg, Object end, Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete the overlay OVERLAY from its buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-overlay", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean deleteOverlay(Object overlay) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Delete all overlays of BUFFER.
     * BUFFER omitted or nil means delete all overlays of the current
     * buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-all-overlays", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteAllOverlays extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteAllOverlays(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position at which OVERLAY starts.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-start", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayStart(Object overlay) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position at which OVERLAY ends.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-end", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayEnd(Object overlay) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer OVERLAY belongs to.
     * Return nil if OVERLAY has been deleted.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayBuffer(Object overlay) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of the properties on OVERLAY.
     * This is a copy of OVERLAY's plist; modifying its conses has no effect on
     * OVERLAY.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-properties", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayProperties(Object overlay) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of the overlays that contain the character at POS.
     * If SORTED is non-nil, then sort them by decreasing priority.
     *
     * Zero-length overlays that start and stop at POS are not included in
     * the return value.  Instead use `overlays-in' if those overlays are of
     * interest.
     * </pre>
     */
    @ELispBuiltIn(name = "overlays-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOverlaysAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlaysAt(Object pos, Object sorted) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of the overlays that overlap the region BEG ... END.
     * Overlap means that at least one character is contained within the overlay
     * and also contained within the specified region.
     *
     * Empty overlays are included in the result if they are located at BEG,
     * between BEG and END, or at END provided END denotes the position at the
     * end of the accessible part of the buffer.
     *
     * The resulting list of overlays is in an arbitrary unpredictable order.
     * </pre>
     */
    @ELispBuiltIn(name = "overlays-in", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOverlaysIn extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlaysIn(Object beg, Object end) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the next position after POS where an overlay starts or ends.
     * If there are no overlay boundaries from POS to (point-max),
     * the value is (point-max).
     * </pre>
     */
    @ELispBuiltIn(name = "next-overlay-change", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNextOverlayChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nextOverlayChange(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the previous position before POS where an overlay starts or ends.
     * If there are no overlay boundaries from (point-min) to POS,
     * the value is (point-min).
     * </pre>
     */
    @ELispBuiltIn(name = "previous-overlay-change", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPreviousOverlayChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousOverlayChange(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list giving all the overlays of the current buffer.
     *
     * For backward compatibility, the value is actually a list that
     * holds another list; the overlays are in the inner list.
     * The list you get is a copy, so that changing it has no effect.
     * However, the overlays you get are the real objects that the buffer uses.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-lists", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FOverlayLists extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayLists() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Recenter the overlays of the current buffer around position POS.
     * That makes overlay lookup faster for positions near POS (but perhaps slower
     * for positions far away from POS).
     *
     * Since Emacs 29.1, this function is a no-op, because the implementation
     * of overlays changed and their lookup is now fast regardless of their
     * position in the buffer.  In particular, this function no longer affects
     * the value returned by `overlay-lists'.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-recenter", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayRecenter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayRecenter(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Get the property of overlay OVERLAY with property name PROP.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-get", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOverlayGet extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean overlayGet(Object overlay, Object prop) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Set one property of overlay OVERLAY: give property PROP value VALUE.
     * VALUE will be returned.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FOverlayPut extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean overlayPut(Object overlay, Object prop, Object value) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Get the overlay tree for BUFFER.
     * </pre>
     */
    @ELispBuiltIn(name = "overlay-tree", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayTree extends ELispBuiltInBaseNode {
        @Specialization
        public static Void overlayTree(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }
}
