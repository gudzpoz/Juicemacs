package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInWindow extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInWindowFactory.getFactories();
    }

    /**
     * <pre>
     * Return t if OBJECT is a window and nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "windowp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowp(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a valid window and nil otherwise.
     * A valid window is either a window that displays a buffer or an internal
     * window.  Windows that have been deleted are not valid.
     * </pre>
     */
    @ELispBuiltIn(name = "window-valid-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowValidP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowValidP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a live window and nil otherwise.
     * A live window is a window that displays a buffer.
     * Internal windows and deleted windows are not live.
     * </pre>
     */
    @ELispBuiltIn(name = "window-live-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowLiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowLiveP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the frame that window WINDOW is on.
     * WINDOW must be a valid window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-frame", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowFrame(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the root window of FRAME-OR-WINDOW.
     * If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
     * With a frame argument, return that frame's root window.
     * With a window argument, return the root window of that window's frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-root-window", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameRootWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameRootWindow(Object frameOrWindow) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the minibuffer window for frame FRAME.
     * If FRAME is omitted or nil, it defaults to the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-window", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMinibufferWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferWindow(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if WINDOW is a minibuffer window.
     * WINDOW must be a valid window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-minibuffer-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowMinibufferP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowMinibufferP(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the topmost, leftmost live window on FRAME-OR-WINDOW.
     * If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
     * Else if FRAME-OR-WINDOW denotes a valid window, return the first window
     * of that window's frame.  If FRAME-OR-WINDOW denotes a live frame, return
     * the first window of that frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-first-window", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameFirstWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameFirstWindow(Object frameOrWindow) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the selected window of FRAME-OR-WINDOW.
     * If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
     * Else if FRAME-OR-WINDOW denotes a valid window, return the selected
     * window of that window's frame.  If FRAME-OR-WINDOW denotes a live frame,
     * return the selected window of that frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-selected-window", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameSelectedWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameSelectedWindow(Object frameOrWindow) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return old selected window of FRAME.
     * FRAME must be a live frame and defaults to the selected one.
     *
     * The return value is the window selected on FRAME the last time window
     * change functions were run for FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-old-selected-window", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameOldSelectedWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameOldSelectedWindow(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set selected window of FRAME to WINDOW.
     * FRAME must be a live frame and defaults to the selected one.  If FRAME
     * is the selected frame, this makes WINDOW the selected window.  Optional
     * argument NORECORD non-nil means to neither change the order of recently
     * selected windows nor the buffer list.  WINDOW must denote a live window.
     * Return WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "set-frame-selected-window", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetFrameSelectedWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFrameSelectedWindow(Object frame, Object window, Object norecord) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the selected window.
     * The selected window is the window in which the standard cursor for
     * selected windows appears and to which many commands apply.
     *
     * Also see `old-selected-window' and `minibuffer-selected-window'.
     * </pre>
     */
    @ELispBuiltIn(name = "selected-window", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSelectedWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void selectedWindow() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the old selected window.
     * The return value is the window selected the last time window change
     * functions were run.
     * </pre>
     */
    @ELispBuiltIn(name = "old-selected-window", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FOldSelectedWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void oldSelectedWindow() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Select WINDOW which must be a live window.
     * Also make WINDOW's frame the selected frame and WINDOW that frame's
     * selected window.  In addition, make WINDOW's buffer current and set its
     * buffer's value of `point' to the value of WINDOW's `window-point'.
     * Return WINDOW.
     *
     * Optional second arg NORECORD non-nil means do not put this buffer at the
     * front of the buffer list and do not make this window the most recently
     * selected one.  Also, do not mark WINDOW for redisplay unless NORECORD
     * equals the special symbol `mark-for-redisplay'.
     *
     * Run `buffer-list-update-hook' unless NORECORD is non-nil.  Note that
     * applications and internal routines often select a window temporarily for
     * various purposes; mostly, to simplify coding.  As a rule, such
     * selections should not be recorded and therefore will not pollute
     * `buffer-list-update-hook'.  Selections that "really count" are those
     * causing a visible change in the next redisplay of WINDOW's frame and
     * should always be recorded.  So if you think of running a function each
     * time a window gets selected, put it on `buffer-list-update-hook' or
     * `window-selection-change-functions'.
     *
     * Also note that the main editor command loop sets the current buffer to
     * the buffer of the selected window before each command.
     * </pre>
     */
    @ELispBuiltIn(name = "select-window", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSelectWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void selectWindow(Object window, Object norecord) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer displayed in window WINDOW.
     * If WINDOW is omitted or nil, it defaults to the selected window.
     * Return nil for an internal window or a deleted window.
     * </pre>
     */
    @ELispBuiltIn(name = "window-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowBuffer(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the old buffer displayed by WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * The return value is the buffer shown in WINDOW at the last time window
     * change functions were run.  It is nil if WINDOW was created after
     * that.  It is t if WINDOW has been restored from a window configuration
     * after that.
     * </pre>
     */
    @ELispBuiltIn(name = "window-old-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowOldBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowOldBuffer(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parent window of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return nil for a window with no parent (e.g. a root window).
     * </pre>
     */
    @ELispBuiltIn(name = "window-parent", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowParent(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the topmost child window of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return nil if WINDOW is a live window (live windows have no children).
     * Return nil if WINDOW is an internal window whose children form a
     * horizontal combination.
     * </pre>
     */
    @ELispBuiltIn(name = "window-top-child", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowTopChild extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTopChild(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the leftmost child window of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return nil if WINDOW is a live window (live windows have no children).
     * Return nil if WINDOW is an internal window whose children form a
     * vertical combination.
     * </pre>
     */
    @ELispBuiltIn(name = "window-left-child", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowLeftChild extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowLeftChild(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the next sibling window of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return nil if WINDOW has no next sibling.
     * </pre>
     */
    @ELispBuiltIn(name = "window-next-sibling", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowNextSibling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowNextSibling(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the previous sibling window of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return nil if WINDOW has no previous sibling.
     * </pre>
     */
    @ELispBuiltIn(name = "window-prev-sibling", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPrevSibling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPrevSibling(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return combination limit of window WINDOW.
     * WINDOW must be a valid window used in horizontal or vertical combination.
     * If the return value is nil, child windows of WINDOW can be recombined with
     * WINDOW's siblings.  A return value of t means that child windows of
     * WINDOW are never (re-)combined with WINDOW's siblings.
     * </pre>
     */
    @ELispBuiltIn(name = "window-combination-limit", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowCombinationLimit extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowCombinationLimit(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set combination limit of window WINDOW to LIMIT; return LIMIT.
     * WINDOW must be a valid window used in horizontal or vertical combination.
     * If LIMIT is nil, child windows of WINDOW can be recombined with WINDOW's
     * siblings.  LIMIT t means that child windows of WINDOW are never
     * \(re-)combined with WINDOW's siblings.  Other values are reserved for
     * future use.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-combination-limit", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowCombinationLimit extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowCombinationLimit(Object window, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the use time of window WINDOW.
     * WINDOW must specify a live window and defaults to the selected one.
     *
     * The window with the highest use time is usually the one most recently
     * selected by calling `select-window' with NORECORD nil.  The window with
     * the lowest use time is usually the least recently selected one chosen in
     * such a way.
     *
     * Note that the use time of a window can be also changed by calling
     * `window-bump-use-time' for that window.
     * </pre>
     */
    @ELispBuiltIn(name = "window-use-time", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowUseTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowUseTime(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Mark WINDOW as second most recently used.
     * WINDOW must specify a live window.
     *
     * If WINDOW is not selected and the selected window has the highest use
     * time of all windows, set the use time of WINDOW to that of the selected
     * window, increase the use time of the selected window by one and return
     * the new use time of WINDOW.  Otherwise, do nothing and return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "window-bump-use-time", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowBumpUseTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowBumpUseTime(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width of window WINDOW in pixels.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The return value includes the fringes and margins of WINDOW as well as
     * any vertical dividers or scroll bars belonging to WINDOW.  If WINDOW is
     * an internal window, its pixel width is the width of the screen areas
     * spanned by its children.
     * </pre>
     */
    @ELispBuiltIn(name = "window-pixel-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPixelWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPixelWidth(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height of window WINDOW in pixels.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The return value includes the mode line and header line and the bottom
     * divider, if any.  If WINDOW is an internal window, its pixel height is
     * the height of the screen areas spanned by its children.
     * </pre>
     */
    @ELispBuiltIn(name = "window-pixel-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPixelHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPixelHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return old total pixel width of WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The return value is the total pixel width of WINDOW after the last
     * time window change functions found WINDOW live on its frame.  It is
     * zero if WINDOW was created after that.
     * </pre>
     */
    @ELispBuiltIn(name = "window-old-pixel-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowOldPixelWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowOldPixelWidth(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return old total pixel height of WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The return value is the total pixel height of WINDOW after the last
     * time window change functions found WINDOW live on its frame.  It is
     * zero if WINDOW was created after that.
     * </pre>
     */
    @ELispBuiltIn(name = "window-old-pixel-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowOldPixelHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowOldPixelHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height of window WINDOW in lines.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The return value includes the heights of WINDOW's mode and header line
     * and its bottom divider, if any.  If WINDOW is an internal window, the
     * total height is the height of the screen areas spanned by its children.
     *
     * If WINDOW's pixel height is not an integral multiple of its frame's
     * character height, the number of lines occupied by WINDOW is rounded
     * internally.  This is done in a way such that, if WINDOW is a parent
     * window, the sum of the total heights of all its children internally
     * equals the total height of WINDOW.
     *
     * If the optional argument ROUND is `ceiling', return the smallest integer
     * larger than WINDOW's pixel height divided by the character height of
     * WINDOW's frame.  ROUND `floor' means to return the largest integer
     * smaller than WINDOW's pixel height divided by the character height of
     * WINDOW's frame.  Any other value of ROUND means to return the internal
     * total height of WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "window-total-height", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowTotalHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTotalHeight(Object window, Object round) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the total width of window WINDOW in columns.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The return value includes the widths of WINDOW's fringes, margins,
     * scroll bars and its right divider, if any.  If WINDOW is an internal
     * window, the total width is the width of the screen areas spanned by its
     * children.
     *
     * If WINDOW's pixel width is not an integral multiple of its frame's
     * character width, the number of lines occupied by WINDOW is rounded
     * internally.  This is done in a way such that, if WINDOW is a parent
     * window, the sum of the total widths of all its children internally
     * equals the total width of WINDOW.
     *
     * If the optional argument ROUND is `ceiling', return the smallest integer
     * larger than WINDOW's pixel width divided by the character width of
     * WINDOW's frame.  ROUND `floor' means to return the largest integer
     * smaller than WINDOW's pixel width divided by the character width of
     * WINDOW's frame.  Any other value of ROUND means to return the internal
     * total width of WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "window-total-width", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowTotalWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTotalWidth(Object window, Object round) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the new total size of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The new total size of WINDOW is the value set by the last call of
     * `set-window-new-total' for WINDOW.  If it is valid, it will be shortly
     * installed as WINDOW's total height (see `window-total-height') or total
     * width (see `window-total-width').
     * </pre>
     */
    @ELispBuiltIn(name = "window-new-total", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowNewTotal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowNewTotal(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the normal height of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * If HORIZONTAL is non-nil, return the normal width of WINDOW.
     *
     * The normal height of a frame's root window or a window that is
     * horizontally combined (a window that has a left or right sibling) is
     * 1.0.  The normal height of a window that is vertically combined (has a
     * sibling above or below) is the fraction of the window's height with
     * respect to its parent.  The sum of the normal heights of all windows in a
     * vertical combination equals 1.0.
     *
     * Similarly, the normal width of a frame's root window or a window that is
     * vertically combined equals 1.0.  The normal width of a window that is
     * horizontally combined is the fraction of the window's width with respect
     * to its parent.  The sum of the normal widths of all windows in a
     * horizontal combination equals 1.0.
     *
     * The normal sizes of windows are used to restore the proportional sizes
     * of windows after they have been shrunk to their minimum sizes; for
     * example when a frame is temporarily made very small and afterwards gets
     * re-enlarged to its previous size.
     * </pre>
     */
    @ELispBuiltIn(name = "window-normal-size", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowNormalSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowNormalSize(Object window, Object horizontal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return new normal size of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The new normal size of WINDOW is the value set by the last call of
     * `set-window-new-normal' for WINDOW.  If valid, it will be shortly
     * installed as WINDOW's normal size (see `window-normal-size').
     * </pre>
     */
    @ELispBuiltIn(name = "window-new-normal", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowNewNormal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowNewNormal(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return new pixel size of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     *
     * The new pixel size of WINDOW is the value set by the last call of
     * `set-window-new-pixel' for WINDOW.  If it is valid, it will be shortly
     * installed as WINDOW's pixel height (see `window-pixel-height') or pixel
     * width (see `window-pixel-width').
     * </pre>
     */
    @ELispBuiltIn(name = "window-new-pixel", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowNewPixel extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowNewPixel(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return left pixel edge of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-pixel-left", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPixelLeft extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPixelLeft(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return top pixel edge of window WINDOW.
     * WINDOW must be a valid window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-pixel-top", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPixelTop extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPixelTop(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return left column of window WINDOW.
     * This is the distance, in columns, between the left edge of WINDOW and
     * the left edge of the frame's window area.  For instance, the return
     * value is 0 if there is no window to the left of WINDOW.
     *
     * WINDOW must be a valid window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-left-column", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowLeftColumn extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowLeftColumn(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return top line of window WINDOW.
     * This is the distance, in lines, between the top of WINDOW and the top
     * of the frame's window area.  For instance, the return value is 0 if
     * there is no window above WINDOW.
     *
     * WINDOW must be a valid window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-top-line", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowTopLine extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTopLine(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width of WINDOW's text area.
     * WINDOW must be a live window and defaults to the selected one.  The
     * return value does not include any vertical dividers, fringes or
     * marginal areas, or scroll bars.
     *
     * The optional argument PIXELWISE defines the units to use for the
     * width.  If nil, return the largest integer smaller than WINDOW's pixel
     * width in units of the character width of WINDOW's frame.  If PIXELWISE
     * is `remap' and the default face is remapped (see
     * `face-remapping-alist'), use the remapped face to determine the
     * character width.  For any other non-nil value, return the width in
     * pixels.
     *
     * Note that the returned value includes the column reserved for the
     * continuation glyph.
     *
     * Also see `window-max-chars-per-line'.
     * </pre>
     */
    @ELispBuiltIn(name = "window-body-width", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowBodyWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowBodyWidth(Object window, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height of WINDOW's text area.
     * WINDOW must be a live window and defaults to the selected one.  The
     * return value does not include the mode line or header line or any
     * horizontal divider.
     *
     * The optional argument PIXELWISE defines the units to use for the
     * height.  If nil, return the largest integer smaller than WINDOW's
     * pixel height in units of the character height of WINDOW's frame.  If
     * PIXELWISE is `remap' and the default face is remapped (see
     * `face-remapping-alist'), use the remapped face to determine the
     * character height.  For any other non-nil value, return the height in
     * pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "window-body-height", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowBodyHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowBodyHeight(Object window, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return old width of WINDOW's text area in pixels.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * The return value is the pixel width of WINDOW's text area after the
     * last time window change functions found WINDOW live on its frame.  It
     * is zero if WINDOW was created after that.
     * </pre>
     */
    @ELispBuiltIn(name = "window-old-body-pixel-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowOldBodyPixelWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowOldBodyPixelWidth(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return old height of WINDOW's text area in pixels.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * The return value is the pixel height of WINDOW's text area after the
     * last time window change functions found WINDOW live on its frame.  It
     * is zero if WINDOW was created after that.
     * </pre>
     */
    @ELispBuiltIn(name = "window-old-body-pixel-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowOldBodyPixelHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowOldBodyPixelHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height in pixels of WINDOW's mode-line.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-mode-line-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowModeLineHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowModeLineHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height in pixels of WINDOW's header-line.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-header-line-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowHeaderLineHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowHeaderLineHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height in pixels of WINDOW's tab-line.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-tab-line-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowTabLineHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTabLineHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width in pixels of WINDOW's right divider.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-right-divider-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowRightDividerWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowRightDividerWidth(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width in pixels of WINDOW's bottom divider.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-bottom-divider-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowBottomDividerWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowBottomDividerWidth(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width in pixels of WINDOW's vertical scrollbar.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-scroll-bar-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowScrollBarWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowScrollBarWidth(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height in pixels of WINDOW's horizontal scrollbar.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-scroll-bar-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowScrollBarHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowScrollBarHeight(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of columns by which WINDOW is scrolled from left margin.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-hscroll", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowHscroll extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowHscroll(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set number of columns WINDOW is scrolled from left margin to NCOL.
     * WINDOW must be a live window and defaults to the selected one.
     * Clip the number to a reasonable value if out of range.
     * Return the new number.  NCOL should be zero or positive.
     *
     * Note that if `auto-hscroll-mode' is non-nil, you cannot scroll the
     * window so that the location of point moves off-window.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-hscroll", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowHscroll extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowHscroll(Object window, Object ncol) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if COORDINATES are in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     * COORDINATES is a cons of the form (X . Y), X and Y being distances
     * measured in characters from the upper-left corner of the frame.
     * \(0 . 0) denotes the character in the upper left corner of the
     * frame.
     * If COORDINATES are in the text portion of WINDOW,
     *    the coordinates relative to the window are returned.
     * If they are in the bottom divider of WINDOW, `bottom-divider' is returned.
     * If they are in the right divider of WINDOW, `right-divider' is returned.
     * If they are in the mode line of WINDOW, `mode-line' is returned.
     * If they are in the header line of WINDOW, `header-line' is returned.
     * If they are in the tab line of WINDOW, `tab-line' is returned.
     * If they are in the left fringe of WINDOW, `left-fringe' is returned.
     * If they are in the right fringe of WINDOW, `right-fringe' is returned.
     * If they are on the border between WINDOW and its right sibling,
     *   `vertical-line' is returned.
     * If they are in the windows's left or right marginal areas, `left-margin'\n\
     *   or `right-margin' is returned.
     * </pre>
     */
    @ELispBuiltIn(name = "coordinates-in-window-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCoordinatesInWindowP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void coordinatesInWindowP(Object coordinates, Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return window containing coordinates X and Y on FRAME.
     * FRAME must be a live frame and defaults to the selected one.
     * X and Y are measured in units of canonical columns and rows.
     * The top left corner of the frame is considered to be column 0, row 0.
     * Tool-bar and tab-bar pseudo-windows are ignored by this function: if
     * the specified coordinates are in any of these two windows, this
     * function returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "window-at", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FWindowAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowAt(Object x, Object y, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return current value of point in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * For a nonselected window, this is the value point would have if that
     * window were selected.
     *
     * Note that, when WINDOW is selected, the value returned is the same as
     * that returned by `point' for WINDOW's buffer.  It would be more strictly
     * correct to return the top-level value of `point', outside of any
     * `save-excursion' forms.  But that is hard to define.
     * </pre>
     */
    @ELispBuiltIn(name = "window-point", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPoint extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPoint(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return old value of point in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-old-point", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowOldPoint extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowOldPoint(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return position at which display currently starts in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     * This is updated by redisplay or by calling `set-window-start'.
     * </pre>
     */
    @ELispBuiltIn(name = "window-start", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowStart(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return position at which display currently ends in WINDOW.
     * This is the position after the final character in WINDOW.
     *
     * WINDOW must be a live window and defaults to the selected one.  This
     * is updated by redisplay, when it runs to completion.  Simply changing
     * the buffer text or setting `window-start' does not update this value.
     *
     * Return nil if there is no recorded value.  (This can happen if the
     * last redisplay of WINDOW was preempted, and did not finish.)  If
     * UPDATE is non-nil, compute the up-to-date position if it isn't already
     * recorded.
     * </pre>
     */
    @ELispBuiltIn(name = "window-end", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowEnd(Object window, Object update) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make point value in WINDOW be at position POS in WINDOW's buffer.
     * WINDOW must be a live window and defaults to the selected one.
     * Return POS.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-point", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowPoint extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowPoint(Object window, Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make display in WINDOW start at position POS in WINDOW's buffer.
     * WINDOW must be a live window and defaults to the selected one.  Return
     * POS.
     *
     * Optional third arg NOFORCE non-nil prevents next redisplay from
     * moving point if displaying the window at POS makes point invisible;
     * redisplay will then choose the WINDOW's start position by itself in
     * that case, i.e. it will disregard POS if adhering to it will make
     * point not visible in the window.
     *
     * For reliable setting of WINDOW start position, make sure point is
     * at a position that will be visible when that start is in effect,
     * otherwise there's a chance POS will be disregarded, e.g., if point
     * winds up in a partially-visible line.
     *
     * The setting of the WINDOW's start position takes effect during the
     * next redisplay cycle, not immediately.  If NOFORCE is nil or
     * omitted, forcing the display of WINDOW to start at POS cancels
     * any setting of WINDOW's vertical scroll (\"vscroll\") amount
     * set by `set-window-vscroll' and by scrolling functions.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-start", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowStart(Object window, Object pos, Object noforce) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if position POS is currently on the frame in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * Return nil if that position is scrolled vertically out of view.  If a
     * character is only partially visible, nil is returned, unless the
     * optional argument PARTIALLY is non-nil.  If POS is only out of view
     * because of horizontal scrolling, return non-nil.  If POS is t, it
     * specifies either the first position displayed on the last visible
     * screen line in WINDOW, or the end-of-buffer position, whichever comes
     * first.  POS defaults to point in WINDOW; WINDOW defaults to the
     * selected window.
     *
     * If POS is visible, return t if PARTIALLY is nil; if PARTIALLY is non-nil,
     * the return value is a list of 2 or 6 elements (X Y [RTOP RBOT ROWH VPOS]),
     * where X and Y are the pixel coordinates relative to the top left corner
     * of the window.  The remaining elements are omitted if the character after
     * POS is fully visible; otherwise, RTOP and RBOT are the number of pixels
     * off-window at the top and bottom of the screen line ("row") containing
     * POS, ROWH is the visible height of that row, and VPOS is the row number
     * \(zero-based).
     * </pre>
     */
    @ELispBuiltIn(name = "pos-visible-in-window-p", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPosVisibleInWindowP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posVisibleInWindowP(Object pos, Object window, Object partially) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return height in pixels of text line LINE in window WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * Return height of current line if LINE is omitted or nil.  Return height of
     * header or mode line if LINE is `header-line' or `mode-line'.
     * Otherwise, LINE is a text line number starting from 0.  A negative number
     * counts from the end of the window.
     *
     * Value is a list (HEIGHT VPOS YPOS OFFBOT), where HEIGHT is the height
     * in pixels of the visible part of the line, VPOS and YPOS are the
     * vertical position in lines and pixels of the line, relative to the top
     * of the first text line, and OFFBOT is the number of off-window pixels at
     * the bottom of the text line.  If there are off-window pixels at the top
     * of the (first) text line, YPOS is negative.
     *
     * Return nil if window display is not up-to-date.  In that case, use
     * `pos-visible-in-window-p' to obtain the information.
     * </pre>
     */
    @ELispBuiltIn(name = "window-line-height", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowLineHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowLineHeight(Object line, Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return pixel dimensions of WINDOW's lines.
     * The return value is a list of the x- and y-coordinates of the lower
     * right corner of the last character of each line.  Return nil if the
     * current glyph matrix of WINDOW is not up-to-date.
     *
     * Optional argument WINDOW specifies the window whose lines' dimensions
     * shall be returned.  Nil or omitted means to return the dimensions for
     * the selected window.
     *
     * FIRST, if non-nil, specifies the index of the first line whose
     * dimensions shall be returned.  If FIRST is nil and BODY is non-nil,
     * start with the first text line of WINDOW.  Otherwise, start with the
     * first line of WINDOW.
     *
     * LAST, if non-nil, specifies the last line whose dimensions shall be
     * returned.  If LAST is nil and BODY is non-nil, the last line is the last
     * line of the body (text area) of WINDOW.  Otherwise, last is the last
     * line of WINDOW.
     *
     * INVERSE, if nil, means that the y-pixel value returned for a specific
     * line specifies the distance in pixels from the left edge (body edge if
     * BODY is non-nil) of WINDOW to the right edge of the last glyph of that
     * line.  INVERSE non-nil means that the y-pixel value returned for a
     * specific line specifies the distance in pixels from the right edge of
     * the last glyph of that line to the right edge (body edge if BODY is
     * non-nil) of WINDOW.
     *
     * LEFT non-nil means to return the x- and y-coordinates of the lower left
     * corner of the leftmost character on each line.  This is the value that
     * should be used for buffers that mostly display text from right to left.
     *
     * If LEFT is non-nil and INVERSE is nil, this means that the y-pixel value
     * returned for a specific line specifies the distance in pixels from the
     * left edge of the last (leftmost) glyph of that line to the right edge
     * (body edge if BODY is non-nil) of WINDOW.  If LEFT and INVERSE are both
     * non-nil, the y-pixel value returned for a specific line specifies the
     * distance in pixels from the left edge (body edge if BODY is non-nil) of
     * WINDOW to the left edge of the last (leftmost) glyph of that line.
     *
     * Normally, the value of this function is not available while Emacs is
     * busy, for example, when processing a command.  It should be retrievable
     * though when run from an idle timer with a delay of zero seconds.
     * </pre>
     */
    @ELispBuiltIn(name = "window-lines-pixel-dimensions", minArgs = 0, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FWindowLinesPixelDimensions extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowLinesPixelDimensions(Object window, Object first, Object last, Object body, Object inverse, Object left) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil when WINDOW is dedicated to its buffer.
     * More precisely, return the value assigned by the last call of
     * `set-window-dedicated-p' for WINDOW.  Return nil if that function was
     * never called with WINDOW as its argument, or the value set by that
     * function was internally reset since its last call.  WINDOW must be a
     * live window and defaults to the selected one.
     *
     * When a window is dedicated to its buffer, `display-buffer' will refrain
     * from displaying another buffer in it.  `get-lru-window' and
     * `get-largest-window' treat dedicated windows specially.
     * `delete-windows-on', `replace-buffer-in-windows', `quit-window' and
     * `kill-buffer' can delete a dedicated window and the containing frame.
     *
     * Functions like `set-window-buffer' may change the buffer displayed by a
     * window, unless that window is "strongly" dedicated to its buffer, that
     * is the value returned by `window-dedicated-p' is t.
     * </pre>
     */
    @ELispBuiltIn(name = "window-dedicated-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowDedicatedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowDedicatedP(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Mark WINDOW as dedicated according to FLAG.
     * WINDOW must be a live window and defaults to the selected one.  FLAG
     * non-nil means mark WINDOW as dedicated to its buffer.  FLAG nil means
     * mark WINDOW as non-dedicated.  Return FLAG.
     *
     * When a window is dedicated to its buffer, `display-buffer' will refrain
     * from displaying another buffer in it.  `get-lru-window' and
     * `get-largest-window' treat dedicated windows specially.
     * `delete-windows-on', `replace-buffer-in-windows', `quit-window',
     * `quit-restore-window' and `kill-buffer' can delete a dedicated window
     * and the containing frame.
     *
     * As a special case, if FLAG is t, mark WINDOW as "strongly" dedicated to
     * its buffer.  Functions like `set-window-buffer' may change the buffer
     * displayed by a window, unless that window is strongly dedicated to its
     * buffer.  If and when `set-window-buffer' displays another buffer in a
     * window, it also makes sure that the window is no more dedicated.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-dedicated-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowDedicatedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowDedicatedP(Object window, Object flag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return buffers previously shown in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * The return value is a list of elements (BUFFER WINDOW-START POS),
     * where BUFFER is a buffer, WINDOW-START is the start position of the
     * window for that buffer, and POS is a window-specific point value.
     * </pre>
     */
    @ELispBuiltIn(name = "window-prev-buffers", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowPrevBuffers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowPrevBuffers(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set WINDOW's previous buffers to PREV-BUFFERS.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * PREV-BUFFERS should be a list of elements (BUFFER WINDOW-START POS),
     * where BUFFER is a buffer, WINDOW-START is the start position of the
     * window for that buffer, and POS is a window-specific point value.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-prev-buffers", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowPrevBuffers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowPrevBuffers(Object window, Object prevBuffers) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return list of buffers recently re-shown in WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-next-buffers", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowNextBuffers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowNextBuffers(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set WINDOW's next buffers to NEXT-BUFFERS.
     * WINDOW must be a live window and defaults to the selected one.
     * NEXT-BUFFERS should be a list of buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-next-buffers", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowNextBuffers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowNextBuffers(Object window, Object nextBuffers) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parameters of WINDOW and their values.
     * WINDOW must be a valid window and defaults to the selected one.  The
     * return value is a list of elements of the form (PARAMETER . VALUE).
     * </pre>
     */
    @ELispBuiltIn(name = "window-parameters", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowParameters extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowParameters(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return WINDOW's value for PARAMETER.
     * WINDOW can be any window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-parameter", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowParameter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowParameter(Object window, Object parameter) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set WINDOW's value of PARAMETER to VALUE.
     * WINDOW can be any window and defaults to the selected one.
     * Return VALUE.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-parameter", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowParameter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowParameter(Object window, Object parameter, Object value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the display-table that WINDOW is using.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-display-table", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowDisplayTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowDisplayTable(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set WINDOW's display-table to TABLE.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-display-table", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowDisplayTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowDisplayTable(Object window, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return live window after WINDOW in the cyclic ordering of windows.
     * WINDOW must be a live window and defaults to the selected one.  The
     * optional arguments MINIBUF and ALL-FRAMES specify the set of windows to
     * consider.
     *
     * MINIBUF nil or omitted means consider the minibuffer window only if the
     * minibuffer is active.  MINIBUF t means consider the minibuffer window
     * even if the minibuffer is not active.  Any other value means do not
     * consider the minibuffer window even if the minibuffer is active.
     *
     * ALL-FRAMES nil or omitted means consider all windows on WINDOW's frame,
     * plus the minibuffer window if specified by the MINIBUF argument.  If the
     * minibuffer counts, consider all windows on all frames that share that
     * minibuffer too.  The following non-nil values of ALL-FRAMES have special
     * meanings:
     *
     * - t means consider all windows on all existing frames.
     *
     * - `visible' means consider all windows on all visible frames.
     *
     * - 0 (the number zero) means consider all windows on all visible and
     *   iconified frames.
     *
     * - A frame means consider all windows on that frame only.
     *
     * Anything else means consider all windows on WINDOW's frame and no
     * others.
     *
     * If you use consistent values for MINIBUF and ALL-FRAMES, you can use
     * `next-window' to iterate through the entire cycle of acceptable
     * windows, eventually ending up back at the window you started with.
     * `previous-window' traverses the same cycle, in the reverse order.
     * </pre>
     */
    @ELispBuiltIn(name = "next-window", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FNextWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nextWindow(Object window, Object minibuf, Object allFrames) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return live window before WINDOW in the cyclic ordering of windows.
     * WINDOW must be a live window and defaults to the selected one.  The
     * optional arguments MINIBUF and ALL-FRAMES specify the set of windows to
     * consider.
     *
     * MINIBUF nil or omitted means consider the minibuffer window only if the
     * minibuffer is active.  MINIBUF t means consider the minibuffer window
     * even if the minibuffer is not active.  Any other value means do not
     * consider the minibuffer window even if the minibuffer is active.
     *
     * ALL-FRAMES nil or omitted means consider all windows on WINDOW's frame,
     * plus the minibuffer window if specified by the MINIBUF argument.  If the
     * minibuffer counts, consider all windows on all frames that share that
     * minibuffer too.  The following non-nil values of ALL-FRAMES have special
     * meanings:
     *
     * - t means consider all windows on all existing frames.
     *
     * - `visible' means consider all windows on all visible frames.
     *
     * - 0 (the number zero) means consider all windows on all visible and
     *   iconified frames.
     *
     * - A frame means consider all windows on that frame only.
     *
     * Anything else means consider all windows on WINDOW's frame and no
     * others.
     *
     * If you use consistent values for MINIBUF and ALL-FRAMES, you can
     * use `previous-window' to iterate through the entire cycle of
     * acceptable windows, eventually ending up back at the window you
     * started with.  `next-window' traverses the same cycle, in the
     * reverse order.
     * </pre>
     */
    @ELispBuiltIn(name = "previous-window", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPreviousWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousWindow(Object window, Object minibuf, Object allFrames) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of windows on FRAME, starting with WINDOW.
     * FRAME nil or omitted means use the selected frame.
     * WINDOW nil or omitted means use the window selected within FRAME.
     * MINIBUF t means include the minibuffer window, even if it isn't active.
     * MINIBUF nil or omitted means include the minibuffer window only
     * if it's active.
     * MINIBUF neither nil nor t means never include the minibuffer window.
     * </pre>
     */
    @ELispBuiltIn(name = "window-list", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FWindowList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowList(Object frame, Object minibuf, Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of all live windows.
     * WINDOW specifies the first window to list and defaults to the selected
     * window.
     *
     * Optional argument MINIBUF nil or omitted means consider the minibuffer
     * window only if the minibuffer is active.  MINIBUF t means consider the
     * minibuffer window even if the minibuffer is not active.  Any other value
     * means do not consider the minibuffer window even if the minibuffer is
     * active.
     *
     * Optional argument ALL-FRAMES nil or omitted means consider all windows
     * on WINDOW's frame, plus the minibuffer window if specified by the
     * MINIBUF argument.  If the minibuffer counts, consider all windows on all
     * frames that share that minibuffer too.  The following non-nil values of
     * ALL-FRAMES have special meanings:
     *
     * - t means consider all windows on all existing frames.
     *
     * - `visible' means consider all windows on all visible frames.
     *
     * - 0 (the number zero) means consider all windows on all visible and
     *   iconified frames.
     *
     * - A frame means consider all windows on that frame only.
     *
     * Anything else means consider all windows on WINDOW's frame and no
     * others.
     *
     * If WINDOW is not on the list of windows returned, some other window will
     * be listed first but no error is signaled.
     * </pre>
     */
    @ELispBuiltIn(name = "window-list-1", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FWindowList1 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowList1(Object window, Object minibuf, Object allFrames) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a window currently displaying BUFFER-OR-NAME, or nil if none.
     * BUFFER-OR-NAME may be a buffer or a buffer name and defaults to
     * the current buffer.
     *
     * The optional argument ALL-FRAMES specifies the frames to consider:
     *
     * - t means consider all windows on all existing frames.
     *
     * - `visible' means consider all windows on all visible frames.
     *
     * - 0 (the number zero) means consider all windows on all visible
     *     and iconified frames.
     *
     * - A frame means consider all windows on that frame only.
     *
     * Any other value of ALL-FRAMES means consider all windows on the
     * selected frame and no others.
     * </pre>
     */
    @ELispBuiltIn(name = "get-buffer-window", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetBufferWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getBufferWindow(Object bufferOrName, Object allFrames) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Discard BUFFER from WINDOW.
     * Discard specified live BUFFER from the lists of previous and next
     * buffers of specified live WINDOW.
     *
     * Optional argument ALL non-nil means discard any `quit-restore' and
     * `quit-restore-prev' parameters of WINDOW referencing BUFFER too.
     * </pre>
     */
    @ELispBuiltIn(name = "window-discard-buffer-from-window", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FWindowDiscardBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowDiscardBuffer(Object buffer, Object window, Object all) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make WINDOW fill its frame.
     * Only the frame WINDOW is on is affected.  WINDOW must be a valid window
     * and defaults to the selected one.
     *
     * Optional argument ROOT, if non-nil, must specify an internal window such
     * that WINDOW is in its window subtree.  If this is the case, replace ROOT
     * by WINDOW and leave alone any windows not part of ROOT's subtree.
     *
     * When WINDOW is live try to reduce display jumps by keeping the text
     * previously visible in WINDOW in the same place on the frame.  Doing this
     * depends on the value of (window-start WINDOW), so if calling this
     * function in a program gives strange scrolling, make sure the
     * window-start value is reasonable when this function is called.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-other-windows-internal", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteOtherWindowsInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteOtherWindowsInternal(Object window, Object root) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Run `window-configuration-change-hook' for FRAME.
     * If FRAME is omitted or nil, it defaults to the selected frame.
     *
     * This function should not be needed any more and will be therefore
     * considered obsolete.
     * </pre>
     */
    @ELispBuiltIn(name = "run-window-configuration-change-hook", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRunWindowConfigurationChangeHook extends ELispBuiltInBaseNode {
        @Specialization
        public static Void runWindowConfigurationChangeHook(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Run `window-scroll-functions' for WINDOW.
     * If WINDOW is omitted or nil, it defaults to the selected window.
     *
     * This function is called by `split-window' for the new window, after it
     * has established the size of the new window.
     * </pre>
     */
    @ELispBuiltIn(name = "run-window-scroll-functions", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRunWindowScrollFunctions extends ELispBuiltInBaseNode {
        @Specialization
        public static Void runWindowScrollFunctions(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make WINDOW display BUFFER-OR-NAME.
     * WINDOW must be a live window and defaults to the selected one.
     * BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
     *
     * Optional third argument KEEP-MARGINS non-nil means that WINDOW's current
     * display margins, fringe widths, and scroll bar settings are preserved;
     * the default is to reset these from the local settings for BUFFER-OR-NAME
     * or the frame defaults.  Return nil.
     *
     * This function throws an error when WINDOW is strongly dedicated to its
     * buffer (that is `window-dedicated-p' returns t for WINDOW) and does not
     * already display BUFFER-OR-NAME.
     *
     * This function runs `window-scroll-functions' before running
     * `window-configuration-change-hook'.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-buffer", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowBuffer(Object window, Object bufferOrName, Object keepMargins) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Force all windows to be updated on next redisplay.
     * If optional arg OBJECT is a window, force redisplay of that window only.
     * If OBJECT is a buffer or buffer name, force redisplay of all windows
     * displaying that buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "force-window-update", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForceWindowUpdate extends ELispBuiltInBaseNode {
        @Specialization
        public static Void forceWindowUpdate(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set new pixel size of WINDOW to SIZE.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return SIZE.
     *
     * Optional argument ADD non-nil means add SIZE to the new pixel size of
     * WINDOW and return the sum.
     *
     * The new pixel size of WINDOW, if valid, will be shortly installed as
     * WINDOW's pixel height (see `window-pixel-height') or pixel width (see
     * `window-pixel-width').
     *
     * Note: This function does not operate on any child windows of WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-new-pixel", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowNewPixel extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowNewPixel(Object window, Object size, Object add) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set new total size of WINDOW to SIZE.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return SIZE.
     *
     * Optional argument ADD non-nil means add SIZE to the new total size of
     * WINDOW and return the sum.
     *
     * The new total size of WINDOW, if valid, will be shortly installed as
     * WINDOW's total height (see `window-total-height') or total width (see
     * `window-total-width').
     *
     * Note: This function does not operate on any child windows of WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-new-total", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowNewTotal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowNewTotal(Object window, Object size, Object add) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set new normal size of WINDOW to SIZE.
     * WINDOW must be a valid window and defaults to the selected one.
     * Return SIZE.
     *
     * The new normal size of WINDOW, if valid, will be shortly installed as
     * WINDOW's normal size (see `window-normal-size').
     *
     * Note: This function does not operate on any child windows of WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-new-normal", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowNewNormal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowNewNormal(Object window, Object size) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Apply requested size values for window-tree of FRAME.
     * If FRAME is omitted or nil, it defaults to the selected frame.
     *
     * Optional argument HORIZONTAL omitted or nil means apply requested
     * height values.  HORIZONTAL non-nil means apply requested width values.
     *
     * The requested size values are those set by `set-window-new-pixel' and
     * `set-window-new-normal'.  This function checks whether the requested
     * values sum up to a valid window layout, recursively assigns the new
     * sizes of all child windows and calculates and assigns the new start
     * positions of these windows.
     *
     * Return t if the requested values have been applied correctly, nil
     * otherwise.
     *
     * Note: This function does not check any of `window-fixed-size-p',
     * `window-min-height' or `window-min-width'.  All these checks have to
     * be applied on the Elisp level.
     * </pre>
     */
    @ELispBuiltIn(name = "window-resize-apply", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowResizeApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowResizeApply(Object frame, Object horizontal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Apply requested total size values for window-tree of FRAME.
     * If FRAME is omitted or nil, it defaults to the selected frame.
     *
     * This function does not assign pixel or normal size values.  You should
     * have run `window-resize-apply' before running this.
     *
     * Optional argument HORIZONTAL omitted or nil means apply requested
     * height values.  HORIZONTAL non-nil means apply requested width
     * values.
     * </pre>
     */
    @ELispBuiltIn(name = "window-resize-apply-total", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowResizeApplyTotal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowResizeApplyTotal(Object frame, Object horizontal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Split window OLD.
     * Second argument PIXEL-SIZE specifies the number of pixels of the
     * new window.  It must be a positive integer.
     *
     * Third argument SIDE nil (or `below') specifies that the new window shall
     * be located below WINDOW.  SIDE `above' means the new window shall be
     * located above WINDOW.  In both cases PIXEL-SIZE specifies the pixel
     * height of the new window including space reserved for the mode and/or
     * header/tab line.
     *
     * SIDE t (or `right') specifies that the new window shall be located on
     * the right side of WINDOW.  SIDE `left' means the new window shall be
     * located on the left of WINDOW.  In both cases PIXEL-SIZE specifies the
     * width of the new window including space reserved for fringes and the
     * scrollbar or a divider column.
     *
     * Fourth argument NORMAL-SIZE specifies the normal size of the new window
     * according to the SIDE argument.
     *
     * The new pixel and normal sizes of all involved windows must have been
     * set correctly.  See the code of `split-window' for how this is done.
     * </pre>
     */
    @ELispBuiltIn(name = "split-window-internal", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSplitWindowInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void splitWindowInternal(Object old, Object pixelSize, Object side, Object normalSize) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Remove WINDOW from its frame.
     * WINDOW defaults to the selected window.  Return nil.
     * Signal an error when WINDOW is the only window on its frame.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-window-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteWindowInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteWindowInternal(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Resize mini window WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "resize-mini-window-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FResizeMiniWindowInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void resizeMiniWindowInternal(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Scroll text of selected window upward ARG lines.
     * If ARG is omitted or nil, scroll upward by a near full screen.
     * A near full screen is `next-screen-context-lines' less than a full screen.
     * Negative ARG means scroll downward.
     * If ARG is the atom `-', scroll downward by nearly full screen.
     * When calling from a program, supply as argument a number, nil, or `-'.
     * </pre>
     */
    @ELispBuiltIn(name = "scroll-up", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FScrollUp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scrollUp(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Scroll text of selected window down ARG lines.
     * If ARG is omitted or nil, scroll down by a near full screen.
     * A near full screen is `next-screen-context-lines' less than a full screen.
     * Negative ARG means scroll upward.
     * If ARG is the atom `-', scroll upward by nearly full screen.
     * When calling from a program, supply as argument a number, nil, or `-'.
     * </pre>
     */
    @ELispBuiltIn(name = "scroll-down", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FScrollDown extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scrollDown(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return \"the other\" window for \"other window scroll\" commands.
     * If in the minibuffer, and `minibuffer-scroll-window' is non-nil,
     * it specifies the window to use.
     * Otherwise, if `other-window-scroll-buffer' is a buffer, a window
     * showing that buffer is the window to use, popping it up if necessary.
     * Otherwise, if `other-window-scroll-default' is a function, call it,
     * and the window it returns is the window to use.
     * Finally, the function looks for a neighboring window on the selected
     * frame, followed by windows on all the visible frames on the current
     * terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "other-window-for-scrolling", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FOtherWindowForScrolling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void otherWindowForScrolling() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Scroll selected window display ARG columns left.
     * Default for ARG is window width minus 2.
     * Value is the total amount of leftward horizontal scrolling in
     * effect after the change.
     * If SET-MINIMUM is non-nil, the new scroll amount becomes the
     * lower bound for automatic scrolling, i.e. automatic scrolling
     * will not scroll a window to a column less than the value returned
     * by this function.  This happens in an interactive call.
     * </pre>
     */
    @ELispBuiltIn(name = "scroll-left", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FScrollLeft extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scrollLeft(Object arg, Object setMinimum) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Scroll selected window display ARG columns right.
     * Default for ARG is window width minus 2.
     * Value is the total amount of leftward horizontal scrolling in
     * effect after the change.
     * If SET-MINIMUM is non-nil, the new scroll amount becomes the
     * lower bound for automatic scrolling, i.e. automatic scrolling
     * will not scroll a window to a column less than the value returned
     * by this function.  This happens in an interactive call.
     * </pre>
     */
    @ELispBuiltIn(name = "scroll-right", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FScrollRight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scrollRight(Object arg, Object setMinimum) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return window selected just before minibuffer window was selected.
     * Return nil if the selected window is not a minibuffer window.
     * </pre>
     */
    @ELispBuiltIn(name = "minibuffer-selected-window", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMinibufferSelectedWindow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minibufferSelectedWindow() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Center point in selected window and maybe redisplay frame.
     * With a numeric prefix argument ARG, recenter putting point on screen line ARG
     * relative to the selected window.  If ARG is negative, it counts up from the
     * bottom of the window.  (ARG should be less than the height of the window.)
     *
     * If ARG is omitted or nil, then recenter with point on the middle line
     * of the selected window; if REDISPLAY &amp; `recenter-redisplay' are
     * non-nil, also erase the entire frame and redraw it (when
     * `auto-resize-tool-bars' is set to `grow-only', this resets the
     * tool-bar's height to the minimum height needed); if
     * `recenter-redisplay' has the special value `tty', then only tty frames
     * are redrawn.  Interactively, REDISPLAY is always non-nil.
     *
     * Just C-u as prefix means put point in the center of the window
     * and redisplay normally--don't erase and redraw the frame.
     * </pre>
     */
    @ELispBuiltIn(name = "recenter", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRecenter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void recenter(Object arg, Object redisplay) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the width in columns of the text display area of WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * The returned width does not include dividers, scrollbars, margins,
     * fringes, nor any partial-width columns at the right of the text
     * area.
     *
     * Optional argument PIXELWISE non-nil, means to return the width in
     * pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "window-text-width", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowTextWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTextWidth(Object window, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the height in lines of the text display area of WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * The returned height does not include dividers, the mode line, any header
     * line, nor any partial-height lines at the bottom of the text area.
     *
     * Optional argument PIXELWISE non-nil, means to return the height in
     * pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "window-text-height", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowTextHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTextHeight(Object window, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Position point relative to window.
     * ARG nil means position point at center of window.
     * Else, ARG specifies vertical position within the window;
     * zero means top of window, negative means relative to bottom
     * of window, -1 meaning the last fully visible display line
     * of the window.
     *
     * Value is the screen line of the window point moved to, counting
     * from the top of the window.
     * </pre>
     */
    @ELispBuiltIn(name = "move-to-window-line", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMoveToWindowLine extends ELispBuiltInBaseNode {
        @Specialization
        public static Void moveToWindowLine(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a window-configuration object.
     * </pre>
     */
    @ELispBuiltIn(name = "window-configuration-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowConfigurationP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowConfigurationP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the frame that CONFIG, a window-configuration object, is about.
     * </pre>
     */
    @ELispBuiltIn(name = "window-configuration-frame", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowConfigurationFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowConfigurationFrame(Object config) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the configuration of windows and buffers as specified by CONFIGURATION.
     * CONFIGURATION must be a value previously returned
     * by `current-window-configuration' (which see).
     *
     * Normally, this function selects the frame of the CONFIGURATION, but if
     * DONT-SET-FRAME is non-nil, it leaves selected the frame which was
     * current at the start of the function.  If DONT-SET-MINIWINDOW is non-nil,
     * the mini-window of the frame doesn't get set to the corresponding element
     * of CONFIGURATION.
     *
     * This function consults the variable `window-restore-killed-buffer-windows'
     * when restoring a window whose buffer was killed after CONFIGURATION was
     * recorded.
     *
     * If CONFIGURATION was made from a frame that is now deleted,
     * only frame-independent values can be restored.  In this case,
     * the return value is nil.  Otherwise the value is t.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-configuration", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowConfiguration extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowConfiguration(Object configuration, Object dontSetFrame, Object dontSetMiniwindow) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an object representing the current window configuration of FRAME.
     * If FRAME is nil or omitted, use the selected frame.
     * This describes the number of windows, their sizes and current buffers,
     * and for each displayed buffer, where display starts, and the position of
     * point.  An exception is made for point in the current buffer:
     * its value is -not- saved.
     * This also records the currently selected frame, and FRAME's focus
     * redirection (see `redirect-frame-focus').  The variable
     * `window-persistent-parameters' specifies which window parameters are
     * saved by this function.
     * </pre>
     */
    @ELispBuiltIn(name = "current-window-configuration", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCurrentWindowConfiguration extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentWindowConfiguration(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set width of marginal areas of window WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * Second arg LEFT-WIDTH specifies the number of character cells to
     * reserve for the left marginal area.  Optional third arg RIGHT-WIDTH
     * does the same for the right marginal area.  A nil width parameter
     * means no margin.
     *
     * Leave margins unchanged if WINDOW is not large enough to accommodate
     * margins of the desired width.  Return t if any margin was actually
     * changed and nil otherwise.
     *
     * The margins specified by calling this function may be later overridden
     * by invoking `set-window-buffer' for the same WINDOW, with its
     * KEEP-MARGINS argument nil or omitted.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-margins", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetWindowMargins extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowMargins(Object window, Object leftWidth, Object rightWidth) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Get width of marginal areas of window WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
     * If a marginal area does not exist, its width will be returned
     * as nil.
     * </pre>
     */
    @ELispBuiltIn(name = "window-margins", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowMargins extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowMargins(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set fringes of specified WINDOW.
     * WINDOW must specify a live window and defaults to the selected one.
     *
     * Second arg LEFT-WIDTH specifies the number of pixels to reserve for
     * the left fringe.  Optional third arg RIGHT-WIDTH specifies the right
     * fringe width.  If a fringe width arg is nil, that means to use the
     * frame's default fringe width.  Default fringe widths can be set with
     * the command `set-fringe-style'.
     *
     * If optional fourth arg OUTSIDE-MARGINS is non-nil, draw the fringes
     * outside of the display margins.  By default, fringes are drawn between
     * display marginal areas and the text area.
     *
     * Optional fifth argument PERSISTENT non-nil means that fringe settings
     * for WINDOW are persistent, i.e., remain unchanged when another buffer
     * is shown in WINDOW.  PERSISTENT nil means that fringes are reset from
     * buffer local values when `set-window-buffer' is called on WINDOW with
     * the argument KEEP-MARGINS nil.
     *
     * Leave fringes unchanged if WINDOW is not large enough to accommodate
     * fringes of the desired width.  Return t if any fringe was actually
     * changed and nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-fringes", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FSetWindowFringes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowFringes(Object window, Object leftWidth, Object rightWidth, Object outsideMargins, Object persistent) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return fringe settings for specified WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * Value is a list of the form (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS
     * PERSISTENT), see `set-window-fringes'.
     * </pre>
     */
    @ELispBuiltIn(name = "window-fringes", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowFringes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowFringes(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the `cursor-type' of WINDOW to TYPE.
     *
     * This setting takes precedence over the variable `cursor-type', and TYPE
     * has the same format as the value of that variable.  The initial value
     * for new windows is t, which says to respect the buffer-local value of
     * `cursor-type'.
     *
     * WINDOW nil means use the selected window.  This setting persists across
     * buffers shown in WINDOW, so `set-window-buffer' does not reset it.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-cursor-type", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetWindowCursorType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowCursorType(Object window, Object type) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the `cursor-type' of WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     * </pre>
     */
    @ELispBuiltIn(name = "window-cursor-type", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowCursorType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowCursorType(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set width and type of scroll bars of specified WINDOW.
     * WINDOW must specify a live window and defaults to the selected one.
     *
     * Second argument WIDTH specifies the pixel width for the vertical scroll
     * bar.  If WIDTH is nil, use the scroll bar width of WINDOW's frame.
     * Third argument VERTICAL-TYPE specifies the type of the vertical scroll
     * bar: left, right, nil or t where nil means to not display a vertical
     * scroll bar on WINDOW and t means to use WINDOW frame's vertical scroll
     * bar type.
     *
     * Fourth argument HEIGHT specifies the pixel height for the horizontal
     * scroll bar.  If HEIGHT is nil, use the scroll bar height of WINDOW's
     * frame.  Fifth argument HORIZONTAL-TYPE specifies the type of the
     * horizontal scroll bar: bottom, nil, or t where nil means to not
     * display a horizontal scroll bar on WINDOW and t means to use WINDOW
     * frame's horizontal scroll bar type.  If WINDOW is a mini window, t
     * effectively behaves like nil.  HORIZONTAL-TYPE must equal bottom in
     * order to show a scroll bar for mini windows.
     *
     * Optional sixth argument PERSISTENT non-nil means that scroll bar
     * settings for WINDOW are persistent, i.e., remain unchanged when
     * another buffer is shown in WINDOW.  PERSISTENT nil means that scroll
     * bars are reset from buffer local values when `set-window-buffer' is
     * called on WINDOW with the argument KEEP-MARGINS nil.
     *
     * If WINDOW is not large enough to accommodate a scroll bar of the
     * desired dimension, leave the corresponding scroll bar unchanged.
     * Return t if scroll bars were actually changed and nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-scroll-bars", minArgs = 1, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FSetWindowScrollBars extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowScrollBars(Object window, Object width, Object verticalType, Object height, Object horizontalType, Object persistent) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Get width and type of scroll bars of window WINDOW.
     * WINDOW must be a live window and defaults to the selected one.
     *
     * Value is a list of the form (WIDTH COLUMNS VERTICAL-TYPE HEIGHT LINES
     * HORIZONTAL-TYPE PERSISTENT).  WIDTH reports the pixel width of the
     * vertical scroll bar; COLUMNS is the equivalent number of columns.
     * Similarly, HEIGHT and LINES are the height of the horizontal scroll
     * bar in pixels and the equivalent number of lines.  VERTICAL-TYPE
     * reports the type of the vertical scroll bar, either left, right, nil,
     * or t.  HORIZONTAL-TYPE reports the type of the horizontal scroll bar,
     * either bottom, nil or t.  PERSISTENT reports the value specified by
     * the last successful call to `set-window-scroll-bars', or nil if there
     * was none.
     *
     * If WIDTH or HEIGHT is nil or VERTICAL-TYPE or HORIZONTAL-TYPE is t,
     * WINDOW is using the corresponding value specified for the frame.
     * </pre>
     */
    @ELispBuiltIn(name = "window-scroll-bars", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowScrollBars extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowScrollBars(Object window) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the amount by which WINDOW is scrolled vertically.
     * This takes effect when displaying tall lines or images.
     *
     * If WINDOW is omitted or nil, it defaults to the selected window.
     * Normally, value is a multiple of the canonical character height of WINDOW;
     * optional second arg PIXELS-P means value is measured in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "window-vscroll", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowVscroll extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowVscroll(Object window, Object pixelsP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set amount by which WINDOW should be scrolled vertically to VSCROLL.
     * This takes effect when displaying tall lines or images.
     *
     * WINDOW nil means use the selected window.  Normally, VSCROLL is a
     * non-negative multiple of the canonical character height of WINDOW;
     * optional third arg PIXELS-P non-nil means that VSCROLL is in pixels.
     * If PIXELS-P is nil, VSCROLL may have to be rounded so that it
     * corresponds to an integral number of pixels.  The return value is the
     * result of this rounding.
     * If PIXELS-P is non-nil, the return value is VSCROLL.
     *
     * PRESERVE-VSCROLL-P makes setting the start of WINDOW preserve the
     * vscroll if its start is "frozen" due to a resized mini-window.
     * </pre>
     */
    @ELispBuiltIn(name = "set-window-vscroll", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetWindowVscroll extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setWindowVscroll(Object window, Object vscroll, Object pixelsP, Object preserveVscrollP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Say whether two window configurations have the same window layout.
     * This function ignores details such as the values of point and
     * scrolling positions.
     * </pre>
     */
    @ELispBuiltIn(name = "window-configuration-equal-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWindowConfigurationEqualP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowConfigurationEqualP(Object x, Object y) {
            throw new UnsupportedOperationException();
        }
    }
}
