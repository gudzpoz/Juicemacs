package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInFrame extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFrameFactory.getFactories();
    }

    /**
     * <pre>
     * Return non-nil if OBJECT is a frame.
     * Value is:
     *   t for a termcap frame (a character-only terminal),
     *  `x' for an Emacs frame that is really an X window,
     *  `w32' for an Emacs frame that is a window on MS-Windows display,
     *  `ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
     *  `pc' for a direct-write MS-DOS frame,
     *  `pgtk' for an Emacs frame running on pure GTK.
     *  `haiku' for an Emacs frame running in Haiku.
     *  `android' for an Emacs frame running in Android.
     * See also `frame-live-p'.
     * </pre>
     */
    @ELispBuiltIn(name = "framep", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFramep extends ELispBuiltInBaseNode {
        @Specialization
        public static Void framep(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if OBJECT is a frame which has not been deleted.
     * Value is nil if OBJECT is not a live frame.  If object is a live
     * frame, the return value indicates what sort of terminal device it is
     * displayed on.  See the documentation of `framep' for possible
     * return values.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-live-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameLiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameLiveP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * The name of the window system that FRAME is displaying through.
     * The value is a symbol:
     *  nil for a termcap frame (a character-only terminal),
     *  `x' for an Emacs frame that is really an X window,
     *  `w32' for an Emacs frame that is a window on MS-Windows display,
     *  `ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
     *  `pc' for a direct-write MS-DOS frame.
     *  `pgtk' for an Emacs frame using pure GTK facilities.
     *  `haiku' for an Emacs frame running in Haiku.
     *  `android' for an Emacs frame running in Android.
     *
     * FRAME defaults to the currently selected frame.
     *
     * Use of this function as a predicate is deprecated.  Instead,
     * use `display-graphic-p' or any of the other `display-*-p'
     * predicates which report frame's specific UI-related capabilities.
     * </pre>
     */
    @ELispBuiltIn(name = "window-system", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FWindowSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowSystem(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * SKIP: real doc in window.el.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-windows-min-size", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FFrameWindowsMinSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameWindowsMinSize(Object frame, Object horizontal, Object ignore, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create an additional terminal frame, possibly on another terminal.
     * This function takes one argument, an alist specifying frame parameters.
     *
     * You can create multiple frames on a single text terminal, but only one
     * of them (the selected terminal frame) is actually displayed.
     *
     * In practice, generally you don't need to specify any parameters,
     * except when you want to create a new frame on another terminal.
     * In that case, the `tty' parameter specifies the device file to open,
     * and the `tty-type' parameter specifies the terminal type.  Example:
     *
     *    (make-terminal-frame \\='((tty . "/dev/pts/5") (tty-type . "xterm")))
     *
     * Note that changing the size of one terminal frame automatically
     * affects all frames on the same terminal device.
     * </pre>
     */
    @ELispBuiltIn(name = "make-terminal-frame", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeTerminalFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeTerminalFrame(Object parms) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Select FRAME.
     * Subsequent editing commands apply to its selected window.
     * Optional argument NORECORD means to neither change the order of
     * recently selected windows nor the buffer list.
     *
     * The selection of FRAME lasts until the next time the user does
     * something to select a different frame, or until the next time
     * this function is called.  If you are using a window system, the
     * previously selected frame may be restored as the selected frame
     * when returning to the command loop, because it still may have
     * the window system's input focus.  On a text terminal, the next
     * redisplay will display FRAME.
     *
     * This function returns FRAME, or nil if FRAME has been deleted.
     * </pre>
     */
    @ELispBuiltIn(name = "select-frame", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSelectFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void selectFrame(Object frame, Object norecord) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Handle a switch-frame event EVENT.
     * Switch-frame events are usually bound to this function.
     * A switch-frame event is an event Emacs sends itself to
     * indicate that input is arriving in a new frame. It does not
     * necessarily represent user-visible input focus.
     * </pre>
     */
    @ELispBuiltIn(name = "handle-switch-frame", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHandleSwitchFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void handleSwitchFrame(Object event) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the frame that is now selected.
     * </pre>
     */
    @ELispBuiltIn(name = "selected-frame", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSelectedFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void selectedFrame() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the old selected FRAME.
     * FRAME must be a live frame and defaults to the selected one.
     *
     * The return value is the frame selected the last time window change
     * functions were run.
     * </pre>
     */
    @ELispBuiltIn(name = "old-selected-frame", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FOldSelectedFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void oldSelectedFrame() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of all live frames.
     * The return value does not include any tooltip frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-list", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FFrameList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameList() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parent frame of FRAME.
     * The parent frame of FRAME is the Emacs frame whose window-system window
     * is the parent window of FRAME's window-system window.  When such a frame
     * exists, FRAME is considered a child frame of that frame.
     *
     * Return nil if FRAME has no parent frame.  This means that FRAME's
     * window-system window is either a "top-level" window (a window whose
     * parent window is the window-system's root window) or an embedded window
     * \(a window whose parent window is owned by some other application).
     * </pre>
     */
    @ELispBuiltIn(name = "frame-parent", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameParent(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if ANCESTOR is an ancestor of DESCENDANT.
     * ANCESTOR is an ancestor of DESCENDANT when it is either DESCENDANT's
     * parent frame or it is an ancestor of DESCENDANT's parent frame.  Both,
     * ANCESTOR and DESCENDANT must be live frames and default to the selected
     * frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-ancestor-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFrameAncestorP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameAncestorP(Object ancestor, Object descendant) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the next frame in the frame list after FRAME.
     * Only frames on the same terminal as FRAME are included in the list
     * of candidate frames.  If omitted, FRAME defaults to the selected frame.
     *
     * If MINIFRAME is nil (the default), include all frames except
     * minibuffer-only frames.
     *
     * If MINIFRAME is a window, include only its own frame and any frame now
     * using that window as the minibuffer.
     *
     * If MINIFRAME is `visible', include only visible frames.
     *
     * If MINIFRAME is 0, include only visible and iconified frames.
     *
     * If MINIFRAME is any other value, include all frames.
     * </pre>
     */
    @ELispBuiltIn(name = "next-frame", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNextFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nextFrame(Object frame, Object miniframe) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the previous frame in the frame list before FRAME.
     * It considers only frames on the same terminal as FRAME.
     * By default, skip minibuffer-only frames.
     * If omitted, FRAME defaults to the selected frame.
     * If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
     * If MINIFRAME is a window, include only its own frame
     * and any frame now using that window as the minibuffer.
     * If MINIFRAME is `visible', include all visible frames.
     * If MINIFRAME is 0, include all visible and iconified frames.
     * Otherwise, include all frames.
     * </pre>
     */
    @ELispBuiltIn(name = "previous-frame", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPreviousFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousFrame(Object frame, Object miniframe) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return last non-minibuffer frame selected.
     * </pre>
     */
    @ELispBuiltIn(name = "last-nonminibuffer-frame", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FLastNonminibufFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lastNonminibufFrame() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete FRAME, eliminating it from use.
     * FRAME must be a live frame and defaults to the selected one.
     *
     * When `undelete-frame-mode' is enabled, the 16 most recently deleted
     * frames can be undeleted with `undelete-frame', which see.
     *
     * A frame may not be deleted if its minibuffer serves as surrogate
     * minibuffer for another frame.  Normally, you may not delete a frame if
     * all other frames are invisible, but if the second optional argument
     * FORCE is non-nil, you may do so.
     *
     * This function runs `delete-frame-functions' before actually
     * deleting the frame, unless the frame is a tooltip.
     * The functions are run with one argument, the frame to be deleted.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-frame", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteFrame(Object frame, Object force) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list (FRAME X . Y) giving the current mouse frame and position.
     * The position is given in canonical character cells, where (0, 0) is the
     * upper-left corner of the frame, X is the horizontal offset, and Y is the
     * vertical offset, measured in units of the frame's default character size.
     * If Emacs is running on a mouseless terminal or hasn't been programmed
     * to read the mouse position, it returns the selected frame for FRAME
     * and nil for X and Y.
     *
     * FRAME might be nil if `track-mouse' is set to `drag-source'.  This
     * means there is no frame under the mouse.  If `mouse-position-function'
     * is non-nil, `mouse-position' calls it, passing the normal return value
     * to that function as an argument, and returns whatever that function
     * returns.
     * </pre>
     */
    @ELispBuiltIn(name = "mouse-position", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMousePosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mousePosition() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list (FRAME X . Y) giving the current mouse frame and position.
     * The position is given in pixel units, where (0, 0) is the
     * upper-left corner of the frame, X is the horizontal offset, and Y is
     * the vertical offset.
     * FRAME might be nil if `track-mouse' is set to `drag-source'.  This
     * means there is no frame under the mouse.  If Emacs is running on a
     * mouseless terminal or hasn't been programmed to read the mouse
     * position, it returns the selected frame for FRAME and nil for X and
     * Y.
     * </pre>
     */
    @ELispBuiltIn(name = "mouse-pixel-position", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMousePixelPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mousePixelPosition() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move the mouse pointer to the center of character cell (X,Y) in FRAME.
     * Coordinates are relative to the frame, not a window,
     * so the coordinates of the top left character in the frame
     * may be nonzero due to left-hand scroll bars or the menu bar.
     *
     * The position is given in canonical character cells, where (0, 0) is
     * the upper-left corner of the frame, X is the horizontal offset, and
     * Y is the vertical offset, measured in units of the frame's default
     * character size.
     *
     * This function is a no-op for an X frame that is not visible.
     * If you have just created a frame, you must wait for it to become visible
     * before calling this function on it, like this.
     *   (while (not (frame-visible-p frame)) (sleep-for .5))
     * </pre>
     */
    @ELispBuiltIn(name = "set-mouse-position", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetMousePosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setMousePosition(Object frame, Object x, Object y) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move the mouse pointer to pixel position (X,Y) in FRAME.
     * The position is given in pixels, where (0, 0) is the upper-left corner
     * of the frame, X is the horizontal offset, and Y is the vertical offset.
     *
     * Note, this is a no-op for an X frame that is not visible.
     * If you have just created a frame, you must wait for it to become visible
     * before calling this function on it, like this.
     *   (while (not (frame-visible-p frame)) (sleep-for .5))
     * </pre>
     */
    @ELispBuiltIn(name = "set-mouse-pixel-position", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetMousePixelPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setMousePixelPosition(Object frame, Object x, Object y) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make the frame FRAME visible (assuming it is an X window).
     * If omitted, FRAME defaults to the currently selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "make-frame-visible", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeFrameVisible extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeFrameVisible(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make the frame FRAME invisible.
     * If omitted, FRAME defaults to the currently selected frame.
     * On graphical displays, invisible frames are not updated and are
     * usually not displayed at all, even in a window system's \"taskbar\".
     *
     * Normally you may not make FRAME invisible if all other frames are invisible,
     * but if the second optional argument FORCE is non-nil, you may do so.
     *
     * This function has no effect on text terminal frames.  Such frames are
     * always considered visible, whether or not they are currently being
     * displayed in the terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "make-frame-invisible", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeFrameInvisible extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeFrameInvisible(Object frame, Object force) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make the frame FRAME into an icon.
     * If omitted, FRAME defaults to the currently selected frame.
     *
     * If FRAME is a child frame, consult the variable `iconify-child-frame'
     * for how to proceed.
     * </pre>
     */
    @ELispBuiltIn(name = "iconify-frame", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIconifyFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void iconifyFrame(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if FRAME is \"visible\" (actually in use for display).
     * Return the symbol `icon' if FRAME is iconified or \"minimized\".
     * Return nil if FRAME was made invisible, via `make-frame-invisible'.
     * On graphical displays, invisible frames are not updated and are
     * usually not displayed at all, even in a window system's \"taskbar\".
     *
     * If FRAME is a text terminal frame, this always returns t.
     * Such frames are always considered visible, whether or not they are
     * currently being displayed on the terminal.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-visible-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameVisibleP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameVisibleP(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of all frames now \"visible\" (being updated).
     * </pre>
     */
    @ELispBuiltIn(name = "visible-frame-list", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FVisibleFrameList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void visibleFrameList() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Bring FRAME to the front, so it occludes any frames it overlaps.
     * If FRAME is invisible or iconified, make it visible.
     * If you don't specify a frame, the selected frame is used.
     * If Emacs is displaying on an ordinary terminal or some other device which
     * doesn't support multiple overlapping frames, this function selects FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "raise-frame", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRaiseFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void raiseFrame(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Send FRAME to the back, so it is occluded by any frames that overlap it.
     * If you don't specify a frame, the selected frame is used.
     * If Emacs is displaying on an ordinary terminal or some other device which
     * doesn't support multiple overlapping frames, this function does nothing.
     * </pre>
     */
    @ELispBuiltIn(name = "lower-frame", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLowerFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lowerFrame(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.
     * In other words, switch-frame events caused by events in FRAME will
     * request a switch to FOCUS-FRAME, and `last-event-frame' will be
     * FOCUS-FRAME after reading an event typed at FRAME.
     *
     * If FOCUS-FRAME is nil, any existing redirection is canceled, and the
     * frame again receives its own keystrokes.
     *
     * Focus redirection is useful for temporarily redirecting keystrokes to
     * a surrogate minibuffer frame when a frame doesn't have its own
     * minibuffer window.
     *
     * A frame's focus redirection can be changed by `select-frame'.  If frame
     * FOO is selected, and then a different frame BAR is selected, any
     * frames redirecting their focus to FOO are shifted to redirect their
     * focus to BAR.  This allows focus redirection to work properly when the
     * user switches from one frame to another using `select-window'.
     *
     * This means that a frame whose focus is redirected to itself is treated
     * differently from a frame whose focus is redirected to nil; the former
     * is affected by `select-frame', while the latter is not.
     *
     * The redirection lasts until `redirect-frame-focus' is called to change it.
     * </pre>
     */
    @ELispBuiltIn(name = "redirect-frame-focus", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRedirectFrameFocus extends ELispBuiltInBaseNode {
        @Specialization
        public static Void redirectFrameFocus(Object frame, Object focusFrame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the frame to which FRAME's keystrokes are currently being sent.
     * If FRAME is omitted or nil, the selected frame is used.
     * Return nil if FRAME's focus is not redirected.
     * See `redirect-frame-focus'.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-focus", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameFocus extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameFocus(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the input focus to FRAME.
     * FRAME nil means use the selected frame.  Optional argument NOACTIVATE
     * means do not activate FRAME.
     *
     * If there is no window system support, this function does nothing.
     * </pre>
     */
    @ELispBuiltIn(name = "x-focus-frame", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FXFocusFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void xFocusFrame(Object frame, Object noactivate) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Mark FRAME as made.
     * FRAME nil means use the selected frame.  Second argument MADE non-nil
     * means functions on `window-configuration-change-hook' are called
     * whenever the window configuration of FRAME changes.  MADE nil means
     * these functions are not called.
     *
     * This function is currently called by `make-frame' only and should be
     * otherwise used with utter care to avoid that running functions on
     * `window-configuration-change-hook' is impeded forever.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-after-make-frame", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFrameAfterMakeFrame extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameAfterMakeFrame(Object frame, Object made) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parameters-alist of frame FRAME.
     * It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.
     * The meaningful PARMs depend on the kind of frame.
     * If FRAME is omitted or nil, return information on the currently selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-parameters", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameParameters extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameParameters(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return FRAME's value for parameter PARAMETER.
     * If FRAME is nil, describe the currently selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-parameter", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFrameParameter extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameParameter(Object frame, Object parameter) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Modify FRAME according to new values of its parameters in ALIST.
     * If FRAME is nil, it defaults to the selected frame.
     * ALIST is an alist of parameters to change and their new values.
     * Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.
     * Which PARMs are meaningful depends on the kind of frame.
     * The meaningful parameters are acted upon, i.e. the frame is changed
     * according to their new values, and are also stored in the frame's
     * parameter list so that `frame-parameters' will return them.
     * PARMs that are not meaningful are still stored in the frame's parameter
     * list, but are otherwise ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "modify-frame-parameters", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FModifyFrameParameters extends ELispBuiltInBaseNode {
        @Specialization
        public static Void modifyFrameParameters(Object frame, Object alist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Height in pixels of a line in the font in frame FRAME.
     * If FRAME is omitted or nil, the selected frame is used.
     * For a terminal frame, the value is always 1.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-char-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameCharHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameCharHeight(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Width in pixels of characters in the font in frame FRAME.
     * If FRAME is omitted or nil, the selected frame is used.
     * On a graphical screen, the width is the standard width of the default font.
     * For a terminal screen, the value is always 1.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-char-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameCharWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameCharWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return FRAME's native width in pixels.
     * For a terminal frame, the result really gives the width in characters.
     * If FRAME is omitted or nil, the selected frame is used.
     *
     * If you're interested only in the width of the text portion of the
     * frame, see `frame-text-width' instead.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-native-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameNativeWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameNativeWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return FRAME's native height in pixels.
     * If FRAME is omitted or nil, the selected frame is used.  The exact value
     * of the result depends on the window-system and toolkit in use:
     *
     * In the Gtk+ and NS versions, it includes only any window (including the
     * minibuffer or echo area), mode line, and header line.  It does not
     * include the tool bar or menu bar.  With other graphical versions, it may
     * also include the tool bar and the menu bar.
     *
     * If you're interested only in the height of the text portion of the
     * frame, see `frame-text-height' instead.
     *
     * For a text terminal, it includes the menu bar.  In this case, the
     * result is really in characters rather than pixels (i.e., is identical
     * to `frame-height').
     * </pre>
     */
    @ELispBuiltIn(name = "frame-native-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameNativeHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameNativeHeight(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width in pixels of FRAME's tool bar.
     * The result is greater than zero only when the tool bar is on the left
     * or right side of FRAME.  If FRAME is omitted or nil, the selected frame
     * is used.
     * </pre>
     */
    @ELispBuiltIn(name = "tool-bar-pixel-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FToolBarPixelWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void toolBarPixelWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width in columns of FRAME's text area.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-text-cols", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTextCols extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameTextCols(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return height in lines of FRAME's text area.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-text-lines", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTextLines extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameTextLines(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return number of total columns of FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-total-cols", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTotalCols extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameTotalCols(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return number of total lines of FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-total-lines", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTotalLines extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameTotalLines(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return text area width of FRAME in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-text-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTextWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameTextWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return text area height of FRAME in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-text-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameTextHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameTextHeight(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return scroll bar width of FRAME in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-scroll-bar-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FScrollBarWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scrollBarWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return scroll bar height of FRAME in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-scroll-bar-height", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FScrollBarHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void scrollBarHeight(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return fringe width of FRAME in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-fringe-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFringeWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fringeWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width of FRAME's child-frame border in pixels.
     *  If FRAME's `child-frame-border-width' parameter is nil, return FRAME's
     *  internal border width instead.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-child-frame-border-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameChildFrameBorderWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameChildFrameBorderWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width of FRAME's internal border in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-internal-border-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameInternalBorderWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameInternalBorderWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width (in pixels) of vertical window dividers on FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-right-divider-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRightDividerWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void rightDividerWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width (in pixels) of horizontal window dividers on FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-bottom-divider-width", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBottomDividerWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bottomDividerWidth(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set text height of frame FRAME to HEIGHT lines.
     * Optional third arg PRETEND non-nil means that redisplay should use
     * HEIGHT lines but that the idea of the actual height of the frame should
     * not be changed.
     *
     * Optional fourth argument PIXELWISE non-nil means that FRAME should be
     * HEIGHT pixels high.  Note: When `frame-resize-pixelwise' is nil, some
     * window managers may refuse to honor a HEIGHT that is not an integer
     * multiple of the default frame font height.
     *
     * When called interactively, HEIGHT is the numeric prefix and the
     * currently selected frame will be set to this height.
     *
     * If FRAME is nil, it defaults to the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "set-frame-height", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetFrameHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFrameHeight(Object frame, Object height, Object pretend, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set text width of frame FRAME to WIDTH columns.
     * Optional third arg PRETEND non-nil means that redisplay should use WIDTH
     * columns but that the idea of the actual width of the frame should not
     * be changed.
     *
     * Optional fourth argument PIXELWISE non-nil means that FRAME should be
     * WIDTH pixels wide.  Note: When `frame-resize-pixelwise' is nil, some
     * window managers may refuse to honor a WIDTH that is not an integer
     * multiple of the default frame font width.
     *
     * When called interactively, WIDTH is the numeric prefix and the
     * currently selected frame will be set to this width.
     *
     * If FRAME is nil, it defaults to the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "set-frame-width", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetFrameWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFrameWidth(Object frame, Object width, Object pretend, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set text size of FRAME to WIDTH by HEIGHT, measured in characters.
     * Optional argument PIXELWISE non-nil means to measure in pixels.  Note:
     * When `frame-resize-pixelwise' is nil, some window managers may refuse to
     * honor a WIDTH that is not an integer multiple of the default frame font
     * width or a HEIGHT that is not an integer multiple of the default frame
     * font height.
     *
     * If FRAME is nil, it defaults to the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "set-frame-size", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetFrameSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFrameSize(Object frame, Object width, Object height, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return top left corner of FRAME in pixels.
     * FRAME must be a live frame and defaults to the selected one.  The return
     * value is a cons (x, y) of the coordinates of the top left corner of
     * FRAME's outer frame, in pixels relative to an origin (0, 0) of FRAME's
     * display.
     *
     * Note that the values returned are not guaranteed to be accurate: The
     * values depend on the underlying window system, and some systems add a
     * constant offset to the values.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-position", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFramePosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void framePosition(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set position of FRAME to (X, Y).
     * FRAME must be a live frame and defaults to the selected one.  X and Y,
     * if positive, specify the coordinate of the left and top edge of FRAME's
     * outer frame in pixels relative to an origin (0, 0) of FRAME's display.
     * If any of X or Y is negative, it specifies the coordinates of the right
     * or bottom edge of the outer frame of FRAME relative to the right or
     * bottom edge of FRAME's display.
     * </pre>
     */
    @ELispBuiltIn(name = "set-frame-position", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetFramePosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFramePosition(Object frame, Object x, Object y) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if FRAME's window state change flag is set, nil otherwise.
     * FRAME must be a live frame and defaults to the selected one.
     *
     * If FRAME's window state change flag is set, the default values of
     * `window-state-change-functions' and `window-state-change-hook' will be
     * run during next redisplay, regardless of whether a window state change
     * actually occurred on FRAME or not.  After that, the value of this flag
     * is reset.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-window-state-change", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameWindowStateChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameWindowStateChange(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set FRAME's window state change flag according to ARG.
     * Set FRAME's window state change flag if ARG is non-nil, reset it
     * otherwise.
     *
     * If FRAME's window state change flag is set, the default values of
     * `window-state-change-functions' and `window-state-change-hook' will be
     * run during next redisplay, regardless of whether a window state change
     * actually occurred on FRAME or not.  After that, the value of FRAME's
     * window state change flag is reset.
     * </pre>
     */
    @ELispBuiltIn(name = "set-frame-window-state-change", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetFrameWindowStateChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFrameWindowStateChange(Object frame, Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return FRAMEs scale factor.
     * If FRAME is omitted or nil, the selected frame is used.
     * The scale factor is the amount by which a logical pixel size must be
     * multiplied to find the real number of pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-scale-factor", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameScaleFactor extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameScaleFactor(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.
     * This uses `INSTANCE.ATTRIBUTE' as the key and `Emacs.CLASS' as the
     * class, where INSTANCE is the name under which Emacs was invoked, or
     * the name specified by the `-name' or `-rn' command-line arguments.
     *
     * The optional arguments COMPONENT and SUBCLASS add to the key and the
     * class, respectively.  You must specify both of them or neither.
     * If you specify them, the key is `INSTANCE.COMPONENT.ATTRIBUTE'
     * and the class is `Emacs.CLASS.SUBCLASS'.
     * </pre>
     */
    @ELispBuiltIn(name = "x-get-resource", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FXGetResource extends ELispBuiltInBaseNode {
        @Specialization
        public static Void xGetResource(Object attribute, Object class_, Object component, Object subclass) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Parse a display geometry string STRING.
     * Returns an alist of the form ((top . TOP), (left . LEFT) ... ).
     * The properties returned may include `top', `left', `height', and `width'.
     * For X, the value of `left' or `top' may be an integer,
     * or a list (+ N) meaning N pixels relative to top/left corner,
     * or a list (- N) meaning -N pixels relative to bottom/right corner.
     * On Nextstep, this just calls `ns-parse-geometry'.
     * </pre>
     */
    @ELispBuiltIn(name = "x-parse-geometry", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FXParseGeometry extends ELispBuiltInBaseNode {
        @Specialization
        public static Void xParseGeometry(Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if the mouse pointer displayed on FRAME is visible.
     * Otherwise it returns nil.  FRAME omitted or nil means the
     * selected frame.  This is useful when `make-pointer-invisible' is set.
     * </pre>
     */
    @ELispBuiltIn(name = "frame-pointer-visible-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFramePointerVisibleP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void framePointerVisibleP(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set FRAME's was-invisible flag if WAS-INVISIBLE is non-nil.
     * This function is for internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "frame--set-was-invisible", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFrameSetWasInvisible extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameSetWasInvisible(Object frame, Object wasInvisible) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Recreate FRAME's default font using updated font parameters.
     * Signal an error if FRAME is not a window system frame.  This should be
     * called after a `config-changed' event is received, signaling that the
     * parameters (such as pixel density) used by the system to open fonts
     * have changed.
     * </pre>
     */
    @ELispBuiltIn(name = "reconsider-frame-fonts", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReconsiderFrameFonts extends ELispBuiltInBaseNode {
        @Specialization
        public static Void reconsiderFrameFonts(Object frame) {
            throw new UnsupportedOperationException();
        }
    }
}
