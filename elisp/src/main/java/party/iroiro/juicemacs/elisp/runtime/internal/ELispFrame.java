package party.iroiro.juicemacs.elisp.runtime.internal;

public final class ELispFrame {
    private final ELispKboard kboard;

    public ELispFrame() {
        kboard = new ELispKboard();
    }

    public ELispKboard getKboard() {
        return kboard;
    }

    //#region struct frame
    /**
     * <pre>
     * Name of this frame: a Lisp string.  It is used for looking up resources,
     * as well as for the title in some cases.
     * </pre>
     */
    private Object name = false;
    public Object getName() { return name; }
    public void setName(Object value) { name = value; }
    /**
     * <pre>
     * The name to use for the icon, the last time
     * it was refreshed.  nil means not explicitly specified.
     * </pre>
     */
    private Object iconName = false;
    public Object getIconName() { return iconName; }
    public void setIconName(Object value) { iconName = value; }
    /**
     * <pre>
     * This is the frame title specified explicitly, if any.
     * Usually it is nil.
     * </pre>
     */
    private Object title = false;
    public Object getTitle() { return title; }
    public void setTitle(Object value) { title = value; }
    /**
     * <pre>
     * Last device to move over this frame.  Any value that isn't a
     * string means the "Virtual core pointer".
     * </pre>
     */
    private Object lastMouseDevice = false;
    public Object getLastMouseDevice() { return lastMouseDevice; }
    public void setLastMouseDevice(Object value) { lastMouseDevice = value; }
    /**
     * <pre>
     * The frame which should receive keystrokes that occur in this
     * frame, or nil if they should go to the frame itself.  This is
     * usually nil, but if the frame is minibufferless, we can use this
     * to redirect keystrokes to a surrogate minibuffer frame when
     * needed.
     *
     * Note that a value of nil is different from having the field point
     * to the frame itself.  Whenever the Fselect_frame function is used
     * to shift from one frame to the other, any redirections to the
     * original frame are shifted to the newly selected frame; if
     * focus_frame is nil, Fselect_frame will leave it alone.
     * </pre>
     */
    private Object focusFrame = false;
    public Object getFocusFrame() { return focusFrame; }
    public void setFocusFrame(Object value) { focusFrame = value; }
    /**
     * <pre>
     * This frame's root window.  Every frame has one.
     * If the frame has only a minibuffer window, this is it.
     * Otherwise, if the frame has a minibuffer window, this is its sibling.
     * </pre>
     */
    private Object rootWindow = false;
    public Object getRootWindow() { return rootWindow; }
    public void setRootWindow(Object value) { rootWindow = value; }
    /**
     * <pre>
     * This frame's selected window.
     * Each frame has its own window hierarchy
     * and one of the windows in it is selected within the frame.
     * This window may be the mini-window of the frame, if any.
     * The selected window of the selected frame is Emacs's selected window.
     * </pre>
     */
    private Object selectedWindow = false;
    public Object getSelectedWindow() { return selectedWindow; }
    public void setSelectedWindow(Object value) { selectedWindow = value; }
    /**
     * <pre>
     * This frame's selected window when run_window_change_functions was
     * called the last time on this frame.
     * </pre>
     */
    private Object oldSelectedWindow = false;
    public Object getOldSelectedWindow() { return oldSelectedWindow; }
    public void setOldSelectedWindow(Object value) { oldSelectedWindow = value; }
    /**
     * <pre>
     * This frame's minibuffer window.
     * Most frames have their own minibuffer windows,
     * but only the selected frame's minibuffer window
     * can actually appear to exist.
     * </pre>
     */
    private Object minibufferWindow = false;
    public Object getMinibufferWindow() { return minibufferWindow; }
    public void setMinibufferWindow(Object value) { minibufferWindow = value; }
    /**
     * <pre>
     * Parameter alist of this frame.
     * These are the parameters specified when creating the frame
     * or modified with modify-frame-parameters.
     * </pre>
     */
    private Object paramAlist = false;
    public Object getParamAlist() { return paramAlist; }
    public void setParamAlist(Object value) { paramAlist = value; }
    /**
     * <pre>
     * List of scroll bars on this frame.
     * Actually, we don't specify exactly what is stored here at all; the
     * scroll bar implementation code can use it to store anything it likes.
     * This field is marked by the garbage collector.  It is here
     * instead of in the `device' structure so that the garbage
     * collector doesn't need to look inside the window-system-dependent
     * structure.
     * </pre>
     */
    private Object scrollBars = false;
    public Object getScrollBars() { return scrollBars; }
    public void setScrollBars(Object value) { scrollBars = value; }
    private Object condemnedScrollBars = false;
    public Object getCondemnedScrollBars() { return condemnedScrollBars; }
    public void setCondemnedScrollBars(Object value) { condemnedScrollBars = value; }
    /**
     * <pre>
     * Vector describing the items to display in the menu bar.
     * Each item has four elements in this vector.
     * They are KEY, STRING, SUBMAP, and HPOS.
     * (HPOS is not used in when the X toolkit is in use.)
     * There are four additional elements of nil at the end, to terminate.
     * </pre>
     */
    private Object menuBarItems = false;
    public Object getMenuBarItems() { return menuBarItems; }
    public void setMenuBarItems(Object value) { menuBarItems = value; }
    /**
     * <pre>
     * Hash table of FACE-NAME keys and FACE-VECTOR-DATA values.
     * </pre>
     */
    private Object faceHashTable = false;
    public Object getFaceHashTable() { return faceHashTable; }
    public void setFaceHashTable(Object value) { faceHashTable = value; }
    /**
     * <pre>
     * A vector that records the entire structure of this frame's menu bar.
     * For the format of the data, see extensive comments in xmenu.c.
     * Only the X toolkit version uses this.
     * </pre>
     */
    private Object menuBarVector = false;
    public Object getMenuBarVector() { return menuBarVector; }
    public void setMenuBarVector(Object value) { menuBarVector = value; }
    /**
     * <pre>
     * Predicate for selecting buffers for other-buffer.
     * </pre>
     */
    private Object bufferPredicate = false;
    public Object getBufferPredicate() { return bufferPredicate; }
    public void setBufferPredicate(Object value) { bufferPredicate = value; }
    /**
     * <pre>
     * List of buffers viewed in this frame, for other-buffer.
     * </pre>
     */
    private Object bufferList = false;
    public Object getBufferList() { return bufferList; }
    public void setBufferList(Object value) { bufferList = value; }
    /**
     * <pre>
     * List of buffers that were viewed, then buried in this frame.  The
     * most recently buried buffer is first.  For last-buffer.
     * </pre>
     */
    private Object buriedBufferList = false;
    public Object getBuriedBufferList() { return buriedBufferList; }
    public void setBuriedBufferList(Object value) { buriedBufferList = value; }
    /**
     * <pre>
     * Where tool bar is, can be left, right, top or bottom.
     * Except with GTK, the only supported position is `top'.
     * </pre>
     */
    private Object toolBarPosition = false;
    public Object getToolBarPosition() { return toolBarPosition; }
    public void setToolBarPosition(Object value) { toolBarPosition = value; }
    /**
     * <pre>
     * Desired and current tab-bar items.
     * </pre>
     */
    private Object tabBarItems = false;
    public Object getTabBarItems() { return tabBarItems; }
    public void setTabBarItems(Object value) { tabBarItems = value; }
    /**
     * <pre>
     * Desired and current tool-bar items.
     * </pre>
     */
    private Object toolBarItems = false;
    public Object getToolBarItems() { return toolBarItems; }
    public void setToolBarItems(Object value) { toolBarItems = value; }
    //#endregion struct frame
}
