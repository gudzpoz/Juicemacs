package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInXDisp extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInXDispFactory.getFactories();
    }

    /**
     * <pre>
     * Mark the current buffer for redisplay.
     * This function may be passed to `add-variable-watcher'.
     * </pre>
     */
    @ELispBuiltIn(name = "set-buffer-redisplay", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetBufferRedisplay extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setBufferRedisplay(Object symbol, Object newval, Object op, Object where) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return height in pixels of text line in the selected window.
     *
     * Value is the height in pixels of the line at point.
     * </pre>
     */
    @ELispBuiltIn(name = "line-pixel-height", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FLinePixelHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void linePixelHeight() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the extents of glyph in FRAME for mouse event generation.
     * Return a rectangle (X Y WIDTH HEIGHT) representing the confines, in
     * pixel coordinates, of the glyph at X, Y and in FRAME, or, should
     * `mouse-fine-grained-tracking' or `window-resize-pixelwise` be enabled,
     * an approximation thereof.  All coordinates are relative to the origin
     * point of FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "remember-mouse-glyph", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FRememberMouseGlyph extends ELispBuiltInBaseNode {
        @Specialization
        public static Void rememberMouseGlyph(Object frame, Object x, Object y) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Get the value of the `display' property PROP at POSITION.
     * If OBJECT, this should be a buffer or string where the property is
     * fetched from.  If omitted, OBJECT defaults to the current buffer.
     *
     * If PROPERTIES, look for value of PROP in PROPERTIES instead of the
     * properties at POSITION.
     * </pre>
     */
    @ELispBuiltIn(name = "get-display-property", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FGetDisplayProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getDisplayProperty(Object position, Object prop, Object object, Object properties) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the dimensions of the text of WINDOW's buffer in pixels.
     * WINDOW must be a live window and defaults to the selected one.  The
     * return value is a cons of the maximum pixel-width of any text line and
     * the pixel-height of all the text lines in the accessible portion of
     * buffer text.
     *
     * If FROM is a cons cell, the return value includes, in addition to the
     * dimensions, also a third element that provides the buffer position
     * from which measuring of the text dimensions was actually started.
     *
     * This function exists to allow Lisp programs to adjust the dimensions
     * of WINDOW to the buffer text it needs to display.
     *
     * The optional argument FROM, if non-nil, specifies the first text
     * position to consider, and defaults to the minimum accessible position
     * of the buffer.  If FROM is a cons, its car specifies a buffer
     * position, and its cdr specifies the vertical offset in pixels from
     * that position to the first screen line to be measured.  If FROM is t,
     * it stands for the minimum accessible position that starts a non-empty
     * line.  TO, if non-nil, specifies the last text position and defaults
     * to the maximum accessible position of the buffer.  If TO is t, it
     * stands for the maximum accessible position that ends a non-empty line.
     *
     * The optional argument X-LIMIT, if non-nil, specifies the maximum X
     * coordinate beyond which the text should be ignored.  It is therefore
     * also the maximum width that the function can return.  X-LIMIT nil or
     * omitted means to use the pixel-width of WINDOW's body.  This default
     * means text of truncated lines wider than the window will be ignored;
     * specify a non-nil value for X-LIMIT if lines are truncated and you need
     * to account for the truncated text.
     *
     * Use nil for X-LIMIT if you want to know how high WINDOW should become in
     * order to fit all of its buffer's text with the width of WINDOW
     * unaltered.  Use the maximum width WINDOW may assume if you intend to
     * change WINDOW's width.  Use t for the maximum possible value.  Since
     * calculating the width of long lines can take some time, it's always a
     * good idea to make this argument as small as possible; in particular, if
     * the buffer contains long lines that shall be truncated anyway.
     *
     * The optional argument Y-LIMIT, if non-nil, specifies the maximum Y
     * coordinate beyond which the text is to be ignored; it is therefore
     * also the maximum height that the function can return (excluding the
     * height of the mode- or header-line, if any).  Y-LIMIT nil or omitted
     * means consider all of the accessible portion of buffer text up to the
     * position specified by TO.  Since calculating the text height of a
     * large buffer can take some time, it makes sense to specify this
     * argument if the size of the buffer is large or unknown.
     *
     * Optional argument MODE-LINES nil or omitted means do not include the
     * height of the mode-, tab- or header-line of WINDOW in the return value.
     * If it is the symbol `mode-line', `tab-line' or `header-line', include
     * only the height of that line, if present, in the return value.  If t,
     * include the height of any of these, if present, in the return value.
     *
     * IGNORE-LINE-AT-END, if non-nil, means to not add the height of the
     * screen line that includes TO to the returned height of the text.
     * </pre>
     */
    @ELispBuiltIn(name = "window-text-pixel-size", minArgs = 0, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FWindowTextPixelSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void windowTextPixelSize(Object window, Object from, Object to, Object xLimit, Object yLimit, Object modeLines, Object ignoreLineAtEnd) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the dimensions of whole text of BUFFER-OR-NAME in WINDOW.
     * BUFFER-OR-NAME must specify a live buffer or the name of a live buffer
     * and defaults to the current buffer.  WINDOW must be a live window and
     * defaults to the selected one.  The return value is a cons of the maximum
     * pixel-width of any text line and the pixel-height of all the text lines
     * of the buffer specified by BUFFER-OR-NAME.
     *
     * The optional arguments X-LIMIT and Y-LIMIT have the same meaning as with
     * `window-text-pixel-size'.
     *
     * Do not use this function if the buffer specified by BUFFER-OR-NAME is
     * already displayed in WINDOW.  `window-text-pixel-size' is cheaper in
     * that case because it does not have to temporarily show that buffer in
     * WINDOW.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-text-pixel-size", minArgs = 0, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FBufferTextPixelSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferTextPixelSize(Object bufferOrName, Object window, Object xLimit, Object yLimit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if the current screen line is continued on display.
     * </pre>
     */
    @ELispBuiltIn(name = "display--line-is-continued-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDisplayLineIsContinuedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void displayLineIsContinuedP() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of lines occupied by the tab bar of FRAME.
     * If FRAME is nil or omitted, use the selected frame.  Optional argument
     * PIXELWISE non-nil means return the height of the tab bar in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "tab-bar-height", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTabBarHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void tabBarHeight(Object frame, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of lines occupied by the tool bar of FRAME.
     * If FRAME is nil or omitted, use the selected frame.  Optional argument
     * PIXELWISE non-nil means return the height of the tool bar in pixels.
     * </pre>
     */
    @ELispBuiltIn(name = "tool-bar-height", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FToolBarHeight extends ELispBuiltInBaseNode {
        @Specialization
        public static Void toolBarHeight(Object frame, Object pixelwise) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if long-line optimizations are in effect in current buffer.
     * See `long-line-threshold' and `large-hscroll-threshold' for what these
     * optimizations mean and when they are in effect.
     * </pre>
     */
    @ELispBuiltIn(name = "long-line-optimizations-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FLongLineOptimizationsP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void longLineOptimizationsP() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Dump the current matrix of the selected window to stderr.
     * Shows contents of glyph row structures.  With non-nil
     * parameter GLYPHS, dump glyphs as well.  If GLYPHS is 1 show
     * glyphs in short form, otherwise show glyphs in long form.
     *
     * Interactively, no argument means show glyphs in short form;
     * with numeric argument, its value is passed as the GLYPHS flag.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-glyph-matrix", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDumpGlyphMatrix extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpGlyphMatrix(Object glyphs) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Dump the current glyph matrix of the selected frame to stderr.
     * Only text-mode frames have frame glyph matrices.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-frame-glyph-matrix", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDumpFrameGlyphMatrix extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpFrameGlyphMatrix() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Dump glyph row ROW to stderr.
     * Interactively, ROW is the prefix numeric argument and defaults to
     * the row which displays point.
     * Optional argument GLYPHS 0 means don't dump glyphs.
     * GLYPHS 1 means dump glyphs in short form.
     * GLYPHS &gt; 1 or omitted means dump glyphs in long form.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-glyph-row", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpGlyphRow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpGlyphRow(Object row, Object glyphs) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Dump glyph row ROW of the tab-bar of the current frame to stderr.
     * Interactively, ROW is the prefix numeric argument and defaults to zero.
     * GLYPHS 0 means don't dump glyphs.
     * GLYPHS 1 means dump glyphs in short form.
     * GLYPHS &gt; 1 or omitted means dump glyphs in long form.
     *
     * If there's no tab-bar, or if the tab-bar is not drawn by Emacs,
     * do nothing.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-tab-bar-row", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpTabBarRow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpTabBarRow(Object row, Object glyphs) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Dump glyph row ROW of the tool-bar of the current frame to stderr.
     * Interactively, ROW is the prefix numeric argument and defaults to zero.
     * GLYPHS 0 means don't dump glyphs.
     * GLYPHS 1 means dump glyphs in short form.
     * GLYPHS &gt; 1 or omitted means dump glyphs in long form.
     *
     * If there's no tool-bar, or if the tool-bar is not drawn by Emacs,
     * do nothing.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-tool-bar-row", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpToolBarRow extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpToolBarRow(Object row, Object glyphs) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Toggle tracing of redisplay.
     * With ARG, turn tracing on if and only if ARG is positive.
     * </pre>
     */
    @ELispBuiltIn(name = "trace-redisplay", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTraceRedisplay extends ELispBuiltInBaseNode {
        @Specialization
        public static Void traceRedisplay(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Like `format', but print result to stderr.
     * usage: (trace-to-stderr STRING &amp;rest OBJECTS)
     * </pre>
     */
    @ELispBuiltIn(name = "trace-to-stderr", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FTraceToStderr extends ELispBuiltInBaseNode {
        @Specialization
        public static Void traceToStderr(Object string, Object[] objects) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return paragraph direction at point in BUFFER.
     * Value is either `left-to-right' or `right-to-left'.
     * If BUFFER is omitted or nil, it defaults to the current buffer.
     *
     * Paragraph direction determines how the text in the paragraph is displayed.
     * In left-to-right paragraphs, text begins at the left margin of the window
     * and the reading direction is generally left to right.  In right-to-left
     * paragraphs, text begins at the right margin and is read from right to left.
     *
     * See also `bidi-paragraph-direction'.
     * </pre>
     */
    @ELispBuiltIn(name = "current-bidi-paragraph-direction", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCurrentBidiParagraphDirection extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentBidiParagraphDirection(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return position between FROM and TO where directionality was overridden.
     *
     * This function returns the first character position in the specified
     * region of OBJECT where characters have their bidirectional
     * properties affected in a way that might make its text look confusingly
     * on display.  For example, characters whose `bidi-class' property is `L',
     * could be forced to display as `R' by a directional override, and
     * likewise characters whose `bidi-class' is `R' or `AL' that are
     * forced to display as `L'.
     *
     * If no such character is found, the function returns nil.
     *
     * OBJECT is a Lisp string or buffer to search for overridden
     * directionality, and defaults to the current buffer if nil.
     * OBJECT can also be a window, in which case the function will search
     * the buffer displayed in that window.  Passing the window instead of
     * a buffer is preferable when the buffer is displayed in some window,
     * because this function will then be able to correctly account for
     * window-specific overlays, which can affect the results.
     *
     * Optional argument BASE-DIR specifies the base paragraph directory
     * of the text.  It should be a symbol, either `left-to-right'
     * or `right-to-left', and defaults to `left-to-right'.
     *
     * Strong directional characters `L', `R', and `AL' can have their
     * intrinsic directionality overridden by directional override control
     * characters RLO (u+202E) and LRO (u+202D).  They can also have their
     * directionality affected by other formatting control characters: LRE
     * (u+202A), RLE (u+202B), LRI (u+2066), and RLI (u+2067).  See the
     * function `get-char-code-property' for a way to inquire about the
     * `bidi-class' property of a character.  Characters whose intrinsic
     * directionality is weak or neutral, such as numbers or punctuation
     * characters, can be forced to display in a very different place with
     * respect of its surrounding characters, so as to make the surrounding
     * text confuse the user regarding what the text says.
     *
     * Also see the `highlight-confusing-reorderings' function, which can be
     * useful in similar circumstances as this function.
     * </pre>
     */
    @ELispBuiltIn(name = "bidi-find-overridden-directionality", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FBidiFindOverriddenDirectionality extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bidiFindOverriddenDirectionality(Object from, Object to, Object object, Object baseDir) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Move point in the visual order in the specified DIRECTION.
     * DIRECTION can be 1, meaning move to the right, or -1, which moves to the
     * left.
     *
     * Value is the new character position of point.
     * </pre>
     */
    @ELispBuiltIn(name = "move-point-visually", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMovePointVisually extends ELispBuiltInBaseNode {
        @Specialization
        public static Void movePointVisually(Object direction) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the resolved bidirectional levels of characters at VPOS.
     *
     * The resolved levels are produced by the Emacs bidi reordering engine
     * that implements the UBA, the Unicode Bidirectional Algorithm.  Please
     * read the Unicode Standard Annex 9 (UAX#9) for background information
     * about these levels.
     *
     * VPOS is the zero-based number of the current window's screen line
     * for which to produce the resolved levels.  If VPOS is nil or omitted,
     * it defaults to the screen line of point.  If the window displays a
     * header line, VPOS of zero will report on the header line, and first
     * line of text in the window will have VPOS of 1.
     *
     * Value is an array of resolved levels, indexed by glyph number.
     * Glyphs are numbered from zero starting from the beginning of the
     * screen line, i.e. the left edge of the window for left-to-right lines
     * and from the right edge for right-to-left lines.  The resolved levels
     * are produced only for the window's text area; text in display margins
     * is not included.
     *
     * If the selected window's display is not up-to-date, or if the specified
     * screen line does not display text, this function returns nil.  It is
     * highly recommended to bind this function to some simple key, like F8,
     * in order to avoid these problems.
     *
     * This function exists mainly for testing the correctness of the
     * Emacs UBA implementation, in particular with the test suite.
     * </pre>
     */
    @ELispBuiltIn(name = "bidi-resolved-levels", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBidiResolvedLevels extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bidiResolvedLevels(Object vpos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a string formatted according to mode-line format specification.
     * First arg FORMAT specifies the mode line format (see `mode-line-format'
     * for details) to use.
     *
     * By default, the format is evaluated for the currently selected window.
     *
     * Optional second arg FACE specifies the face property to put on all
     * characters for which no face is specified.  The value nil means the
     * default face.  The value t means whatever face the window's mode line
     * currently uses (either `mode-line' or `mode-line-inactive',
     * depending on whether the window is the selected window or not).
     * An integer value means the value string has no text
     * properties.
     *
     * Optional third and fourth args WINDOW and BUFFER specify the window
     * and buffer to use as the context for the formatting (defaults
     * are the selected window and the WINDOW's buffer).
     * </pre>
     */
    @ELispBuiltIn(name = "format-mode-line", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FFormatModeLine extends ELispBuiltInBaseNode {
        @Specialization
        public static Void formatModeLine(Object format, Object face, Object window, Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Non-nil if text properties at POS cause text there to be currently invisible.
     * POS should be a marker or a buffer position; the value of the `invisible'
     * property at that position in the current buffer is examined.
     * POS can also be the actual value of the `invisible' text or overlay
     * property of the text of interest, in which case the value itself is
     * examined.
     *
     * The non-nil value returned can be t for currently invisible text that is
     * entirely hidden on display, or some other non-nil, non-t value if the
     * text is replaced by an ellipsis.
     *
     * Note that whether text with `invisible' property is actually hidden on
     * display may depend on `buffer-invisibility-spec', which see.
     * </pre>
     */
    @ELispBuiltIn(name = "invisible-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInvisibleP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void invisibleP(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Lookup in image map MAP coordinates X and Y.
     * An image map is an alist where each element has the format (AREA ID PLIST).
     * An AREA is specified as either a rectangle, a circle, or a polygon:
     * A rectangle is a cons (rect . ((x0 . y0) . (x1 . y1))) specifying the
     * pixel coordinates of the upper left and bottom right corners.
     * A circle is a cons (circle . ((x0 . y0) . r)) specifying the center
     * and the radius of the circle; r may be a float or integer.
     * A polygon is a cons (poly . [x0 y0 x1 y1 ...]) where each pair in the
     * vector describes one corner in the polygon.
     * Returns the alist element for the first matching AREA in MAP.
     * </pre>
     */
    @ELispBuiltIn(name = "lookup-image-map", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FLookupImageMap extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lookupImageMap(Object map, Object x, Object y) {
            throw new UnsupportedOperationException();
        }
    }
}
