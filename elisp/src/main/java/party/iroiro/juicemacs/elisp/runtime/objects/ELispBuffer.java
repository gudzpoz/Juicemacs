package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import party.iroiro.juicemacs.elisp.forms.BuiltInCaseTab;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInFileIO;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage.Forwarded;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;

import static party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.getMiniBuffer;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispBuffer extends AbstractELispIdentityObject {
    private final HashMap<ELispSymbol, Forwarded> localVariables;
    private final boolean inhibitBufferHooks;
    private final PieceTreeBase content;
    private long point;

    private ELispBuffer(Object[] bufferLocalFields, PieceTreeBase content, boolean inhibitBufferHooks) {
        this.bufferLocalFields = bufferLocalFields;
        this.content = content;
        this.inhibitBufferHooks = inhibitBufferHooks;
        this.point = 1;
        this.localVariables = new HashMap<>();
    }

    public ELispBuffer(Object[] bufferLocalFields) {
        // TODO: Should not be public
        this(
                bufferLocalFields,
                new PieceTreeBase(List.of(), PieceTreeBase.EndOfLine.LF, true),
                true
        );
    }

    public ELispBuffer(ELispBuffer defaults, boolean inhibitBufferHooks) {
        this(
                Arrays.copyOf(defaults.bufferLocalFields, defaults.bufferLocalFields.length),
                new PieceTreeBase(List.of(), PieceTreeBase.EndOfLine.LF, true),
                inhibitBufferHooks
        );
    }

    /// @return 1-based index of the current editing point
    public long getPoint() {
        return point;
    }

    /// @param point 1-based index of the new editing point
    public void setPoint(long point) {
        this.point = point;
    }

    public long getChar(long point) {
        return content.getCharCode((int) point - 1);
    }

    public void insert(MuleString text) {
        if (text.length() == 0) {
            return;
        }
        content.insert((int) point - 1, text, false);
        point += text.length();
    }

    public void delete(long start, long length) {
        if (length == 0) {
            return;
        }
        content.delete((int) start - 1, (int) length);
        if (start < point && point < start + length) {
            point = start;
        } else if (point >= start + length) {
            point -= length;
        }
    }

    public ELispString bufferString() {
        return new ELispString(content.getLinesRawContent());
    }

    public int length() {
        return content.getLength();
    }

    @Nullable
    public Forwarded getLocal(ELispSymbol symbol) {
        return localVariables.get(symbol);
    }

    public Forwarded makeLocal(ELispSymbol symbol) {
        return localVariables.computeIfAbsent(symbol, _ -> new Forwarded());
    }

    public Object getSlot(int index) {
        return bufferLocalFields[index];
    }

    public void setSlot(int index, Object value) {
        bufferLocalFields[index] = value;
    }

    public boolean isLive() {
        return !isNil(getName());
    }

    public void resetLocalVariables(boolean permanentToo) {
        setMajorMode(FUNDAMENTAL_MODE);
        setKeymap(false);
        setModeName(new ELispString("Fundamental"));
        ELispContext context = ELispContext.get(null);
        BuiltInCaseTab caseTab = context.globals().builtInCaseTab;
        ELispCharTable asciiDowncaseTable = caseTab.asciiDowncaseTable;
        if (!(BuiltInData.FCharTableP.charTableP(asciiDowncaseTable.getExtra(0))
                && BuiltInData.FCharTableP.charTableP(asciiDowncaseTable.getExtra(1))
                && BuiltInData.FCharTableP.charTableP(asciiDowncaseTable.getExtra(2)))) {
            caseTab.setCaseTable(context, asciiDowncaseTable, true);
        }
        setDowncaseTable(asciiDowncaseTable);
        setUpcaseTable(asciiDowncaseTable.getExtra(0));
        setCaseCanonTable(asciiDowncaseTable.getExtra(1));
        setCaseEqvTable(asciiDowncaseTable.getExtra(2));
        setInvisibilitySpec(true);

        // TODO: Reset vars
    }

    //#region struct buffer
    private final Object[] bufferLocalFields;
    private static final byte[] BUFFER_LOCAL_FLAGS = new byte[77];
    /**
     * The name of this buffer.
     */
    public final static int BVAR_NAME = 0;
    /**
     * The last name of this buffer before it was renamed or killed.
     */
    public final static int BVAR_LAST_NAME = 1;
    /**
     * The name of the file visited in this buffer, or nil.
     */
    public final static int BVAR_FILENAME = 2;
    /**
     * Directory for expanding relative file names.
     */
    public final static int BVAR_DIRECTORY = 3;
    /**
     * True if this buffer has been backed up (if you write to the visited
     * file and it hasn't been backed up, then a backup will be made).
     */
    public final static int BVAR_BACKED_UP = 4;
    /**
     * Length of file when last read or saved.
     * -1 means auto saving turned off because buffer shrank a lot.
     * -2 means don't turn off auto saving if buffer shrinks.
     *   (That value is used with buffer-swap-text.)
     * This is not in the  struct buffer_text
     * because it's not used in indirect buffers at all.
     */
    public final static int BVAR_SAVE_LENGTH = 5;
    /**
     * File name used for auto-saving this buffer.
     * This is not in the  struct buffer_text
     * because it's not used in indirect buffers at all.
     */
    public final static int BVAR_AUTO_SAVE_FILE_NAME = 6;
    /**
     * Non-nil if buffer read-only.
     */
    public final static int BVAR_READ_ONLY = 7;
    /**
     * "The mark".  This is a marker which may
     * point into this buffer or may point nowhere.
     */
    public final static int BVAR_MARK = 8;
    /**
     * Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER) for all
     * per-buffer variables of this buffer.  For locally unbound
     * symbols, just the symbol appears as the element.
     */
    public final static int BVAR_LOCAL_VAR_ALIST = 9;
    /**
     * Symbol naming major mode (e.g., lisp-mode).
     */
    public final static int BVAR_MAJOR_MODE = 10;
    /**
     * Symbol listing all currently enabled minor modes.
     */
    public final static int BVAR_LOCAL_MINOR_MODES = 11;
    /**
     * Pretty name of major mode (e.g., "Lisp").
     */
    public final static int BVAR_MODE_NAME = 12;
    /**
     * Mode line element that controls format of mode line.
     */
    public final static int BVAR_MODE_LINE_FORMAT = 13;
    /**
     * Analogous to mode_line_format for the line displayed at the top
     * of windows.  Nil means don't display that line.
     */
    public final static int BVAR_HEADER_LINE_FORMAT = 14;
    /**
     * Analogous to mode_line_format for the line displayed at the top
     * of windows.  Nil means don't display that line.
     */
    public final static int BVAR_TAB_LINE_FORMAT = 15;
    /**
     * Keys that are bound local to this buffer.
     */
    public final static int BVAR_KEYMAP = 16;
    /**
     * This buffer's local abbrev table.
     */
    public final static int BVAR_ABBREV_TABLE = 17;
    /**
     * This buffer's syntax table.
     */
    public final static int BVAR_SYNTAX_TABLE = 18;
    /**
     * This buffer's category table.
     */
    public final static int BVAR_CATEGORY_TABLE = 19;
    /**
     * tab-width is buffer-local so that redisplay can find it
     * in buffers that are not current.
     */
    public final static int BVAR_TAB_WIDTH = 20;
    public final static int BVAR_FILL_COLUMN = 21;
    public final static int BVAR_LEFT_MARGIN = 22;
    /**
     * Function to call when insert space past fill column.
     */
    public final static int BVAR_AUTO_FILL_FUNCTION = 23;
    /**
     * Case table for case-conversion in this buffer.
     * This char-table maps each char into its lower-case version.
     */
    public final static int BVAR_DOWNCASE_TABLE = 24;
    /**
     * Char-table mapping each char to its upper-case version.
     */
    public final static int BVAR_UPCASE_TABLE = 25;
    /**
     * Char-table for conversion for case-folding search.
     */
    public final static int BVAR_CASE_CANON_TABLE = 26;
    /**
     * Char-table of equivalences for case-folding search.
     */
    public final static int BVAR_CASE_EQV_TABLE = 27;
    /**
     * Non-nil means do not display continuation lines.
     */
    public final static int BVAR_TRUNCATE_LINES = 28;
    /**
     * Non-nil means to use word wrapping when displaying continuation lines.
     */
    public final static int BVAR_WORD_WRAP = 29;
    /**
     * Non-nil means display ctl chars with uparrow.
     */
    public final static int BVAR_CTL_ARROW = 30;
    /**
     * Non-nil means reorder bidirectional text for display in the
     * visual order.
     */
    public final static int BVAR_BIDI_DISPLAY_REORDERING = 31;
    /**
     * If non-nil, specifies which direction of text to force in all the
     * paragraphs of the buffer.  Nil means determine paragraph
     * direction dynamically for each paragraph.
     */
    public final static int BVAR_BIDI_PARAGRAPH_DIRECTION = 32;
    /**
     * If non-nil, a regular expression for bidi paragraph separator.
     */
    public final static int BVAR_BIDI_PARAGRAPH_SEPARATE_RE = 33;
    /**
     * If non-nil, a regular expression for bidi paragraph start.
     */
    public final static int BVAR_BIDI_PARAGRAPH_START_RE = 34;
    /**
     * Non-nil means do selective display;
     * see doc string in syms_of_buffer (buffer.c) for details.
     */
    public final static int BVAR_SELECTIVE_DISPLAY = 35;
    /**
     * Non-nil means show ... at end of line followed by invisible lines.
     */
    public final static int BVAR_SELECTIVE_DISPLAY_ELLIPSES = 36;
    /**
     * t if "self-insertion" should overwrite; `binary' if it should also
     * overwrite newlines and tabs - for editing executables and the like.
     */
    public final static int BVAR_OVERWRITE_MODE = 37;
    /**
     * Non-nil means abbrev mode is on.  Expand abbrevs automatically.
     */
    public final static int BVAR_ABBREV_MODE = 38;
    /**
     * Display table to use for text in this buffer.
     */
    public final static int BVAR_DISPLAY_TABLE = 39;
    /**
     * t means the mark and region are currently active.
     */
    public final static int BVAR_MARK_ACTIVE = 40;
    /**
     * Non-nil means the buffer contents are regarded as multi-byte
     * form of characters, not a binary code.
     */
    public final static int BVAR_ENABLE_MULTIBYTE_CHARACTERS = 41;
    /**
     * Coding system to be used for encoding the buffer contents on
     * saving.
     */
    public final static int BVAR_BUFFER_FILE_CODING_SYSTEM = 42;
    /**
     * List of symbols naming the file format used for visited file.
     */
    public final static int BVAR_FILE_FORMAT = 43;
    /**
     * List of symbols naming the file format used for auto-save file.
     */
    public final static int BVAR_AUTO_SAVE_FILE_FORMAT = 44;
    /**
     * True if the newline position cache, width run cache and BIDI paragraph
     * cache are enabled.  See search.c, indent.c and bidi.c for details.
     */
    public final static int BVAR_CACHE_LONG_SCANS = 45;
    /**
     * If the width run cache is enabled, this table contains the
     * character widths width_run_cache (see above) assumes.  When we
     * do a thorough redisplay, we compare this against the buffer's
     * current display table to see whether the display table has
     * affected the widths of any characters.  If it has, we
     * invalidate the width run cache, and re-initialize width_table.
     */
    public final static int BVAR_WIDTH_TABLE = 46;
    /**
     * In an indirect buffer, or a buffer that is the base of an
     * indirect buffer, this holds a marker that records
     * PT for this buffer when the buffer is not current.
     */
    public final static int BVAR_PT_MARKER = 47;
    /**
     * In an indirect buffer, or a buffer that is the base of an
     * indirect buffer, this holds a marker that records
     * BEGV for this buffer when the buffer is not current.
     */
    public final static int BVAR_BEGV_MARKER = 48;
    /**
     * In an indirect buffer, or a buffer that is the base of an
     * indirect buffer, this holds a marker that records
     * ZV for this buffer when the buffer is not current.
     */
    public final static int BVAR_ZV_MARKER = 49;
    /**
     * This holds the point value before the last scroll operation.
     * Explicitly setting point sets this to nil.
     */
    public final static int BVAR_POINT_BEFORE_SCROLL = 50;
    /**
     * Truename of the visited file, or nil.
     */
    public final static int BVAR_FILE_TRUENAME = 51;
    /**
     * Invisibility spec of this buffer.
     * t =&gt; any non-nil `invisible' property means invisible.
     * A list =&gt; `invisible' property means invisible
     * if it is memq in that list.
     */
    public final static int BVAR_INVISIBILITY_SPEC = 52;
    /**
     * This is the last window that was selected with this buffer in it,
     * or nil if that window no longer displays this buffer.
     */
    public final static int BVAR_LAST_SELECTED_WINDOW = 53;
    /**
     * Incremented each time the buffer is displayed in a window.
     */
    public final static int BVAR_DISPLAY_COUNT = 54;
    /**
     * Widths of left and right marginal areas for windows displaying
     * this buffer.
     */
    public final static int BVAR_LEFT_MARGIN_COLS = 55;
    public final static int BVAR_RIGHT_MARGIN_COLS = 56;
    /**
     * Widths of left and right fringe areas for windows displaying
     * this buffer.
     */
    public final static int BVAR_LEFT_FRINGE_WIDTH = 57;
    public final static int BVAR_RIGHT_FRINGE_WIDTH = 58;
    /**
     * Non-nil means fringes are drawn outside display margins;
     * othersize draw them between margin areas and text.
     */
    public final static int BVAR_FRINGES_OUTSIDE_MARGINS = 59;
    /**
     * Width, height and types of scroll bar areas for windows displaying
     * this buffer.
     */
    public final static int BVAR_SCROLL_BAR_WIDTH = 60;
    public final static int BVAR_SCROLL_BAR_HEIGHT = 61;
    public final static int BVAR_VERTICAL_SCROLL_BAR_TYPE = 62;
    public final static int BVAR_HORIZONTAL_SCROLL_BAR_TYPE = 63;
    /**
     * Non-nil means indicate lines not displaying text (in a style
     * like vi).
     */
    public final static int BVAR_INDICATE_EMPTY_LINES = 64;
    /**
     * Non-nil means indicate buffer boundaries and scrolling.
     */
    public final static int BVAR_INDICATE_BUFFER_BOUNDARIES = 65;
    /**
     * Logical to physical fringe bitmap mappings.
     */
    public final static int BVAR_FRINGE_INDICATOR_ALIST = 66;
    /**
     * Logical to physical cursor bitmap mappings.
     */
    public final static int BVAR_FRINGE_CURSOR_ALIST = 67;
    /**
     * Time stamp updated each time this buffer is displayed in a window.
     */
    public final static int BVAR_DISPLAY_TIME = 68;
    /**
     * If scrolling the display because point is below the bottom of a
     * window showing this buffer, try to choose a window start so
     * that point ends up this number of lines from the top of the
     * window.  Nil means that scrolling method isn't used.
     */
    public final static int BVAR_SCROLL_UP_AGGRESSIVELY = 69;
    /**
     * If scrolling the display because point is above the top of a
     * window showing this buffer, try to choose a window start so
     * that point ends up this number of lines from the bottom of the
     * window.  Nil means that scrolling method isn't used.
     */
    public final static int BVAR_SCROLL_DOWN_AGGRESSIVELY = 70;
    /**
     * Desired cursor type in this buffer.  See the doc string of
     * per-buffer variable `cursor-type'.
     */
    public final static int BVAR_CURSOR_TYPE = 71;
    /**
     * An integer &gt; 0 means put that number of pixels below text lines
     * in the display of this buffer.
     */
    public final static int BVAR_EXTRA_LINE_SPACING = 72;
    /**
     * A list of tree-sitter parsers for this buffer.
     */
    public final static int BVAR_TS_PARSER_LIST = 73;
    /**
     * What type of text conversion the input method should apply to
     * this buffer.
     */
    public final static int BVAR_TEXT_CONVERSION_STYLE = 74;
    /**
     * Cursor type to display in non-selected windows.
     * t means to use hollow box cursor.
     * See `cursor-type' for other values.
     */
    public final static int BVAR_CURSOR_IN_NON_SELECTED_WINDOWS = 75;
    /**
     * Changes in the buffer are recorded here for undo, and t means
     * don't record anything.  This information belongs to the base
     * buffer of an indirect buffer.  But we can't store it in the
     * struct buffer_text because local variables have to be right in
     * the struct buffer. So we copy it around in set_buffer_internal.
     */
    public final static int BVAR_UNDO_LIST = 76;
    public Object getName() { return bufferLocalFields[BVAR_NAME]; }
    public void setName(Object value) { bufferLocalFields[BVAR_NAME] = value; }
    public Object getLastName() { return bufferLocalFields[BVAR_LAST_NAME]; }
    public void setLastName(Object value) { bufferLocalFields[BVAR_LAST_NAME] = value; }
    public Object getFilename() { return bufferLocalFields[BVAR_FILENAME]; }
    public void setFilename(Object value) { bufferLocalFields[BVAR_FILENAME] = value; }
    public Object getDirectory() { return bufferLocalFields[BVAR_DIRECTORY]; }
    public void setDirectory(Object value) { bufferLocalFields[BVAR_DIRECTORY] = value; }
    public Object getBackedUp() { return bufferLocalFields[BVAR_BACKED_UP]; }
    public void setBackedUp(Object value) { bufferLocalFields[BVAR_BACKED_UP] = value; }
    public Object getSaveLength() { return bufferLocalFields[BVAR_SAVE_LENGTH]; }
    public void setSaveLength(Object value) { bufferLocalFields[BVAR_SAVE_LENGTH] = value; }
    public Object getAutoSaveFileName() { return bufferLocalFields[BVAR_AUTO_SAVE_FILE_NAME]; }
    public void setAutoSaveFileName(Object value) { bufferLocalFields[BVAR_AUTO_SAVE_FILE_NAME] = value; }
    public Object getReadOnly() { return bufferLocalFields[BVAR_READ_ONLY]; }
    public void setReadOnly(Object value) { bufferLocalFields[BVAR_READ_ONLY] = value; }
    public Object getMark() { return bufferLocalFields[BVAR_MARK]; }
    public void setMark(Object value) { bufferLocalFields[BVAR_MARK] = value; }
    public Object getLocalVarAlist() { return bufferLocalFields[BVAR_LOCAL_VAR_ALIST]; }
    public void setLocalVarAlist(Object value) { bufferLocalFields[BVAR_LOCAL_VAR_ALIST] = value; }
    public Object getMajorMode() { return bufferLocalFields[BVAR_MAJOR_MODE]; }
    public void setMajorMode(Object value) { bufferLocalFields[BVAR_MAJOR_MODE] = value; }
    public Object getLocalMinorModes() { return bufferLocalFields[BVAR_LOCAL_MINOR_MODES]; }
    public void setLocalMinorModes(Object value) { bufferLocalFields[BVAR_LOCAL_MINOR_MODES] = value; }
    public Object getModeName() { return bufferLocalFields[BVAR_MODE_NAME]; }
    public void setModeName(Object value) { bufferLocalFields[BVAR_MODE_NAME] = value; }
    public Object getModeLineFormat() { return bufferLocalFields[BVAR_MODE_LINE_FORMAT]; }
    public void setModeLineFormat(Object value) { bufferLocalFields[BVAR_MODE_LINE_FORMAT] = value; }
    public Object getHeaderLineFormat() { return bufferLocalFields[BVAR_HEADER_LINE_FORMAT]; }
    public void setHeaderLineFormat(Object value) { bufferLocalFields[BVAR_HEADER_LINE_FORMAT] = value; }
    public Object getTabLineFormat() { return bufferLocalFields[BVAR_TAB_LINE_FORMAT]; }
    public void setTabLineFormat(Object value) { bufferLocalFields[BVAR_TAB_LINE_FORMAT] = value; }
    public Object getKeymap() { return bufferLocalFields[BVAR_KEYMAP]; }
    public void setKeymap(Object value) { bufferLocalFields[BVAR_KEYMAP] = value; }
    public Object getAbbrevTable() { return bufferLocalFields[BVAR_ABBREV_TABLE]; }
    public void setAbbrevTable(Object value) { bufferLocalFields[BVAR_ABBREV_TABLE] = value; }
    public Object getSyntaxTable() { return bufferLocalFields[BVAR_SYNTAX_TABLE]; }
    public void setSyntaxTable(Object value) { bufferLocalFields[BVAR_SYNTAX_TABLE] = value; }
    public Object getCategoryTable() { return bufferLocalFields[BVAR_CATEGORY_TABLE]; }
    public void setCategoryTable(Object value) { bufferLocalFields[BVAR_CATEGORY_TABLE] = value; }
    public Object getTabWidth() { return bufferLocalFields[BVAR_TAB_WIDTH]; }
    public void setTabWidth(Object value) { bufferLocalFields[BVAR_TAB_WIDTH] = value; }
    public Object getFillColumn() { return bufferLocalFields[BVAR_FILL_COLUMN]; }
    public void setFillColumn(Object value) { bufferLocalFields[BVAR_FILL_COLUMN] = value; }
    public Object getLeftMargin() { return bufferLocalFields[BVAR_LEFT_MARGIN]; }
    public void setLeftMargin(Object value) { bufferLocalFields[BVAR_LEFT_MARGIN] = value; }
    public Object getAutoFillFunction() { return bufferLocalFields[BVAR_AUTO_FILL_FUNCTION]; }
    public void setAutoFillFunction(Object value) { bufferLocalFields[BVAR_AUTO_FILL_FUNCTION] = value; }
    public Object getDowncaseTable() { return bufferLocalFields[BVAR_DOWNCASE_TABLE]; }
    public void setDowncaseTable(Object value) { bufferLocalFields[BVAR_DOWNCASE_TABLE] = value; }
    public Object getUpcaseTable() { return bufferLocalFields[BVAR_UPCASE_TABLE]; }
    public void setUpcaseTable(Object value) { bufferLocalFields[BVAR_UPCASE_TABLE] = value; }
    public Object getCaseCanonTable() { return bufferLocalFields[BVAR_CASE_CANON_TABLE]; }
    public void setCaseCanonTable(Object value) { bufferLocalFields[BVAR_CASE_CANON_TABLE] = value; }
    public Object getCaseEqvTable() { return bufferLocalFields[BVAR_CASE_EQV_TABLE]; }
    public void setCaseEqvTable(Object value) { bufferLocalFields[BVAR_CASE_EQV_TABLE] = value; }
    public Object getTruncateLines() { return bufferLocalFields[BVAR_TRUNCATE_LINES]; }
    public void setTruncateLines(Object value) { bufferLocalFields[BVAR_TRUNCATE_LINES] = value; }
    public Object getWordWrap() { return bufferLocalFields[BVAR_WORD_WRAP]; }
    public void setWordWrap(Object value) { bufferLocalFields[BVAR_WORD_WRAP] = value; }
    public Object getCtlArrow() { return bufferLocalFields[BVAR_CTL_ARROW]; }
    public void setCtlArrow(Object value) { bufferLocalFields[BVAR_CTL_ARROW] = value; }
    public Object getBidiDisplayReordering() { return bufferLocalFields[BVAR_BIDI_DISPLAY_REORDERING]; }
    public void setBidiDisplayReordering(Object value) { bufferLocalFields[BVAR_BIDI_DISPLAY_REORDERING] = value; }
    public Object getBidiParagraphDirection() { return bufferLocalFields[BVAR_BIDI_PARAGRAPH_DIRECTION]; }
    public void setBidiParagraphDirection(Object value) { bufferLocalFields[BVAR_BIDI_PARAGRAPH_DIRECTION] = value; }
    public Object getBidiParagraphSeparateRe() { return bufferLocalFields[BVAR_BIDI_PARAGRAPH_SEPARATE_RE]; }
    public void setBidiParagraphSeparateRe(Object value) { bufferLocalFields[BVAR_BIDI_PARAGRAPH_SEPARATE_RE] = value; }
    public Object getBidiParagraphStartRe() { return bufferLocalFields[BVAR_BIDI_PARAGRAPH_START_RE]; }
    public void setBidiParagraphStartRe(Object value) { bufferLocalFields[BVAR_BIDI_PARAGRAPH_START_RE] = value; }
    public Object getSelectiveDisplay() { return bufferLocalFields[BVAR_SELECTIVE_DISPLAY]; }
    public void setSelectiveDisplay(Object value) { bufferLocalFields[BVAR_SELECTIVE_DISPLAY] = value; }
    public Object getSelectiveDisplayEllipses() { return bufferLocalFields[BVAR_SELECTIVE_DISPLAY_ELLIPSES]; }
    public void setSelectiveDisplayEllipses(Object value) { bufferLocalFields[BVAR_SELECTIVE_DISPLAY_ELLIPSES] = value; }
    public Object getOverwriteMode() { return bufferLocalFields[BVAR_OVERWRITE_MODE]; }
    public void setOverwriteMode(Object value) { bufferLocalFields[BVAR_OVERWRITE_MODE] = value; }
    public Object getAbbrevMode() { return bufferLocalFields[BVAR_ABBREV_MODE]; }
    public void setAbbrevMode(Object value) { bufferLocalFields[BVAR_ABBREV_MODE] = value; }
    public Object getDisplayTable() { return bufferLocalFields[BVAR_DISPLAY_TABLE]; }
    public void setDisplayTable(Object value) { bufferLocalFields[BVAR_DISPLAY_TABLE] = value; }
    public Object getMarkActive() { return bufferLocalFields[BVAR_MARK_ACTIVE]; }
    public void setMarkActive(Object value) { bufferLocalFields[BVAR_MARK_ACTIVE] = value; }
    public Object getEnableMultibyteCharacters() { return bufferLocalFields[BVAR_ENABLE_MULTIBYTE_CHARACTERS]; }
    public void setEnableMultibyteCharacters(Object value) { bufferLocalFields[BVAR_ENABLE_MULTIBYTE_CHARACTERS] = value; }
    public Object getBufferFileCodingSystem() { return bufferLocalFields[BVAR_BUFFER_FILE_CODING_SYSTEM]; }
    public void setBufferFileCodingSystem(Object value) { bufferLocalFields[BVAR_BUFFER_FILE_CODING_SYSTEM] = value; }
    public Object getFileFormat() { return bufferLocalFields[BVAR_FILE_FORMAT]; }
    public void setFileFormat(Object value) { bufferLocalFields[BVAR_FILE_FORMAT] = value; }
    public Object getAutoSaveFileFormat() { return bufferLocalFields[BVAR_AUTO_SAVE_FILE_FORMAT]; }
    public void setAutoSaveFileFormat(Object value) { bufferLocalFields[BVAR_AUTO_SAVE_FILE_FORMAT] = value; }
    public Object getCacheLongScans() { return bufferLocalFields[BVAR_CACHE_LONG_SCANS]; }
    public void setCacheLongScans(Object value) { bufferLocalFields[BVAR_CACHE_LONG_SCANS] = value; }
    public Object getWidthTable() { return bufferLocalFields[BVAR_WIDTH_TABLE]; }
    public void setWidthTable(Object value) { bufferLocalFields[BVAR_WIDTH_TABLE] = value; }
    public Object getPtMarker() { return bufferLocalFields[BVAR_PT_MARKER]; }
    public void setPtMarker(Object value) { bufferLocalFields[BVAR_PT_MARKER] = value; }
    public Object getBegvMarker() { return bufferLocalFields[BVAR_BEGV_MARKER]; }
    public void setBegvMarker(Object value) { bufferLocalFields[BVAR_BEGV_MARKER] = value; }
    public Object getZvMarker() { return bufferLocalFields[BVAR_ZV_MARKER]; }
    public void setZvMarker(Object value) { bufferLocalFields[BVAR_ZV_MARKER] = value; }
    public Object getPointBeforeScroll() { return bufferLocalFields[BVAR_POINT_BEFORE_SCROLL]; }
    public void setPointBeforeScroll(Object value) { bufferLocalFields[BVAR_POINT_BEFORE_SCROLL] = value; }
    public Object getFileTruename() { return bufferLocalFields[BVAR_FILE_TRUENAME]; }
    public void setFileTruename(Object value) { bufferLocalFields[BVAR_FILE_TRUENAME] = value; }
    public Object getInvisibilitySpec() { return bufferLocalFields[BVAR_INVISIBILITY_SPEC]; }
    public void setInvisibilitySpec(Object value) { bufferLocalFields[BVAR_INVISIBILITY_SPEC] = value; }
    public Object getLastSelectedWindow() { return bufferLocalFields[BVAR_LAST_SELECTED_WINDOW]; }
    public void setLastSelectedWindow(Object value) { bufferLocalFields[BVAR_LAST_SELECTED_WINDOW] = value; }
    public Object getDisplayCount() { return bufferLocalFields[BVAR_DISPLAY_COUNT]; }
    public void setDisplayCount(Object value) { bufferLocalFields[BVAR_DISPLAY_COUNT] = value; }
    public Object getLeftMarginCols() { return bufferLocalFields[BVAR_LEFT_MARGIN_COLS]; }
    public void setLeftMarginCols(Object value) { bufferLocalFields[BVAR_LEFT_MARGIN_COLS] = value; }
    public Object getRightMarginCols() { return bufferLocalFields[BVAR_RIGHT_MARGIN_COLS]; }
    public void setRightMarginCols(Object value) { bufferLocalFields[BVAR_RIGHT_MARGIN_COLS] = value; }
    public Object getLeftFringeWidth() { return bufferLocalFields[BVAR_LEFT_FRINGE_WIDTH]; }
    public void setLeftFringeWidth(Object value) { bufferLocalFields[BVAR_LEFT_FRINGE_WIDTH] = value; }
    public Object getRightFringeWidth() { return bufferLocalFields[BVAR_RIGHT_FRINGE_WIDTH]; }
    public void setRightFringeWidth(Object value) { bufferLocalFields[BVAR_RIGHT_FRINGE_WIDTH] = value; }
    public Object getFringesOutsideMargins() { return bufferLocalFields[BVAR_FRINGES_OUTSIDE_MARGINS]; }
    public void setFringesOutsideMargins(Object value) { bufferLocalFields[BVAR_FRINGES_OUTSIDE_MARGINS] = value; }
    public Object getScrollBarWidth() { return bufferLocalFields[BVAR_SCROLL_BAR_WIDTH]; }
    public void setScrollBarWidth(Object value) { bufferLocalFields[BVAR_SCROLL_BAR_WIDTH] = value; }
    public Object getScrollBarHeight() { return bufferLocalFields[BVAR_SCROLL_BAR_HEIGHT]; }
    public void setScrollBarHeight(Object value) { bufferLocalFields[BVAR_SCROLL_BAR_HEIGHT] = value; }
    public Object getVerticalScrollBarType() { return bufferLocalFields[BVAR_VERTICAL_SCROLL_BAR_TYPE]; }
    public void setVerticalScrollBarType(Object value) { bufferLocalFields[BVAR_VERTICAL_SCROLL_BAR_TYPE] = value; }
    public Object getHorizontalScrollBarType() { return bufferLocalFields[BVAR_HORIZONTAL_SCROLL_BAR_TYPE]; }
    public void setHorizontalScrollBarType(Object value) { bufferLocalFields[BVAR_HORIZONTAL_SCROLL_BAR_TYPE] = value; }
    public Object getIndicateEmptyLines() { return bufferLocalFields[BVAR_INDICATE_EMPTY_LINES]; }
    public void setIndicateEmptyLines(Object value) { bufferLocalFields[BVAR_INDICATE_EMPTY_LINES] = value; }
    public Object getIndicateBufferBoundaries() { return bufferLocalFields[BVAR_INDICATE_BUFFER_BOUNDARIES]; }
    public void setIndicateBufferBoundaries(Object value) { bufferLocalFields[BVAR_INDICATE_BUFFER_BOUNDARIES] = value; }
    public Object getFringeIndicatorAlist() { return bufferLocalFields[BVAR_FRINGE_INDICATOR_ALIST]; }
    public void setFringeIndicatorAlist(Object value) { bufferLocalFields[BVAR_FRINGE_INDICATOR_ALIST] = value; }
    public Object getFringeCursorAlist() { return bufferLocalFields[BVAR_FRINGE_CURSOR_ALIST]; }
    public void setFringeCursorAlist(Object value) { bufferLocalFields[BVAR_FRINGE_CURSOR_ALIST] = value; }
    public Object getDisplayTime() { return bufferLocalFields[BVAR_DISPLAY_TIME]; }
    public void setDisplayTime(Object value) { bufferLocalFields[BVAR_DISPLAY_TIME] = value; }
    public Object getScrollUpAggressively() { return bufferLocalFields[BVAR_SCROLL_UP_AGGRESSIVELY]; }
    public void setScrollUpAggressively(Object value) { bufferLocalFields[BVAR_SCROLL_UP_AGGRESSIVELY] = value; }
    public Object getScrollDownAggressively() { return bufferLocalFields[BVAR_SCROLL_DOWN_AGGRESSIVELY]; }
    public void setScrollDownAggressively(Object value) { bufferLocalFields[BVAR_SCROLL_DOWN_AGGRESSIVELY] = value; }
    public Object getCursorType() { return bufferLocalFields[BVAR_CURSOR_TYPE]; }
    public void setCursorType(Object value) { bufferLocalFields[BVAR_CURSOR_TYPE] = value; }
    public Object getExtraLineSpacing() { return bufferLocalFields[BVAR_EXTRA_LINE_SPACING]; }
    public void setExtraLineSpacing(Object value) { bufferLocalFields[BVAR_EXTRA_LINE_SPACING] = value; }
    public Object getTsParserList() { return bufferLocalFields[BVAR_TS_PARSER_LIST]; }
    public void setTsParserList(Object value) { bufferLocalFields[BVAR_TS_PARSER_LIST] = value; }
    public Object getTextConversionStyle() { return bufferLocalFields[BVAR_TEXT_CONVERSION_STYLE]; }
    public void setTextConversionStyle(Object value) { bufferLocalFields[BVAR_TEXT_CONVERSION_STYLE] = value; }
    public Object getCursorInNonSelectedWindows() { return bufferLocalFields[BVAR_CURSOR_IN_NON_SELECTED_WINDOWS]; }
    public void setCursorInNonSelectedWindows(Object value) { bufferLocalFields[BVAR_CURSOR_IN_NON_SELECTED_WINDOWS] = value; }
    public Object getUndoList() { return bufferLocalFields[BVAR_UNDO_LIST]; }
    public void setUndoList(Object value) { bufferLocalFields[BVAR_UNDO_LIST] = value; }
    //#endregion struct buffer

    @SuppressWarnings("DuplicatedCode")
    public static void initBufferLocalVars(ELispContext context, ELispBuffer defaultValues) {
        //#region init_buffer_once
        defaultValues.setName(new ELispString(" *buffer-defaults*"));
        defaultValues.setMajorMode(FUNDAMENTAL_MODE);
        defaultValues.setModeLineFormat(new ELispString("%-"));
        defaultValues.setTabWidth((long) (8));
        defaultValues.setFillColumn((long) (70));
        defaultValues.setLeftMargin((long) (0));
        defaultValues.setCtlArrow(T);
        defaultValues.setBidiDisplayReordering(T);
        defaultValues.setSelectiveDisplayEllipses(T);
        defaultValues.setEnableMultibyteCharacters(T);
        defaultValues.setAutoSaveFileFormat(T);
        defaultValues.setCacheLongScans(T);
        defaultValues.setDisplayCount((long) (0));
        defaultValues.setLeftMarginCols((long) (0));
        defaultValues.setRightMarginCols((long) (0));
        defaultValues.setVerticalScrollBarType(T);
        defaultValues.setHorizontalScrollBarType(T);
        defaultValues.setCursorType(T);
        defaultValues.setCursorInNonSelectedWindows(T);
        BUFFER_LOCAL_FLAGS[BVAR_NAME] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_LAST_NAME] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_FILENAME] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_DIRECTORY] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_BACKED_UP] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_SAVE_LENGTH] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_AUTO_SAVE_FILE_NAME] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_READ_ONLY] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_MARK] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_LOCAL_VAR_ALIST] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_MAJOR_MODE] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_LOCAL_MINOR_MODES] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_MODE_NAME] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_MODE_LINE_FORMAT] = 1;
        BUFFER_LOCAL_FLAGS[BVAR_HEADER_LINE_FORMAT] = 38;
        BUFFER_LOCAL_FLAGS[BVAR_TAB_LINE_FORMAT] = 39;
        BUFFER_LOCAL_FLAGS[BVAR_KEYMAP] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_ABBREV_TABLE] = 13;
        BUFFER_LOCAL_FLAGS[BVAR_SYNTAX_TABLE] = 15;
        BUFFER_LOCAL_FLAGS[BVAR_CATEGORY_TABLE] = 17;
        BUFFER_LOCAL_FLAGS[BVAR_TAB_WIDTH] = 7;
        BUFFER_LOCAL_FLAGS[BVAR_FILL_COLUMN] = 11;
        BUFFER_LOCAL_FLAGS[BVAR_LEFT_MARGIN] = 12;
        BUFFER_LOCAL_FLAGS[BVAR_AUTO_FILL_FUNCTION] = 4;
        BUFFER_LOCAL_FLAGS[BVAR_DOWNCASE_TABLE] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_UPCASE_TABLE] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_CASE_CANON_TABLE] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_CASE_EQV_TABLE] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_TRUNCATE_LINES] = Byte.MIN_VALUE; // PERMANENT_LOCAL
        BUFFER_LOCAL_FLAGS[BVAR_WORD_WRAP] = 9;
        BUFFER_LOCAL_FLAGS[BVAR_CTL_ARROW] = 10;
        BUFFER_LOCAL_FLAGS[BVAR_BIDI_DISPLAY_REORDERING] = 18;
        BUFFER_LOCAL_FLAGS[BVAR_BIDI_PARAGRAPH_DIRECTION] = 19;
        BUFFER_LOCAL_FLAGS[BVAR_BIDI_PARAGRAPH_SEPARATE_RE] = 20;
        BUFFER_LOCAL_FLAGS[BVAR_BIDI_PARAGRAPH_START_RE] = 21;
        BUFFER_LOCAL_FLAGS[BVAR_SELECTIVE_DISPLAY] = 5;
        BUFFER_LOCAL_FLAGS[BVAR_SELECTIVE_DISPLAY_ELLIPSES] = 6;
        BUFFER_LOCAL_FLAGS[BVAR_OVERWRITE_MODE] = 3;
        BUFFER_LOCAL_FLAGS[BVAR_ABBREV_MODE] = 2;
        BUFFER_LOCAL_FLAGS[BVAR_DISPLAY_TABLE] = 14;
        BUFFER_LOCAL_FLAGS[BVAR_MARK_ACTIVE] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_ENABLE_MULTIBYTE_CHARACTERS] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_BUFFER_FILE_CODING_SYSTEM] = Byte.MIN_VALUE; // PERMANENT_LOCAL
        BUFFER_LOCAL_FLAGS[BVAR_FILE_FORMAT] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_AUTO_SAVE_FILE_FORMAT] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_CACHE_LONG_SCANS] = 16;
        BUFFER_LOCAL_FLAGS[BVAR_WIDTH_TABLE] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_PT_MARKER] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_BEGV_MARKER] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_ZV_MARKER] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_POINT_BEFORE_SCROLL] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_FILE_TRUENAME] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_INVISIBILITY_SPEC] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_LAST_SELECTED_WINDOW] = 0;
        BUFFER_LOCAL_FLAGS[BVAR_DISPLAY_COUNT] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_LEFT_MARGIN_COLS] = 23;
        BUFFER_LOCAL_FLAGS[BVAR_RIGHT_MARGIN_COLS] = 24;
        BUFFER_LOCAL_FLAGS[BVAR_LEFT_FRINGE_WIDTH] = 25;
        BUFFER_LOCAL_FLAGS[BVAR_RIGHT_FRINGE_WIDTH] = 26;
        BUFFER_LOCAL_FLAGS[BVAR_FRINGES_OUTSIDE_MARGINS] = 27;
        BUFFER_LOCAL_FLAGS[BVAR_SCROLL_BAR_WIDTH] = 28;
        BUFFER_LOCAL_FLAGS[BVAR_SCROLL_BAR_HEIGHT] = 29;
        BUFFER_LOCAL_FLAGS[BVAR_VERTICAL_SCROLL_BAR_TYPE] = 30;
        BUFFER_LOCAL_FLAGS[BVAR_HORIZONTAL_SCROLL_BAR_TYPE] = 31;
        BUFFER_LOCAL_FLAGS[BVAR_INDICATE_EMPTY_LINES] = 32;
        BUFFER_LOCAL_FLAGS[BVAR_INDICATE_BUFFER_BOUNDARIES] = 33;
        BUFFER_LOCAL_FLAGS[BVAR_FRINGE_INDICATOR_ALIST] = 34;
        BUFFER_LOCAL_FLAGS[BVAR_FRINGE_CURSOR_ALIST] = 35;
        BUFFER_LOCAL_FLAGS[BVAR_DISPLAY_TIME] = -1;
        BUFFER_LOCAL_FLAGS[BVAR_SCROLL_UP_AGGRESSIVELY] = 36;
        BUFFER_LOCAL_FLAGS[BVAR_SCROLL_DOWN_AGGRESSIVELY] = 37;
        BUFFER_LOCAL_FLAGS[BVAR_CURSOR_TYPE] = 40;
        BUFFER_LOCAL_FLAGS[BVAR_EXTRA_LINE_SPACING] = 41;
        BUFFER_LOCAL_FLAGS[BVAR_TS_PARSER_LIST] = 42;
        BUFFER_LOCAL_FLAGS[BVAR_TEXT_CONVERSION_STYLE] = 43;
        BUFFER_LOCAL_FLAGS[BVAR_CURSOR_IN_NON_SELECTED_WINDOWS] = 44;
        BUFFER_LOCAL_FLAGS[BVAR_UNDO_LIST] = -1;
        context.forwardTo(BUFFER_FILE_NAME, new ValueStorage.ForwardedPerBuffer(BVAR_FILENAME, STRINGP));
        context.forwardTo(DEFAULT_DIRECTORY, new ValueStorage.ForwardedPerBuffer(BVAR_DIRECTORY, STRINGP));
        context.forwardTo(BUFFER_BACKED_UP, new ValueStorage.ForwardedPerBuffer(BVAR_BACKED_UP, NIL));
        context.forwardTo(BUFFER_SAVED_SIZE, new ValueStorage.ForwardedPerBuffer(BVAR_SAVE_LENGTH, INTEGERP));
        context.forwardTo(BUFFER_AUTO_SAVE_FILE_NAME, new ValueStorage.ForwardedPerBuffer(BVAR_AUTO_SAVE_FILE_NAME, STRINGP));
        context.forwardTo(BUFFER_READ_ONLY, new ValueStorage.ForwardedPerBuffer(BVAR_READ_ONLY, NIL));
        context.forwardTo(MAJOR_MODE, new ValueStorage.ForwardedPerBuffer(BVAR_MAJOR_MODE, SYMBOLP));
        context.forwardTo(LOCAL_MINOR_MODES, new ValueStorage.ForwardedPerBuffer(BVAR_LOCAL_MINOR_MODES, NIL));
        context.forwardTo(MODE_NAME, new ValueStorage.ForwardedPerBuffer(BVAR_MODE_NAME, NIL));
        context.forwardTo(MODE_LINE_FORMAT, new ValueStorage.ForwardedPerBuffer(BVAR_MODE_LINE_FORMAT, NIL));
        context.forwardTo(HEADER_LINE_FORMAT, new ValueStorage.ForwardedPerBuffer(BVAR_HEADER_LINE_FORMAT, NIL));
        context.forwardTo(TAB_LINE_FORMAT, new ValueStorage.ForwardedPerBuffer(BVAR_TAB_LINE_FORMAT, NIL));
        context.forwardTo(LOCAL_ABBREV_TABLE, new ValueStorage.ForwardedPerBuffer(BVAR_ABBREV_TABLE, NIL));
        context.forwardTo(TAB_WIDTH, new ValueStorage.ForwardedPerBuffer(BVAR_TAB_WIDTH, INTEGERP));
        context.forwardTo(FILL_COLUMN, new ValueStorage.ForwardedPerBuffer(BVAR_FILL_COLUMN, INTEGERP));
        context.forwardTo(LEFT_MARGIN, new ValueStorage.ForwardedPerBuffer(BVAR_LEFT_MARGIN, INTEGERP));
        context.forwardTo(AUTO_FILL_FUNCTION, new ValueStorage.ForwardedPerBuffer(BVAR_AUTO_FILL_FUNCTION, NIL));
        context.forwardTo(TRUNCATE_LINES, new ValueStorage.ForwardedPerBuffer(BVAR_TRUNCATE_LINES, NIL));
        context.forwardTo(WORD_WRAP, new ValueStorage.ForwardedPerBuffer(BVAR_WORD_WRAP, NIL));
        context.forwardTo(CTL_ARROW, new ValueStorage.ForwardedPerBuffer(BVAR_CTL_ARROW, NIL));
        context.forwardTo(BIDI_DISPLAY_REORDERING, new ValueStorage.ForwardedPerBuffer(BVAR_BIDI_DISPLAY_REORDERING, NIL));
        context.forwardTo(BIDI_PARAGRAPH_DIRECTION, new ValueStorage.ForwardedPerBuffer(BVAR_BIDI_PARAGRAPH_DIRECTION, NIL));
        context.forwardTo(BIDI_PARAGRAPH_SEPARATE_RE, new ValueStorage.ForwardedPerBuffer(BVAR_BIDI_PARAGRAPH_SEPARATE_RE, NIL));
        context.forwardTo(BIDI_PARAGRAPH_START_RE, new ValueStorage.ForwardedPerBuffer(BVAR_BIDI_PARAGRAPH_START_RE, NIL));
        context.forwardTo(SELECTIVE_DISPLAY, new ValueStorage.ForwardedPerBuffer(BVAR_SELECTIVE_DISPLAY, NIL));
        context.forwardTo(SELECTIVE_DISPLAY_ELLIPSES, new ValueStorage.ForwardedPerBuffer(BVAR_SELECTIVE_DISPLAY_ELLIPSES, NIL));
        context.forwardTo(OVERWRITE_MODE, new ValueStorage.ForwardedPerBuffer(BVAR_OVERWRITE_MODE, OVERWRITE_MODE));
        context.forwardTo(ABBREV_MODE, new ValueStorage.ForwardedPerBuffer(BVAR_ABBREV_MODE, NIL));
        context.forwardTo(BUFFER_DISPLAY_TABLE, new ValueStorage.ForwardedPerBuffer(BVAR_DISPLAY_TABLE, NIL));
        context.forwardTo(MARK_ACTIVE, new ValueStorage.ForwardedPerBuffer(BVAR_MARK_ACTIVE, NIL));
        context.forwardTo(ENABLE_MULTIBYTE_CHARACTERS, new ValueStorage.ForwardedPerBuffer(BVAR_ENABLE_MULTIBYTE_CHARACTERS, NIL));
        context.forwardTo(BUFFER_FILE_CODING_SYSTEM, new ValueStorage.ForwardedPerBuffer(BVAR_BUFFER_FILE_CODING_SYSTEM, NIL));
        context.forwardTo(BUFFER_FILE_FORMAT, new ValueStorage.ForwardedPerBuffer(BVAR_FILE_FORMAT, NIL));
        context.forwardTo(BUFFER_AUTO_SAVE_FILE_FORMAT, new ValueStorage.ForwardedPerBuffer(BVAR_AUTO_SAVE_FILE_FORMAT, NIL));
        context.forwardTo(CACHE_LONG_SCANS, new ValueStorage.ForwardedPerBuffer(BVAR_CACHE_LONG_SCANS, NIL));
        context.forwardTo(POINT_BEFORE_SCROLL, new ValueStorage.ForwardedPerBuffer(BVAR_POINT_BEFORE_SCROLL, NIL));
        context.forwardTo(BUFFER_FILE_TRUENAME, new ValueStorage.ForwardedPerBuffer(BVAR_FILE_TRUENAME, STRINGP));
        context.forwardTo(BUFFER_INVISIBILITY_SPEC, new ValueStorage.ForwardedPerBuffer(BVAR_INVISIBILITY_SPEC, NIL));
        context.forwardTo(BUFFER_DISPLAY_COUNT, new ValueStorage.ForwardedPerBuffer(BVAR_DISPLAY_COUNT, INTEGERP));
        context.forwardTo(LEFT_MARGIN_WIDTH, new ValueStorage.ForwardedPerBuffer(BVAR_LEFT_MARGIN_COLS, INTEGERP));
        context.forwardTo(RIGHT_MARGIN_WIDTH, new ValueStorage.ForwardedPerBuffer(BVAR_RIGHT_MARGIN_COLS, INTEGERP));
        context.forwardTo(LEFT_FRINGE_WIDTH, new ValueStorage.ForwardedPerBuffer(BVAR_LEFT_FRINGE_WIDTH, INTEGERP));
        context.forwardTo(RIGHT_FRINGE_WIDTH, new ValueStorage.ForwardedPerBuffer(BVAR_RIGHT_FRINGE_WIDTH, INTEGERP));
        context.forwardTo(FRINGES_OUTSIDE_MARGINS, new ValueStorage.ForwardedPerBuffer(BVAR_FRINGES_OUTSIDE_MARGINS, NIL));
        context.forwardTo(SCROLL_BAR_WIDTH, new ValueStorage.ForwardedPerBuffer(BVAR_SCROLL_BAR_WIDTH, INTEGERP));
        context.forwardTo(SCROLL_BAR_HEIGHT, new ValueStorage.ForwardedPerBuffer(BVAR_SCROLL_BAR_HEIGHT, INTEGERP));
        context.forwardTo(VERTICAL_SCROLL_BAR, new ValueStorage.ForwardedPerBuffer(BVAR_VERTICAL_SCROLL_BAR_TYPE, VERTICAL_SCROLL_BAR));
        context.forwardTo(HORIZONTAL_SCROLL_BAR, new ValueStorage.ForwardedPerBuffer(BVAR_HORIZONTAL_SCROLL_BAR_TYPE, NIL));
        context.forwardTo(INDICATE_EMPTY_LINES, new ValueStorage.ForwardedPerBuffer(BVAR_INDICATE_EMPTY_LINES, NIL));
        context.forwardTo(INDICATE_BUFFER_BOUNDARIES, new ValueStorage.ForwardedPerBuffer(BVAR_INDICATE_BUFFER_BOUNDARIES, NIL));
        context.forwardTo(FRINGE_INDICATOR_ALIST, new ValueStorage.ForwardedPerBuffer(BVAR_FRINGE_INDICATOR_ALIST, NIL));
        context.forwardTo(FRINGE_CURSOR_ALIST, new ValueStorage.ForwardedPerBuffer(BVAR_FRINGE_CURSOR_ALIST, NIL));
        context.forwardTo(BUFFER_DISPLAY_TIME, new ValueStorage.ForwardedPerBuffer(BVAR_DISPLAY_TIME, NIL));
        context.forwardTo(SCROLL_UP_AGGRESSIVELY, new ValueStorage.ForwardedPerBuffer(BVAR_SCROLL_UP_AGGRESSIVELY, FRACTION));
        context.forwardTo(SCROLL_DOWN_AGGRESSIVELY, new ValueStorage.ForwardedPerBuffer(BVAR_SCROLL_DOWN_AGGRESSIVELY, FRACTION));
        context.forwardTo(CURSOR_TYPE, new ValueStorage.ForwardedPerBuffer(BVAR_CURSOR_TYPE, NIL));
        context.forwardTo(LINE_SPACING, new ValueStorage.ForwardedPerBuffer(BVAR_EXTRA_LINE_SPACING, NUMBERP));
        context.forwardTo(TEXT_CONVERSION_STYLE, new ValueStorage.ForwardedPerBuffer(BVAR_TEXT_CONVERSION_STYLE, NIL));
        context.forwardTo(CURSOR_IN_NON_SELECTED_WINDOWS, new ValueStorage.ForwardedPerBuffer(BVAR_CURSOR_IN_NON_SELECTED_WINDOWS, NIL));
        context.forwardTo(BUFFER_UNDO_LIST, new ValueStorage.ForwardedPerBuffer(BVAR_UNDO_LIST, NIL));
        //#endregion init_buffer_once
        defaultValues.resetLocalVariables(true);
    }

    public static void initDirectory() {
        String cwd = ELispContext.get(null).getEnv("user.dir");
        ELispBuffer currentBuffer = currentBuffer();
        if (cwd == null) {
            currentBuffer.setDirectory(false);
        } else {
            String dirString = Path.of(cwd).toAbsolutePath() + File.separator;
            ELispString dir = new ELispString(dirString);
            Object handler = BuiltInFileIO.FFindFileNameHandler.findFileNameHandler(dir, true);
            if (!isNil(handler) && !dirString.equals("/")) {
                currentBuffer.setDirectory(new ELispString("/:" + dirString));
            } else {
                currentBuffer.setDirectory(dir);
            }
        }
        getMiniBuffer(0).setDirectory(currentBuffer.getDirectory());
    }
}
