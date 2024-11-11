package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import party.iroiro.juicemacs.elisp.forms.BuiltInBuffer.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInCaseTab;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.*;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol.Value.Forwarded;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;

import static party.iroiro.juicemacs.elisp.forms.BuiltInCaseTab.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

public final class ELispBuffer implements ELispValue {
    private final HashMap<ELispSymbol, Forwarded> localVariables;
    private final boolean inhibitBufferHooks;
    private final PieceTreeBase content;
    private long point;

    private ELispBuffer(Object[] bufferLocalFields, PieceTreeBase content, boolean inhibitBufferHooks) {
        this.bufferLocalFields = bufferLocalFields;
        this.content = content;
        this.inhibitBufferHooks = inhibitBufferHooks;
        this.point = 0;
        this.localVariables = new HashMap<>();
    }

    public ELispBuffer(boolean inhibitBufferHooks) {
        this(
                Arrays.copyOf(BUFFER_DEFAULTS, BUFFER_DEFAULTS.length),
                new PieceTreeBase(List.of(), PieceTreeBase.EndOfLine.LF, true),
                inhibitBufferHooks
        );
    }

    public long getPoint() {
        return point;
    }

    public void setPoint(long point) {
        this.point = point;
    }

    public long getChar(long point) {
        return content.charAt((int) point);
    }

    public void insert(String text) {
        content.insert((int) point, text, false);
        point += text.length();
    }

    @Nullable
    public Forwarded getLocal(ELispSymbol symbol) {
        return localVariables.get(symbol);
    }

    public Forwarded makeLocal(ELispSymbol symbol) {
        return localVariables.computeIfAbsent(symbol, _ -> new Forwarded());
    }

    @Override
    public boolean lispEquals(Object other) {
        return this.equals(other);
    }

    public Object getSlot(int index) {
        return bufferLocalFields[index];
    }

    public void setSlot(int index, Object value) {
        bufferLocalFields[index] = value;
    }

    public void resetLocalVariables(boolean permanentToo) {
        setMajorMode(FUNDAMENTAL_MODE);
        setKeymap(false);
        setModeName(new ELispString("Fundamental"));
        if (!(BuiltInData.FCharTableP.charTableP(asciiDowncaseTable.getExtra(0))
                && BuiltInData.FCharTableP.charTableP(asciiDowncaseTable.getExtra(1))
                && BuiltInData.FCharTableP.charTableP(asciiDowncaseTable.getExtra(2)))) {
            BuiltInCaseTab.FSetStandardCaseTable.setStandardCaseTable(asciiDowncaseTable);
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
    private final static Object[] BUFFER_DEFAULTS = Collections.nCopies(77, false).toArray();
    private final static byte[] BUFFER_LOCAL_FLAGS = new byte[77];
    private final static byte[] BUFFER_PERMANENT_LOCAL_FLAGS = new byte[77];
    private final static ELispSymbol[] BUFFER_LOCAL_SYMBOLS = new ELispSymbol[77];
    public final static int BVAR_NAME = 0;
    public final static int BVAR_LAST_NAME = 1;
    public final static int BVAR_FILENAME = 2;
    public final static int BVAR_DIRECTORY = 3;
    public final static int BVAR_BACKED_UP = 4;
    public final static int BVAR_SAVE_LENGTH = 5;
    public final static int BVAR_AUTO_SAVE_FILE_NAME = 6;
    public final static int BVAR_READ_ONLY = 7;
    public final static int BVAR_MARK = 8;
    public final static int BVAR_LOCAL_VAR_ALIST = 9;
    public final static int BVAR_MAJOR_MODE = 10;
    public final static int BVAR_LOCAL_MINOR_MODES = 11;
    public final static int BVAR_MODE_NAME = 12;
    public final static int BVAR_MODE_LINE_FORMAT = 13;
    public final static int BVAR_HEADER_LINE_FORMAT = 14;
    public final static int BVAR_TAB_LINE_FORMAT = 15;
    public final static int BVAR_KEYMAP = 16;
    public final static int BVAR_ABBREV_TABLE = 17;
    public final static int BVAR_SYNTAX_TABLE = 18;
    public final static int BVAR_CATEGORY_TABLE = 19;
    public final static int BVAR_TAB_WIDTH = 20;
    public final static int BVAR_FILL_COLUMN = 21;
    public final static int BVAR_LEFT_MARGIN = 22;
    public final static int BVAR_AUTO_FILL_FUNCTION = 23;
    public final static int BVAR_DOWNCASE_TABLE = 24;
    public final static int BVAR_UPCASE_TABLE = 25;
    public final static int BVAR_CASE_CANON_TABLE = 26;
    public final static int BVAR_CASE_EQV_TABLE = 27;
    public final static int BVAR_TRUNCATE_LINES = 28;
    public final static int BVAR_WORD_WRAP = 29;
    public final static int BVAR_CTL_ARROW = 30;
    public final static int BVAR_BIDI_DISPLAY_REORDERING = 31;
    public final static int BVAR_BIDI_PARAGRAPH_DIRECTION = 32;
    public final static int BVAR_BIDI_PARAGRAPH_SEPARATE_RE = 33;
    public final static int BVAR_BIDI_PARAGRAPH_START_RE = 34;
    public final static int BVAR_SELECTIVE_DISPLAY = 35;
    public final static int BVAR_SELECTIVE_DISPLAY_ELLIPSES = 36;
    public final static int BVAR_OVERWRITE_MODE = 37;
    public final static int BVAR_ABBREV_MODE = 38;
    public final static int BVAR_DISPLAY_TABLE = 39;
    public final static int BVAR_MARK_ACTIVE = 40;
    public final static int BVAR_ENABLE_MULTIBYTE_CHARACTERS = 41;
    public final static int BVAR_BUFFER_FILE_CODING_SYSTEM = 42;
    public final static int BVAR_FILE_FORMAT = 43;
    public final static int BVAR_AUTO_SAVE_FILE_FORMAT = 44;
    public final static int BVAR_CACHE_LONG_SCANS = 45;
    public final static int BVAR_WIDTH_TABLE = 46;
    public final static int BVAR_PT_MARKER = 47;
    public final static int BVAR_BEGV_MARKER = 48;
    public final static int BVAR_ZV_MARKER = 49;
    public final static int BVAR_POINT_BEFORE_SCROLL = 50;
    public final static int BVAR_FILE_TRUENAME = 51;
    public final static int BVAR_INVISIBILITY_SPEC = 52;
    public final static int BVAR_LAST_SELECTED_WINDOW = 53;
    public final static int BVAR_DISPLAY_COUNT = 54;
    public final static int BVAR_LEFT_MARGIN_COLS = 55;
    public final static int BVAR_RIGHT_MARGIN_COLS = 56;
    public final static int BVAR_LEFT_FRINGE_WIDTH = 57;
    public final static int BVAR_RIGHT_FRINGE_WIDTH = 58;
    public final static int BVAR_FRINGES_OUTSIDE_MARGINS = 59;
    public final static int BVAR_SCROLL_BAR_WIDTH = 60;
    public final static int BVAR_SCROLL_BAR_HEIGHT = 61;
    public final static int BVAR_VERTICAL_SCROLL_BAR_TYPE = 62;
    public final static int BVAR_HORIZONTAL_SCROLL_BAR_TYPE = 63;
    public final static int BVAR_INDICATE_EMPTY_LINES = 64;
    public final static int BVAR_INDICATE_BUFFER_BOUNDARIES = 65;
    public final static int BVAR_FRINGE_INDICATOR_ALIST = 66;
    public final static int BVAR_FRINGE_CURSOR_ALIST = 67;
    public final static int BVAR_DISPLAY_TIME = 68;
    public final static int BVAR_SCROLL_UP_AGGRESSIVELY = 69;
    public final static int BVAR_SCROLL_DOWN_AGGRESSIVELY = 70;
    public final static int BVAR_CURSOR_TYPE = 71;
    public final static int BVAR_EXTRA_LINE_SPACING = 72;
    public final static int BVAR_TS_PARSER_LIST = 73;
    public final static int BVAR_TEXT_CONVERSION_STYLE = 74;
    public final static int BVAR_CURSOR_IN_NON_SELECTED_WINDOWS = 75;
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

    public static void initBufferLocalVars() {
        //#region init_buffer_once
        BUFFER_DEFAULTS[BVAR_NAME] = new ELispString(" *buffer-defaults*");
        BUFFER_DEFAULTS[BVAR_MAJOR_MODE] = FUNDAMENTAL_MODE;
        BUFFER_DEFAULTS[BVAR_MODE_LINE_FORMAT] = new ELispString("%-");
        BUFFER_DEFAULTS[BVAR_HEADER_LINE_FORMAT] = false;
        BUFFER_DEFAULTS[BVAR_TAB_LINE_FORMAT] = false;
        BUFFER_DEFAULTS[BVAR_ABBREV_TABLE] = false;
        BUFFER_DEFAULTS[BVAR_TAB_WIDTH] = 8L;
        BUFFER_DEFAULTS[BVAR_FILL_COLUMN] = 70L;
        BUFFER_DEFAULTS[BVAR_LEFT_MARGIN] = 0L;
        BUFFER_DEFAULTS[BVAR_AUTO_FILL_FUNCTION] = false;
        BUFFER_DEFAULTS[BVAR_TRUNCATE_LINES] = false;
        BUFFER_DEFAULTS[BVAR_WORD_WRAP] = false;
        BUFFER_DEFAULTS[BVAR_CTL_ARROW] = true;
        BUFFER_DEFAULTS[BVAR_BIDI_DISPLAY_REORDERING] = true;
        BUFFER_DEFAULTS[BVAR_BIDI_PARAGRAPH_DIRECTION] = false;
        BUFFER_DEFAULTS[BVAR_BIDI_PARAGRAPH_SEPARATE_RE] = false;
        BUFFER_DEFAULTS[BVAR_BIDI_PARAGRAPH_START_RE] = false;
        BUFFER_DEFAULTS[BVAR_SELECTIVE_DISPLAY] = false;
        BUFFER_DEFAULTS[BVAR_SELECTIVE_DISPLAY_ELLIPSES] = true;
        BUFFER_DEFAULTS[BVAR_OVERWRITE_MODE] = false;
        BUFFER_DEFAULTS[BVAR_ABBREV_MODE] = false;
        BUFFER_DEFAULTS[BVAR_DISPLAY_TABLE] = false;
        BUFFER_DEFAULTS[BVAR_MARK_ACTIVE] = false;
        BUFFER_DEFAULTS[BVAR_ENABLE_MULTIBYTE_CHARACTERS] = true;
        BUFFER_DEFAULTS[BVAR_BUFFER_FILE_CODING_SYSTEM] = false;
        BUFFER_DEFAULTS[BVAR_FILE_FORMAT] = false;
        BUFFER_DEFAULTS[BVAR_AUTO_SAVE_FILE_FORMAT] = true;
        BUFFER_DEFAULTS[BVAR_CACHE_LONG_SCANS] = true;
        BUFFER_DEFAULTS[BVAR_FILE_TRUENAME] = false;
        BUFFER_DEFAULTS[BVAR_DISPLAY_COUNT] = 0L;
        BUFFER_DEFAULTS[BVAR_LEFT_MARGIN_COLS] = 0L;
        BUFFER_DEFAULTS[BVAR_RIGHT_MARGIN_COLS] = 0L;
        BUFFER_DEFAULTS[BVAR_LEFT_FRINGE_WIDTH] = false;
        BUFFER_DEFAULTS[BVAR_RIGHT_FRINGE_WIDTH] = false;
        BUFFER_DEFAULTS[BVAR_FRINGES_OUTSIDE_MARGINS] = false;
        BUFFER_DEFAULTS[BVAR_SCROLL_BAR_WIDTH] = false;
        BUFFER_DEFAULTS[BVAR_SCROLL_BAR_HEIGHT] = false;
        BUFFER_DEFAULTS[BVAR_VERTICAL_SCROLL_BAR_TYPE] = true;
        BUFFER_DEFAULTS[BVAR_HORIZONTAL_SCROLL_BAR_TYPE] = true;
        BUFFER_DEFAULTS[BVAR_INDICATE_EMPTY_LINES] = false;
        BUFFER_DEFAULTS[BVAR_INDICATE_BUFFER_BOUNDARIES] = false;
        BUFFER_DEFAULTS[BVAR_FRINGE_INDICATOR_ALIST] = false;
        BUFFER_DEFAULTS[BVAR_FRINGE_CURSOR_ALIST] = false;
        BUFFER_DEFAULTS[BVAR_DISPLAY_TIME] = false;
        BUFFER_DEFAULTS[BVAR_SCROLL_UP_AGGRESSIVELY] = false;
        BUFFER_DEFAULTS[BVAR_SCROLL_DOWN_AGGRESSIVELY] = false;
        BUFFER_DEFAULTS[BVAR_CURSOR_TYPE] = true;
        BUFFER_DEFAULTS[BVAR_EXTRA_LINE_SPACING] = false;
        BUFFER_DEFAULTS[BVAR_TEXT_CONVERSION_STYLE] = false;
        BUFFER_DEFAULTS[BVAR_CURSOR_IN_NON_SELECTED_WINDOWS] = true;
        BUFFER_DEFAULTS[BVAR_UNDO_LIST] = false;
        BUFFER_PERMANENT_LOCAL_FLAGS[8] = 1;
        BUFFER_PERMANENT_LOCAL_FLAGS[22] = 1;
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
        BUFFER_LOCAL_FLAGS[BVAR_TRUNCATE_LINES] = 8;
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
        BUFFER_LOCAL_FLAGS[BVAR_BUFFER_FILE_CODING_SYSTEM] = 22;
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
        BUFFER_LOCAL_FLAGS[BVAR_TEXT_CONVERSION_STYLE] = 42;
        BUFFER_LOCAL_FLAGS[BVAR_CURSOR_IN_NON_SELECTED_WINDOWS] = 43;
        BUFFER_LOCAL_FLAGS[BVAR_UNDO_LIST] = -1;
        DEFAULT_VALUES.resetLocalVariables(true);
        FPut.put(KILL_BUFFER_HOOK, PERMANENT_LOCAL, true);
        FSetBuffer.setBuffer(FGetBufferCreate.getBufferCreate(new ELispString("*scratch*"), false));
        //#endregion init_buffer_once
        //#region DEFVAR_PER_BUFFER
        TAB_LINE_FORMAT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(15, NIL)); // tab-line-format
        BUFFER_LOCAL_SYMBOLS[15] = TAB_LINE_FORMAT;
        HEADER_LINE_FORMAT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(14, NIL)); // header-line-format
        BUFFER_LOCAL_SYMBOLS[14] = HEADER_LINE_FORMAT;
        MODE_LINE_FORMAT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(13, NIL)); // mode-line-format
        BUFFER_LOCAL_SYMBOLS[13] = MODE_LINE_FORMAT;
        MAJOR_MODE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(10, SYMBOLP)); // major-mode
        BUFFER_LOCAL_SYMBOLS[10] = MAJOR_MODE;
        LOCAL_MINOR_MODES.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(11, NIL)); // local-minor-modes
        BUFFER_LOCAL_SYMBOLS[11] = LOCAL_MINOR_MODES;
        MODE_NAME.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(12, NIL)); // mode-name
        BUFFER_LOCAL_SYMBOLS[12] = MODE_NAME;
        LOCAL_ABBREV_TABLE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(17, NIL)); // local-abbrev-table
        BUFFER_LOCAL_SYMBOLS[17] = LOCAL_ABBREV_TABLE;
        ABBREV_MODE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(38, NIL)); // abbrev-mode
        BUFFER_LOCAL_SYMBOLS[38] = ABBREV_MODE;
        FILL_COLUMN.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(21, INTEGERP)); // fill-column
        BUFFER_LOCAL_SYMBOLS[21] = FILL_COLUMN;
        LEFT_MARGIN.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(22, INTEGERP)); // left-margin
        BUFFER_LOCAL_SYMBOLS[22] = LEFT_MARGIN;
        TAB_WIDTH.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(20, INTEGERP)); // tab-width
        BUFFER_LOCAL_SYMBOLS[20] = TAB_WIDTH;
        CTL_ARROW.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(30, NIL)); // ctl-arrow
        BUFFER_LOCAL_SYMBOLS[30] = CTL_ARROW;
        ENABLE_MULTIBYTE_CHARACTERS.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(41, NIL)); // enable-multibyte-characters
        BUFFER_LOCAL_SYMBOLS[41] = ENABLE_MULTIBYTE_CHARACTERS;
        BUFFER_FILE_CODING_SYSTEM.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(42, NIL)); // buffer-file-coding-system
        BUFFER_LOCAL_SYMBOLS[42] = BUFFER_FILE_CODING_SYSTEM;
        BIDI_DISPLAY_REORDERING.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(31, NIL)); // bidi-display-reordering
        BUFFER_LOCAL_SYMBOLS[31] = BIDI_DISPLAY_REORDERING;
        BIDI_PARAGRAPH_START_RE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(34, NIL)); // bidi-paragraph-start-re
        BUFFER_LOCAL_SYMBOLS[34] = BIDI_PARAGRAPH_START_RE;
        BIDI_PARAGRAPH_SEPARATE_RE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(33, NIL)); // bidi-paragraph-separate-re
        BUFFER_LOCAL_SYMBOLS[33] = BIDI_PARAGRAPH_SEPARATE_RE;
        BIDI_PARAGRAPH_DIRECTION.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(32, NIL)); // bidi-paragraph-direction
        BUFFER_LOCAL_SYMBOLS[32] = BIDI_PARAGRAPH_DIRECTION;
        TRUNCATE_LINES.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(28, NIL)); // truncate-lines
        BUFFER_LOCAL_SYMBOLS[28] = TRUNCATE_LINES;
        WORD_WRAP.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(29, NIL)); // word-wrap
        BUFFER_LOCAL_SYMBOLS[29] = WORD_WRAP;
        DEFAULT_DIRECTORY.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(3, STRINGP)); // default-directory
        BUFFER_LOCAL_SYMBOLS[3] = DEFAULT_DIRECTORY;
        AUTO_FILL_FUNCTION.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(23, NIL)); // auto-fill-function
        BUFFER_LOCAL_SYMBOLS[23] = AUTO_FILL_FUNCTION;
        BUFFER_FILE_NAME.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(2, STRINGP)); // buffer-file-name
        BUFFER_LOCAL_SYMBOLS[2] = BUFFER_FILE_NAME;
        BUFFER_FILE_TRUENAME.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(51, STRINGP)); // buffer-file-truename
        BUFFER_LOCAL_SYMBOLS[51] = BUFFER_FILE_TRUENAME;
        BUFFER_AUTO_SAVE_FILE_NAME.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(6, STRINGP)); // buffer-auto-save-file-name
        BUFFER_LOCAL_SYMBOLS[6] = BUFFER_AUTO_SAVE_FILE_NAME;
        BUFFER_READ_ONLY.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(7, NIL)); // buffer-read-only
        BUFFER_LOCAL_SYMBOLS[7] = BUFFER_READ_ONLY;
        BUFFER_BACKED_UP.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(4, NIL)); // buffer-backed-up
        BUFFER_LOCAL_SYMBOLS[4] = BUFFER_BACKED_UP;
        BUFFER_SAVED_SIZE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(5, INTEGERP)); // buffer-saved-size
        BUFFER_LOCAL_SYMBOLS[5] = BUFFER_SAVED_SIZE;
        SELECTIVE_DISPLAY.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(35, NIL)); // selective-display
        BUFFER_LOCAL_SYMBOLS[35] = SELECTIVE_DISPLAY;
        SELECTIVE_DISPLAY_ELLIPSES.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(36, NIL)); // selective-display-ellipses
        BUFFER_LOCAL_SYMBOLS[36] = SELECTIVE_DISPLAY_ELLIPSES;
        OVERWRITE_MODE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(37, OVERWRITE_MODE)); // overwrite-mode
        BUFFER_LOCAL_SYMBOLS[37] = OVERWRITE_MODE;
        BUFFER_DISPLAY_TABLE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(39, NIL)); // buffer-display-table
        BUFFER_LOCAL_SYMBOLS[39] = BUFFER_DISPLAY_TABLE;
        LEFT_MARGIN_WIDTH.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(55, INTEGERP)); // left-margin-width
        BUFFER_LOCAL_SYMBOLS[55] = LEFT_MARGIN_WIDTH;
        RIGHT_MARGIN_WIDTH.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(56, INTEGERP)); // right-margin-width
        BUFFER_LOCAL_SYMBOLS[56] = RIGHT_MARGIN_WIDTH;
        LEFT_FRINGE_WIDTH.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(57, INTEGERP)); // left-fringe-width
        BUFFER_LOCAL_SYMBOLS[57] = LEFT_FRINGE_WIDTH;
        RIGHT_FRINGE_WIDTH.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(58, INTEGERP)); // right-fringe-width
        BUFFER_LOCAL_SYMBOLS[58] = RIGHT_FRINGE_WIDTH;
        FRINGES_OUTSIDE_MARGINS.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(59, NIL)); // fringes-outside-margins
        BUFFER_LOCAL_SYMBOLS[59] = FRINGES_OUTSIDE_MARGINS;
        SCROLL_BAR_WIDTH.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(60, INTEGERP)); // scroll-bar-width
        BUFFER_LOCAL_SYMBOLS[60] = SCROLL_BAR_WIDTH;
        SCROLL_BAR_HEIGHT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(61, INTEGERP)); // scroll-bar-height
        BUFFER_LOCAL_SYMBOLS[61] = SCROLL_BAR_HEIGHT;
        VERTICAL_SCROLL_BAR.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(62, VERTICAL_SCROLL_BAR)); // vertical-scroll-bar
        BUFFER_LOCAL_SYMBOLS[62] = VERTICAL_SCROLL_BAR;
        HORIZONTAL_SCROLL_BAR.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(63, NIL)); // horizontal-scroll-bar
        BUFFER_LOCAL_SYMBOLS[63] = HORIZONTAL_SCROLL_BAR;
        INDICATE_EMPTY_LINES.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(64, NIL)); // indicate-empty-lines
        BUFFER_LOCAL_SYMBOLS[64] = INDICATE_EMPTY_LINES;
        INDICATE_BUFFER_BOUNDARIES.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(65, NIL)); // indicate-buffer-boundaries
        BUFFER_LOCAL_SYMBOLS[65] = INDICATE_BUFFER_BOUNDARIES;
        FRINGE_INDICATOR_ALIST.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(66, NIL)); // fringe-indicator-alist
        BUFFER_LOCAL_SYMBOLS[66] = FRINGE_INDICATOR_ALIST;
        FRINGE_CURSOR_ALIST.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(67, NIL)); // fringe-cursor-alist
        BUFFER_LOCAL_SYMBOLS[67] = FRINGE_CURSOR_ALIST;
        SCROLL_UP_AGGRESSIVELY.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(69, FRACTION)); // scroll-up-aggressively
        BUFFER_LOCAL_SYMBOLS[69] = SCROLL_UP_AGGRESSIVELY;
        SCROLL_DOWN_AGGRESSIVELY.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(70, FRACTION)); // scroll-down-aggressively
        BUFFER_LOCAL_SYMBOLS[70] = SCROLL_DOWN_AGGRESSIVELY;
        BUFFER_UNDO_LIST.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(76, NIL)); // buffer-undo-list
        BUFFER_LOCAL_SYMBOLS[76] = BUFFER_UNDO_LIST;
        MARK_ACTIVE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(40, NIL)); // mark-active
        BUFFER_LOCAL_SYMBOLS[40] = MARK_ACTIVE;
        CACHE_LONG_SCANS.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(45, NIL)); // cache-long-scans
        BUFFER_LOCAL_SYMBOLS[45] = CACHE_LONG_SCANS;
        POINT_BEFORE_SCROLL.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(50, NIL)); // point-before-scroll
        BUFFER_LOCAL_SYMBOLS[50] = POINT_BEFORE_SCROLL;
        BUFFER_FILE_FORMAT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(43, NIL)); // buffer-file-format
        BUFFER_LOCAL_SYMBOLS[43] = BUFFER_FILE_FORMAT;
        BUFFER_AUTO_SAVE_FILE_FORMAT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(44, NIL)); // buffer-auto-save-file-format
        BUFFER_LOCAL_SYMBOLS[44] = BUFFER_AUTO_SAVE_FILE_FORMAT;
        BUFFER_INVISIBILITY_SPEC.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(52, NIL)); // buffer-invisibility-spec
        BUFFER_LOCAL_SYMBOLS[52] = BUFFER_INVISIBILITY_SPEC;
        BUFFER_DISPLAY_COUNT.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(54, INTEGERP)); // buffer-display-count
        BUFFER_LOCAL_SYMBOLS[54] = BUFFER_DISPLAY_COUNT;
        BUFFER_DISPLAY_TIME.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(68, NIL)); // buffer-display-time
        BUFFER_LOCAL_SYMBOLS[68] = BUFFER_DISPLAY_TIME;
        CURSOR_TYPE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(71, NIL)); // cursor-type
        BUFFER_LOCAL_SYMBOLS[71] = CURSOR_TYPE;
        LINE_SPACING.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(72, NUMBERP)); // line-spacing
        BUFFER_LOCAL_SYMBOLS[72] = LINE_SPACING;
        CURSOR_IN_NON_SELECTED_WINDOWS.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(75, NIL)); // cursor-in-non-selected-windows
        BUFFER_LOCAL_SYMBOLS[75] = CURSOR_IN_NON_SELECTED_WINDOWS;
        TEXT_CONVERSION_STYLE.initForwardTo(new ELispSymbol.Value.ForwardedPerBuffer(74, NIL)); // text-conversion-style
        BUFFER_LOCAL_SYMBOLS[74] = TEXT_CONVERSION_STYLE;
        //#endregion DEFVAR_PER_BUFFER
    }

    public static final ELispBuffer DEFAULT_VALUES = new ELispBuffer(
            BUFFER_DEFAULTS,
            new PieceTreeBase(List.of(), PieceTreeBase.EndOfLine.LF, true),
            true
    );
}
