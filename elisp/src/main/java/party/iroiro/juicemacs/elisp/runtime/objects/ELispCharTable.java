package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.*;
import java.util.function.BiPredicate;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.CHAR_CODE_PROPERTY_TABLE;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asSym;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

/**
 * Char table object
 *
 * <p>
 * The internals of ELisp char-tables are rather under-documented,
 * and this Javadoc serves to clarify how char-tables are actually
 * implemented in Emacs, based on Emacs source code {@code src/chartab.c}.
 * Readers of this documentation should first familiarize themselves
 * with <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Char_002dTables.html">
 * the Char-Tables section in the official documentation</a>.
 * </p>
 * <h2>Internal Structures</h2>
 * <p>
 * The following comment is taken from {@code src/lisp.h}, briefly describing
 * char-tables in Emacs:
 * </p>
 * <blockquote>
 * /* A char-table is a kind of vectorlike, with contents like a vector,
 * but with a few additional slots.  For some purposes, it makes sense
 * to handle a char-table as type 'struct Lisp_Vector'.  An element of
 * a char-table can be any Lisp object, but if it is a sub char-table,
 * we treat it as a table that contains information of a specific
 * range of characters.  A sub char-table is like a vector, but with
 * two integer fields between the header and Lisp data, which means
 * that it has to be marked with some precautions (see mark_char_table
 * in alloc.c).  A sub char-table appears in an element of a char-table.  &#42;/
 * </blockquote>
 * <p>
 * Looking at {@code struct Lisp_Char_Table} and {@code struct Lisp_Sub_Char_Table},
 * basically, the memory layout of a char-table / sub char-table is similar to a vector
 * of the following structure:
 * </p>
 * <pre><code>
 * struct Lisp_Char_Table :: vector [
 *     default-value parent-table purpose ascii-sub-table
 *     content-slot-1 ... content-slot-64  ;; 64 == 1 << CHARTAB_SIZE_BITS_0
 *     extra-slot-1 ... extra-slot-N       ;; determined by 'purpose
 * ]
 * struct Lisp_Sub_Char_Table :: vector [
 *     depth     ;; 1, 2, or 3
 *     min_char  ;; character range left bound
 *     content-slot-1 ... content-slot-N  ;; N == 1 << CHARTAB_SIZE_BITS_<depth>
 * ]
 * </code></pre>
 * <p>
 * One can look into the internal structure of a char-table / sub char-table
 * using by simply printing it out in the scratch buffer. (And exactly
 * because how Emacs exposes the internal structure of char-tables, we will
 * need to understand what each field / slot does to deserialize a char-table
 * in our Lisp parser.)
 * </p>
 * <p>
 * Overall, char-tables holds several sub char-tables (depth-1), which in turn
 * holds several sub char-tables (depth-2), which in turn holds several sub char
 * tables (depth-3). (It is quite similar to multilevel page tables, if you are
 * familiar with operating systems.) Quoting from an Emacs comment in
 * {@code src/lisp.h}:
 * </p>
 * <blockquote>
 * /* Depth of this sub char-table.  It should be 1, 2, or 3.  A sub
 * char-table of depth 1 contains 16 elements, and each element
 * covers 4096 (128*32) characters.  A sub char-table of depth 2
 * contains 32 elements, and each element covers 128 characters.  A
 * sub char-table of depth 3 contains 128 elements, and each element
 * is for one character.  &#42;/
 * </blockquote>
 * <h2>Logic</h2>
 * <p>
 * Although structures of char-tables are similar to vectors, {@code aref} on
 * char-tables is not as simple as {@code aref} on vectors. Instead, vector
 * operations perform char look-ups / modifications through the char table.
 * To avoid bloating up the documentation, the logic documentation is
 * scattered into the member functions of {@link party.iroiro.juicemacs.elisp.forms.BuiltInCharTab}.
 * See there instead.
 * </p>
 */
public final class ELispCharTable extends AbstractELispVector {
    private final static int[] CHARTAB_BITS       = {
            CHARTAB_SIZE_BITS_3 + CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_1,
            CHARTAB_SIZE_BITS_3 + CHARTAB_SIZE_BITS_2,
            CHARTAB_SIZE_BITS_3,
            0,
    };
    public final static int MAX_CHAR_INDEX =
            (1 << (CHARTAB_SIZE_BITS_3 + CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_1 + CHARTAB_SIZE_BITS_0)) - 1;
    public final static int DEFAULT_VALUT_SLOT = 0;
    public final static int PARENT_SLOT = 1;
    public final static int PURPOSE_SLOT = 2;
    public final static int ASCII_SLOT = 3;
    public final static int CONTENT_BASE_SLOT = 4;
    public final static int SUB_CONTENT_BASE_SLOT = 2;

    private ELispCharTable(Object[] inner) {
        super(inner);
        setAsciiSlot(getAsciiValue());
        checkCompression(inner);
    }

    private void checkCompression(Object[] inner) {
        // TODO: Decide whether to handle this when setPurpose is called
        if (isUnipropTable()) {
            for (Object slot : inner) {
                if (slot instanceof SubTable subTable) {
                    Object[] objects = subTable.inner;
                    for (int j = 0; j < objects.length; j++) {
                        Object depth2 = objects[j];
                        if (depth2 instanceof SubTable maybeCompressed) {
                            objects[j] = new CompressedUnipropSubTable(maybeCompressed);
                        }
                    }
                }
            }
        }
    }

    private boolean isUnipropTable() {
        return getPurpose() == CHAR_CODE_PROPERTY_TABLE && extraSlots() == 5;
    }

    public ELispCharTable copy() {
        return new ELispCharTable(inner.clone());
    }

    private Object notNilOrDefault(Object value) {
        return isNil(value) ? inner[DEFAULT_VALUT_SLOT] : value;
    }

    public Object getDefault() {
        return inner[DEFAULT_VALUT_SLOT];
    }

    public void setDefault(Object value) {
        inner[DEFAULT_VALUT_SLOT] = value;
    }

    public Object getParent() {
        return inner[PARENT_SLOT];
    }

    public void setParent(Object parent) {
        inner[PARENT_SLOT] = parent;
    }

    public ELispSymbol getPurpose() {
        return asSym(inner[PURPOSE_SLOT]);
    }

    public void setPurpose(ELispSymbol purpose) {
        inner[PURPOSE_SLOT] = purpose;
    }

    public Object getAsciiSlot() {
        return inner[ASCII_SLOT];
    }

    public void setAsciiSlot(Object ascii) {
        inner[ASCII_SLOT] = ascii;
    }

    private Object getAsciiValue() {
        Object content = inner[CONTENT_BASE_SLOT];
        if (!(content instanceof SubTable depth1)) {
            return content;
        }
        content = depth1.get(SUB_CONTENT_BASE_SLOT);
        if (!(content instanceof SubTable depth2)) {
            return content;
        }
        return depth2.get(SUB_CONTENT_BASE_SLOT);
    }

    private Object getContentSlot(int tableIndex) {
        return inner[tableIndex + CONTENT_BASE_SLOT];
    }

    private void setContentSlot(int tableIndex, Object content) {
        inner[tableIndex + CONTENT_BASE_SLOT] = content;
    }

    private SubTable getSubTable(int tableIndex) {
        Object contentSlot = getContentSlot(tableIndex);
        if (contentSlot instanceof SubTable table) {
            return table;
        }
        SubTable table = new SubTable(1, tableIndex * (1 << CHARTAB_BITS[0]), contentSlot);
        setContentSlot(tableIndex, table);
        return table;
    }

    @TruffleBoundary
    public Object getChar(int codepoint) {
        // TODO: When does Emacs fetch from parent tables?
        if (codepoint < 128 && getAsciiSlot() instanceof SubTable ascii) {
            return notNilOrDefault(ascii.getChar(codepoint));
        }
        int i = charTableIndex(codepoint, 0, 0);
        return notNilOrDefault(getSubTable(i).getChar(codepoint));
    }

    @Override
    public Object get(int index) {
        return getChar(index);
    }

    @Override
    public Object set(int index, Object element) {
        setChar(index, element);
        return element;
    }

    @Override
    public void setUntyped(int i, Object object) {
        setChar(i, object);
    }

    @Override
    public int size() {
        return MAX_CHAR_INDEX + 1;
    }

    public int slots() {
        return inner.length;
    }

    @TruffleBoundary
    public void setChar(int codepoint, Object value) {
        if (codepoint < 128 && getAsciiSlot() instanceof SubTable ascii) {
            ascii.setChar(codepoint, value);
            return;
        }
        int i = charTableIndex(codepoint, 0, 0);
        getSubTable(i).setChar(codepoint, value);
        if (codepoint < 128) {
            setAsciiSlot(getAsciiValue());
        }
    }

    @TruffleBoundary
    public void setRange(int from, int to, Object value) {
        if (from == to) {
            setChar(from, value);
            return;
        }
        int limit = charTableIndex(to, 0, 0);
        int step = 1 << CHARTAB_BITS[0];
        for (int i = charTableIndex(from, 0, 0), minChar = i * step;
             i <= limit && minChar <= to; i++, minChar += step) {
            if (from <= minChar && minChar + step - 1 <= to) {
                setContentSlot(i, value);
            } else {
                getSubTable(i).setRange(from, to, value);
            }
        }
        if (from < 128) {
            setAsciiSlot(getAsciiValue());
        }
    }

    @Nullable
    @TruffleBoundary
    public <T> T map(MapConsumer<T> callback, int startingChar) {
        int i = charTableIndex(startingChar, 0, 0);
        Object defaultValue = inner[DEFAULT_VALUT_SLOT];
        for (; i < (1 << CHARTAB_SIZE_BITS_0); i++) {
            Object slot = getContentSlot(i);
            T result;
            if (slot instanceof SubTable sub) {
                result = sub.map(callback, defaultValue, startingChar);
            } else {
                int c = i << CHARTAB_BITS[0];
                result = callback.accept(c, notNilOrDefault(slot));
            }
            if (result != null) {
                return result;
            }
        }
        return callback.accept(MAX_CHAR_INDEX + 1, false);
    }

    public void optimize(BiPredicate<Object, Object> eq) {
        for (int i = 0; i < (1 << CHARTAB_SIZE_BITS_0); i++) {
            Object slot = getContentSlot(i);
            if (slot instanceof SubTable sub) {
                setContentSlot(i, sub.optimize(eq));
            }
        }
    }

    @TruffleBoundary
    public RefRangeResult refRange(int from, int target, int to) {
        return Objects.requireNonNull(map(new MapConsumer<>() {
            int last = from;
            Object value = false;

            @Override
            @Nullable
            public RefRangeResult accept(int codepoint, Object value) {
                if (codepoint > to) {
                    return new RefRangeResult(last, to, this.value);
                }
                if (BuiltInData.FEq.eq(this.value, value)) {
                    return null;
                }
                if (codepoint <= target) {
                    last = codepoint;
                    this.value = value;
                    return null;
                } else {
                    return new RefRangeResult(last, codepoint - 1, this.value);
                }
            }
        }, from));
    }

    public void setAll(Object value) {
        setAsciiSlot(value);
        for (int i = 0; i < (1 << CHARTAB_SIZE_BITS_0); i++) {
            setContentSlot(i, value);
        }
    }

    /**
     * See {@code CHARTAB_IDX} in {@code src/chartab.c}
     */
    public static int charTableIndex(int codepoint, int depth, int minChar) {
        return (codepoint - minChar) >> CHARTAB_BITS[depth];
    }

    public Object getExtra(int n) {
        if (n < 0) {
            throw new ArrayIndexOutOfBoundsException();
        }
        return inner[n + CHAR_TABLE_STANDARD_SLOTS];
    }

    public void setExtra(int n, Object value) {
        if (n < 0) {
            throw new ArrayIndexOutOfBoundsException();
        }
        inner[n + CHAR_TABLE_STANDARD_SLOTS] = value;
    }

    public int extraSlots() {
        return inner.length - CHAR_TABLE_STANDARD_SLOTS;
    }

    @Override
    public void display(ELispPrint print) {
        vectorPrintHelper(print, "#^[", "]", inner);
    }

    public static ELispCharTable create(ArrayList<Object> objects) {
        if (objects.size() < CHAR_TABLE_STANDARD_SLOTS) {
            throw ELispSignals.invalidReadSyntax("Invalid size char-table");
        }
        return new ELispCharTable(objects.toArray());
    }

    public static ELispCharTable create(Object init, ELispSymbol purpose, int extraSlots) {
        Object[] inner = new Object[CHAR_TABLE_STANDARD_SLOTS + extraSlots];
        inner[DEFAULT_VALUT_SLOT] = init;
        inner[PARENT_SLOT] = false;
        inner[PURPOSE_SLOT] = purpose;
        Arrays.fill(inner, CONTENT_BASE_SLOT, inner.length, init);
        return new ELispCharTable(inner);
    }

    public static sealed class SubTable extends AbstractELispVector {
        public final static int DEPTH_SLOT = 0;
        public final static int MIN_CHAR_SLOT = 1;

        private SubTable(List<Object> inner) {
            super(inner.toArray());
        }

        public SubTable(Object[] inner) {
            super(inner);
        }

        public SubTable(int depth, int minChar, Object defaultValue) {
            this(getInner(depth, minChar, defaultValue));
        }

        private static List<Object> getInner(int depth, int minChar, Object defaultValue) {
            int slots = getSlots(depth);
            ArrayList<Object> list = new ArrayList<>(2 + slots);
            list.add((long) depth);
            list.add((long) minChar);
            for (int i = 0; i < slots; i++) {
                list.add(defaultValue);
            }
            return list;
        }

        private static int getSlots(int depth) {
            return 1 << switch (depth) {
                case 1 -> CHARTAB_SIZE_BITS_1;
                case 2 -> CHARTAB_SIZE_BITS_2;
                case 3 -> CHARTAB_SIZE_BITS_3;
                default -> throw CompilerDirectives.shouldNotReachHere();
            };
        }

        public static SubTable create(ArrayList<Object> objects) {
            int depth = (int) (long) objects.get(DEPTH_SLOT);
            int minChar = (int) (long) objects.get(MIN_CHAR_SLOT);
            if (
                    minChar < 0 || Character.MAX_CODE_POINT < minChar
                    || depth <= 0 || 3 < depth
                    || (objects.size() < 2 + getSlots(depth))
            ) {
                throw ELispSignals.invalidReadSyntax("Invalid size sub-char-table");
            }
            return new SubTable(objects);
        }

        public int getDepth() {
            return (int) (long) get(DEPTH_SLOT);
        }

        public int getMinChar() {
            return (int) (long) get(MIN_CHAR_SLOT);
        }

        @TruffleBoundary
        public Object getChar(int codepoint) {
            int depth = getDepth();
            int i = charTableIndex(codepoint, depth, getMinChar());
            if (depth == 3) {
                return getContentSlot(i);
            } else {
                return getSubTable(i).getChar(codepoint);
            }
        }

        @TruffleBoundary
        public void setChar(int codepoint, Object value) {
            int depth = getDepth();
            int i = charTableIndex(codepoint, depth, getMinChar());
            if (depth == 3) {
                setContentSlot(i, value);
            } else {
                getSubTable(i).setChar(codepoint, value);
            }
        }

        public Object getContentSlot(int tableIndex) {
            return get(tableIndex + SUB_CONTENT_BASE_SLOT);
        }

        public void setContentSlot(int tableIndex, Object value) {
            set(tableIndex + SUB_CONTENT_BASE_SLOT, value);
        }

        public SubTable getSubTable(int tableIndex) {
            Object sub = getContentSlot(tableIndex);
            if (sub instanceof SubTable table) {
                return table;
            }
            int depth = getDepth();
            SubTable table = new SubTable(
                    depth + 1,
                    getMinChar() + tableIndex * (1 << CHARTAB_BITS[depth]),
                    sub
            );
            setContentSlot(tableIndex, table);
            return table;
        }

        @TruffleBoundary
        public void setRange(int from, int to, Object value) {
            int minChar = getMinChar();
            int depth = getDepth();
            if (from < minChar) {
                from = minChar;
            }
            int limit = getSlots(depth);
            int step = 1 << CHARTAB_BITS[depth];
            for (int i = charTableIndex(from, depth, minChar), min = minChar + i * step;
                 i < limit && min <= to; i++, min += step) {
                if (from <= min && min + step - 1 <= to) {
                    setContentSlot(i, value);
                } else {
                    getSubTable(i).setRange(from, to, value);
                }
            }
        }

        @Nullable
        @TruffleBoundary
        public <T> T map(MapConsumer<T> callback, Object defaultValue, int startingChar) {
            int minChar = getMinChar();
            int depth = getDepth();
            int limit = getSlots(depth);
            int step = 1 << CHARTAB_BITS[depth];
            int i = Math.max(charTableIndex(startingChar, depth, minChar), 0);
            for (; i < limit; i++) {
                Object slot = getContentSlot(i);
                T result;
                if (slot instanceof SubTable sub) {
                    result = sub.map(callback, defaultValue, startingChar);
                } else {
                    int c = minChar + i * step;
                    result = callback.accept(c, isNil(slot) ? defaultValue : slot);
                }
                if (result != null) {
                    return result;
                }
            }
            return null;
        }

        @TruffleBoundary
        public Object optimize(BiPredicate<Object, Object> eq) {
            boolean optimizable = true;
            Object value = null;
            int slots = getSlots(getDepth());
            for (int i = 0; i < slots; i++) {
                Object slot = getContentSlot(i);
                if (slot instanceof SubTable sub) {
                    slot = sub.optimize(eq);
                }
                if (slot instanceof SubTable) {
                    optimizable = false;
                } else {
                    if (value != null) {
                        if (eq.test(value, slot)) {
                            slot = value;
                        } else {
                            optimizable = false;
                            value = slot;
                        }
                    } else {
                        value = slot;
                    }
                }
                setContentSlot(i, slot);
            }
            return optimizable ? Objects.requireNonNull(value) : this;
        }

        @Override
        public void display(ELispPrint print) {
            vectorPrintHelper(print, "#^[", "]", inner);
        }
    }

    public static final class CompressedUnipropSubTable extends SubTable {
        public CompressedUnipropSubTable(SubTable wrapped) {
            super(wrapped.inner);
            if (getDepth() != 2) {
                throw ELispSignals.fatal("Invalid uniprop table");
            }
        }

        @Override
        public Object getContentSlot(int tableIndex) {
            Object slot = super.getContentSlot(tableIndex);
            if (slot instanceof ELispString compressed && compressed.length() != 0) {
                long start = compressed.codePointAt(0);
                if (start == 1 || start == 2) {
                    int minChar = getMinChar() + tableIndex * SubTable.getSlots(3);
                    slot = decompressTable(compressed, minChar);
                    setContentSlot(tableIndex, slot);
                }
            }
            return slot;
        }

        private SubTable decompressTable(ELispString compressed, int minChar) {
            PrimitiveIterator.OfInt i = compressed.iterator(0);
            int type = i.nextInt();
            SubTable table = new SubTable(3, minChar, false);
            if (type == 1) {
                // Simple
                int index = i.nextInt();
                int endIndex = SubTable.getSlots(3);
                while (i.hasNext() && index < endIndex) {
                    int v = i.nextInt();
                    table.setContentSlot(index, v > 0 ? (long) v : false);
                    index++;
                }
            } else if (type == 2) {
                // Run-length
                int index = 0;
                while (i.hasNext()) {
                    int v = i.nextInt();
                    do {
                        int count = 1;
                        if (i.hasNext()) {
                            count = i.nextInt();
                            if (count < 128) {
                                table.setContentSlot(index, (long) v);
                                index++;
                                v = count;
                                // A dirty way to "peek" the run-length encoding
                                continue;
                            } else {
                                count -= 128;
                            }
                        }
                        while (count-- > 0) {
                            table.setContentSlot(index, v > 0 ? (long) v : false);
                            index++;
                        }
                        break;
                    } while (true);
                }
            } else {
                throw ELispSignals.fatal("Invalid uniprop table");
            }
            return table;
        }
    }

    public record RefRangeResult(int start, int end, Object value) {
    }

    /// Used by [#map(MapConsumer)]
    public interface MapConsumer<T> {
        /// Gets called with the individual values the char table records
        ///
        /// Note that the table only calls the function with the *starting codepoint*
        /// of each interval of the same value.
        ///
        /// @param codepoint the starting codepoint
        /// @param value the associated value
        /// @return non-null to exit early
        @Nullable
        T accept(int codepoint, Object value);
    }
}
