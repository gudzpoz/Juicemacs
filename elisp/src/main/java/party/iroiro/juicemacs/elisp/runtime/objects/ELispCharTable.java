package party.iroiro.juicemacs.elisp.runtime.objects;

import java.util.ArrayList;
import java.util.List;

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
public class ELispCharTable extends AbstractELispVector {
    public final static int CHARTAB_SIZE_BITS_0 = 6;
    public final static int CHARTAB_SIZE_BITS_1 = 4;
    public final static int CHARTAB_SIZE_BITS_2 = 5;
    public final static int CHARTAB_SIZE_BITS_3 = 7;
    private final static int[] CHARTAB_BITS = new int[] {
            CHARTAB_SIZE_BITS_3 + CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_1,
            CHARTAB_SIZE_BITS_3 + CHARTAB_SIZE_BITS_2,
            CHARTAB_SIZE_BITS_3,
            0,
    };
    public final static int CHARTAB_STANDARD_SLOTS = 4 + (1 << CHARTAB_SIZE_BITS_0);
    public final static int DEFAULT_VALUT_SLOT = 0;
    public final static int PARENT_SLOT = 1;
    public final static int PURPOSE_SLOT = 2;
    public final static int ASCII_SLOT = 3;

    private ELispCharTable(List<Object> inner) {
        super(inner);
    }

    public ELispCharTable(Object init, int extraSlots) {
        this(getInner(init, extraSlots));
    }

    private static List<Object> getInner(Object init, int extraSlots) {
        ArrayList<Object> objects = new ArrayList<>(CHARTAB_STANDARD_SLOTS + extraSlots);
        for (int i = 0; i < CHARTAB_STANDARD_SLOTS + extraSlots; i++) {
            objects.add(init);
        }
        return objects;
    }

    public void setParent(Object parent) {
        set(PARENT_SLOT, parent);
    }

    public void setPurpose(ELispSymbol purpose) {
        set(PURPOSE_SLOT, purpose);
    }

    public ELispSymbol getPurpose() {
        return (ELispSymbol) get(PURPOSE_SLOT);
    }

    public Object getParent() {
        return get(PARENT_SLOT);
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
        return get(n + CHARTAB_STANDARD_SLOTS);
    }

    public void setExtra(int n, Object value) {
        if (n < 0) {
            throw new ArrayIndexOutOfBoundsException();
        }
        set(n + CHARTAB_STANDARD_SLOTS, value);
    }

    public static ELispCharTable create(List<Object> objects) {
        if (objects.size() < CHARTAB_STANDARD_SLOTS) {
            throw new IllegalArgumentException();
        }
        return new ELispCharTable(objects);
    }

    public static class SubTable extends AbstractELispVector {
        public final static int DEPTH_SLOT = 0;
        public final static int MIN_CHAR_SLOT = 1;

        private SubTable(List<Object> inner) {
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
                default -> throw new IllegalArgumentException();
            };
        }

        public static SubTable create(List<Object> objects) {
            //noinspection SequencedCollectionMethodCanBeUsed
            int depth = (int) (long) objects.get(DEPTH_SLOT);
            int minChar = (int) (long) objects.get(MIN_CHAR_SLOT);
            if (
                    (minChar < 0 || Character.MAX_CODE_POINT < minChar)
                    || (depth <= 0 || 3 < depth)
                    || (objects.size() < 2 + getSlots(depth))
            ) {
                throw new IllegalArgumentException();
            }
            return new SubTable(objects);
        }

        public int getDepth() {
            return (int) (long) get(DEPTH_SLOT);
        }

        public int getMinChar() {
            return (int) (long) get(MIN_CHAR_SLOT);
        }
    }
}
