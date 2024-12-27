package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.apache.commons.text.WordUtils;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.List;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCharTable;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.notNilOr;

public class BuiltInCaseFiddle extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCaseFiddleFactory.getFactories();
    }

    /**
     * <pre>
     * Convert argument to upper case and return that.
     * The argument may be a character or string.  The result has the same
     * type.  (See `downcase' for further details about the type.)
     *
     * The argument object is not altered--the value is a copy.  If argument
     * is a character, characters which map to multiple code points when
     * cased, e.g. ﬁ, are returned unchanged.
     *
     * See also `capitalize', `downcase' and `upcase-initials'.
     * </pre>
     */
    @ELispBuiltIn(name = "upcase", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUpcase extends ELispBuiltInBaseNode {
        @Specialization
        public static long upcaseChar(long obj) {
            Object upcase = asCharTable(currentBuffer().getUpcaseTable()).getChar(Math.toIntExact(obj));
            return notNilOr(upcase, obj);
        }
        @Specialization
        public static ELispString upcaseString(ELispString obj) {
            MuleStringBuffer builder = new MuleStringBuffer();
            PrimitiveIterator.OfInt iterator = obj.value().iterator(0);
            while (iterator.hasNext()) {
                builder.append(Math.toIntExact(upcaseChar(iterator.nextInt())));
            }
            return new ELispString(builder.build());
        }
    }

    /**
     * <pre>
     * Convert argument to lower case and return that.
     * The argument may be a character or string.  The result has the same type,
     * including the multibyteness of the string.
     *
     * This means that if this function is called with a unibyte string
     * argument, and downcasing it would turn it into a multibyte string
     * (according to the current locale), the downcasing is done using ASCII
     * \"C\" rules instead.  To accurately downcase according to the current
     * locale, the string must be converted into multibyte first.
     *
     * The argument object is not altered--the value is a copy.
     * </pre>
     */
    @ELispBuiltIn(name = "downcase", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDowncase extends ELispBuiltInBaseNode {
        @Specialization
        public static long downcaseChar(long obj) {
            Object downcase = asCharTable(currentBuffer().getDowncaseTable()).getChar(Math.toIntExact(obj));
            return notNilOr(downcase, obj);
        }
        @Specialization
        public static ELispString downcaseString(ELispString obj) {
            MuleStringBuffer builder = new MuleStringBuffer();
            PrimitiveIterator.OfInt iterator = obj.value().iterator(0);
            while (iterator.hasNext()) {
                builder.append(Math.toIntExact(downcaseChar(iterator.nextInt())));
            }
            return new ELispString(builder.build());
        }
    }

    /**
     * <pre>
     * Convert argument to capitalized form and return that.
     * This means that each word's first character is converted to either
     * title case or upper case, and the rest to lower case.
     *
     * The argument may be a character or string.  The result has the same
     * type.  (See `downcase' for further details about the type.)
     *
     * The argument object is not altered--the value is a copy.  If argument
     * is a character, characters which map to multiple code points when
     * cased, e.g. ﬁ, are returned unchanged.
     * </pre>
     */
    @ELispBuiltIn(name = "capitalize", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCapitalize extends ELispBuiltInBaseNode {
        @Specialization
        public static long capitalize(long obj) {
            return Character.toUpperCase((int) obj);
        }
        @Specialization
        public static ELispString capitalizeStr(ELispString obj) {
            return new ELispString(WordUtils.capitalize(obj.toString()));
        }
    }

    /**
     * <pre>
     * Convert the initial of each word in the argument to upper case.
     * This means that each word's first character is converted to either
     * title case or upper case, and the rest are left unchanged.
     *
     * The argument may be a character or string.  The result has the same
     * type.  (See `downcase' for further details about the type.)
     *
     * The argument object is not altered--the value is a copy.  If argument
     * is a character, characters which map to multiple code points when
     * cased, e.g. ﬁ, are returned unchanged.
     * </pre>
     */
    @ELispBuiltIn(name = "upcase-initials", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUpcaseInitials extends ELispBuiltInBaseNode {
        @Specialization
        public static Void upcaseInitials(Object obj) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert the region to upper case.  In programs, wants two arguments.
     * These arguments specify the starting and ending character numbers of
     * the region to operate on.  When used as a command, the text between
     * point and the mark is operated on.
     * See also `capitalize-region'.
     * </pre>
     */
    @ELispBuiltIn(name = "upcase-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FUpcaseRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void upcaseRegion(Object beg, Object end, Object regionNoncontiguousP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert the region to lower case.  In programs, wants two arguments.
     * These arguments specify the starting and ending character numbers of
     * the region to operate on.  When used as a command, the text between
     * point and the mark is operated on.
     * </pre>
     */
    @ELispBuiltIn(name = "downcase-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDowncaseRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void downcaseRegion(Object beg, Object end, Object regionNoncontiguousP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert the region to capitalized form.
     * This means that each word's first character is converted to either
     * title case or upper case, and the rest to lower case.
     * In programs, give two arguments, the starting and ending
     * character positions to operate on.
     * </pre>
     */
    @ELispBuiltIn(name = "capitalize-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FCapitalizeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void capitalizeRegion(Object beg, Object end, Object regionNoncontiguousP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Upcase the initial of each word in the region.
     * This means that each word's first character is converted to either
     * title case or upper case, and the rest are left unchanged.
     * In programs, give two arguments, the starting and ending
     * character positions to operate on.
     * </pre>
     */
    @ELispBuiltIn(name = "upcase-initials-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FUpcaseInitialsRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void upcaseInitialsRegion(Object beg, Object end, Object regionNoncontiguousP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert to upper case from point to end of word, moving over.
     *
     * If point is in the middle of a word, the part of that word before point
     * is ignored when moving forward.
     *
     * With negative argument, convert previous words but do not move.
     * See also `capitalize-word'.
     * </pre>
     */
    @ELispBuiltIn(name = "upcase-word", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUpcaseWord extends ELispBuiltInBaseNode {
        @Specialization
        public static Void upcaseWord(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert to lower case from point to end of word, moving over.
     *
     * If point is in the middle of a word, the part of that word before point
     * is ignored when moving forward.
     *
     * With negative argument, convert previous words but do not move.
     * </pre>
     */
    @ELispBuiltIn(name = "downcase-word", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDowncaseWord extends ELispBuiltInBaseNode {
        @Specialization
        public static Void downcaseWord(Object arg) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Capitalize from point to the end of word, moving over.
     * With numerical argument ARG, capitalize the next ARG-1 words as well.
     * This gives the word(s) a first character in upper case
     * and the rest lower case.
     *
     * If point is in the middle of a word, the part of that word before point
     * is ignored when moving forward.
     *
     * With negative argument, capitalize previous words but do not move.
     * </pre>
     */
    @ELispBuiltIn(name = "capitalize-word", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCapitalizeWord extends ELispBuiltInBaseNode {
        @Specialization
        public static Void capitalizeWord(Object arg) {
            throw new UnsupportedOperationException();
        }
    }
}
