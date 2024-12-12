package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_CHAR;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asInt;

public class BuiltInCharacter extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharacterFactory.getFactories();
    }

    /**
     * <pre>
     * Return non-nil if OBJECT is a character.
     * In Emacs Lisp, characters are represented by character codes, which
     * are non-negative integers.  The function `max-char' returns the
     * maximum character code.
     * usage: (characterp OBJECT)
     * </pre>
     */
    @ELispBuiltIn(name = "characterp", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharacterp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean characterp(Object object, Object ignore) {
            return object instanceof Long l && l <= FMaxChar.maxChar(true);
        }
    }

    /**
     * <pre>
     * Return the maximum character code.
     * If UNICODE is non-nil, return the maximum character code defined
     * by the Unicode Standard.
     * </pre>
     */
    @ELispBuiltIn(name = "max-char", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMaxChar extends ELispBuiltInBaseNode {
        @Specialization
        public static long maxChar(boolean unicode) {
            if (unicode) {
                return Character.MAX_CODE_POINT;
            }
            return MAX_CHAR;
        }
    }

    /**
     * <pre>
     * Convert the byte CH to multibyte character.
     * </pre>
     */
    @ELispBuiltIn(name = "unibyte-char-to-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUnibyteCharToMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unibyteCharToMultibyte(Object ch) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert the multibyte character CH to a byte.
     * If the multibyte character does not represent a byte, return -1.
     * </pre>
     */
    @ELispBuiltIn(name = "multibyte-char-to-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMultibyteCharToUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void multibyteCharToUnibyte(Object ch) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width of CHAR in columns when displayed in the current buffer.
     * The width of CHAR is measured by how many columns it will occupy on the screen.
     * This is based on data in `char-width-table', and ignores the actual
     * metrics of the character's glyph as determined by its font.
     * If the display table in effect replaces CHAR on display with
     * something else, the function returns the width of the replacement.
     * Tab is taken to occupy `tab-width' columns.
     * usage: (char-width CHAR)
     * </pre>
     */
    @ELispBuiltIn(name = "char-width", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charWidth(Object char_) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return width of STRING in columns when displayed in the current buffer.
     * Width of STRING is measured by how many columns it will occupy on the screen.
     *
     * Optional arguments FROM and TO specify the substring of STRING to
     * consider, and are interpreted as in `substring'.
     *
     * Width of each character in STRING is generally taken according to
     * `char-width', but character compositions and the display table in
     * effect are taken into consideration.
     * Tabs in STRING are always assumed to occupy `tab-width' columns,
     * although they might take fewer columns depending on the column where
     * they begin on display.
     * The effect of faces and fonts, including fonts used for non-Latin and
     * other unusual characters, such as emoji, is ignored, as are display
     * properties and invisible text.
     *
     * For these reasons, the results are just an approximation, especially
     * on GUI frames; for accurate dimensions of text as it will be
     * displayed, use `string-pixel-width' or `window-text-pixel-size'
     * instead.
     * usage: (string-width STRING &amp;optional FROM TO)
     * </pre>
     */
    @ELispBuiltIn(name = "string-width", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FStringWidth extends ELispBuiltInBaseNode {
        @Specialization
        public static Void stringWidth(Object string, Object from, Object to) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Concatenate all the argument characters and make the result a string.
     * usage: (string &amp;rest CHARACTERS)
     * </pre>
     */
    @ELispBuiltIn(name = "string", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FString extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString string(Object[] characters) {
            MuleStringBuffer builder = new MuleStringBuffer();
            for (Object c : characters) {
                builder.append(asInt(c));
            }
            return new ELispString(builder.build());
        }
    }

    /**
     * <pre>
     * Concatenate all the argument bytes and make the result a unibyte string.
     * usage: (unibyte-string &amp;rest BYTES)
     * </pre>
     */
    @ELispBuiltIn(name = "unibyte-string", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FUnibyteString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unibyteString(Object[] bytes) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Resolve modifiers in the character CHAR.
     * The value is a character with modifiers resolved into the character
     * code.  Unresolved modifiers are kept in the value.
     * usage: (char-resolve-modifiers CHAR)
     * </pre>
     */
    @ELispBuiltIn(name = "char-resolve-modifiers", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharResolveModifiers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void charResolveModifiers(Object char_) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a byte value of a character at point.
     * Optional 1st arg POSITION, if non-nil, is a position of a character to get
     * a byte value.
     * Optional 2nd arg STRING, if non-nil, is a string of which first
     * character is a target to get a byte value.  In this case, POSITION, if
     * non-nil, is an index of a target character in the string.
     *
     * If the current buffer (or STRING) is multibyte, and the target
     * character is not ASCII nor 8-bit character, an error is signaled.
     * </pre>
     */
    @ELispBuiltIn(name = "get-byte", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetByte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getByte(Object position, Object string) {
            throw new UnsupportedOperationException();
        }
    }
}
