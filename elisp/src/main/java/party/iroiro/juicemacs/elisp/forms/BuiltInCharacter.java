package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.FCompareStrings;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.string.CharIterator;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

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
            return object instanceof Long l && l <= FMaxChar.maxChar(false);
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
        public static long unibyteCharToMultibyte(long ch) {
            if (ch < 0 || 0xFF < ch) {
                throw ELispSignals.error("not a unibyte char");
            }
            if (ch < 0x7F) {
                return ch;
            }
            return ELispBuiltInConstants.MAX_5_BYTE_CHAR - 0x7F + ch;
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
        public static long multibyteCharToUnibyte(long ch) {
            if (ch < 0x100) {
                return ch;
            }
            if (0x3FFF80 <= ch && ch <= 0x3FFFFF) {
                return ch & 0xFF;
            }
            return -1;
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
        public static long charWidth(long char_) {
            asChar(char_);
            if (char_ == '\t') {
                return asLong(ELispContext.get(null).currentBuffer().getTabWidth());
            }
            if (char_ == '\n') {
                return 0;
            }
            // TODO
            return 1;
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
        public static long stringWidth(ELispString string, Object from, Object to) {
            int[] range = FCompareStrings.checkStringRange(string, from, to);
            int len = range[1] - range[0];
            CharIterator iterator = string.iterator(range[0]);
            long width = 0;
            while (iterator.hasNext() && len-- > 0) {
                width += FCharWidth.charWidth(iterator.nextInt());
            }
            return width;
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
            ELispString.Builder builder = new ELispString.Builder();
            for (Object c : characters) {
                builder.appendCodePoint(asChar(c));
            }
            return builder.build();
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
        public static ELispString unibyteString(Object[] bytes) {
            byte[] b = new byte[bytes.length];
            for (int i = 0; i < bytes.length; i++) {
                b[i] = (byte) asRanged(bytes[i], 0L, 255L);
            }
            return ELispString.ofBytes(b);
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
        /// @see party.iroiro.juicemacs.elisp.parser.ELispLexer
        @SuppressWarnings("OctalInteger")
        @Specialization
        public static long charResolveModifiers(long char_) {
            // from character.c: char_resolve_modifier_mask

            /* A non-ASCII character can't reflect modifier bits to the code.  */
            if ((char_ & ~CHAR_MODIFIER_MASK) >= 0x80) {
                return char_;
            }
            /* For Meta, Shift, and Control modifiers, we need special care.  */
            if ((char_ & CHAR_SHIFT) != 0) {
                /* Shift modifier is valid only with [A-Za-z].  */
                if ('A' <= (char_ & 0377) && (char_ & 0377) <= 'Z') {
                    char_ &= ~CHAR_SHIFT;
                } else if ('a' <= (char_ & 0377) && (char_ & 0377) <= 'z') {
                    char_ = (char_ & ~CHAR_SHIFT) - ('a' - 'A');
                } else if ((char_ & ~CHAR_MODIFIER_MASK) <= 0x20) {
                    /* Shift modifier for control characters and SPC is ignored.  */
                    char_ &= ~CHAR_SHIFT;
                }
            }
            if ((char_ & CHAR_CTL) != 0) {
                /* Simulate the code in lread.c.  */
                /* Allow `\C- ' and `\C-?'.  */
                if ((char_ & 0377) == ' ') {
                    char_ &= ~0177 & ~CHAR_CTL;
                } else if ((char_ & 0377) == '?') {
                    char_ = 0177 | (char_ & ~0177 & ~CHAR_CTL);
                }
                /* ASCII control chars are made from letters (both cases),
                   as well as the non-letters within 0100...0137.  */
                else if ((char_ & 0137) >= 0101 && (char_ & 0137) <= 0132) {
                    char_ &= (037 | (~0177 & ~CHAR_CTL));
                } else if ((char_ & 0177) >= 0100 && (char_ & 0177) <= 0137) {
                    char_ &= (037 | (~0177 & ~CHAR_CTL));
                }
            }
            return char_;
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
        public long getByte(Object position, Object string) {
            int c;
            if (isNil(string)) {
                ELispBuffer buffer = getContext().currentBuffer();
                c = buffer.getChar(isNil(position) ? buffer.getPoint() : asLong(position));
            } else {
                ELispString s = asStr(string);
                c = s.codePointAt(isNil(position) ? 0 : asInt(position));
            }
            if (c < 256) {
                return c;
            }
            if (c >= 0x3FFF80) {
                return c & 0xFF;
            }
            throw ELispSignals.error("Not an ASCII nor an 8-bit character");
        }
    }
}
