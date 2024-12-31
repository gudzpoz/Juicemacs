package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.UNSPECIFIED;

public class BuiltInXFaces extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInXFacesFactory.getFactories();
    }

    /**
     * <pre>
     * Dump currently allocated colors to stderr.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-colors", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDumpColors extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpColors() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Clear face caches on all frames.
     * Optional THOROUGHLY non-nil means try to free unused fonts, too.
     * </pre>
     */
    @ELispBuiltIn(name = "clear-face-cache", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClearFaceCache extends ELispBuiltInBaseNode {
        @Specialization
        public static Void clearFaceCache(Object thoroughly) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Value is non-nil if OBJECT is a valid bitmap specification.
     * A bitmap specification is either a string, a file name, or a list
     * \(WIDTH HEIGHT DATA) where WIDTH is the pixel width of the bitmap,
     * HEIGHT is its height, and DATA is a string containing the bits of
     * the pixmap.  Bits are stored row by row, each row occupies
     * \(WIDTH + 7)/8 bytes.
     * </pre>
     */
    @ELispBuiltIn(name = "bitmap-spec-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBitmapSpecP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bitmapSpecP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Parse color SPEC as a numeric color and return (RED GREEN BLUE).
     * This function recognizes the following formats for SPEC:
     *
     *  #RGB, where R, G and B are hex numbers of equal length, 1-4 digits each.
     *  rgb:R/G/B, where R, G, and B are hex numbers, 1-4 digits each.
     *  rgbi:R/G/B, where R, G and B are floating-point numbers in [0,1].
     *
     * If SPEC is not in one of the above forms, return nil.
     *
     * Each of the 3 integer members of the resulting list, RED, GREEN, and BLUE,
     * is normalized to have its value in [0,65535].
     * </pre>
     */
    @ELispBuiltIn(name = "color-values-from-color-spec", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FColorValuesFromColorSpec extends ELispBuiltInBaseNode {
        @Specialization
        public static Void colorValuesFromColorSpec(Object spec) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if COLOR is a shade of gray (or white or black).
     * FRAME specifies the frame and thus the display for interpreting COLOR.
     * If FRAME is nil or omitted, use the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "color-gray-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FColorGrayP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void colorGrayP(Object color, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if COLOR can be displayed on FRAME.
     * BACKGROUND-P non-nil means COLOR is used as a background.
     * Otherwise, this function tells whether it can be used as a foreground.
     * If FRAME is nil or omitted, use the selected frame.
     * COLOR must be a valid color name.
     * </pre>
     */
    @ELispBuiltIn(name = "color-supported-p", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FColorSupportedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void colorSupportedP(Object color, Object frame, Object backgroundP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of available fonts of family FAMILY on FRAME.
     * If FAMILY is omitted or nil, list all families.
     * Otherwise, FAMILY must be a string, possibly containing wildcards
     * `?' and `*'.
     * If FRAME is omitted or nil, use the selected frame.
     *
     * Each element of the result is a vector [FAMILY WIDTH POINT-SIZE WEIGHT
     * SLANT FIXED-P FULL REGISTRY-AND-ENCODING].
     *
     * FAMILY is the font family name.
     * POINT-SIZE is the size of the font in 1/10 pt.
     * WIDTH, WEIGHT, and SLANT are symbols describing the width, weight
     *   and slant of the font.  These symbols are the same as for face
     *   attributes, see `set-face-attribute'.
     * FIXED-P is non-nil if the font is fixed-pitch.
     * FULL is the full name of the font.
     * REGISTRY-AND-ENCODING is a string giving the registry and encoding of
     *   the font.
     *
     * The resulting list is sorted according to the current setting of
     * the face font sort order, see `face-font-selection-order'.
     * </pre>
     */
    @ELispBuiltIn(name = "x-family-fonts", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FXFamilyFonts extends ELispBuiltInBaseNode {
        @Specialization
        public static Void xFamilyFonts(Object family, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of the names of available fonts matching PATTERN.
     * If optional arguments FACE and FRAME are specified, return only fonts
     * the same size as FACE on FRAME.
     *
     * PATTERN should be a string containing a font name in the XLFD,
     * Fontconfig, or GTK format.  A font name given in the XLFD format may
     * contain wildcard characters:
     *   the * character matches any substring, and
     *   the ? character matches any single character.
     *   PATTERN is case-insensitive.
     *
     * The return value is a list of strings, suitable as arguments to
     * `set-face-font'.
     *
     * Fonts Emacs can't use may or may not be excluded
     * even if they match PATTERN and FACE.
     * The optional fourth argument MAXIMUM sets a limit on how many
     * fonts to match.  The first MAXIMUM fonts are reported.
     * The optional fifth argument WIDTH, if specified, is a number of columns
     * occupied by a character of a font.  In that case, return only fonts
     * the WIDTH times as wide as FACE on FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "x-list-fonts", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FXListFonts extends ELispBuiltInBaseNode {
        @Specialization
        public static Void xListFonts(Object pattern, Object face, Object frame, Object maximum, Object width) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make FACE, a symbol, a Lisp face with all attributes nil.
     * If FACE was not known as a face before, create a new one.
     * If optional argument FRAME is specified, make a frame-local face
     * for that frame.  Otherwise operate on the global face definition.
     * Value is a vector of face attributes.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-make-lisp-face", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalMakeLispFace extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalMakeLispFace(Object face, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if FACE names a face.
     * FACE should be a symbol or string.
     * If optional second argument FRAME is non-nil, check for the
     * existence of a frame-local face with name FACE on that frame.
     * Otherwise check for the existence of a global face.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-lisp-face-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalLispFaceP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean internalLispFaceP(Object face, Object frame) {
            // TODO: fonts?
            return true;
        }
    }

    /**
     * <pre>
     * Copy face FROM to TO.
     * If FRAME is t, copy the global face definition of FROM.
     * Otherwise, copy the frame-local definition of FROM on FRAME.
     * If NEW-FRAME is a frame, copy that data into the frame-local
     * definition of TO on NEW-FRAME.  If NEW-FRAME is nil,
     * FRAME controls where the data is copied to.
     *
     * The value is TO.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-copy-lisp-face", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FInternalCopyLispFace extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalCopyLispFace(Object from, Object to, Object frame, Object newFrame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set attribute ATTR of FACE to VALUE.
     * FRAME being a frame means change the face on that frame.
     * FRAME nil means change the face of the selected frame.
     * FRAME t means change the default for new frames.
     * FRAME 0 means change the face on all frames, and change the default
     *   for new frames.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-set-lisp-face-attribute", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FInternalSetLispFaceAttribute extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalSetLispFaceAttribute(Object face, Object attr, Object value, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Get the value of X resource RESOURCE, class CLASS.
     * Returned value is for the display of frame FRAME.  If FRAME is not
     * specified or nil, use selected frame.  This function exists because
     * ordinary `x-get-resource' doesn't take a frame argument.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-face-x-get-resource", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalFaceXGetResource extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalFaceXGetResource(Object resource, Object class_, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * </pre>
     */
    @ELispBuiltIn(name = "internal-set-lisp-face-attribute-from-resource", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FInternalSetLispFaceAttributeFromResource extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalSetLispFaceAttributeFromResource(Object face, Object attr, Object value, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Check whether a face attribute value is relative.
     * Specifically, this function returns t if the attribute ATTRIBUTE
     * with the value VALUE is relative.
     *
     * A relative value is one that doesn't entirely override whatever is
     * inherited from another face.  For most possible attributes,
     * the only relative value that users see is `unspecified'.
     * However, for :height, floating point values are also relative.
     * </pre>
     */
    @ELispBuiltIn(name = "face-attribute-relative-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFaceAttributeRelativeP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void faceAttributeRelativeP(Object attribute, Object value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return face ATTRIBUTE VALUE1 merged with VALUE2.
     * If VALUE1 or VALUE2 are absolute (see `face-attribute-relative-p'), then
     * the result will be absolute, otherwise it will be relative.
     * </pre>
     */
    @ELispBuiltIn(name = "merge-face-attribute", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMergeFaceAttribute extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mergeFaceAttribute(Object attribute, Object value1, Object value2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return face attribute KEYWORD of face SYMBOL.
     * If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid
     * face attribute name, signal an error.
     * If the optional argument FRAME is given, report on face SYMBOL in that
     * frame.  If FRAME is t, report on the defaults for face SYMBOL (for new
     * frames).  If FRAME is omitted or nil, use the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-get-lisp-face-attribute", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalGetLispFaceAttribute extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol internalGetLispFaceAttribute(ELispSymbol symbol, ELispSymbol keyword, Object frame) {
            // TODO
            return UNSPECIFIED;
        }
    }

    /**
     * <pre>
     * Return a list of valid discrete values for face attribute ATTR.
     * Value is nil if ATTR doesn't have a discrete set of valid values.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-lisp-face-attribute-values", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalLispFaceAttributeValues extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalLispFaceAttributeValues(Object attr) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Add attributes from frame-default definition of FACE to FACE on FRAME.
     * Default face attributes override any local face attributes.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-merge-in-global-face", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalMergeInGlobalFace extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalMergeInGlobalFace(Object face, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the font name of face FACE, or nil if it is unspecified.
     * The font name is, by default, for ASCII characters.
     * If the optional argument FRAME is given, report on face FACE in that frame.
     * If FRAME is t, report on the defaults for face FACE (for new frames).
     *   The font default for a face is either nil, or a list
     *   of the form (bold), (italic) or (bold italic).
     * If FRAME is omitted or nil, use the selected frame.
     * If FRAME is anything but t, and the optional third argument CHARACTER
     * is given, return the font name used by FACE for CHARACTER on FRAME.
     * </pre>
     */
    @ELispBuiltIn(name = "face-font", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFaceFont extends ELispBuiltInBaseNode {
        @Specialization
        public static Void faceFont(Object face, Object frame, Object character) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * True if FACE1 and FACE2 are equal.
     * If the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.
     * If FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).
     * If FRAME is omitted or nil, use the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-lisp-face-equal-p", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalLispFaceEqualP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalLispFaceEqualP(Object face1, Object face2, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * True if FACE has no attribute specified.
     * If the optional argument FRAME is given, report on face FACE in that frame.
     * If FRAME is t, report on the defaults for face FACE (for new frames).
     * If FRAME is omitted or nil, use the selected frame.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-lisp-face-empty-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalLispFaceEmptyP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalLispFaceEmptyP(Object face, Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a hash table of frame-local faces defined on FRAME.
     * For internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "frame--face-hash-table", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFrameFaceHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Void frameFaceHashTable(Object frame) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return an integer distance between COLOR1 and COLOR2 on FRAME.
     * COLOR1 and COLOR2 may be either strings containing the color name,
     * or lists of the form (RED GREEN BLUE), each in the range 0 to 65535 inclusive.
     * If FRAME is unspecified or nil, the current frame is used.
     * If METRIC is specified, it should be a function that accepts
     * two lists of the form (RED GREEN BLUE) aforementioned.
     * Despite the name, this is not a true distance metric as it does not satisfy
     * the triangle inequality.
     * </pre>
     */
    @ELispBuiltIn(name = "color-distance", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FColorDistance extends ELispBuiltInBaseNode {
        @Specialization
        public static Void colorDistance(Object color1, Object color2, Object frame, Object metric) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a vector of face attributes corresponding to PLIST.
     * </pre>
     */
    @ELispBuiltIn(name = "face-attributes-as-vector", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFaceAttributesAsVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Void faceAttributesAsVector(Object plist) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if all the face attributes in ATTRIBUTES are supported.
     * The optional argument DISPLAY can be a display name, a frame, or
     * nil (meaning the selected frame's display).
     *
     * For instance, to check whether the display supports underlining:
     *
     *   (display-supports-face-attributes-p \\='(:underline t))
     *
     * The definition of `supported' is somewhat heuristic, but basically means
     * that a face containing all the attributes in ATTRIBUTES, when merged
     * with the default face for display, can be represented in a way that's
     *
     *  (1) different in appearance from the default face, and
     *  (2) `close in spirit' to what the attributes specify, if not exact.
     *
     * Point (2) implies that a `:weight black' attribute will be satisfied by
     * any display that can display bold, and a `:foreground \"yellow\"' as long
     * as it can display a yellowish color, but `:slant italic' will _not_ be
     * satisfied by the tty display code's automatic substitution of a `dim'
     * face for italic.
     * </pre>
     */
    @ELispBuiltIn(name = "display-supports-face-attributes-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDisplaySupportsFaceAttributesP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void displaySupportsFaceAttributesP(Object attributes, Object display) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set font selection order for face font selection to ORDER.
     * ORDER must be a list of length 4 containing the symbols `:width',
     * `:height', `:weight', and `:slant'.  Face attributes appearing
     * first in ORDER are matched first, e.g. if `:height' appears before
     * `:weight' in ORDER, font selection first tries to find a font with
     * a suitable height, and then tries to match the font weight.
     * Value is ORDER.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-set-font-selection-order", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalSetFontSelectionOrder extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean internalSetFontSelectionOrder(Object order) {
            // TODO: fonts?
            return false;
        }
    }

    /**
     * <pre>
     * Define alternative font families to try in face font selection.
     * ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
     * Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can
     * be found.  Value is ALIST.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-set-alternative-font-family-alist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalSetAlternativeFontFamilyAlist extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean internalSetAlternativeFontFamilyAlist(Object alist) {
            // TODO: fonts?
            return false;
        }
    }

    /**
     * <pre>
     * Define alternative font registries to try in face font selection.
     * ALIST is an alist of (REGISTRY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
     * Each ALTERNATIVE is tried in order if no fonts of font registry REGISTRY can
     * be found.  Value is ALIST.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-set-alternative-font-registry-alist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalSetAlternativeFontRegistryAlist extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean internalSetAlternativeFontRegistryAlist(Object alist) {
            // TODO: fonts?
            return false;
        }
    }

    /**
     * <pre>
     * Suppress/allow boldness of faces with inverse default colors.
     * SUPPRESS non-nil means suppress it.
     * This affects bold faces on TTYs whose foreground is the default background
     * color of the display and whose background is the default foreground color.
     * For such faces, the bold face attribute is ignored if this variable
     * is non-nil.
     * </pre>
     */
    @ELispBuiltIn(name = "tty-suppress-bold-inverse-default-colors", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTtySuppressBoldInverseDefaultColors extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ttySuppressBoldInverseDefaultColors(Object suppress) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create an alist of color entries from an external file.
     *
     * The file should define one named RGB color per line like so:
     *   R G B   name
     * where R,G,B are numbers between 0 and 255 and name is an arbitrary string.
     * </pre>
     */
    @ELispBuiltIn(name = "x-load-color-file", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FXLoadColorFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void xLoadColorFile(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * </pre>
     */
    @ELispBuiltIn(name = "dump-face", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDumpFace extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpFace(Object n) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * </pre>
     */
    @ELispBuiltIn(name = "show-face-resources", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FShowFaceResources extends ELispBuiltInBaseNode {
        @Specialization
        public static Void showFaceResources() {
            throw new UnsupportedOperationException();
        }
    }
}
