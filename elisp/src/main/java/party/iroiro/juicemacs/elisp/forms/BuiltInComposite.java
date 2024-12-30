package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInComposite extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCompositeFactory.getFactories();
    }

    /**
     * <pre>
     * Internal use only.
     * Clear composition cache.
     * </pre>
     */
    @ELispBuiltIn(name = "clear-composition-cache", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FClearCompositionCache extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean clearCompositionCache() {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return a glyph-string for characters between FROM and TO.
     * If the glyph string is for graphic display, FONT-OBJECT must be
     * a font-object to use for those characters.
     * Otherwise (for terminal display), FONT-OBJECT must be a terminal ID, a
     * frame, or nil for the selected frame's terminal device.
     *
     * If the optional 4th argument STRING is not nil, it is a string
     * containing the target characters between indices FROM and TO,
     * which are treated as in `substring'.  Otherwise FROM and TO are
     * character positions in current buffer; they can be in either order,
     * and can be integers or markers.
     *
     * A glyph-string is a vector containing information about how to display
     * a specific character sequence.  The format is:
     *    [HEADER ID GLYPH ...]
     *
     * HEADER is a vector of this form:
     *     [FONT-OBJECT CHAR ...]
     * where
     *     FONT-OBJECT is a font-object for all glyphs in the glyph-string,
     *     or the terminal coding system of the specified terminal.
     *     CHARs are characters to be composed by GLYPHs.
     *
     * ID is an identification number of the glyph-string.  It may be nil if
     * not yet shaped.
     *
     * GLYPH is a vector whose elements have this form:
     *     [ FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT
     *       [ [X-OFF Y-OFF WADJUST] | nil] ]
     * where
     *     FROM-IDX and TO-IDX are used internally and should not be touched.
     *     C is the character of the glyph.
     *     CODE is the glyph-code of C in FONT-OBJECT.
     *     WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
     *     X-OFF and Y-OFF are offsets to the base position for the glyph.
     *     WADJUST is the adjustment to the normal width of the glyph.
     *
     * If GLYPH is nil, the remaining elements of the glyph-string vector
     * should be ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "composition-get-gstring", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FCompositionGetGstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compositionGetGstring(Object from, Object to, Object fontObject, Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     *
     * Compose text in the region between START and END.
     * Optional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC
     * for the composition.  See `compose-region' for more details.
     * </pre>
     */
    @ELispBuiltIn(name = "compose-region-internal", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FComposeRegionInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void composeRegionInternal(Object start, Object end, Object components, Object modificationFunc) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     *
     * Compose text between indices START and END of STRING, where
     * START and END are treated as in `substring'.  Optional 4th
     * and 5th arguments are COMPONENTS and MODIFICATION-FUNC
     * for the composition.  See `compose-string' for more details.
     * </pre>
     */
    @ELispBuiltIn(name = "compose-string-internal", minArgs = 3, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FComposeStringInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void composeStringInternal(Object string, Object start, Object end, Object components, Object modificationFunc) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     *
     * Return information about composition at or nearest to position POS.
     * See `find-composition' for more details.
     * </pre>
     */
    @ELispBuiltIn(name = "find-composition-internal", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FFindCompositionInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void findCompositionInternal(Object pos, Object limit, Object string, Object detailP) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Sort composition RULES by their LOOKBACK parameter.
     *
     * If RULES include just one rule, return RULES.
     * Otherwise, return a new list of rules where all the rules are
     * arranged in decreasing order of the LOOKBACK parameter of the
     * rules (the second element of the rule's vector).  This is required
     * when combining composition rules from different sources, because
     * of the way buffer text is examined for matching one of the rules.
     * </pre>
     */
    @ELispBuiltIn(name = "composition-sort-rules", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCompositionSortRules extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compositionSortRules(Object rules) {
            throw new UnsupportedOperationException();
        }
    }
}
