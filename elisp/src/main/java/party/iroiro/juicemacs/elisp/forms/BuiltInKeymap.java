package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.Iterator;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInFns.iterateSequence;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.KEYMAP;

public class BuiltInKeymap extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInKeymapFactory.getFactories();
    }

    @CompilerDirectives.TruffleBoundary
    public static void keymapSet(ELispCons keymap, Iterator<?> iterator, Object value) {
        Object next = iterator.next();
        if (next instanceof ELispSymbol symbol) {
            // TODO: What does this do?
            System.out.println(symbol);
            return;
        }
        long c = ((Number) next).longValue();
        if (keymap.cdr() instanceof ELispCons cdr && cdr.car() instanceof ELispCharTable charTable) {
            if (iterator.hasNext()) {
                Object nested = charTable.getChar((int) c);
                if (nested instanceof ELispCons cons) {
                    keymapSet(cons, iterator, value);
                } else {
                    ELispCons cons = new ELispCons(KEYMAP);
                    charTable.setChar((int) c, cons);
                    keymapSet(cons, iterator, value);
                }
            } else {
                charTable.setChar((int) c, value);
            }
        } else {
            // sparse keymap
            Object cell = BuiltInFns.FAssq.assq(c, keymap.cdr());
            if (cell instanceof ELispCons cons) {
                if (iterator.hasNext()) {
                    if (cons.cdr() instanceof ELispCons cdr) {
                        keymapSet(cdr, iterator, value);
                    } else {
                        ELispCons cdr = new ELispCons(KEYMAP);
                        cons.setCdr(cdr);
                        keymapSet(cdr, iterator, value);
                    }
                } else {
                    cons.setCdr(value);
                }
            } else {
                if (iterator.hasNext()) {
                    ELispCons nested = new ELispCons(KEYMAP);
                    keymap.insertAfter(new ELispCons(c, nested));
                    keymapSet(nested, iterator, value);
                } else {
                    keymap.insertAfter(new ELispCons(c, value));
                }
            }
        }
    }

    /**
     * <pre>
     * Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
     * CHARTABLE is a char-table that holds the bindings for all characters
     * without modifiers.  All entries in it are initially nil, meaning
     * "command undefined".  ALIST is an assoc-list which holds bindings for
     * function keys, mouse events, and any other things that appear in the
     * input stream.  Initially, ALIST is nil.
     *
     * The optional arg STRING supplies a menu name for the keymap
     * in case you use it as a menu with `x-popup-menu'.
     * </pre>
     */
    @ELispBuiltIn(name = "make-keymap", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeKeymap extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCons makeKeymap(Object string) {
            return new ELispCons(
                    KEYMAP,
                    new ELispCons(
                            BuiltInCharTab.FMakeCharTable.makeCharTable(KEYMAP, false),
                            ELispSymbol.isNil(string) ? false : new ELispCons(string)
                    )
            );
        }
    }

    /**
     * <pre>
     * Construct and return a new sparse keymap.
     * Its car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),
     * which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),
     * which binds the function key or mouse event SYMBOL to DEFINITION.
     * Initially the alist is nil.
     *
     * The optional arg STRING supplies a menu name for the keymap
     * in case you use it as a menu with `x-popup-menu'.
     * </pre>
     */
    @ELispBuiltIn(name = "make-sparse-keymap", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeSparseKeymap extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCons makeSparseKeymap(Object string) {
            return new ELispCons(
                    KEYMAP,
                    ELispSymbol.isNil(string) ? false : new ELispCons(string)
            );
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a keymap.
     *
     * A keymap is a list (keymap . ALIST),
     * or a symbol whose function definition is itself a keymap.
     * ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);
     * a vector of densely packed bindings for small character codes
     * is also allowed as an element.
     * </pre>
     */
    @ELispBuiltIn(name = "keymapp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKeymapp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean keymapp(Object object) {
            // TODO: Autoload and function values
            return object instanceof ELispCons cons && cons.car() == KEYMAP;
        }
    }

    /**
     * <pre>
     * Return the prompt-string of a keymap MAP.
     * If non-nil, the prompt is shown in the echo-area
     * when reading a key-sequence to be looked-up in this keymap.
     * </pre>
     */
    @ELispBuiltIn(name = "keymap-prompt", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKeymapPrompt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void keymapPrompt(Object map) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parent keymap of KEYMAP.
     * If KEYMAP has no parent, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "keymap-parent", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKeymapParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object keymapParent(ELispCons keymap) {
            ELispCons.BrentTortoiseHareIterator i = keymap.listIterator(1);
            while (i.hasNext()) {
                Object next = i.next();
                if (FKeymapp.keymapp(next)) {
                    return next;
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Modify KEYMAP to set its parent map to PARENT.
     * Return PARENT.  PARENT should be nil or another keymap.
     * </pre>
     */
    @ELispBuiltIn(name = "set-keymap-parent", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetKeymapParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setKeymapParent(ELispCons keymap, Object parent) {
            ELispCons.BrentTortoiseHareIterator i = keymap.listIterator(1);
            ELispCons prev = keymap;
            while (i.hasNext()) {
                if (FKeymapp.keymapp(i.currentCons().car())) {
                    i.currentCons().setCar(parent);
                    return parent;
                }
                prev = i.currentCons();
                i.next();
            }
            prev.setCdr(new ELispCons(parent));
            return parent;
        }
    }

    /**
     * <pre>
     * Call FUNCTION once for each event binding in KEYMAP.
     * FUNCTION is called with two arguments: the event that is bound, and
     * the definition it is bound to.  The event may be a character range.
     * If KEYMAP has a parent, this function returns it without processing it.
     * </pre>
     */
    @ELispBuiltIn(name = "map-keymap-internal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapKeymapInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mapKeymapInternal(Object function, Object keymap) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Call FUNCTION once for each event binding in KEYMAP.
     * FUNCTION is called with two arguments: the event that is bound, and
     * the definition it is bound to.  The event may be a character range.
     *
     * If KEYMAP has a parent, the parent's bindings are included as well.
     * This works recursively: if the parent has itself a parent, then the
     * grandparent's bindings are also included and so on.
     *
     * For more information, see Info node `(elisp) Keymaps'.
     *
     * usage: (map-keymap FUNCTION KEYMAP)
     * </pre>
     */
    @ELispBuiltIn(name = "map-keymap", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMapKeymap extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mapKeymap(Object function, Object keymap, Object sortFirst) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Given OBJECT which was found in a slot in a keymap,
     * trace indirect definitions to get the actual definition of that slot.
     * An indirect definition is a list of the form
     * (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
     * and INDEX is the object to look up in KEYMAP to yield the definition.
     *
     * Also if OBJECT has a menu string as the first element,
     * remove that.  Also remove a menu help string as second element.
     *
     * If AUTOLOAD, load autoloadable keymaps
     * that are referred to with indirection.
     * </pre>
     */
    @ELispBuiltIn(name = "keymap--get-keyelt", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FKeymapGetKeyelt extends ELispBuiltInBaseNode {
        @Specialization
        public static Void keymapGetKeyelt(Object object, Object autoload) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a copy of the keymap KEYMAP.
     *
     * Note that this is almost never needed.  If you want a keymap that's like
     * another yet with a few changes, you should use keymap inheritance rather
     * than copying.  That is, something like:
     *
     *     (defvar-keymap foo-map
     *       :parent &lt;theirmap&gt;
     *       ...)
     *
     * Or, if you need to support Emacs versions older than 29:
     *
     *     (let ((map (make-sparse-keymap)))
     *       (set-keymap-parent map &lt;theirmap&gt;)
     *       (define-key map ...)
     *       ...)
     *
     * After performing `copy-keymap', the copy starts out with the same definitions
     * of KEYMAP, but changing either the copy or KEYMAP does not affect the other.
     * Any key definitions that are subkeymaps are recursively copied.
     * However, a key definition which is a symbol whose definition is a keymap
     * is not copied.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-keymap", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopyKeymap extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copyKeymap(Object keymap) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * In KEYMAP, define key sequence KEY as DEF.
     * This is a legacy function; see `keymap-set' for the recommended
     * function to use instead.
     *
     * KEYMAP is a keymap.
     *
     * KEY is a string or a vector of symbols and characters, representing a
     * sequence of keystrokes and events.  Non-ASCII characters with codes
     * above 127 (such as ISO Latin-1) can be represented by vectors.
     * Two types of vector have special meanings:
     *  [remap COMMAND] remaps any key binding for COMMAND.
     *  [t] creates a default definition, which applies to any event with no
     *     other definition in KEYMAP.
     *
     * DEF is anything that can be a key's definition:
     *  nil (means key is undefined in this keymap),
     *  a command (a Lisp function suitable for interactive calling),
     *  a string (treated as a keyboard macro),
     *  a keymap (to define a prefix key),
     *  a symbol (when the key is looked up, the symbol will stand for its
     *     function definition, which should at that time be one of the above,
     *     or another symbol whose function definition is used, etc.),
     *  a cons (STRING . DEFN), meaning that DEFN is the definition
     *     (DEFN should be a valid definition in its own right) and
     *     STRING is the menu item name (which is used only if the containing
     *     keymap has been created with a menu name, see `make-keymap'),
     *  or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
     *  or an extended menu item definition.
     *  (See info node `(elisp)Extended Menu Items'.)
     *
     * If REMOVE is non-nil, the definition will be removed.  This is almost
     * the same as setting the definition to nil, but makes a difference if
     * the KEYMAP has a parent, and KEY is shadowing the same binding in the
     * parent.  With REMOVE, subsequent lookups will return the binding in
     * the parent, and with a nil DEF, the lookups will return nil.
     *
     * If KEYMAP is a sparse keymap with a binding for KEY, the existing
     * binding is altered.  If there is no binding for KEY, the new pair
     * binding KEY to DEF is added at the front of KEYMAP.
     * </pre>
     */
    @ELispBuiltIn(name = "define-key", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FDefineKey extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean defineKey(ELispCons keymap, Object key, Object def, Object remove) {
            Iterator<?> i = iterateSequence(key);
            keymapSet(keymap, i, def);
            return false;
        }
    }

    /**
     * <pre>
     * Return the remapping for command COMMAND.
     * Returns nil if COMMAND is not remapped (or not a symbol).
     *
     * If the optional argument POSITION is non-nil, it specifies a mouse
     * position as returned by `event-start' and `event-end', and the
     * remapping occurs in the keymaps associated with it.  It can also be a
     * number or marker, in which case the keymap properties at the specified
     * buffer position instead of point are used.  The KEYMAPS argument is
     * ignored if POSITION is non-nil.
     *
     * If the optional argument KEYMAPS is non-nil, it should be a keymap or list of
     * keymaps to search for command remapping.  Otherwise, search for the
     * remapping in all currently active keymaps.
     * </pre>
     */
    @ELispBuiltIn(name = "command-remapping", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FCommandRemapping extends ELispBuiltInBaseNode {
        @Specialization
        public static Void commandRemapping(Object command, Object position, Object keymaps) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Look up key sequence KEY in KEYMAP.  Return the definition.
     * This is a legacy function; see `keymap-lookup' for the recommended
     * function to use instead.
     *
     * A value of nil means undefined.  See doc of `define-key'
     * for kinds of definitions.
     *
     * A number as value means KEY is "too long";
     * that is, characters or symbols in it except for the last one
     * fail to be a valid sequence of prefix characters in KEYMAP.
     * The number is how many characters at the front of KEY
     * it takes to reach a non-prefix key.
     * KEYMAP can also be a list of keymaps.
     *
     * Normally, `lookup-key' ignores bindings for t, which act as default
     * bindings, used when nothing else in the keymap applies; this makes it
     * usable as a general function for probing keymaps.  However, if the
     * third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
     * recognize the default bindings, just as `read-key-sequence' does.
     * </pre>
     */
    @ELispBuiltIn(name = "lookup-key", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FLookupKey extends ELispBuiltInBaseNode {
        @Specialization
        public static Void lookupKey(Object keymap, Object key, Object acceptDefault) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of the currently active keymaps.
     * OLP if non-nil indicates that we should obey `overriding-local-map' and
     * `overriding-terminal-local-map'.  POSITION can specify a click position
     * like in the respective argument of `key-binding'.
     * </pre>
     */
    @ELispBuiltIn(name = "current-active-maps", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCurrentActiveMaps extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentActiveMaps(Object olp, Object position) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the binding for command KEY in current keymaps.
     * KEY is a string or vector, a sequence of keystrokes.
     * The binding is probably a symbol with a function definition.
     *
     * Normally, `key-binding' ignores bindings for t, which act as default
     * bindings, used when nothing else in the keymap applies; this makes it
     * usable as a general function for probing keymaps.  However, if the
     * optional second argument ACCEPT-DEFAULT is non-nil, `key-binding' does
     * recognize the default bindings, just as `read-key-sequence' does.
     *
     * Like the normal command loop, `key-binding' will remap the command
     * resulting from looking up KEY by looking up the command in the
     * current keymaps.  However, if the optional third argument NO-REMAP
     * is non-nil, `key-binding' returns the unmapped command.
     *
     * If KEY is a key sequence initiated with the mouse, the used keymaps
     * will depend on the clicked mouse position with regard to the buffer
     * and possible local keymaps on strings.
     *
     * If the optional argument POSITION is non-nil, it specifies a mouse
     * position as returned by `event-start' and `event-end', and the lookup
     * occurs in the keymaps associated with it instead of KEY.  It can also
     * be a number or marker, in which case the keymap properties at the
     * specified buffer position instead of point are used.
     * </pre>
     */
    @ELispBuiltIn(name = "key-binding", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FKeyBinding extends ELispBuiltInBaseNode {
        @Specialization
        public static Void keyBinding(Object key, Object acceptDefault, Object noRemap, Object position) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Find the visible minor mode bindings of KEY.
     * Return an alist of pairs (MODENAME . BINDING), where MODENAME is
     * the symbol which names the minor mode binding KEY, and BINDING is
     * KEY's definition in that mode.  In particular, if KEY has no
     * minor-mode bindings, return nil.  If the first binding is a
     * non-prefix, all subsequent bindings will be omitted, since they would
     * be ignored.  Similarly, the list doesn't include non-prefix bindings
     * that come after prefix bindings.
     *
     * If optional argument ACCEPT-DEFAULT is non-nil, recognize default
     * bindings; see the description of `lookup-key' for more details about this.
     * </pre>
     */
    @ELispBuiltIn(name = "minor-mode-key-binding", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMinorModeKeyBinding extends ELispBuiltInBaseNode {
        @Specialization
        public static Void minorModeKeyBinding(Object key, Object acceptDefault) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Select KEYMAP as the global keymap.
     * </pre>
     */
    @ELispBuiltIn(name = "use-global-map", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUseGlobalMap extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean useGlobalMap(Object keymap) {
            // TODO: Set up global map reference
            return false;
        }
    }

    /**
     * <pre>
     * Select KEYMAP as the local keymap.
     * If KEYMAP is nil, that means no local keymap.
     * </pre>
     */
    @ELispBuiltIn(name = "use-local-map", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUseLocalMap extends ELispBuiltInBaseNode {
        @Specialization
        public static Void useLocalMap(Object keymap) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return current buffer's local keymap, or nil if it has none.
     * Normally the local keymap is set by the major mode with `use-local-map'.
     * </pre>
     */
    @ELispBuiltIn(name = "current-local-map", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentLocalMap extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentLocalMap() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current global keymap.
     * </pre>
     */
    @ELispBuiltIn(name = "current-global-map", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentGlobalMap extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentGlobalMap() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of keymaps for the minor modes of the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "current-minor-mode-maps", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentMinorModeMaps extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentMinorModeMaps() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Find all keymaps accessible via prefix characters from KEYMAP.
     * Returns a list of elements of the form (KEYS . MAP), where the sequence
     * KEYS starting from KEYMAP gets you to MAP.  These elements are ordered
     * so that the KEYS increase in length.  The first element is ([] . KEYMAP).
     * An optional argument PREFIX, if non-nil, should be a key sequence;
     * then the value includes only maps for prefixes that start with PREFIX.
     * </pre>
     */
    @ELispBuiltIn(name = "accessible-keymaps", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAccessibleKeymaps extends ELispBuiltInBaseNode {
        @Specialization
        public static Void accessibleKeymaps(Object keymap, Object prefix) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a pretty description of key-sequence KEYS.
     * Optional arg PREFIX is the sequence of keys leading up to KEYS.
     * For example, [?\\C-x ?l] is converted into the string \"C-x l\".
     *
     * For an approximate inverse of this, see `kbd'.
     * </pre>
     */
    @ELispBuiltIn(name = "key-description", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FKeyDescription extends ELispBuiltInBaseNode {
        @Specialization
        public static Void keyDescription(Object keys, Object prefix) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a pretty description of a character event KEY.
     * Control characters turn into C-whatever, etc.
     * Optional argument NO-ANGLES non-nil means don't put angle brackets
     * around function keys and event symbols.
     *
     * See `text-char-description' for describing character codes.
     * </pre>
     */
    @ELispBuiltIn(name = "single-key-description", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSingleKeyDescription extends ELispBuiltInBaseNode {
        @Specialization
        public static Void singleKeyDescription(Object key, Object noAngles) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the description of CHARACTER in standard Emacs notation.
     * CHARACTER must be a valid character code that passes the `characterp' test.
     * Control characters turn into "^char", and characters with Meta and other
     * modifiers signal an error, as they are not valid character codes.
     * This differs from `single-key-description' which accepts character events,
     * and thus doesn't enforce the `characterp' condition, turns control
     * characters into "C-char", and uses the 2**27 bit for Meta.
     * See Info node `(elisp)Describing Characters' for examples.
     * </pre>
     */
    @ELispBuiltIn(name = "text-char-description", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTextCharDescription extends ELispBuiltInBaseNode {
        @Specialization
        public static Void textCharDescription(Object character) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return list of keys that invoke DEFINITION.
     * If KEYMAP is a keymap, search only KEYMAP and the global keymap.
     * If KEYMAP is nil, search all the currently active keymaps, except
     *  for `overriding-local-map' (which is ignored).
     * If KEYMAP is a list of keymaps, search only those keymaps.
     *
     * If optional 3rd arg FIRSTONLY is non-nil, return the first key sequence found,
     * rather than a list of all possible key sequences.
     * If FIRSTONLY is the symbol `non-ascii', return the first binding found,
     * no matter what it is.
     * If FIRSTONLY has another non-nil value, prefer bindings
     * that use the modifier key specified in `where-is-preferred-modifier'
     * \(or their meta variants) and entirely reject menu bindings.
     *
     * If optional 4th arg NOINDIRECT is non-nil, don't extract the commands inside
     * menu-items.  This makes it possible to search for a menu-item itself.
     *
     * The optional 5th arg NO-REMAP alters how command remapping is handled:
     *
     * - If another command OTHER-COMMAND is remapped to DEFINITION, normally
     *   search for the bindings of OTHER-COMMAND and include them in the
     *   returned list.  But if NO-REMAP is non-nil, include the vector
     *   [remap OTHER-COMMAND] in the returned list instead, without
     *   searching for those other bindings.
     *
     * - If DEFINITION is remapped to OTHER-COMMAND, normally return the
     *   bindings for OTHER-COMMAND.  But if NO-REMAP is non-nil, return the
     *   bindings for DEFINITION instead, ignoring its remapping.
     *
     * Keys that are represented as events that have a `non-key-event' non-nil
     * symbol property are ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "where-is-internal", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FWhereIsInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void whereIsInternal(Object definition, Object keymap, Object firstonly, Object noindirect, Object noRemap) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert the list of all defined keys and their definitions.
     * The list is inserted in the current buffer, while the bindings are
     * looked up in BUFFER.
     * The optional argument PREFIX, if non-nil, should be a key sequence;
     * then we display only bindings that start with that prefix.
     * The optional argument MENUS, if non-nil, says to mention menu bindings.
     * \(Ordinarily these are omitted from the output.)
     * </pre>
     */
    @ELispBuiltIn(name = "describe-buffer-bindings", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDescribeBufferBindings extends ELispBuiltInBaseNode {
        @Specialization
        public static Void describeBufferBindings(Object buffer, Object prefix, Object menus) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert a description of contents of VECTOR.
     * This is text showing the elements of vector matched against indices.
     * DESCRIBER is the output function used; nil means use `princ'.
     * </pre>
     */
    @ELispBuiltIn(name = "describe-vector", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDescribeVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Void describeVector(Object vector, Object describer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert in the current buffer a description of the contents of VECTOR.
     * Call DESCRIBER to insert the description of one value found in VECTOR.
     *
     * PREFIX is a string describing the key which leads to the keymap that
     * this vector is in.
     *
     * If PARTIAL, it means do not mention suppressed commands.
     *
     * SHADOW is a list of keymaps that shadow this map.
     * If it is non-nil, look up the key in those maps and don't mention it
     * if it is defined by any of them.
     *
     * ENTIRE-MAP is the keymap in which this vector appears.
     * If the definition in effect in the whole map does not match
     * the one in this keymap, we ignore this one.
     * </pre>
     */
    @ELispBuiltIn(name = "help--describe-vector", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FHelpDescribeVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Void helpDescribeVector(Object vector, Object prefix, Object describer, Object partial, Object shadow, Object entireMap, Object mentionShadow) {
            throw new UnsupportedOperationException();
        }
    }
}
