package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInTextProp extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInTextPropFactory.getFactories();
    }

    /**
     * <pre>
     * Return the list of properties of the character at POSITION in OBJECT.
     * If the optional second argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     *
     * If OBJECT is a string, POSITION is a 0-based index into it.
     *
     * If POSITION is at the end of OBJECT, the value is nil, but note that
     * buffer narrowing does not affect the value.  That is, if OBJECT is a
     * buffer or nil, and the buffer is narrowed and POSITION is at the end
     * of the narrowed buffer, the result may be non-nil.
     *
     * If you want to display the text properties at point in a human-readable
     * form, use the `describe-text-properties' command.
     * </pre>
     */
    @ELispBuiltIn(name = "text-properties-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTextPropertiesAt extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean textPropertiesAt(Object position, Object object) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the value of POSITION's property PROP, in OBJECT.
     * OBJECT should be a buffer or a string; if omitted or nil, it defaults
     * to the current buffer.
     *
     * If POSITION is at the end of OBJECT, the value is nil, but note that
     * buffer narrowing does not affect the value.  That is, if the buffer is
     * narrowed and POSITION is at the end of the narrowed buffer, the result
     * may be non-nil.
     * </pre>
     */
    @ELispBuiltIn(name = "get-text-property", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGetTextProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean getTextProperty(Object position, Object prop, Object object) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the value of POSITION's property PROP, in OBJECT.
     * Both overlay properties and text properties are checked.
     * OBJECT is optional and defaults to the current buffer.
     * If POSITION is at the end of OBJECT, the value is nil.
     * If OBJECT is a buffer, then overlay properties are considered as well as
     * text properties.
     * If OBJECT is a window, then that window's buffer is used, but window-specific
     * overlays are considered only if they are associated with OBJECT.
     * </pre>
     */
    @ELispBuiltIn(name = "get-char-property", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGetCharProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean getCharProperty(Object position, Object prop, Object object) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Like `get-char-property', but with extra overlay information.
     * The value is a cons cell.  Its car is the return value of `get-char-property'
     * with the same arguments--that is, the value of POSITION's property
     * PROP in OBJECT.  Its cdr is the overlay in which the property was
     * found, or nil, if it was found as a text property or not found at all.
     *
     * OBJECT is optional and defaults to the current buffer.  OBJECT may be
     * a string, a buffer or a window.  For strings, the cdr of the return
     * value is always nil, since strings do not have overlays.  If OBJECT is
     * a window, then that window's buffer is used, but window-specific
     * overlays are considered only if they are associated with OBJECT.  If
     * POSITION is at the end of OBJECT, both car and cdr are nil.
     * </pre>
     */
    @ELispBuiltIn(name = "get-char-property-and-overlay", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGetCharPropertyAndOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getCharPropertyAndOverlay(Object position, Object prop, Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of next text property or overlay change.
     * This scans characters forward in the current buffer from POSITION till
     * it finds a change in some text property, or the beginning or end of an
     * overlay, and returns the position of that.
     * If none is found, and LIMIT is nil or omitted, the function
     * returns (point-max).
     *
     * If the optional second argument LIMIT is non-nil, the function doesn't
     * search past position LIMIT, and returns LIMIT if nothing is found
     * before LIMIT.  LIMIT is a no-op if it is greater than (point-max).
     * </pre>
     */
    @ELispBuiltIn(name = "next-char-property-change", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNextCharPropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nextCharPropertyChange(Object position, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of previous text property or overlay change.
     * Scans characters backward in the current buffer from POSITION till it
     * finds a change in some text property, or the beginning or end of an
     * overlay, and returns the position of that.
     * If none is found, and LIMIT is nil or omitted, the function
     * returns (point-min).
     *
     * If the optional second argument LIMIT is non-nil, the function doesn't
     * search before position LIMIT, and returns LIMIT if nothing is found
     * before LIMIT.  LIMIT is a no-op if it is less than (point-min).
     * </pre>
     */
    @ELispBuiltIn(name = "previous-char-property-change", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FPreviousCharPropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousCharPropertyChange(Object position, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of next text property or overlay change for a specific property.
     * Scans characters forward from POSITION till it finds
     * a change in the PROP property, then returns the position of the change.
     * If the optional third argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     * If OBJECT is a string, POSITION is a 0-based index into it.
     *
     * In a string, scan runs to the end of the string, unless LIMIT is non-nil.
     * In a buffer, scan runs to end of buffer, unless LIMIT is non-nil.
     * If the optional fourth argument LIMIT is non-nil, don't search
     * past position LIMIT; return LIMIT if nothing is found before LIMIT.
     * However, if OBJECT is a buffer and LIMIT is beyond the end of the
     * buffer, this function returns `point-max', not LIMIT.
     *
     * The property values are compared with `eq'.
     * </pre>
     */
    @ELispBuiltIn(name = "next-single-char-property-change", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FNextSingleCharPropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public long nextSingleCharPropertyChange(Object position, Object prop, Object object, Object limit) {
            // TODO
            if (object instanceof ELispString s) {
                return s.length();
            }
            ELispBuffer buffer = isNil(object) ? getContext().currentBuffer() : asBuffer(object);
            return buffer.pointMax();
        }
    }

    /**
     * <pre>
     * Return the position of previous text property or overlay change for a specific property.
     * Scans characters backward from POSITION till it finds
     * a change in the PROP property, then returns the position of the change.
     * If the optional third argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     * If OBJECT is a string, POSITION is a 0-based index into it.
     *
     * In a string, scan runs to the start of the string, unless LIMIT is non-nil.
     * In a buffer, if LIMIT is nil or omitted, it runs to (point-min), and the
     * value cannot be less than that.
     * If the optional fourth argument LIMIT is non-nil, don't search back past
     * position LIMIT; return LIMIT if nothing is found before reaching LIMIT.
     *
     * The property values are compared with `eq'.
     * If the property is constant all the way to the start of OBJECT, return the
     * first valid position in OBJECT.
     * </pre>
     */
    @ELispBuiltIn(name = "previous-single-char-property-change", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPreviousSingleCharPropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousSingleCharPropertyChange(Object position, Object prop, Object object, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of next property change.
     * Scans characters forward from POSITION in OBJECT till it finds
     * a change in some text property, then returns the position of the change.
     * If the optional second argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     * If OBJECT is a string, POSITION is a 0-based index into it.
     * Return nil if LIMIT is nil or omitted, and the property is constant all
     * the way to the end of OBJECT; if the value is non-nil, it is a position
     * greater than POSITION, never equal.
     *
     * If the optional third argument LIMIT is non-nil, don't search
     * past position LIMIT; return LIMIT if nothing is found before LIMIT.
     * </pre>
     */
    @ELispBuiltIn(name = "next-property-change", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FNextPropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nextPropertyChange(long position, Object object, Object limit) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the position of next property change for a specific property.
     * Scans characters forward from POSITION till it finds
     * a change in the PROP property, then returns the position of the change.
     * If the optional third argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     * If OBJECT is a string, POSITION is a 0-based index into it.
     * The property values are compared with `eq'.
     * Return nil if LIMIT is nil or omitted, and the property is constant all
     * the way to the end of OBJECT; if the value is non-nil, it is a position
     * greater than POSITION, never equal.
     *
     * If the optional fourth argument LIMIT is non-nil, don't search
     * past position LIMIT; return LIMIT if nothing is found before LIMIT.
     * </pre>
     */
    @ELispBuiltIn(name = "next-single-property-change", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FNextSinglePropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nextSinglePropertyChange(Object position, Object prop, Object object, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of previous property change.
     * Scans characters backwards from POSITION in OBJECT till it finds
     * a change in some text property, then returns the position of the change.
     * If the optional second argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     * If OBJECT is a string, POSITION is a 0-based index into it.
     * Return nil if LIMIT is nil or omitted, and the property is constant all
     * the way to the start of OBJECT; if the value is non-nil, it is a position
     * less than POSITION, never equal.
     *
     * If the optional third argument LIMIT is non-nil, don't search
     * back past position LIMIT; return LIMIT if nothing is found until LIMIT.
     * </pre>
     */
    @ELispBuiltIn(name = "previous-property-change", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPreviousPropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousPropertyChange(Object position, Object object, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of previous property change for a specific property.
     * Scans characters backward from POSITION till it finds
     * a change in the PROP property, then returns the position of the change.
     * If the optional third argument OBJECT is a buffer (or nil, which means
     * the current buffer), POSITION is a buffer position (integer or marker).
     * If OBJECT is a string, POSITION is a 0-based index into it.
     * The property values are compared with `eq'.
     * Return nil if LIMIT is nil or omitted, and the property is constant all
     * the way to the start of OBJECT; if the value is non-nil, it is a position
     * less than POSITION, never equal.
     *
     * If the optional fourth argument LIMIT is non-nil, don't search
     * back past position LIMIT; return LIMIT if nothing is found until LIMIT.
     * </pre>
     */
    @ELispBuiltIn(name = "previous-single-property-change", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPreviousSinglePropertyChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void previousSinglePropertyChange(Object position, Object prop, Object object, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Add properties to the text from START to END.
     * The third argument PROPERTIES is a property list
     * specifying the property values to add.  If the optional fourth argument
     * OBJECT is a buffer (or nil, which means the current buffer),
     * START and END are buffer positions (integers or markers).
     * If OBJECT is a string, START and END are 0-based indices into it.
     * Return t if any property value actually changed, nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "add-text-properties", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FAddTextProperties extends ELispBuiltInBaseNode {
        @Specialization
        public Object addTextProperties(long start, long end, Object properties, Object object) {
            ELispBuffer buffer = isNil(object) ? getContext().currentBuffer() : asBuffer(object);
            ConsIterator i = asConsIter(properties);
            while (i.hasNext()) {
                Object property = i.next();
                Object value = i.next();
                buffer.putProperty(start, end, property, value);
            }
            return properties;
        }
    }

    /**
     * <pre>
     * Set one property of the text from START to END.
     * The third and fourth arguments PROPERTY and VALUE
     * specify the property to add.
     * If the optional fifth argument OBJECT is a buffer (or nil, which means
     * the current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices into it.
     * </pre>
     */
    @ELispBuiltIn(name = "put-text-property", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FPutTextProperty extends ELispBuiltInBaseNode {
        @Specialization
        public Object putTextProperty(long start, long end, Object property, Object value, Object object) {
            ELispBuffer buffer = isNil(object) ? getContext().currentBuffer() : asBuffer(object);
            buffer.putProperty(start, end, property, value);
            return value;
        }
    }

    /**
     * <pre>
     * Completely replace properties of text from START to END.
     * The third argument PROPERTIES is the new property list.
     * If the optional fourth argument OBJECT is a buffer (or nil, which means
     * the current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices into it.
     * If PROPERTIES is nil, the effect is to remove all properties from
     * the designated part of OBJECT.
     * </pre>
     */
    @ELispBuiltIn(name = "set-text-properties", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetTextProperties extends ELispBuiltInBaseNode {
        @Specialization
        public Object setTextProperties(long start, long end, Object properties, Object object) {
            ELispBuffer buffer = isNil(object) ? getContext().currentBuffer() : asBuffer(object);
            buffer.setProperties(start, end, properties);
            return properties;
        }
    }

    /**
     * <pre>
     * Add the face property to the text from START to END.
     * FACE specifies the face to add.  It should be a valid value of the
     * `face' property (typically a face name or a plist of face attributes
     * and values).
     *
     * If any text in the region already has a non-nil `face' property, those
     * face(s) are retained.  This is done by setting the `face' property to
     * a list of faces, with FACE as the first element (by default) and the
     * pre-existing faces as the remaining elements.
     *
     * If optional fourth argument APPEND is non-nil, append FACE to the end
     * of the face list instead.
     *
     * If optional fifth argument OBJECT is a buffer (or nil, which means the
     * current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices
     * into it.
     * </pre>
     */
    @ELispBuiltIn(name = "add-face-text-property", minArgs = 3, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FAddFaceTextProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static Void addFaceTextProperty(Object start, Object end, Object face, Object append, Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Remove some properties from text from START to END.
     * The third argument PROPERTIES is a property list
     * whose property names specify the properties to remove.
     * \(The values stored in PROPERTIES are ignored.)
     * If the optional fourth argument OBJECT is a buffer (or nil, which means
     * the current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices into it.
     * Return t if any property was actually removed, nil otherwise.
     *
     * Use `set-text-properties' if you want to remove all text properties.
     * </pre>
     */
    @ELispBuiltIn(name = "remove-text-properties", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FRemoveTextProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void removeTextProperties(Object start, Object end, Object properties, Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Remove some properties from text from START to END.
     * The third argument LIST-OF-PROPERTIES is a list of property names to remove.
     * If the optional fourth argument OBJECT is a buffer (or nil, which means
     * the current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices into it.
     * Return t if any property was actually removed, nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "remove-list-of-text-properties", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FRemoveListOfTextProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void removeListOfTextProperties(Object start, Object end, Object listOfProperties, Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Check text from START to END for property PROPERTY equaling VALUE.
     * If so, return the position of the first character whose property PROPERTY
     * is `eq' to VALUE.  Otherwise return nil.
     * If the optional fifth argument OBJECT is a buffer (or nil, which means
     * the current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices into it.
     * </pre>
     */
    @ELispBuiltIn(name = "text-property-any", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FTextPropertyAny extends ELispBuiltInBaseNode {
        @Specialization
        public static Void textPropertyAny(Object start, Object end, Object property, Object value, Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Check text from START to END for property PROPERTY not equaling VALUE.
     * If so, return the position of the first character whose property PROPERTY
     * is not `eq' to VALUE.  Otherwise, return nil.
     * If the optional fifth argument OBJECT is a buffer (or nil, which means
     * the current buffer), START and END are buffer positions (integers or
     * markers).  If OBJECT is a string, START and END are 0-based indices into it.
     * </pre>
     */
    @ELispBuiltIn(name = "text-property-not-all", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FTextPropertyNotAll extends ELispBuiltInBaseNode {
        @Specialization
        public Object textPropertyNotAll(long start, long end, Object property, Object value, Object object) {
            if (isNil(object)) {
                object = getContext().currentBuffer();
            }
            long offset = start;
            long len = end - start;
            @Nullable IntervalPieceTree<Object> tree;
            if (object instanceof ELispString s) {
                tree = s.getIntervals();
            } else {
                tree = asBuffer(object).getIntervals();
                offset--;
            }
            if (tree == null) {
                return isNil(value) ? false : start;
            }
            @Nullable Long diffStart = tree.forPropertiesIn(
                    offset, len, true,
                    (properties, propsStart, _) -> {
                        Object actual = BuiltInData.FCdrSafe.cdrSafe(
                                BuiltInFns.FPlistMember.plistMemberEq(properties == null ? false : properties, property)
                        );
                        if (!BuiltInData.FEq.eq(actual, value)) {
                            return propsStart;
                        }
                        return null;
                    }
            );
            if (diffStart == null) {
                return false;
            }
            return diffStart + (start - offset);
        }
    }
}
