package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.graalvm.polyglot.SandboxPolicy;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_CHAR;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.CASE_FOLD_SEARCH;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.SYSTEM_NAME;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInEditFns extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEditFnsFactory.getFactories();
    }

    /**
     * <pre>
     * Convert arg CHAR to a string containing that character.
     * usage: (char-to-string CHAR)
     * </pre>
     */
    @ELispBuiltIn(name = "char-to-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharToString extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString charToString(long char_) {
            return new MuleStringBuilder().appendCodePoint(asChar(char_)).buildString();
        }
    }

    /**
     * <pre>
     * Convert arg BYTE to a unibyte string containing that byte.
     * </pre>
     */
    @ELispBuiltIn(name = "byte-to-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FByteToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void byteToString(Object byte_) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the first character in STRING.
     * </pre>
     */
    @ELispBuiltIn(name = "string-to-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToChar extends ELispBuiltInBaseNode {
        @Specialization
        public static long stringToChar(ELispString string) {
            return string.codePointAt(0);
        }
    }

    /**
     * <pre>
     * Return value of point, as an integer.
     * Beginning of buffer is position (point-min).
     * </pre>
     */
    @ELispBuiltIn(name = "point", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPoint extends ELispBuiltInBaseNode {
        @Specialization
        public static long point() {
            return currentBuffer().getPoint();
        }
    }

    /**
     * <pre>
     * Return value of point, as a marker object.
     * </pre>
     */
    @ELispBuiltIn(name = "point-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMarker extends ELispBuiltInBaseNode {
        @Specialization
        public ELispMarker pointMarker() {
            ELispContext context = getContext();
            ELispBuffer buffer = context.currentBuffer();
            return new ELispMarker(buffer, buffer.getPoint());
        }
    }

    /**
     * <pre>
     * Set point to POSITION, a number or marker.
     * Beginning of buffer is position (point-min), end is (point-max).
     *
     * The return value is POSITION.
     *
     * If called interactively, a numeric prefix argument specifies
     * POSITION; without a numeric prefix argument, read POSITION from the
     * minibuffer.  The default value is the number at point (if any).
     * </pre>
     */
    @ELispBuiltIn(name = "goto-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGotoChar extends ELispBuiltInBaseNode {
        @Specialization
        public boolean gotoChar(long position) {
            getContext().currentBuffer().setPoint(position);
            return true;
        }
    }

    /**
     * <pre>
     * Return the integer value of point or mark, whichever is smaller.
     * </pre>
     */
    @ELispBuiltIn(name = "region-beginning", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRegionBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Void regionBeginning() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the integer value of point or mark, whichever is larger.
     * </pre>
     */
    @ELispBuiltIn(name = "region-end", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRegionEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void regionEnd() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return this buffer's mark, as a marker object.
     * Watch out!  Moving this marker changes the mark position.
     * If you set the marker not to point anywhere, the buffer will have no mark.
     * </pre>
     */
    @ELispBuiltIn(name = "mark-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMarkMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Void markMarker() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the value of POSITION's property PROP, in OBJECT.
     * Almost identical to `get-char-property' except for the following difference:
     * Whereas `get-char-property' returns the property of the char at (i.e. right
     * after) POSITION, this pays attention to properties's stickiness and overlays's
     * advancement settings, in order to find the property of POSITION itself,
     * i.e. the property that a char would inherit if it were inserted
     * at POSITION.
     * </pre>
     */
    @ELispBuiltIn(name = "get-pos-property", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGetPosProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static Void getPosProperty(Object position, Object prop, Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete the field surrounding POS.
     * A field is a region of text with the same `field' property.
     * If POS is nil, the value of point is used for POS.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-field", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteField extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteField(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the contents of the field surrounding POS as a string.
     * A field is a region of text with the same `field' property.
     * If POS is nil, the value of point is used for POS.
     * </pre>
     */
    @ELispBuiltIn(name = "field-string", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFieldString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fieldString(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the contents of the field around POS, without text properties.
     * A field is a region of text with the same `field' property.
     * If POS is nil, the value of point is used for POS.
     * </pre>
     */
    @ELispBuiltIn(name = "field-string-no-properties", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFieldStringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fieldStringNoProperties(Object pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the beginning of the field surrounding POS.
     * A field is a region of text with the same `field' property.
     * If POS is nil, the value of point is used for POS.
     * If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
     * field, then the beginning of the *previous* field is returned.
     * If LIMIT is non-nil, it is a buffer position; if the beginning of the field
     * is before LIMIT, then LIMIT will be returned instead.
     * </pre>
     */
    @ELispBuiltIn(name = "field-beginning", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFieldBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fieldBeginning(Object pos, Object escapeFromEdge, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the end of the field surrounding POS.
     * A field is a region of text with the same `field' property.
     * If POS is nil, the value of point is used for POS.
     * If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
     * then the end of the *following* field is returned.
     * If LIMIT is non-nil, it is a buffer position; if the end of the field
     * is after LIMIT, then LIMIT will be returned instead.
     * </pre>
     */
    @ELispBuiltIn(name = "field-end", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFieldEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fieldEnd(Object pos, Object escapeFromEdge, Object limit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position closest to NEW-POS that is in the same field as OLD-POS.
     * A field is a region of text with the same `field' property.
     *
     * If NEW-POS is nil, then use the current point instead, and move point
     * to the resulting constrained position, in addition to returning that
     * position.
     *
     * If OLD-POS is at the boundary of two fields, then the allowable
     * positions for NEW-POS depends on the value of the optional argument
     * ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
     * constrained to the field that has the same `field' char-property
     * as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
     * is non-nil, NEW-POS is constrained to the union of the two adjacent
     * fields.  Additionally, if two fields are separated by another field with
     * the special value `boundary', then any point within this special field is
     * also considered to be `on the boundary'.
     *
     * If the optional argument ONLY-IN-LINE is non-nil and constraining
     * NEW-POS would move it to a different line, NEW-POS is returned
     * unconstrained.  This is useful for commands that move by line, like
     * \\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
     * only in the case where they can still move to the right line.
     *
     * If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
     * a non-nil property of that name, then any field boundaries are ignored.
     *
     * Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil.
     * </pre>
     */
    @ELispBuiltIn(name = "constrain-to-field", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FConstrainToField extends ELispBuiltInBaseNode {
        @Specialization
        public static Object constrainToField(Object newPos, Object oldPos, Object escapeFromEdge, Object onlyInLine, Object inhibitCaptureProperty) {
            // TODO
            return oldPos;
        }
    }

    /**
     * <pre>
     * Return the position of the first character on the current line.
     * With optional argument N, scan forward N - 1 lines first.
     * If the scan reaches the end of the buffer, return that position.
     *
     * This function ignores text display directionality; it returns the
     * position of the first character in logical order, i.e. the smallest
     * character position on the logical line.  See `vertical-motion' for
     * movement by screen lines.
     *
     * This function does not move point.  Also see `line-beginning-position'.
     * </pre>
     */
    @ELispBuiltIn(name = "pos-bol", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPosBol extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posBol(Object n) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of the first character in the current line/field.
     * With optional argument N non-nil, move forward N - 1 lines first.
     * This function is like `pos-bol' (which see), but respects fields.
     *
     * This function constrains the returned position to the current field
     * unless that position would be on a different line from the original,
     * unconstrained result.  If N is nil or 1, and a front-sticky field
     * starts at point, the scan stops as soon as it starts.  To ignore field
     * boundaries, bind `inhibit-field-text-motion' to t.
     *
     * This function does not move point.
     * </pre>
     */
    @ELispBuiltIn(name = "line-beginning-position", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLineBeginningPosition extends ELispBuiltInBaseNode {
        @Specialization
        public long lineBeginningPosition(Object n) {
            ELispBuffer buffer = getContext().currentBuffer();
            return lineEdgePosition(n, buffer, true);
        }

        public static long lineEdgePosition(Object n, ELispBuffer buffer, boolean beginning) {
            long line = notNilOr(n, 1) - 1;
            PieceTreeBase.Position position = buffer.getPosition();
            int maxLine = buffer.getLineCount();

            long target = position.line() + line;
            int nextLine = Math.clamp(target, 1, maxLine);
            return buffer.getPoint(new PieceTreeBase.Position(
                    nextLine,
                    nextLine == target ? (beginning ? 1 : Integer.MAX_VALUE)
                            : (line < 0 ? 1 : Integer.MAX_VALUE)
            ));
        }
    }

    /**
     * <pre>
     * Return the position of the last character on the current line.
     * With argument N not nil or 1, move forward N - 1 lines first.
     * If scan reaches end of buffer, return that position.
     *
     * This function ignores text display directionality; it returns the
     * position of the last character in logical order, i.e. the largest
     * character position on the line.
     *
     * This function does not move point.  Also see `line-end-position'.
     * </pre>
     */
    @ELispBuiltIn(name = "pos-eol", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPosEol extends ELispBuiltInBaseNode {
        @Specialization
        public static Void posEol(Object n) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the position of the last character in the current line/field.
     * With argument N not nil or 1, move forward N - 1 lines first.
     * If scan reaches end of buffer, return that position.
     *
     * This function is like `pos-eol' (which see), but respects fields.
     *
     * This function constrains the returned position to the current field
     * unless that would be on a different line from the original,
     * unconstrained result.  If N is nil or 1, and a rear-sticky field ends
     * at point, the scan stops as soon as it starts.  To ignore field
     * boundaries bind `inhibit-field-text-motion' to t.
     *
     * This function does not move point.
     * </pre>
     */
    @ELispBuiltIn(name = "line-end-position", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLineEndPosition extends ELispBuiltInBaseNode {
        @Specialization
        public long lineEndPosition(Object n) {
            ELispBuffer buffer = getContext().currentBuffer();
            return FLineBeginningPosition.lineEdgePosition(n, buffer, false);
        }
    }

    /**
     * <pre>
     * Save point, and current buffer; execute BODY; restore those things.
     * Executes BODY just like `progn'.
     * The values of point and the current buffer are restored
     * even in case of abnormal exit (throw or error).
     *
     * If you only want to save the current buffer but not point,
     * then just use `save-current-buffer', or even `with-current-buffer'.
     *
     * Before Emacs 25.1, `save-excursion' used to save the mark state.
     * To save the mark state as well as point and the current buffer, use
     * `save-mark-and-excursion'.
     *
     * usage: (save-excursion &amp;rest BODY)
     * </pre>
     */
    @ELispBuiltIn(name = "save-excursion", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSaveExcursion extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void saveExcursionBailout(Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new SaveExcursionNode(arguments);
        }

        private static class SaveExcursionNode extends ELispExpressionNode {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            private ELispExpressionNode bodyNode;

            SaveExcursionNode(Object[] body) {
                bodyNode = BuiltInEval.FProgn.progn(body);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                ValueStorage.Forwarded forwarded = getLanguage().currentBuffer();
                Object prevBuffer = forwarded.getValue();
                ELispMarker point = prevBuffer instanceof ELispBuffer buffer
                        ? new ELispMarker(buffer, buffer.getPoint())
                        : null;
                try {
                    bodyNode.executeVoid(frame);
                } finally {
                    forwarded.setValue(prevBuffer);
                    if (point != null) {
                        asBuffer(prevBuffer).setPoint(point.point());
                        point.setBuffer(null, -1);
                    }
                }
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ValueStorage.Forwarded forwarded = getLanguage().currentBuffer();
                Object prevBuffer = forwarded.getValue();
                ELispMarker point = prevBuffer instanceof ELispBuffer buffer
                        ? new ELispMarker(buffer, buffer.getPoint())
                        : null;
                try {
                    return bodyNode.executeGeneric(frame);
                } finally {
                    forwarded.setValue(prevBuffer);
                    if (point != null) {
                        asBuffer(prevBuffer).setPoint(point.point());
                        point.setBuffer(null, -1);
                    }
                }
            }
        }
    }

    /**
     * <pre>
     * Record which buffer is current; execute BODY; make that buffer current.
     * BODY is executed just like `progn'.
     * usage: (save-current-buffer &amp;rest BODY)
     * </pre>
     */
    @ELispBuiltIn(name = "save-current-buffer", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSaveCurrentBuffer extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void saveCurrentBufferBailout(Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            return new SaveCurrentBufferNode(arguments);
        }

        private static class SaveCurrentBufferNode extends ELispExpressionNode {
            @Child
            ELispExpressionNode bodyNode;

            SaveCurrentBufferNode(Object[] body) {
                bodyNode = BuiltInEval.FProgn.progn(body);
            }

            @Override
            public void executeVoid(VirtualFrame frame) {
                ValueStorage.Forwarded forwarded = getLanguage().currentBuffer();
                Object prevBuffer = forwarded.getValue();
                try {
                    bodyNode.executeVoid(frame);
                } finally {
                    forwarded.setValue(prevBuffer);
                }
            }

            @Override
            public Object executeGeneric(VirtualFrame frame) {
                ValueStorage.Forwarded forwarded = getLanguage().currentBuffer();
                Object prevBuffer = forwarded.getValue();
                try {
                    return bodyNode.executeGeneric(frame);
                } finally {
                    forwarded.setValue(prevBuffer);
                }
            }
        }
    }

    /**
     * <pre>
     * Return the number of characters in the current buffer.
     * If BUFFER is not nil, return the number of characters in that buffer
     * instead.
     *
     * This does not take narrowing into account; to count the number of
     * characters in the accessible portion of the current buffer, use
     * `(- (point-max) (point-min))', and to count the number of characters
     * in the accessible portion of some other BUFFER, use
     * `(with-current-buffer BUFFER (- (point-max) (point-min)))'.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-size", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void bufferSize(Object buffer) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the minimum permissible value of point in the current buffer.
     * This is 1, unless narrowing (a buffer restriction) is in effect.
     * </pre>
     */
    @ELispBuiltIn(name = "point-min", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMin extends ELispBuiltInBaseNode {
        @Specialization
        public static long pointMin() {
            ELispBuffer buffer = currentBuffer();
            // TODO: narrowing?
            return 1L;
        }
    }

    /**
     * <pre>
     * Return a marker to the minimum permissible value of point in this buffer.
     * This is the beginning, unless narrowing (a buffer restriction) is in effect.
     * </pre>
     */
    @ELispBuiltIn(name = "point-min-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMinMarker extends ELispBuiltInBaseNode {
        @Specialization
        public ELispMarker pointMinMarker() {
            ELispBuffer buffer = getContext().currentBuffer();
            return new ELispMarker(buffer, buffer.pointMin());
        }
    }

    /**
     * <pre>
     * Return the maximum permissible value of point in the current buffer.
     * This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
     * is in effect, in which case it is less.
     * </pre>
     */
    @ELispBuiltIn(name = "point-max", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMax extends ELispBuiltInBaseNode {
        @Specialization
        public long pointMax() {
            return getContext().currentBuffer().pointMax();
        }
    }

    /**
     * <pre>
     * Return a marker to the maximum permissible value of point in this buffer.
     * This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
     * is in effect, in which case it is less.
     * </pre>
     */
    @ELispBuiltIn(name = "point-max-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMaxMarker extends ELispBuiltInBaseNode {
        @Specialization
        public ELispMarker pointMaxMarker() {
            ELispBuffer buffer = getContext().currentBuffer();
            return new ELispMarker(buffer, buffer.pointMax());
        }
    }

    /**
     * <pre>
     * Return the position of the gap, in the current buffer.
     * See also `gap-size'.
     * </pre>
     */
    @ELispBuiltIn(name = "gap-position", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGapPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void gapPosition() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the size of the current buffer's gap.
     * See also `gap-position'.
     * </pre>
     */
    @ELispBuiltIn(name = "gap-size", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGapSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Void gapSize() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the byte position for character position POSITION.
     * If POSITION is out of range, the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "position-bytes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPositionBytes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void positionBytes(Object position) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the character position for byte position BYTEPOS.
     * If BYTEPOS is out of range, the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "byte-to-position", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FByteToPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void byteToPosition(Object bytepos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the character following point, as a number.
     * At the end of the buffer or accessible region, return 0.
     * </pre>
     */
    @ELispBuiltIn(name = "following-char", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FFollowingChar extends ELispBuiltInBaseNode {
        @Specialization
        public long followingChar() {
            ELispBuffer buffer = getContext().currentBuffer();
            return followingCharBuffer(buffer);
        }

        public static long followingCharBuffer(ELispBuffer buffer) {
            long point = buffer.getPoint();
            return point == buffer.pointMax() ? 0 : buffer.getChar(point);
        }
    }

    /**
     * <pre>
     * Return the character preceding point, as a number.
     * At the beginning of the buffer or accessible region, return 0.
     * </pre>
     */
    @ELispBuiltIn(name = "preceding-char", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPreviousChar extends ELispBuiltInBaseNode {
        @Specialization
        public long previousChar() {
            ELispBuffer buffer = getContext().currentBuffer();
            return previousCharBuffer(buffer);
        }

        public static long previousCharBuffer(ELispBuffer buffer) {
            long point = buffer.getPoint();
            return point == buffer.pointMin() ? 0 : buffer.getChar(point);
        }
    }

    /**
     * <pre>
     * Return t if point is at the beginning of the buffer.
     * If the buffer is narrowed, this means the beginning of the narrowed part.
     * </pre>
     */
    @ELispBuiltIn(name = "bobp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBobp extends ELispBuiltInBaseNode {
        @Specialization
        public boolean bobp() {
            ELispBuffer buffer = getContext().currentBuffer();
            return bobpBuffer(buffer);
        }

        public static boolean bobpBuffer(ELispBuffer buffer) {
            return buffer.getPoint() == buffer.pointMin();
        }
    }

    /**
     * <pre>
     * Return t if point is at the end of the buffer.
     * If the buffer is narrowed, this means the end of the narrowed part.
     * </pre>
     */
    @ELispBuiltIn(name = "eobp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEobp extends ELispBuiltInBaseNode {
        @Specialization
        public boolean eobp() {
            ELispBuffer buffer = getContext().currentBuffer();
            return eobpBuffer(buffer);
        }

        public static boolean eobpBuffer(ELispBuffer buffer) {
            return buffer.getPoint() == buffer.pointMax();
        }
    }

    /**
     * <pre>
     * Return t if point is at the beginning of a line.
     * </pre>
     */
    @ELispBuiltIn(name = "bolp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBolp extends ELispBuiltInBaseNode {
        @Specialization
        public boolean bolp() {
            ELispBuffer buffer = getContext().currentBuffer();
            return eolpBuffer(buffer);
        }

        private static boolean eolpBuffer(ELispBuffer buffer) {
            long point = buffer.getPoint();
            return point == buffer.pointMin() || buffer.getChar(point - 1) == '\n';
        }
    }

    /**
     * <pre>
     * Return t if point is at the end of a line.
     * `End of a line' includes point being at the end of the buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "eolp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEolp extends ELispBuiltInBaseNode {
        @Specialization
        public boolean eolp() {
            ELispBuffer buffer = getContext().currentBuffer();
            return eolpBuffer(buffer);
        }

        public static boolean eolpBuffer(ELispBuffer buffer) {
            long point = buffer.getPoint();
            return point == buffer.pointMax() || buffer.getChar(point) == '\n';
        }
    }

    /**
     * <pre>
     * Return character in current buffer at position POS.
     * POS is an integer or a marker and defaults to point.
     * If POS is out of range, the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "char-after", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharAfter extends ELispBuiltInBaseNode {
        @Specialization
        public Object charAfter(Object pos) {
            ELispBuffer buffer = getContext().currentBuffer();
            return charAfterBuffer(pos, buffer);
        }

        public static Object charAfterBuffer(Object pos, ELispBuffer buffer) {
            long point = pos instanceof Long l ? l : buffer.getPoint();
            if (point < buffer.pointMax()) {
                return (long) buffer.getChar(point);
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return character in current buffer preceding position POS.
     * POS is an integer or a marker and defaults to point.
     * If POS is out of range, the value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "char-before", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharBefore extends ELispBuiltInBaseNode {
        @Specialization
        public Object charBefore(Object pos) {
            ELispBuffer buffer = getContext().currentBuffer();
            long point = pos instanceof Long l ? l : buffer.getPoint();
            if (point > buffer.pointMin()) {
                return (long) buffer.getChar(point - 1);
            }
            return false;
        }
    }

    /**
     * <pre>
     * Return the name under which the user logged in, as a string.
     * This is based on the effective uid, not the real uid.
     * Also, if the environment variables LOGNAME or USER are set,
     * that determines the value of this function.
     *
     * If optional argument UID is an integer, return the login name
     * of the user with that uid, or nil if there is no such user.
     * </pre>
     */
    @ELispBuiltIn(name = "user-login-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUserLoginName extends ELispBuiltInBaseNode {
        @Specialization
        public Object userLoginName(Object uid) {
            if (!isNil(uid)) {
                return false;
            }
            ELispContext context = getContext();
            String name = context.getEnv("LOGNAME");
            if (name == null) {
                name = context.getEnv("USER");
            }
            if (name != null) {
                return new ELispString(name);
            }
            return getLoginName(context);
        }

        public static Object getLoginName(ELispContext context) {
            TruffleLanguage.Env env = context.truffleEnv();
            SandboxPolicy policy = env.getSandboxPolicy();
            if (policy == SandboxPolicy.TRUSTED) {
                String name = context.getEnv("user.name");
                if (name != null) {
                    return new ELispString(name);
                }
            }
            return new ELispString("jvm");
        }
    }

    /**
     * <pre>
     * Return the name of the user's real uid, as a string.
     * This ignores the environment variables LOGNAME and USER, so it differs from
     * `user-login-name' when running under `su'.
     * </pre>
     */
    @ELispBuiltIn(name = "user-real-login-name", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUserRealLoginName extends ELispBuiltInBaseNode {
        @Specialization
        public Object userRealLoginName() {
            return FUserLoginName.getLoginName(getContext());
        }
    }

    /**
     * <pre>
     * Return the effective uid of Emacs, as an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "user-uid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUserUid extends ELispBuiltInBaseNode {
        @Specialization
        public static Void userUid() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the real uid of Emacs, as an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "user-real-uid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUserRealUid extends ELispBuiltInBaseNode {
        @Specialization
        public static Void userRealUid() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the name of the group whose numeric group ID is GID.
     * The argument GID should be an integer or a float.
     * Return nil if a group with such GID does not exists or is not known.
     * </pre>
     */
    @ELispBuiltIn(name = "group-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGroupName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void groupName(Object gid) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the effective gid of Emacs, as an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "group-gid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGroupGid extends ELispBuiltInBaseNode {
        @Specialization
        public static Void groupGid() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the real gid of Emacs, as an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "group-real-gid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGroupRealGid extends ELispBuiltInBaseNode {
        @Specialization
        public static Void groupRealGid() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the full name of the user logged in, as a string.
     * If the full name corresponding to Emacs's userid is not known,
     * return "unknown".
     *
     * If optional argument UID is an integer, return the full name
     * of the user with that uid, or nil if there is no such user.
     * If UID is a string, return the full name of the user with that login
     * name, or nil if there is no such user.
     *
     * If the full name includes commas, remove everything starting with
     * the first comma, because the \\='gecos\\=' field of the \\='/etc/passwd\\=' file
     * is in general a comma-separated list.
     * </pre>
     */
    @ELispBuiltIn(name = "user-full-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUserFullName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void userFullName(Object uid) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the host name of the machine you are running on, as a string.
     * </pre>
     */
    @ELispBuiltIn(name = "system-name", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSystemName extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public static ELispString systemName() {
            Object value = SYSTEM_NAME.getValue();
            if (value instanceof ELispString s) {
                return s;
            }
            ELispString s;
            try {
                String hostName = InetAddress.getLocalHost().getHostName();
                s = new ELispString(hostName);
            } catch (UnknownHostException e) {
                s = new ELispString("jvm-" + System.getProperty("java.vm.version", "unknown"));
            }
            SYSTEM_NAME.setValue(s);
            return s;
        }
    }

    /**
     * <pre>
     * Return the process ID of Emacs, as an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "emacs-pid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEmacsPid extends ELispBuiltInBaseNode {
        @Specialization
        public static long emacsPid() {
            return ProcessHandle.current().pid();
        }
    }

    /**
     * <pre>
     * Insert the arguments, either strings or characters, at point.
     * Point and after-insertion markers move forward to end up
     *  after the inserted text.
     * Any other markers at the point of insertion remain before the text.
     *
     * If the current buffer is multibyte, unibyte strings are converted
     * to multibyte for insertion (see `string-make-multibyte').
     * If the current buffer is unibyte, multibyte strings are converted
     * to unibyte for insertion (see `string-make-unibyte').
     *
     * When operating on binary data, it may be necessary to preserve the
     * original bytes of a unibyte string when inserting it into a multibyte
     * buffer; to accomplish this, apply `string-as-multibyte' to the string
     * and insert the result.
     *
     * usage: (insert &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "insert", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsert extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public static boolean insert(Object[] args) {
            ELispBuffer buffer = currentBuffer();
            for (Object arg : args) {
                if (arg instanceof ELispString s) {
                    buffer.insert(s);
                } else {
                    buffer.insert(new MuleStringBuilder().appendCodePoint(asInt(arg)).buildString());
                }
            }
            return false;
        }
    }

    /**
     * <pre>
     * Insert the arguments at point, inheriting properties from adjoining text.
     * Point and after-insertion markers move forward to end up
     *  after the inserted text.
     * Any other markers at the point of insertion remain before the text.
     *
     * If the current buffer is multibyte, unibyte strings are converted
     * to multibyte for insertion (see `unibyte-char-to-multibyte').
     * If the current buffer is unibyte, multibyte strings are converted
     * to unibyte for insertion.
     *
     * usage: (insert-and-inherit &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "insert-and-inherit", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsertAndInherit extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean insertAndInherit(Object[] args) {
            // TODO
            return FInsert.insert(args);
        }
    }

    /**
     * <pre>
     * Insert strings or characters at point, relocating markers after the text.
     * Point and markers move forward to end up after the inserted text.
     *
     * If the current buffer is multibyte, unibyte strings are converted
     * to multibyte for insertion (see `unibyte-char-to-multibyte').
     * If the current buffer is unibyte, multibyte strings are converted
     * to unibyte for insertion.
     *
     * If an overlay begins at the insertion point, the inserted text falls
     * outside the overlay; if a nonempty overlay ends at the insertion
     * point, the inserted text falls inside that overlay.
     *
     * usage: (insert-before-markers &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "insert-before-markers", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsertBeforeMarkers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void insertBeforeMarkers(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert text at point, relocating markers and inheriting properties.
     * Point and markers move forward to end up after the inserted text.
     *
     * If the current buffer is multibyte, unibyte strings are converted
     * to multibyte for insertion (see `unibyte-char-to-multibyte').
     * If the current buffer is unibyte, multibyte strings are converted
     * to unibyte for insertion.
     *
     * usage: (insert-before-markers-and-inherit &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "insert-before-markers-and-inherit", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsertAndInheritBeforeMarkers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void insertAndInheritBeforeMarkers(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert COUNT copies of CHARACTER.
     * Interactively, prompt for CHARACTER using `read-char-by-name'.
     * You can specify CHARACTER in one of these ways:
     *
     *  - As its Unicode character name, e.g. \"LATIN SMALL LETTER A\".
     *    Completion is available; if you type a substring of the name
     *    preceded by an asterisk `*', Emacs shows all names which include
     *    that substring, not necessarily at the beginning of the name.
     *
     *  - As a hexadecimal code point, e.g. 263A.  Note that code points in
     *    Emacs are equivalent to Unicode up to 10FFFF (which is the limit of
     *    the Unicode code space).
     *
     *  - As a code point with a radix specified with #, e.g. #o21430
     *    (octal), #x2318 (hex), or #10r8984 (decimal).
     *
     * If called interactively, COUNT is given by the prefix argument.  If
     * omitted or nil, it defaults to 1.
     *
     * Inserting the character(s) relocates point and before-insertion
     * markers in the same ways as the function `insert'.
     *
     * The optional third argument INHERIT, if non-nil, says to inherit text
     * properties from adjoining text, if those properties are sticky.  If
     * called interactively, INHERIT is t.
     * </pre>
     */
    @ELispBuiltIn(name = "insert-char", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInsertChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void insertChar(Object character, Object count, Object inherit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert COUNT (second arg) copies of BYTE (first arg).
     * Both arguments are required.
     * BYTE is a number of the range 0..255.
     *
     * If BYTE is 128..255 and the current buffer is multibyte, the
     * corresponding eight-bit character is inserted.
     *
     * Point, and before-insertion markers, are relocated as in the function `insert'.
     * The optional third arg INHERIT, if non-nil, says to inherit text properties
     * from adjoining text, if those properties are sticky.
     * </pre>
     */
    @ELispBuiltIn(name = "insert-byte", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInsertByte extends ELispBuiltInBaseNode {
        @Specialization
        public static Void insertByte(Object byte_, Object count, Object inherit) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the contents of part of the current buffer as a string.
     * The two arguments START and END are character positions;
     * they can be in either order.
     * The string returned is multibyte if the buffer is multibyte.
     *
     * This function copies the text properties of that part of the buffer
     * into the result string; if you don't want the text properties,
     * use `buffer-substring-no-properties' instead.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-substring", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBufferSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public ELispString bufferSubstring(long start, long end) {
            // TODO
            return getContext().currentBuffer().subString(start, end);
        }

        public static ELispString bufferSubstringBuffer(long start, long end, ELispBuffer buffer) {
            return buffer.subString(start, end);
        }
    }

    /**
     * <pre>
     * Return the characters of part of the buffer, without the text properties.
     * The two arguments START and END are character positions;
     * they can be in either order.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-substring-no-properties", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBufferSubstringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public ELispString bufferSubstringNoProperties(long start, long end) {
            return getContext().currentBuffer().subString(start, end);
        }
    }

    /**
     * <pre>
     * Return the contents of the current buffer as a string.
     * If narrowing is in effect, this function returns only the visible part
     * of the buffer.
     *
     * This function copies the text properties of that part of the buffer
     * into the result string; if you don’t want the text properties,
     * use `buffer-substring-no-properties' instead.
     * </pre>
     */
    @ELispBuiltIn(name = "buffer-string", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBufferString extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString bufferString() {
            // TODO: Properties
            return currentBuffer().bufferString();
        }
    }

    /**
     * <pre>
     * Insert before point a substring of the contents of BUFFER.
     * BUFFER may be a buffer or a buffer name.
     * Arguments START and END are character positions specifying the substring.
     * They default to the values of (point-min) and (point-max) in BUFFER.
     *
     * Point and before-insertion markers move forward to end up after the
     * inserted text.
     * Any other markers at the point of insertion remain before the text.
     *
     * If the current buffer is multibyte and BUFFER is unibyte, or vice
     * versa, strings are converted from unibyte to multibyte or vice versa
     * using `string-make-multibyte' or `string-make-unibyte', which see.
     * </pre>
     */
    @ELispBuiltIn(name = "insert-buffer-substring", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInsertBufferSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public boolean insertBufferSubstring(Object buffer, Object start, Object end) {
            ELispBuffer b;
            if (buffer instanceof ELispBuffer b1) {
                b = b1;
            } else {
                b = asBuffer(BuiltInBuffer.FGetBuffer.getBuffer(buffer));
            }
            ELispBuffer current = getContext().currentBuffer();
            long startI = notNilOr(start, b.pointMin());
            long endI = notNilOr(end, b.pointMax());
            current.insert(b.subString(startI, endI));
            return false;
        }
    }

    /**
     * <pre>
     * Compare two substrings of two buffers; return result as number.
     * Return -N if first string is less after N-1 chars, +N if first string is
     * greater after N-1 chars, or 0 if strings match.
     * The first substring is in BUFFER1 from START1 to END1 and the second
     * is in BUFFER2 from START2 to END2.
     * All arguments may be nil.  If BUFFER1 or BUFFER2 is nil, the current
     * buffer is used.  If START1 or START2 is nil, the value of `point-min'
     * in the respective buffers is used.  If END1 or END2 is nil, the value
     * of `point-max' in the respective buffers is used.
     * The value of `case-fold-search' in the current buffer
     * determines whether case is significant or ignored.
     * </pre>
     */
    @ELispBuiltIn(name = "compare-buffer-substrings", minArgs = 6, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FCompareBufferSubstrings extends ELispBuiltInBaseNode {
        @Specialization
        public static Void compareBufferSubstrings(Object buffer1, Object start1, Object end1, Object buffer2, Object start2, Object end2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Replace accessible portion of current buffer with that of SOURCE.
     * SOURCE can be a buffer or a string that names a buffer.
     * Interactively, prompt for SOURCE.
     *
     * As far as possible the replacement is non-destructive, i.e. existing
     * buffer contents, markers, properties, and overlays in the current
     * buffer stay intact.
     *
     * Because this function can be very slow if there is a large number of
     * differences between the two buffers, there are two optional arguments
     * mitigating this issue.
     *
     * The MAX-SECS argument, if given, defines a hard limit on the time used
     * for comparing the buffers.  If it takes longer than MAX-SECS, the
     * function falls back to a plain `delete-region' and
     * `insert-buffer-substring'.  (Note that the checks are not performed
     * too evenly over time, so in some cases it may run a bit longer than
     * allowed).
     *
     * The optional argument MAX-COSTS defines the quality of the difference
     * computation.  If the actual costs exceed this limit, heuristics are
     * used to provide a faster but suboptimal solution.  The default value
     * is 1000000.
     *
     * This function returns t if a non-destructive replacement could be
     * performed.  Otherwise, i.e., if MAX-SECS was exceeded, it returns
     * nil.
     * </pre>
     */
    @ELispBuiltIn(name = "replace-buffer-contents", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReplaceBufferContents extends ELispBuiltInBaseNode {
        @Specialization
        public static Void replaceBufferContents(Object source, Object maxSecs, Object maxCosts) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * From START to END, replace FROMCHAR with TOCHAR each time it occurs.
     * If optional arg NOUNDO is non-nil, don't record this change for undo
     * and don't mark the buffer as really changed.
     * Both characters must have the same length of multi-byte form.
     * </pre>
     */
    @ELispBuiltIn(name = "subst-char-in-region", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FSubstCharInRegion extends ELispBuiltInBaseNode {
        @Specialization
        public boolean substCharInRegion(long start, long end, long fromchar, long tochar, Object noundo) {
            MuleStringBuilder builder = new MuleStringBuilder();
            ELispBuffer buffer = getContext().currentBuffer();
            PrimitiveIterator.OfInt i = buffer.iterator(start, end);
            boolean changed = false;
            int from = (int) fromchar, to = (int) tochar;
            while (i.hasNext()) {
                int c = i.nextInt();
                changed = changed || c == fromchar;
                builder.appendCodePoint(c == from ? to : c);
            }
            if (changed) {
                buffer.replace(start, builder.buildString());
            }
            return false;
        }
    }

    /**
     * <pre>
     * Internal use only.
     * From START to END, translate characters according to TABLE.
     * TABLE is a string or a char-table; the Nth character in it is the
     * mapping for the character with code N.
     * It returns the number of characters changed.
     * </pre>
     */
    @ELispBuiltIn(name = "translate-region-internal", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTranslateRegionInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void translateRegionInternal(Object start, Object end, Object table) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete the text between START and END.
     * If called interactively, delete the region between point and mark.
     * This command deletes buffer text without modifying the kill ring.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-region", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean deleteRegion(long start, long end) {
            long length = Math.abs(end - start);
            start = Math.min(start, end);
            ELispBuffer buffer = currentBuffer();
            if (start < buffer.pointMin() || start + length > buffer.pointMax()) {
                throw ELispSignals.argsOutOfRange(start, end);
            }
            buffer.delete(start, length);
            return false;
        }
    }

    /**
     * <pre>
     * Delete the text between START and END and return it.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-and-extract-region", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteAndExtractRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void deleteAndExtractRegion(Object start, Object end) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Remove restrictions (narrowing) from current buffer.
     *
     * This allows the buffer's full text to be seen and edited.
     *
     * However, when restrictions have been set by `with-restriction' with a
     * label, `widen' restores the narrowing limits set by `with-restriction'.
     * To gain access to other portions of the buffer, use
     * `without-restriction' with the same label.
     * </pre>
     */
    @ELispBuiltIn(name = "widen", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FWiden extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean widen() {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Restrict editing in this buffer to the current region.
     * The rest of the text becomes temporarily invisible and untouchable
     * but is not deleted; if you save the buffer in a file, the invisible
     * text is included in the file.  \\[widen] makes all visible again.
     * See also `save-restriction'.
     *
     * When calling from Lisp, pass two arguments START and END:
     * positions (integers or markers) bounding the text that should
     * remain visible.
     *
     * However, when restrictions have been set by `with-restriction' with a
     * label, `narrow-to-region' can be used only within the limits of these
     * restrictions.  If the START or END arguments are outside these limits,
     * the corresponding limit set by `with-restriction' is used instead of the
     * argument.  To gain access to other portions of the buffer, use
     * `without-restriction' with the same label.
     * </pre>
     */
    @ELispBuiltIn(name = "narrow-to-region", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNarrowToRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void narrowToRegion(Object start, Object end) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Restrict this buffer to START-END, and label the restriction with LABEL.
     *
     * This is an internal function used by `with-restriction'.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--labeled-narrow-to-region", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalLabeledNarrowToRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalLabeledNarrowToRegion(Object start, Object end, Object label) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Remove the current restriction if it is labeled with LABEL, and widen.
     *
     * This is an internal function used by `without-restriction'.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--labeled-widen", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalLabeledWiden extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalLabeledWiden(Object label) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Execute BODY, saving and restoring current buffer's restrictions.
     * The buffer's restrictions make parts of the beginning and end invisible.
     * \(They are set up with `narrow-to-region' and eliminated with `widen'.)
     * This special form, `save-restriction', saves the current buffer's
     * restrictions, including those that were set by `with-restriction' with a
     * label argument, when it is entered, and restores them when it is exited.
     * So any `narrow-to-region' within BODY lasts only until the end of the form.
     * The old restrictions settings are restored even in case of abnormal exit
     * \(throw or error).
     *
     * The value returned is the value of the last form in BODY.
     *
     * Note: if you are using both `save-excursion' and `save-restriction',
     * use `save-excursion' outermost:
     *     (save-excursion (save-restriction ...))
     *
     * usage: (save-restriction &amp;rest BODY)
     * </pre>
     */
    @ELispBuiltIn(name = "save-restriction", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSaveRestriction extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.SpecialFactory {
        @Specialization
        public static Void saveRestrictionBailout(Object[] body) {
            return null;
        }

        @Override
        public ELispExpressionNode createNode(Object[] arguments) {
            // TODO
            return BuiltInEval.FProgn.progn(arguments);
        }
    }

    /**
     * <pre>
     * Return the translation of MSGID (plural MSGID-PLURAL) depending on N.
     * MSGID is the singular form of the string to be converted;
     * use it as the key for the search in the translation catalog.
     * MSGID-PLURAL is the plural form.  Use N to select the proper translation.
     * If no message catalog is found, MSGID is returned if N is equal to 1,
     * otherwise MSGID-PLURAL.
     * </pre>
     */
    @ELispBuiltIn(name = "ngettext", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FNgettext extends ELispBuiltInBaseNode {
        @Specialization
        public static Void ngettext(Object msgid, Object msgidPlural, Object n) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Display a message at the bottom of the screen.
     * The message also goes into the `*Messages*' buffer, if `message-log-max'
     * is non-nil.  (In keyboard macros, that's all it does.)
     * Return the message.
     *
     * In batch mode, the message is printed to the standard error stream,
     * followed by a newline.
     *
     * The first argument is a format control string, and the rest are data
     * to be formatted under control of the string.  Percent sign (%), grave
     * accent (\\=`) and apostrophe (\\=') are special in the format; see
     * `format-message' for details.  To display STRING without special
     * treatment, use (message "%s" STRING).
     *
     * If the first argument is nil or the empty string, the function clears
     * any existing message; this lets the minibuffer contents show.  See
     * also `current-message'.
     *
     * usage: (message FORMAT-STRING &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "message", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMessage extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public Object message(Object formatString, Object[] args) {
            if (isNil(formatString)) {
                // TODO
                return false;
            }
            ELispString formatted = FFormat.format(asStr(formatString), args);
            getContext().out().println(formatted);
            return formatted;
        }

        @TruffleBoundary
        public static void message(ELispContext context, String s) {
            context.out().println(s);
        }
    }

    /**
     * <pre>
     * Display a message, in a dialog box if possible.
     * If a dialog box is not available, use the echo area.
     * The first argument is a format control string, and the rest are data
     * to be formatted under control of the string.  See `format-message' for
     * details.
     *
     * If the first argument is nil or the empty string, clear any existing
     * message; let the minibuffer contents show.
     *
     * usage: (message-box FORMAT-STRING &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "message-box", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMessageBox extends ELispBuiltInBaseNode {
        @Specialization
        public static Void messageBox(Object formatString, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Display a message in a dialog box or in the echo area.
     * If this command was invoked with the mouse, use a dialog box if
     * `use-dialog-box' is non-nil.
     * Otherwise, use the echo area.
     * The first argument is a format control string, and the rest are data
     * to be formatted under control of the string.  See `format-message' for
     * details.
     *
     * If the first argument is nil or the empty string, clear any existing
     * message; let the minibuffer contents show.
     *
     * usage: (message-or-box FORMAT-STRING &amp;rest ARGS)
     * </pre>
     */
    @ELispBuiltIn(name = "message-or-box", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMessageOrBox extends ELispBuiltInBaseNode {
        @Specialization
        public static Void messageOrBox(Object formatString, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the string currently displayed in the echo area, or nil if none.
     * </pre>
     */
    @ELispBuiltIn(name = "current-message", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean currentMessage() {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return a copy of STRING with text properties added.
     * First argument is the string to copy.
     * Remaining arguments form a sequence of PROPERTY VALUE pairs for text
     * properties to add to the result.
     *
     * See Info node `(elisp) Text Properties' for more information.
     * usage: (propertize STRING &amp;rest PROPERTIES)
     * </pre>
     */
    @ELispBuiltIn(name = "propertize", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FPropertize extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString propertize(ELispString string, Object[] properties) {
            // TODO
            ELispString s = BuiltInFns.FCopySequence.copySequenceString(string);
            long length = s.length();
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            for (int i = 0; i < properties.length; i += 2) {
                builder.add(properties[i]);
                builder.add(properties[i + 1]);
            }
            s.syncFromPlist(Arrays.asList(false, 0L, length, builder.build()));
            return s;
        }
    }

    /**
     * <pre>
     * Format a string out of a format-string and arguments.
     * The first argument is a format control string.
     * The other arguments are substituted into it to make the result, a string.
     *
     * The format control string may contain %-sequences meaning to substitute
     * the next available argument, or the argument explicitly specified:
     *
     * %s means produce a string argument.  Actually, produces any object with `princ'.
     * %d means produce as signed number in decimal.
     * %o means produce a number in octal.
     * %x means produce a number in hex.
     * %X is like %x, but uses upper case.
     * %e means produce a number in exponential notation.
     * %f means produce a number in decimal-point notation.
     * %g means produce a number in exponential notation if the exponent would be
     *    less than -4 or greater than or equal to the precision (default: 6);
     *    otherwise it produces in decimal-point notation.
     * %c means produce a number as a single character.
     * %S means produce any object as an s-expression (using `prin1').
     *
     * The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
     * %o, %x, and %X treat arguments as unsigned if `binary-as-unsigned' is t
     *   (this is experimental; email 32252&#64;debbugs.gnu.org if you need it).
     * Use %% to put a single % into the output.
     *
     * A %-sequence other than %% may contain optional field number, flag,
     * width, and precision specifiers, as follows:
     *
     *   %&lt;field&gt;&lt;flags&gt;&lt;width&gt;&lt;precision&gt;character
     *
     * where field is [0-9]+ followed by a literal dollar "$", flags is
     * [+ #0-]+, width is [0-9]+, and precision is a literal period "."
     * followed by [0-9]+.
     *
     * If a %-sequence is numbered with a field with positive value N, the
     * Nth argument is substituted instead of the next one.  A format can
     * contain either numbered or unnumbered %-sequences but not both, except
     * that %% can be mixed with numbered %-sequences.
     *
     * The + flag character inserts a + before any nonnegative number, while a
     * space inserts a space before any nonnegative number; these flags
     * affect only numeric %-sequences, and the + flag takes precedence.
     * The - and 0 flags affect the width specifier, as described below.
     *
     * The # flag means to use an alternate display form for %o, %x, %X, %e,
     * %f, and %g sequences: for %o, it ensures that the result begins with
     * \"0\"; for %x and %X, it prefixes nonzero results with \"0x\" or \"0X\";
     * for %e and %f, it causes a decimal point to be included even if the
     * precision is zero; for %g, it causes a decimal point to be
     * included even if the precision is zero, and also forces trailing
     * zeros after the decimal point to be left in place.
     *
     * The width specifier supplies a lower limit for the length of the
     * produced representation.  The padding, if any, normally goes on the
     * left, but it goes on the right if the - flag is present.  The padding
     * character is normally a space, but it is 0 if the 0 flag is present.
     * The 0 flag is ignored if the - flag is present, or the format sequence
     * is something other than %d, %o, %x, %e, %f, and %g.
     *
     * For %e and %f sequences, the number after the "." in the precision
     * specifier says how many decimal places to show; if zero, the decimal
     * point itself is omitted.  For %g, the precision specifies how many
     * significant digits to produce; zero or omitted are treated as 1.
     * For %s and %S, the precision specifier truncates the string to the
     * given width.
     *
     * Text properties, if any, are copied from the format-string to the
     * produced text.
     *
     * usage: (format STRING &amp;rest OBJECTS)
     * </pre>
     */
    @ELispBuiltIn(name = "format", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFormat extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public static ELispString format(ELispString string, Object[] objects) {
            // TODO
            String s = string.toString();
            for (int i = 0; i < objects.length; i++) {
                Object o = objects[i];
                if (o instanceof Long l && 0 <= l && l <= MAX_CHAR) {
                    objects[i] = l.intValue(); // NOPMD
                }
            }
            s = s.replace("%S", "%s");
            return new ELispString(String.format(s, objects));
        }
    }

    /**
     * <pre>
     * Format a string out of a format-string and arguments.
     * The first argument is a format control string.
     * The other arguments are substituted into it to make the result, a string.
     *
     * This acts like `format', except it also replaces each grave accent (\\=`)
     * by a left quote, and each apostrophe (\\=') by a right quote.  The left
     * and right quote replacement characters are specified by
     * `text-quoting-style'.
     *
     * usage: (format-message STRING &amp;rest OBJECTS)
     * </pre>
     */
    @ELispBuiltIn(name = "format-message", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFormatMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString formatMessage(ELispString string, Object[] objects) {
            // TODO
            return FFormat.format(string, objects);
        }
    }

    /**
     * <pre>
     * Return t if two characters match, optionally ignoring case.
     * Both arguments must be characters (i.e. integers).
     * Case is ignored if `case-fold-search' is non-nil in the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "char-equal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharEqual extends ELispBuiltInBaseNode {
        @Specialization
        public boolean charEqual(long c1, long c2) {
            int c1i = asChar(c1);
            int c2i = asChar(c2);
            ELispContext context = getContext();
            ELispBuffer buffer = context.currentBuffer();
            Object caseFold = context.getStorage(CASE_FOLD_SEARCH).getValue(CASE_FOLD_SEARCH);
            if (!isNil(caseFold)) {
                ELispCharTable canon = asCharTable(buffer.getCaseCanonTable());
                c1i = (int) notNilOr(canon.getChar(c1i), c1i);
                c2i = (int) notNilOr(canon.getChar(c2i), c2i);
            }
            return c1i == c2i;
        }
    }

    /**
     * <pre>
     * Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
     * The regions should not be overlapping, because the size of the buffer is
     * never changed in a transposition.
     *
     * Optional fifth arg LEAVE-MARKERS, if non-nil, means don't update
     * any markers that happen to be located in the regions.
     *
     * Transposing beyond buffer boundaries is an error.
     *
     * Interactively, STARTR1 and ENDR1 are point and mark; STARTR2 and ENDR2
     * are the last two marks pushed to the mark ring; LEAVE-MARKERS is nil.
     * If a prefix argument N is given, STARTR2 and ENDR2 are the two
     * successive marks N entries back in the mark ring.  A negative prefix
     * argument instead counts forward from the oldest mark in the mark
     * ring.
     * </pre>
     */
    @ELispBuiltIn(name = "transpose-regions", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FTransposeRegions extends ELispBuiltInBaseNode {
        @Specialization
        public static Void transposeRegions(Object startr1, Object endr1, Object startr2, Object endr2, Object leaveMarkers) {
            throw new UnsupportedOperationException();
        }
    }
}
