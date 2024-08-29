package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInEditFns extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInEditFnsFactory.getFactories();
    }

    @ELispBuiltIn(name = "char-to-string", minArgs = 1, maxArgs = 1, doc = "Convert arg CHAR to a string containing that character.\nusage: (char-to-string CHAR)")
    @GenerateNodeFactory
    public abstract static class FCharToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charToString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "byte-to-string", minArgs = 1, maxArgs = 1, doc = "Convert arg BYTE to a unibyte string containing that byte.")
    @GenerateNodeFactory
    public abstract static class FByteToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object byteToString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-char", minArgs = 1, maxArgs = 1, doc = "Return the first character in STRING.")
    @GenerateNodeFactory
    public abstract static class FStringToChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToChar(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point", minArgs = 0, maxArgs = 0, doc = "Return value of point, as an integer.\nBeginning of buffer is position (point-min).")
    @GenerateNodeFactory
    public abstract static class FPoint extends ELispBuiltInBaseNode {
        @Specialization
        public static Object point() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-marker", minArgs = 0, maxArgs = 0, doc = "Return value of point, as a marker object.")
    @GenerateNodeFactory
    public abstract static class FPointMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "goto-char", minArgs = 1, maxArgs = 1, doc = "Set point to POSITION, a number or marker.\nBeginning of buffer is position (point-min), end is (point-max).\n\nThe return value is POSITION.\n\nIf called interactively, a numeric prefix argument specifies\nPOSITION; without a numeric prefix argument, read POSITION from the\nminibuffer.  The default value is the number at point (if any).")
    @GenerateNodeFactory
    public abstract static class FGotoChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gotoChar(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "region-beginning", minArgs = 0, maxArgs = 0, doc = "Return the integer value of point or mark, whichever is smaller.")
    @GenerateNodeFactory
    public abstract static class FRegionBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Object regionBeginning() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "region-end", minArgs = 0, maxArgs = 0, doc = "Return the integer value of point or mark, whichever is larger.")
    @GenerateNodeFactory
    public abstract static class FRegionEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object regionEnd() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mark-marker", minArgs = 0, maxArgs = 0, doc = "Return this buffer's mark, as a marker object.\nWatch out!  Moving this marker changes the mark position.\nIf you set the marker not to point anywhere, the buffer will have no mark.")
    @GenerateNodeFactory
    public abstract static class FMarkMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object markMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-pos-property", minArgs = 2, maxArgs = 3, doc = "Return the value of POSITION's property PROP, in OBJECT.\nAlmost identical to `get-char-property' except for the following difference:\nWhereas `get-char-property' returns the property of the char at (i.e. right\nafter) POSITION, this pays attention to properties's stickiness and overlays's\nadvancement settings, in order to find the property of POSITION itself,\ni.e. the property that a char would inherit if it were inserted\nat POSITION.")
    @GenerateNodeFactory
    public abstract static class FGetPosProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getPosProperty(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-field", minArgs = 0, maxArgs = 1, doc = "Delete the field surrounding POS.\nA field is a region of text with the same `field' property.\nIf POS is nil, the value of point is used for POS.")
    @GenerateNodeFactory
    public abstract static class FDeleteField extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteField(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-string", minArgs = 0, maxArgs = 1, doc = "Return the contents of the field surrounding POS as a string.\nA field is a region of text with the same `field' property.\nIf POS is nil, the value of point is used for POS.")
    @GenerateNodeFactory
    public abstract static class FFieldString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-string-no-properties", minArgs = 0, maxArgs = 1, doc = "Return the contents of the field around POS, without text properties.\nA field is a region of text with the same `field' property.\nIf POS is nil, the value of point is used for POS.")
    @GenerateNodeFactory
    public abstract static class FFieldStringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldStringNoProperties(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-beginning", minArgs = 0, maxArgs = 3, doc = "Return the beginning of the field surrounding POS.\nA field is a region of text with the same `field' property.\nIf POS is nil, the value of point is used for POS.\nIf ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its\nfield, then the beginning of the *previous* field is returned.\nIf LIMIT is non-nil, it is a buffer position; if the beginning of the field\nis before LIMIT, then LIMIT will be returned instead.")
    @GenerateNodeFactory
    public abstract static class FFieldBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldBeginning(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-end", minArgs = 0, maxArgs = 3, doc = "Return the end of the field surrounding POS.\nA field is a region of text with the same `field' property.\nIf POS is nil, the value of point is used for POS.\nIf ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,\nthen the end of the *following* field is returned.\nIf LIMIT is non-nil, it is a buffer position; if the end of the field\nis after LIMIT, then LIMIT will be returned instead.")
    @GenerateNodeFactory
    public abstract static class FFieldEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldEnd(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "constrain-to-field", minArgs = 2, maxArgs = 5, doc = "Return the position closest to NEW-POS that is in the same field as OLD-POS.\nA field is a region of text with the same `field' property.\n\nIf NEW-POS is nil, then use the current point instead, and move point\nto the resulting constrained position, in addition to returning that\nposition.\n\nIf OLD-POS is at the boundary of two fields, then the allowable\npositions for NEW-POS depends on the value of the optional argument\nESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is\nconstrained to the field that has the same `field' char-property\nas any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE\nis non-nil, NEW-POS is constrained to the union of the two adjacent\nfields.  Additionally, if two fields are separated by another field with\nthe special value `boundary', then any point within this special field is\nalso considered to be `on the boundary'.\n\nIf the optional argument ONLY-IN-LINE is non-nil and constraining\nNEW-POS would move it to a different line, NEW-POS is returned\nunconstrained.  This is useful for commands that move by line, like\n\\\\[next-line] or \\\\[beginning-of-line], which should generally respect field boundaries\nonly in the case where they can still move to the right line.\n\nIf the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has\na non-nil property of that name, then any field boundaries are ignored.\n\nField boundaries are not noticed if `inhibit-field-text-motion' is non-nil.")
    @GenerateNodeFactory
    public abstract static class FConstrainToField extends ELispBuiltInBaseNode {
        @Specialization
        public static Object constrainToField(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "pos-bol", minArgs = 0, maxArgs = 1, doc = "Return the position of the first character on the current line.\nWith optional argument N, scan forward N - 1 lines first.\nIf the scan reaches the end of the buffer, return that position.\n\nThis function ignores text display directionality; it returns the\nposition of the first character in logical order, i.e. the smallest\ncharacter position on the logical line.  See `vertical-motion' for\nmovement by screen lines.\n\nThis function does not move point.  Also see `line-beginning-position'.")
    @GenerateNodeFactory
    public abstract static class FPosBol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posBol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "line-beginning-position", minArgs = 0, maxArgs = 1, doc = "Return the position of the first character in the current line/field.\nWith optional argument N non-nil, move forward N - 1 lines first.\nThis function is like `pos-bol' (which see), but respects fields.\n\nThis function constrains the returned position to the current field\nunless that position would be on a different line from the original,\nunconstrained result.  If N is nil or 1, and a front-sticky field\nstarts at point, the scan stops as soon as it starts.  To ignore field\nboundaries, bind `inhibit-field-text-motion' to t.\n\nThis function does not move point.")
    @GenerateNodeFactory
    public abstract static class FLineBeginningPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lineBeginningPosition(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "pos-eol", minArgs = 0, maxArgs = 1, doc = "Return the position of the last character on the current line.\nWith argument N not nil or 1, move forward N - 1 lines first.\nIf scan reaches end of buffer, return that position.\n\nThis function ignores text display directionality; it returns the\nposition of the last character in logical order, i.e. the largest\ncharacter position on the line.\n\nThis function does not move point.  Also see `line-end-position'.")
    @GenerateNodeFactory
    public abstract static class FPosEol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posEol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "line-end-position", minArgs = 0, maxArgs = 1, doc = "Return the position of the last character in the current line/field.\nWith argument N not nil or 1, move forward N - 1 lines first.\nIf scan reaches end of buffer, return that position.\n\nThis function is like `pos-eol' (which see), but respects fields.\n\nThis function constrains the returned position to the current field\nunless that would be on a different line from the original,\nunconstrained result.  If N is nil or 1, and a rear-sticky field ends\nat point, the scan stops as soon as it starts.  To ignore field\nboundaries bind `inhibit-field-text-motion' to t.\n\nThis function does not move point.")
    @GenerateNodeFactory
    public abstract static class FLineEndPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lineEndPosition(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "save-excursion", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Save point, and current buffer; execute BODY; restore those things.\nExecutes BODY just like `progn'.\nThe values of point and the current buffer are restored\neven in case of abnormal exit (throw or error).\n\nIf you only want to save the current buffer but not point,\nthen just use `save-current-buffer', or even `with-current-buffer'.\n\nBefore Emacs 25.1, `save-excursion' used to save the mark state.\nTo save the mark state as well as point and the current buffer, use\n`save-mark-and-excursion'.\n\nusage: (save-excursion &rest BODY)")
    @GenerateNodeFactory
    public abstract static class FSaveExcursion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object saveExcursion(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "save-current-buffer", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Record which buffer is current; execute BODY; make that buffer current.\nBODY is executed just like `progn'.\nusage: (save-current-buffer &rest BODY)")
    @GenerateNodeFactory
    public abstract static class FSaveCurrentBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object saveCurrentBuffer(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-size", minArgs = 0, maxArgs = 1, doc = "Return the number of characters in the current buffer.\nIf BUFFER is not nil, return the number of characters in that buffer\ninstead.\n\nThis does not take narrowing into account; to count the number of\ncharacters in the accessible portion of the current buffer, use\n`(- (point-max) (point-min))', and to count the number of characters\nin the accessible portion of some other BUFFER, use\n`(with-current-buffer BUFFER (- (point-max) (point-min)))'.")
    @GenerateNodeFactory
    public abstract static class FBufferSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-min", minArgs = 0, maxArgs = 0, doc = "Return the minimum permissible value of point in the current buffer.\nThis is 1, unless narrowing (a buffer restriction) is in effect.")
    @GenerateNodeFactory
    public abstract static class FPointMin extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMin() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-min-marker", minArgs = 0, maxArgs = 0, doc = "Return a marker to the minimum permissible value of point in this buffer.\nThis is the beginning, unless narrowing (a buffer restriction) is in effect.")
    @GenerateNodeFactory
    public abstract static class FPointMinMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMinMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-max", minArgs = 0, maxArgs = 0, doc = "Return the maximum permissible value of point in the current buffer.\nThis is (1+ (buffer-size)), unless narrowing (a buffer restriction)\nis in effect, in which case it is less.")
    @GenerateNodeFactory
    public abstract static class FPointMax extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMax() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-max-marker", minArgs = 0, maxArgs = 0, doc = "Return a marker to the maximum permissible value of point in this buffer.\nThis is (1+ (buffer-size)), unless narrowing (a buffer restriction)\nis in effect, in which case it is less.")
    @GenerateNodeFactory
    public abstract static class FPointMaxMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMaxMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "gap-position", minArgs = 0, maxArgs = 0, doc = "Return the position of the gap, in the current buffer.\nSee also `gap-size'.")
    @GenerateNodeFactory
    public abstract static class FGapPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gapPosition() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "gap-size", minArgs = 0, maxArgs = 0, doc = "Return the size of the current buffer's gap.\nSee also `gap-position'.")
    @GenerateNodeFactory
    public abstract static class FGapSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gapSize() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "position-bytes", minArgs = 1, maxArgs = 1, doc = "Return the byte position for character position POSITION.\nIf POSITION is out of range, the value is nil.")
    @GenerateNodeFactory
    public abstract static class FPositionBytes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object positionBytes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "byte-to-position", minArgs = 1, maxArgs = 1, doc = "Return the character position for byte position BYTEPOS.\nIf BYTEPOS is out of range, the value is nil.")
    @GenerateNodeFactory
    public abstract static class FByteToPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object byteToPosition(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "following-char", minArgs = 0, maxArgs = 0, doc = "Return the character following point, as a number.\nAt the end of the buffer or accessible region, return 0.")
    @GenerateNodeFactory
    public abstract static class FFollowingChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object followingChar() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "preceding-char", minArgs = 0, maxArgs = 0, doc = "Return the character preceding point, as a number.\nAt the beginning of the buffer or accessible region, return 0.")
    @GenerateNodeFactory
    public abstract static class FPreviousChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object previousChar() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bobp", minArgs = 0, maxArgs = 0, doc = "Return t if point is at the beginning of the buffer.\nIf the buffer is narrowed, this means the beginning of the narrowed part.")
    @GenerateNodeFactory
    public abstract static class FBobp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bobp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eobp", minArgs = 0, maxArgs = 0, doc = "Return t if point is at the end of the buffer.\nIf the buffer is narrowed, this means the end of the narrowed part.")
    @GenerateNodeFactory
    public abstract static class FEobp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eobp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bolp", minArgs = 0, maxArgs = 0, doc = "Return t if point is at the beginning of a line.")
    @GenerateNodeFactory
    public abstract static class FBolp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bolp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eolp", minArgs = 0, maxArgs = 0, doc = "Return t if point is at the end of a line.\n`End of a line' includes point being at the end of the buffer.")
    @GenerateNodeFactory
    public abstract static class FEolp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eolp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-after", minArgs = 0, maxArgs = 1, doc = "Return character in current buffer at position POS.\nPOS is an integer or a marker and defaults to point.\nIf POS is out of range, the value is nil.")
    @GenerateNodeFactory
    public abstract static class FCharAfter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charAfter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-before", minArgs = 0, maxArgs = 1, doc = "Return character in current buffer preceding position POS.\nPOS is an integer or a marker and defaults to point.\nIf POS is out of range, the value is nil.")
    @GenerateNodeFactory
    public abstract static class FCharBefore extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charBefore(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-login-name", minArgs = 0, maxArgs = 1, doc = "Return the name under which the user logged in, as a string.\nThis is based on the effective uid, not the real uid.\nAlso, if the environment variables LOGNAME or USER are set,\nthat determines the value of this function.\n\nIf optional argument UID is an integer, return the login name\nof the user with that uid, or nil if there is no such user.")
    @GenerateNodeFactory
    public abstract static class FUserLoginName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userLoginName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-real-login-name", minArgs = 0, maxArgs = 0, doc = "Return the name of the user's real uid, as a string.\nThis ignores the environment variables LOGNAME and USER, so it differs from\n`user-login-name' when running under `su'.")
    @GenerateNodeFactory
    public abstract static class FUserRealLoginName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userRealLoginName() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-uid", minArgs = 0, maxArgs = 0, doc = "Return the effective uid of Emacs, as an integer.")
    @GenerateNodeFactory
    public abstract static class FUserUid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userUid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-real-uid", minArgs = 0, maxArgs = 0, doc = "Return the real uid of Emacs, as an integer.")
    @GenerateNodeFactory
    public abstract static class FUserRealUid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userRealUid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "group-name", minArgs = 1, maxArgs = 1, doc = "Return the name of the group whose numeric group ID is GID.\nThe argument GID should be an integer or a float.\nReturn nil if a group with such GID does not exists or is not known.")
    @GenerateNodeFactory
    public abstract static class FGroupName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object groupName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "group-gid", minArgs = 0, maxArgs = 0, doc = "Return the effective gid of Emacs, as an integer.")
    @GenerateNodeFactory
    public abstract static class FGroupGid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object groupGid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "group-real-gid", minArgs = 0, maxArgs = 0, doc = "Return the real gid of Emacs, as an integer.")
    @GenerateNodeFactory
    public abstract static class FGroupRealGid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object groupRealGid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-full-name", minArgs = 0, maxArgs = 1, doc = "Return the full name of the user logged in, as a string.\nIf the full name corresponding to Emacs's userid is not known,\nreturn \"unknown\".\n\nIf optional argument UID is an integer, return the full name\nof the user with that uid, or nil if there is no such user.\nIf UID is a string, return the full name of the user with that login\nname, or nil if there is no such user.\n\nIf the full name includes commas, remove everything starting with\nthe first comma, because the \\\\='gecos\\\\=' field of the \\\\='/etc/passwd\\\\=' file\nis in general a comma-separated list.")
    @GenerateNodeFactory
    public abstract static class FUserFullName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userFullName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "system-name", minArgs = 0, maxArgs = 0, doc = "Return the host name of the machine you are running on, as a string.")
    @GenerateNodeFactory
    public abstract static class FSystemName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object systemName() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "emacs-pid", minArgs = 0, maxArgs = 0, doc = "Return the process ID of Emacs, as an integer.")
    @GenerateNodeFactory
    public abstract static class FEmacsPid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object emacsPid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Insert the arguments, either strings or characters, at point.\nPoint and after-insertion markers move forward to end up\n after the inserted text.\nAny other markers at the point of insertion remain before the text.\n\nIf the current buffer is multibyte, unibyte strings are converted\nto multibyte for insertion (see `string-make-multibyte').\nIf the current buffer is unibyte, multibyte strings are converted\nto unibyte for insertion (see `string-make-unibyte').\n\nWhen operating on binary data, it may be necessary to preserve the\noriginal bytes of a unibyte string when inserting it into a multibyte\nbuffer; to accomplish this, apply `string-as-multibyte' to the string\nand insert the result.\n\nusage: (insert &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FInsert extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insert(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-and-inherit", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Insert the arguments at point, inheriting properties from adjoining text.\nPoint and after-insertion markers move forward to end up\n after the inserted text.\nAny other markers at the point of insertion remain before the text.\n\nIf the current buffer is multibyte, unibyte strings are converted\nto multibyte for insertion (see `unibyte-char-to-multibyte').\nIf the current buffer is unibyte, multibyte strings are converted\nto unibyte for insertion.\n\nusage: (insert-and-inherit &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FInsertAndInherit extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertAndInherit(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-before-markers", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Insert strings or characters at point, relocating markers after the text.\nPoint and markers move forward to end up after the inserted text.\n\nIf the current buffer is multibyte, unibyte strings are converted\nto multibyte for insertion (see `unibyte-char-to-multibyte').\nIf the current buffer is unibyte, multibyte strings are converted\nto unibyte for insertion.\n\nIf an overlay begins at the insertion point, the inserted text falls\noutside the overlay; if a nonempty overlay ends at the insertion\npoint, the inserted text falls inside that overlay.\n\nusage: (insert-before-markers &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FInsertBeforeMarkers extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertBeforeMarkers(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-before-markers-and-inherit", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Insert text at point, relocating markers and inheriting properties.\nPoint and markers move forward to end up after the inserted text.\n\nIf the current buffer is multibyte, unibyte strings are converted\nto multibyte for insertion (see `unibyte-char-to-multibyte').\nIf the current buffer is unibyte, multibyte strings are converted\nto unibyte for insertion.\n\nusage: (insert-before-markers-and-inherit &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FInsertAndInheritBeforeMarkers extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertAndInheritBeforeMarkers(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-char", minArgs = 1, maxArgs = 3, doc = "Insert COUNT copies of CHARACTER.\nInteractively, prompt for CHARACTER using `read-char-by-name'.\nYou can specify CHARACTER in one of these ways:\n\n - As its Unicode character name, e.g. \\\"LATIN SMALL LETTER A\\\".\n   Completion is available; if you type a substring of the name\n   preceded by an asterisk `*', Emacs shows all names which include\n   that substring, not necessarily at the beginning of the name.\n\n - As a hexadecimal code point, e.g. 263A.  Note that code points in\n   Emacs are equivalent to Unicode up to 10FFFF (which is the limit of\n   the Unicode code space).\n\n - As a code point with a radix specified with #, e.g. #o21430\n   (octal), #x2318 (hex), or #10r8984 (decimal).\n\nIf called interactively, COUNT is given by the prefix argument.  If\nomitted or nil, it defaults to 1.\n\nInserting the character(s) relocates point and before-insertion\nmarkers in the same ways as the function `insert'.\n\nThe optional third argument INHERIT, if non-nil, says to inherit text\nproperties from adjoining text, if those properties are sticky.  If\ncalled interactively, INHERIT is t.")
    @GenerateNodeFactory
    public abstract static class FInsertChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertChar(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-byte", minArgs = 2, maxArgs = 3, doc = "Insert COUNT (second arg) copies of BYTE (first arg).\nBoth arguments are required.\nBYTE is a number of the range 0..255.\n\nIf BYTE is 128..255 and the current buffer is multibyte, the\ncorresponding eight-bit character is inserted.\n\nPoint, and before-insertion markers, are relocated as in the function `insert'.\nThe optional third arg INHERIT, if non-nil, says to inherit text properties\nfrom adjoining text, if those properties are sticky.")
    @GenerateNodeFactory
    public abstract static class FInsertByte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertByte(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-substring", minArgs = 2, maxArgs = 2, doc = "Return the contents of part of the current buffer as a string.\nThe two arguments START and END are character positions;\nthey can be in either order.\nThe string returned is multibyte if the buffer is multibyte.\n\nThis function copies the text properties of that part of the buffer\ninto the result string; if you don't want the text properties,\nuse `buffer-substring-no-properties' instead.")
    @GenerateNodeFactory
    public abstract static class FBufferSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSubstring(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-substring-no-properties", minArgs = 2, maxArgs = 2, doc = "Return the characters of part of the buffer, without the text properties.\nThe two arguments START and END are character positions;\nthey can be in either order.")
    @GenerateNodeFactory
    public abstract static class FBufferSubstringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSubstringNoProperties(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-string", minArgs = 0, maxArgs = 0, doc = "Return the contents of the current buffer as a string.\nIf narrowing is in effect, this function returns only the visible part\nof the buffer.\n\nThis function copies the text properties of that part of the buffer\ninto the result string; if you don\u2019t want the text properties,\nuse `buffer-substring-no-properties' instead.")
    @GenerateNodeFactory
    public abstract static class FBufferString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferString() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-buffer-substring", minArgs = 1, maxArgs = 3, doc = "Insert before point a substring of the contents of BUFFER.\nBUFFER may be a buffer or a buffer name.\nArguments START and END are character positions specifying the substring.\nThey default to the values of (point-min) and (point-max) in BUFFER.\n\nPoint and before-insertion markers move forward to end up after the\ninserted text.\nAny other markers at the point of insertion remain before the text.\n\nIf the current buffer is multibyte and BUFFER is unibyte, or vice\nversa, strings are converted from unibyte to multibyte or vice versa\nusing `string-make-multibyte' or `string-make-unibyte', which see.")
    @GenerateNodeFactory
    public abstract static class FInsertBufferSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertBufferSubstring(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "compare-buffer-substrings", minArgs = 6, maxArgs = 6, doc = "Compare two substrings of two buffers; return result as number.\nReturn -N if first string is less after N-1 chars, +N if first string is\ngreater after N-1 chars, or 0 if strings match.\nThe first substring is in BUFFER1 from START1 to END1 and the second\nis in BUFFER2 from START2 to END2.\nAll arguments may be nil.  If BUFFER1 or BUFFER2 is nil, the current\nbuffer is used.  If START1 or START2 is nil, the value of `point-min'\nin the respective buffers is used.  If END1 or END2 is nil, the value\nof `point-max' in the respective buffers is used.\nThe value of `case-fold-search' in the current buffer\ndetermines whether case is significant or ignored.")
    @GenerateNodeFactory
    public abstract static class FCompareBufferSubstrings extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compareBufferSubstrings(Object a, Object b, Object c, Object d, Object e, Object f) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "replace-buffer-contents", minArgs = 1, maxArgs = 3, doc = "Replace accessible portion of current buffer with that of SOURCE.\nSOURCE can be a buffer or a string that names a buffer.\nInteractively, prompt for SOURCE.\n\nAs far as possible the replacement is non-destructive, i.e. existing\nbuffer contents, markers, properties, and overlays in the current\nbuffer stay intact.\n\nBecause this function can be very slow if there is a large number of\ndifferences between the two buffers, there are two optional arguments\nmitigating this issue.\n\nThe MAX-SECS argument, if given, defines a hard limit on the time used\nfor comparing the buffers.  If it takes longer than MAX-SECS, the\nfunction falls back to a plain `delete-region' and\n`insert-buffer-substring'.  (Note that the checks are not performed\ntoo evenly over time, so in some cases it may run a bit longer than\nallowed).\n\nThe optional argument MAX-COSTS defines the quality of the difference\ncomputation.  If the actual costs exceed this limit, heuristics are\nused to provide a faster but suboptimal solution.  The default value\nis 1000000.\n\nThis function returns t if a non-destructive replacement could be\nperformed.  Otherwise, i.e., if MAX-SECS was exceeded, it returns\nnil.")
    @GenerateNodeFactory
    public abstract static class FReplaceBufferContents extends ELispBuiltInBaseNode {
        @Specialization
        public static Object replaceBufferContents(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subst-char-in-region", minArgs = 4, maxArgs = 5, doc = "From START to END, replace FROMCHAR with TOCHAR each time it occurs.\nIf optional arg NOUNDO is non-nil, don't record this change for undo\nand don't mark the buffer as really changed.\nBoth characters must have the same length of multi-byte form.")
    @GenerateNodeFactory
    public abstract static class FSubstCharInRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substCharInRegion(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "translate-region-internal", minArgs = 3, maxArgs = 3, doc = "Internal use only.\nFrom START to END, translate characters according to TABLE.\nTABLE is a string or a char-table; the Nth character in it is the\nmapping for the character with code N.\nIt returns the number of characters changed.")
    @GenerateNodeFactory
    public abstract static class FTranslateRegionInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object translateRegionInternal(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-region", minArgs = 2, maxArgs = 2, doc = "Delete the text between START and END.\nIf called interactively, delete the region between point and mark.\nThis command deletes buffer text without modifying the kill ring.")
    @GenerateNodeFactory
    public abstract static class FDeleteRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteRegion(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-and-extract-region", minArgs = 2, maxArgs = 2, doc = "Delete the text between START and END and return it.")
    @GenerateNodeFactory
    public abstract static class FDeleteAndExtractRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteAndExtractRegion(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widen", minArgs = 0, maxArgs = 0, doc = "Remove restrictions (narrowing) from current buffer.\n\nThis allows the buffer's full text to be seen and edited.\n\nHowever, when restrictions have been set by `with-restriction' with a\nlabel, `widen' restores the narrowing limits set by `with-restriction'.\nTo gain access to other portions of the buffer, use\n`without-restriction' with the same label.")
    @GenerateNodeFactory
    public abstract static class FWiden extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widen() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "narrow-to-region", minArgs = 2, maxArgs = 2, doc = "Restrict editing in this buffer to the current region.\nThe rest of the text becomes temporarily invisible and untouchable\nbut is not deleted; if you save the buffer in a file, the invisible\ntext is included in the file.  \\\\[widen] makes all visible again.\nSee also `save-restriction'.\n\nWhen calling from Lisp, pass two arguments START and END:\npositions (integers or markers) bounding the text that should\nremain visible.\n\nHowever, when restrictions have been set by `with-restriction' with a\nlabel, `narrow-to-region' can be used only within the limits of these\nrestrictions.  If the START or END arguments are outside these limits,\nthe corresponding limit set by `with-restriction' is used instead of the\nargument.  To gain access to other portions of the buffer, use\n`without-restriction' with the same label.")
    @GenerateNodeFactory
    public abstract static class FNarrowToRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object narrowToRegion(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--labeled-narrow-to-region", minArgs = 3, maxArgs = 3, doc = "Restrict this buffer to START-END, and label the restriction with LABEL.\n\nThis is an internal function used by `with-restriction'.")
    @GenerateNodeFactory
    public abstract static class FInternalLabeledNarrowToRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalLabeledNarrowToRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--labeled-widen", minArgs = 1, maxArgs = 1, doc = "Remove the current restriction if it is labeled with LABEL, and widen.\n\nThis is an internal function used by `without-restriction'.")
    @GenerateNodeFactory
    public abstract static class FInternalLabeledWiden extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalLabeledWiden(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "save-restriction", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true, doc = "Execute BODY, saving and restoring current buffer's restrictions.\nThe buffer's restrictions make parts of the beginning and end invisible.\n\\(They are set up with `narrow-to-region' and eliminated with `widen'.)\nThis special form, `save-restriction', saves the current buffer's\nrestrictions, including those that were set by `with-restriction' with a\nlabel argument, when it is entered, and restores them when it is exited.\nSo any `narrow-to-region' within BODY lasts only until the end of the form.\nThe old restrictions settings are restored even in case of abnormal exit\n\\(throw or error).\n\nThe value returned is the value of the last form in BODY.\n\nNote: if you are using both `save-excursion' and `save-restriction',\nuse `save-excursion' outermost:\n    (save-excursion (save-restriction ...))\n\nusage: (save-restriction &rest BODY)")
    @GenerateNodeFactory
    public abstract static class FSaveRestriction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object saveRestriction(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "ngettext", minArgs = 3, maxArgs = 3, doc = "Return the translation of MSGID (plural MSGID-PLURAL) depending on N.\nMSGID is the singular form of the string to be converted;\nuse it as the key for the search in the translation catalog.\nMSGID-PLURAL is the plural form.  Use N to select the proper translation.\nIf no message catalog is found, MSGID is returned if N is equal to 1,\notherwise MSGID-PLURAL.")
    @GenerateNodeFactory
    public abstract static class FNgettext extends ELispBuiltInBaseNode {
        @Specialization
        public static Object ngettext(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "message", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Display a message at the bottom of the screen.\nThe message also goes into the `*Messages*' buffer, if `message-log-max'\nis non-nil.  (In keyboard macros, that's all it does.)\nReturn the message.\n\nIn batch mode, the message is printed to the standard error stream,\nfollowed by a newline.\n\nThe first argument is a format control string, and the rest are data\nto be formatted under control of the string.  Percent sign (%), grave\naccent (\\\\=`) and apostrophe (\\\\=') are special in the format; see\n`format-message' for details.  To display STRING without special\ntreatment, use (message \"%s\" STRING).\n\nIf the first argument is nil or the empty string, the function clears\nany existing message; this lets the minibuffer contents show.  See\nalso `current-message'.\n\nusage: (message FORMAT-STRING &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object message(Object a, Object[] args) {
            // TODO
            System.out.println(a);
            for (Object arg : args) {
                System.out.print(arg);
            }
            System.out.println();
            return a;
        }
    }

    @ELispBuiltIn(name = "message-box", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Display a message, in a dialog box if possible.\nIf a dialog box is not available, use the echo area.\nThe first argument is a format control string, and the rest are data\nto be formatted under control of the string.  See `format-message' for\ndetails.\n\nIf the first argument is nil or the empty string, clear any existing\nmessage; let the minibuffer contents show.\n\nusage: (message-box FORMAT-STRING &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMessageBox extends ELispBuiltInBaseNode {
        @Specialization
        public static Object messageBox(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "message-or-box", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Display a message in a dialog box or in the echo area.\nIf this command was invoked with the mouse, use a dialog box if\n`use-dialog-box' is non-nil.\nOtherwise, use the echo area.\nThe first argument is a format control string, and the rest are data\nto be formatted under control of the string.  See `format-message' for\ndetails.\n\nIf the first argument is nil or the empty string, clear any existing\nmessage; let the minibuffer contents show.\n\nusage: (message-or-box FORMAT-STRING &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMessageOrBox extends ELispBuiltInBaseNode {
        @Specialization
        public static Object messageOrBox(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "current-message", minArgs = 0, maxArgs = 0, doc = "Return the string currently displayed in the echo area, or nil if none.")
    @GenerateNodeFactory
    public abstract static class FCurrentMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object currentMessage() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "propertize", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Return a copy of STRING with text properties added.\nFirst argument is the string to copy.\nRemaining arguments form a sequence of PROPERTY VALUE pairs for text\nproperties to add to the result.\n\nSee Info node `(elisp) Text Properties' for more information.\nusage: (propertize STRING &rest PROPERTIES)")
    @GenerateNodeFactory
    public abstract static class FPropertize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object propertize(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "format", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Format a string out of a format-string and arguments.\nThe first argument is a format control string.\nThe other arguments are substituted into it to make the result, a string.\n\nThe format control string may contain %-sequences meaning to substitute\nthe next available argument, or the argument explicitly specified:\n\n%s means produce a string argument.  Actually, produces any object with `princ'.\n%d means produce as signed number in decimal.\n%o means produce a number in octal.\n%x means produce a number in hex.\n%X is like %x, but uses upper case.\n%e means produce a number in exponential notation.\n%f means produce a number in decimal-point notation.\n%g means produce a number in exponential notation if the exponent would be\n   less than -4 or greater than or equal to the precision (default: 6);\n   otherwise it produces in decimal-point notation.\n%c means produce a number as a single character.\n%S means produce any object as an s-expression (using `prin1').\n\nThe argument used for %d, %o, %x, %e, %f, %g or %c must be a number.\n%o, %x, and %X treat arguments as unsigned if `binary-as-unsigned' is t\n  (this is experimental; email 32252@debbugs.gnu.org if you need it).\nUse %% to put a single % into the output.\n\nA %-sequence other than %% may contain optional field number, flag,\nwidth, and precision specifiers, as follows:\n\n  %<field><flags><width><precision>character\n\nwhere field is [0-9]+ followed by a literal dollar \"$\", flags is\n[+ #0-]+, width is [0-9]+, and precision is a literal period \".\"\nfollowed by [0-9]+.\n\nIf a %-sequence is numbered with a field with positive value N, the\nNth argument is substituted instead of the next one.  A format can\ncontain either numbered or unnumbered %-sequences but not both, except\nthat %% can be mixed with numbered %-sequences.\n\nThe + flag character inserts a + before any nonnegative number, while a\nspace inserts a space before any nonnegative number; these flags\naffect only numeric %-sequences, and the + flag takes precedence.\nThe - and 0 flags affect the width specifier, as described below.\n\nThe # flag means to use an alternate display form for %o, %x, %X, %e,\n%f, and %g sequences: for %o, it ensures that the result begins with\n\\\"0\\\"; for %x and %X, it prefixes nonzero results with \\\"0x\\\" or \\\"0X\\\";\nfor %e and %f, it causes a decimal point to be included even if the\nprecision is zero; for %g, it causes a decimal point to be\nincluded even if the precision is zero, and also forces trailing\nzeros after the decimal point to be left in place.\n\nThe width specifier supplies a lower limit for the length of the\nproduced representation.  The padding, if any, normally goes on the\nleft, but it goes on the right if the - flag is present.  The padding\ncharacter is normally a space, but it is 0 if the 0 flag is present.\nThe 0 flag is ignored if the - flag is present, or the format sequence\nis something other than %d, %o, %x, %e, %f, and %g.\n\nFor %e and %f sequences, the number after the \".\" in the precision\nspecifier says how many decimal places to show; if zero, the decimal\npoint itself is omitted.  For %g, the precision specifies how many\nsignificant digits to produce; zero or omitted are treated as 1.\nFor %s and %S, the precision specifier truncates the string to the\ngiven width.\n\nText properties, if any, are copied from the format-string to the\nproduced text.\n\nusage: (format STRING &rest OBJECTS)")
    @GenerateNodeFactory
    public abstract static class FFormat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object format(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "format-message", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Format a string out of a format-string and arguments.\nThe first argument is a format control string.\nThe other arguments are substituted into it to make the result, a string.\n\nThis acts like `format', except it also replaces each grave accent (\\\\=`)\nby a left quote, and each apostrophe (\\\\=') by a right quote.  The left\nand right quote replacement characters are specified by\n`text-quoting-style'.\n\nusage: (format-message STRING &rest OBJECTS)")
    @GenerateNodeFactory
    public abstract static class FFormatMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object formatMessage(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-equal", minArgs = 2, maxArgs = 2, doc = "Return t if two characters match, optionally ignoring case.\nBoth arguments must be characters (i.e. integers).\nCase is ignored if `case-fold-search' is non-nil in the current buffer.")
    @GenerateNodeFactory
    public abstract static class FCharEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charEqual(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "transpose-regions", minArgs = 4, maxArgs = 5, doc = "Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.\nThe regions should not be overlapping, because the size of the buffer is\nnever changed in a transposition.\n\nOptional fifth arg LEAVE-MARKERS, if non-nil, means don't update\nany markers that happen to be located in the regions.\n\nTransposing beyond buffer boundaries is an error.\n\nInteractively, STARTR1 and ENDR1 are point and mark; STARTR2 and ENDR2\nare the last two marks pushed to the mark ring; LEAVE-MARKERS is nil.\nIf a prefix argument N is given, STARTR2 and ENDR2 are the two\nsuccessive marks N entries back in the mark ring.  A negative prefix\nargument instead counts forward from the oldest mark in the mark\nring.")
    @GenerateNodeFactory
    public abstract static class FTransposeRegions extends ELispBuiltInBaseNode {
        @Specialization
        public static Object transposeRegions(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }
}
