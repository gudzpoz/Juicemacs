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

    @ELispBuiltIn(name = "char-to-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charToString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "byte-to-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FByteToString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object byteToString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToChar(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPoint extends ELispBuiltInBaseNode {
        @Specialization
        public static Object point() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "goto-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGotoChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gotoChar(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "region-beginning", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRegionBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Object regionBeginning() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "region-end", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRegionEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object regionEnd() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mark-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMarkMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object markMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-pos-property", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGetPosProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getPosProperty(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-field", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteField extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteField(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-string", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFieldString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-string-no-properties", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFieldStringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldStringNoProperties(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-beginning", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFieldBeginning extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldBeginning(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "field-end", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFieldEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fieldEnd(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "constrain-to-field", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FConstrainToField extends ELispBuiltInBaseNode {
        @Specialization
        public static Object constrainToField(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "pos-bol", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPosBol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posBol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "line-beginning-position", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLineBeginningPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lineBeginningPosition(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "pos-eol", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPosEol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object posEol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "line-end-position", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLineEndPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lineEndPosition(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "save-excursion", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSaveExcursion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object saveExcursion(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "save-current-buffer", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSaveCurrentBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object saveCurrentBuffer(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-size", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-min", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMin extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMin() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-min-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMinMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMinMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-max", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMax extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMax() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "point-max-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPointMaxMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object pointMaxMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "gap-position", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGapPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gapPosition() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "gap-size", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGapSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gapSize() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "position-bytes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPositionBytes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object positionBytes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "byte-to-position", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FByteToPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Object byteToPosition(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "following-char", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FFollowingChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object followingChar() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "preceding-char", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPreviousChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object previousChar() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bobp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBobp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bobp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eobp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEobp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eobp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bolp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBolp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bolp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eolp", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEolp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eolp() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-after", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharAfter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charAfter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-before", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharBefore extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charBefore(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-login-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUserLoginName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userLoginName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-real-login-name", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUserRealLoginName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userRealLoginName() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-uid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUserUid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userUid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-real-uid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUserRealUid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userRealUid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "group-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGroupName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object groupName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "group-gid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGroupGid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object groupGid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "group-real-gid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGroupRealGid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object groupRealGid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "user-full-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUserFullName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object userFullName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "system-name", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSystemName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object systemName() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "emacs-pid", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEmacsPid extends ELispBuiltInBaseNode {
        @Specialization
        public static Object emacsPid() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsert extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insert(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-and-inherit", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsertAndInherit extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertAndInherit(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-before-markers", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsertBeforeMarkers extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertBeforeMarkers(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-before-markers-and-inherit", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FInsertAndInheritBeforeMarkers extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertAndInheritBeforeMarkers(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-char", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInsertChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertChar(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-byte", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInsertByte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertByte(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-substring", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBufferSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSubstring(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-substring-no-properties", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBufferSubstringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSubstringNoProperties(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-string", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FBufferString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferString() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-buffer-substring", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInsertBufferSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertBufferSubstring(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "compare-buffer-substrings", minArgs = 6, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FCompareBufferSubstrings extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compareBufferSubstrings(Object a, Object b, Object c, Object d, Object e, Object f) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "replace-buffer-contents", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReplaceBufferContents extends ELispBuiltInBaseNode {
        @Specialization
        public static Object replaceBufferContents(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "subst-char-in-region", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FSubstCharInRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substCharInRegion(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "translate-region-internal", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTranslateRegionInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object translateRegionInternal(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-region", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteRegion(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-and-extract-region", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDeleteAndExtractRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteAndExtractRegion(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widen", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FWiden extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widen() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "narrow-to-region", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNarrowToRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object narrowToRegion(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--labeled-narrow-to-region", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalLabeledNarrowToRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalLabeledNarrowToRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--labeled-widen", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalLabeledWiden extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalLabeledWiden(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "save-restriction", minArgs = 0, maxArgs = 0, varArgs = true, rawArg = true)
    @GenerateNodeFactory
    public abstract static class FSaveRestriction extends ELispBuiltInBaseNode {
        @Specialization
        public static Object saveRestriction(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "ngettext", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FNgettext extends ELispBuiltInBaseNode {
        @Specialization
        public static Object ngettext(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "message", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object message(Object a, Object[] args) {
            // TODO
            System.out.print(a);
            for (Object arg : args) {
                System.out.print('\t');
                System.out.print(arg);
            }
            System.out.println();
            return a;
        }
    }

    @ELispBuiltIn(name = "message-box", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMessageBox extends ELispBuiltInBaseNode {
        @Specialization
        public static Object messageBox(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "message-or-box", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMessageOrBox extends ELispBuiltInBaseNode {
        @Specialization
        public static Object messageOrBox(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "current-message", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object currentMessage() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "propertize", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FPropertize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object propertize(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "format", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFormat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object format(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "format-message", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFormatMessage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object formatMessage(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-equal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charEqual(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "transpose-regions", minArgs = 4, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FTransposeRegions extends ELispBuiltInBaseNode {
        @Specialization
        public static Object transposeRegions(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }
}
