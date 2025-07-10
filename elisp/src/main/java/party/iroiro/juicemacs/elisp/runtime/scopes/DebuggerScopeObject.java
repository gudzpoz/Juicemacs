package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispFrameSlotReadNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ArrayList;

@ExportLibrary(InteropLibrary.class)
public record DebuggerScopeObject(
        ELispContext context,
        ELispLexical.Scope lexical,
        MaterializedFrame frame
) implements TruffleObject {
    @ExportMessage
    public boolean isScope() {
        return true;
    }

    @ExportMessage
    public boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    public Class<? extends TruffleLanguage<?>> getLanguage() {
        return ELispLanguage.class;
    }

    @ExportMessage
    public Object toDisplayString(boolean allowSideEffects) {
        return toString();
    }

    @ExportMessage
    public boolean hasMembers() {
        return true;
    }

    @ExportMessage
    public boolean isMemberInsertable(String member) {
        return false;
    }

    @ExportMessage
    public Object getMembers(boolean includeInternal) {
        ArrayList<ELispString> members = new ArrayList<>();
        ELispLexical.Scope scope = lexical;
        while (scope != null) {
            int limit = scope.limit();
            for (int i = limit - 1; i >= 0; i--) {
                if (scope.block().slots()[i] != ELispLexical.DYNAMIC_VARIABLE_SLOT) {
                    members.add(new ELispString(scope.block().getSymbol(i).name()));
                }
            }
            scope = scope.block().upperScope();
        }
        return new ELispVector(members.toArray());
    }

    @ExportMessage
    public boolean isMemberReadable(String member) {
        ELispLexical.@Nullable LexicalReference ref = lexical.getReference(context.intern(member));
        return ref != null;
    }

    @ExportMessage
    public Object readMember(String member) throws UnsupportedMessageException {
        ELispLexical.@Nullable LexicalReference ref = lexical.getReference(context.intern(member));
        if (ref == null) {
            throw UnsupportedMessageException.create();
        }
        ELispExpressionNode read = ELispFrameSlotReadNode.createRead(ref);
        return read.executeGeneric(frame);
    }

    @ExportMessage
    public boolean isMemberModifiable(String member) {
        return false;
    }

    @ExportMessage
    public void writeMember(String member, Object value) throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    public boolean hasScopeParent() {
        ELispLexical.@Nullable Scope parent = lexical.block().upperScope();
        return parent != null && ELispLexical.getFrameSlot(frame) != null;
    }

    @ExportMessage
    public DebuggerScopeObject getScopeParent() throws UnsupportedMessageException {
        ELispLexical.@Nullable Scope parent = lexical.block().upperScope();
        if (parent == null || ELispLexical.getFrameSlot(frame) == null) {
            throw UnsupportedMessageException.create();
        }
        return new DebuggerScopeObject(context, parent, ELispLexical.getFrameSlot(frame));
    }
}
