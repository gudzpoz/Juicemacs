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
import party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ArrayList;

@ExportLibrary(InteropLibrary.class)
public record DebuggerScopeObject(
        ELispContext context,
        ELispLexical lexical,
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
        ELispLexical scope = lexical;
        while (scope != null) {
            for (ELispSymbol symbol : scope.symbols()) {
                members.add(new ELispString(symbol.name()));
            }
            scope = scope.parentScope();
        }
        return new ELispVector(members.toArray());
    }

    @ExportMessage
    public boolean isMemberReadable(String member) {
        int ref = lexical.getSlot(context.intern(member));
        return ref != ELispLexical.NON_VAR_SLOT0;
    }

    @ExportMessage
    public Object readMember(String member) throws UnsupportedMessageException {
        ELispLexical.@Nullable LexicalReference ref = lexical.getReference(context.intern(member));
        if (ref == null) {
            throw UnsupportedMessageException.create();
        }
        ELispExpressionNode read = ELispFrameSlotNode.createRead(ref.index(), ref.frame());
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
        return lexical.parentScope() != null;
    }

    @ExportMessage
    public DebuggerScopeObject getScopeParent() throws UnsupportedMessageException {
        @Nullable ELispLexical parent = lexical.parentScope();
        if (parent == null) {
            throw UnsupportedMessageException.create();
        }
        return new DebuggerScopeObject(
                context,
                parent,
                lexical.materializedParent() == null ? frame : lexical.materializedParent()
        );
    }
}
