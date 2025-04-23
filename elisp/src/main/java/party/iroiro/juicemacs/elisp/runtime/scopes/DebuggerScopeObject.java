package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNode;
import party.iroiro.juicemacs.elisp.nodes.ELispFrameSlotNodeFactory;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ArrayList;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asSym;

@ExportLibrary(InteropLibrary.class)
public record DebuggerScopeObject(
        ELispContext context,
        ELispLexical lexical,
        Frame frame
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
        Iterable<?> i = ELispCons.iterate(lexical.toAssocList(frame));
        for (Object o : i) {
            if (o instanceof ELispCons cons) {
                members.add(new ELispString(asSym(cons.car()).name()));
            }
        }
        return new ELispVector(members.toArray());
    }

    @ExportMessage
    public boolean isMemberReadable(String member) {
        ELispLexical.@Nullable LexicalReference ref = lexical.getLexicalReference(frame, context.intern(member));
        return ref != null;
    }

    @ExportMessage
    public Object readMember(String member) {
        ELispLexical.@Nullable LexicalReference ref = lexical.getLexicalReference(frame, context.intern(member));
        if (ref == null) {
            throw new UnsupportedOperationException();
        }
        ELispFrameSlotNode.ELispFrameSlotReadNode read = ELispFrameSlotNodeFactory.ELispFrameSlotReadNodeGen.create(ref.index(), ref.frame());
        return read.executeGeneric((VirtualFrame) frame);
    }

    @ExportMessage
    public boolean isMemberModifiable(String member) {
        return false;
    }

    @ExportMessage
    public void writeMember(String member, Object value) {
        throw new UnsupportedOperationException();
    }
}
