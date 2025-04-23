package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@ExportLibrary(InteropLibrary.class)
public final class ELispVector extends AbstractELispVector implements TruffleObject {
    public ELispVector(List<Object> inner) {
        super(inner.toArray());
    }

    public ELispVector(Object[] inner) {
        super(inner);
    }

    public ELispVector(ELispVector other) {
        super(Arrays.copyOf(other.inner, other.inner.length));
    }

    public ELispVector(int count, Object value) {
        this(Collections.nCopies(count, value));
    }

    public ELispVector reverse() {
        return new ELispVector(Arrays.asList(inner).reversed());
    }

    @Override
    public void display(ELispPrint print) {
        displayHelper(print, "[", "]");
    }

    //#region InteropLibrary
    @ExportMessage
    public boolean hasArrayElements() {
        return true;
    }
    @ExportMessage
    public long getArraySize() {
        return inner.length;
    }
    @ExportMessage
    public boolean isArrayElementReadable(long index) {
        return index >= 0 && index < inner.length;
    }
    @ExportMessage
    public boolean isArrayElementModifiable(long index) {
        return isArrayElementReadable(index);
    }
    @ExportMessage
    public Object readArrayElement(long index) throws InvalidArrayIndexException {
        if (!isArrayElementModifiable(index)) {
            throw InvalidArrayIndexException.create(index);
        }
        return get(Math.toIntExact(index));
    }
    @ExportMessage
    public void writeArrayElement(long index, Object element) throws InvalidArrayIndexException {
        if (!isArrayElementModifiable(index)) {
            throw InvalidArrayIndexException.create(index);
        }
        set(Math.toIntExact(index), element);
    }
    //#endregion InteropLibrary
}
