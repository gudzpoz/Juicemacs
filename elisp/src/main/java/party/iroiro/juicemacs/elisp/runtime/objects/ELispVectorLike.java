package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.AbstractList;
import java.util.Iterator;
import java.util.List;

@ExportLibrary(InteropLibrary.class)
public abstract class ELispVectorLike<T> extends AbstractList<T> implements List<T>, ELispValue, TruffleObject {
    public abstract void setUntyped(int i, Object object);

    @Override
    public final boolean equals(Object o) {
        // AbstractList overrides equals and hashCode, and we need to get it back.
        return this == o;
    }

    @Override
    public final int hashCode() {
        // AbstractList overrides equals and hashCode, and we need to get it back.
        return System.identityHashCode(this);
    }

    @Override
    public String toString() {
        return ELispPrint.toString(this).toString();
    }

    protected void vectorPrintHelper(ELispPrint print, String prefix, String suffix, Iterator<?> iterator) {
        print.print(prefix).start(this);
        if (iterator.hasNext()) {
            print.print(iterator.next());
        }
        while (iterator.hasNext()) {
            print.sep().print(iterator.next());
        }
        print.print(suffix).end();
    }

    protected void displayHelper(ELispPrint print, String prefix, String suffix) {
        vectorPrintHelper(print, prefix, suffix, iterator());
    }

    //#region Interop Array Elements
    @ExportMessage
    public boolean hasArrayElements() {
        return true;
    }
    @ExportMessage
    public long getArraySize() {
        return size();
    }
    @ExportMessage
    public boolean isArrayElementReadable(long index) {
        return index >= 0 && index < size();
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
        setUntyped(Math.toIntExact(index), element);
    }
    @ExportMessage
    public boolean isArrayElementInsertable(long ignored) {
        return false;
    }
    @ExportMessage
    public boolean isArrayElementRemovable(long ignored) {
        return false;
    }
    @ExportMessage
    public void removeArrayElement(long ignored) throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }
    //#endregion Interop Array Elements
}
