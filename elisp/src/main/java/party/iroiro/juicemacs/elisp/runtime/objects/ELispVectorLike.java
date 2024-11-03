package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;

import java.util.AbstractList;
import java.util.Iterator;
import java.util.List;

@ExportLibrary(InteropLibrary.class)
public abstract class ELispVectorLike<T> extends AbstractList<T> implements List<T>, ELispValue, TruffleObject {
    public abstract void setUntyped(int i, Object object);

    @Override
    public boolean lispEquals(Object other) {
        if (this.getClass() == other.getClass()) {
            ELispVectorLike<?> list = (ELispVectorLike<?>) other;
            if (list.size() != size()) {
                return false;
            }
            for (int i = 0; i < size(); i++) {
                if (!BuiltInFns.FEqual.equal(get(i), list.get(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean equals(Object o) {
        return this == o;
    }

    protected static String vectorToStringHelper(String prefix, String suffix, Iterator<?> iterator) {
        StringBuilder builder = new StringBuilder(prefix);
        if (iterator.hasNext()) {
            builder.append(ELispValue.display(iterator.next()));
        }
        while (iterator.hasNext()) {
            builder.append(" ");
            builder.append(ELispValue.display(iterator.next()));
        }
        return builder.append(suffix).toString();
    }

    protected String toStringHelper(String prefix, String suffix) {
        return vectorToStringHelper(prefix, suffix, iterator());
    }

    //#region
}
