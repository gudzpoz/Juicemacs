package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.strings.*;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;

import java.util.Iterator;
import java.util.List;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

@ExportLibrary(InteropLibrary.class)
public final class ELispString implements TruffleObject, ELispValue {

    private MuleString value;

    @Nullable
    private IntervalPieceTree<Object> intervals = null;

    /**
     * @param init the string value, no copy is performed
     */
    public ELispString(MuleString init) {
        this.value = init;
    }

    public ELispString(String init) {
        this(MuleString.fromString(init));
    }

    public ELispString(byte[] latin1) {
        this(MuleString.fromLatin1(latin1));
    }

    public long length() {
        return value.length();
    }

    public long codePointAt(long idx) {
        return value.codePointAt(idx);
    }

    public MuleString value() {
        return value;
    }

    @Nullable
    public static Long toValidChar(Object a) {
        if (a instanceof Long l) {
            return Character.isValidCodePoint(Math.toIntExact(l)) ? l : null;
        }
        return null;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    @Override
    public void display(ELispPrint print) {
        print.startString();
        print.print(toString().replace("\"", "\\\""));
        print.endString();
    }

    public boolean hasIntervals() {
        return intervals != null;
    }

    public void forRangeProperties(long i, IntervalPieceTree.IntervalConsumer<Object, ?> propertiesConsumer) {
        if (intervals == null) {
            return;
        }
        intervals.forPropertiesIn(i, 1, false, propertiesConsumer);
    }

    public void forProperties(IntervalPieceTree.IntervalConsumer<Object, ?> propertiesConsumer) {
        if (intervals == null) {
            return;
        }
        intervals.forPropertiesIn(0, Long.MAX_VALUE, false, propertiesConsumer);
    }

    @Nullable
    public IntervalPieceTree<Object> getIntervals() {
        return intervals;
    }

    public void syncFromPlist(List<Object> list) {
        if ((list.size() - 1) % 3 != 0) {
            throw ELispSignals.error("Odd length text property list");
        }
        intervals = new IntervalPieceTree<>();
        intervals.insert(0, length(), null);
        for (int i = 1; i < list.size(); i += 3) {
            long start = (long) list.get(i);
            long end = (long) list.get(i + 1);
            Object props = list.get(i + 2);
            if (isNil(props) || (props instanceof ELispCons cons && cons.size() % 2 == 0)) {
                intervals.putPropertiesFor(start, end - start, props);
            } else {
                throw ELispSignals.argsOutOfRange(start, end);
            }
        }
    }

    //#region InteropLibrary exports
    @ExportMessage
    public boolean isString() {
        return true;
    }
    @ExportMessage
    public String asString() {
        return toString();
    }
    @ExportMessage
    public TruffleString asTruffleString() {
        return TruffleString.fromJavaStringUncached(value.toString(), TruffleString.Encoding.UTF_32);
    }
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return ELispPrint.toString(this).toString();
    }
    @ExportMessage
    public boolean hasLanguage() {
        return true;
    }
    @ExportMessage
    public Class<? extends TruffleLanguage<?>> getLanguage() {
        return ELispLanguage.class;
    }
    //#endregion InteropLibrary exports

    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispString s && value.equals(s.value);
    }
    @Override
    public int lispHashCode() {
        return value.hashCode();
    }

    public Iterator<Object> iterator() {
        PrimitiveIterator.OfInt i = codePointIterator();
        return new Iterator<>() {
            @Override
            public boolean hasNext() {
                return i.hasNext();
            }

            @Override
            public Object next() {
                return (long) i.nextInt();
            }
        };
    }

    public PrimitiveIterator.OfInt codePointIterator() {
        return value.iterator(0);
    }
}
