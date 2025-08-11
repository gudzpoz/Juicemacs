package party.iroiro.juicemacs.elisp.runtime.string;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.strings.*;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInData.FAref;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;
import party.iroiro.juicemacs.piecetree.StringNodes;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;

import java.util.Iterator;
import java.util.List;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;
import static party.iroiro.juicemacs.elisp.runtime.string.StringSupport.*;

@ExportLibrary(InteropLibrary.class)
public final class ELispString implements TruffleObject, ELispValue {
    private static final TruffleString.CodePointLengthNode LENGTH_NODE = TruffleString.CodePointLengthNode.create();

    private AbstractTruffleString value;
    private int state;

    @Nullable
    private IntervalPieceTree<Object> intervals = null;

    /**
     * @param init the string value, no copy is performed
     * @param state the string state
     */
    public ELispString(AbstractTruffleString init, int state) {
        this.value = init;
        this.state = state;
    }

    public ELispString(AbstractTruffleString init) {
        this.value = init;
        if (value.getStringCompactionLevelUncached(TruffleString.Encoding.UTF_32) == TruffleString.CompactionLevel.S1) {
            TruffleString.CodeRange range = value.getCodeRangeUncached(TruffleString.Encoding.UTF_32);
            state = range == TruffleString.CodeRange.ASCII ? StringSupport.STATE_ASCII : StringSupport.STATE_UTF32;
        } else {
            state = StringSupport.STATE_UTF32;
        }
    }

    public ELispString(byte[] unibyteBytes) {
        this.value = StringSupport.fromBytes(unibyteBytes);
        this.state = STATE_BYTES;
    }

    public ELispString(String init) {
        this(StringSupport.tString(init));
    }

    public AbstractTruffleString value() {
        return value;
    }

    public int state() {
        return state;
    }

    public int length() {
        return LENGTH_NODE.execute(value, TruffleString.Encoding.UTF_32);
    }

    public MutableTruffleString asMutableTruffleString(MutableTruffleString.AsMutableTruffleStringNode convert) {
        MutableTruffleString mut = convert.execute(value, TruffleString.Encoding.UTF_32);
        value = mut;
        return mut;
    }

    public int codePointAt(int index) {
        return (int) FAref.arefStringUncached(this, index);
    }

    public Iterator<Long> iteratorUncached() {
        return new Iterator<>() {
            final TruffleStringIterator i = value.createCodePointIteratorUncached(UTF_32);

            @Override
            public boolean hasNext() {
                return i.hasNext();
            }

            @Override
            public Long next() {
                int c = i.nextUncached();
                if (state == STATE_BYTES) {
                    return (long) (c < 128 ? c : c + 0x3FFF00);
                }
                return (long) c;
            }
        };
    }

    public PrimitiveIterator.OfInt iterator(int start) {
        int length = length();
        return new PrimitiveIterator.OfInt() {
            int i = start;

            @Override
            public int nextInt() {
                int c = StringNodes.charAt(value, i++);
                if (state == STATE_BYTES) {
                    return c < 128 ? c : c + 0x3FFF00;
                }
                return c;
            }

            @Override
            public boolean hasNext() {
                return i < length;
            }
        };
    }

    @Override
    public String toString() {
        // TODO: transcode
        return value().toJavaStringUncached();
    }

    @Override
    public void display(ELispPrint print) {
        print.startString();
        TruffleStringIterator i = value.createCodePointIteratorUncached(TruffleString.Encoding.UTF_32);
        while (i.hasNext()) {
            int c = i.nextUncached();
            print.print(state == STATE_BYTES && c >= 0x80 ? c + 0x3FFF00 : c);
        }
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

    @TruffleBoundary
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
    public TruffleString asTruffleString(@Cached TruffleString.AsTruffleStringNode convertNode) {
        return convertNode.execute(value, TruffleString.Encoding.UTF_32);
    }
    public TruffleString asTruffleStringUncached() {
        return TruffleString.AsTruffleStringNode.getUncached().execute(value, TruffleString.Encoding.UTF_32);
    }
    @ExportMessage
    @TruffleBoundary
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
        if (!(other instanceof ELispString s)) {
            return false;
        }
        return (state == s.state || (state | s.state) != STATE_BYTES)
                && value.equalsUncached(s.value, TruffleString.Encoding.UTF_32);
    }
    @Override
    public int lispHashCode(int depth) {
        return (state == STATE_BYTES ? 31 : 0) + value.hashCode();
    }

    public boolean startsWithUncached(String s) {
        StringSupport.StartsWithStringNode uncached = StringSupportFactory.StartsWithStringNodeGen.getUncached();
        return uncached.execute(uncached, this, new ELispString(s));
    }
}
