package party.iroiro.juicemacs.elisp.runtime.objects;

import com.lodborg.intervaltree.IntegerInterval;
import com.lodborg.intervaltree.Interval;
import com.lodborg.intervaltree.IntervalTree;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.strings.*;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

@ExportLibrary(InteropLibrary.class)
public final class ELispString implements TruffleObject, ELispValue {

    public static final TruffleString.Encoding ENCODING = TruffleString.Encoding.UTF_32;

    public static MutableTruffleString from(String str) {
        Builder builder = new Builder();
        builder.appendString(str);
        return builder.toTruffleString();
    }

    @Override
    public boolean lispEquals(Object other) {
        return other instanceof ELispString s && value.equals(s.value);
    }

    public long codepointCount() {
        return value.codePointLengthUncached(ENCODING);
    }

    public long codepointAt(int idx) {
        return value.codePointAtIndexUncached(idx, ENCODING);
    }

    public AbstractTruffleString value() {
        return value;
    }

    @CompilerDirectives.TruffleBoundary
    public ELispString reverse() {
        TruffleStringIterator i = value.createBackwardCodePointIteratorUncached(ENCODING);
        Builder builder = new Builder();
        while (i.hasPrevious()) {
            builder.appendCodePoint(i.previousUncached());
        }
        return new ELispString(builder.toTruffleString());
    }

    public Iterator<Object> iterator() {
        TruffleStringIterator i = codePointIterator();
        return new Iterator<>() {
            @Override
            public boolean hasNext() {
                return i.hasNext();
            }

            @Override
            public Object next() {
                return (long) i.nextUncached();
            }
        };
    }

    public TruffleStringIterator codePointIterator() {
        return value.createCodePointIteratorUncached(ENCODING);
    }

    public TruffleString toTruffleString() {
        return value.asTruffleStringUncached(ENCODING);
    }

    public static final class Properties extends IntegerInterval {
        Object propertyList;

        Properties(int start, int end, Object propertyList) {
            super(start, end, Bounded.CLOSED_LEFT); // NOPMD
            this.propertyList = propertyList;
        }

        @Override
        protected Properties create() {
            return new Properties(0, 0, NIL);
        }

        @Override
        public int hashCode() {
            // OpenJDK Arrays::hashCode
            return 31 + super.hashCode() * 31 + propertyList.hashCode() * 31 * 31;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof Properties props
                    && propertyList.equals(props.propertyList)
                    && super.equals(obj);
        }
    }

    private final MutableTruffleString value;
    private final IntervalTree<Integer> intervals;

    /**
     * @param init the string value, no copy is performed
     */
    public ELispString(MutableTruffleString init) {
        this.value = init;
        this.intervals = new IntervalTree<>();
    }

    public ELispString(TruffleString init) {
        this(init.asMutableTruffleStringUncached(ENCODING));
    }

    public ELispString(String init) {
        this(from(init));
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
    public String display() {
        return "\"" + toString().replace("\"", "\\\"") + "\"";
    }

    public boolean isMultibyte() {
        return true;
    }

    public int intervals() {
        return intervals.size();
    }

    public void forRangeProperties(int i, Consumer<Object> propertiesConsumer) {
        Set<Interval<Integer>> query = intervals.query(i); // NOPMD
        query.forEach((props) ->
                propertiesConsumer.accept(((Properties) props).propertyList));
    }

    public void forProperties(Consumer<Object> propertiesConsumer) {
        intervals.forEach((props) -> propertiesConsumer.accept(((Properties) props).propertyList));
    }

    public void syncFromPlist(List<Object> list) {
        if ((list.size() - 1) % 3 != 0) {
            throw ELispSignals.error("Odd length text property list");
        }
        for (int i = 1; i < list.size(); i += 3) {
            long start = (long) list.get(i);
            long end = (long) list.get(i + 1);
            Object props = list.get(i + 2);
            if (ELispSymbol.isNil(props) || (props instanceof ELispCons cons && cons.size() % 2 == 0)) {
                intervals.add(new Properties((int) start, (int) end, props));
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
        return value.asTruffleStringUncached(ENCODING);
    }
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return display();
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

    public static final class Builder {
        private final TruffleStringBuilder builder = TruffleStringBuilder.createUTF32();

        public void appendCodePoint(int codepoint) {
            builder.appendCodePointUncached(codepoint);
        }

        public void appendString(String s) {
            builder.appendStringUncached(TruffleString.fromJavaStringUncached(s, ENCODING));
        }

        public MutableTruffleString toTruffleString() {
            return builder.toStringUncached().asMutableTruffleStringUncached(ENCODING);
        }
    }
}
