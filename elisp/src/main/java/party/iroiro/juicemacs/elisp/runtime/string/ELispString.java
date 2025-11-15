package party.iroiro.juicemacs.elisp.runtime.string;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.strings.*;
import com.oracle.truffle.api.strings.TruffleString.Encoding;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInData.FAref;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;
import party.iroiro.juicemacs.mule.CodingUtils;
import party.iroiro.juicemacs.mule.CodingUtils.StringAttrs;
import party.iroiro.juicemacs.mule.MuleStringBuilder;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asRanged;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;
import static party.iroiro.juicemacs.elisp.runtime.string.StringSupport.codepointIndexToByteIndex;
import static party.iroiro.juicemacs.mule.CodingUtils.*;

/// ELisp strings, encoded in `utf-8-emacs`
///
/// @see CodingUtils
@ExportLibrary(InteropLibrary.class)
public final class ELispString implements TruffleObject, ELispValue {
    public static final ELispString EMPTY = new ELispString(new byte[0], 0, 0);

    private static final int STATE_HASHED_MASK = 0b0100;
    private static final int STATE_MUTABLE_MASK = 0b1000;

    private final int length;
    private byte[] value;
    private int state;

    @Nullable
    private IntervalPieceTree<Object> intervals = null;

    //#region constructors
    private ELispString(byte[] bytes, int codepoints, int state) {
        this.value = bytes;
        this.length = codepoints;
        this.state = state;
    }

    public ELispString(StringAttrs attrs) {
        this.value = attrs.bytes();
        this.state = attrs.state();
        this.length = attrs.codepointLength();
    }

    public ELispString(String init) {
        this(StringAttrs.calculate(CodingUtils.fromString(init)));
    }

    public static ELispString ofJava(String init) {
        return new ELispString(StringAttrs.calculate(CodingUtils.fromString(init)));
    }

    public static ELispString ofBytes(byte[] unibyteBytes) {
        return new ELispString(
                unibyteBytes,
                unibyteBytes.length,
                CodingUtils.calculateUnibyteState(unibyteBytes)
        );
    }

    public static ELispString ofAsciiBytes(byte[] unibyteBytes) {
        return new ELispString(
                unibyteBytes,
                unibyteBytes.length,
                0
        );
    }

    public static ELispString ofUtf8(byte[] bytes) {
        return new ELispString(StringAttrs.calculate(bytes));
    }

    public static ELispString ofKnown(byte[] bytes, int codepoints, int state) {
        return new ELispString(bytes, codepoints, state);
    }
    //#endregion constructors

    public int state() {
        return state & STATE_MASK;
    }

    public boolean isUnibyte() {
        return isUnibyteState(state) || value.length == length;
    }

    public boolean isAscii() {
        int state = state();
        return state == STATE_ASCII || (state != STATE_BYTES && value.length == length);
    }

    public boolean isVariableLength() {
        return state() != STATE_ASCII;
    }

    public boolean isUtf8Compatible() {
        int state = state();
        return state == STATE_ASCII || state == STATE_UTF_8;
    }

    public int length() {
        return length;
    }

    public byte[] bytes() {
        return value;
    }

    public byte[] asMutable() {
        if ((state & STATE_MUTABLE_MASK) == 0) {
            if ((state & STATE_HASHED_MASK) != 0) {
                throw ELispSignals.error("must not modify a hashed string");
            }
            value = value.clone();
            state |= STATE_MUTABLE_MASK;
        }
        return value;
    }

    public boolean isImmutable() {
        return (state & STATE_MUTABLE_MASK) == 0;
    }

    /// Used by symbols, setting hash and forbidding modification
    public void setImmutable() {
        this.lispHashCode(0);
    }

    public int codePointAt(int index) {
        return (int) FAref.arefStringUncached(this, index);
    }

    public CharIterator iterator(int start) {
        CharIterator i = new CharIterator(bytes(), isUnibyte());
        if (start < 0) {
            start = length + start;
        }
        start = asRanged(start, 0, length);
        i.skipChars(start);
        return i;
    }

    public CharIterator reverseIterator() {
        return new CharIterator(bytes(), isUnibyte(), length, value.length);
    }

    @Override
    @TruffleBoundary
    public String toString() {
        int state = state();
        if (state == STATE_ASCII || state == STATE_UTF_8) {
            return new String(bytes(), StandardCharsets.UTF_8);
        }
        return toDisplayString(true);
    }

    @Override
    public void display(ELispPrint print) {
        print.startString();
        boolean bytes = state() == STATE_BYTES;
        CharIterator i = iterator(0);
        while (i.hasNext()) {
            int c = i.nextInt();
            print.print(bytes && c >= 0x80 ? c + 0x3FFF00 : c);
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
    public TruffleString asTruffleString() {
        return TruffleString.fromByteArrayUncached(value, Encoding.UTF_8);
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
        int state = state();
        int oState = s.state();
        return (
                (state | oState) == STATE_BYTES // both are unibyte strings
                        || (state != STATE_BYTES) == (oState != STATE_BYTES) // both utf
        ) && Arrays.equals(value, s.value);
    }
    @Override
    public int lispHashCode(int depth) {
        if ((state & STATE_HASHED_MASK) != 0) {
            return state >> 4;
        }
        if ((state & STATE_MUTABLE_MASK) != 0) {
            state = state & ~STATE_MUTABLE_MASK;
        }
        int computed = (state() == STATE_BYTES ? 31 : 0) + Arrays.hashCode(value);
        computed = computed << 4;
        state = (state & 0b1111) | STATE_HASHED_MASK | computed;
        return computed >> 4;
    }

    public boolean startsWithAscii(String s) {
        if (s.length() > value.length) {
            return false;
        }
        for (int i = 0; i < s.length(); i++) {
            if (value[i] != s.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    public static final class Builder extends MuleStringBuilder {
        public Builder() {
            super(32);
        }
        public Builder(int initialCapacity) {
            super(initialCapacity);
        }

        public Builder append(ELispString s) {
            return appendInner(s, s.length(), 0, s.bytes().length);
        }

        public Builder append(int codePoint) {
            appendCodePoint(codePoint);
            return this;
        }

        public Builder append(ELispString s, int start, int end) {
            int startByte = codepointIndexToByteIndex(s, start, 0);
            int endByte = codepointIndexToByteIndex(s, end - start, startByte);
            return appendInner(s, end - start, startByte, endByte);
        }

        private Builder appendInner(ELispString s, int codepoints, int startBytes, int endBytes) {
            byte[] bytes = s.bytes();
            checkRange(bytes, startBytes, endBytes);
            this.codepoints += codepoints;
            int oState = s.state();
            int newState = oState | this.state;
            if (newState == STATE_EMACS) {
                if (this.state == STATE_BYTES) {
                    convertToMultibyte();
                }
                if (oState == STATE_BYTES) {
                    this.state = STATE_EMACS;
                    for (int i = startBytes; i < endBytes; i++) {
                        byte b = bytes[i];
                        writeRawByte(b, this);
                    }
                    return this;
                }
            }
            this.state = newState;
            writeBytes(bytes, startBytes, endBytes - startBytes);
            return this;
        }

        public ELispString build() {
            return ELispString.ofKnown(toByteArray(), codepoints, state);
        }
    }
}
