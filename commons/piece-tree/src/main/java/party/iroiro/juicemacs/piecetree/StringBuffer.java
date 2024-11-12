package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.strings.*;
import org.eclipse.jdt.annotation.Nullable;

import java.util.Objects;

public final class StringBuffer {
    public static final TruffleString.Encoding ENCODING = TruffleString.Encoding.UTF_32;

    private static final TruffleString.CodePointLengthNode LENGTH = TruffleString.CodePointLengthNode.create();
    private static final TruffleString.SubstringNode SUBSTRING = TruffleString.SubstringNode.create();
    private static final TruffleString.CodePointAtIndexNode CHAR_AT = TruffleString.CodePointAtIndexNode.create();
    private static final TruffleString.ConcatNode CONCAT = TruffleString.ConcatNode.create();
    private static final TruffleString.FromJavaStringNode FROM_JAVA_STRING =
            TruffleString.FromJavaStringNode.create();
    private static final TruffleString.CreateCodePointIteratorNode CODE_POINT_ITERATOR =
            TruffleString.CreateCodePointIteratorNode.create();
    private static final TruffleStringIterator.NextNode ITERATOR_NEXT = TruffleStringIterator.NextNode.create();
    private static final TruffleStringIterator.PreviousNode ITERATOR_PREV = TruffleStringIterator.PreviousNode.create();

    private static final TruffleStringBuilder.AppendCodePointNode APPEND_CODE_POINT =
            TruffleStringBuilder.AppendCodePointNode.create();
    private static final TruffleStringBuilder.AppendStringNode APPEND_STRING =
            TruffleStringBuilder.AppendStringNode.create();
    private static final TruffleStringBuilder.ToStringNode TO_STRING =
            TruffleStringBuilder.ToStringNode.create();

    @Nullable
    private final TruffleStringBuilderUTF32 buffer;
    public OrderedIntArrayList lineStarts;
    private TruffleString cached;

    public StringBuffer(TruffleString buffer, boolean readonly) {
        this(buffer, PieceTreeBase.createLineStartsFast(buffer, readonly), readonly);
    }

    public StringBuffer(TruffleString buffer, OrderedIntArrayList lineStarts, boolean readonly) {
        this.cached = buffer;
        if (readonly) {
            this.buffer = null;
        } else {
            this.buffer = TruffleStringBuilderUTF32.createUTF32();
            this.buffer.appendStringUncached(cached);
        }
        this.lineStarts = lineStarts;
    }

    public static int length(AbstractTruffleString string) {
        return LENGTH.execute(string, ENCODING);
    }

    public static int charAt(AbstractTruffleString string, int offset) {
        return CHAR_AT.execute(string, offset, ENCODING);
    }

    public static TruffleStringIterator iterator(AbstractTruffleString string) {
        return CODE_POINT_ITERATOR.execute(string, ENCODING);
    }

    public static int iteratorNext(TruffleStringIterator iterator) {
        return ITERATOR_NEXT.execute(iterator);
    }

    public static TruffleString substringLazy(AbstractTruffleString string, int start, int end) {
        return SUBSTRING.execute(string, start, end - start, ENCODING, true);
    }

    public static TruffleString substring(AbstractTruffleString string, int start, int end) {
        return SUBSTRING.execute(string, start, end - start, ENCODING, false);
    }

    public static TruffleString concat(AbstractTruffleString string1, AbstractTruffleString string2) {
    return CONCAT.execute(string1, string2, ENCODING, true);
    }

    public static void appendCodepoint(TruffleStringBuilderUTF32 sb, int codepoint) {
        APPEND_CODE_POINT.execute(sb, codepoint);
    }

    public static void appendSubstring(TruffleStringBuilderUTF32 sb, StringBuffer buffer,
                                       int start, int end) {
        APPEND_STRING.execute(sb, buffer.substringLazy(start, end));
    }

    public static TruffleString trimEol(AbstractTruffleString string) {
        int start = 0;
        int end = length(string);
        if (start < end && charAt(string, end - 1) == '\n') {
            end--;
        }
        if (start < end && charAt(string, end - 1) == '\r') {
            end--;
        }
        return substringLazy(string, start, end);
    }

    public static void appendSubstringTrimEol(TruffleStringBuilderUTF32 sb, StringBuffer buffer,
                                        int start, int end) {
        if (start < end && buffer.charAt(end - 1) == '\n') {
            end--;
        }
        if (start < end && buffer.charAt(end - 1) == '\r') {
            end--;
        }
        appendSubstring(sb, buffer, start, end);
    }

    public static void appendString(TruffleStringBuilderUTF32 sb, AbstractTruffleString string) {
        APPEND_STRING.execute(sb, string);
    }

    public static TruffleString replaceAllEol(TruffleString string, TruffleString newEol) {
        TruffleStringBuilderUTF32 sb = TruffleStringBuilderUTF32.createUTF32();
        TruffleStringIterator i = iterator(string);
        while (i.hasNext()) {
            int c = iteratorNext(i);
            if (c == '\r') {
                appendString(sb, newEol);
                if (i.hasNext()) {
                    c = iteratorNext(i);
                    if (c != '\n') {
                        ITERATOR_PREV.execute(i);
                    }
                }
            } else if (c == '\n') {
                appendString(sb, newEol);
            } else {
                appendCodepoint(sb, c);
            }
        }
        return toTString(sb);
    }

    public static TruffleString fromString(String buffer) {
        return FROM_JAVA_STRING.execute(buffer, ENCODING);
    }

    public static TruffleString toTString(TruffleStringBuilderUTF32 sb) {
        return TO_STRING.execute(sb, false);
    }

    public int length() {
        return LENGTH.execute(cached, ENCODING);
    }

    public boolean isEmpty() {
        return length() == 0;
    }

    public TruffleString substring(int start, int end) {
        return SUBSTRING.execute(cached, start, end - start, ENCODING, false);
    }

    public TruffleString substringLazy(int start, int end) {
        return SUBSTRING.execute(cached, start, end - start, ENCODING, true);
    }

    public int charAt(int offset) {
        return CHAR_AT.execute(cached, offset, ENCODING);
    }

    public StringBuffer append(int codepoint) {
        TruffleStringBuilderUTF32 sb = Objects.requireNonNull(buffer);
        APPEND_CODE_POINT.execute(sb, codepoint);
        updateCache(sb);
        return this;
    }

    public StringBuffer append(AbstractTruffleString string) {
        TruffleStringBuilderUTF32 sb = Objects.requireNonNull(buffer);
        APPEND_STRING.execute(sb, string);
        updateCache(sb);
        return this;
    }

    private void updateCache(TruffleStringBuilderUTF32 sb) {
        cached = TO_STRING.execute(sb, true);
    }
}
