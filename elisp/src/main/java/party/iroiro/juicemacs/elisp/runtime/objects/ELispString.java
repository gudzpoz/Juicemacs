package party.iroiro.juicemacs.elisp.runtime.objects;

import com.lodborg.intervaltree.IntegerInterval;
import com.lodborg.intervaltree.IntervalTree;
import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Consumer;

public final class ELispString implements ELispValue {

    private static final class Properties extends IntegerInterval {
        Object property;

        Properties(int start, int end, Object property) {
            super(start, end, Bounded.CLOSED_LEFT);
            this.property = property;
        }

        @Override
        protected Properties create() {
            return new Properties(0, 0, false);
        }
    }

    private MutableTruffleString value;
    private TruffleString.Encoding encoding;
    private final IntervalTree<Integer> properties;

    public ELispString(String init) {
        this.value = TruffleString.fromConstant(init, TruffleString.Encoding.UTF_8)
                .asMutableTruffleStringUncached(TruffleString.Encoding.UTF_8);
        this.encoding = TruffleString.Encoding.UTF_8;
        this.properties = new IntervalTree<>();
    }

    public ELispString(byte[] bytes) {
        this.value = MutableTruffleString.fromByteArrayUncached(
                bytes,
                0,
                bytes.length,
                TruffleString.Encoding.BYTES,
                false
        );
        this.encoding = TruffleString.Encoding.BYTES;
        this.properties = new IntervalTree<>();
    }

    @Nullable
    public static Long toValidChar(Object a) {
        return switch (a) {
            case Long l -> Character.isValidCodePoint(Math.toIntExact(l)) ? l : null;
            case ELispBigNum(BigInteger i) when i.bitLength() < 32 ->
                    Character.isValidCodePoint(i.intValue()) ? i.longValue() : null;
            default -> null;
        };
    }

    @Override
    public String type() {
        return "string";
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public boolean isMultibyte() {
        return encoding.equals(TruffleString.Encoding.BYTES);
    }

    public int intervals() {
        return properties.size();
    }

    public void forRangeProperties(int i, Consumer<Object> propertiesConsumer) {
        properties.query(i).forEach((props) ->
                propertiesConsumer.accept(((Properties) props).property));
    }

    public void forProperties(Consumer<Object> propertiesConsumer) {
        properties.forEach((props) -> propertiesConsumer.accept(((Properties) props).property));
    }

    public void syncFromPlist(ELispContext context, List<Object> list) {
        if ((list.size() - 1) % 3 != 0) {
            throw new IllegalArgumentException();
        }
        for (int i = 1; i < list.size(); i += 3) {
            long start = (long) list.get(i);
            long end = (long) list.get(i + 1);
            Object props = list.get(i + 2);
            if (context.isNil(props) || (props instanceof ELispCons cons && cons.size() % 2 == 0)) {
                properties.add(new Properties((int) start, (int) end, props));
            }
        }
    }
}
