package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ListIterator;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

@ExportLibrary(InteropLibrary.class)
public final class ELispCons implements ELispValue, ListIteratorList, TruffleObject, LocationProvider {
    Object car;
    Object cdr;
    long encodedLocation;

    private ELispCons(Object car) {
        this(car, false);
    }
    private ELispCons(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public Object car() {
        return car;
    }
    public Object cdr() {
        return cdr;
    }
    public void setCar(Object car) {
        this.car = car;
    }
    public void setCdr(Object cdr) {
        this.cdr = cdr;
    }
    public void insertAfter(Object car) {
        this.cdr = new ELispCons(car, cdr);
    }

    @Override
    public ConsIterator listIterator(int index) {
        BrentTortoiseHareIterator i = new BrentTortoiseHareIterator(this);
        if (index == 0) {
            return i;
        }
        for (int j = 0; j < index; j++) {
            i.next();
        }
        return i;
    }

    //#region Lisp object
    @Override
    public boolean equals(Object o) {
        return this == o;
    }
    @Override
    public int hashCode() {
        return System.identityHashCode(this);
    }
    @Override
    public boolean lispEquals(Object other) {
        if (this == other) {
            return true;
        }
        return other instanceof ELispCons cons
                && BuiltInFns.FEqual.equal(car(), cons.car())
                && BuiltInFns.FEqual.equal(cdr(), cons.cdr());
    }
    @Override
    public int lispHashCode(int depth) {
        if (depth > LISP_HASH_CODE_MAX_DEPTH) {
            return 0;
        }
        int result = 1;
        ConsIterator i = listIterator(0);
        ELispCons last = this;
        while (i.hasNextCons()) {
            last = i.nextCons();
            result = 31 * result + ELispValue.lispHashCode(last.car, depth + 1);
        }
        result = 31 * result + ELispValue.lispHashCode(last.cdr, depth + 1);
        return result;
    }
    @Override
    public void display(ELispPrint print) {
        print.printCons(this);
    }
    @Override
    public String toString() {
        return ELispPrint.toString(this).toString();
    }
    //#endregion Lisp object

    //#region Debug info
    @Override
    public long getEncodedLocation() {
        return encodedLocation;
    }
    @Override
    public void setEncodedLocation(long encodedLocation) {
        this.encodedLocation = encodedLocation;
    }
    //#endregion Debug info

    //#region InteropLibrary
    @ExportMessage
    public boolean hasMembers() {
        return true;
    }
    @ExportMessage
    public Object getMembers(boolean ignored) {
        return new ELispVector(new Object[]{"car", "cdr"});
    }
    @ExportMessage
    public boolean isMemberReadable(String member) {
        return member.equals("car") || member.equals("cdr");
    }
    @ExportMessage
    public Object readMember(String member) throws UnknownIdentifierException {
        return switch (member) {
            case "car" -> car;
            case "cdr" -> cdr;
            default -> throw UnknownIdentifierException.create(member);
        };
    }
    @ExportMessage
    public boolean isMemberModifiable(String member) {
        return isMemberReadable(member);
    }
    @ExportMessage
    public boolean isMemberInsertable(String ignored) {
        return false;
    }
    @ExportMessage
    public void writeMember(String member, Object value) throws UnknownIdentifierException {
        switch (member) {
            case "car" -> car = value;
            case "cdr" -> cdr = value;
            default -> throw UnknownIdentifierException.create(member);
        }
    }
    @ExportMessage
    public boolean hasArrayElements() {
        return true;
    }
    @ExportMessage
    public long getArraySize() {
        return BuiltInFns.FSafeLength.safeLength(this);
    }
    @ExportMessage
    public boolean isArrayElementReadable(long index) {
        return true;
    }
    @ExportMessage
    public Object readArrayElement(long index) {
        return BuiltInFns.FNth.nth(index, this);
    }
    //#endregion InteropLibrary

    //#region Utilities
    public static ELispCons cons(Object car, Object cdr) {
        return new ELispCons(car, cdr);
    }
    public static ELispCons listOf(Object car) {
        return new ELispCons(car);
    }
    public static ELispCons listOf(Object car, Object cadr) {
        return new ELispCons(car, new ELispCons(cadr));
    }
    public static Object listOf(Object... elements) {
        ListBuilder builder = new ListBuilder();
        for (Object element : elements) {
            builder.add(element);
        }
        return builder.build();
    }
    public static Iterable<Object> iterate(Object cons) {
        if (isNil(cons)) {
            return ELispCons::emptyIterator;
        }
        return asCons(cons);
    }
    public static ConsIterator emptyIterator() {
        return new BrentTortoiseHareIterator(false);
    }
    //#endregion Utilities

    public interface ConsIterator extends ListIterator<Object> {
        boolean hasNextCons();
        ELispCons currentCons();
        ELispCons nextCons();
    }

    public static final class ListBuilder {
        @Nullable
        private ELispCons cons;
        @Nullable
        private ELispCons tail;

        public ListBuilder() {
            this(null);
        }

        public ListBuilder(@Nullable ELispCons object) {
            cons = object;
            tail = object;
        }

        public ListBuilder add(Object obj) {
            if (tail == null) {
                cons = ELispCons.listOf(obj);
                tail = cons;
            } else {
                ELispCons next = ELispCons.listOf(obj);
                tail.setCdr(next);
                tail = next;
            }
            return this; // NOPMD
        }

        public Object build() {
            return cons == null ? false : cons;
        }

        public Object buildWithCdr(Object tailCdr) {
            if (tail != null) {
                this.tail.setCdr(tailCdr);
                //noinspection DataFlowIssue: tail != null => cons != null
                return cons;
            }
            return tailCdr;
        }

        public static Collector<Object, ListBuilder, Object> collector() {
            return new Collector<>() {
                @Override
                public Supplier<ListBuilder> supplier() {
                    return ListBuilder::new;
                }

                @Override
                public BiConsumer<ListBuilder, Object> accumulator() {
                    return ListBuilder::add;
                }

                @Override
                public BinaryOperator<ListBuilder> combiner() {
                    return (list1, list2) -> {
                        if (list1.tail == null) {
                            return list2;
                        }
                        if (list2.cons != null) {
                            list1.tail.setCdr(list2.cons);
                            list1.tail = list2.tail;
                        }
                        return list1;
                    };
                }

                @Override
                public Function<ListBuilder, Object> finisher() {
                    return ListBuilder::build;
                }

                @Override
                public Set<Characteristics> characteristics() {
                    return Set.of();
                }
            };
        }
    }
}
