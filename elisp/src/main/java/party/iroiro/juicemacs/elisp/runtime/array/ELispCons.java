package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;

@ExportLibrary(InteropLibrary.class)
public final class ELispCons implements ListIteratorList, ELispValue {
    final ELispConsArray array;
    final int index;

    ELispCons(ELispConsArray array, int index) {
        this.array = array;
        this.index = index;
    }

    @NeverDefault
    ArrayStrategy strategy() {
        return array.strategy;
    }

    public Object car() {
        return strategy().car(array, index);
    }
    public Object cdr() {
        return strategy().cdr(array, index);
    }
    public void setCar(Object car) {
        strategy().setCar(array, index, car);
    }
    public void setCdr(Object cdr) {
        strategy().setCdr(array, index, cdr);
    }
    public void insertAfter(Object cadr) {
        setCdr(cons(cadr, cdr()));
    }

    public ConsIterator listIterator(int index) {
        return strategy().listIterator(array, this.index, index);
    }

    @Override
    public int size() {
        return strategy().size(array, index);
    }

    @Override
    public Object[] toArray() {
        if (strategy() == SingleArrayStrategy.INSTANCE) {
            Object[] inner = SingleArrayStrategy.INSTANCE.getArray(array);
            Object[] objects = Arrays.copyOf(inner, index + 1);
            ArrayUtils.reverse(objects);
            return objects;
        }
        return ListIteratorList.super.toArray();
    }

    @Override
    public Object get(int index) {
        if (strategy() == SingleArrayStrategy.INSTANCE) {
            return SingleArrayStrategy.INSTANCE.car(array, this.index - index);
        }
        return ListIteratorList.super.get(index);
    }

    @Override
    public Object set(int index, Object element) {
        if (strategy() == SingleArrayStrategy.INSTANCE) {
            SingleArrayStrategy.INSTANCE.setCar(array, this.index - index, element);
            return element;
        }
        return ListIteratorList.super.set(index, element);
    }

    public int getStartLine() {
        return array.getStartLine();
    }
    public int getStartColumn() {
        return array.getStartColumn();
    }
    public int getEndLine() {
        return array.getEndLine();
    }
    public int getEndColumn() {
        return array.getEndColumn();
    }

    public void setSourceLocation(int startLine, int startColumn, int endLine, int endColumn) {
        array.setSourceLocation(startLine, startColumn, endLine, endColumn);
    }
    public void fillDebugInfo(Node source) {
        SourceSection location = source.getSourceSection();
        if (location != null) {
            setSourceLocation(
                    location.getStartLine(), location.getStartColumn(),
                    location.getEndLine(), location.getEndColumn()
            );
        }
    }
    public void fillDebugInfo(ELispCons source) {
        if (getStartLine() == 0 && source.getStartLine() != 0) {
            setSourceLocation(
                    source.getStartLine(),
                    source.getStartColumn(),
                    source.getEndLine(),
                    source.getEndColumn()
            );
        }
    }

    @Nullable
    public SourceSection getSourceSection(@Nullable Source rootSource) {
        if (rootSource == null) {
            return null;
        }
        if (getStartLine() == 0) {
            return null;
        }
        try {
            return rootSource.createSection(getStartLine(), getStartColumn(), getEndLine(), getEndColumn());
        } catch (IllegalArgumentException ignored) {
            return null;
        }
    }


    //#region InteropLibrary
    @ExportMessage
    public boolean hasMembers() {
        return true;
    }
    @ExportMessage
    public Object getMembers(boolean ignored) {
        return new ELispVector(List.of("car", "cdr"));
    }
    @ExportMessage
    public boolean isMemberReadable(String member) {
        return member.equals("car") || member.equals("cdr");
    }
    @ExportMessage
    public Object readMember(String member) throws UnknownIdentifierException {
        return switch (member) {
            case "car" -> car();
            case "cdr" -> cdr();
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
            case "car" -> setCar(value);
            case "cdr" -> setCdr(value);
            default -> throw UnknownIdentifierException.create(member);
        }
    }
    //#endregion InteropLibrary

    public ELispCons forwarded() {
        return strategy() instanceof ForwardArrayStrategy forward
                ? forward.forwarded(array, index)
                : this;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ELispCons cons) {
            if (cons == this) {
                return true;
            }
            if (cons.array == array) {
                return cons.index == index;
            }
            cons = cons.forwarded();
            ELispCons thisCons = forwarded();
            return cons.array == thisCons.array && cons.index == thisCons.index;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return strategy().hashCode(array, index);
    }

    @Override
    public String toString() {
        return ELispPrint.toString(this).toString();
    }

    @Override
    public void display(ELispPrint print) {
        print.printCons(this);
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
            result = 31 * result + ELispValue.lispHashCode(last.car(), depth + 1);
        }
        result = 31 * result + ELispValue.lispHashCode(last.cdr(), depth + 1);
        return result;
    }

    public ELispCons copy() {
        return strategy().copy(array, index);
    }

    public interface ConsIterator extends ListIterator<Object> {
        boolean hasNextCons();
        ELispCons currentCons();
        ELispCons nextCons();
    }

    public static ELispCons cons(Object car, Object cdr) {
        return ELispConsAccessFactory.ConsPrependConsNodeGen.getUncached()
                .executeCons(null, car, cdr);
    }

    public static ELispCons listOf(Object element1) {
        return SingleArrayStrategy.INSTANCE.create(element1);
    }

    public static ELispCons listOf(Object element1, Object element2) {
        return SingleArrayStrategy.INSTANCE.create(element2, element1);
    }

    public static Object listOf(Object... elements) {
        if (elements.length == 0) {
            return false;
        }
        if (elements.length > 1) {
            ArrayUtils.reverse(elements);
        }
        return SingleArrayStrategy.INSTANCE.create(elements);
    }
    public static Object listWithCdrOf(Object[] elements, Object cdr) {
        if (elements.length == 0) {
            return cdr;
        }
        if (elements.length > 1) {
            ArrayUtils.reverse(elements);
        }
        return SingleArrayStrategy.INSTANCE.createWithCdr(elements, cdr);
    }
    public static Object listOfReversed(Object[] elements) {
        return SingleArrayStrategy.INSTANCE.create(elements);
    }

    public static Iterable<Object> iterate(Object sequence) {
        if (ELispTypeSystem.isNil(sequence)) {
            return List.of();
        }
        return asCons(sequence);
    }

    public static ConsIterator emptyIterator() {
        return new WithCdrStrategy.ConsArrayIterator(
                new ELispConsArray(false, 0, SingleArrayStrategy.INSTANCE), 0, 1);
    }

    public static final class ListBuilder {
        private final ArrayList<Object> elements = new ArrayList<>();

        public ListBuilder add(Object element) {
            elements.add(element);
            return this;
        }

        public Object build() {
            if (elements.isEmpty()) {
                return false;
            }
            return listOf(elements.toArray());
        }

        public Object buildWithCdr(Object cdr) {
            if (elements.isEmpty()) {
                return cdr;
            }
            return listWithCdrOf(elements.toArray(), cdr);
        }

        public static Collector<Object, ?, Object> collector() {
            return Collectors.collectingAndThen(
                    Collectors.toList(),
                    (list) -> listOf(list.toArray())
            );
        }
    }
}
