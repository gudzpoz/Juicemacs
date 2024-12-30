package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

/**
 * A cons cell in ELisp
 *
 * <p>
 * It is not a list, and all the {@link List} methods are utility functions
 * that might raise exceptions, if the list if found to be circular, for example.
 * </p>
 */
@ExportLibrary(InteropLibrary.class)
public final class ELispCons extends AbstractSequentialList<Object> implements ELispValue, TruffleObject {

    public ELispCons(Object car) {
        this.car = Objects.requireNonNull(car);
        this.cdr = false;
    }

    public ELispCons(Object car, Object cdr) {
        this.car = Objects.requireNonNull(car);
        this.cdr = Objects.requireNonNull(cdr);
    }

    private int startLine, startColumn, endLine, endColumn;
    private Object car;
    private Object cdr;

    public Object car() {
        return car;
    }

    public Object cdr() {
        return cdr;
    }

    public void setCar(Object car) {
        this.car = Objects.requireNonNull(car);
    }

    public void setCdr(Object cdr) {
        this.cdr = cdr;
    }

    public int getStartLine() {
        return startLine;
    }

    public int getStartColumn() {
        return startColumn;
    }

    public int getEndLine() {
        return endLine;
    }

    public int getEndColumn() {
        return endColumn;
    }

    public void setSourceLocation(int startLine, int startColumn, int endLine, int endColumn) {
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.endLine = endLine;
        this.endColumn = endColumn;
    }

    public void fillDebugInfo(@Nullable Node parent) {
        if (getStartLine() != 0) {
            return;
        }
        while (parent != null) {
            if (parent instanceof ELispInterpretedNode.ELispConsExpressionNode consExpr
                    && consExpr.getCons().getStartLine() != 0) {
                ELispCons upper = consExpr.getCons();
                setSourceLocation(
                        upper.getStartLine(),
                        upper.getStartColumn(),
                        upper.getEndLine(),
                        upper.getEndColumn()
                );
                return;
            }
            parent = parent.getParent(); // NOPMD
        }
    }

    public void fillDebugInfo(@Nullable SourceSection source) {
        if (source == null) {
            return;
        }
        setSourceLocation(
                source.getStartLine(),
                source.getStartColumn(),
                source.getEndLine(),
                source.getEndColumn()
        );
    }

    public SourceSection getSourceSection(Source source) {
        return startLine == 0 ? source.createUnavailableSection() : source.createSection(startLine, startColumn, endLine, endColumn);
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public BrentTortoiseHareIterator listIterator(int i) {
        BrentTortoiseHareIterator iterator = new BrentTortoiseHareIterator();
        for (int j = 0; j < i; j++) {
            if (!iterator.hasNext()) {
                throw new IndexOutOfBoundsException();
            }
            iterator.next();
        }
        return iterator;
    }

    public ConsIterator consIterator(int i) {
        return listIterator(i);
    }

    public ELispCons getCons(int i) {
        return listIterator(i).nextCons();
    }

    @Override
    public int size() {
        int i = 0;
        for (Object _ : this) {
            i++;
        }
        return i;
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
    public int lispHashCode() {
        int result = 1;
        ConsIterator i = consIterator(0);
        ELispCons last = this;
        while (i.hasNextCons()) {
            last = i.nextCons();
            result = 31 * result + ELispValue.lispHashCode(last.car);
        }
        result = 31 * result + ELispValue.lispHashCode(last.cdr);
        return result;
    }

    public ELispCons tail() {
        BrentTortoiseHareIterator i = listIterator(0);
        while (i.hasNext() && i.currentCons().cdr() instanceof ELispCons) {
            i.next();
        }
        return i.currentCons();
    }

    public void insertAfter(Object object) {
        setCdr(new ELispCons(object, cdr()));
    }

    public interface ConsIterator {
        boolean hasNextCons();
        ELispCons nextCons();
    }

    public final class BrentTortoiseHareIterator implements ListIterator<Object>, ConsIterator {
        private Object tortoise = ELispCons.this;
        private Object tail = ELispCons.this;

        int i = 0;

        int max = 2;
        int n = 0;
        int q = 2;

        @Override
        public boolean hasNext() {
            return hasNextCdr();
        }

        @Override
        public boolean hasNextCons() {
            return tail instanceof ELispCons;
        }

        public boolean hasNextCdr() {
            return !isNil(tail);
        }

        public Object current() {
            return tail;
        }

        public ELispCons currentCons() {
            if (tail instanceof ELispCons cons) {
                return cons;
            }
            throw new NoSuchElementException();
        }

        @Override
        public Object next() {
            return nextCons().car;
        }

        @Override
        public ELispCons nextCons() {
            ELispCons next;
            // hasNext() should be called before next()
            if (tail instanceof ELispCons cons) {
                next = cons;
                tail = cons.cdr;
                i++;
            } else if (isNil(tail)) {
                throw new NoSuchElementException();
            } else {
                throw ELispSignals.wrongTypeArgument(LISTP, ELispCons.this);
            }
            // The following ensures the tortoise *occasionally* teleports.
            // Code modified from Emacs' src/lisp.h (FOR_EACH_TAIL_INTERNAL).
            q--;
            boolean teleport = false;
            if (q == 0) {
                // maybe_quit();
                n--;
                if (n <= 0) {
                    teleport = true;
                }
            }
            if (teleport) {
                max <<= 1;
                n = max;
                q = max;
                n >>= 16; // USHRT_WIDTH;
                tortoise = tail;
            } else if (tail == tortoise) {
                throw ELispSignals.circularList(ELispCons.this);
            }
            return next;
        }

        @Override
        public boolean hasPrevious() {
            return false;
        }

        @Override
        public Object previous() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int nextIndex() {
            return i;
        }

        @Override
        public int previousIndex() {
            return -1;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void set(Object e) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void add(Object e) {
            throw new UnsupportedOperationException();
        }

        public Object[] toArray() {
            if (!hasNext()) {
                return new Object[0];
            }
            List<Object> list = new ArrayList<>();
            while (hasNext()) {
                list.add(next());
            }
            return list.toArray();
        }
    }

    @Override
    public boolean equals(Object o) {
        return this == o;
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this);
    }

    @Override
    public String toString() {
        switch (cdr) {
            case ELispCons quoted when car == QUOTE && isNil(quoted.cdr) -> {
                return "'" + ELispValue.display(quoted.car);
            }
            case ELispCons function when car == FUNCTION && isNil(function.cdr) -> {
                return "#'" + ELispValue.display(function.car);
            }
            case ELispCons backquote when car == BACKQUOTE && isNil(backquote.cdr) -> {
                return "`" + ELispValue.display(backquote.car);
            }
            case ELispCons comma when car == COMMA && isNil(comma.cdr) -> {
                return "," + ELispValue.display(comma.car);
            }
            case ELispCons comma when car == COMMA_AT && isNil(comma.cdr) -> {
                return ",@" + ELispValue.display(comma.car);
            }
            default -> {}
        }
        StringBuilder sb = new StringBuilder("(").append(ELispValue.display(car()));
        BrentTortoiseHareIterator i = listIterator(1);
        while (i.hasNextCdr()) {
            if (i.hasNextCons()) {
                sb.append(" ").append(ELispValue.display(i.next()));
            } else {
                sb.append(" . ").append(ELispValue.display(i.current()));
                break;
            }
        }
        sb.append(")");
        return sb.toString();
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
    //#endregion InteropLibrary

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
            return addWithLocation(obj, 0, 0, 0, 0);
        }

        public ListBuilder addWithLocation(Object obj, int startLine, int startCol, int endLine, int endCol) {
            if (tail == null) {
                cons = new ELispCons(obj);
                tail = cons;
            } else {
                ELispCons next = new ELispCons(obj);
                tail.setCdr(next);
                tail = next;
            }
            tail.setSourceLocation(startLine, startCol, endLine, endCol);
            return this;
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
    }

    public static ELispCons listOf(Object a, Object b) {
        return new ELispCons(a, new ELispCons(b));
    }

    public static Object listOf(Object first, Object... elements) {
        ListBuilder builder = new ListBuilder();
        builder.add(first);
        for (Object element : elements) {
            builder.add(element);
        }
        return builder.build();
    }

    public static Iterable<?> iterate(Object list) {
        if (isNil(list)) {
            return Collections.emptyList();
        } else {
            return (ELispCons) list;
        }
    }
}
