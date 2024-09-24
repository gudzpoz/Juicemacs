package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

/**
 * A cons cell in ELisp
 *
 * <p>
 * It is not a list, and all the {@link List} methods are utility functions
 * that might raise exceptions, if the list if found to be circular, for example.
 * </p>
 */
public final class ELispCons extends AbstractSequentialList<Object> implements ELispValue {

    public ELispCons(Object car) {
        this.car = Objects.requireNonNull(car);
        this.cdr = NIL;
    }

    public ELispCons(Object car, Object cdr) {
        this.car = Objects.requireNonNull(car);
        this.cdr = Objects.requireNonNull(cdr);
    }

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

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public BrentTortoiseHareIterator listIterator(int i) {
        var iterator = new BrentTortoiseHareIterator();
        for (int j = 0; j < i; j++) {
            if (!iterator.hasNext()) {
                throw new IndexOutOfBoundsException();
            }
            iterator.next();
        }
        return iterator;
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
        return other instanceof ELispCons cons
                && BuiltInFns.FEqual.equal(car(), cons.car())
                && BuiltInFns.FEqual.equal(cdr(), cons.cdr());
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

    public final class BrentTortoiseHareIterator implements ListIterator<Object> {
        private Object tortoise = ELispCons.this;
        private Object tail = ELispCons.this;

        int i = 0;

        int max = 2;
        int n = 0;
        int q = 2;

        @Override
        public boolean hasNext() {
            return tail instanceof ELispCons;
        }

        public boolean hasNextCdr() {
            return !ELispSymbol.isNil(tail);
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
            Object next;
            // hasNext() should be called before next()
            if (tail instanceof ELispCons cons) {
                next = cons.car;
                tail = cons.cdr;
                i++;
            } else if (ELispSymbol.isNil(tail)) {
                throw new NoSuchElementException();
            } else {
                throw new RuntimeException("Not a lisp list");
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
                throw new IllegalStateException("Cycle detected");
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
    }

    public static ELispCons listOf(Object a, Object b) {
        ELispCons cons = new ELispCons(a);
        cons.setCdr(new ELispCons(b));
        return cons;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("(").append(car());
        BrentTortoiseHareIterator i = listIterator(1);
        while (i.hasNextCdr()) {
            if (i.hasNext()) {
                sb.append(" ").append(ELispValue.display(i.next()));
            } else {
                sb.append(" . ").append(ELispValue.display(i.current()));
                break;
            }
        }
        sb.append(")");
        return sb.toString();
    }

    public static class ListBuilder {
        @Nullable
        private ELispCons cons = null;
        @Nullable
        private ELispCons tail = null;

        public ListBuilder add(Object obj) {
            if (tail == null) {
                cons = new ELispCons(obj);
                tail = cons;
            } else {
                ELispCons next = new ELispCons(obj);
                tail.setCdr(next);
                tail = next;
            }
            return this;
        }

        public Object build() {
            return cons == null ? false : cons;
        }

        public Object build(Object tailCdr) {
            if (tail != null) {
                this.tail.setCdr(tailCdr);
            }
            return build();
        }
    }
}
