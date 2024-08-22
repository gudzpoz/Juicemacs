package party.iroiro.juicemacs.elisp.runtime.objects;

import org.eclipse.jdt.annotation.NonNull;
import party.iroiro.juicemacs.elisp.nodes.ELispCallFormNode;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

import java.util.AbstractSequentialList;
import java.util.List;
import java.util.ListIterator;

/**
 * A cons cell in ELisp
 *
 * <p>
 * It is not a list, and all the {@link List} methods are utility functions
 * that might raise exceptions, if the list if found to be circular, for example.
 * </p>
 */
public final class ELispCons extends AbstractSequentialList<Object> implements ELispValue {

    public Object car;
    public Object cdr;

    @NonNull
    @Override
    public ListIterator<Object> listIterator(int i) {
        if (car == null) {
            return List.of().listIterator();
        }
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
    public ELispExpressionNode eval(ELispContext context) {
        if (car == null && cdr == null) {
            return ELispContext.NIL.eval(context);
        }
        if (car == null || cdr == null) {
            throw new IllegalArgumentException();
        }
        return new ELispCallFormNode(this, context);
    }

    @Override
    public String type() {
        return "cons";
    }

    private final class BrentTortoiseHareIterator implements ListIterator<Object> {
        private Object tortoise = ELispCons.this;
        private Object tail = ELispCons.this;

        int i = 0;

        int max = 2;
        int n = 0;
        int q = 2;

        @Override
        public boolean hasNext() {
            return tail != null;
        }

        @Override
        public Object next() {
            Object next;
            // hasNext() should be called before next()
            if (tail instanceof ELispCons cons) {
                next = cons.car;
                tail = cons.cdr;
                i++;
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

}
