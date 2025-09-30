package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.ListIterator;
import java.util.NoSuchElementException;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LISTP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

final class BrentTortoiseHareIterator implements ListIterator<Object>, ConsIterator {
    private Object tortoise;
    private Object tail;

    public BrentTortoiseHareIterator(Object cons) {
        this.tortoise = cons;
        this.tail = cons;
    }

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

    @Override
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

    @TruffleBoundary
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
            throw ELispSignals.wrongTypeArgument(LISTP, tortoise);
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
            throw ELispSignals.circularList(tortoise);
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
