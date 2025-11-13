package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

import java.util.ListIterator;
import java.util.NoSuchElementException;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LISTP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

/// An iterator for [ELispCons] with circular list detection based on
/// Brent's tortoise-hare teleportation algorithm
///
/// ## API Names
///
/// So, the [java.util.Iterator] interface is for proper lists. However, conses
/// has three kinds of terminal states during iteration:
///
/// - Ends with `nil`: proper ending of lists
/// - Ends with non-cons: (invalid) dotted cons cell
/// - Circular: (invalid) circular list
///
/// The [#hasNext()] method does not provide a way to distinguish all three
/// kinds of termination. And this is what this [ConsIterator] is for.
///
/// - [#next()] yields three kinds of errors:
///   - [#hasNext()]: `true` if [#next()] will not emit [NoSuchElementException] (non-nil)
///   - [#hasProperNext()]: `true` if [#next()] will not emit
///     [party.iroiro.juicemacs.elisp.runtime.ELispGlobals#LISTP] (proper-list)
///   - [#nextOrCircular()] / [#nextConsOrCircular()]: `null` if circular list detected
///
/// It's certainly not the best design I guess? But it's the best we can do for now.
public final class ConsIterator implements ListIterator<Object> {
    private Object tortoise;
    private Object tail;

    public ConsIterator(Object cons) {
        this.tortoise = cons;
        this.tail = cons;
    }

    private int i = 0;

    private int max = 2;
    private int n = 0;
    private int q = 2;

    public Object peekNextCdr() {
        return tail;
    }

    /// Returns the next cons cell or `null` if the list is circular
    ///
    /// The internals of this iterator is always consistent, even if
    /// a circular list is detected:
    ///
    /// - After n-th call to [#next()] or [#nextCons()] or [#nextConsOrCircular()],
    ///   [#peekNextCons()] points to `nthcdr`.
    ///
    /// An example:
    ///
    /// ```java
    /// ConsIterator i = list.iterator(0);
    /// i.peekNextCons(); // (nthcdr 0 list)
    /// i.nextCons();     // (nthcdr 0 list)
    /// i.peekNextCons(); // (nthcdr 1 list)
    /// i.nextCons();     // (nthcdr 1 list)
    /// // ...
    /// ```
    @TruffleBoundary
    @Nullable
    public ELispCons nextConsOrCircular() {
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
            return null;
        }
        return next;
    }

    /// Returns `true` is [#peekNextCdr()] is not nil
    ///
    /// The list is not guaranteed to be well-formed.
    @Override
    public boolean hasNext() {
        return !isNil(peekNextCdr());
    }

    /// Returns `true` if [#nextOrCircular()] and [#nextConsOrCircular()] will succeed
    /// (provided that the list is circular)
    public boolean hasProperNext() {
        return peekNextCdr() instanceof ELispCons;
    }

    /// Returns the next cons cell
    public ELispCons peekNextCons() {
        if (peekNextCdr() instanceof ELispCons cons) {
            return cons;
        }
        throw new NoSuchElementException();
    }

    /// Returns the next cons cell
    ///
    /// @throws party.iroiro.juicemacs.elisp.runtime.ELispSignals.ELispSignalException if the list is circular
    public ELispCons nextCons() {
        ELispCons next = nextConsOrCircular();
        if (next == null) {
            throw ELispSignals.circularList(peekNextCdr());
        }
        return next;
    }

    /// Shortcut to `nextCons().car()`
    @Override
    public Object next() {
        return nextCons().car;
    }

    @Nullable
    public Object nextOrCircular() {
        ELispCons next = nextConsOrCircular();
        return next == null ? null : next.car;
    }

    public Object nextOr(Object defaultValue) {
        if (hasProperNext()) {
            Object next = nextOrCircular();
            if (next != null) {
                return next;
            }
        }
        return defaultValue;
    }

    //#region ListIterator
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
    //#endregion ListIterator
}
