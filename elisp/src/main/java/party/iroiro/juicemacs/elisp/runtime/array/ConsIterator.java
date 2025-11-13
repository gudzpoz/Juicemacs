package party.iroiro.juicemacs.elisp.runtime.array;

import org.jspecify.annotations.Nullable;

import java.util.ListIterator;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

/// An iterator for [ELispCons] with circular list detection
public sealed interface ConsIterator extends ListIterator<Object> permits BrentTortoiseHareIterator {
    /// Returns `true` if [#peekNextCons()] will succeed
    ///
    /// This method does not check for circularity.
    boolean hasNextCons();

    /// Returns `true` is [#peekNextCdr()] is not nil
    default boolean endOfList() {
        return isNil(peekNextCdr());
    }

    /// Returns the next cons cell
    ELispCons peekNextCons();

    /// Returns the next cons cell
    ///
    /// @throws party.iroiro.juicemacs.elisp.runtime.ELispSignals.ELispSignalException if the list is circular
    ELispCons nextCons();

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
    @Nullable ELispCons nextConsOrCircular();

    /// Shortcut to `nextCons().car()`
    @Override default Object next() {
        return nextCons().car;
    }

    Object peekNextCdr();
}
