package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.interop.TruffleObject;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

public interface ELispValue extends TruffleObject {
    int LISP_HASH_CODE_MAX_DEPTH = 3;

    /// Compares against another object with `equal`-like behavior
    ///
    /// @see party.iroiro.juicemacs.elisp.forms.BuiltInFns.FEqual
    boolean lispEquals(Object other);
    /// Computes the hash code for this object, following [#lispEquals(Object)]
    ///
    /// For complex objects, if we are to guarantee an identical hash code for
    /// equal objects ([#lispEquals(Object)]), we need a "deep" hash.
    /// Use [#hashCode()] if you are comparing with [#equals(Object)].
    int lispHashCode(int depth);

    /// Compares against another object with `eq`-like behavior
    ///
    /// @see party.iroiro.juicemacs.elisp.forms.BuiltInData.FEq
    @Override
    boolean equals(Object other);
    /// Similar to [#lispHashCode(int)], but for [#equals(Object)]
    @Override
    int hashCode();

    static int lispHashCode(Object o, int depth) {
        return o instanceof ELispValue value ? value.lispHashCode(depth) : o.hashCode();
    }

    void display(ELispPrint print);
}
