package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

public interface ELispValue {
    /// Compares against another object with `equal`-like behavior
    ///
    /// @see party.iroiro.juicemacs.elisp.forms.BuiltInFns.FEqual
    boolean lispEquals(Object other);
    /// Computes the hash code for this object, following [#lispEquals(Object)]
    ///
    /// For complex objects, if we are to guarantee an identical hash code for
    /// equal objects ([#lispEquals(Object)]), we need a "deep" hash.
    /// Use [#hashCode()] if you are comparing with [#equals(Object)].
    int lispHashCode();

    /// Compares against another object with `eq`-like behavior
    ///
    /// @see party.iroiro.juicemacs.elisp.forms.BuiltInData.FEq
    @Override
    boolean equals(Object other);
    /// Similar to [#lispHashCode()], but for [#equals(Object)]
    @Override
    int hashCode();

    static int lispHashCode(Object o) {
        return o instanceof ELispValue value ? value.lispHashCode() : o.hashCode();
    }

    void display(ELispPrint print);
}
