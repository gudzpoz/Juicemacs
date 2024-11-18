package party.iroiro.juicemacs.elisp.runtime.objects;

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

    default String display() {
        return toString();
    }

    static String display(Object o) {
        if (o instanceof ELispValue value) {
            return value.display();
        }
        if (o == Boolean.TRUE) {
            return "t";
        }
        if (o == Boolean.FALSE) {
            return "nil";
        }
        return o.toString();
    }
}
