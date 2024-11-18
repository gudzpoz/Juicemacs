package party.iroiro.juicemacs.elisp.runtime.objects;

/// Abstract parent class for those objects that `eq` indicates `equal`
public abstract class AbstractELispIdentityObject implements ELispValue {
    @Override
    public final boolean lispEquals(Object other) {
        return this == other;
    }

    @Override
    public final int lispHashCode() {
        return System.identityHashCode(this);
    }
}
