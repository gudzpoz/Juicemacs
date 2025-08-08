package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.Objects;

/// Abstract parent class for those objects that `eq` indicates `equal`
public abstract class AbstractELispIdentityObject implements ELispValue {
    @Override
    public final boolean lispEquals(Object other) {
        return this == other;
    }

    @Override
    public final int lispHashCode(int depth) {
        return System.identityHashCode(this);
    }

    @Override
    public void display(ELispPrint print) {
        print.print('#').print('<')
                .print(new ELispString(Objects.toIdentityString(this)))
                .print('>');
    }

    @Override
    public String toString() {
        return ELispPrint.toString(this).toString();
    }
}
