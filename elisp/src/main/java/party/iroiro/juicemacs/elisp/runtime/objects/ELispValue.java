package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CallTarget;
import org.eclipse.jdt.annotation.Nullable;

public interface ELispValue {
    @Nullable
    default CallTarget getCallTarget() {
        return null;
    }

    boolean lispEquals(Object other);

    default String display() {
        return toString();
    }

    static String display(Object o) {
        if (o instanceof ELispValue value) {
            return value.display();
        }
        return o.toString();
    }
}
