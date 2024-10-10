package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.exception.AbstractTruffleException;

/**
 * Internal presentations of {@code catch/throw} and {@code condition-case/signal}
 */
public class ELispSignals {
    private abstract sealed static class ELispNonLocalExitException extends AbstractTruffleException {
        private final Object tag;
        private final Object data;

        private ELispNonLocalExitException(Object tag, Object data) {
            this.tag = tag;
            this.data = data;
        }

        public Object getTag() {
            return tag;
        }

        public Object getData() {
            return data;
        }
    }

    public static final class ELispSignalException extends ELispNonLocalExitException {
        public ELispSignalException(Object tag, Object data) {
            super(tag, data);
        }
    }

    public static final class ELispCatchException extends ELispNonLocalExitException {
        public ELispCatchException(Object tag, Object data) {
            super(tag, data);
        }
    }
}
