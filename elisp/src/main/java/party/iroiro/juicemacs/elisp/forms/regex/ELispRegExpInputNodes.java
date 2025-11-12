package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

abstract class ELispRegExpInputNodes {
    @GenerateInline(value = false)
    abstract static class InputLengthNode extends Node {
        public abstract long execute(Object input);

        @Specialization
        public long testInputStrLength(ELispString input) {
            return input.length();
        }

        @Specialization
        public long testInputBufferLength(ELispBuffer input) {
            return input.pointMax();
        }
    }

    @GenerateInline(value = false)
    abstract static class InputStartIndexNode extends Node {
        public abstract long execute(Object input);

        @Specialization
        public long inputGetStrStart(ELispString input) {
            return 0;
        }

        @Specialization
        public long inputGetBufferStart(ELispBuffer input) {
            return input.pointMin();
        }
    }

    @GenerateInline(value = false)
    abstract static class InputGetCharNode extends Node {
        public abstract int execute(Object input, long index);

        @Specialization(guards = "input.isUnibyte()")
        public int inputGetStrCharUnibyte(ELispString input, long index) {
            return input.bytes()[(int) index] & 0xFF;
        }

        @Specialization(guards = "!input.isUnibyte()")
        public int inputGetStrCharMultibyte(ELispString input, long index) {
            // TODO: cache or use iterator
            return input.codePointAt((int) index);
        }

        @Specialization
        public int inputGetBufferChar(ELispBuffer input, long index) {
            return input.getChar(index);
        }
    }
}
