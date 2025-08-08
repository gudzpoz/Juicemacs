package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.strings.TruffleString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;

abstract class ELispRegExpInputNodes {
    @GenerateInline(value = false)
    abstract static class InputLengthNode extends Node {
        public abstract long execute(Object input);

        @Specialization
        public long testInputStrLength(
                ELispString input,
                @Cached TruffleString.CodePointLengthNode length
        ) {
            return length.execute(input.value(), StringSupport.UTF_32);
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

        @Specialization(guards = "input.state() != 1")
        public int inputGetStrCharMultibyte(
                ELispString input, long index,
                @Cached @Cached.Shared TruffleString.CodePointAtIndexNode charAt
        ) {
            return charAt.execute(input.value(), (int) index, StringSupport.UTF_32);
        }

        @Specialization(guards = "input.state() == 1")
        public int inputGetStrCharUnibyte(
                ELispString input, long index,
                @Cached @Cached.Shared TruffleString.CodePointAtIndexNode charAt
        ) {
            int c = charAt.execute(input.value(), (int) index, StringSupport.UTF_32);
            return c < 128 ? c : c + 0x3FFF00;
        }

        @Specialization
        public int inputGetBufferChar(ELispBuffer input, long index) {
            return input.getChar(index);
        }
    }
}
