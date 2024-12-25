package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.mule.MuleString;

abstract class ELispRegExpInputNodes {
    public static final TruffleString.Encoding ENCODING = TruffleString.Encoding.UTF_32;
    // TODO: Add buffer specializations

    @GenerateInline(value = false)
    abstract static class InputLengthNode extends Node {
        public abstract long execute(VirtualFrame frame, Object input);

        @Specialization
        public long testInputStrLength(MuleString input) {
            return input.length();
        }

        @Specialization
        public long testInputBufferLength(ELispBuffer input) {
            return input.length();
        }
    }

    @GenerateInline(value = false)
    abstract static class InputGetCharNode extends Node {
        public abstract int execute(VirtualFrame frame, Object input, long index);

        @Specialization
        public int inputGetStrChar(MuleString input, long index) {
            return input.codePointAt(index);
        }

        @Specialization
        public int inputGetBufferChar(ELispBuffer input, long index) {
            return (int) input.getChar(index);
        }
    }
}
