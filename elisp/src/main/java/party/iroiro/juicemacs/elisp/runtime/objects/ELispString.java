package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;

public class ELispString implements ELispValue {

    private MutableTruffleString value;

    public ELispString(String init) {
        this.value = TruffleString.fromConstant(init, TruffleString.Encoding.UTF_8)
                .asMutableTruffleStringUncached(TruffleString.Encoding.UTF_8);
    }

    public ELispString(byte[] bytes) {
        this.value = MutableTruffleString.fromByteArrayUncached(
                bytes,
                0,
                bytes.length,
                TruffleString.Encoding.BYTES,
                false
        );
    }

    @Override
    public String type() {
        return "string";
    }
}
