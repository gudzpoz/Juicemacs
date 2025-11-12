package party.iroiro.juicemacs.elisp.runtime.objects;

import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import static org.junit.jupiter.api.Assertions.*;

public class ELispObarrayTest {
    @Test
    public void testObarray() {
        ELispObarray obarray = new ELispObarray();
        for (int i = 0; i < 50000; i++) {
            String s = "test" + i;
            ELispSymbol symbol = obarray.intern(ELispString.ofJava(s));
            assertNotNull(symbol);
            ELispSymbol result = obarray.internSoft(ELispString.ofJava(s));
            assertSame(symbol, result);
        }
    }

}
