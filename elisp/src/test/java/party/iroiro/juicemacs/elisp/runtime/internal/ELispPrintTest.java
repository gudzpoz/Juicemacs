package party.iroiro.juicemacs.elisp.runtime.internal;

import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBoolVector;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

import java.util.BitSet;

import static org.junit.jupiter.api.Assertions.*;

public class ELispPrintTest {
    private String print(Object o) {
        return ELispPrint.toString(o).toString();
    }

    @Test
    public void testConsPrint() {
        assertEquals("(nil)", print(ELispCons.listOf(false)));
        assertEquals("(nil)", print(new ELispCons(false, false)));
        assertEquals("(1 2 3)", print(ELispCons.listOf(1L, 2L, 3L)));
        assertEquals("(1 . 2)", print(new ELispCons(1L, 2L)));
        assertEquals("(1 2 . 3)", print(new ELispCons(1L, new ELispCons(2L, 3L))));
    }

    @Test
    public void testBoolVecPrint() {
        assertEquals("#&10\"\\0\\0\"", print(new ELispBoolVector(new long[1], 10)));
    }
}
