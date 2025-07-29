package party.iroiro.juicemacs.elisp.runtime.internal;

import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBoolVector;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;

import static org.junit.jupiter.api.Assertions.*;

@SuppressWarnings("CollectionAddedToSelf")
public class ELispPrintTest {
    private String print(Object o) {
        return ELispPrint.toString(o).toString();
    }

    @Test
    public void testConsPrint() {
        assertEquals("(nil)", print(ELispCons.listOf(false)));
        assertEquals("(nil)", print(ELispCons.cons(false, false)));
        assertEquals("(1 2 3)", print(ELispCons.listOf(1L, 2L, 3L)));
        assertEquals("(1 . 2)", print(ELispCons.cons(1L, 2L)));
        assertEquals("(1 2 . 3)", print(ELispCons.cons(1L, ELispCons.cons(2L, 3L))));
    }

    @Test
    public void testBoolVecPrint() {
        assertEquals("#&10\"\\0\\0\"", print(new ELispBoolVector(new long[1], 10)));
    }

    @Test
    public void testCyclicList() {
        ELispCons cons = ELispCons.listOf(1L);
        cons.setCdr(cons);
        assertEquals("(1 #1)", print(cons));
    }
}
