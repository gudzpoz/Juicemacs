package party.iroiro.juicemacs.elisp.runtime.array;

import org.apache.commons.lang3.stream.IntStreams;
import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.forms.BuiltInAlloc;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;

import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public class ELispConsTest {
    @Test
    public void testCar() {
        assertEquals(false, BuiltInData.FCar.car(false));
        assertEquals(false, ELispCons.cons(false, true).car());
        assertEquals(false, ELispCons.listOf(false).car());
        assertEquals(false, ELispCons.listOf(false, true).car());
    }

    @Test
    public void testCdr() {
        assertEquals(false, BuiltInData.FCdr.cdr(false));
        assertEquals(true, ELispCons.cons(false, true).cdr());
        assertTrue(isNil(ELispCons.listOf(false).cdr()));
        Object cdr = ELispCons.listOf(false, true).cdr();
        ELispCons cons = assertInstanceOf(ELispCons.class, cdr);
        assertEquals(true, cons.car());
        assertTrue(isNil(cons.cdr()));

        assertEquals(false, BuiltInFns.FNthcdr.nthcdr(10, ELispCons.listOf(1L, 2L, 3L)));
    }

    @Test
    public void testCons() {
        assertEquals("(nil)", ELispCons.cons(false, false).toString());
        assertEquals("(nil)", ELispCons.cons(false, ELispCons.listOf()).toString());
        assertEquals("(nil . t)", ELispCons.cons(false, true).toString());
        assertEquals("(nil 1 2)", ELispCons.cons(false, ELispCons.listOf(1L, 2L)).toString());
        assertEquals("(nil 1 . 2)", ELispCons.cons(false, ELispCons.cons(1L, 2L)).toString());
    }

    @Test
    public void testConsToString() {
        assertEquals(false, ELispCons.listOf());
        assertEquals("(nil)", ELispCons.listOf(false).toString());
        assertEquals("(nil . t)", ELispCons.cons(false, true).toString());
        assertEquals("(t nil nil)", ELispCons.listOf(true, false, false).toString());

        ELispCons list = (ELispCons) ELispCons.listOf(1L, 2L, 3L);
        assertEquals("(1 2 3)", list.toString());
        assertEquals("(2 3)", list.cdr().toString());
        assertEquals("(1 2 3)", ELispCons.cons(1L, list.cdr()).toString());

        Object cdr = list.cdr();
        list.setCdr(false);
        assertEquals("(1)", list.toString());
        assertEquals("(2 3)", cdr.toString());
        assertEquals("(3)", BuiltInData.FCdr.cdr(cdr).toString());
    }

    @Test
    public void testListBuild() {
        ELispCons list = (ELispCons) ELispCons.listOf(IntStreams.range(100).mapToObj(i -> (long) i).toArray());
        for (int i = 0; i < 100; i++) {
            assertEquals(
                    "(" + IntStreams.range(100).skip(i)
                            .mapToObj(Integer::toString).collect(Collectors.joining(" "))
                    + ")",
                    BuiltInFns.FNthcdr.nthcdr(i, list).toString()
            );
        }
    }

    @Test
    public void testManualBuild() {
        ELispCons head = ELispCons.cons(-1L, false);
        ELispCons tail = head;
        for (long i = 0; i < 100; i++) {
            ELispCons next = ELispCons.listOf(i);
            tail.setCdr(next);
            tail = next;
        }
        for (int i = 0; i < 100; i++) {
            assertEquals(
                    "(" + IntStreams.range(100).skip(i)
                            .mapToObj(Integer::toString).collect(Collectors.joining(" "))
                    + ")",
                    BuiltInFns.FNthcdr.nthcdr(i + 1, head).toString()
            );
        }
        ELispCons.ConsIterator iter = head.listIterator(0);
        for (int i = -1; i < 100; i++) {
            assertTrue(iter.hasNextCons());
            assertTrue(iter.hasNext());
            assertEquals((long) i, iter.nextCons().car());
        }
    }

    @Test
    public void testListIterator() {
        assertEquals(false, BuiltInFns.FNth.nth(3, ELispCons.listOf(1, 2, 3)));
        assertEquals(2, BuiltInFns.FNth.nth(1, ELispCons.listOf(1, 2, 3)));

        Object list = ELispCons.listOf(1, 2, 3);
        list = BuiltInAlloc.FCons.cons(-1, BuiltInData.FCdr.cdr(list));
        assertEquals(3, BuiltInFns.FNth.nth(2, list));
    }

    @Test
    public void testDelq() {
        Object list = ELispCons.listOf(1L, 2L, 3L);
        BuiltInData.FSetcdr.setcdr(asCons(list), BuiltInFns.FNthcdr.nthcdr(2, list));
        Object delq = BuiltInFns.FDelq.delq(1L, list);
        assertInstanceOf(ELispCons.class, delq);
    }

    @Test
    public void testFilter() {
        Object o = ELispConsAccessFactory.ConsFilterNodeGen.getUncached()
                .executeFilter(null, ELispCons.listOf(1L, 2L, 3L, 2L, 5L),
                        (i) -> i != (Long) 2L);
        assertEquals("(1 3 5)", o.toString());
        o = ELispConsAccessFactory.ConsFilterNodeGen.getUncached()
                .executeFilter(null, o, (i) -> i != (Long) 3L);
        assertEquals("(1 5)", o.toString());
    }
}
