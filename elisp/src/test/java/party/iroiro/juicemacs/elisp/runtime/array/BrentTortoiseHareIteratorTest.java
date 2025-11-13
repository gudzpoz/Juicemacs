package party.iroiro.juicemacs.elisp.runtime.array;

import org.junit.jupiter.api.Test;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.FNthcdr;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;

import java.math.BigInteger;
import java.util.NoSuchElementException;
import java.util.stream.LongStream;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;

public class BrentTortoiseHareIteratorTest {
    @Test
    public void testApi() {
        ELispCons list2 = ELispCons.cons(2L, false);
        ELispCons list1 = ELispCons.cons(1L, list2);
        ELispCons list0 = ELispCons.cons(0L, list1);
        ConsIterator iterator = list0.iterator();

        assertSame(list0, iterator.peekNextCons());
        assertTrue(iterator.hasNext());
        assertEquals(0L, iterator.next());

        assertSame(list1, iterator.peekNextCons());
        assertTrue(iterator.hasNext());
        assertSame(list1, iterator.nextCons());

        assertSame(list2, iterator.peekNextCons());
        assertTrue(iterator.hasNext());
        assertEquals(2L, iterator.next());

        assertFalse(iterator.hasNext());
        assertThrows(NoSuchElementException.class, iterator::peekNextCons);
    }

    @Test
    public void testCircularConsistent() {
        ELispCons list2 = ELispCons.cons(2L, false);
        ELispCons list1 = ELispCons.cons(1L, list2);
        ELispCons list0 = ELispCons.cons(0L, list1);
        list2.setCdr(list0);

        for (int count = 0; count < 100; count++) {
            long expected = count % 3;
            ConsIterator iterator = list0.iterator();
            ELispCons tail = list0;
            for (int j = 0; j <= count; j++) {
                assertTrue(iterator.hasNext());
                tail = iterator.peekNextCons();
                iterator.nextConsOrCircular();
            }
            assertEquals(expected, tail.car());
        }
    }

    @Test
    public void testCircular() {
        ELispCons list = asCons(ELispCons.listOf(LongStream.range(0, 100).boxed().toArray()));
        asCons(FNthcdr.nthcdr(99, list)).setCdr(list);

        for (long i = 0; i < 10000; i++) {
            long expected = i % 100;
            assertEquals(expected, asCons(FNthcdr.nthcdr(i, list)).car()); // non-circular detection
            assertEquals(expected, asCons(FNthcdr.nthcdrMaybeCircular(i, list)).car()); // circular modulus
        }
        for (long i = Long.MAX_VALUE - 10000; i < Long.MAX_VALUE; i++) {
            long idx = i + 1;
            long expected = idx % 100;
            assertEquals(expected, asCons(FNthcdr.nthcdr(expected, list)).car()); // non-circular detection
            assertEquals(expected, asCons(FNthcdr.nthcdrMaybeCircular(idx, list)).car()); // circular modulus
        }
        for (long i = 0; i < 10000; i++) {
            long expected = i % 100;
            Object idx = ELispBigNum.wrap(
                    new BigInteger("100").pow(10)
                            .add(BigInteger.valueOf(i))
            );
            assertEquals(expected, asCons(FNthcdr.nthcdrMaybeCircular(idx, list)).car()); // circular modulus
        }
    }
}
