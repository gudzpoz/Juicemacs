package party.iroiro.juicemacs.piecetree;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class OrderedIntArrayListTest {
    @Test
    public void testExceptions() {
        OrderedIntArrayList list = OrderedIntArrayList.ofConstant(OrderedIntArrayList.ofLazy(0, 0));
        assertThrows(UnsupportedOperationException.class, () -> list.set(0, 1));
        assertThrows(UnsupportedOperationException.class, list::pop);
        assertThrows(UnsupportedOperationException.class, () -> list.addAll(
                OrderedIntArrayList.ofLazy(0, 0),
                1
        ));
    }

    @Test
    public void testConstant() {
        int[] maxValues = new int[] { Integer.MAX_VALUE, Short.MAX_VALUE };
        Class<?>[] types = new Class<?>[] { OrderedIntArrayList.OfIntConstant.class, OrderedIntArrayList.OfShortConstant.class };
        for (int i = 0; i < 2; i++) {
            OrderedIntArrayList.LazyArrayList lazy = OrderedIntArrayList.ofLazy(10, maxValues[i]);
            lazy.set(20, 100);
            OrderedIntArrayList list = OrderedIntArrayList.ofConstant(lazy);
            assertInstanceOf(types[i], list);
            assertEquals(100, list.get(20));
            assertEquals(100, list.getLast());
            assertEquals(21, list.size());
            assertThrows(IndexOutOfBoundsException.class, () -> list.get(21));
        }
    }

    @Test
    public void testLazy() {
        OrderedIntArrayList.LazyArrayList lazy = OrderedIntArrayList.ofLazy(10, Short.MAX_VALUE);
        assertEquals(0, lazy.size());
        lazy.pop();
        assertEquals(0, lazy.size());
        lazy.set(20, 100);
        assertEquals(21, lazy.size());
        assertEquals(100, lazy.get(20));
        assertEquals(100, lazy.getLast());
        lazy.pop();
        assertEquals(20, lazy.size());
        lazy.set(0, 1);
        assertEquals(1, lazy.get(0));

        lazy.set(100, Integer.MAX_VALUE);
        assertEquals(101, lazy.size());
        assertEquals(Integer.MAX_VALUE, lazy.get(100));
        lazy.set(0, 2);
        assertEquals(2, lazy.get(0));

        lazy.set(100, 100);
        lazy.addAll(lazy, 2);
        assertEquals(200, lazy.size());
        int sum = 0;
        for (int i = 0; i < lazy.size(); i++) {
            sum += lazy.get(i);
        }
        assertEquals(2 + 100 + 100 + 100 + 100, sum);
    }

    public void assertList(int[] expected, OrderedIntArrayList list) {
        assertEquals(expected.length, list.size());
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], list.get(i));
        }
    }

    @Test
    public void testLazyAddAll() {
        OrderedIntArrayList.LazyArrayList shortLazy = OrderedIntArrayList.ofLazy(1, Short.MAX_VALUE);
        shortLazy.set(0, 1);
        OrderedIntArrayList shorts = OrderedIntArrayList.ofConstant(shortLazy);
        OrderedIntArrayList.LazyArrayList intLazy = OrderedIntArrayList.ofLazy(1, Integer.MAX_VALUE);
        intLazy.set(1, 100);
        OrderedIntArrayList integers = OrderedIntArrayList.ofConstant(intLazy);

        OrderedIntArrayList.LazyArrayList lazy = OrderedIntArrayList.ofLazy(2, Short.MAX_VALUE);
        lazy.set(0, 20);
        lazy.addAll(shorts, 1);
        assertList(new int[] { 20 }, lazy);
        lazy.addAll(shorts, 0);
        assertList(new int[] { 20, 1 }, lazy);
        lazy.addAll(lazy, 1);
        assertList(new int[] { 20, 1, 1 }, lazy);
        lazy.addAll(integers, 0); // converted to integers
        assertList(new int[] { 20, 1, 1, 0, 100 }, lazy);
        lazy.addAll(shortLazy, 0);
        assertList(new int[] { 20, 1, 1, 0, 100, 1 }, lazy);
        lazy.addAll(shorts, 0);
        assertList(new int[] { 20, 1, 1, 0, 100, 1, 1 }, lazy);

        lazy = OrderedIntArrayList.ofLazy(2, Short.MAX_VALUE);
        lazy.addAll(intLazy, 1);
        assertList(new int[] { 100 }, lazy);
    }
}
