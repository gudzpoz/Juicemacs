package party.iroiro.juicemacs.elisp.runtime.objects;

import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.forms.BuiltInCharTab.charTableMap;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;
import static party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable.*;

public class ELispCharTableTest {
    @Test
    public void testCharTableMap() {
        ELispCharTable table = ELispCharTable.create(false, NIL, 0);
        int subTableEnd = 1 << (CHARTAB_SIZE_BITS_3 + CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_1);
        table.setChar(subTableEnd - 1, true);
        table.setChar(subTableEnd, true);
        AtomicInteger count = new AtomicInteger(0);
        charTableMap(table, (cons, v) -> {
            assertEquals("(65535 . 65536)", cons.toString());
            assertEquals(true, v);
            count.incrementAndGet();
        });
        assertEquals(1, count.get());
    }
}
