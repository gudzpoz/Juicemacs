package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.strings.AbstractTruffleString;
import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList;

public final class LineStartList extends IntArrayList {
    public void appendLineStarts(AbstractTruffleString str, int offset) {
        int length = StringNodes.length(str);
        int last = 0;
        while (last < length) {
            last = StringNodes.indexOf(str, '\n', last, length);
            if (last < 0) {
                break;
            }
            add(++last + offset);
        }
    }

    public void addAll(LineStartList other) {
        int[] inner = other.items;
        ensureCapacity(size + other.size());
        System.arraycopy(inner, 0, items, size, other.size());
        size += other.size();
    }
}
