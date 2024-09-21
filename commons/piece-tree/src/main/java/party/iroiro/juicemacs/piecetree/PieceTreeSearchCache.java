/*---------------------------------------------------------------------------------------------
 * Copyright (c) 2024 gudzpoz
 * Copyright (c) 2015 - present Microsoft Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * See LICENSE.piece-tree for license information.
 *--------------------------------------------------------------------------------------------*/

package party.iroiro.juicemacs.piecetree;

import org.eclipse.jdt.annotation.Nullable;

import java.util.*;

public final class PieceTreeSearchCache {
    private final int limit;
    private final ArrayDeque<CacheEntry> cache;

    public PieceTreeSearchCache(int limit) {
        this.limit = limit;
        this.cache = new ArrayDeque<>();
    }

    @Nullable
    public CacheEntry get(int offset) {
        Iterator<CacheEntry> i = cache.descendingIterator();
        while (i.hasNext()) {
            CacheEntry nodePos = i.next();
            if (nodePos.nodeStartOffset <= offset &&
                    nodePos.nodeStartOffset + nodePos.node.piece.length() >= offset) {
                return nodePos;
            }
        }
        return null;
    }

    @Nullable
    public CacheEntry get2(int lineNumber) {
        Iterator<CacheEntry> i = cache.descendingIterator();
        while (i.hasNext()) {
            CacheEntry nodePos = i.next();
            if (nodePos.nodeStartLineNumber == -1) {
                continue;
            }
            if (nodePos.nodeStartLineNumber < lineNumber &&
                    nodePos.nodeStartLineNumber + nodePos.node.piece.lineFeedCnt() >= lineNumber) {
                return nodePos;
            }
        }
        return null;
    }

    public void add(CacheEntry nodePos) {
        if (cache.size() >= limit) {
            cache.pop();
        }
        cache.push(nodePos);
    }

    public void validate(int offset) {
        cache.removeIf((nodePos) -> nodePos.node.isDetached() || nodePos.nodeStartOffset >= offset);
    }

    public void clear() {
        cache.clear();
    }

    public record CacheEntry(
            TreeNode node,
            int nodeStartOffset,
            int nodeStartLineNumber
    ) {
    }
}
