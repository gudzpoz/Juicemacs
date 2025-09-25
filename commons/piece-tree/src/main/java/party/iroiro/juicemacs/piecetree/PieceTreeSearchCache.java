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

import org.jspecify.annotations.Nullable;

/// Search cache for [PieceTreeBase].
///
/// The original TypeScript implementation allows arbitrary cache sizes. However,
/// it is always used by the piece tree as a single cache entry... Therefore,
/// currently, we expect the limit to be always `1`. Otherwise, the piece table
/// will very likely end up in an invalid state (because of insufficient cache
/// flush in the original implementation).
final class PieceTreeSearchCache {
    @Nullable
    private CacheEntry cache;

    public PieceTreeSearchCache() {
        this.cache = null;
    }

    @Nullable
    public CacheEntry get(long offset) {
        CacheEntry nodePos = cache;
        if (nodePos != null) {
            if (nodePos.nodeStartOffset <= offset &&
                    nodePos.nodeStartOffset + nodePos.node.piece.length() >= offset) {
                return nodePos;
            }
        }
        return null;
    }

    @Nullable
    public CacheEntry get2(int lineNumber) {
        CacheEntry nodePos = cache;
        if (nodePos != null) {
            if (nodePos.nodeStartLineNumber != -1) {
                if (nodePos.nodeStartLineNumber < lineNumber &&
                        nodePos.nodeStartLineNumber + nodePos.node.piece.lineFeedCnt() >= lineNumber) {
                    return nodePos;
                }
            }
        }
        return null;
    }

    public void add(CacheEntry nodePos) {
        cache = nodePos;
    }

    public void validate(long offset) {
        CacheEntry nodePos = cache;
        if (nodePos != null) {
            if (nodePos.node.isDetached() || nodePos.nodeStartOffset >= offset) {
                cache = null;
            }
        }
    }

    public void clear() {
        cache = null;
    }

    public record CacheEntry(
            TreeNode node,
            long nodeStartOffset,
            int nodeStartLineNumber
    ) {
    }
}
