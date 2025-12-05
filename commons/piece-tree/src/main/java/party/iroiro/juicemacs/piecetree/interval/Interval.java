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

package party.iroiro.juicemacs.piecetree.interval;

public final class Interval {
    //#region Constants
    public static final int COLOR_OFFSET         = 0;
    public static final int IS_VISITED_OFFSET    = 1;
    public static final int FRONT_ADVANCE_OFFSET = 2;
    public static final int REAR_ADVANCE_OFFSET  = 3;

    public static final int COLOR_MASK         = 1 << COLOR_OFFSET;
    public static final int IS_VISITED_MASK    = 1 << IS_VISITED_OFFSET;
    public static final int FRONT_ADVANCE_MASK = 1 << FRONT_ADVANCE_OFFSET;
    public static final int REAR_ADVANCE_MASK  = 1 << REAR_ADVANCE_OFFSET;
    //#endregion

    public static final boolean RED   = true;
    public static final boolean BLACK = false;
    public static final Interval SENTINEL = new Interval(0, 0);
    static { SENTINEL.setColor(BLACK); }

    /// contains binary encoded information for color, visited, isForValidation and stickiness.
    public int metadata;

    Interval parent;
    Interval left;
    Interval right;

    long start;
    long end;
    long delta;
    long maxEnd;

    public int cachedVersionId;
    public long cachedAbsoluteStart;
    public long cachedAbsoluteEnd;

    public Object userData = false;

    public Interval(long start, long end) {
        metadata = 0;

        parent = left = right = this;
        setColor(RED);

        this.start = start;
        this.end = end;
        this.delta = 0;
        this.maxEnd = end;

        cachedVersionId = 0;
        cachedAbsoluteStart = start;
        cachedAbsoluteEnd = end;

        setVisited(false);
    }

    public void reset(int versionId, long start, long end) {
        this.start = start;
        this.end = end;
        this.maxEnd = end;
        this.cachedVersionId = versionId;
        this.cachedAbsoluteStart = start;
        this.cachedAbsoluteEnd = end;
    }

    public boolean isRed() {
        return (metadata & COLOR_MASK) != 0;
    }

    public void setColor(boolean red) {
        metadata = (metadata & ~COLOR_MASK) | (red ? COLOR_MASK : 0);
    }

    public boolean isVisited() {
        return (metadata & IS_VISITED_MASK) != 0;
    }

    public void setVisited(boolean visited) {
        metadata = (metadata & ~IS_VISITED_MASK) | (visited ? IS_VISITED_MASK : 0);
    }

    public boolean isFrontAdvance() {
        return (metadata & FRONT_ADVANCE_MASK) != 0;
    }

    public void setFrontAdvance(boolean frontAdvance) {
        metadata = (metadata & ~FRONT_ADVANCE_MASK) | (frontAdvance ? FRONT_ADVANCE_MASK : 0);
    }

    public boolean isRearAdvance() {
        return (metadata & REAR_ADVANCE_MASK) != 0;
    }

    public void setRearAdvance(boolean rearAdvance) {
        metadata = (metadata & ~REAR_ADVANCE_MASK) | (rearAdvance ? REAR_ADVANCE_MASK : 0);
    }

    public void setCachedOffsets(long absoluteStart, long absoluteEnd, int cachedVersionId) {
        this.cachedAbsoluteStart = absoluteStart;
        this.cachedAbsoluteEnd = absoluteEnd;
        this.cachedVersionId = cachedVersionId;
    }

    @SuppressWarnings("unchecked")
    public <T> T getUserData() {
        return (T) userData;
    }

    public void setUserData(Object userData) {
        this.userData = userData;
    }

    public Interval leftest() {
        Interval x = this;
        while (x.left != SENTINEL) {
            x = x.left;
        }
        return x;
    }

    @SuppressWarnings("DataFlowIssue")
    public void detach() {
        parent = left = right = null;
    }

    //#region Editing
    boolean adjustMarkerBeforeColumn(
            long markerOffset, boolean markerStickToPreviousCharacter, long checkOffset, boolean forceStay
    ) {
        if (markerOffset < checkOffset) {
            return true;
        }
        if (markerOffset > checkOffset) {
            return false;
        }
        if (forceStay) {
            return true;
        }
        return markerStickToPreviousCharacter;
    }
    /**
     * This is a lot more complicated than strictly necessary to maintain the same behaviour
     * as when decorations were implemented using two markers.
     */
    @SuppressWarnings("UnnecessaryLocalVariable")
    void acceptEdit(long start, long end, long textLength) {
        boolean frontAdvance = isFrontAdvance();
        boolean rearAdvance = isRearAdvance();
        boolean startStickToPreviousCharacter = !frontAdvance;
        boolean endStickToPreviousCharacter = !rearAdvance;
        Interval node = this;

        long deletingCnt = end - start;
        long insertingCnt = textLength;
        long commonLength = Math.min(deletingCnt, insertingCnt);

        long nodeStart = node.start;
        boolean startDone = false;

        long nodeEnd = node.end;
        boolean endDone = false;

        {
            boolean moveSemantics = deletingCnt > 0;
            if (adjustMarkerBeforeColumn(nodeStart, startStickToPreviousCharacter, start, moveSemantics)) {
                startDone = true;
            }
            if (adjustMarkerBeforeColumn(nodeEnd, endStickToPreviousCharacter, start, moveSemantics)) {
                endDone = true;
            }
        }

        if (commonLength > 0) {
            boolean moveSemantics = deletingCnt > insertingCnt;
            if (!startDone && adjustMarkerBeforeColumn(nodeStart, startStickToPreviousCharacter, start + commonLength, moveSemantics)) {
                startDone = true;
            }
            if (!endDone && adjustMarkerBeforeColumn(nodeEnd, endStickToPreviousCharacter, start + commonLength, moveSemantics)) {
                endDone = true;
            }
        }

        {
            boolean moveSemantics = false;
            if (!startDone && adjustMarkerBeforeColumn(nodeStart, startStickToPreviousCharacter, end, moveSemantics)) {
                node.start = start + insertingCnt;
                startDone = true;
            }
            if (!endDone && adjustMarkerBeforeColumn(nodeEnd, endStickToPreviousCharacter, end, moveSemantics)) {
                node.end = start + insertingCnt;
                endDone = true;
            }
        }

        // Finish
        long deltaColumn = (insertingCnt - deletingCnt);
        if (!startDone) {
            node.start = Math.max(0, nodeStart + deltaColumn);
        }
        if (!endDone) {
            node.end = Math.max(0, nodeEnd + deltaColumn);
        }

        if (node.start > node.end) {
            node.start = node.end;
        }
    }
    //#endregion

    //#region max end computation
    long computeMaxEnd() {
        Interval node = this;
        long maxEnd = node.end;
        if (node.left != SENTINEL) {
            maxEnd = Math.max(maxEnd, node.left.maxEnd);
        }
        if (node.right != SENTINEL) {
            maxEnd = Math.max(maxEnd, node.right.maxEnd + node.delta);
        }
        return maxEnd;
    }
    void recomputeMaxEnd() {
        maxEnd = computeMaxEnd();
    }
    void recomputeMaxEndWalkToRoot() {
        Interval node = this;
        while (node != SENTINEL) {
            long maxEnd = node.computeMaxEnd();
            if (node.maxEnd == maxEnd) {
                // no need to go further
                return;
            }
            node.maxEnd = maxEnd;
            node = node.parent;
        }
    }
    //#endregion
}
