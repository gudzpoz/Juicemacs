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

import java.util.ArrayList;
import java.util.function.Consumer;

import static party.iroiro.juicemacs.piecetree.interval.Interval.*;

/// Transliterated from [pieceTreeBase.ts @ vscode](https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/intervalTree.ts)
public final class IntervalTree {
    public static final int MIN_SAFE_DELTA = Integer.MIN_VALUE;
    public static final int MAX_SAFE_DELTA = Integer.MAX_VALUE;

    public Interval root;
    public boolean requestNormalizeDelta;

    public IntervalTree() {
        root = SENTINEL;
        requestNormalizeDelta = false;
    }

    public void insert(Interval interval) {
        rbTreeInsert(interval);
        normalizeDeltaIfNecessary();
    }
    public void delete(Interval interval) {
        rbTreeDelete(interval);
        normalizeDeltaIfNecessary();
    }
    public void resolveNode(Interval node, int cachedVersionId) {
        Interval initialNode = node;
        long delta = 0;
        while (node != root) {
            if (node == node.parent.right) {
                delta += node.parent.delta;
            }
            node = node.parent;
        }

        long nodeStart = initialNode.start + delta;
        long nodeEnd = initialNode.end + delta;
        initialNode.setCachedOffsets(nodeStart, nodeEnd, cachedVersionId);
    }

    public void acceptReplace(long offset, long length, long textLength) {
        // Our strategy is to remove all directly impacted nodes, and then add them back to the tree.

        // (1) collect all nodes that are intersecting this edit as nodes of interest
		ArrayList<Interval> nodesOfInterest = searchForEditing(offset, offset + length);

		// (2) remove all nodes that are intersecting this edit
        for (Interval node : nodesOfInterest) {
            rbTreeDelete(node);
        }
		this.normalizeDeltaIfNecessary();

		// (3) edit all tree nodes except the nodes of interest
		noOverlapReplace(offset, offset + length, textLength);
		this.normalizeDeltaIfNecessary();

		// (4) edit the nodes of interest and insert them back in the tree
        for (Interval node : nodesOfInterest) {
            node.start = node.cachedAbsoluteStart;
            node.end = node.cachedAbsoluteEnd;
            node.acceptEdit(offset, (offset + length), textLength);
            node.maxEnd = node.end;
            rbTreeInsert(node);
        }
		this.normalizeDeltaIfNecessary();
    }

    //#region Editing
    @SuppressWarnings("DuplicatedCode")
    private ArrayList<Interval> searchForEditing(long start, long end) {
        // https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree
        // Now, it is known that two intervals A and B overlap only when both
        // A.low <= B.high and A.high >= B.low. When searching the trees for
        // nodes overlapping with a given interval, you can immediately skip:
        //  a) all nodes to the right of nodes whose low value is past the end of the given interval.
        //  b) all nodes that have their maximum 'high' value below the start of the given interval.
        Interval node = root;
        long delta = 0;
        long nodeMaxEnd;
        long nodeStart;
        long nodeEnd;
        ArrayList<Interval> result = new ArrayList<>();
        while (node != SENTINEL) {
            if (node.isVisited()) {
                // going up from this node
                node.left.setVisited(false);
                node.right.setVisited(false);
                if (node == node.parent.right) {
                    delta -= node.parent.delta;
                }
                node = node.parent;
                continue;
            }

            if (!node.left.isVisited()) {
                // first time seeing this node
                nodeMaxEnd = delta + node.maxEnd;
                if (nodeMaxEnd < start) {
                    // cover case b) from above
                    // there is no need to search this node or its children
                    node.setVisited(true);
                    continue;
                }

                if (node.left != SENTINEL) {
                    // go left
                    node = node.left;
                    continue;
                }
            }

            // handle current node
            nodeStart = delta + node.start;
            if (nodeStart > end) {
                // cover case a) from above
                // there is no need to search this node or its right subtree
                node.setVisited(true);
                continue;
            }

            nodeEnd = delta + node.end;
            if (nodeEnd >= start) {
                node.setCachedOffsets(nodeStart, nodeEnd, 0);
                result.add(node);
            }
            node.setVisited(true);

            if (node.right != SENTINEL && !node.right.isVisited()) {
                // go right
                delta += node.delta;
                node = node.right;
                // continue;
            }
        }

        root.setVisited(false);
        return result;
    }

    @SuppressWarnings("DuplicatedCode")
    private void noOverlapReplace(long start, long end, long textLength) {
        // https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree
        // Now, it is known that two intervals A and B overlap only when both
        // A.low <= B.high and A.high >= B.low. When searching the trees for
        // nodes overlapping with a given interval, you can immediately skip:
        //  a) all nodes to the right of nodes whose low value is past the end of the given interval.
        //  b) all nodes that have their maximum 'high' value below the start of the given interval.
        Interval node = root;
        long delta = 0;
        long nodeMaxEnd;
        long nodeStart;
        long editDelta = textLength - (end - start);
        while (node != SENTINEL) {
            if (node.isVisited()) {
                // going up from this node
                node.left.setVisited(false);
                node.right.setVisited(false);
                if (node == node.parent.right) {
                    delta -= node.parent.delta;
                }
                node.recomputeMaxEnd();
                node = node.parent;
                continue;
            }

            if (!node.left.isVisited()) {
                // first time seeing this node
                nodeMaxEnd = delta + node.maxEnd;
                if (nodeMaxEnd < start) {
                    // cover case b) from above
                    // there is no need to search this node or its children
                    node.setVisited(true);
                    continue;
                }

                if (node.left != SENTINEL) {
                    // go left
                    node = node.left;
                    continue;
                }
            }

            // handle current node
            nodeStart = delta + node.start;
            if (nodeStart > end) {
                node.start += editDelta;
                node.end += editDelta;
                node.delta += editDelta;
                if (node.delta < MIN_SAFE_DELTA || node.delta > MAX_SAFE_DELTA) {
                    requestNormalizeDelta = true;
                }
                // cover case a) from above
                // there is no need to search this node or its right subtree
                node.setVisited(true);
                continue;
            }

            node.setVisited(true);

            if (node.right != SENTINEL && !node.right.isVisited()) {
                // go right
                delta += node.delta;
                node = node.right;
                // continue;
            }
        }

        root.setVisited(false);
    }
    //#endregion

    //#region Delta Normalization
    private void normalizeDeltaIfNecessary() {
        if (!requestNormalizeDelta) {
            return;
        }
        requestNormalizeDelta = false;
        normalizeDelta();
    }
    private void normalizeDelta() {
        Interval node = root;
        long delta = 0;
        while (node != SENTINEL) {
            if (node.left != SENTINEL && !node.left.isVisited()) {
                // go left
                node = node.left;
                continue;
            }

            if (node.right != SENTINEL && !node.right.isVisited()) {
                // go right
                delta += node.delta;
                node = node.right;
                continue;
            }

            // handle current node
            node.start = delta + node.start;
            node.end = delta + node.end;
            node.delta = 0;
            node.recomputeMaxEnd();

            node.setVisited(true);

            // going up from this node
            node.left.setVisited(false);
            node.right.setVisited(false);
            if (node == node.parent.right) {
                delta -= node.parent.delta;
            }
            node = node.parent;
        }

        root.setVisited(false);
    }
    //#endregion

    //#region Searching
    @SuppressWarnings("DuplicatedCode")
    public void intervalSearch(long intervalStart, long intervalEnd, int cachedVersionId, Consumer<Interval> consumer) {
        if (root == SENTINEL) {
            return;
        }

        // https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree
        // Now, it is known that two intervals A and B overlap only when both
        // A.low <= B.high and A.high >= B.low. When searching the trees for
        // nodes overlapping with a given interval, you can immediately skip:
        //  a) all nodes to the right of nodes whose low value is past the end of the given interval.
        //  b) all nodes that have their maximum 'high' value below the start of the given interval.
        Interval node = root;
        long delta = 0;
        long nodeMaxEnd;
        long nodeStart;
        long nodeEnd;
        while (node != SENTINEL) {
            if (node.isVisited()) {
                // going up from this node
                node.left.setVisited(false);
                node.right.setVisited(false);
                if (node == node.parent.right) {
                    delta -= node.parent.delta;
                }
                node = node.parent;
                continue;
            }

            if (!node.left.isVisited()) {
                // first time seeing this node
                nodeMaxEnd = delta + node.maxEnd;
                if (nodeMaxEnd < intervalStart) {
                    // cover case b) from above
                    // there is no need to search this node or its children
                    node.setVisited(true);
                    continue;
                }

                if (node.left != SENTINEL) {
                    // go left
                    node = node.left;
                    continue;
                }
            }

            // handle current node
            nodeStart = delta + node.start;
            if (nodeStart > intervalEnd) {
                // cover case a) from above
                // there is no need to search this node or its right subtree
                node.setVisited(true);
                continue;
            }

            nodeEnd = delta + node.end;

            if (nodeEnd >= intervalStart) {
                // There is overlap
                node.setCachedOffsets(nodeStart, nodeEnd, cachedVersionId);

                consumer.accept(node);
            }

            node.setVisited(true);
            if (node.right != SENTINEL && !node.right.isVisited()) {
                // go right
                delta += node.delta;
                node = node.right;
                // continue;
            }
        }

        root.setVisited(false);
    }
    //#endregion

    //#region Insertion
    @SuppressWarnings("SuspiciousNameCombination")
    private void rbTreeInsert(Interval newNode) {
        if (root == SENTINEL) {
            newNode.parent = newNode.left = newNode.right = SENTINEL;
            newNode.setColor(BLACK);
            root = newNode;
            return;
        }

        treeInsert(newNode);
        newNode.parent.recomputeMaxEndWalkToRoot();

        // repair tree
        Interval x = newNode;
        while (x != root && x.parent.isRed()) {
            if (x.parent == x.parent.parent.left) {
                Interval y = x.parent.parent.right;

                if (y.isRed()) {
                    x.parent.setColor(BLACK);
                    y.setColor(BLACK);
                    x.parent.parent.setColor(RED);
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.right) {
                        x = x.parent;
                        leftRotate(x);
                    }
                    x.parent.setColor(BLACK);
                    x.parent.parent.setColor(RED);
                    rightRotate(x.parent.parent);
                }
            } else {
                Interval y = x.parent.parent.left;

                if (y.isRed()) {
                    x.parent.setColor(BLACK);
                    y.setColor(BLACK);
                    x.parent.parent.setColor(RED);
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.left) {
                        x = x.parent;
                        rightRotate(x);
                    }
                    x.parent.setColor(BLACK);
                    x.parent.parent.setColor(RED);
                    leftRotate(x.parent.parent);
                }
            }
        }

        root.setColor(BLACK);
    }
    private void treeInsert(Interval z) {
        long delta = 0;
        Interval x = root;
        long zAbsoluteStart = z.start;
        long zAbsoluteEnd = z.end;
        while (true) {
            int cmp = intervalCompare(zAbsoluteStart, zAbsoluteEnd, x.start + delta, x.end + delta);
            if (cmp < 0) {
                // this node should be inserted to the left
                // => it is not affected by the node's delta
                if (x.left == SENTINEL) {
                    z.start -= delta;
                    z.end -= delta;
                    z.maxEnd -= delta;
                    x.left = z;
                    break;
                } else {
                    x = x.left;
                }
            } else {
                // this node should be inserted to the right
                // => it is not affected by the node's delta
                if (x.right == SENTINEL) {
                    z.start -= delta + x.delta;
                    z.end -= delta + x.delta;
                    z.maxEnd -= delta + x.delta;
                    x.right = z;
                    break;
                } else {
                    delta += x.delta;
                    x = x.right;
                }
            }
        }

        z.parent = x;
        z.left = SENTINEL;
        z.right = SENTINEL;
        z.setColor(RED);
    }
    //#endregion

    //#region Deletion
    @SuppressWarnings("SuspiciousNameCombination")
    private void rbTreeDelete(Interval z) {
        Interval x, y;

        // RB-DELETE except we don't swap z and y in case c)
        // i.e. we always delete what's pointed at by z.

        if (z.left == SENTINEL) {
            x = z.right;
            y = z;

            // x's delta is no longer influenced by z's delta
            x.delta += z.delta;
            if (x.delta < MIN_SAFE_DELTA || x.delta > MAX_SAFE_DELTA) {
                requestNormalizeDelta = true;
            }
            x.start += z.delta;
            x.end += z.delta;
        } else if (z.right == SENTINEL) {
            x = z.left;
            y = z;
        } else {
            y = z.right.leftest();
            x = y.right;

            // y's delta is no longer influenced by z's delta,
            // but we don't want to walk the entire right-hand-side subtree of x.
            // we therefore maintain z's delta in y, and adjust only x
            x.start += y.delta;
            x.end += y.delta;
            x.delta += y.delta;
            if (x.delta < MIN_SAFE_DELTA || x.delta > MAX_SAFE_DELTA) {
                requestNormalizeDelta = true;
            }

            y.start += z.delta;
            y.end += z.delta;
            y.delta = z.delta;
            if (y.delta < MIN_SAFE_DELTA || y.delta > MAX_SAFE_DELTA) {
                requestNormalizeDelta = true;
            }
        }

        if (y == root) {
            root = x;
            x.setColor(BLACK);

            z.detach();
            resetSentinel();
            x.recomputeMaxEnd();
            root.parent = SENTINEL;
            return;
        }

        boolean yWasRed = y.isRed();

        if (y == y.parent.left) {
            y.parent.left = x;
        } else {
            y.parent.right = x;
        }

        if (y == z) {
            x.parent = y.parent;
        } else {

            if (y.parent == z) {
                x.parent = y;
            } else {
                x.parent = y.parent;
            }

            y.left = z.left;
            y.right = z.right;
            y.parent = z.parent;
            y.setColor(z.isRed());

            if (z == root) {
                root = y;
            } else {
                if (z == z.parent.left) {
                    z.parent.left = y;
                } else {
                    z.parent.right = y;
                }
            }

            if (y.left != SENTINEL) {
                y.left.parent = y;
            }
            if (y.right != SENTINEL) {
                y.right.parent = y;
            }
        }

        z.detach();

        if (yWasRed) {
            x.parent.recomputeMaxEndWalkToRoot();
            if (y != z) {
                y.recomputeMaxEndWalkToRoot();
                y.parent.recomputeMaxEndWalkToRoot();
            }
            resetSentinel();
            return;
        }

        x.recomputeMaxEndWalkToRoot();
        x.parent.recomputeMaxEndWalkToRoot();
        if (y != z) {
            y.recomputeMaxEndWalkToRoot();
            y.parent.recomputeMaxEndWalkToRoot();
        }

        // RB-DELETE-FIXUP
        Interval w;
        while (x != root && !x.isRed()) {

            if (x == x.parent.left) {
                w = x.parent.right;

                if (w.isRed()) {
                    w.setColor(BLACK);
                    x.parent.setColor(RED);
                    leftRotate(x.parent);
                    w = x.parent.right;
                }

                if (!w.left.isRed() && !w.right.isRed()) {
                    w.setColor(RED);
                    x = x.parent;
                } else {
                    if (!w.right.isRed()) {
                        w.left.setColor(BLACK);
                        w.setColor(RED);
                        rightRotate(w);
                        w = x.parent.right;
                    }

                    w.setColor(x.parent.isRed());
                    x.parent.setColor(BLACK);
                    w.right.setColor(BLACK);
                    leftRotate(x.parent);
                    x = root;
                }

            } else {
                w = x.parent.left;

                if (w.isRed()) {
                    w.setColor(BLACK);
                    x.parent.setColor(RED);
                    rightRotate(x.parent);
                    w = x.parent.left;
                }

                if (!w.left.isRed() && !w.right.isRed()) {
                    w.setColor(RED);
                    x = x.parent;
                } else {
                    if (!w.left.isRed()) {
                        w.right.setColor(BLACK);
                        w.setColor(RED);
                        leftRotate(w);
                        w = x.parent.left;
                    }

                    w.setColor(x.parent.isRed());
                    x.parent.setColor(BLACK);
                    w.left.setColor(BLACK);
                    rightRotate(x.parent);
                    x = root;
                }
            }
        }

        x.setColor(BLACK);
        resetSentinel();
    }

    static void resetSentinel() {
        SENTINEL.parent = SENTINEL;
        SENTINEL.delta = 0;
        SENTINEL.start = 0;
        SENTINEL.end = 0;
    }
    //#endregion

    //#region Rotations
    @SuppressWarnings("SuspiciousNameCombination")
    private void leftRotate(Interval x) {
        Interval y = x.right;    // set y.

        y.delta += x.delta;  // y's delta is no longer influenced by x's delta
        if (y.delta < MIN_SAFE_DELTA || y.delta > MAX_SAFE_DELTA) {
            requestNormalizeDelta = true;
        }
        y.start += x.delta;
        y.end += x.delta;

        x.right = y.left;    // turn y's left subtree into x's right subtree.
        if (y.left != SENTINEL) {
            y.left.parent = x;
        }
        y.parent = x.parent; // link x's parent to y.
        if (x.parent == SENTINEL) {
            root = y;
        } else if (x == x.parent.left) {
            x.parent.left = y;
        } else {
            x.parent.right = y;
        }
        y.left = x;          // put x on y's left.
        x.parent = y;
        x.recomputeMaxEnd();
        y.recomputeMaxEnd();
    }
    @SuppressWarnings("SuspiciousNameCombination")
    private void rightRotate(Interval y) {
        Interval x = y.left;

        y.delta -= x.delta;
        if (y.delta < MIN_SAFE_DELTA || y.delta > MAX_SAFE_DELTA) {
            requestNormalizeDelta = true;
        }
        y.start -= x.delta;
        y.end -= x.delta;

        y.left = x.right;
        if (x.right != SENTINEL) {
            x.right.parent = y;
        }
        x.parent = y.parent;
        if (y.parent == SENTINEL) {
            root = x;
        } else if (y == y.parent.right) {
            y.parent.right = x;
        } else {
            y.parent.left = x;
        }
        x.right = y;
        y.parent = x;
        y.recomputeMaxEnd();
        x.recomputeMaxEnd();
    }
    //#endregion

    //#region utils
    static int intervalCompare(long aStart, long aEnd, long bStart, long bEnd) {
        if (aStart == bStart) {
            return Long.compare(aEnd, bEnd);
        }
        return Long.compare(aStart, bStart);
    }
    //#endregion
}
