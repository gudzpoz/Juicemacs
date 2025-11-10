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

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import party.iroiro.juicemacs.piecetree.PieceTreeBase.Piece;

/**
 * Transliterated from <a href="https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/pieceTreeTextBuffer/rbTreeBase.ts">
 * rbTreeBase.ts @ vscode</a>
 */
final class TreeNode {
    @SuppressWarnings({"DataFlowIssue", "NullAway"})
    public final static TreeNode SENTINEL = new TreeNode(null, TreeNode.BLACK);
    public final static boolean RED = true;
    public final static boolean BLACK = false;

    TreeNode left, right, parent;
    boolean color;

    Piece piece;

    /**
     * size of the left subtree (not in order)
     */
    long size_left;
    /**
     * line feeds cnt in the left subtree (not in order)
     */
    int lf_left;

    public TreeNode(@SuppressWarnings("NullableProblems") Piece piece, boolean color) {
        this.piece = piece;
        this.color = color;
        this.size_left = 0;
        this.lf_left = 0;
        this.parent = this;
        this.left = this;
        this.right = this;
    }

    public TreeNode next() {
        if (right != SENTINEL) {
            return right.leftest();
        }
        TreeNode node = this;
        while (node.parent != SENTINEL) {
            if (node.parent.left == node) {
                break;
            }
            node = node.parent;
        }
        if (node.parent == SENTINEL) {
            return SENTINEL;
        }
        return node.parent;
    }

    public TreeNode prev() {
        if (left != SENTINEL) {
            return left.rightest();
        }
        TreeNode node = this;
        while (node.parent != SENTINEL) {
            if (node.parent.right == node) {
                break;
            }
            node = node.parent;
        }
        if (node.parent == SENTINEL) {
            return SENTINEL;
        }
        return node.parent;
    }

    @SuppressWarnings({"DataFlowIssue", "NullAway"}) // sentinel
    public void detach() {
        parent = null;
        left = null;
        right = null;
    }

    public boolean isDetached() {
        //noinspection ConstantValue
        return parent == null;
    }

    public TreeNode leftest() {
        TreeNode node = this;
        while (node.left != SENTINEL) {
            node = node.left;
        }
        return node;
    }

    public TreeNode rightest() {
        TreeNode node = this;
        while (node.right != SENTINEL) {
            node = node.right;
        }
        return node;
    }

    @TruffleBoundary
    private long calculateSize() {
        if (this == SENTINEL) {
            return 0;
        }
        return size_left + piece.length() + right.calculateSize();
    }

    @TruffleBoundary
    private int calculateLf() {
        if (this == SENTINEL) {
            return 0;
        }
        return lf_left + piece.lineFeedCnt() + right.calculateLf();
    }

    public static void resetSentinel() {
        SENTINEL.parent = SENTINEL;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static void leftRotate(PieceTreeBase tree, TreeNode x) {
        TreeNode y = x.right;

        y.size_left += x.size_left + (x == SENTINEL ? 0 : x.piece.length());
        y.lf_left += x.lf_left + (x == SENTINEL ? 0 : x.piece.lineFeedCnt());
        x.right = y.left;

        if (y.left != SENTINEL) {
            y.left.parent = x;
        }
        y.parent = x.parent;

        if (x.parent == SENTINEL) {
            tree.root = y;
        } else if (x.parent.left == x) {
            x.parent.left = y;
        } else {
            x.parent.right = y;
        }
        y.left = x;
        x.parent = y;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static void rightRotate(PieceTreeBase tree, TreeNode y) {
        TreeNode x = y.left;
        y.left = x.right;
        if (x.right != SENTINEL) {
            x.right.parent = y;
        }
        x.parent = y.parent;

        y.size_left -= x.size_left + (x == SENTINEL ? 0 : x.piece.length());
        y.lf_left -= x.lf_left + (x == SENTINEL ? 0 : x.piece.lineFeedCnt());

        if (y.parent == SENTINEL) {
            tree.root = x;
        } else if (y.parent.right == y) {
            y.parent.right = x;
        } else {
            y.parent.left = x;
        }
        x.right = y;
        y.parent = x;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static void rbDelete(PieceTreeBase tree, TreeNode z) {
        TreeNode x, y;
        if (z.left == SENTINEL) {
            y = z;
            x = y.right;
        } else if (z.right == SENTINEL) {
            y = z;
            x = y.left;
        } else {
            y = z.right.leftest();
            x = y.right;
        }

        if (y == tree.root) {
            tree.root = x;
            x.color = TreeNode.BLACK;
            z.detach();
            resetSentinel();
            tree.root.parent = SENTINEL;
            return;
        }

        final boolean yWasRed = y.color;

        if (y == y.parent.left) {
            y.parent.left = x;
        } else {
            y.parent.right = x;
        }

        if (y == z) {
            x.parent = y.parent;
            recomputeTreeMetadata(tree, x);
        } else {
            if (y.parent == z) {
                x.parent = y;
            } else {
                x.parent = y.parent;
            }
            recomputeTreeMetadata(tree, x);
            y.left = z.left;
            y.right = z.right;
            y.parent = z.parent;
            y.color = z.color;
            if (z == tree.root) {
                tree.root = y;
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
            y.size_left = z.size_left;
            y.lf_left = z.lf_left;
            recomputeTreeMetadata(tree, y);
        }
        z.detach();

        if (x.parent.left == x) {
            long newSizeLeft = x.calculateSize();
            int newLfLeft = x.calculateLf();
            if (newSizeLeft != x.parent.size_left || newLfLeft != x.parent.lf_left) {
                long delta = newSizeLeft - x.parent.size_left;
                int lfDelta = newLfLeft - x.parent.lf_left;
                x.parent.size_left = newSizeLeft;
                x.parent.lf_left = newLfLeft;
                updateTreeMetadata(tree, x.parent, delta, lfDelta);
            }
        }
        recomputeTreeMetadata(tree, x.parent);

        if (yWasRed) {
            resetSentinel();
            return;
        }

        // delete fixup
        TreeNode w;
        while (x != tree.root && x.color == TreeNode.BLACK) {
            if (x == x.parent.left) {
                w = x.parent.right;
                if (w.color == TreeNode.RED) {
                    w.color = TreeNode.BLACK;
                    x.parent.color = TreeNode.RED;
                    leftRotate(tree, x.parent);
                    w = x.parent.right;
                }

                if (w.left.color == TreeNode.BLACK && w.right.color == TreeNode.BLACK) {
                    w.color = TreeNode.RED;
                    x = x.parent;
                } else {
                    if (w.right.color == TreeNode.BLACK) {
                        w.left.color = TreeNode.BLACK;
                        w.color = TreeNode.RED;
                        rightRotate(tree, w);
                        w = x.parent.right;
                    }
                    w.color = x.parent.color;
                    x.parent.color = TreeNode.BLACK;
                    w.right.color = TreeNode.BLACK;
                    leftRotate(tree, x.parent);
                    x = tree.root;
                }
            } else {
                w = x.parent.left;
                if (w.color == TreeNode.RED) {
                    w.color = TreeNode.BLACK;
                    x.parent.color = TreeNode.RED;
                    rightRotate(tree, x.parent);
                    w = x.parent.left;
                }

                if (w.right.color == TreeNode.BLACK && w.left.color == TreeNode.BLACK) {
                    w.color = TreeNode.RED;
                    x = x.parent;
                } else {
                    if (w.left.color == TreeNode.BLACK) {
                        w.right.color = TreeNode.BLACK;
                        w.color = TreeNode.RED;
                        leftRotate(tree, w);
                        w = x.parent.left;
                    }
                    w.color = x.parent.color;
                    x.parent.color = TreeNode.BLACK;
                    w.left.color = TreeNode.BLACK;
                    rightRotate(tree, x.parent);
                    x = tree.root;
                }
            }
        }
        x.color = TreeNode.BLACK;
        resetSentinel();
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static void fixInsert(PieceTreeBase tree, TreeNode x) {
        recomputeTreeMetadata(tree, x);

        while (x != tree.root && x.parent.color == TreeNode.RED) {
            if (x.parent == x.parent.parent.left) {
                TreeNode y = x.parent.parent.right;
                if (y.color == TreeNode.RED) {
                    x.parent.color = TreeNode.BLACK;
                    y.color = TreeNode.BLACK;
                    x.parent.parent.color = TreeNode.RED;
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.right) {
                        x = x.parent;
                        leftRotate(tree, x);
                    }
                    x.parent.color = TreeNode.BLACK;
                    x.parent.parent.color = TreeNode.RED;
                    rightRotate(tree, x.parent.parent);
                }
            } else {
                TreeNode y = x.parent.parent.left;
                if (y.color == TreeNode.RED) {
                    x.parent.color = TreeNode.BLACK;
                    y.color = TreeNode.BLACK;
                    x.parent.parent.color = TreeNode.RED;
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.left) {
                        x = x.parent;
                        rightRotate(tree, x);
                    }
                    x.parent.color = TreeNode.BLACK;
                    x.parent.parent.color = TreeNode.RED;
                    leftRotate(tree, x.parent.parent);
                }
            }
        }
        tree.root.color = TreeNode.BLACK;
    }

    public static void updateTreeMetadata(PieceTreeBase tree, TreeNode x, long deltaSize, int deltaLf) {
        while (x != tree.root && x != SENTINEL) {
            if (x == x.parent.left) {
                x.parent.size_left += deltaSize;
                x.parent.lf_left += deltaLf;
            }
            x = x.parent;
        }
    }

    public static void recomputeTreeMetadata(PieceTreeBase tree, TreeNode x) {
        long delta;
        int deltaLf;
        if (x == tree.root) {
            return;
        }

        while (x != tree.root && x == x.parent.right) {
            x = x.parent;
        }

        if (x == tree.root) {
            return;
        }

        x = x.parent;

        delta = x.left.calculateSize() - x.size_left;
        deltaLf = x.left.calculateLf() - x.lf_left;
        x.size_left += delta;
        x.lf_left += deltaLf;

        if (delta != 0 || deltaLf != 0) {
            while (x != tree.root) {
                if (x == x.parent.left) {
                    x.parent.size_left += delta;
                    x.parent.lf_left += deltaLf;
                }
                x = x.parent;
            }
        }
    }
}
