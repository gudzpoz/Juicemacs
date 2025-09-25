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

package party.iroiro.juicemacs.piecetree.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.piecetree.meta.MarkPieceTreeBase.Piece;

/**
 * Transliterated from <a href="https://github.com/microsoft/vscode/blob/main/src/vs/editor/common/model/pieceTreeTextBuffer/rbTreeBase.ts">
 * rbTreeBase.ts @ vscode</a>
 */
@SuppressWarnings({"DuplicatedCode", "unchecked", "rawtypes"})
public final class MarkTreeNode<T> {
    public final static MarkTreeNode SENTINEL = new MarkTreeNode<>(null, MarkTreeNode.BLACK);
    public final static boolean RED = true;
    public final static boolean BLACK = false;

    MarkTreeNode<T> left, right, parent;
    boolean color;

    Piece<T> piece;

    /**
     * size of the left subtree (not in order)
     */
    long size_left;

    MarkTreeNode(@Nullable Piece<T> piece, boolean color) {
        //noinspection DataFlowIssue: should always be guarded by SENTINEL check
        this.piece = piece;
        this.color = color;
        this.size_left = 0;
        this.parent = this;
        this.left = this;
        this.right = this;
    }

    public MarkTreeNode<T> next() {
        if (right != SENTINEL) {
            return right.leftest();
        }
        MarkTreeNode<T> node = this;
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

    public MarkTreeNode<T> prev() {
        if (left != SENTINEL) {
            return left.rightest();
        }
        MarkTreeNode<T> node = this;
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

    @SuppressWarnings("DataFlowIssue")
    public void detach() {
        parent = null;
        left = null;
        right = null;
    }

    public MarkTreeNode<T> leftest() {
        MarkTreeNode<T> node = this;
        while (node.left != SENTINEL) {
            node = node.left;
        }
        return node;
    }

    public MarkTreeNode<T> rightest() {
        MarkTreeNode<T> node = this;
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

    public static void resetSentinel() {
        SENTINEL.parent = SENTINEL;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static <T> void leftRotate(MarkPieceTreeBase<T> tree, MarkTreeNode<T> x) {
        MarkTreeNode<T> y = x.right;

        y.size_left += x.size_left + (x == SENTINEL ? 0 : x.piece.length());
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
    public static <T> void rightRotate(MarkPieceTreeBase<T> tree, MarkTreeNode<T> y) {
        MarkTreeNode<T> x = y.left;
        y.left = x.right;
        if (x.right != SENTINEL) {
            x.right.parent = y;
        }
        x.parent = y.parent;

        y.size_left -= x.size_left + (x == SENTINEL ? 0 : x.piece.length());

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
    public static <T> void rbDelete(MarkPieceTreeBase<T> tree, MarkTreeNode<T> z) {
        MarkTreeNode<T> x, y;
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
            x.color = MarkTreeNode.BLACK;
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
            recomputeTreeMetadata(tree, y);
        }
        z.detach();

        if (x.parent.left == x) {
            long newSizeLeft = x.calculateSize();
            if (newSizeLeft != x.parent.size_left) {
                long delta = newSizeLeft - x.parent.size_left;
                x.parent.size_left = newSizeLeft;
                updateTreeMetadata(tree, x.parent, delta);
            }
        }
        recomputeTreeMetadata(tree, x.parent);

        if (yWasRed) {
            resetSentinel();
            return;
        }

        // delete fixup
        MarkTreeNode<T> w;
        while (x != tree.root && x.color == MarkTreeNode.BLACK) {
            if (x == x.parent.left) {
                w = x.parent.right;
                if (w.color == MarkTreeNode.RED) {
                    w.color = MarkTreeNode.BLACK;
                    x.parent.color = MarkTreeNode.RED;
                    leftRotate(tree, x.parent);
                    w = x.parent.right;
                }

                if (w.left.color == MarkTreeNode.BLACK && w.right.color == MarkTreeNode.BLACK) {
                    w.color = MarkTreeNode.RED;
                    x = x.parent;
                } else {
                    if (w.right.color == MarkTreeNode.BLACK) {
                        w.left.color = MarkTreeNode.BLACK;
                        w.color = MarkTreeNode.RED;
                        rightRotate(tree, w);
                        w = x.parent.right;
                    }
                    w.color = x.parent.color;
                    x.parent.color = MarkTreeNode.BLACK;
                    w.right.color = MarkTreeNode.BLACK;
                    leftRotate(tree, x.parent);
                    x = tree.root;
                }
            } else {
                w = x.parent.left;
                if (w.color == MarkTreeNode.RED) {
                    w.color = MarkTreeNode.BLACK;
                    x.parent.color = MarkTreeNode.RED;
                    rightRotate(tree, x.parent);
                    w = x.parent.left;
                }

                if (w.right.color == MarkTreeNode.BLACK && w.left.color == MarkTreeNode.BLACK) {
                    w.color = MarkTreeNode.RED;
                    x = x.parent;
                } else {
                    if (w.left.color == MarkTreeNode.BLACK) {
                        w.right.color = MarkTreeNode.BLACK;
                        w.color = MarkTreeNode.RED;
                        leftRotate(tree, w);
                        w = x.parent.left;
                    }
                    w.color = x.parent.color;
                    x.parent.color = MarkTreeNode.BLACK;
                    w.left.color = MarkTreeNode.BLACK;
                    rightRotate(tree, x.parent);
                    x = tree.root;
                }
            }
        }
        x.color = MarkTreeNode.BLACK;
        resetSentinel();
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static <T> void fixInsert(MarkPieceTreeBase<T> tree, MarkTreeNode<T> x) {
        recomputeTreeMetadata(tree, x);

        while (x != tree.root && x.parent.color == MarkTreeNode.RED) {
            if (x.parent == x.parent.parent.left) {
                MarkTreeNode<T> y = x.parent.parent.right;
                if (y.color == MarkTreeNode.RED) {
                    x.parent.color = MarkTreeNode.BLACK;
                    y.color = MarkTreeNode.BLACK;
                    x.parent.parent.color = MarkTreeNode.RED;
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.right) {
                        x = x.parent;
                        leftRotate(tree, x);
                    }
                    x.parent.color = MarkTreeNode.BLACK;
                    x.parent.parent.color = MarkTreeNode.RED;
                    rightRotate(tree, x.parent.parent);
                }
            } else {
                MarkTreeNode<T> y = x.parent.parent.left;
                if (y.color == MarkTreeNode.RED) {
                    x.parent.color = MarkTreeNode.BLACK;
                    y.color = MarkTreeNode.BLACK;
                    x.parent.parent.color = MarkTreeNode.RED;
                    x = x.parent.parent;
                } else {
                    if (x == x.parent.left) {
                        x = x.parent;
                        rightRotate(tree, x);
                    }
                    x.parent.color = MarkTreeNode.BLACK;
                    x.parent.parent.color = MarkTreeNode.RED;
                    leftRotate(tree, x.parent.parent);
                }
            }
        }
        tree.root.color = MarkTreeNode.BLACK;
    }

    public static <T> void updateTreeMetadata(MarkPieceTreeBase<T> tree, MarkTreeNode<T> x, long deltaSize) {
        while (x != tree.root && x != SENTINEL) {
            if (x == x.parent.left) {
                x.parent.size_left += deltaSize;
            }
            x = x.parent;
        }
    }

    public static <T> void recomputeTreeMetadata(MarkPieceTreeBase<T> tree, MarkTreeNode<T> x) {
        long delta;
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
        x.size_left += delta;

        while (x != tree.root && delta != 0) {
            if (x == x.parent.left) {
                x.parent.size_left += delta;
            }
            x = x.parent;
        }
    }
}
