package party.iroiro.juicemacs.elisp.nodes.local;

import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;

public abstract sealed class GetFrameNode extends Node {
    public abstract Frame execute(VirtualFrame frame);

    public static GetFrameNode create(int level) {
        if (level == 0) {
            return new GetThisFrameNode();
        }
        return new GetLevelFrameNode(level);
    }

    public static final class GetThisFrameNode extends GetFrameNode {
        @Override
        public Frame execute(VirtualFrame frame) {
            return frame;
        }
    }

    public static final class GetLevelFrameNode extends GetFrameNode {
        private final int level;
        public GetLevelFrameNode(int level) {
            this.level = level;
        }
        @ExplodeLoop
        @Override
        public Frame execute(VirtualFrame frame) {
            for (int i = 0; i < level; i++) {
                frame = ELispLexical.getFrameSlot(frame);
            }
            return frame;
        }
    }
}
