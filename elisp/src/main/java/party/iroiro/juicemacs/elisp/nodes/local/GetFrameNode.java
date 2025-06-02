package party.iroiro.juicemacs.elisp.nodes.local;

import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;

public abstract sealed class GetFrameNode extends Node {
    public abstract Frame execute(VirtualFrame frame);

    public static GetFrameNode create(int level, @Nullable MaterializedFrame parentFrame) {
        if (parentFrame == null) {
            if (level == 0) {
                return new GetThisFrameNode();
            }
            return new GetLevelFrameNode(level);
        }
        if (level != 0) {
            throw ELispSignals.fatal("internal error");
        }
        return new GetParentFrameNode(parentFrame);
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
        public Frame execute(VirtualFrame frame) {
            for (int i = 0; i < level; i++) {
                frame = ELispLexical.getFrameSlot(frame);
            }
            return frame;
        }
    }

    public static final class GetParentFrameNode extends GetFrameNode {
        private final MaterializedFrame parentFrame;
        public GetParentFrameNode(MaterializedFrame parentFrame) {
            this.parentFrame = parentFrame;
        }
        public Frame execute(VirtualFrame frame) {
            return parentFrame;
        }
    }
}
