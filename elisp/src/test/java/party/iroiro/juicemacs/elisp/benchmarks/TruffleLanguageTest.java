package party.iroiro.juicemacs.elisp.benchmarks;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.HostCompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TruffleLanguageTest {
    @Test
    public void test() {
        FrameDescriptor.Builder builder = FrameDescriptor.newBuilder();
        builder.addSlots(1, FrameSlotKind.Int);
        RootNode rootNode = new RootNode(null, builder.build()) {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            BytecodeInterpreter interpreter = new BytecodeInterpreter();

            @Override
            public Object execute(VirtualFrame frame) {
                return interpreter.execute(frame);
            }
        };
        DirectCallNode callNode = Truffle.getRuntime().createDirectCallNode(rootNode.getCallTarget());
        for (int i = 0; i < Integer.MAX_VALUE; i++) {
            assertEquals(i - 100, callNode.call(i));
        }
    }

    static final class BytecodeInterpreter extends Node {
        public static final byte REF_ARG = 0;
        public static final byte ADD_CONST = 1;
        public static final byte RETURN = 2;

        @CompilerDirectives.CompilationFinal(dimensions = 1)
        final byte[] instructions = { REF_ARG, ADD_CONST, -100, RETURN };

        @HostCompilerDirectives.BytecodeInterpreterSwitch
        @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.MERGE_EXPLODE)
        int execute(VirtualFrame frame) {
            int pc = 0;
            while (true) {
                byte instruction = instructions[pc++];
                switch (instruction) {
                    case REF_ARG:
                        frame.setInt(0, (Integer) frame.getArguments()[0]);
                        break;
                    case ADD_CONST:
                        frame.setInt(0, frame.getInt(0) + instructions[pc++]);
                        break;
                    case RETURN:
                        return frame.getInt(0);
                }
            }
        }
    }
}
