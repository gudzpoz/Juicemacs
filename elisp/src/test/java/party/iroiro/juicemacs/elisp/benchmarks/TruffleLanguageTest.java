package party.iroiro.juicemacs.elisp.benchmarks;

import com.oracle.truffle.api.*;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;

import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;

public class TruffleLanguageTest {
    /// Tests a basic bytecode compiler
    @Test
    public void testBytecode() {
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
        for (int i = 0; i < 10000; i++) {
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

    @Test
    public void testChainedMaterializedFrames() {
        try (Context context = getTestingContext()) {
            context.eval("elisp", """
                    ;;; -*- lexical-binding: t -*-
                    (defalias 'vector-length #'(lambda (x y)
                      (let ((xx (* x x)))
                        (let ((yy (* y y)))
                          (sqrt (+ xx yy))))))
                    """);
            for (int i = 0; i < 10000; i++) {
                Value value = context.eval("elisp", "(vector-length 400 300)");
                assertEquals(500.0, value.asDouble());
            }
        }
    }

    /// Tests whether materialized frames are inlined
    @Test
    public void testMaterializedFrame() {
        LexicalScopeAddNode ast = new LexicalScopeAddNode(
                new LexicalScopeReadArgNode(),
                new LexicalScopeNode(
                        new LexicalScopeAddNode(
                                new LexicalScopeAddNode(
                                        new LexicalScopeConstantNode(100),
                                        new LexicalScopeUpperNode(1, 1)
                                ),
                                new LexicalScopeAccessNode(1)
                        )
                )
        );

        FrameDescriptor.Builder builder = FrameDescriptor.newBuilder();
        builder.addSlots(1, FrameSlotKind.Object);
        builder.addSlots(1, FrameSlotKind.Int);
        RootNode rootNode = new RootNode(null, builder.build()) {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            LexicalNode body = ast;

            @Override
            public Object execute(VirtualFrame frame) {
                return body.execute(frame);
            }
        };
        DirectCallNode callNode = Truffle.getRuntime().createDirectCallNode(rootNode.getCallTarget());
        for (int i = 0; i < 10000; i++) {
            assertEquals(i * 3 + 200, callNode.call(i));
        }
    }

    public abstract static class LexicalNode extends Node {
        public abstract Object execute(VirtualFrame frame);
    }

    public static final class LexicalScopeNode extends LexicalNode {
        private static final Object[] EMPTY_ARRAY = new Object[0];
        private final FrameDescriptor descriptor;
        @Child
        public LexicalNode lexicalNode;

        public LexicalScopeNode(LexicalNode lexicalNode) {
            this.lexicalNode = lexicalNode;
            FrameDescriptor.Builder descriptor = FrameDescriptor.newBuilder();
            descriptor.addSlots(1, FrameSlotKind.Object);
            descriptor.addSlots(1, FrameSlotKind.Int);
            this.descriptor = descriptor.build();
        }

        @Override
        public Object execute(VirtualFrame frame) {
            VirtualFrame newFrame = Truffle.getRuntime().createVirtualFrame(EMPTY_ARRAY, descriptor);
            newFrame.setObject(0, frame.materialize());
            return lexicalNode.execute(newFrame);
        }
    }

    public static final class LexicalScopeAccessNode extends LexicalNode {
        private final int slot;

        public LexicalScopeAccessNode(int slot) {
            this.slot = slot;
        }

        @Override
        public Object execute(VirtualFrame frame) {
            return frame.getInt(slot);
        }
    }

    public static final class LexicalScopeUpperNode extends LexicalNode {
        private final int slot;
        private final int level;

        public LexicalScopeUpperNode(int slot, int level) {
            this.slot = slot;
            this.level = level;
        }

        @ExplodeLoop
        private VirtualFrame getFrame(VirtualFrame frame) {
            for (int i = 0; i < level; i++) {
                frame = fastCast(frame.getObject(0));
            }
            return frame;
        }

        @SuppressWarnings("unchecked")
        private <T> T fastCast(Object o) {
            return (T) o;
        }

        @Override
        public Object execute(VirtualFrame frame) {
            return getFrame(frame).getInt(slot);
        }
    }

    public static final class LexicalScopeAddNode extends LexicalNode {
        @Child
        public LexicalNode left;
        @Child
        public LexicalNode right;

        public LexicalScopeAddNode(LexicalNode left, LexicalNode right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public Object execute(VirtualFrame frame) {
            int i = (Integer) left.execute(frame) + (Integer) right.execute(frame);
            frame.setInt(1, i);
            return i;
        }
    }

    public static final class LexicalScopeConstantNode extends LexicalNode {
        final Integer value;
        public LexicalScopeConstantNode(int value) {
            this.value = value;
        }
        @Override
        public Object execute(VirtualFrame frame) {
            return value;
        }
    }

    public static final class LexicalScopeReadArgNode extends LexicalNode {
        @Override
        public Object execute(VirtualFrame frame) {
            Object argument = frame.getArguments()[0];
            frame.setInt(1, (Integer) argument);
            return argument;
        }
    }

    @Test
    public void testEnumInlining() {
        EnumTestRootNode rootNode = new EnumTestRootNode();
        RootCallTarget callTarget = rootNode.getCallTarget();
        for (int i = 0; i < 1000000; i++) {
            assertEquals(i, callTarget.call(i));
        }
    }

    static final class EnumTestRootNode extends RootNode {
        EnumTestRootNode() {
            super(null);
        }

        @Override
        public Object execute(VirtualFrame frame) {
            Object input = frame.getArguments()[0];
            return TestEnum.MINUS1.apply(TestEnum.PLUS1.apply(input));
        }
    }
    enum TestEnum implements Function<Object, Object> {
        PLUS1 {
            @Override
            public Object apply(Object object) {
                return 1 + (Integer) object;
            }
        },
        MINUS1 {
            @Override
            public Object apply(Object object) {
                return (Integer) object - 1;
            }
        }
    }
}
