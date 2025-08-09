package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.strings.InternalByteArray;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.nodes.bytecode.ELispBytecodeFallbackNode;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.array.LocationProvider;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupportFactory;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asLong;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispBytecode extends AbstractELispClosure implements LocationProvider {
    private long encodedLocation = 0;

    ELispBytecode(Object[] inner, ClosureCommons commons) {
        super(inner, commons);
    }

    public byte[] getBytecode() {
        ELispString s = (ELispString) get(CLOSURE_CODE);
        StringSupport.GetInternalBytesNode uncached = StringSupportFactory.GetInternalBytesNodeGen.getUncached();
        InternalByteArray array = uncached.execute(uncached, s);
        if (array.getOffset() != 0) {
            throw new UnsupportedOperationException();
        }
        return array.getArray();
    }

    public Object[] getConstants() {
        return ((ELispVector) get(CLOSURE_CONSTANTS)).inner;
    }

    public long getStackDepth() {
        return (Long) get(CLOSURE_STACK_DEPTH);
    }

    @Override
    public long getEncodedLocation() {
        return encodedLocation;
    }

    @Override
    public void setEncodedLocation(long encodedLocation) {
        this.encodedLocation = encodedLocation;
    }

    @Override
    public FunctionRootNode getFunctionRootNode() {
        BytecodeCallNode node = new BytecodeCallNode();
        ReadFunctionArgNode.ArgCountVerificationNode wrapper = new ReadFunctionArgNode.ArgCountVerificationNode(
                node, node.requiredArgCount, node.maxArgCount
        );
        return new FunctionRootNode(
                ELispLanguage.get(node),
                this.name == null ? this : this.name,
                wrapper,
                node.descriptor()
        );
    }

    public final class BytecodeCallNode extends ELispExpressionNode {
        final ELispInterpretedClosure.@Nullable ClosureArgs dynamicArgs;
        final ELispSymbol @Nullable[] argSymbols;
        final int requiredArgCount;
        final int optionalArgCount;
        final int maxArgCount;

        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ReadFunctionArgNode[] optionalRestArgs;
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispBytecodeFallbackNode body;

        BytecodeCallNode() {
            Object args = getArgs();
            dynamicArgs = isNil(args) || args instanceof ELispCons ? ELispInterpretedClosure.ClosureArgs.parse(args) : null;
            if (dynamicArgs == null) {
                long encodedArgs = asLong(args);
                requiredArgCount = (int) (encodedArgs & 0x7F);
                optionalArgCount = (int) (encodedArgs >> 8) - requiredArgCount;
                maxArgCount = (encodedArgs & 0x80) == 0 ? requiredArgCount + optionalArgCount : -1;
                argSymbols = null;
            } else {
                requiredArgCount = dynamicArgs.requiredArgCount();
                optionalArgCount = dynamicArgs.optionalArgs().length;
                maxArgCount = dynamicArgs.maxArgCount();
                argSymbols = dynamicArgs.argSymbols();
            }

            List<ReadFunctionArgNode> argNodes = new ArrayList<>();
            for (int i = 0; i < requiredArgCount; i++) {
                argNodes.add(new ReadFunctionArgNode(i));
            }
            for (int i = 0; i < optionalArgCount; i++) {
                argNodes.add(new ReadFunctionArgNode(i + requiredArgCount));
            }
            if (maxArgCount == -1) {
                argNodes.add(new ReadFunctionArgNode.ReadFunctionRestArgsAsConsNode(
                        optionalArgCount + requiredArgCount
                ));
            }
            this.optionalRestArgs = argNodes.toArray(new ReadFunctionArgNode[0]);
            body = new ELispBytecodeFallbackNode(
                    ELispBytecode.this,
                    dynamicArgs == null ? argNodes.size() : 0
            );
        }

        FrameDescriptor descriptor() {
            int args;
            if (dynamicArgs != null) {
                args = 0;
            } else {
                args = optionalRestArgs.length;
            }
            FrameDescriptor.Builder descriptor = FrameDescriptor.newBuilder();
            descriptor.addSlots((int) getStackDepth() + args, FrameSlotKind.Object);
            return descriptor.build();
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            executeGeneric(frame);
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            @Nullable Dynamic dynamic;
            if (argSymbols == null) {
                for (int i = 0; i < optionalRestArgs.length; i++) {
                    frame.setObject(i, optionalRestArgs[i].executeGeneric(frame));
                }
                dynamic = null;
            } else {
                Object[] values = new Object[optionalRestArgs.length];
                for (int i = 0; i < optionalRestArgs.length; i++) {
                    values[i] = optionalRestArgs[i].executeGeneric(frame);
                }
                dynamic = Dynamic.pushDynamic(argSymbols, values);
            }
            @Nullable Dynamic scope = dynamic;
            try {
                return body.executeGeneric(frame);
            } finally {
                if (scope != null) {
                    scope.close();
                }
            }
        }

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return tag == StandardTags.RootTag.class;
        }

        @Nullable
        @Override
        public SourceSection getSourceSection() {
            @Nullable Source rootSource = commons.source;
            if (rootSource == null) {
                return null;
            }
            return ELispBytecode.this.getSourceSection(rootSource); // NOPMD (not recursion)
        }
    }
}
