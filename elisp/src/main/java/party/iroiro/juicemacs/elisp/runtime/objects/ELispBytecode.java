package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.nodes.bytecode.ELispBytecodeFallbackNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.mule.MuleByteArrayString;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInData.isMultibyte;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asLong;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispBytecode extends AbstractELispVector {
    @Nullable
    private volatile FunctionRootNode functionRootNode = null;
    @Nullable
    private volatile ELispFunctionObject function = null;
    @Nullable
    private Object name = null;

    private ELispBytecode(Object[] inner) {
        super(inner);
    }

    public static ELispBytecode create(List<?> inner) {
        Object argList = inner.get(CLOSURE_ARGLIST);
        Object byteCode = inner.get(CLOSURE_CODE);
        Object constants = inner.get(CLOSURE_CONSTANTS);
        Object stack = inner.get(CLOSURE_STACK_DEPTH);
        if (!(
                (argList instanceof Long || argList instanceof ELispCons || isNil(argList))
                        && (byteCode instanceof ELispString s && !isMultibyte(s.value()))
                        && (constants instanceof ELispVector)
                        && (stack instanceof Long)
        )) {
            throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
        }
        if (inner.size() >= 5) {
            Object doc = inner.get(4);
            if (!(isNil(doc) || doc instanceof ELispString || doc instanceof ELispCons)) {
                // Not string or autoload string
                throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
            }
        }
        if (inner.size() >= 7) {
            throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
        }
        return new ELispBytecode(inner.toArray());
    }

    public Object getArgs() {
        return get(CLOSURE_ARGLIST);
    }

    public byte[] getBytecode() {
        return ((MuleByteArrayString) ((ELispString) get(CLOSURE_CODE)).value()).bytes();
    }

    public Object[] getConstants() {
        return ((ELispVector) get(CLOSURE_CONSTANTS)).inner;
    }

    public long getStackDepth() {
        return (Long) get(CLOSURE_STACK_DEPTH);
    }

    @Override
    public void display(ELispPrint print) {
        displayHelper(print, "#[", "]");
    }

    public ELispFunctionObject getFunction() {
        ELispFunctionObject f = function;
        if (f == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            BytecodeCallNode node = new BytecodeCallNode();
            ReadFunctionArgNode.ArgCountVerificationNode wrapper = new ReadFunctionArgNode.ArgCountVerificationNode(
                    node, node.requiredArgCount, node.maxArgCount
            );
            FunctionRootNode root = new FunctionRootNode(
                    ELispLanguage.get(node),
                    this.name == null ? this : this.name,
                    wrapper,
                    node.descriptor()
            );
            f = new ELispFunctionObject(root.getCallTarget());
            functionRootNode = root; // NOPMD
            function = f;
        }
        return f;
    }

    public Object getName() {
        return name == null ? this : name;
    }

    public void setName(Object name) {
        this.name = name;
        FunctionRootNode f = functionRootNode;
        if (f != null) {
            f.setLispFunction(name);
        }
    }

    private final class BytecodeCallNode extends ELispExpressionNode {
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
                argSymbols = dynamicArgs.variableLikeSymbols(false);
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
            body = new ELispBytecodeFallbackNode(ELispBytecode.this, dynamicArgs == null ? argNodes.size() : 0);
        }

        FrameDescriptor descriptor() {
            int args;
            if (dynamicArgs != null) {
                args = 0;
            } else {
                args = optionalRestArgs.length;
            }
            FrameDescriptor.Builder descriptor = FrameDescriptor.newBuilder();
            descriptor.addSlots((int) getStackDepth() + args + 1, FrameSlotKind.Object);
            return descriptor.build();
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            executeGeneric(frame);
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            ELispLexical.@Nullable Dynamic dynamic;
            if (argSymbols == null) {
                for (int i = 0; i < optionalRestArgs.length; i++) {
                    frame.setObject(i + 1, optionalRestArgs[i].executeGeneric(frame));
                }
                dynamic = null;
            } else {
                Object[] values = new Object[optionalRestArgs.length];
                for (int i = 0; i < optionalRestArgs.length; i++) {
                    values[i] = optionalRestArgs[i].executeGeneric(frame);
                }
                dynamic = ELispLexical.pushDynamic(argSymbols, values);
            }
            try (ELispLexical.Dynamic _ = dynamic) {
                return body.executeGeneric(frame);
            }
        }
    }
}
