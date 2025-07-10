package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispFrameSlotWriteNode;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;

import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public final class ELispInterpretedClosure extends AbstractELispClosure {
    @Nullable
    private transient MaterializedFrame upperFrame;
    private transient ELispLexical.@Nullable Scope upperScope;

    ELispInterpretedClosure(Object[] array, @Nullable Source source) {
        super(array, source);
        updateEnv(array[CLOSURE_CONSTANTS]);
    }

    @Override
    public Object set(int index, Object element) {
        Object set = super.set(index, element);
        if (index == CLOSURE_CONSTANTS) {
            updateEnv(element);
        }
        return set;
    }

    private void updateEnv(Object element) {
        switch (element) {
            case ELispLexical.Captured(ELispLexical.Scope scope, MaterializedFrame frame) -> {
                this.upperFrame = frame;
                this.upperScope = scope;
            }
            case ELispCons cons -> {
                CompilerDirectives.transferToInterpreter();
                this.upperFrame = Truffle.getRuntime().createMaterializedFrame(
                        new Object[0],
                        ELispLexical.rootFrameDescriptor(cons.size(), true)
                );
                this.upperScope = ELispLexical.newBlockFromAlist(upperFrame, cons);
            }
            default -> {
                upperFrame = null;
                upperScope = null;
            }
        };
    }

    @Override
    public Object get(int index) {
        if (index != CLOSURE_CONSTANTS) {
            return super.get(index);
        }
        Object envObject = inner[CLOSURE_CONSTANTS];
        if (!(envObject instanceof ELispLexical.Captured(ELispLexical.Scope scope, MaterializedFrame frame))) {
            return envObject;
        }
        Object list = scope.toAssocList(frame);
        if (isNil(list)) {
            list = new ELispCons(true);
        }
        inner[CLOSURE_CONSTANTS] = list;
        return list;
    }

    private ELispCons getBody() {
        return (ELispCons) inner[CLOSURE_CODE];
    }

    @Nullable
    private MaterializedFrame getUpperFrame() {
        return upperFrame;
    }

    @Override
    protected FunctionRootNode getFunctionRootNode() {
        ELispClosureCallNode node = new ELispClosureCallNode();
        ReadFunctionArgNode.ArgCountVerificationNode wrapper = new ReadFunctionArgNode.ArgCountVerificationNode(
                node, node.args.requiredArgCount(), node.args.maxArgCount()
        );
        int argCount = node.args.requiredArgs.length + node.args.optionalArgs.length
                + (node.args.rest == null ? 0 : 1);
        return new FunctionRootNode(
                ELispLanguage.get(node),
                this.name == null ? this : this.name,
                wrapper,
                ELispLexical.rootFrameDescriptor(argCount, getUpperFrame() != null)
        );
    }

    public final class ELispClosureCallNode extends ELispExpressionNode implements ELispLexical.ScopeProvider {

        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ELispExpressionNode[] readArgNodes;

        private final ClosureArgs args;
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private final ELispSymbol[] argSymbols;

        private final boolean isLexical;

        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode body;

        private final ELispLexical.@Nullable Scope argScope;

        public ELispClosureCallNode() {
            ELispLexical.@Nullable Scope env = ELispInterpretedClosure.this.upperScope;
            isLexical = env != null;
            args = ClosureArgs.parse(getArgs());
            body = BuiltInEval.FProgn.progn(getBody().toArray());

            List<ELispExpressionNode> argNodes = new ArrayList<>();
            for (int i = 0; i < args.requiredArgs.length; i++) {
                argNodes.add(new ReadFunctionArgNode(i));
            }
            for (int i = 0; i < args.optionalArgs.length; i++) {
                argNodes.add(new ReadFunctionArgNode(i + args.requiredArgs.length));
            }
            if (args.rest != null) {
                argNodes.add(new ReadFunctionArgNode.ReadFunctionRestArgsAsConsNode(
                        args.optionalArgs.length + args.requiredArgs.length
                ));
            }
            this.readArgNodes = argNodes.toArray(new ELispExpressionNode[0]);
            ELispSymbol[] argSymbols = args.argSymbols();
            if (isLexical) {
                argScope = ELispLexical.newBlock(env, argSymbols).newScope(argSymbols.length);
                this.argSymbols = new ELispSymbol[0];
                int[] slots = argScope.block().slots();
                for (int i = 0; i < this.readArgNodes.length; i++) {
                    ELispExpressionNode read = readArgNodes[i];
                    int slot = slots[i];
                    readArgNodes[i] = ELispFrameSlotWriteNode.createWrite(0, slot, read);
                }
            } else {
                argScope = null;
                this.argSymbols = argSymbols;
            }
        }

        @ExplodeLoop
        public @Nullable Dynamic pushScope(VirtualFrame frame) {
            if (isLexical) {
                ELispInterpretedClosure closure = ELispLexical.getCallee(frame);
                frame.setObject(ELispLexical.UPPER_FRAME_SLOT, closure.getUpperFrame());
                for (ELispExpressionNode arg : readArgNodes) {
                    arg.executeVoid(frame);
                }
                return null;
            } else {
                int length = readArgNodes.length;
                Object[] newValues = new Object[length];
                for (int i = 0; i < length; i++) {
                    newValues[i] = readArgNodes[i].executeGeneric(frame);
                }
                return Dynamic.pushDynamic(argSymbols, newValues);
            }
        }

        @Override
        public void executeVoid(VirtualFrame frame) {
            execute(frame, true);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return execute(frame, false);
        }

        public Object execute(VirtualFrame frame, boolean isVoid) {
            try (Dynamic _ = pushScope(frame)) {
                if (isVoid) {
                    body.executeVoid(frame);
                    return false;
                }
                return body.executeGeneric(frame);
            }
        }

        @Nullable
        @Override
        public SourceSection getSourceSection() {
            if (rootSource == null) {
                return null;
            }
            return getBody().getSourceSection(rootSource);
        }

        public ELispInterpretedClosure getClosure() {
            return ELispInterpretedClosure.this;
        }

        @Override
        public ELispLexical.@Nullable Scope getScope() {
            return isLexical ? argScope : null;
        }

        @Override
        public boolean hasTag(Class<? extends Tag> tag) {
            return tag == StandardTags.RootTag.class;
        }
    }

    public record ClosureArgs(ELispSymbol[] requiredArgs, ELispSymbol[] optionalArgs, @Nullable ELispSymbol rest) {
        public static ClosureArgs parse(Object args) {
            if (isNil(args)) {
                return new ClosureArgs(new ELispSymbol[0], new ELispSymbol[0], null);
            }
            if (args instanceof Long bytecodeArgs) {
                return parseBytecodeArgs(bytecodeArgs);
            }
            ArrayList<ELispSymbol> requiredArgs = new ArrayList<>();
            ArrayList<ELispSymbol> optionalArgs = new ArrayList<>();
            @Nullable ELispSymbol rest = null;
            int state = 0; // 0: required args, 1: optional args, 2: rest args, 3: end
            for (Object arg : asCons(args)) {
                ELispSymbol symbol = asSym(arg);
                if (symbol == AND_OPTIONAL) {
                    if (state >= 2) {
                        throw ELispSignals.error("&optional found after &rest in anonymous lambda");
                    }
                    state = 1;
                } else if (symbol == AND_REST) {
                    if (state >= 2) {
                        throw ELispSignals.error("Nothing after &rest in anonymous lambda");
                    }
                    state = 2;
                } else {
                    switch (state) {
                        case 0 -> requiredArgs.add(symbol);
                        case 1 -> optionalArgs.add(symbol);
                        case 2 -> {
                            rest = symbol;
                            state = 3;
                        }
                        default -> throw ELispSignals.error("Multiple vars after &rest in anonymous lambda");
                    }
                }
            }
            return new ClosureArgs(requiredArgs.toArray(new ELispSymbol[0]), optionalArgs.toArray(new ELispSymbol[0]), rest);
        }

        public static ClosureArgs parseBytecodeArgs(long encodedArgs) {
            int requiredArgCount = (int) (encodedArgs & 0x7F);
            int optionalArgCount = (int) (encodedArgs >> 8) - requiredArgCount;
            int maxArgCount = (encodedArgs & 0x80) == 0 ? requiredArgCount + optionalArgCount : -1;
            return new ClosureArgs(
                    new ELispSymbol[requiredArgCount],
                    new ELispSymbol[optionalArgCount],
                    maxArgCount == -1 ? AND_REST : null
            );
        }

        public ELispSymbol[] argSymbols() {
            int start;
            ELispSymbol[] symbols;
            symbols = new ELispSymbol[requiredArgs.length + optionalArgs.length + (rest == null ? 0 : 1)];
            System.arraycopy(requiredArgs, 0, symbols, 0, requiredArgs.length);
            start = requiredArgs.length;
            System.arraycopy(optionalArgs, 0, symbols, start, optionalArgs.length);
            if (rest != null) {
                symbols[symbols.length - 1] = rest;
            }
            return symbols;
        }

        public int requiredArgCount() {
            return requiredArgs.length;
        }

        public int maxArgCount() {
            if (rest != null) {
                return -1;
            }
            return requiredArgs.length + optionalArgs.length;
        }
    }
}
