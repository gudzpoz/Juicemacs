package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class ELispInterpretedClosure extends AbstractELispVector implements ELispLexical.ScopeHolder {
    @Nullable
    private final RootNode rootNode;
    @Nullable
    private volatile FunctionRootNode functionRootNode = null;
    @Nullable
    private volatile ELispFunctionObject function = null;
    @Nullable
    private Object name = null;

    public ELispInterpretedClosure(
            Object args, ELispCons body, Object env, Object doc, Object iForm, @Nullable RootNode rootNode
    ) {
        super(new Object[6]);
        set(CLOSURE_ARGLIST, args);
        set(CLOSURE_CODE, body);
        set(CLOSURE_CONSTANTS, env);
        set(CLOSURE_STACK_DEPTH, false);
        set(CLOSURE_DOC_STRING, doc);
        set(CLOSURE_INTERACTIVE, iForm);
        this.rootNode = rootNode;
    }

    @Override
    public Object get(int index) {
        if (index == CLOSURE_CONSTANTS) {
            Object env = inner[CLOSURE_CONSTANTS];
            if (env instanceof ELispLexical lexicalEnv) {
                @Nullable ELispLexical scope = lexicalEnv.parentScope();
                @Nullable MaterializedFrame frame = lexicalEnv.materializedParent();
                Object list = Objects.requireNonNull(scope).toAssocList(Objects.requireNonNull(frame));
                if (isNil(list)) {
                    return new ELispCons(true);
                }
                Object envTrim = INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION.getValue();
                if (isNil(envTrim)) {
                    return list;
                }
                Object closure = BuiltInEval.FFuncall.funcall(
                        null,
                        envTrim,
                        getArgs(),
                        getBody(),
                        list,
                        get(CLOSURE_DOC_STRING),
                        get(CLOSURE_INTERACTIVE)
                );
                return BuiltInData.FAref.aref(closure, CLOSURE_CONSTANTS);
            }
            return env;
        }
        return super.get(index);
    }

    private Object getArgs() {
        return inner[CLOSURE_ARGLIST];
    }

    private ELispCons getBody() {
        return (ELispCons) inner[CLOSURE_CODE];
    }

    private Object getEnv() {
        return inner[CLOSURE_CONSTANTS];
    }

    public ELispFunctionObject getFunction() {
        ELispFunctionObject f = function;
        if (f == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            ELispClosureCallNode node = new ELispClosureCallNode();
            ReadFunctionArgNode.ArgCountVerificationNode wrapper = new ReadFunctionArgNode.ArgCountVerificationNode(
                    node, node.args.requiredArgCount(), node.args.maxArgCount()
            );
            FunctionRootNode root = new FunctionRootNode(
                    ELispLanguage.get(node),
                    this.name == null ? this : this.name,
                    wrapper,
                    ELispLexical.frameDescriptor(!isNil(getEnv()))
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

    @Override
    public void display(ELispPrint print) {
        if (getEnv() instanceof ELispLexical) {
            vectorPrintHelper(print, "#[", "]", subList(0, CLOSURE_CONSTANTS).iterator());
        } else {
            displayHelper(print, "#[", "]");
        }
    }

    @Override
    public ELispLexical getScope() {
        return (ELispLexical) getEnv();
    }

    @Override
    public void setScope(ELispLexical scope) {
        set(CLOSURE_CONSTANTS, scope);
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

        private final ELispLexical.@Nullable Allocator rootLexical;

        public ELispClosureCallNode() {
            Object env = getEnv();
            isLexical = !isNil(env);
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
                ELispLexical.Allocator lexical = new ELispLexical.Allocator();
                ELispLexical rootScope = initializeLexical(env, argSymbols, lexical);
                setScope(rootScope);
                rootLexical = lexical;
                this.argSymbols = new ELispSymbol[0];
                for (int i = 0; i < this.readArgNodes.length; i++) {
                    ELispExpressionNode read = readArgNodes[i];
                    int slot = rootScope.slots().get(i);
                    readArgNodes[i] = ELispFrameSlotNode.createWrite(slot, null, read);
                }
            } else {
                rootLexical = null;
                this.argSymbols = argSymbols;
            }
        }

        @CompilerDirectives.TruffleBoundary
        private static ELispLexical initializeLexical(Object env, ELispSymbol[] argSymbols, ELispLexical.Allocator lexical) {
            CompilerDirectives.transferToInterpreter();
            ELispLexical rootScope;
            rootScope = switch (env) {
                case ELispLexical scope -> scope;
                case ELispCons cons -> {
                    MaterializedFrame parentFrame = Truffle.getRuntime().createMaterializedFrame(
                            new Object[0],
                            ELispLexical.frameDescriptor(true)
                    );
                    ELispLexical parentScope = ELispLexical.newRoot();
                    ELispCons.BrentTortoiseHareIterator i = cons.listIterator(0);
                    ELispLexical.Allocator counter = new ELispLexical.Allocator();
                    while (i.hasNext()) {
                        ELispSymbol symbol = (ELispSymbol) i.next();
                        int slot = parentScope.addVariable(counter, symbol);
                        ELispLexical.setVariable(parentFrame, slot, i.next());
                    }
                    yield ELispLexical.newRoot(parentScope, parentFrame);
                }
                default -> ELispLexical.newRoot();
            };
            for (ELispSymbol symbol : argSymbols) {
                rootScope.addVariable(lexical, symbol);
            }
            return rootScope;
        }

        @ExplodeLoop
        public ELispLexical.@Nullable Dynamic pushScope(VirtualFrame frame) {
            if (isLexical) {
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
                return ELispLexical.pushDynamic(argSymbols, newValues);
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
            try (ELispLexical.Dynamic _ = pushScope(frame)) {
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
            if (rootNode == null) {
                return null;
            }
            return ELispInterpretedNode.ELispConsExpressionNode.getConsSourceSection(rootNode, getBody());
        }

        public ELispInterpretedClosure getClosure() {
            return ELispInterpretedClosure.this;
        }

        @Override
        public @Nullable ELispLexical lexicalScope() {
            return isLexical ? getScope() : null;
        }

        @Override
        public ELispLexical.@Nullable Allocator rootScope() {
            return rootLexical;
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
