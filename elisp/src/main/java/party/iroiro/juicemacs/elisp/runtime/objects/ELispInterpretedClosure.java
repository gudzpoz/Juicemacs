package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.AND_OPTIONAL;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.AND_REST;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class ELispInterpretedClosure extends AbstractELispVector {
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
            if (env instanceof LexicalEnvironment lexicalEnv) {
                Object list = lexicalEnv.toAssocList();
                if (isNil(list)) {
                    return new ELispCons(true);
                }
                return list;
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
        if (getEnv() instanceof LexicalEnvironment) {
            vectorPrintHelper(print, "#[", "]", subList(0, CLOSURE_CONSTANTS).iterator());
        } else {
            displayHelper(print, "#[", "]");
        }
    }

    public record LexicalEnvironment(MaterializedFrame frame, ELispLexical lexicalFrame) {
        @Override
        public String toString() {
            return "#lexical-" + Integer.toHexString(System.identityHashCode(lexicalFrame));
        }

        public Object toAssocList() {
            return lexicalFrame.toAssocList(frame);
        }
    }

    public final class ELispClosureCallNode extends ELispExpressionNode {

        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ReadFunctionArgNode[] optionalRestArgs;

        private final ClosureArgs args;
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private final ELispSymbol[] lexicallyBoundSymbols;
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private final ELispSymbol[] variableLikeBoundSymbols;

        private final boolean isLexical;

        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode body;

        @Nullable
        private final LexicalEnvironment lexical;

        private final ELispLexical.StableTopAssumption assumption;
        private final ELispLexical.StackSizeProfile stackSizeProfile;

        public ELispClosureCallNode() {
            Object env = getEnv();
            isLexical = !isNil(env);
            lexical = isLexical ? initializeLexical(env) : null;
            assumption = new ELispLexical.StableTopAssumption();
            stackSizeProfile = new ELispLexical.StackSizeProfile();
            body = BuiltInEval.FProgn.progn(getBody().toArray());

            this.args = ClosureArgs.parse(getArgs());
            this.lexicallyBoundSymbols = isLexical ? args.requiredArgs : new ELispSymbol[0];
            this.variableLikeBoundSymbols = args.variableLikeSymbols(isLexical);

            List<ReadFunctionArgNode> argNodes = new ArrayList<>();
            if (!isLexical) {
                for (int i = 0; i < args.requiredArgs.length; i++) {
                    argNodes.add(new ReadFunctionArgNode(i));
                }
            }
            for (int i = 0; i < args.optionalArgs.length; i++) {
                argNodes.add(new ReadFunctionArgNode(i + args.requiredArgs.length));
            }
            if (args.rest != null) {
                argNodes.add(new ReadFunctionArgNode.ReadFunctionRestArgsAsConsNode(
                        args.optionalArgs.length + args.requiredArgs.length
                ));
            }
            this.optionalRestArgs = argNodes.toArray(new ReadFunctionArgNode[0]);
        }

        @Nullable
        @CompilerDirectives.TruffleBoundary
        private static LexicalEnvironment initializeLexical(Object env) {
            CompilerDirectives.transferToInterpreter();
            if (env instanceof LexicalEnvironment l) {
                return l;
            }
            if (env instanceof ELispCons cons) {
                MaterializedFrame frame = Truffle.getRuntime().createMaterializedFrame(
                        new Object[0],
                        ELispLexical.frameDescriptor(true)
                );
                ELispLexical lexical = ELispLexical.create(
                        frame,
                        new ELispLexical.StableTopAssumption(),
                        new ELispLexical.StackSizeProfile()
                );
                ELispCons.BrentTortoiseHareIterator i = cons.listIterator(0);
                while (i.hasNext()) {
                    ELispSymbol symbol = (ELispSymbol) i.next();
                    lexical.addVariable(frame, symbol, i.next());
                }
                return new LexicalEnvironment(frame, lexical);
            }
            return null;
        }

        public ELispLexical.@Nullable Dynamic pushScope(VirtualFrame frame, Object[] newValues) {
            if (isLexical && lexical != null) {
                ELispLexical lexicalFrame = ELispLexical.create(
                        frame,
                        lexical.lexicalFrame,
                        lexical.frame,
                        List.of(lexicallyBoundSymbols),
                        stackSizeProfile,
                        assumption
                );
                for (int i = 0; i < variableLikeBoundSymbols.length; i++) {
                    // Always lexically bound, even for "special == true" symbols
                    // HashMaps are too complex, causing Truffle to bailout, hence @TruffleBoundary.
                    lexicalFrame.addVariable(frame, variableLikeBoundSymbols[i], newValues[i]);
                }
                return null;
            } else {
                return ELispLexical.pushDynamic(variableLikeBoundSymbols, newValues);
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

        @ExplodeLoop
        public Object execute(VirtualFrame frame, boolean isVoid) {
            int length = variableLikeBoundSymbols.length;
            Object[] newValues = new Object[length];
            for (int i = 0; i < length; i++) {
                newValues[i] = optionalRestArgs[i].executeGeneric(frame);
            }
            try (ELispLexical.Dynamic _ = pushScope(frame, newValues)) {
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
            RootNode root = rootNode;
            if (root == null) {
                return null;
            }
            SourceSection sourceSection = root.getSourceSection();
            if (sourceSection == null) {
                return null;
            }
            return getBody().getSourceSection(sourceSection.getSource());
        }

        public ELispInterpretedClosure getClosure() {
            return ELispInterpretedClosure.this;
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

        public ELispSymbol[] variableLikeSymbols(boolean isLexical) {
            int start;
            ELispSymbol[] symbols;
            if (isLexical) {
                symbols = new ELispSymbol[optionalArgs.length + (rest == null ? 0 : 1)];
                start = 0;
            } else {
                symbols = new ELispSymbol[requiredArgs.length + optionalArgs.length + (rest == null ? 0 : 1)];
                System.arraycopy(requiredArgs, 0, symbols, 0, requiredArgs.length);
                start = requiredArgs.length;
            }
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
