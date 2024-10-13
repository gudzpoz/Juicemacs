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

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

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
        super(List.of(args, body, env, doc, iForm));
        this.rootNode = rootNode;
    }

    private Object getArgs() {
        return get(0);
    }

    private ELispCons getBody() {
        return (ELispCons) get(1);
    }

    private Object getEnv() {
        return get(2);
    }

    public ELispFunctionObject getFunction() {
        ELispFunctionObject f = function;
        if (f == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            ELispClosureCallNode node = new ELispClosureCallNode();
            ReadFunctionArgNode.ArgCountVerificationNode wrapper = new ReadFunctionArgNode.ArgCountVerificationNode(
                    node, node.requiredArgs, node.maxArgs
            );
            String name = this.name == null ? this.toString() : this.name.toString();
            FunctionRootNode root = new FunctionRootNode(
                    ELispLanguage.get(node), name, wrapper,
                    ELispLexical.frameDescriptor(!ELispSymbol.isNil(getEnv()))
            );
            f = new ELispFunctionObject(root.getCallTarget());
            functionRootNode = root;
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
            f.setName(name.toString());
        }
    }

    @Override
    public String toString() {
        return toStringHelper("(lambda ", ")");
    }

    public record LexicalEnvironment(MaterializedFrame frame, ELispLexical lexicalFrame) {
    }

    public final class ELispClosureCallNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ReadFunctionArgNode[] optionalRestArgs;

        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private ELispSymbol[] requiredArgSymbols;
        private final int requiredArgs;
        private final int maxArgs;
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private ELispSymbol[] optionalArgSymbols;

        private final boolean isLexical;

        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode body;

        @Nullable
        private final LexicalEnvironment lexical;

        public ELispClosureCallNode() {
            Object env = getEnv();
            isLexical = !ELispSymbol.isNil(env);
            lexical = isLexical ? initializeLexical(env) : null;
            body = BuiltInEval.FProgn.progn(getBody().toArray());

            this.requiredArgSymbols = new ELispSymbol[0];
            if (ELispSymbol.isNil(getArgs())) {
                requiredArgs = 0;
                maxArgs = 0;
                this.optionalRestArgs = new ReadFunctionArgNode[0];
                this.optionalArgSymbols = new ELispSymbol[0];
                return;
            }
            List<ReadFunctionArgNode> argNodes = new ArrayList<>();
            List<ELispSymbol> symbols = new ArrayList<>();
            int state = 0; // 0: required args, 1: optional args, 2: rest args, 3: end
            int argI = 0;
            int required = 0;
            for (Object arg : (ELispCons) getArgs()) {
                ELispSymbol symbol = (ELispSymbol) arg;
                if (symbol == AND_OPTIONAL) {
                    if (state >= 2) {
                        throw ELispSignals.error("&optional found after &rest in anonymous lambda");
                    }
                    if (isLexical) {
                        this.requiredArgSymbols = symbols.toArray(new ELispSymbol[0]);
                        symbols.clear();
                    }
                    state = 1;
                } else if (symbol == AND_REST) {
                    if (state >= 2) {
                        throw ELispSignals.error("Nothing after &rest in anonymous lambda");
                    }
                    if (isLexical) {
                        this.requiredArgSymbols = symbols.toArray(new ELispSymbol[0]);
                        symbols.clear();
                    }
                    state = 2;
                } else {
                    symbols.add(symbol);
                    if (state == 2) {
                        argNodes.add(new ReadFunctionArgNode.ReadFunctionRestArgsAsConsNode(argI));
                        argI = Integer.MIN_VALUE;
                        state = 3;
                    } else if (state == 1) {
                        argNodes.add(new ReadFunctionArgNode(argI));
                    } else if (state == 0) {
                        required++;
                        if (!isLexical) {
                            argNodes.add(new ReadFunctionArgNode(argI));
                        }
                    } else {
                        throw ELispSignals.error("Multiple vars after &rest in anonymous lambda");
                    }
                    argI++;
                }
            }
            requiredArgs = required;
            maxArgs = argI;
            if (state == 0) {
                if (isLexical) {
                    this.requiredArgSymbols = symbols.toArray(new ELispSymbol[0]);
                    this.optionalArgSymbols = new ELispSymbol[0];
                    this.optionalRestArgs = new ReadFunctionArgNode[0];
                    adoptChildren();
                    return;
                }
            }
            this.optionalArgSymbols = symbols.toArray(new ELispSymbol[0]);
            this.optionalRestArgs = argNodes.toArray(new ReadFunctionArgNode[0]);
            adoptChildren();
        }

        @Nullable
        @CompilerDirectives.TruffleBoundary
        private static LexicalEnvironment initializeLexical(Object env) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            if (env instanceof LexicalEnvironment l) {
                return l;
            }
            if (env instanceof ELispCons cons) {
                MaterializedFrame frame = Truffle.getRuntime().createMaterializedFrame(
                        new Object[0],
                        ELispLexical.frameDescriptor(true)
                );
                ELispLexical lexical =
                        new ELispLexical(frame, null, null, List.of());
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
                ELispLexical lexicalFrame =
                        new ELispLexical(frame, lexical.frame, lexical.lexicalFrame, List.of(requiredArgSymbols));
                for (int i = 0; i < optionalArgSymbols.length; i++) {
                    // Always lexically bound, even for "special == true" symbols
                    // HashMaps are too complex, causing Truffle to bailout, hence @TruffleBoundary.
                    lexicalFrame.addVariable(frame, optionalArgSymbols[i], newValues[i]);
                }
                return null;
            } else {
                return ELispLexical.pushDynamic(optionalArgSymbols, newValues);
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
            Object[] newValues;
            int length = optionalArgSymbols.length;
            newValues = new Object[length];
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
            return root == null ? null : getBody().getSourceSection(root.getSourceSection().getSource());
        }

        public ELispInterpretedClosure getClosure() {
            return ELispInterpretedClosure.this;
        }
    }
}
