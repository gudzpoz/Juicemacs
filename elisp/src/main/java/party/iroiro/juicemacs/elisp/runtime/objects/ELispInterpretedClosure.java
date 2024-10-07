package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.runtime.ELispBindingScope;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.AND_OPTIONAL;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.AND_REST;

public class ELispInterpretedClosure extends AbstractELispVector {
    @Nullable
    private final RootNode rootNode;
    @Nullable
    private volatile ELispFunctionObject function = null;

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
            // TODO: Get a real name for the function.
            FunctionRootNode root = new FunctionRootNode(ELispLanguage.get(node), toString(), node);
            f = new ELispFunctionObject(root.getCallTarget());
            function = f;
        }
        return f;
    }

    @Override
    public String toString() {
        return toStringHelper("(lambda ", ")");
    }

    private class ELispClosureCallNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ReadFunctionArgNode[] args;
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private ELispSymbol[] argSymbols;

        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode body;

        private final ELispBindingScope.ClosableScope.@Nullable Lexical lexical;

        public ELispClosureCallNode() {
            lexical = initializeLexical();
            body = BuiltInEval.FProgn.progn(getBody().toArray());

            if (ELispSymbol.isNil(getArgs())) {
                this.args = new ReadFunctionArgNode[0];
                this.argSymbols = new ELispSymbol[0];
                return;
            }
            List<ReadFunctionArgNode> argNodes = new ArrayList<>();
            List<ELispSymbol> symbols = new ArrayList<>();
            int state = 0; // 0: required args, 1: optional args, 2: rest args, 3: end
            int argI = 0;
            for (Iterator<Object> iterator = ((ELispCons) getArgs()).iterator(); iterator.hasNext(); ) {
                Object arg = iterator.next();
                ELispSymbol symbol = (ELispSymbol) arg;
                if (symbol == AND_OPTIONAL) {
                    if (state >= 2) {
                        throw new IllegalArgumentException();
                    }
                    state = 1;
                } else if (symbol == AND_REST) {
                    if (state >= 2) {
                        throw new IllegalArgumentException();
                    }
                    state = 2;
                } else {
                    symbols.add(symbol);
                    if (state == 2) {
                        argNodes.add(new ReadFunctionArgNode.ReadFunctionRestArgsAsConsNode(argI));
                        state = 3;
                    } else if (state <= 1) {
                        argNodes.add(new ReadFunctionArgNode(argI, state == 0, !iterator.hasNext()));
                    } else {
                        throw new IllegalArgumentException();
                    }
                    argI++;
                }
            }
            this.args = argNodes.toArray(new ReadFunctionArgNode[0]);
            this.argSymbols = symbols.toArray(new ELispSymbol[0]);
            adoptChildren();
        }

        @CompilerDirectives.TruffleBoundary
        private ELispBindingScope.ClosableScope.@Nullable Lexical initializeLexical() {
            Object env = getEnv();
            if (ELispSymbol.isNil(env)) {
                return null;
            }
            if (env instanceof ELispBindingScope.ClosableScope.Lexical l) {
                return l;
            }
            if (ELispSymbol.isT(env)) {
                return ELispBindingScope.EMPTY_LEXICAL;
            }
            ELispCons cons = (ELispCons) env;
            HashMap<ELispSymbol, ELispSymbol.Value.Forwarded> map = new HashMap<>();
            ELispCons.BrentTortoiseHareIterator i = cons.listIterator(0);
            while (i.hasNext()) {
                ELispSymbol symbol = (ELispSymbol) i.next();
                map.put(symbol, new ELispSymbol.Value.Forwarded(i.next()));
            }
            return new ELispBindingScope.ClosableScope.Lexical(map, null);
        }

        @CompilerDirectives.TruffleBoundary
        public ELispBindingScope.ClosableScope pushScope(Object[] newValues) {
            if (lexical == null) {
                return ELispBindingScope.pushDynamic(argSymbols, newValues);
            } else {
                ELispBindingScope.ClosableScope outer = ELispBindingScope.switchLexical(lexical);
                HashMap<ELispSymbol, ELispSymbol.Value.Forwarded> map = new HashMap<>();
                for (int i = 0; i < argSymbols.length; i++) {
                    // Always lexically bound, even for "special == true" symbols
                    // HashMaps are too complex, causing Truffle to bailout, hence @TruffleBoundary.
                    map.put(argSymbols[i], new ELispSymbol.Value.Forwarded(newValues[i]));
                }
                return ELispBindingScope.pushComposite(
                        ELispBindingScope.pushLexical(map),
                        outer
                );
            }
        }

        @Override
        @ExplodeLoop
        public Object executeGeneric(VirtualFrame frame) {
            Object[] newValues;
            newValues = new Object[argSymbols.length];
            int length = args.length;
            for (int i = 0; i < length; i++) {
                newValues[i] = args[i].executeGeneric(frame);
            }
            try (ELispBindingScope.ClosableScope _ = pushScope(newValues)) {
                return body.executeGeneric(frame);
            }
        }

        @Nullable
        @Override
        public SourceSection getSourceSection() {
            RootNode root = rootNode;
            return root == null ? null : getBody().getSourceSection(root.getSourceSection().getSource());
        }
    }
}
