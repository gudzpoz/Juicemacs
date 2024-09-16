package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.runtime.ELispBindingScope;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.AND_OPTIONAL;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.AND_REST;

public class ELispInterpretedClosure extends AbstractELispVector {
    @Nullable
    private ELispFunctionObject function = null;

    public ELispInterpretedClosure(
            Object args, ELispCons body, Object env, Object doc, Object iForm) {
        super(List.of(args, body, env, doc, iForm));
    }

    private Object getArgs() {
        return get(0);
    }

    private Object getBody() {
        return get(1);
    }

    private Object getEnv() {
        return get(2);
    }

    public ELispFunctionObject getFunction() {
        if (function == null) {
            ELispClosureCallNode node = new ELispClosureCallNode();
            FunctionRootNode root = new FunctionRootNode(ELispLanguage.get(node), node);
            function = new ELispFunctionObject(root.getCallTarget());
        }
        return function;
    }

    @Override
    public CallTarget getCallTarget() {
        return getFunction().callTarget();
    }

    private class ELispClosureCallNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ReadFunctionArgNode[] args;
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal(dimensions = 1)
        private ELispSymbol[] argSymbols;

        private final ELispBindingScope.ClosableScope.@Nullable Lexical lexical;

        public ELispClosureCallNode() {
            lexical = initializeLexical();

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

        }

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

        @ExplodeLoop
        public ELispBindingScope.ClosableScope pushScope(VirtualFrame frame) {
            Object[] newValues;
            newValues = new Object[argSymbols.length];
            for (int i = 0; i < args.length; i++) {
                newValues[i] = args[i].executeGeneric(frame);
            }
            if (lexical == null) {
                return ELispBindingScope.pushDynamic(argSymbols, newValues);
            } else {
                ELispBindingScope.ClosableScope outer = ELispBindingScope.switchLexical(lexical);
                HashMap<ELispSymbol, ELispSymbol.Value.Forwarded> map = new HashMap<>();
                for (int i = 0; i < argSymbols.length; i++) {
                    // TODO: Handle "special == true" symbols
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
            Object body = getBody();
            Object result = false;
            try (ELispBindingScope.ClosableScope _ = pushScope(frame)) {
                if (!ELispSymbol.isNil(body)) {
                    for (Object form : ((ELispCons) body)) {
                        result = BuiltInEval.evalSub(form);
                    }
                }
            }
            return result;
        }
    }
}
