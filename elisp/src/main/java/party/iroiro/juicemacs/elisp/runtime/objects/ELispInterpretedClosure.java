package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;

import java.util.ArrayList;
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

    private class ELispClosureCallNode extends ELispExpressionNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ReadFunctionArgNode[] args;
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private ELispSymbol[] argSymbols;

        public ELispClosureCallNode() {
            List<ReadFunctionArgNode> argNodes = new ArrayList<>();
            List<ELispSymbol> symbols = new ArrayList<>();
            int state = 0; // 0: required args, 1: optional args, 2: rest args, 3: end
            int argI = 0;
            if (ELispSymbol.isNil(getArgs())) {
                this.args = new ReadFunctionArgNode[0];
                this.argSymbols = new ELispSymbol[0];
                return;
            }
            for (Object arg : (ELispCons) getArgs()) {
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
                        argNodes.add(new ReadFunctionArgNode(argI, state == 0));
                    } else {
                        throw new IllegalArgumentException();
                    }
                    argI++;
                }
            }
            this.args = argNodes.toArray(new ReadFunctionArgNode[0]);
            this.argSymbols = symbols.toArray(new ELispSymbol[0]);
        }

        @Override
        @ExplodeLoop
        public Object executeGeneric(VirtualFrame frame) {
            Object[] prevValues = null;
            if (ELispSymbol.isNil(getEnv())) {
                // Dynamic binding
                prevValues = new Object[argSymbols.length];
                for (int i = 0; i < args.length; i++) {
                    Object v = args[i].executeGeneric(frame);
                    prevValues[i] = argSymbols[i].getValue();
                    // TODO: This should be a thread-local value
                    argSymbols[i].setValue(v);
                }
            } // TODO: Handle lexical binding
            Object body = getBody();
            Object result = false;
            if (!ELispSymbol.isNil(body)) {
                for (Object form : ((ELispCons) body)) {
                    result = BuiltInEval.evalSub(form);
                }
            }
            if (prevValues != null) {
                for (int i = 0; i < args.length; i++) {
                    argSymbols[i].setValue(prevValues[i]);
                }
            }
            return result;
        }
    }
}
