package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

public class ELispCallFormNode extends ELispExpressionNode {

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private ELispSymbolNode targetFunction;

    @Children
    private final ELispExpressionNode[] callArguments;

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private FunctionDispatchNode dispatchNode;

    public ELispCallFormNode(ELispCons lisp, ELispContext context) {
        this.targetFunction = (ELispSymbolNode) ((ELispSymbol) lisp.car()).eval(context);
        if (lisp.cdr() instanceof ELispCons) {
            this.callArguments = ((ELispCons) lisp.cdr()).stream()
                    .map(context::valueToExpression)
                    .toArray(ELispExpressionNode[]::new);
        } else {
            this.callArguments = new ELispExpressionNode[0];
        }
        this.dispatchNode = FunctionDispatchNodeGen.create();
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        ELispSymbol function = this.targetFunction.executeGeneric(frame);
        Object[] argumentValues = new Object[this.callArguments.length];
        for (int i = 0; i < this.callArguments.length; i++) {
            argumentValues[i] = this.callArguments[i].executeGeneric(frame);
        }
        return this.dispatchNode.executeDispatch(
                new ELispFunctionObject(function.getFunction().getCallTarget()),
                argumentValues
        );
    }
}
