package party.iroiro.juicemacs.elisp.nodes.ast;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.FunctionObjectCallNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionObjectNodes;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;

import java.util.Objects;

public abstract class ConsFunctionCallNode extends ConsCallNode {
    @Children
    protected ELispExpressionNode[] args;

    ConsFunctionCallNode(ELispCons cons) {
        super(cons);
        this.args = initChildren(cons, 1);
        this.args[0] = ReadFunctionObjectNodes.createFormCardinal(cons.car());
    }

    @Specialization
    @ExplodeLoop
    public Object call(VirtualFrame frame, @Cached(inline = true) FunctionObjectCallNode dispatchNode) {
        int length = Objects.requireNonNull(this.args).length;
        Object[] args = new Object[length];
        Object function = this.args[0].executeGeneric(frame);
        if (function instanceof ELispCons) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            return replace(new LazyConsExpressionNode(cons)).executeGeneric(frame);
        }
        args[0] = function;
        for (int i = 1; i < length; i++) {
            args[i] = this.args[i].executeGeneric(frame);
        }
        return dispatchNode.executeCall(this, cons.car(), args);
    }
}
