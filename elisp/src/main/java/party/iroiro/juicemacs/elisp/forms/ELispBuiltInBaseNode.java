package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeChild;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;

@NodeChild(value = "arguments", type = ReadFunctionArgNode[].class)
public abstract class ELispBuiltInBaseNode extends ELispExpressionNode {
}
