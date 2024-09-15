package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeFactory;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;

import java.util.ArrayList;
import java.util.List;

public abstract class ELispBuiltIns {

    protected abstract List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories();

    public void initialize(ELispLanguage language, ELispContext context) {
        List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> factories = getNodeFactories();
        for (var factory : factories) {
            for (var builtIn : factory.getNodeClass().getAnnotationsByType(ELispBuiltIn.class)) {
                List<ReadFunctionArgNode> args = new ArrayList<>(builtIn.maxArgs() + (builtIn.varArgs() ? 1 : 0));
                for (int i = 0; i < builtIn.minArgs(); i++) {
                    args.add(new ReadFunctionArgNode(i, true));
                }
                for (int i = builtIn.minArgs(); i < builtIn.maxArgs(); i++) {
                    args.add(new ReadFunctionArgNode(i, false));
                }
                if (builtIn.varArgs()) {
                    args.add(new ReadFunctionArgNode.ReadFunctionRestArgsNode(builtIn.maxArgs()));
                }
                ELispBuiltInBaseNode function = factory.createNode((Object) args.toArray(ReadFunctionArgNode[]::new));
                FunctionRootNode rootNode = new FunctionRootNode(language, function);
                context.registerFunction(
                        builtIn.name(),
                        new ELispSubroutine(rootNode.getCallTarget(), builtIn.rawArg())
                );
            }
        }
    }

}
