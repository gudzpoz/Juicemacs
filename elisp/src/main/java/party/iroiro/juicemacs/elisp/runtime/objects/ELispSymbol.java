package party.iroiro.juicemacs.elisp.runtime.objects;

import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispSymbolNode;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

public final class ELispSymbol implements ELispValue {

    private final String name;

    private Object value;

    private ELispValue function;

    public ELispSymbol(String name) {
        this.name = name;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(ELispValue value) {
        this.value = value;
    }

    public ELispValue getFunction() {
        return function;
    }

    public void setFunction(ELispValue function) {
        this.function = function;
    }

    @Override
    public String type() {
        return "symbol";
    }

    @Override
    public ELispExpressionNode eval(ELispContext context) {
        return new ELispSymbolNode(this);
    }

    public String name() {
        return name;
    }

}
