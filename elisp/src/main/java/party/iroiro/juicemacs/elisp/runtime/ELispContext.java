package party.iroiro.juicemacs.elisp.runtime;

import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInMath;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIns;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispFixNumLiteralNode;
import party.iroiro.juicemacs.elisp.nodes.ELispFloatLiteralNode;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispValue;

import java.util.HashMap;

public class ELispContext {

    // TODO: Replace this with obarray
    private final HashMap<String, ELispSymbol> internMap = new HashMap<>();

    public ELispSymbol intern(String symbol) {
        return internMap.computeIfAbsent(symbol, ELispSymbol::new);
    }

    public String applyShorthands(String symbol) {
        // TODO: Implementation
        return symbol;
    }

    public final static ELispSymbol T = new ELispSymbol("t");
    public final static ELispSymbol NIL = new ELispSymbol("nil");
    public final static ELispSymbol QUOTE = new ELispSymbol("quote");
    public final static ELispSymbol BACKQUOTE = new ELispSymbol("`");
    public final static ELispSymbol UNQUOTE = new ELispSymbol(",");
    public final static ELispSymbol UNQUOTE_SPLICING = new ELispSymbol(",@");

    public ELispExpressionNode valueToExpression(Object value) {
        return switch (value) {
            case Long l -> new ELispFixNumLiteralNode(l);
            case Double d -> new ELispFloatLiteralNode(d);
            case ELispValue v -> v.eval(this);
            default -> throw new IllegalArgumentException();
        };
    }

    public void registerFunction(String name, ELispValue function) {
        ELispSymbol symbol = intern(name);
        symbol.setFunction(function);
    }

    public void initGlobal(ELispLanguage language) {
        initBuiltIns(language, new BuiltInMath());
    }

    private void initBuiltIns(ELispLanguage language, ELispBuiltIns builtIns) {
        builtIns.initialize(language, this);
    }
}
