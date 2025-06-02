package party.iroiro.juicemacs.elisp.nodes.local;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LEXICAL_BINDING;

/// Dynamic-binding scopes, usually used with `try-with` blocks
public interface Dynamic extends AutoCloseable {
    static Dynamic withLexicalBinding(boolean value) {
        return pushDynamic(LEXICAL_BINDING, value);
    }

    static Dynamic pushDynamic(ELispSymbol symbol, Object newValue) {
        return new DynamicSingle(symbol, symbol.swapThreadLocalValue(newValue));
    }

    static Dynamic pushDynamic(ELispSymbol[] symbols, Object[] newValues) {
        for (int i = 0; i < symbols.length; i++) {
            newValues[i] = symbols[i].swapThreadLocalValue(newValues[i]);
        }
        return new DynamicBatch(symbols, newValues);
    }

    static Dynamic preparePopDynamic(ELispSymbol[] symbols, Object[] oldValues) {
        return new DynamicBatch(symbols, oldValues);
    }

    @Override
    void close();

    record DynamicBatch(ELispSymbol[] symbols, Object[] prevValues) implements Dynamic {
        @Override
        public void close() {
            for (int i = 0; i < symbols.length; i++) {
                symbols[i].swapThreadLocalValue(prevValues[i]);
            }
        }
    }

    record DynamicSingle(ELispSymbol symbol, Object prevValue) implements Dynamic {
        @Override
        public void close() {
            symbol.swapThreadLocalValue(prevValue);
        }
    }
}
