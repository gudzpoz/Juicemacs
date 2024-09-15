package party.iroiro.juicemacs.elisp.runtime;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.Map;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.LEXICAL_BINDING;

public final class ELispBindingScope {
    public final static ClosableScope.Lexical EMPTY_LEXICAL = new ClosableScope.Lexical(Map.of(), null);
    private final static ThreadLocal<ClosableScope.@Nullable Lexical> currentScope = new ThreadLocal<>();

    public sealed interface ClosableScope extends AutoCloseable {
        void close();
        
        record Dynamic(ELispSymbol[] symbols, Object[] prevValues) implements ClosableScope {
            @Override
            public void close() {
                for (int i = 0; i < symbols.length; i++) {
                    symbols[i].swapThreadLocalValue(prevValues[i]);
                }
            }
        }

        record Lexical(
                Map<ELispSymbol, ELispSymbol.Value.Forwarded> values,
                @Nullable Lexical parent
        ) implements ClosableScope {
            @Override
            public void close() {
                currentScope.set(parent);
            }
        }
        
        record LexicalSwitch(@Nullable Lexical prev) implements ClosableScope {
            @Override
            public void close() {
                currentScope.set(prev);
            }
        }

        record Composite(ClosableScope[] scopes) implements ClosableScope {
            @Override
            public void close() {
                for (ClosableScope scope : scopes) {
                    scope.close();
                }
            }
        }
    }

    private static ELispSymbol.Value.@Nullable Forwarded getForwardedLexical(ELispSymbol symbol) {
        ClosableScope.Lexical lexical = currentScope.get();
        while (lexical != null) {
            ELispSymbol.Value.Forwarded value = lexical.values.get(symbol);
            if (value != null) {
                return value;
            }
            lexical = lexical.parent;
        }
        return null;
    }
    
    @Nullable
    public static Object getLexical(ELispSymbol symbol) {
        ELispSymbol.Value.Forwarded forwarded = getForwardedLexical(symbol);
        return forwarded != null ? forwarded.getValue() : null;
    }
    
    public static boolean setLexical(ELispSymbol symbol, Object value) {
        ELispSymbol.Value.Forwarded forwarded = getForwardedLexical(symbol);
        if (forwarded != null) {
            forwarded.setValue(value);
            return true;
        }
        return false;
    }

    public static ClosableScope withLexicalBinding(boolean value) {
        return pushDynamic(new ELispSymbol[]{LEXICAL_BINDING}, new Object[]{value});
    }

    public static ClosableScope.@Nullable Lexical getCurrentLexical() {
        return currentScope.get();
    }

    public static ClosableScope switchLexical(ClosableScope.Lexical lexical) {
        ClosableScope.Lexical prev = currentScope.get();
        currentScope.set(lexical);
        return new ClosableScope.LexicalSwitch(prev);
    }
    
    public static ClosableScope pushLexical(Map<ELispSymbol, ELispSymbol.Value.Forwarded> values) {
        ClosableScope.Lexical lexical = new ClosableScope.Lexical(values, currentScope.get());
        currentScope.set(lexical);
        return lexical;
    }

    public static ClosableScope pushDynamic(ELispSymbol[] symbols, Object[] newValues) {
        for (int i = 0; i < symbols.length; i++) {
            newValues[i] = symbols[i].swapThreadLocalValue(newValues[i]);
        }
        return new ClosableScope.Dynamic(symbols, newValues);
    }

    public static ClosableScope pushComposite(ClosableScope... scopes) {
        return new ClosableScope.Composite(scopes);
    }
}
