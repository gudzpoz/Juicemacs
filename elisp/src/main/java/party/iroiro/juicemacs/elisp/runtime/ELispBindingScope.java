package party.iroiro.juicemacs.elisp.runtime;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.Map;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.LEXICAL_BINDING;

/**
 * Basically our implementation of GNU Emacs' {@code internal-interpreter-environment}
 *
 * <ul>
 * <li>Dynamic bindings are simply utility functions that swap values in and out.</li>
 * <li>Lexical bindings are more complex, whose implementation entails a few functions
 * in {@link party.iroiro.juicemacs.elisp.forms.BuiltInEval} ({@code src/eval.c}).</li>
 * </ul>
 *
 * <h2>Lexical Scoping</h2>
 * <p>
 * Whether a variable is lexical or dynamic is determined by several things in GNU Emacs:
 * </p>
 * <ul>
 * <li>If the {@code special} field of a symbol is set, {@code let/let*} statements should
 * always bind it dynamically.</li>
 * <li>If a symbol is somehow marked "special" in {@code internal-interpreter-environment},
 * then it should be bound dynamically by {@code let/let*}.</li>
 * <li>If a symbol is found "normal" in {@code internal-interpreter-environment}, it is
 * lexically bound and looked up in {@code internal-interpreter-environment}. Otherwise,
 * it is still dynamically (i.e. globally) bound.</li>
 * </ul>
 */
public final class ELispBindingScope {
    private final static ELispSymbol.Value.Forwarded _DYNAMIC = new ELispSymbol.Value.Forwarded();
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

    public static void markDynamic(ELispSymbol symbol) {
        ClosableScope.Lexical lexical = getCurrentLexical();
        if (lexical != null) {
            lexical.values.put(symbol, _DYNAMIC);
        }
    }

    public static boolean isDynamic(ELispSymbol symbol) {
        return symbol.isSpecial() || getForwardedLexical(symbol) == _DYNAMIC;
    }

    @Nullable
    public static Object getLexical(ELispSymbol symbol) {
        ELispSymbol.Value.Forwarded forwarded = getForwardedLexical(symbol);
        return (forwarded != null && forwarded != _DYNAMIC) ? forwarded.getValue() : null;
    }

    public static boolean setLexical(ELispSymbol symbol, Object value) {
        ELispSymbol.Value.Forwarded forwarded = getForwardedLexical(symbol);
        if (forwarded != null && forwarded != _DYNAMIC) {
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
