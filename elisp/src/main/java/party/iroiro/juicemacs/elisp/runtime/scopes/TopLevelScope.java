package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;
import java.util.Optional;

@ExportLibrary(InteropLibrary.class)
public final class TopLevelScope implements TruffleObject {
    private final ELispLanguage language;
    private final ELispContext context;
    private final HashMap<String, ELispSymbol> symbolCache;
    private final HashMap<String, ValueStorage> cache;

    public TopLevelScope(ELispLanguage language, ELispContext context) {
        this.language = language;
        this.context = context;
        this.symbolCache = new HashMap<>();
        this.cache = new HashMap<>();
    }

    @ExportMessage
    boolean isScope() {
        return true;
    }

    @ExportMessage
    @TruffleBoundary
    String toDisplayString(boolean allowSideEffects) {
        return toString();
    }

    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    Class<ELispLanguage> getLanguage() {
        return ELispLanguage.class;
    }

    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    @ExportMessage
    @TruffleBoundary
    public Object getMembers(boolean includeInternal) {
        ArrayList<ELispString> members = new ArrayList<>();
        language.globalVariablesMap.keySet().forEach((sym) -> members.add(new ELispString(sym.name())));
        language.globalFunctionsMap.keySet().forEach((sym) -> members.add(new ELispString(sym.name())));
        return new ELispVector(members.toArray());
    }

    @ExportMessage
    @TruffleBoundary
    boolean isMemberReadable(String member) {
        ValueStorage storage = cache.get(member);
        if (storage == null) {
            ELispSymbol symbol = context.obarray().internSoft(member);
            if (symbol == null) {
                return false;
            }
            symbolCache.put(member, symbol);
            Optional<ValueStorage> valueStorage = context.getStorageLazy(symbol);
            if (valueStorage.isPresent()) {
                storage = valueStorage.get();
                cache.put(member, storage);
            } else {
                return false;
            }
        }
        return storage.isBound();
    }

    @ExportMessage
    @TruffleBoundary
    Object readMember(String member) throws UnknownIdentifierException {
        if (!isMemberReadable(member)) {
            throw UnknownIdentifierException.create(member);
        }
        try {
            Object v = Objects.requireNonNull(cache.get(member))
                    .getValue(Objects.requireNonNull(symbolCache.get(member)));
            if (InteropLibrary.isValidValue(v)) {
                return v;
            }
        } catch (ELispSignals.ELispSignalException ignored) {
            // ignored
        }
        return ELispGlobals.NIL;
    }

    @ExportMessage
    boolean isMemberInsertable(String member) {
        return !isMemberReadable(member);
    }

    @ExportMessage
    @TruffleBoundary
    boolean isMemberModifiable(String member) {
        return isMemberReadable(member) && !Objects.requireNonNull(cache.get(member)).isConstant();
    }

    @ExportMessage
    @TruffleBoundary
    void writeMember(String member, Object value) throws UnknownIdentifierException, UnsupportedTypeException {
        ELispSymbol symbol = Objects.requireNonNull(symbolCache.get(member));
        ValueStorage storage = cache.get(member);
        if (storage == null) {
            symbol = context.obarray().intern(member);
            storage = context.getStorage(symbol);
            symbolCache.put(member, symbol);
            cache.put(member, storage);
        }
        if (storage.isConstant()) {
            throw UnknownIdentifierException.create(member);
        }
        if (InteropLibrary.isValidValue(value)) {
            storage.setValue(value, symbol, context);
        } else {
            throw UnsupportedTypeException.create(new Object[]{value});
        }
    }
}
