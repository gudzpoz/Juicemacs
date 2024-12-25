package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.HashSet;
import java.util.Optional;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage.UNBOUND;


/// The symbol part of `struct Lisp_Symbol`
///
/// Different from the C version, we do not keep the dynamically-bound value
/// in the symbol, but in the Truffle context to allow parallel context usages.
@ExportLibrary(InteropLibrary.class)
public final class ELispSymbol extends AbstractELispIdentityObject implements TruffleObject {
    public static final String SPECIAL_CHARS = "()[]#?\"'`,;.";

    private final MuleString name;
    private final boolean isKeyword;

    public ELispSymbol(String name) {
        this(MuleString.fromString(name), name.startsWith(":"));
    }

    public ELispSymbol(MuleString name) {
        this(name, name.length() > 0 && name.charAt(0) == ':');
    }

    public ELispSymbol(MuleString name, boolean isKeyword) {
        this.name = name;
        this.isKeyword = isKeyword;
    }

    private Optional<ValueStorage> tryGetStorage() {
        return ELispContext.get(null).getStorageLazy(this);
    }

    private ValueStorage getStorage() {
        return ELispContext.get(null).getStorage(this);
    }

    public void setConstant(boolean b) {
        getStorage().setConstant(b);
    }

    public boolean isConstant() {
        return getStorage().isConstant();
    }

    /**
     * @return the value of the symbol, or {@link ValueStorage#UNBOUND}
     */
    private Object getAnyValue() {
        Optional<ValueStorage> storage = tryGetStorage();
        return storage.map(ValueStorage::getAnyValue).orElse(UNBOUND);
    }

    public Object getValue() {
        if (isKeyword) {
            return this;
        }
        return checkUnbound(getAnyValue());
    }

    /**
     * Sets the thread-local value and returns the previous value
     * <p>
     * It might expose the internally used {@code UNBOUND} value, so the caller should make sure
     * to either dispose the returned value or only swap it back afterward.
     * </p>
     * @param value the new thread-local value
     * @return the previous thread-local value, which should be treated as non-transparent
     */
    public Object swapThreadLocalValue(Object value) {
        return getStorage().swapThreadLocalValue(value, this);
    }

    public void setValue(Object value) {
        if (isKeyword) {
            if (value == this) {
                return;
            }
            throw ELispSignals.settingConstant(this);
        }
        ELispContext context = ELispContext.get(null);
        context.getStorage(this).setValue(value, this, context);
    }

    public boolean isBound() {
        return getAnyValue() != UNBOUND;
    }

    public Object getDefaultValue() {
        Object value = tryGetStorage().map(ValueStorage::getDefaultValue).orElse(UNBOUND);
        return checkUnbound(value);
    }

    public void setDefaultValue(Object value) {
        ELispContext context = ELispContext.get(null);
        context.getStorage(this).setDefaultValue(value, this, context);
    }

    public void setBufferLocal(boolean localIfSet) {
        getStorage().setBufferLocal(localIfSet, this);
    }

    public boolean isBufferLocal(Object buffer) {
        Optional<ValueStorage> storage = tryGetStorage();
        //noinspection OptionalIsPresent
        if (storage.isEmpty()) {
            return false;
        }
        return storage.get().isBufferLocal(buffer);
    }

    public boolean isBufferLocalIfSet(Object buffer) {
        Optional<ValueStorage> storage = tryGetStorage();
        //noinspection OptionalIsPresent
        if (storage.isEmpty()) {
            return false;
        }
        return storage.get().isBufferLocalIfSet(buffer);
    }

    public void aliasSymbol(ELispSymbol symbol) {
        getStorage().aliasSymbol(this, symbol);
    }

    public Object getIndirectFunction() {
        ELispContext context = ELispContext.get(null);
        Object o = context.getFunctionStorage(this).get();
        if (CompilerDirectives.injectBranchProbability(CompilerDirectives.FASTPATH_PROBABILITY, !(o instanceof ELispSymbol))) {
            return o;
        }
        return slowPathGetIndirectFunction((ELispSymbol) o, context);
    }

    @CompilerDirectives.TruffleBoundary
    private static Object slowPathGetIndirectFunction(ELispSymbol symbol, ELispContext context) {
        Object o;
        HashSet<ELispSymbol> visited = new HashSet<>();
        visited.add(symbol);
        while (true) {
            o = context.getFunctionStorage(symbol).get();
            if (!(o instanceof ELispSymbol nextSymbol)) {
                return o;
            }
            if (visited.contains(nextSymbol)) {
                throw ELispSignals.cyclicVariableIndirection(symbol);
            }
            visited.add(nextSymbol);
            symbol = nextSymbol;
        }
    }

    public Object getFunction() {
        Optional<FunctionStorage> storage = ELispContext.get(null).getFunctionStorageLazy(this);
        return storage.map(FunctionStorage::get).orElse(false);
    }

    public void setFunction(Object function) {
        ELispContext.get(null).getFunctionStorage(this).set(function, this);
    }

    public MuleString name() {
        return name;
    }

    public boolean isKeyword() {
        return isKeyword;
    }

    public boolean isSpecial() {
        return tryGetStorage().map(ValueStorage::isSpecial).orElse(false);
    }

    public void setSpecial(boolean b) {
        ELispContext context = ELispContext.get(null);
        context.getStorage(this).setSpecial(context, b);
    }

    public void putProperty(Object k, Object v) {
        getStorage().putProperty(k, v);
    }

    public Object getProperty(Object k) {
        return getStorage().getProperty(k);
    }

    public Object getProperties() {
        return getStorage().getProperties();
    }

    @Override
    public String display() {
        PrimitiveIterator.OfInt i = name.codePoints().iterator();
        StringBuilder result = new StringBuilder(Math.toIntExact(name.length()));
        while (i.hasNext()) {
            int c = i.nextInt();
            if (c <= ' ' || SPECIAL_CHARS.indexOf(c) != -1 || c == 0x00A0 /* no breaking space */) {
                result.append('\\');
            }
            result.appendCodePoint(c);
        }
        return result.toString();
    }

    @Override
    public String toString() {
        return name.toString();
    }

    //#region InteropLibrary
    @ExportMessage
    public boolean isMetaObject() {
        return true;
    }
    @ExportMessage
    public Object getMetaQualifiedName() {
        return getMetaSimpleName();
    }
    @ExportMessage
    public Object getMetaSimpleName() {
        return name;
    }
    @ExportMessage
    public boolean isMetaInstance(Object ignored) {
        return false;
    }
    //#endregion InteropLibrary

    private Object checkUnbound(Object value) {
        if (value == UNBOUND) {
            throw ELispSignals.voidVariable(this);
        }
        return value;
    }
}
