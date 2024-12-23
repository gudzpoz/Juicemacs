package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.util.Objects;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public final class ValueStorage {
    /// The unbound value, used as the value of void variables and not exposed to any caller.
    public static final Object UNBOUND = new Object();

    private final CyclicAssumption unchangedAssumption = new CyclicAssumption("unchanged");
    private int changes = 0;
    @CompilerDirectives.CompilationFinal
    private volatile boolean assumeConstant = true;

    /**
     * True means that this variable has been explicitly declared
     * special (with `defvar' etc.), and shouldn't be lexically bound.
     */
    private boolean special;
    /**
     * Indicates whether operations are needed before any writes.
     */
    private TrappedWrite trappedWrite;
    /**
     * Indicates where the value can be found.
     */
    private Value delegate;
    /**
     * Indicates whether the value is thread-locally overridden.
     */
    @Nullable
    private ThreadLocalStorage threadLocalValue = null;
    /**
     * The symbol's property list.
     */
    @Nullable
    private ELispHashtable properties = null;

    public ValueStorage() {
        this(new PlainValue(UNBOUND));
    }

    public ValueStorage(Value value) {
        this.special = false;
        this.trappedWrite = TrappedWrite.NORMAL_WRITE;
        this.delegate = value;
    }

    public boolean isConstant() {
        return trappedWrite == TrappedWrite.NO_WRITE;
    }

    public void setConstant(boolean constant) {
        trappedWrite = constant ? TrappedWrite.NO_WRITE : TrappedWrite.NORMAL_WRITE;
    }

    public boolean isSpecial() {
        return special;
    }

    public void setSpecial(ELispContext context, boolean special) {
        this.special = special;
        context.invalidateSpecialVariables();
    }

    @Idempotent
    public boolean isPlainValue() {
        return delegate instanceof ValueStorage.PlainValue && threadLocalValue == null;
    }

    //#region Constant folding
    public Assumption getUnchangedAssumption() {
        return unchangedAssumption.getAssumption();
    }
    @Idempotent
    public boolean isAssumeConstant() {
        return assumeConstant;
    }
    @CompilerDirectives.TruffleBoundary
    public void updateAssumeConstant() {
        if (!assumeConstant) {
            return;
        }
        synchronized (this) {
            if (changes <= 3) {
                changes++;
                unchangedAssumption.invalidate();
            } else {
                noLongerAssumeConstant();
            }
        }
    }
    @CompilerDirectives.TruffleBoundary
    public void noLongerAssumeConstant() {
        if (!assumeConstant) {
            return;
        }
        synchronized (this) {
            assumeConstant = false;
            unchangedAssumption.getAssumption().invalidate();
        }
    }
    //#endregion Constant folding

    //#region Value API
    /**
     * @return the value of the symbol, or {@link #UNBOUND}
     */
    public Object getAnyValue() {
        if (threadLocalValue != null) {
            Object local = threadLocalValue.getValue();
            if (local != UNBOUND) {
                return local;
            }
        }
        return delegate.getValue();
    }

    public Object getValue(ELispSymbol symbol) {
        Object rawValue = getAnyValue();
        if (rawValue == UNBOUND) {
            throw ELispSignals.voidVariable(symbol);
        }
        return rawValue;
    }

    public void aliasSymbol(ELispSymbol from, ELispSymbol to) {
        if (isConstant()) {
            throw ELispSignals.error("Cannot make a constant an alias: " + from);
        }
        switch (delegate) {
            case PlainValue _, VarAlias _ -> delegate = new VarAlias(to);
            default -> throw ELispSignals.error("Don’t know how to make a buffer-local variable an alias: " + from);
        }
        noLongerAssumeConstant();
    }

    public Object swapThreadLocalValue(Object value, ELispSymbol symbol) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(symbol);
        }
        noLongerAssumeConstant();
        if (threadLocalValue == null) {
            threadLocalValue = new ThreadLocalStorage(value);
            return UNBOUND;
        } else {
            Object prev = threadLocalValue.getValue();
            threadLocalValue.setValue(value);
            return prev;
        }
    }

    public void setValue(Object value, ELispSymbol symbol) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(symbol);
        }
        if (threadLocalValue != null && threadLocalValue.isBoundAndSetValue(value)) {
            return;
        }
        updateAssumeConstant();
        this.delegate.setValue(value);
    }

    public void makeUnbound() {
        this.delegate = new PlainValue(UNBOUND);
        updateAssumeConstant();
    }

    //#endregion Value API

    //#region Buffer local
    public boolean isBound() {
        return getAnyValue() != UNBOUND;
    }

    public boolean isDefaultBound() {
        if (delegate instanceof BufferLocal local) {
            return local.defaultValue != UNBOUND;
        }
        return isBound();
    }

    public Object getDefaultValue() {
        return delegate instanceof BufferLocal local ? local.defaultValue : delegate.getValue();
    }

    @CompilerDirectives.TruffleBoundary
    public void setDefaultValue(Object value, ELispSymbol symbol) {
        if (isConstant()) {
            if (symbol.isKeyword() && value == symbol) {
                // "Allow setting keywords to their own value"?
                return;
            }
            throw ELispSignals.settingConstant(symbol);
        }
        if (delegate instanceof BufferLocal local) {
            // TODO: "If this variable is not always local in all buffers"...
            // What is a not-always-local buffer-local variable?
            local.defaultValue = value;
        } else if (delegate instanceof VarAlias(ELispSymbol target)) {
            // Recursive call: Truffle bails out
            target.setDefaultValue(value);
        } else {
            updateAssumeConstant();
            delegate.setValue(value);
        }
    }

    @CompilerDirectives.TruffleBoundary
    public void setBufferLocal(boolean localIfSet, ELispSymbol symbol) {
        noLongerAssumeConstant();
        if (delegate instanceof VarAlias(ELispSymbol target)) {
            target.setBufferLocal(localIfSet);
            return;
        }
        if (delegate instanceof BufferLocal local) {
            if (localIfSet) {
                // make-variable-buffer-local
                local.localIfSet = true;
            } else {
                // make-local-variable
                local.setBufferLocalValue(getAnyValue());
            }
            return;
        }
        Object prevValue = getAnyValue();
        BufferLocal local = new BufferLocal(symbol);
        delegate = local;
        local.localIfSet = localIfSet;
        if (localIfSet) {
            // make-variable-buffer-local
            local.defaultValue = prevValue == UNBOUND ? false : prevValue;
        } else {
            // make-local-variable
            local.defaultValue = prevValue;
            local.setBufferLocalValue(prevValue);
        }
    }

    public boolean isBufferLocal(Object buffer) {
        return delegate instanceof BufferLocal local && local.getBufferLocalValue() != UNBOUND;
    }

    public boolean isBufferLocalIfSet(Object buffer) {
        // TODO: How does buffer-local work?
        return delegate instanceof BufferLocal local &&
                (local.getBufferLocalValue() != UNBOUND || local.localIfSet);
    }
    //#endregion Buffer local

    public ELispHashtable ensureProperties() {
        ELispHashtable props = properties;
        if (props == null) {
            props = new ELispHashtable();
            properties = props;
        }
        return props;
    }

    public void putProperty(Object k, Object v) {
        ensureProperties().put(k, v);
    }

    public Object getProperty(Object k) {
        return ensureProperties().get(k);
    }

    public Object getProperties() {
        ELispHashtable props = properties;
        if (props == null) {
            return false;
        }
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        props.forEach((k, v) -> builder.add(k).add(v));
        return builder.build();
    }

    /**
     * Interface with similar semantics to {@code enum symbol_redirect}
     * and the corresponding enum in Emacs
     */
    public sealed interface Value {
        Object getValue();
        void setValue(Object value);
    }

    public static final class PlainValue implements Value {
        private Object value;

        public PlainValue(Object value) {
            this.value = value;
        }

        @Override
        public Object getValue() {
            return value;
        }

        @Override
        public void setValue(Object value) {
            this.value = value;
        }
    }

    public record VarAlias(ELispSymbol target) implements Value {
        @Override
        public Object getValue() {
            ELispContext context = ELispContext.get(null);
            return context.getValue(getAliased(context));
        }

        @Override
        public void setValue(Object value) {
            target.setValue(value);
        }

        public ELispSymbol getAliased(ELispContext context) {
            ELispSymbol alias = target;
            // TODO: Cache symbol value storage
            while (context.getStorage(alias).delegate instanceof VarAlias(ELispSymbol next)) {
                alias = next;
            }
            return alias;
        }
    }

    public static final class BufferLocal implements Value {
        @Nullable
        private ELispBuffer cachedBuffer = null;
        @Nullable
        private Forwarded value = null;
        private boolean localIfSet = false;
        private Object defaultValue = UNBOUND;
        private final ELispSymbol symbol;

        public BufferLocal(ELispSymbol symbol) {
            this.symbol = symbol;
        }

        private void updateCache() {
            ELispBuffer buffer = ELispContext.get(null).currentBuffer();
            if (cachedBuffer != buffer) {
                cachedBuffer = buffer;
                value = cachedBuffer.getLocal(symbol);
            }
        }

        public Object getDefaultValue() {
            return defaultValue;
        }

        public Object getBufferLocalValue() {
            return value == null ? UNBOUND : value.getValue();
        }

        public void setBufferLocalValue(Object value) {
            updateCache();
            if (this.value == null) {
                this.value = Objects.requireNonNull(cachedBuffer).makeLocal(symbol);
            }
            this.value.setValue(value);
        }

        @Override
        public Object getValue() {
            updateCache();
            return value == null ? defaultValue : value.getValue();
        }

        @Override
        public void setValue(Object value) {
            updateCache();
            if (this.value == null) {
                if (!localIfSet) {
                    this.defaultValue = value;
                    return;
                }
                this.value = Objects.requireNonNull(cachedBuffer).makeLocal(symbol);
            }
            this.value.setValue(value);
        }
    }

    public sealed abstract static class AbstractForwarded<T> implements Value {
        protected T value;

        public AbstractForwarded() {
            this.value = defaultValue();
        }

        public AbstractForwarded(T o) {
            this.value = o;
        }

        @Override
        public T getValue() {
            return value;
        }

        @Override
        public void setValue(Object value) {
            this.value = typeCheck(value);
        }

        abstract T defaultValue();
        abstract T typeCheck(Object o);
    }

    public static final class Forwarded extends AbstractForwarded<Object> {
        public Forwarded() {
            super();
        }
        public Forwarded(Object o) {
            super(o);
        }
        @Override
        Object defaultValue() {
            return NIL;
        }
        @Override
        Object typeCheck(Object o) {
            return o;
        }
    }

    public static final class ForwardedLong extends AbstractForwarded<Long> {
        public ForwardedLong() {
            super();
        }
        public ForwardedLong(long o) {
            super(o);
        }
        @Override
        Long defaultValue() {
            return 0L;
        }
        @Override
        Long typeCheck(Object o) {
            if (o instanceof Long l) {
                return l;
            }
            throw ELispSignals.wrongTypeArgument(INTEGERP, o);
        }
    }

    public static final class ForwardedBool extends AbstractForwarded<Boolean> {
        public ForwardedBool() {
            super();
        }
        public ForwardedBool(Boolean o) {
            super(o);
        }
        @Override
        Boolean defaultValue() {
            return Boolean.FALSE;
        }
        @Override
        Boolean typeCheck(Object o) {
            return isNil(o) ? Boolean.FALSE : Boolean.TRUE;
        }
        public boolean isT() {
            return value;
        }
    }

    public static final class ForwardedPerBuffer extends AbstractForwarded<Object> {
        private final int index;
        public ForwardedPerBuffer(int index, ELispSymbol predicate) {
            super(predicate);
            this.index = index;
        }
        @Override
        public Object getValue() {
            ELispContext context = ELispContext.get(null);
            return context.currentBuffer().getSlot(index);
        }
        @Override
        public void setValue(Object value) {
            ELispContext context = ELispContext.get(null);
            setBufferValue(value, context.currentBuffer());
        }
        public void setBufferValue(Object value, ELispBuffer buffer) {
            ELispSymbol predicate = asSym(this.value);
            if (isInvalid(predicate, value)) {
                throw ELispSignals.wrongTypeArgument(predicate, value);
            }
            buffer.setSlot(index, value);
        }
        private static boolean isInvalid(ELispSymbol predicate, Object value) {
            if (!isNil(value) && predicate != NIL) {
                Object choices = BuiltInFns.FGet.get(predicate, CHOICE);
                if (!isNil(choices)) {
                    return isNil(BuiltInFns.FMemq.memq(value, choices));
                }
                Object range = BuiltInFns.FGet.get(predicate, RANGE);
                if (range instanceof ELispCons cons) {
                    return (!BuiltInData.FNumberp.numberp(value)
                            || !(
                            BuiltInData.compareTo(cons.car(), value) <= 0
                                    && BuiltInData.compareTo(value, cons.cdr()) <= 0
                    ));
                }
                if (BuiltInEval.FFunctionp.functionp(predicate)) {
                    return isNil(BuiltInEval.FFuncall.funcall(predicate, new Object[]{value}));
                }
            }
            return false;
        }
        @Override
        Object defaultValue() {
            throw CompilerDirectives.shouldNotReachHere();
        }
        @Override
        Object typeCheck(Object o) {
            return o;
        }
    }

    public enum TrappedWrite {
        NORMAL_WRITE,
        NO_WRITE,
        TRAPPED_WRITE,
    }
}
