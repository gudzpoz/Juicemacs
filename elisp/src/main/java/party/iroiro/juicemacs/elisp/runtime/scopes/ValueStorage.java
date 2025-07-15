package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentBuffer;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentFrame;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public final class ValueStorage implements Externalizable {
    /// The unbound value, used as the value of void variables and not exposed to any caller.
    public static final Object UNBOUND = new Object();

    private final CyclicAssumption unchangedAssumption = new CyclicAssumption("variable assumed constant");
    private transient int changes = 0;
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
        if (isAssumeConstant() && delegate instanceof ValueStorage.PlainValue) {
            return unchangedAssumption.getAssumption();
        }
        return Assumption.NEVER_VALID;
    }
    @Idempotent
    public boolean isAssumeConstant() {
        return assumeConstant;
    }
    @CompilerDirectives.TruffleBoundary
    public void updateAssumeConstant(ELispContext context) {
        if (!assumeConstant) {
            return;
        }
        synchronized (this) {
            if (changes <= context.options().globalVariableMaxInvalidations()) {
                changes++;
                unchangedAssumption.invalidate();
            } else {
                noLongerAssumeConstant();
            }
        }
    }
    public void noLongerAssumeConstant() {
        if (!assumeConstant) {
            return;
        }
        synchronized (this) {
            assumeConstant = false;
            unchangedAssumption.invalidate();
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
        if (value != UNBOUND && this.delegate instanceof AbstractForwarded<?> forwarded) {
            value = forwarded.typeCheck(value);
        }
        if (threadLocalValue == null) {
            threadLocalValue = new ThreadLocalStorage(value);
            return UNBOUND;
        } else {
            Object prev = threadLocalValue.getValue();
            threadLocalValue.setValue(value);
            return prev;
        }
    }

    public void setValue(Object value, ELispSymbol symbol, ELispContext context) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(symbol);
        }
        if (threadLocalValue != null && threadLocalValue.isBoundAndSetValue(value)) {
            return;
        }
        if (isAssumeConstant()) {
            updateAssumeConstant(context);
        }
        this.delegate.setValue(value);
    }

    public void setValue(Object value, ELispSymbol symbol, Node node) {
        if (isConstant()) {
            throw ELispSignals.settingConstant(symbol);
        }
        if (threadLocalValue != null && threadLocalValue.isBoundAndSetValue(value)) {
            return;
        }
        if (isAssumeConstant()) {
            updateAssumeConstant(ELispContext.get(node));
        }
        this.delegate.setValue(value);
    }

    public void makeUnbound(ELispContext context) {
        if (threadLocalValue != null && threadLocalValue.isBoundAndSetValue(UNBOUND)) {
            return;
        }
        if (isAssumeConstant()) {
            updateAssumeConstant(context);
        }
        this.delegate = new PlainValue(UNBOUND);
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
    public void setDefaultValue(Object value, ELispSymbol symbol, ELispContext context) {
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
            delegate.setValue(value);
            if (isAssumeConstant()) {
                updateAssumeConstant(context);
            }
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

    @Override
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(trappedWrite);
        if (trappedWrite != TrappedWrite.NO_WRITE) {
            out.writeBoolean(assumeConstant);
        }
        out.writeBoolean(special);
        out.writeObject(delegate);
        out.writeObject(properties);
    }

    @Override
    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        trappedWrite = (TrappedWrite) in.readObject();
        if (trappedWrite == TrappedWrite.NO_WRITE) {
            assumeConstant = true;
        } else {
            assumeConstant = in.readBoolean();
        }
        special = in.readBoolean();
        delegate = (Value) in.readObject();
        properties = (ELispHashtable) in.readObject();
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
            ELispBuffer buffer = asBuffer(ELispLanguage.get(null).currentBuffer().getValue());
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
            return currentBuffer().getSlot(index);
        }
        @Override
        public void setValue(Object value) {
            setBufferValue(value, currentBuffer());
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
                    return isNil(BuiltInEval.FFuncall.funcall(null, predicate, value));
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

    public static final class ForwardedPerKboard extends AbstractForwarded<Object> {
        public ForwardedPerKboard(int index) {
            super(index); // NOPMD
        }
        @Override
        public Object getValue() {
            return currentFrame().getKboard().getSlot((Integer) value);
        }
        @Override
        public void setValue(Object newValue) {
            currentFrame().getKboard().setSlot((Integer) value, newValue);
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
