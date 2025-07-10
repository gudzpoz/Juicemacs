package party.iroiro.juicemacs.elisp.runtime.scopes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.InlinedConditionProfile;
import org.eclipse.jdt.annotation.Nullable;

import static party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage.UNBOUND;

/// A thread-local dynamic scope value
///
/// This class assumes that all virtual threads are hosted on a single
/// thread, and thus all operations are not synchronized.
///
/// The volatile keyword is used to avoid virtual-thread-local caching.
public class ThreadLocalStorage {
    private transient volatile Thread lastThread;
    private transient Object lastThreadValue;
    @Nullable
    private transient volatile ThreadLocal<Object> values;

    public ThreadLocalStorage(Object initValue) {
        lastThread = Thread.currentThread();
        lastThreadValue = initValue;
    }

    @Nullable
    public Object get(Node node, InlinedConditionProfile sameThreadProfile) {
        if (sameThreadProfile.profile(node, Thread.currentThread() == lastThread)) {
            return lastThreadValue;
        } else {
            return fallbackGet();
        }
    }

    public Object getValue() {
        Thread current = Thread.currentThread();
        if (current == lastThread) {
            return lastThreadValue;
        } else {
            return getSlowPath(current);
        }
    }

    @CompilerDirectives.TruffleBoundary
    private Object getSlowPath(Thread current) {
        Object value = fallbackGet();
        if (!lastThread.isAlive()) {
            lastThread = current;
            lastThreadValue = value;
        }
        return value;
    }

    private ThreadLocal<Object> getThreadValues(@Nullable ThreadLocal<Object> threadLocal) {
        if (threadLocal != null) {
            return threadLocal;
        }
        threadLocal = new ThreadLocal<>();
        values = threadLocal;
        return threadLocal;
    }

    @CompilerDirectives.TruffleBoundary
    private Object fallbackGet() {
        ThreadLocal<Object> threadLocal = values;
        return threadLocal == null ? UNBOUND : getThreadValues(threadLocal).get();
    }

    public void set(Node node, Object value, InlinedConditionProfile sameThreadProfile) {
        Thread last = lastThread;
        boolean isCached = Thread.currentThread() == last;
        if (sameThreadProfile.profile(node, isCached || !last.isAlive())) {
            lastThreadValue = value;
            if (!isCached) {
                lastThread = Thread.currentThread();
            }
        } else {
            fallbackSet(value);
        }
    }

    public void setValue(Object value) {
        if (Thread.currentThread() == lastThread) {
            lastThreadValue = value;
        } else {
            fallbackSet(value);
        }
    }

    @CompilerDirectives.TruffleBoundary
    private void fallbackSet(Object value) {
        getThreadValues(values).set(value);
    }

    public boolean isBoundAndSetValue(Object value) {
        if (getValue() == UNBOUND) {
            return false;
        }
        setValue(value);
        return true;
    }
}
