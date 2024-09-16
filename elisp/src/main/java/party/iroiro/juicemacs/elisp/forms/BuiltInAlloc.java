package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;

import java.util.ArrayList;
import java.util.List;

public class BuiltInAlloc extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInAllocFactory.getFactories();
    }

    @ELispBuiltIn(name = "make-string", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMakeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeString(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-bool-vector", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeBoolVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeBoolVector(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FBoolVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVector(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "cons", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCons extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cons(Object a, Object b) {
            return new ELispCons(a, b);
        }
    }

    @ELispBuiltIn(name = "list", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object list(Object[] args) {
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            for (Object arg : args) {
                builder.add(arg);
            }
            return builder.build();
        }
    }

    @ELispBuiltIn(name = "make-list", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeList(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-record", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMakeRecord extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeRecord(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "record", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRecord extends ELispBuiltInBaseNode {
        @Specialization
        public static Object record(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-vector", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeVector(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "vector", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object vector(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-byte-code", minArgs = 4, maxArgs = 4, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeByteCode extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeByteCode(Object a, Object b, Object c, Object d, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-closure", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeClosure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeClosure(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-symbol", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeSymbol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMakeMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-finalizer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeFinalizer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeFinalizer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "purecopy", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPurecopy extends ELispBuiltInBaseNode {
        @Specialization
        public static Object purecopy(Object a) {
            // bool-vec, hash-table, etc. are not copied
            return switch (a) {
                case ELispString s -> new ELispString(s.toString());
                case ELispVector v -> {
                    ArrayList<Object> inner = new ArrayList<>(v.size());
                    for (Object o : v) {
                        inner.add(purecopy(o));
                    }
                    yield new ELispVector(inner);
                }
                case ELispCons cons -> new ELispCons(purecopy(cons.car()), purecopy(cons.cdr()));
                default -> a;
            };
        }
    }

    @ELispBuiltIn(name = "garbage-collect", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGarbageCollect extends ELispBuiltInBaseNode {
        @Specialization
        public static Object garbageCollect() {
            Runtime.getRuntime().gc();
            // TODO: Return info
            return false;
        }
    }

    @ELispBuiltIn(name = "garbage-collect-maybe", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGarbageCollectMaybe extends ELispBuiltInBaseNode {
        @Specialization
        public static Object garbageCollectMaybe(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "memory-info", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMemoryInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memoryInfo() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "memory-use-counts", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMemoryUseCounts extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memoryUseCounts() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "malloc-info", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMallocInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mallocInfo() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "malloc-trim", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMallocTrim extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mallocTrim(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
