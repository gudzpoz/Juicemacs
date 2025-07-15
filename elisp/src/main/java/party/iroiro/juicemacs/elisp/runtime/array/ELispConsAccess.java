package party.iroiro.juicemacs.elisp.runtime.array;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import org.eclipse.jdt.annotation.Nullable;

import java.util.function.Predicate;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public abstract class ELispConsAccess {
    private ELispConsAccess() {}

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class IsNilNode extends Node {
        public abstract boolean executeIsNil(@Nullable Node node, Object object);

        @Specialization
        static boolean isBooleanNil(boolean bool) {
            return !bool;
        }
        @Specialization(replaces = {"isBooleanNil"})
        static boolean isNil(Object object) {
            return object == Boolean.FALSE || object == NIL;
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsCarNode extends Node {
        public abstract Object executeCar(@Nullable Node node, ELispCons cons);

        @Specialization(guards = {"iter.strategy() == strategy"}, limit = "2")
        static Object carConsPair(
                ELispCons iter,
                @Cached(value = "iter.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            return strategy.car(iter.array, iter.index);
        }
        @Specialization
        static Object carConsUncached(ELispCons iter) {
            return iter.car();
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsCdrNode extends Node {
        public abstract Object executeCdr(@Nullable Node node, ELispCons cons);

        @Specialization(guards = {"iter.strategy() == strategy"}, limit = "2")
        static Object cdrConsPair(
                ELispCons iter,
                @Cached(value = "iter.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            return strategy.cdr(iter.array, iter.index);
        }
        @Specialization
        static Object cdrConsUncached(ELispCons iter) {
            return iter.cdr();
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsSetCarNode extends Node {
        public abstract void executeSetCar(@Nullable Node node, ELispCons cons, Object element);

        @Specialization(guards = {"iter.strategy() == strategy"}, limit = "2")
        static void setCarConsPair(
                ELispCons iter,
                Object element,
                @Cached(value = "iter.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            strategy.setCar(iter.array, iter.index, element);
        }
        @Specialization
        static void setCarConsUncached(ELispCons iter, Object element) {
            iter.setCar(element);
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsSetCdrNode extends Node {
        public abstract void executeSetCdr(@Nullable Node node, ELispCons cons, Object element);

        @Specialization(guards = {"iter.strategy() == strategy"}, limit = "2")
        static void setCdrConsPair(
                ELispCons iter,
                Object element,
                @Cached(value = "iter.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            strategy.setCdr(iter.array, iter.index, element);
        }
        @Specialization
        static void setCdrConsUncached(ELispCons iter, Object element) {
            iter.setCdr(element);
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsPrependConsNode extends Node {
        public abstract ELispCons executeCons(@Nullable Node node, Object car, Object cdr);

        @Specialization
        static ELispCons consArrayCons(
                Object car, ELispCons iter,
                @Cached(value = "iter.strategy()", allowUncached = true)
                ArrayStrategy strategy
        ) {
            return strategy.cons(iter.array, iter.index, car);
        }
        @Specialization(replaces = "consArrayCons")
        static ELispCons consConsPair(Object car, Object cdr) {
            if (isNil(cdr)) {
                return ELispCons.listOf(car);
            }
            if (cdr instanceof ELispCons cons) {
                return cons.strategy().cons(cons.array, cons.index, car);
            }
            ELispCons cons = ELispCons.listOf(car);
            cons.setCdr(cdr);
            return cons;
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsNReverseNode extends Node {
        public abstract ELispCons executeNReverse(@Nullable Node node, Object list);

        @Specialization(guards = "strategy == seq.strategy()", limit = "2")
        static ELispCons nReverseArray(
                ELispCons seq,
                @Cached(value = "seq.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            return strategy.nReverse(seq.array, seq.index);
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsReverseNode extends Node {
        public abstract ELispCons executeReverse(@Nullable Node node, Object list);

        @Specialization(guards = "strategy == seq.strategy()", limit = "2")
        static ELispCons reverseArray(
                ELispCons seq,
                @Cached(value = "seq.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            return strategy.reverse(seq.array, seq.index);
        }
    }

    @GenerateUncached
    @GenerateInline(inlineByDefault = true)
    public abstract static class ConsFilterNode extends Node {
        public abstract Object executeFilter(@Nullable Node node, Object list, Predicate<Object> predicate);

        @Specialization(guards = "strategy == seq.strategy()", limit = "2")
        static Object filterArray(
                ELispCons seq,
                Predicate<Object> predicate,
                @Cached(value = "seq.strategy()", allowUncached = true) ArrayStrategy strategy
        ) {
            return strategy.filter(seq.array, seq.index, predicate);
        }
    }
}
