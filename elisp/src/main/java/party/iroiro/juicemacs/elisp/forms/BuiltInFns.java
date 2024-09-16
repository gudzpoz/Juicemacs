package party.iroiro.juicemacs.elisp.forms;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.SUBFEATURES;

import com.oracle.truffle.api.strings.TruffleStringBuilderUTF16;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

/**
 * Built-in functions from {@code src/comp.c}
 */
public class BuiltInFns extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFnsFactory.getFactories();
    }

    public static Iterator<?> iterateSequence(Object sequence) {
        if (ELispSymbol.isNil(sequence)) {
            return Collections.emptyIterator();
        }
        return switch (sequence) {
            case ELispCons cons -> cons.iterator();
            case ELispVector vector -> vector.iterator();
            case ELispString string -> string.iterator();
            case ELispBoolVector boolVector -> boolVector.iterator();
            default -> throw new IllegalArgumentException();
        };
    }

    @ELispBuiltIn(name = "identity", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FIdentity extends ELispBuiltInBaseNode {
        @Specialization
        public static Object identity(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "random", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRandom extends ELispBuiltInBaseNode {
        @Specialization
        public static Object random(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLength extends ELispBuiltInBaseNode {
        @Specialization
        public static long lengthNil(ELispSymbol a) {
            if (ELispSymbol.isNil(a)) {
                return 0;
            }
            throw new IllegalArgumentException();
        }

        @Specialization
        public static long lengthCons(ELispCons a) {
            return a.size();
        }

        @Specialization
        public static long lengthVector(ELispVector a) {
            return a.size();
        }

        @Specialization
        public static long lengthString(ELispString a) {
            return a.codepointCount();
        }
    }

    @ELispBuiltIn(name = "safe-length", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSafeLength extends ELispBuiltInBaseNode {
        @Specialization
        public static Object safeLength(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length<", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLengthLess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lengthLess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length>", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLengthGreater extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lengthGreater(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "length=", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLengthEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lengthEqual(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "proper-list-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProperListP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object properListP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-bytes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringBytes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringBytes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-distance", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FStringDistance extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringDistance(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-equal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringEqual(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "compare-strings", minArgs = 6, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FCompareStrings extends ELispBuiltInBaseNode {
        @Specialization
        public static Object compareStrings(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-lessp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringLessp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-version-lessp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStringVersionLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringVersionLessp(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-collate-lessp", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FStringCollateLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringCollateLessp(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-collate-equalp", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FStringCollateEqualp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringCollateEqualp(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "append", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FAppend extends ELispBuiltInBaseNode {
        @Specialization
        public static Object append(Object[] args) {
            if (args.length == 0) {
                return false;
            }
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            for (int i = 0; i < args.length - 1; i++) {
                Iterator<?> iterator = iterateSequence(args[i]);
                while (iterator.hasNext()) {
                    builder.add(iterator.next());
                }
            }
            return builder.build(args[args.length - 1]);
        }
    }

    @ELispBuiltIn(name = "concat", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FConcat extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString concat(Object[] args) {
            TruffleStringBuilderUTF16 builder = TruffleStringBuilderUTF16.createUTF16();
            for (Object arg : args) {
                if (arg instanceof ELispString s) {
                    builder.appendStringUncached(s.toTruffleString());
                } else {
                    Iterator<?> i = iterateSequence(arg);
                    while (i.hasNext()) {
                        builder.appendCodePointUncached((int) (long) i.next());
                    }
                }
            }
            return new ELispString(builder.toStringUncached());
        }
    }

    @ELispBuiltIn(name = "vconcat", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FVconcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object vconcat(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-sequence", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopySequence extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copySequenceNil(ELispSymbol a) {
            if (ELispSymbol.isNil(a)) {
                return false;
            }
            throw new IllegalArgumentException();
        }

        @Specialization
        public static Object copySequenceList(ELispCons a) {
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            for (Object e : a) {
                builder.add(e);
            }
            return builder.build();
        }

        @Specialization
        public static Object copySequenceVector(ELispVector a) {
            return new ELispVector(a);
        }

        @Specialization
        public static Object copySequenceBoolVec(ELispBoolVector a) {
            return new ELispBoolVector(a);
        }

        @Specialization
        public static Object copySequenceString(ELispString a) {
            return new ELispString(a.toString());
        }
    }

    @ELispBuiltIn(name = "string-make-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringMakeMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringMakeMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-make-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringMakeUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringMakeUnibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-as-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringAsUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringAsUnibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-as-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringAsMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringAsMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-to-unibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FStringToUnibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringToUnibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-alist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopyAlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copyAlist(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "substring", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSubstring extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substring(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "substring-no-properties", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSubstringNoProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substringNoProperties(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "take", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTake extends ELispBuiltInBaseNode {
        @Specialization
        public static Object take(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "ntake", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNtake extends ELispBuiltInBaseNode {
        @Specialization
        public static Object ntake(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nthcdr", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNthcdr extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nthcdr(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nth", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNth extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nth(long a, Object b) {
            if (ELispSymbol.isNil(b)) {
                return false;
            }
            try {
                return ((ELispCons) b).get((int) a);
            } catch (IndexOutOfBoundsException ignored) {
                return false;
            }
        }
    }

    @ELispBuiltIn(name = "elt", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FElt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object elt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "member", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMember extends ELispBuiltInBaseNode {
        @Specialization
        public static Object member(Object a, Object b) {
            if (ELispSymbol.isNil(b)) {
                return false;
            }
            ELispCons.BrentTortoiseHareIterator iterator = ((ELispCons) b).listIterator(0);
            while (iterator.hasNext()) {
                if (FEqual.equal(iterator.currentCons().car(), a)) {
                    return iterator.currentCons();
                }
                iterator.next();
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "memq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMemq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memq(Object a, Object b) {
            if (ELispSymbol.isNil(b)) {
                return false;
            }
            ELispCons.BrentTortoiseHareIterator iterator = ((ELispCons) b).listIterator(0);
            while (iterator.hasNext()) {
                if (BuiltInData.FEq.eq(iterator.currentCons().car(), a)) {
                    return iterator.currentCons();
                }
                iterator.next();
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "memql", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMemql extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memql(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "assq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAssq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assq(Object a, Object sequence) {
            if (ELispSymbol.isNil(sequence)) {
                return false;
            }
            for (Object e : (ELispCons) sequence) {
                if (e instanceof ELispCons cons && BuiltInData.FEq.eq(a, cons.car())) {
                    return cons;
                }
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "assoc", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAssoc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object assoc(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rassq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRassq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object rassq(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rassoc", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRassoc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object rassoc(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDelq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object delq(Object a, Object sequence) {
            if (ELispSymbol.isNil(sequence)) {
                return false;
            }
            ELispCons cons = (ELispCons) sequence;
            while (BuiltInData.FEq.eq(cons.car(), a)) {
                Object cdr = cons.cdr();
                if (ELispSymbol.isNil(cdr)) {
                    return false;
                }
                cons = (ELispCons) cdr;
            }
            ELispCons.BrentTortoiseHareIterator i = cons.listIterator(1);
            ELispCons prev = cons;
            while (i.hasNext()) {
                if (BuiltInData.FEq.eq(i.currentCons().car(), a)) {
                    prev.setCdr(i.currentCons().cdr());
                } else {
                    prev = i.currentCons();
                }
                i.next();
            }
            return cons;
        }
    }

    @ELispBuiltIn(name = "delete", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDelete extends ELispBuiltInBaseNode {
        @Specialization
        public static Object delete(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nreverse", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNreverse extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean nreverseNil(ELispSymbol a) {
            if (ELispSymbol.isNil(a)) {
                return false;
            }
            throw new IllegalArgumentException();
        }

        @Specialization
        public static Object nreverseBoolVec(ELispBoolVector bv) {
            return bv.reverse();
        }

        @Specialization
        public static ELispVector nreverseVec(ELispVector vector) {
            return vector.reverse();
        }

        @Specialization
        public static ELispString nreverseString(ELispString string) {
            return string.reverse();
        }

        @Specialization
        public static ELispCons nreverseList(ELispCons list) {
            ELispCons head = new ELispCons(list.car());
            if (ELispSymbol.isNil(list.cdr())) {
                return head;
            }
            for (Object e : (ELispCons) list.cdr()) {
                ELispCons cons = new ELispCons(e);
                cons.setCdr(head);
                head = cons;
            }
            return head;
        }
    }

    @ELispBuiltIn(name = "reverse", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReverse extends ELispBuiltInBaseNode {
        @Specialization
        public static Object reverse(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sort", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FSort extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sort(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "plist-get", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPlistGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistGet(ELispCons list, Object prop, Object predicate) {
            ELispSymbol eq = ELispSymbol.isNil(predicate) ? ELispContext.EQ : (ELispSymbol) predicate;
            Iterator<Object> iterator = list.iterator();
            try {
                Object[] args = new Object[2];
                args[0] = prop;
                while (iterator.hasNext()) {
                    args[1] = iterator.next();
                    if (!ELispSymbol.isNil(BuiltInEval.FFuncall.funcall(eq, args))) {
                        return iterator.next();
                    }
                    iterator.next();
                }
                return false;
            } catch (NoSuchElementException e) {
                return false;
            }
        }
    }

    @ELispBuiltIn(name = "get", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object get(Object symbol, Object prop) {
            if (symbol instanceof ELispSymbol sym) {
                return sym.getProperty(prop);
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "plist-put", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FPlistPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistPut(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object put(ELispSymbol symbol, Object k, Object v) {
            symbol.putProperty(k, v);
            return v;
        }
    }

    @ELispBuiltIn(name = "plist-member", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPlistMember extends ELispBuiltInBaseNode {
        @Specialization
        public static Object plistMember(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eql", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEql extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean eql(Object a, Object b) {
            return BuiltInData.FEq.eq(a, b)
                    || (a instanceof Double da && b instanceof Double db &&
                    Double.doubleToRawLongBits(da) == Double.doubleToRawLongBits(db))
                    || (a instanceof ELispBigNum ia && b instanceof ELispBigNum ib && ia.value.equals(ib.value));
        }
    }

    @ELispBuiltIn(name = "equal", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean equal(Object a, Object b) {
            return switch (a) {
                case Long l when b instanceof Long n -> l.equals(n);
                case Long _ -> equal(b, a);
                case Double d when b instanceof Long n -> d.equals((double) n);
                case Double d when b instanceof Double n -> d.equals(n);
                case Double _ -> equal(b, a);
                case ELispValue v -> v.lispEquals(b);
                default -> BuiltInData.FEq.eq(a, b);
            };
        }
    }

    @ELispBuiltIn(name = "equal-including-properties", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEqualIncludingProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object equalIncludingProperties(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "value<", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FValuelt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object valuelt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "fillarray", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFillarray extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fillarray(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clear-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClearString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clearString(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "nconc", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FNconc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nconc(Object[] args) {
            Object result = false;
            @Nullable Object prev = null;
            for (Object arg : args) {
                if (ELispSymbol.isNil(arg)) {
                    continue;
                }
                if (prev == null) {
                    result = arg;
                } else {
                    ((ELispCons) prev).tail().setCdr(arg);
                }
                prev = arg;
            }
            return result;
        }
    }

    @ELispBuiltIn(name = "mapconcat", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMapconcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapconcat(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapcar", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapcar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapcar(Object f, Object sequence) {
            Iterator<?> i = iterateSequence(sequence);
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            while (i.hasNext()) {
                builder.add(BuiltInEval.FFuncall.funcall(f, new Object[]{i.next()}));
            }
            return builder.build();
        }
    }

    @ELispBuiltIn(name = "mapc", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapc extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapc(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapcan", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapcan extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapcan(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "yes-or-no-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FYesOrNoP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object yesOrNoP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "load-average", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLoadAverage extends ELispBuiltInBaseNode {
        @Specialization
        public static Object loadAverage(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "featurep", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFeaturep extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean featurep(ELispSymbol feature, Object sub) {
            Object isMem = FMemq.memq(feature, ELispGlobals.features.getValue());
            if (ELispSymbol.isNil(isMem)) {
                return !ELispSymbol.isNil(isMem);
            }
            if (ELispSymbol.isNil(sub)) {
                return true;
            }
            ELispSymbol feat = (ELispSymbol) BuiltInData.FCar.car(isMem);
            return !ELispSymbol.isNil(FMemq.memq(sub, feat.getProperty(SUBFEATURES)));
        }
    }

    @ELispBuiltIn(name = "provide", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FProvide extends ELispBuiltInBaseNode {
        @Specialization
        public static Object provide(ELispSymbol feature, Object sub) {
            Object isMem = FMemq.memq(feature, ELispGlobals.features.getValue());
            if (ELispSymbol.isNil(isMem)) {
                ELispGlobals.features.setValue(new ELispCons(feature, ELispGlobals.features.getValue()));
            }
            if (!ELispSymbol.isNil(sub)) {
                feature.putProperty(SUBFEATURES, sub);
            }
            // TODO: Run after-load-alist hooks
            return feature;
        }
    }

    @ELispBuiltIn(name = "require", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FRequire extends ELispBuiltInBaseNode {
        @Specialization
        public static Object require(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widget-put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FWidgetPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widgetPut(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widget-get", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FWidgetGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widgetGet(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "widget-apply", minArgs = 2, maxArgs = 2, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FWidgetApply extends ELispBuiltInBaseNode {
        @Specialization
        public static Object widgetApply(Object a, Object b, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "locale-info", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FLocaleInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object localeInfo(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-encode-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBase64EncodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64EncodeRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64url-encode-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBase64urlEncodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64urlEncodeRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-encode-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBase64EncodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64EncodeString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64url-encode-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBase64urlEncodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64urlEncodeString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-decode-region", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FBase64DecodeRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64DecodeRegion(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "base64-decode-string", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FBase64DecodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object base64DecodeString(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-eq", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEq extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEq(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-eql", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEql extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEql(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-equal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEqual extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEqual(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sxhash-equal-including-properties", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSxhashEqualIncludingProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sxhashEqualIncludingProperties(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-hash-table", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispHashtable makeHashTable(Object[] args) {
            // TODO
            return new ELispHashtable();
        }
    }

    @ELispBuiltIn(name = "copy-hash-table", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCopyHashTable extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copyHashTable(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-count", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableCount extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableCount(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-rehash-size", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableRehashSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableRehashSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-rehash-threshold", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableRehashThreshold extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableRehashThreshold(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-size", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-test", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableTest extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableTest(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-weakness", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableWeakness extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableWeakness(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "hash-table-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FHashTableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object hashTableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clrhash", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FClrhash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clrhash(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "gethash", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FGethash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object gethash(Object k, ELispHashtable hashtable, Object defaultValue) {
            return hashtable.get(k, defaultValue);
        }
    }

    @ELispBuiltIn(name = "puthash", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FPuthash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object puthash(Object k, Object v, ELispHashtable a) {
            a.put(k, v);
            return v;
        }
    }

    @ELispBuiltIn(name = "remhash", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRemhash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object remhash(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "maphash", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMaphash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object maphash(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "define-hash-table-test", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDefineHashTableTest extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defineHashTableTest(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--hash-table-histogram", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalHashTableHistogram extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalHashTableHistogram(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--hash-table-buckets", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalHashTableBuckets extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalHashTableBuckets(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--hash-table-index-size", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalHashTableIndexSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalHashTableIndexSize(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "secure-hash-algorithms", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSecureHashAlgorithms extends ELispBuiltInBaseNode {
        @Specialization
        public static Object secureHashAlgorithms() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "md5", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMd5 extends ELispBuiltInBaseNode {
        @Specialization
        public static Object md5(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "secure-hash", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FSecureHash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object secureHash(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-hash", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferHash extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferHash(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-line-statistics", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLineStatistics extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLineStatistics(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "string-search", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FStringSearch extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stringSearch(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "object-intervals", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObjectIntervals extends ELispBuiltInBaseNode {
        @Specialization
        public static Object objectIntervals(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "line-number-at-pos", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FLineNumberAtPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lineNumberAtPos(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }
}
