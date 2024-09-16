package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.Source;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispBindingScope;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInEval.evalSub;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

/**
 * Built-in functions from {@code src/lread.c}
 */
public class BuiltInLRead extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInLReadFactory.getFactories();
    }

    @ELispBuiltIn(name = "read-char", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object readChar(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "read-event", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadEvent extends ELispBuiltInBaseNode {
        @Specialization
        public static Object readEvent(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "read-char-exclusive", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadCharExclusive extends ELispBuiltInBaseNode {
        @Specialization
        public static Object readCharExclusive(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-load-suffixes", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGetLoadSuffixes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getLoadSuffixes() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "load", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FLoad extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean load(ELispString file, boolean noError, boolean noMessage,
                                   boolean noSuffix, boolean mustSuffix) {
            Object loadPath = ELispGlobals.loadPath.getValue();
            if (ELispSymbol.isNil(loadPath)) {
                return false;
            }
            String stem = file.toString();
            for (Object path : ((ELispCons) loadPath)) {
                Path directory = Path.of(((ELispString) path).toString());
                Path target = directory.resolve(stem + ".elc");
                if (!target.toFile().isFile()) {
                    target = directory.resolve(stem + ".el");
                }
                if (target.toFile().isFile()) {
                    try {
                        System.out.println("load: " + target);
                        ELispParser parser = new ELispParser(Source.newBuilder(
                                "elisp",
                                new FileReader(target.toFile()),
                                target.toFile().getName()
                        ).build());
                        try (var _ = ELispBindingScope.withLexicalBinding(parser.getLexicalBinding())) {
                            while (parser.hasNext()) {
                                Object lisp = parser.nextLisp();
                                evalSub(lisp);
                            }
                        }
                        return true;
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            return false;
        }
    }

    @ELispBuiltIn(name = "locate-file-internal", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FLocateFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object locateFileInternal(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eval-buffer", minArgs = 0, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FEvalBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object evalBuffer(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "eval-region", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FEvalRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object evalRegion(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "read", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRead extends ELispBuiltInBaseNode {
        @Specialization
        public static Object read(Object a) {
            if (ELispSymbol.isNil(a)) {
                // TODO: Vstandard_input
                throw new UnsupportedOperationException();
            }
            if (ELispSymbol.isT(a)) {
                a = READ_CHAR;
            }
            if (a == READ_CHAR) {
                throw new UnsupportedOperationException();
            }
            return readInternalStart(a, NIL, NIL, false);
        }
    }

    public static Object readInternalStart(
            Object stream, Object start, Object end, boolean locateSymbols
    ) {
        // TODO: Handle stream instanceof ELispBuffer, ELispCons
        ELispString s = (ELispString) stream;
        Source source = Source.newBuilder(ELispLanguage.ID, s.toString(), null).build();
        try {
            return ELispParser.read(source);
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @ELispBuiltIn(name = "read-positioning-symbols", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReadPositioningSymbols extends ELispBuiltInBaseNode {
        @Specialization
        public static Object readPositioningSymbols(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "read-from-string", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadFromString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object readFromString(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "lread--substitute-object-in-subtree", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FLreadSubstituteObjectInSubtree extends ELispBuiltInBaseNode {
        @Specialization
        public static Object lreadSubstituteObjectInSubtree(Object obj, Object placeholder, Object recursive) {
            new SubstituteObjectRecurse(
                    obj,
                    placeholder,
                    recursive instanceof ELispHashtable t ? t : null,
                    recursive instanceof ELispHashtable ? null : new HashSet<>()
            ).substitute(obj);
            return NIL; // return Qnil;
        }
    }

    /**
     * See {@code substitute_object_recurse} in {@code src/lread.c}
     */
    public record SubstituteObjectRecurse(
            Object object,
            Object placeholder,
            @Nullable ELispHashtable recursive,
            @Nullable HashSet<Object> seen
    ) {
        public Object substitute(Object tree) {
            if (tree == placeholder) {
                return object;
            }
            return switch (tree) {
                case Long _, Double _, ELispBigNum _, ELispSymbol _ -> tree;
                case ELispString s when s.intervals() == 0 -> tree;
                case ELispBoolVector _ -> tree;
                default -> {
                    if (recursive == null) {
                        if (seen != null && seen.contains(tree)) {
                            yield tree;
                        }
                    } else if (recursive.containsKey(tree)) {
                        yield tree;
                    }
                    yield switch (tree) {
                        case ELispCons cons -> {
                            cons.setCar(substitute(cons.car()));
                            cons.setCdr(substitute(cons.cdr()));
                            yield cons;
                        }
                        case ELispString s -> {
                            s.forProperties(this::substitute);
                            yield s;
                        }
                        case ELispVectorLike<?> vec -> {
                            // TODO: CHAR_TABLE_P, SUB_CHAR_TABLE_P, CLOSUREP, HASH_TABLE_P, RECORD P
                            for (int i = 0; i < vec.size(); i++) {
                                vec.setUntyped(i, substitute(vec.get(i)));
                            }
                            yield this;
                        }
                        default -> tree;
                    };
                }
            };
        }
    }

    @ELispBuiltIn(name = "intern", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FIntern extends ELispBuiltInBaseNode {
        @Specialization
        public static Object intern(ELispString a, Object obarray) {
            if (!ELispSymbol.isNil(obarray)) {
                throw new UnsupportedOperationException();
            }
            return ELispContext.intern(a.toString());
        }
    }

    @ELispBuiltIn(name = "intern-soft", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternSoft extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internSoft(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unintern", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FUnintern extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unintern(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "obarray-make", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObarrayMake extends ELispBuiltInBaseNode {
        @Specialization
        public static Object obarrayMake(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "obarrayp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObarrayp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object obarrayp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "obarray-clear", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObarrayClear extends ELispBuiltInBaseNode {
        @Specialization
        public static Object obarrayClear(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "mapatoms", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapatoms extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapatoms(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--obarray-buckets", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalObarrayBuckets extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalObarrayBuckets(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
