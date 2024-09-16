package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInCharSet extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCharSetFactory.getFactories();
    }

    @ELispBuiltIn(name = "charsetp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "map-charset-chars", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMapCharsetChars extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mapCharsetChars(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "define-charset-internal", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FDefineCharsetInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defineCharsetInternal(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "define-charset-alias", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDefineCharsetAlias extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defineCharsetAlias(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-plist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetPlist(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-charset-plist", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetCharsetPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharsetPlist(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unify-charset", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FUnifyCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unifyCharset(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-unused-iso-final-char", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetUnusedIsoFinalChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getUnusedIsoFinalChar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "declare-equiv-charset", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FDeclareEquivCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object declareEquivCharset(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "find-charset-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFindCharsetRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findCharsetRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "find-charset-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFindCharsetString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findCharsetString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "decode-char", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDecodeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object decodeChar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "encode-char", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FEncodeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object encodeChar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-char", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMakeChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeChar(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "split-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSplitChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object splitChar(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "char-charset", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCharCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charCharset(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-after", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetAfter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetAfter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "iso-charset", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FIsoCharset extends ELispBuiltInBaseNode {
        @Specialization
        public static Object isoCharset(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clear-charset-maps", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FClearCharsetMaps extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clearCharsetMaps() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-priority-list", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetPriorityList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetPriorityList(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-charset-priority", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FSetCharsetPriority extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setCharsetPriority(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "charset-id-internal", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCharsetIdInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object charsetIdInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "sort-charsets", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSortCharsets extends ELispBuiltInBaseNode {
        @Specialization
        public static Object sortCharsets(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
