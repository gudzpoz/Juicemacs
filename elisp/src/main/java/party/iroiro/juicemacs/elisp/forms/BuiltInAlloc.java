package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInAlloc extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInAllocFactory.getFactories();
    }

    @ELispBuiltIn(name = "make-string", minArgs = 2, maxArgs = 3, doc = "Return a newly created string of length LENGTH, with INIT in each element.\nLENGTH must be an integer.\nINIT must be an integer that represents a character.\nIf optional argument MULTIBYTE is non-nil, the result will be\na multibyte string even if INIT is an ASCII character.")
    @GenerateNodeFactory
    public abstract static class FMakeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeString(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-bool-vector", minArgs = 2, maxArgs = 2, doc = "Return a new bool-vector of length LENGTH, using INIT for each element.\nLENGTH must be a number.  INIT matters only in whether it is t or nil.")
    @GenerateNodeFactory
    public abstract static class FMakeBoolVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeBoolVector(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bool-vector", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return a new bool-vector with specified arguments as elements.\nAllows any number of arguments, including zero.\nusage: (bool-vector &rest OBJECTS)")
    @GenerateNodeFactory
    public abstract static class FBoolVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object boolVector(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "cons", minArgs = 2, maxArgs = 2, doc = "Create a new cons, give it CAR and CDR as components, and return it.")
    @GenerateNodeFactory
    public abstract static class FCons extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cons(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "list", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return a newly created list with specified arguments as elements.\nAllows any number of arguments, including zero.\nusage: (list &rest OBJECTS)")
    @GenerateNodeFactory
    public abstract static class FList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object list(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-list", minArgs = 2, maxArgs = 2, doc = "Return a newly created list of length LENGTH, with each element being INIT.")
    @GenerateNodeFactory
    public abstract static class FMakeList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeList(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-record", minArgs = 3, maxArgs = 3, doc = "Create a new record.\nTYPE is its type as returned by `type-of'; it should be either a\nsymbol or a type descriptor.  SLOTS is the number of non-type slots,\neach initialized to INIT.")
    @GenerateNodeFactory
    public abstract static class FMakeRecord extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeRecord(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "record", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Create a new record.\nTYPE is its type as returned by `type-of'; it should be either a\nsymbol or a type descriptor.  SLOTS is used to initialize the record\nslots with shallow copies of the arguments.\nusage: (record TYPE &rest SLOTS)")
    @GenerateNodeFactory
    public abstract static class FRecord extends ELispBuiltInBaseNode {
        @Specialization
        public static Object record(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-vector", minArgs = 2, maxArgs = 2, doc = "Return a newly created vector of length LENGTH, with each element being INIT.\nSee also the function `vector'.")
    @GenerateNodeFactory
    public abstract static class FMakeVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeVector(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "vector", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Return a newly created vector with specified arguments as elements.\nAllows any number of arguments, including zero.\nusage: (vector &rest OBJECTS)")
    @GenerateNodeFactory
    public abstract static class FVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Object vector(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-byte-code", minArgs = 4, maxArgs = 4, varArgs = true, doc = "Create a byte-code object with specified arguments as elements.\nThe arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant\nvector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,\nand (optional) INTERACTIVE-SPEC.\nThe first four arguments are required; at most six have any\nsignificance.\nThe ARGLIST can be either like the one of `lambda', in which case the arguments\nwill be dynamically bound before executing the byte code, or it can be an\ninteger of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the\nminimum number of arguments, the 7-bit NNNNNNN specifies the maximum number\nof arguments (ignoring &rest) and the R bit specifies whether there is a &rest\nargument to catch the left-over arguments.  If such an integer is used, the\narguments will not be dynamically bound but will be instead pushed on the\nstack before executing the byte-code.\nusage: (make-byte-code ARGLIST BYTE-CODE CONSTANTS DEPTH &optional DOCSTRING INTERACTIVE-SPEC &rest ELEMENTS)")
    @GenerateNodeFactory
    public abstract static class FMakeByteCode extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeByteCode(Object a, Object b, Object c, Object d, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-closure", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Create a byte-code closure from PROTOTYPE and CLOSURE-VARS.\nReturn a copy of PROTOTYPE, a byte-code object, with CLOSURE-VARS\nreplacing the elements in the beginning of the constant-vector.\nusage: (make-closure PROTOTYPE &rest CLOSURE-VARS)")
    @GenerateNodeFactory
    public abstract static class FMakeClosure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeClosure(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-symbol", minArgs = 1, maxArgs = 1, doc = "Return a newly allocated uninterned symbol whose name is NAME.\nIts value is void, and its function definition and property list are nil.")
    @GenerateNodeFactory
    public abstract static class FMakeSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeSymbol(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-marker", minArgs = 0, maxArgs = 0, doc = "Return a newly allocated marker which does not point at any place.")
    @GenerateNodeFactory
    public abstract static class FMakeMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeMarker() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-finalizer", minArgs = 1, maxArgs = 1, doc = "Make a finalizer that will run FUNCTION.\nFUNCTION will be called after garbage collection when the returned\nfinalizer object becomes unreachable.  If the finalizer object is\nreachable only through references from finalizer objects, it does not\ncount as reachable for the purpose of deciding whether to run\nFUNCTION.  FUNCTION will be run once per finalizer object.")
    @GenerateNodeFactory
    public abstract static class FMakeFinalizer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeFinalizer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "purecopy", minArgs = 1, maxArgs = 1, doc = "Make a copy of object OBJ in pure storage.\nRecursively copies contents of vectors and cons cells.\nDoes not copy symbols.  Copies strings without text properties.")
    @GenerateNodeFactory
    public abstract static class FPurecopy extends ELispBuiltInBaseNode {
        @Specialization
        public static Object purecopy(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "garbage-collect", minArgs = 0, maxArgs = 0, doc = "Reclaim storage for Lisp objects no longer needed.\nGarbage collection happens automatically if you cons more than\n`gc-cons-threshold' bytes of Lisp data since previous garbage collection.\n`garbage-collect' normally returns a list with info on amount of space in use,\nwhere each entry has the form (NAME SIZE USED FREE), where:\n- NAME is a symbol describing the kind of objects this entry represents,\n- SIZE is the number of bytes used by each one,\n- USED is the number of those objects that were found live in the heap,\n- FREE is the number of those objects that are not live but that Emacs\n  keeps around for future allocations (maybe because it does not know how\n  to return them to the OS).\n\nHowever, if there was overflow in pure space, and Emacs was dumped\nusing the \\\"unexec\\\" method, `garbage-collect' returns nil, because\nreal GC can't be done.\n\nNote that calling this function does not guarantee that absolutely all\nunreachable objects will be garbage-collected.  Emacs uses a\nmark-and-sweep garbage collector, but is conservative when it comes to\ncollecting objects in some circumstances.\n\nFor further details, see Info node `(elisp)Garbage Collection'.")
    @GenerateNodeFactory
    public abstract static class FGarbageCollect extends ELispBuiltInBaseNode {
        @Specialization
        public static Object garbageCollect() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "garbage-collect-maybe", minArgs = 1, maxArgs = 1, doc = "Call `garbage-collect' if enough allocation happened.\nFACTOR determines what \"enough\" means here:\nIf FACTOR is a positive number N, it means to run GC if more than\n1/Nth of the allocations needed to trigger automatic allocation took\nplace.\nTherefore, as N gets higher, this is more likely to perform a GC.\nReturns non-nil if GC happened, and nil otherwise.")
    @GenerateNodeFactory
    public abstract static class FGarbageCollectMaybe extends ELispBuiltInBaseNode {
        @Specialization
        public static Object garbageCollectMaybe(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "memory-info", minArgs = 0, maxArgs = 0, doc = "Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP).\nAll values are in Kbytes.  If there is no swap space,\nlast two values are zero.  If the system is not supported\nor memory information can't be obtained, return nil.\nIf `default-directory' is remote, return memory information of the\nrespective remote host.")
    @GenerateNodeFactory
    public abstract static class FMemoryInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memoryInfo() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "memory-use-counts", minArgs = 0, maxArgs = 0, doc = "Return a list of counters that measure how much consing there has been.\nEach of these counters increments for a certain kind of object.\nThe counters wrap around from the largest positive integer to zero.\nGarbage collection does not decrease them.\nThe elements of the value are as follows:\n  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)\nAll are in units of 1 = one object consed\nexcept for VECTOR-CELLS and STRING-CHARS, which count the total length of\nobjects consed.\nFrames, windows, buffers, and subprocesses count as vectors\n  (but the contents of a buffer's text do not count here).")
    @GenerateNodeFactory
    public abstract static class FMemoryUseCounts extends ELispBuiltInBaseNode {
        @Specialization
        public static Object memoryUseCounts() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "malloc-info", minArgs = 0, maxArgs = 0, doc = "Report malloc information to stderr.\nThis function outputs to stderr an XML-formatted\ndescription of the current state of the memory-allocation\narenas.")
    @GenerateNodeFactory
    public abstract static class FMallocInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mallocInfo() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "malloc-trim", minArgs = 0, maxArgs = 1, doc = "Release free heap memory to the OS.\nThis function asks libc to return unused heap memory back to the operating\nsystem.  This function isn't guaranteed to do anything, and is mainly\nmeant as a debugging tool.\n\nIf LEAVE_PADDING is given, ask the system to leave that much unused\nspace in the heap of the Emacs process.  This should be an integer, and if\nnot given, it defaults to 0.\n\nThis function returns nil if no memory could be returned to the\nsystem, and non-nil if some memory could be returned.")
    @GenerateNodeFactory
    public abstract static class FMallocTrim extends ELispBuiltInBaseNode {
        @Specialization
        public static Object mallocTrim(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
