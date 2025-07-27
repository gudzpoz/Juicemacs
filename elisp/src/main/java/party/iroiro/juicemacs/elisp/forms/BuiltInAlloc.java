package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CLOSURE_CONSTANTS;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.BYTE_CODE_FUNCTION_P;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInAlloc extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInAllocFactory.getFactories();
    }

    /**
     * <pre>
     * Return a newly created string of length LENGTH, with INIT in each element.
     * LENGTH must be an integer.
     * INIT must be an integer that represents a character.
     * If optional argument MULTIBYTE is non-nil, the result will be
     * a multibyte string even if INIT is an ASCII character.
     * </pre>
     */
    @ELispBuiltIn(name = "make-string", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMakeString extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString makeString(long length, Object init, boolean multibyte) {
            if (length <= 0) {
                return new ELispString("");
            }
            int c = asChar(init);
            if (length > Integer.MAX_VALUE || multibyte) {
                throw new UnsupportedOperationException();
            }
            if (c <= 0xFF) {
                byte[] bytes = new byte[(int) length];
                Arrays.fill(bytes, (byte) c);
                return new ELispString(MuleString.fromLatin1(bytes));
            }
            MuleStringBuffer buffer = new MuleStringBuffer();
            for (int i = 0; i < length; i++) {
                buffer.appendCodePoint(c);
            }
            return new ELispString(buffer.build());
        }
    }

    /**
     * <pre>
     * Return a new bool-vector of length LENGTH, using INIT for each element.
     * LENGTH must be a number.  INIT matters only in whether it is t or nil.
     * </pre>
     */
    @ELispBuiltIn(name = "make-bool-vector", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeBoolVector extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispBoolVector makeBoolVector(long length, Object init) {
            int len = Math.toIntExact(length);
            long[] bits = new long[Math.ceilDiv(len, 64)];
            if (!isNil(init)) {
                Arrays.fill(bits, -1L);
            }
            return new ELispBoolVector(bits, len);
        }
    }

    /**
     * <pre>
     * Return a new bool-vector with specified arguments as elements.
     * Allows any number of arguments, including zero.
     * usage: (bool-vector &amp;rest OBJECTS)
     * </pre>
     */
    @ELispBuiltIn(name = "bool-vector", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FBoolVector extends ELispBuiltInBaseNode {
        @Specialization
        public static Void boolVector(Object[] objects) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create a new cons, give it CAR and CDR as components, and return it.
     * </pre>
     */
    @ELispBuiltIn(name = "cons", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCons extends ELispBuiltInBaseNode {
        @Specialization
        public static Object cons(Object car, Object cdr) {
            return ELispCons.cons(car, cdr);
        }
    }

    /**
     * <pre>
     * Return a newly created list with specified arguments as elements.
     * Allows any number of arguments, including zero.
     * usage: (list &amp;rest OBJECTS)
     * </pre>
     */
    @ELispBuiltIn(name = "list", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object list(Object[] objects) {
            return ELispCons.listOf(objects);
        }
    }

    /**
     * <pre>
     * Return a newly created list of length LENGTH, with each element being INIT.
     * </pre>
     */
    @ELispBuiltIn(name = "make-list", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeList(long length, Object init) {
            Object[] objects = new Object[Math.toIntExact(length)];
            Arrays.fill(objects, init);
            return ELispCons.listOf(objects);
        }
    }

    /**
     * <pre>
     * Create a new record.
     * TYPE is its type as returned by `type-of'; it should be either a
     * symbol or a type descriptor.  SLOTS is the number of non-type slots,
     * each initialized to INIT.
     * </pre>
     */
    @ELispBuiltIn(name = "make-record", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMakeRecord extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispRecord makeRecord(Object type, long slots, Object init) {
            Object[] inner = new Object[Math.toIntExact(slots + 1)];
            inner[0] = type;
            Arrays.fill(inner, 1, inner.length, init);
            return new ELispRecord(inner);
        }
    }

    /**
     * <pre>
     * Create a new record.
     * TYPE is its type as returned by `type-of'; it should be either a
     * symbol or a type descriptor.  SLOTS is used to initialize the record
     * slots with shallow copies of the arguments.
     * usage: (record TYPE &amp;rest SLOTS)
     * </pre>
     */
    @ELispBuiltIn(name = "record", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FRecord extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispRecord record(Object type, Object[] slots) {
            Object[] inner = new Object[slots.length + 1];
            System.arraycopy(slots, 0, inner, 1, slots.length);
            inner[0] = type;
            return new ELispRecord(inner);
        }
    }

    /**
     * <pre>
     * Return a newly created vector of length LENGTH, with each element being INIT.
     * See also the function `vector'.
     * </pre>
     */
    @ELispBuiltIn(name = "make-vector", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMakeVector extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispVector makeVector(long length, Object init) {
            return new ELispVector(Collections.nCopies((int) length, init));
        }
    }

    /**
     * <pre>
     * Return a newly created vector with specified arguments as elements.
     * Allows any number of arguments, including zero.
     * usage: (vector &amp;rest OBJECTS)
     * </pre>
     */
    @ELispBuiltIn(name = "vector", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FVector extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispVector vector(Object[] objects) {
            return new ELispVector(Arrays.asList(objects));
        }
    }

    /**
     * <pre>
     * Create a byte-code object with specified arguments as elements.
     * The arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant
     * vector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,
     * and (optional) INTERACTIVE-SPEC.
     * The first four arguments are required; at most six have any
     * significance.
     * The ARGLIST can be either like the one of `lambda', in which case the arguments
     * will be dynamically bound before executing the byte code, or it can be an
     * integer of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the
     * minimum number of arguments, the 7-bit NNNNNNN specifies the maximum number
     * of arguments (ignoring &amp;rest) and the R bit specifies whether there is a &amp;rest
     * argument to catch the left-over arguments.  If such an integer is used, the
     * arguments will not be dynamically bound but will be instead pushed on the
     * stack before executing the byte-code.
     * usage: (make-byte-code ARGLIST BYTE-CODE CONSTANTS DEPTH &amp;optional DOCSTRING INTERACTIVE-SPEC &amp;rest ELEMENTS)
     * </pre>
     */
    @ELispBuiltIn(name = "make-byte-code", minArgs = 4, maxArgs = 4, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeByteCode extends ELispBuiltInBaseNode implements ELispBuiltInBaseNode.InlineFactory {
        @Specialization
        public static ELispBytecode makeByteCode(@Nullable Node node, Object arglist, Object byteCode, Object constants, Object depth, Object[] args) {
            ArrayList<Object> list = new ArrayList<>();
            list.addAll(List.of(arglist, byteCode, constants, depth));
            list.addAll(Arrays.asList(args));
            @Nullable SourceSection section = node == null ? null : node.getSourceSection();
            ELispBytecode bytecode = (ELispBytecode) AbstractELispClosure.create(list, new AbstractELispClosure.ClosureCommons());
            bytecode.setSourceSection(section);
            return bytecode;
        }
    }

    /**
     * <pre>
     * Create a byte-code closure from PROTOTYPE and CLOSURE-VARS.
     * Return a copy of PROTOTYPE, a byte-code object, with CLOSURE-VARS
     * replacing the elements in the beginning of the constant-vector.
     * usage: (make-closure PROTOTYPE &amp;rest CLOSURE-VARS)
     * </pre>
     */
    @ELispBuiltIn(name = "make-closure", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeClosure extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispBytecode makeClosure(Object prototype, Object[] closureVars) {
            if (!(prototype instanceof ELispBytecode bytecode)) {
                throw ELispSignals.wrongTypeArgument(BYTE_CODE_FUNCTION_P, prototype);
            }
            ELispVector constants = asVector(bytecode.get(CLOSURE_CONSTANTS));
            if (closureVars.length > constants.size()) {
                throw ELispSignals.error("closure vars do not fit into constvec");
            }
            ELispVector copyConstants = BuiltInFns.FCopySequence.copySequenceVector(constants);
            copyConstants.fillFrom(closureVars);
            ELispBytecode copy = (ELispBytecode) ELispBytecode.create(bytecode, bytecode.getCommons()); // NOPMD
            copy.setSourceSection(bytecode.getSourceSection());
            copy.set(CLOSURE_CONSTANTS, copyConstants);
            return copy;
        }
    }

    /**
     * <pre>
     * Return a newly allocated uninterned symbol whose name is NAME.
     * Its value is void, and its function definition and property list are nil.
     * </pre>
     */
    @ELispBuiltIn(name = "make-symbol", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeSymbol extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispSymbol makeSymbol(ELispString name) {
            return new ELispSymbol(name.value(), false);
        }
    }

    /**
     * <pre>
     * Return a newly allocated marker which does not point at any place.
     * </pre>
     */
    @ELispBuiltIn(name = "make-marker", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMakeMarker extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispMarker makeMarker() {
            return new ELispMarker();
        }
    }

    /**
     * <pre>
     * Make a finalizer that will run FUNCTION.
     * FUNCTION will be called after garbage collection when the returned
     * finalizer object becomes unreachable.  If the finalizer object is
     * reachable only through references from finalizer objects, it does not
     * count as reachable for the purpose of deciding whether to run
     * FUNCTION.  FUNCTION will be run once per finalizer object.
     * </pre>
     */
    @ELispBuiltIn(name = "make-finalizer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeFinalizer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeFinalizer(Object function) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make a copy of object OBJ in pure storage.
     * Recursively copies contents of vectors and cons cells.
     * Does not copy symbols.  Copies strings without text properties.
     * </pre>
     */
    @ELispBuiltIn(name = "purecopy", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPurecopy extends ELispBuiltInBaseNode {
        @Specialization
        public static Object purecopy(Object obj) {
            return obj;
        }
    }

    /**
     * <pre>
     * Reclaim storage for Lisp objects no longer needed.
     * Garbage collection happens automatically if you cons more than
     * `gc-cons-threshold' bytes of Lisp data since previous garbage collection.
     * `garbage-collect' normally returns a list with info on amount of space in use,
     * where each entry has the form (NAME SIZE USED FREE), where:
     * - NAME is a symbol describing the kind of objects this entry represents,
     * - SIZE is the number of bytes used by each one,
     * - USED is the number of those objects that were found live in the heap,
     * - FREE is the number of those objects that are not live but that Emacs
     *   keeps around for future allocations (maybe because it does not know how
     *   to return them to the OS).
     *
     * However, if there was overflow in pure space, and Emacs was dumped
     * using the \"unexec\" method, `garbage-collect' returns nil, because
     * real GC can't be done.
     *
     * Note that calling this function does not guarantee that absolutely all
     * unreachable objects will be garbage-collected.  Emacs uses a
     * mark-and-sweep garbage collector, but is conservative when it comes to
     * collecting objects in some circumstances.
     *
     * For further details, see Info node `(elisp)Garbage Collection'.
     * </pre>
     */
    @ELispBuiltIn(name = "garbage-collect", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGarbageCollect extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static ELispCons garbageCollect() {
            Runtime.getRuntime().gc();
            // TODO: Return info
            return ELispCons.listOf(false);
        }
    }

    /**
     * <pre>
     * Call `garbage-collect' if enough allocation happened.
     * FACTOR determines what "enough" means here:
     * If FACTOR is a positive number N, it means to run GC if more than
     * 1/Nth of the allocations needed to trigger automatic allocation took
     * place.
     * Therefore, as N gets higher, this is more likely to perform a GC.
     * Returns non-nil if GC happened, and nil otherwise.
     * </pre>
     */
    @ELispBuiltIn(name = "garbage-collect-maybe", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGarbageCollectMaybe extends ELispBuiltInBaseNode {
        @Specialization
        public static Void garbageCollectMaybe(Object factor) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP).
     * All values are in Kbytes.  If there is no swap space,
     * last two values are zero.  If the system is not supported
     * or memory information can't be obtained, return nil.
     * If `default-directory' is remote, return memory information of the
     * respective remote host.
     * </pre>
     */
    @ELispBuiltIn(name = "memory-info", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMemoryInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Void memoryInfo() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of counters that measure how much consing there has been.
     * Each of these counters increments for a certain kind of object.
     * The counters wrap around from the largest positive integer to zero.
     * Garbage collection does not decrease them.
     * The elements of the value are as follows:
     *   (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)
     * All are in units of 1 = one object consed
     * except for VECTOR-CELLS and STRING-CHARS, which count the total length of
     * objects consed.
     * Frames, windows, buffers, and subprocesses count as vectors
     *   (but the contents of a buffer's text do not count here).
     * </pre>
     */
    @ELispBuiltIn(name = "memory-use-counts", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMemoryUseCounts extends ELispBuiltInBaseNode {
        @Specialization
        public static Void memoryUseCounts() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Report malloc information to stderr.
     * This function outputs to stderr an XML-formatted
     * description of the current state of the memory-allocation
     * arenas.
     * </pre>
     */
    @ELispBuiltIn(name = "malloc-info", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FMallocInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mallocInfo() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Release free heap memory to the OS.
     * This function asks libc to return unused heap memory back to the operating
     * system.  This function isn't guaranteed to do anything, and is mainly
     * meant as a debugging tool.
     *
     * If LEAVE_PADDING is given, ask the system to leave that much unused
     * space in the heap of the Emacs process.  This should be an integer, and if
     * not given, it defaults to 0.
     *
     * This function returns nil if no memory could be returned to the
     * system, and non-nil if some memory could be returned.
     * </pre>
     */
    @ELispBuiltIn(name = "malloc-trim", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMallocTrim extends ELispBuiltInBaseNode {
        @Specialization
        public static Void mallocTrim(Object leavePadding) {
            throw new UnsupportedOperationException();
        }
    }
}
