package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

import java.util.List;

public class BuiltInDoc extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInDocFactory.getFactories();
    }

    /**
     * <pre>
     * Return non-nil if OBJECT is a well-formed docstring object.
     * OBJECT can be either a string or a reference if it's kept externally.
     * </pre>
     */
    @ELispBuiltIn(name = "documentation-stringp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDocumentationStringp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean documentationStringp(Object object) {
            return BuiltInData.FStringp.stringp(object)
                    || object instanceof Long
                    || (
                            object instanceof ELispCons cons
                                    && BuiltInData.FStringp.stringp(cons.car())
                                    && cons.cdr() instanceof Long
            );
        }
    }

    /**
     * <pre>
     * Return the documentation string of FUNCTION.
     * Unless a non-nil second argument RAW is given, the
     * string is passed through `substitute-command-keys'.
     * </pre>
     */
    @ELispBuiltIn(name = "documentation", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDocumentation extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean documentation(Object function, Object raw) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the raw documentation info of a C primitive.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-subr-documentation", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubrDocumentation extends ELispBuiltInBaseNode {
        @Specialization
        public static Void subrDocumentation(Object function) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the documentation string that is SYMBOL's PROP property.
     * Third argument RAW omitted or nil means pass the result through
     * `substitute-command-keys' if it is a string.
     *
     * This differs from `get' in that it can refer to strings stored in the
     * `etc/DOC' file; and that it evaluates documentation properties that
     * aren't strings.
     * </pre>
     */
    @ELispBuiltIn(name = "documentation-property", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDocumentationProperty extends ELispBuiltInBaseNode {
        @Specialization
        public static Void documentationProperty(Object symbol, Object prop, Object raw) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Used during Emacs initialization to scan the `etc/DOC...' file.
     * This searches the `etc/DOC...' file for doc strings and
     * records them in function and variable definitions.
     * The function takes one argument, FILENAME, a string;
     * it specifies the file name (without a directory) of the DOC file.
     * That file is found in `../etc' now; later, when the dumped Emacs is run,
     * the same file name is found in the `doc-directory'.
     * </pre>
     */
    @ELispBuiltIn(name = "Snarf-documentation", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSnarfDocumentation extends ELispBuiltInBaseNode {
        @Specialization
        public boolean snarfDocumentation(Object filename) {
            BuiltInEditFns.FMessage.message(getContext(), "Snarf-documentation not implemented");
            return false;
        }
    }

    /**
     * <pre>
     * Return the current effective text quoting style.
     * If the variable `text-quoting-style' is `grave', `straight' or
     * `curve', just return that value.  If it is nil (the default), return
     * `grave' if curved quotes cannot be displayed (for instance, on a
     * terminal with no support for these characters), otherwise return
     * `curve'.  Any other value is treated as `curve'.
     *
     * Note that in contrast to the variable `text-quoting-style', this
     * function will never return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "text-quoting-style", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FTextQuotingStyle extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean textQuotingStyle() {
            // TODO
            return false;
        }
    }
}
