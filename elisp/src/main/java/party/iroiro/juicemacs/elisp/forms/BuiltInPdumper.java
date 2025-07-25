package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.pdump.ELispPortableDumper;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;

public class BuiltInPdumper extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInPdumperFactory.getFactories();
    }

    /**
     * <pre>
     * Dump current state of Emacs into dump file FILENAME.
     * If TRACK-REFERRERS is non-nil, keep additional debugging information
     * that can help track down the provenance of unsupported object
     * types.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-emacs-portable", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpEmacsPortable extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public boolean dumpEmacsPortable(ELispString filename, Object trackReferrers) {
            ELispContext context = getContext();
            Path path = BuiltInFileIO.FExpandFileName.expandFileNamePath(filename, false);
            TruffleFile file = context.truffleEnv().getPublicTruffleFile(path.toString());
            try (OutputStream output = file.newOutputStream(StandardOpenOption.CREATE)) {
                ELispPortableDumper.serializeFromContext(output, context);
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, filename);
            }
            return true;
        }
    }

    /**
     * <pre>
     * Internal relocation sorting function.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-emacs-portable--sort-predicate", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpEmacsPortableSortPredicate extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpEmacsPortableSortPredicate(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal relocation sorting function.
     * </pre>
     */
    @ELispBuiltIn(name = "dump-emacs-portable--sort-predicate-copied", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDumpEmacsPortableSortPredicateCopied extends ELispBuiltInBaseNode {
        @Specialization
        public static Void dumpEmacsPortableSortPredicateCopied(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return statistics about portable dumping used by this session.
     * If this Emacs session was started from a dump file,
     * the return value is an alist of the form:
     *
     *   ((dumped-with-pdumper . t) (load-time . TIME) (dump-file-name . FILE))
     *
     * where TIME is the time in seconds it took to restore Emacs state
     * from the dump file, and FILE is the name of the dump file.
     * Value is nil if this session was not started using a dump file.
     * </pre>
     */
    @ELispBuiltIn(name = "pdumper-stats", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FPdumperStats extends ELispBuiltInBaseNode {
        @Specialization
        public static Void pdumperStats() {
            throw new UnsupportedOperationException();
        }
    }
}
