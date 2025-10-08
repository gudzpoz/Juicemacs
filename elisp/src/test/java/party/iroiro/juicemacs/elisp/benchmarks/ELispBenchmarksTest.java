package party.iroiro.juicemacs.elisp.benchmarks;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.fail;
import static party.iroiro.juicemacs.elisp.TestingUtils.createOut;
import static party.iroiro.juicemacs.elisp.TestingUtils.getContextBuilder;

/// Benchmarking with [`elisp-benchmarks`](https://elpa.gnu.org/packages/elisp-benchmarks.html)
///
/// ## Preparing
///
/// I made several changes to the benchmarking suite to better
/// reflect the performance characteristics of GNU Emacs and Juicemacs.
/// Please copy the `.el` files under the `test/resources` folder to
/// your `elisp-benchmarks` installation.
///
/// ## Running
///
/// To run the Emacs counter-part, run:
///
/// ```
/// emacs -nw --batch -l ~/.emacs.d/init.el --eval '(elisp-benchmark-run ...)'
/// ```
///
/// The required `(elisp-benchmark-run ...)` expression will be printed by this test.
public class ELispBenchmarksTest {
    private static void executeIgnoreError(Context context, String code) {
        try {
            context.eval("elisp", code);
        } catch (PolyglotException e) {
            System.err.println(e.getMessage());
        }
    }

    private static Path findELispBenchmarksDir() {
        String home = System.getProperty("user.home");
        Path glob = Path.of(home, ".emacs.d");
        try {
            Path[] found = new Path[1];
            Files.walkFileTree(glob, new SimpleFileVisitor<>() {
                @SuppressWarnings("NullableProblems")
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (file.getFileName().toString().equals("elisp-benchmarks.el")) {
                        found[0] = file;
                        return FileVisitResult.TERMINATE;
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
            return Objects.requireNonNull(found[0]).getParent().toAbsolutePath();
        } catch (IOException e) {
            fail("please install elisp-benchmarks first", e);
            throw new RuntimeException(e);
        }
    }

    @Test
    public void testELispAstBenchmarks() throws IOException {
        testELispBenchmarks(false);
    }

    @Test
    public void testELispBytecodeBenchmarks() throws IOException {
        testELispBenchmarks(true);
    }

    public void testELispBenchmarks(boolean byteCompile) throws IOException {
        try (ByteArrayOutputStream out = new ByteArrayOutputStream();
             Context context = getContextBuilder(createOut(out))
                     .option("elisp.dumpFile", "emacs.pdmp")
                     .build()) {
            executeIgnoreError(context, "(eval top-level)");
            context.eval(
                    "elisp",
                    "(add-to-list 'load-path (expand-file-name \""
                            + findELispBenchmarksDir().toString().replace("\\", "\\\\")
                            + "\"))"
            );
            context.eval("elisp", """
                    ;; Placeholders for unimplemented functions/semantics
                    (setq noninteractive t)
                    (defun commandp (_) nil)
                    (defun macroexp-file-name () load-file-name)
                    (defalias 'copy-syntax-table #'identity)
                    """);
            executeIgnoreError(context, "(require 'elisp-benchmarks)");
            context.eval("elisp", """
                    (require 'elisp-benchmarks)
                    
                    ;; Do not native-compile
                    (defalias 'native-compile #'identity)
                    """);
            context.eval("elisp", """
                    (defalias 'comp-el-to-eln-filename nil)
                    ;; Do not use org-mode for output
                    (defun pop-to-buffer (&rest _))
                    (defun org-table-align())
                    """);
            String[] benchmarks = new String[]{
                    "bubble-no-cons",
                    "bubble",
                    "dhrystone",
                    // "elb-bytecomp",
                    // "elb-eieio",
                    "elb-pcase",
                    // "elb-scroll",
                    // "elb-smie",
                    // > The fibn benchmarks arguably do not test anything besides pure function removal.
                    // > To test basic arithmetic and function call costs, you are advised to modify fibn.el
                    // > and make the input arguments non-constant (by introducing a global variable).
                    "fibn",
                    "flet",
                    "inclist-type-hints",
                    "inclist",
                    // "listlen-tc",
                    // > The elisp-benchmarks package does not provide the mandelbrot benchmark.
                    // > To run this, copy resources/mandelbrot.el into elisp-benchmarks/benchmarks,
                    // > byte-compile the file before running the benchmarks.
                    "mandelbrot",
                    "map-closure",
                    "nbody",
                    // "pack-unpack",
                    "pidigits",
            };
            String selector = String.join("\\\\|", benchmarks);
            String benchmarkRun = "(elisp-benchmarks-run \"" + selector + "\")";
            System.out.println(benchmarkRun);
            if (!byteCompile) {
                context.eval("elisp", """
                        (setq load-suffixes '(".el"))""");
            }
            context.eval("elisp", benchmarkRun);
        }
    }
}
