package party.iroiro.juicemacs.elisp.benchmarks;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.jupiter.api.Test;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;
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
            exportBenchmarkOutput(out, byteCompile);
        }
    }

    private void exportBenchmarkOutput(ByteArrayOutputStream out, boolean byteCompile) throws IOException {
        String output = out.toString(StandardCharsets.UTF_8);
        String heading = "* Results";
        int start = output.lastIndexOf(heading);
        assertNotEquals(-1, start);
        start += heading.length();
        String table = output.substring(start).trim();
        String[] rows = table.split("\n");
        assertEquals("|test|non-gc avg (s)|gc avg (s)|gcs avg|tot avg (s)|tot avg err (s)", rows[0].trim());

        List<BenchmarkCase> cases = new ArrayList<>();
        for (int i = 1; i < rows.length; i++) {
            String row = rows[i].trim();
            if (row.equals("|-")) {
                continue;
            }
            String[] columns = row.split("\\|");
            assertEquals("", columns[0]);
            String name = columns[1].trim();
            double nonGcAvg = Double.parseDouble(columns[2].trim());
            double gcAvg = Double.parseDouble(columns[3].trim());
            int gcs = Integer.parseInt(columns[4].trim());
            double totAvg = Double.parseDouble(columns[5].trim());
            double totAvgErr = Double.parseDouble(columns[6].trim());
            cases.add(new BenchmarkCase(name, nonGcAvg, gcAvg, gcs, totAvg, totAvgErr));
        }

        String variant = byteCompile ? "elc" : "el";
        Path jsonPath = Paths.get(
                "build", "reports", "elisp",
                "benchmarks-" + variant + ".json"
        );
        assertDoesNotThrow(() -> jsonPath.getParent().toFile().mkdirs());
        try (BufferedWriter writer = Files.newBufferedWriter(jsonPath)) {
            writer.write("[\n");
            for (int i = 0; i < cases.size(); i++) {
                BenchmarkCase bench = cases.get(i);
                writer.write("""
                          {
                            "name": "%s-%s",
                            "unit": "s",
                            "value": %f
                          }%s
                        """.formatted(
                        bench.name, variant,
                        bench.totAvg,
                        i == cases.size() - 1 ? "" : ","
                ));
            }
            writer.write("]\n");
        }
    }

    record BenchmarkCase(String name, double nonGcAvg, double gcAvg, int gcs, double totAvg, double totAvgErr) {
    }
}
