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
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
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
    public void testELispBenchmarks() throws IOException {
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
                    (defalias 'comp-el-to-eln-filename nil)
                    ;; Do not use org-mode for output
                    (defun pop-to-buffer (&rest _))
                    (defun org-table-align())
                    """);
            String[] benchmarks = new String[]{
                    "bubble-no-cons",
                    "bubble",
                    // "dhrystone",
                    // "elb-bytecomp",
                    // "elb-eieio",
                    "elb-pcase",
                    // "elb-scroll",
                    // "elb-smie",
                    "fibn",
                    "flet",
                    "inclist-type-hints",
                    "inclist",
                    // "listlen-tc",
                    "map-closure",
                    "nbody",
                    // "pack-unpack",
                    "pidigits",
            };
            String selector = String.join("\\\\|", benchmarks);
            String benchmarkRun = "(elisp-benchmarks-run \"" + selector + "\")";
            System.out.println(benchmarkRun);
            context.eval("elisp", """
                    (setq load-suffixes '(".el"))""");
            context.eval("elisp", benchmarkRun);
        }
    }
}
