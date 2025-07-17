package party.iroiro.juicemacs.elisp;

import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.FileSystem;
import org.graalvm.polyglot.io.IOAccess;

import java.io.*;
import java.nio.file.Path;

public abstract class TestingUtils {
    private TestingUtils() {}

    public static PrintStream createOut(File file) throws FileNotFoundException {
        return createOut(new FileOutputStream(file));
    }

    public static PrintStream createOut(OutputStream stream) {
        return new PrintStream(new OutputStream() {
            final OutputStream out = stream;

            @Override
            public void write(int b) throws IOException {
                out.write(b);
                System.out.write(b);
            }

            @Override
            public void flush() throws IOException {
                out.flush();
                System.out.flush();
            }

            @Override
            public void close() throws IOException {
                out.close();
            }
        });
    }

    /// Returns a Truffle filesystem with working directory set correctly
    ///
    /// A correctly set PWD allows IDEA to click-to-jump to source location.
    static FileSystem getFileSystem() {
        FileSystem fileSystem = FileSystem.newDefaultFileSystem();
        fileSystem.setCurrentWorkingDirectory(Path.of("..").toAbsolutePath()); // project root dir
        return fileSystem;
    }

    public static IOAccess getTestIOAccess() {
        return IOAccess.newBuilder().fileSystem(getFileSystem()).build();
    }

    public static Context.Builder getContextBuilder(@Nullable PrintStream out) {
        String loadPath = Path.of("emacs", "lisp").toAbsolutePath().toString();
        String dataPath = Path.of("emacs", "etc").toAbsolutePath().toString();
        Context.Builder builder = Context.newBuilder("elisp")
                .allowExperimentalOptions(true)
                // Uncomment the following two lines to use an external debugger for Lisp before we get to edebug.
//                .option("elisp.truffleDebug", "true")
//                .option("inspect", "4242")
                // Uncomment the following two lines to adjust compilation settings.
                // Basically:
                // - engine.Compilation=false: Debug Java code,
                // - engine.Compilation=true && engine.CompilationFailureAction=Diagnose: Debug JIT compilation.
//                .option("engine.Compilation", "false")
//                .option("engine.CompilationFailureAction", "Diagnose")
//                .option("engine.TraceCompilation", "true")
//                .option("engine.TraceInlining", "true")
                .environment("EMACSLOADPATH", loadPath)
                .environment("EMACSDATA", dataPath)
                .allowIO(getTestIOAccess());
        if (out != null) {
            builder.out(out);
        }
        return builder;
    }
}
