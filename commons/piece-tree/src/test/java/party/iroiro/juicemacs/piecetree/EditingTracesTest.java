package party.iroiro.juicemacs.piecetree;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.strings.TruffleString;
import org.ahmadsoft.ropes.Rope;
import org.jspecify.annotations.Nullable;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.BiFunction;
import java.util.zip.GZIPInputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@Fork(1)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 3, time = 5)
public class EditingTracesTest {
    public final static String[] TRACES = {
            "automerge-paper.json.gz",
            "clownschool_flat.json.gz",
            "friendsforever_flat.json.gz",
            "json-crdt-blog-post.json.gz",
            "json-crdt-patch.json.gz",
            "rustcode.json.gz",
            "seph-blog1.json.gz",
            "sveltecomponent.json.gz",
    };

    public final static String PATH = "editing-traces/sequential_traces";

    private static String readFromGzResource(String file) {
        try (InputStream is = EditingTracesTest.class.getClassLoader().getResourceAsStream(PATH + "/" + file);
             GZIPInputStream gis = new GZIPInputStream(Objects.requireNonNull(is))) {
            return new String(gis.readAllBytes());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static Trace parseTrace(String json) {
        JSONObject root = new JSONObject(json);
        List<Edit> edits = new ArrayList<>();
        JSONArray transactions = root.getJSONArray("txns");
        for (Object transaction : transactions) {
            JSONObject obj = (JSONObject) transaction;
            for (Object patch : obj.getJSONArray("patches")) {
                JSONArray patchArr = (JSONArray) patch;
                edits.add(new Edit(patchArr.getInt(0), patchArr.getInt(1), patchArr.getString(2)));
            }
        }
        return new Trace(
                root.getString("startContent"),
                edits.toArray(Edit[]::new),
                root.getString("endContent")
        );
    }

    private static void applyGapEdit(PieceState state, Edit edit) {
        if (edit.numDeleted > 0) {
            state.gapBuffer.delete(edit.position, edit.numDeleted);
        }
        if (!edit.insertedContent.isEmpty()) {
            state.gapBuffer.insert(edit.position, edit.insertedContent);
        }
    }

    private static void applyEdit(Edit edit, PieceTreeBase state) {
        if (edit.numDeleted > 0) {
            state.delete(edit.position, edit.numDeleted);
        }
        if (!edit.insertedContent.isEmpty()) {
            state.insert(edit.position, fromString(edit.insertedContent));
        }
    }

    public static byte[] fromString(String s) {
        return s.getBytes(StandardCharsets.UTF_8);
    }

    private void testEntry(String name, BiFunction<PieceState, long[], @Nullable String> testRunner) throws IOException {
        File stats = File.createTempFile(name, ".csv");
        boolean delete = false;
        try (FileWriter writer = new FileWriter(stats)) {
            writer.append("trace,edit,latency\n");
            for (String file : TRACES) {
                PieceState state = new PieceState();
                state.file = file;
                state.setUp();
                long[] latency = new long[state.trace.edits.length];
                String result = testRunner.apply(state, latency);
                if (result == null) {
                    delete = true;
                } else {
                    assertEquals(state.trace.end, result);
                    for (int i = 0; i < latency.length; i++) {
                        writer
                                .append(file).append(",")
                                .append(String.valueOf(i)).append(",")
                                .append(String.valueOf(latency[i])).append("\n");
                    }
                }
            }
        }
        if (delete) {
            assertTrue(stats.delete());
        } else {
            System.out.println("Stats (" + name + ") (no warm up) at " + stats);
        }
    }

    @Test
    public void testTraces() throws IOException {
        testEntry("editing-traces-piece-table", (state, latency) -> {
            PieceTreeBase tree = new PieceTreeBase(false, fromString(state.trace.start));
            Edit[] edits = state.trace.edits;
            for (int i = 0; i < edits.length; i++) {
                Edit edit = edits[i];
                long start = System.nanoTime();
                applyEdit(edit, tree);
                long delta = System.nanoTime() - start;
                latency[i] = delta;
                if (delta > 1000_000) {
                    System.out.println("Slow edit #" + i + ": del=" + edit.numDeleted
                            + ", ins=" + edit.insertedContent.length() + " (" + ((double) delta) / 1000_000 + " ms)");
                }
            }
            return new String(tree.getLinesRawContent(), StandardCharsets.UTF_8);
        });
    }

    @Test
    public void testRopes() throws IOException {
        testEntry("editing-traces-ropes", (state, latency) -> {
            Rope rope = Rope.BUILDER.build(state.trace.start);
            for (int i = 0; i < latency.length; i++) {
                Edit edit = state.trace.edits[i];
                long start = System.nanoTime();
                if (edit.numDeleted > 0) {
                    rope = rope.delete(edit.position, edit.position + edit.numDeleted);
                }
                if (!edit.insertedContent.isEmpty()) {
                    rope = rope.insert(edit.position, edit.insertedContent);
                }
                latency[i] = System.nanoTime() - start;
            }
            return rope.toString();
        });
    }

    @Test
    public void testStrings() throws IOException {
        testEntry("editing-traces-string", (state, latency) -> {
            String str = state.trace.start;
            for (int i = 0; i < latency.length; i++) {
                Edit edit = state.trace.edits[i];
                long start = System.nanoTime();
                if (edit.numDeleted > 0) {
                    str = str.substring(0, edit.position) + str.substring(edit.position + edit.numDeleted);
                }
                if (!edit.insertedContent.isEmpty()) {
                    str = str.substring(0, edit.position) + edit.insertedContent + str.substring(edit.position);
                }
                latency[i] = System.nanoTime() - start;
            }
            return str;
        });
    }

    @Test
    public void testGapBuffer() throws IOException {
        testEntry("editing-traces-gap", (state, latency) -> {
            for (int i = 0; i < latency.length; i++) {
                Edit edit = state.trace.edits[i];
                long start = System.nanoTime();
                applyGapEdit(state, edit);
                latency[i] = System.nanoTime() - start;
            }
            return state.gapBuffer.toString();
        });
    }

    @Test
    public void testTruffleString() throws IOException {
        RootNode rootNode = new RootNode(null) {
            @SuppressWarnings("FieldMayBeFinal")
            @Child
            TruffleStringTestNode testNode = EditingTracesTestFactory.TruffleStringTestNodeGen.create();

            @Override
            public Object execute(VirtualFrame frame) {
                PieceState state = (PieceState) frame.getArguments()[0];
                long[] latency = (long[]) frame.getArguments()[1];
                return testNode.executeTest(state, latency);
            }

            @Override
            public String getName() {
                return "truffle-string-test";
            }
        };
        RootCallTarget callTarget = rootNode.getCallTarget();
        for (int run = 0; run < 5; run++) {
            int finalRun = run;
            long outerStart = System.nanoTime();
            testEntry("editing-traces-tstring", (state, latency) -> {
                Object result = callTarget.call(state, latency);
                return finalRun == 4 ? (String) result : null;
            });
            System.out.println((System.nanoTime() - outerStart) / 1_000_000.);
        }
    }

    static abstract class TruffleStringTestNode extends Node {
        abstract String executeTest(PieceState state, long[] latency);

        private final TruffleString.Encoding encoding = TruffleString.Encoding.UTF_16;

        @Specialization
        String runTest(
                PieceState state, long[] latency,
                @Cached TruffleString.FromJavaStringNode fromJava,
                @Cached TruffleString.SubstringNode substring,
                @Cached TruffleString.ConcatNode concat,
                @Cached TruffleString.CodePointLengthNode length
        ) {
            TruffleString s = fromJava.execute(state.trace.start, encoding);
            for (int i = 0; i < latency.length; i++) {
                Edit edit = state.trace.edits[i];
                TruffleString insert = fromJava.execute(edit.insertedContent, encoding);
                long start = System.nanoTime();
                TruffleString pre = substring.execute(s, 0, edit.position, encoding, true);
                int len = length.execute(s, encoding);
                int postStart = edit.position + edit.numDeleted;
                TruffleString post = substring.execute(s, postStart, len - postStart, encoding, true);
                if (edit.numDeleted > 0) {
                    s = concat.execute(pre, post, encoding, true);
                }
                if (!edit.insertedContent.isEmpty()) {
                    s = concat.execute(
                            concat.execute(pre, insert, encoding, true),
                            post, encoding, true
                    );
                }
                latency[i] = System.nanoTime() - start;
            }
            return s.toJavaStringUncached();
        }
    }

    @Benchmark
    public long pieceTree(PieceState state) {
        for (Edit edit : state.trace.edits) {
            applyEdit(edit, state.tree);
        }
        return state.tree.getLength();
    }

    @Benchmark
    public int gapBuffer(PieceState state) {
        Edit[] edits = state.trace.edits;
        for (Edit edit : edits) {
            applyGapEdit(state, edit);
        }
        return state.gapBuffer.length();
    }

    private record Edit(int position, int numDeleted, String insertedContent) {
    }

    public record Trace(String start, Edit[] edits, String end) {
    }

    @State(Scope.Benchmark)
    public static class PieceState {

        @SuppressWarnings("NotNullFieldNotInitialized")
        @Param({
                "automerge-paper.json.gz",
                "clownschool_flat.json.gz",
                "friendsforever_flat.json.gz",
                "json-crdt-blog-post.json.gz",
                "json-crdt-patch.json.gz",
                "rustcode.json.gz",
                "seph-blog1.json.gz",
                "sveltecomponent.json.gz",
        })
        public String file;
        @SuppressWarnings("NotNullFieldNotInitialized")
        public Trace trace;
        @SuppressWarnings("NotNullFieldNotInitialized")
        public PieceTreeBase tree;
        @SuppressWarnings("NotNullFieldNotInitialized")
        public GapBuffer gapBuffer;

        @Setup(Level.Iteration)
        public void setUp() {
            trace = parseTrace(readFromGzResource(file));
            tree = new PieceTreeBase(false, trace.start.getBytes(StandardCharsets.UTF_8));
            gapBuffer = new GapBuffer(trace.start);
        }

        public static final class GapBuffer {
            public final static int INIT_SIZE = 1024;
            public char[] buffer;
            public int gapStart;
            public int gapEnd;

            public GapBuffer(String init) {
                buffer = new char[INIT_SIZE + init.length()];
                init.getChars(0, init.length(), buffer, 0);
                gapStart = init.length();
                gapEnd = INIT_SIZE + init.length();
            }

            private void moveGap(int position) {
                if (position == gapStart) {
                    return;
                }
                if (gapStart >= gapEnd) {
                    gapStart = gapEnd = position;
                    return;
                }
                if (position < gapStart) {
                    int delta = gapStart - position;
                    System.arraycopy(buffer, position, buffer, gapEnd - delta, delta);
                    gapStart = position;
                    gapEnd -= delta;
                } else {
                    int delta = position - gapStart;
                    System.arraycopy(buffer, gapEnd, buffer, gapStart, delta);
                    gapStart = position;
                    gapEnd += delta;
                }
            }

            public void delete(int position, int count) {
                moveGap(position + count);
                gapStart -= count;
            }

            public void insert(int position, String content) {
                moveGap(position);
                if (gapEnd - gapStart < content.length()) {
                    int length = buffer.length;
                    buffer = Arrays.copyOf(buffer, Math.max(
                            length * 2,
                            length + content.length() + INIT_SIZE
                    ));
                    int delta = buffer.length - length;
                    System.arraycopy(buffer, gapEnd, buffer, gapEnd + delta, length - gapEnd);
                    gapEnd += delta;
                }
                content.getChars(0, content.length(), buffer, gapStart);
                gapStart += content.length();
            }

            public int length() {
                return buffer.length - (gapEnd - gapStart);
            }

            @Override
            public String toString() {
                return String.valueOf(buffer, 0, gapStart) +
                        String.valueOf(buffer, gapEnd, buffer.length - gapEnd);
            }
        }
    }
}
