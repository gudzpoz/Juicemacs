package party.iroiro.juicemacs.piecetree;

import org.ahmadsoft.ropes.Rope;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.zip.GZIPInputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static party.iroiro.juicemacs.piecetree.StringBuffer.fromString;

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

    private record Edit(int position, int numDeleted, String insertedContent) {
    }

    public record Trace(String start, Edit[] edits, String end) {
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

    private long[] testTrace(String file) {
        String s = readFromGzResource(file);
        Trace trace = parseTrace(s);
        PieceTreeBase tree = new PieceTreeBase(
                List.of(new StringBuffer(fromString(trace.start), true)),
                PieceTreeBase.EndOfLine.LF,
                !trace.start.contains("\r")
        );
        long[] latency = new long[trace.edits.length];
        Edit[] edits = trace.edits;
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
        assertEquals(trace.end, tree.getLinesRawContent().toString());
        return latency;
    }

    @Test
    public void testTraces() throws IOException {
        File stats = File.createTempFile("editing-traces", ".csv");
        try (FileWriter writer = new FileWriter(stats)) {
            writer.append("trace,edit,latency\n");
            for (String file : TRACES) {
                long[] latency = testTrace(file);
                for (int i = 0; i < latency.length; i++) {
                    writer
                            .append(file).append(",")
                            .append(String.valueOf(i)).append(",")
                            .append(String.valueOf(latency[i])).append("\n");
                }
            }
            writer.flush();
        }
        System.out.println("Stats (no warm up) at " + stats);
    }

    @Test
    public void testRopes() throws IOException {
        File stats = File.createTempFile("editing-traces-ropes", ".csv");
        try (FileWriter writer = new FileWriter(stats)) {
            writer.append("trace,edit,latency\n");
            for (String file : TRACES) {
                PieceState state = new PieceState();
                state.file = file;
                state.setUp();
                Rope rope = Rope.BUILDER.build(state.trace.start);
                long[] latency = new long[state.trace.edits.length];
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
                assertEquals(state.trace.end, rope.toString());
                for (int i = 0; i < latency.length; i++) {
                    writer
                            .append(file).append(",")
                            .append(String.valueOf(i)).append(",")
                            .append(String.valueOf(latency[i])).append("\n");
                }
            }
            writer.flush();
        }
        System.out.println("Stats (rope) (no warm up) at " + stats);
    }

    @Test
    public void testStrings() throws IOException {
        File stats = File.createTempFile("editing-traces-string", ".csv");
        try (FileWriter writer = new FileWriter(stats)) {
            writer.append("trace,edit,latency\n");
            for (String file : TRACES) {
                PieceState state = new PieceState();
                state.file = file;
                state.setUp();
                String str = state.trace.start;
                long[] latency = new long[state.trace.edits.length];
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
                assertEquals(state.trace.end, str);
                for (int i = 0; i < latency.length; i++) {
                    writer
                            .append(file).append(",")
                            .append(String.valueOf(i)).append(",")
                            .append(String.valueOf(latency[i])).append("\n");
                }
            }
            writer.flush();
        }
        System.out.println("Stats (string) (no warm up) at " + stats);
    }

    @Test
    public void testGapBuffer() throws IOException {
        File stats = File.createTempFile("editing-traces-gap", ".csv");
        try (FileWriter writer = new FileWriter(stats)) {
            writer.append("trace,edit,latency\n");
            for (String file : TRACES) {
                PieceState state = new PieceState();
                state.file = file;
                state.setUp();
                long[] latency = new long[state.trace.edits.length];
                for (int i = 0; i < latency.length; i++) {
                    Edit edit = state.trace.edits[i];
                    long start = System.nanoTime();
                    applyGapEdit(state, edit);
                    latency[i] = System.nanoTime() - start;
                }
                assertEquals(state.trace.end, state.gapBuffer.toString());
                for (int i = 0; i < latency.length; i++) {
                    writer
                            .append(file).append(",")
                            .append(String.valueOf(i)).append(",")
                            .append(String.valueOf(latency[i])).append("\n");
                }
            }
            writer.flush();
        }
        System.out.println("Stats (gap) (no warm up) at " + stats);
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

        @Setup(Level.Iteration)
        public void setUp() {
            trace = parseTrace(readFromGzResource(file));
            tree = new PieceTreeBase(
                    List.of(new StringBuffer(fromString(trace.start), true)),
                    PieceTreeBase.EndOfLine.LF,
                    !trace.start.contains("\r")
            );
            gapBuffer = new GapBuffer(trace.start);
        }
    }

    @Benchmark
    public int pieceTree(PieceState state) {
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
            state.insert(edit.position, fromString(edit.insertedContent), false);
        }
    }
}
