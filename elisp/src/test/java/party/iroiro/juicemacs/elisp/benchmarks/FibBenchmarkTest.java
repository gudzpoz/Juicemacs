package party.iroiro.juicemacs.elisp.benchmarks;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIfSystemProperty;
import org.openjdk.jmh.annotations.*;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@Fork(1)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 3, time = 5)
@State(Scope.Benchmark)
@DisabledIfSystemProperty(named = "coverage", matches = "true")
public class FibBenchmarkTest {
    public static final String FIB = """
            ;;; -*- lexical-binding: t -*-
            (defalias 'fib
              #'(lambda (x)
                  (if (<= x 2)
                      1
                    (+ (fib (1- x)) (fib (- x 2))))))
            nil
            """;
    public static final String FIB_BYTE_COMPILED = """
            ;;; -*- lexical-binding: t -*-
            (defalias 'fib-byte-compiled
              (make-byte-code 257
                              (unibyte-string 137 192 88 131 8 0 193 135 194 1 83 33 194 2
                                              192 90 33 92 135)
                              [2 1 fib-byte-compiled] 5))
            nil
            """;

    private Context context;

    @Setup
    public void setup() throws IOException {
        context = getTestingContext();
        context.eval(Source.newBuilder("elisp", FIB, "fib").build());
        context.eval(Source.newBuilder("elisp", FIB_BYTE_COMPILED, "fib").build());
    }

    @TearDown
    public void tearDown() {
        context.close();
    }

    @Benchmark
    public long fib() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(fib 35)", "<35>").build());
        return v.asLong();
    }
    @Benchmark
    public long fibByteCompiled() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(fib-byte-compiled 35)", "<35-bytecode>").build());
        return v.asLong();
    }
    @Benchmark
    public long fibJava() {
        // Hopefully Java is not smart enough to constant-fold this.
        return fibJavaImpl(35);
    }

    @Test
    public void testFib() throws IOException {
        FibBenchmarkTest benchmark = new FibBenchmarkTest();
        benchmark.setup();
        assertEquals(9227465, benchmark.fib());
        benchmark.tearDown();
    }
    @Test
    public void testFibByteCompiled() throws IOException {
        FibBenchmarkTest benchmark = new FibBenchmarkTest();
        benchmark.setup();
        assertEquals(9227465, benchmark.fibByteCompiled());
        benchmark.tearDown();
    }
    @Test
    public void testFibJava() {
        assertEquals(9227465, fibJava());
    }

    private long fibJavaImpl(int x) {
        if (x <= 2) {
            return 1;
        }
        return fibJavaImpl(x - 1) + fibJavaImpl(x - 2);
    }
}
