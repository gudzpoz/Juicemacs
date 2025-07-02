package party.iroiro.juicemacs.elisp.benchmarks;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContextBuilder;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@Fork(1)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 3, time = 5)
@State(Scope.Benchmark)
public class PiDigitsBenchmarkTest {
    @Test
    public void truncateTest() {
        try (Context context = getTestingContext()) {
            Value value = context.eval("elisp", """
                    (setq elb-num 130767436800000000
                          elb-acc 1269589784327156250
                          elb-den 191898783962510625
                          nth 3)
                    (truncate (+ (* elb-num nth) elb-acc) elb-den)
                    """);
            assertEquals(8, value.asLong());
        }
    }

    private Context context;

    @Setup
    public void setup() throws IOException {
        context = getTestingContextBuilder()
                .allowExperimentalOptions(true)
                .option("engine.CompilationFailureAction", "Diagnose")
                .option("engine.TraceCompilation", "true")
                .build();
        try (InputStream src = Objects.requireNonNull(getClass().getResourceAsStream("/pidigits.el"))) {
            context.eval(Source.newBuilder("elisp", new InputStreamReader(src), "pidigits").build());
        }
    }

    @TearDown
    public void tearDown() {
        context.close();
    }

    @Benchmark
    public Object piDigits() throws IOException {
        return context.eval(Source.newBuilder("elisp", "(elb-pidigits-entry)", "<35>").build());
    }

    @Test
    public void testPiDigits() throws IOException {
        PiDigitsBenchmarkTest benchmark = new PiDigitsBenchmarkTest();
        benchmark.setup();
        assertEquals("false", benchmark.piDigits().toString());
        benchmark.tearDown();
    }
}
