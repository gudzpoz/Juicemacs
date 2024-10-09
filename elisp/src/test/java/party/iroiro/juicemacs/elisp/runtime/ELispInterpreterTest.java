package party.iroiro.juicemacs.elisp.runtime;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@Fork(1)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 3, time = 5)
@State(Scope.Benchmark)
public class ELispInterpreterTest {
    public final static String MANDELBROT = """
;;; -*- lexical-binding: t -*-
(defalias
  'mandelbrot
  #'(lambda (size)
      (let ((sum 0)
            (byte-acc 0)
            (bit-num 0)
            (y 0)
            ;; maintaining dynamic static analysis info along the stack comes at a cost...
            ;; changing from using =let= blocks to using a single `let' block results in
            ;; a 20% speedup (from ~8s to 5.6s).
            ci x zr zrzr zi zizi cr z escape tr ti)
        (while (< y size)
          (setq ci (- (/ (* 2.0 y) size) 1.0)
                x 0)
          (while (< x size)
            (setq zr 0
                  zrzr 0
                  zi 0
                  zizi 0
                  cr (- (/ (* 2.0 x) size) 1.5)
                  z 0
                  escape 1) ;; this was originally a let block
            (while (< z 50)
              (setq tr (+ (- zrzr zizi) cr)
                    ti (+ (* 2.0 zr zi) ci)) ;; this was originally a let block
              (setq zr tr
                    zi ti)
              (setq zrzr (* zr zr)
                    zizi (* zi zi))
              (if (> (+ zrzr zizi) 4.0)
                  (setq escape 0
                        z 50))
              (setq z (1+ z)))
            (setq byte-acc (logior (ash byte-acc 1) escape))
            (setq bit-num (1+ bit-num))
            (if (= 8 bit-num)
                (setq sum (logxor sum byte-acc)
                      byte-acc 0
                      bit-num 0)
              (if (= x (1- size))
                  (setq byte-acc (ash byte-acc (- 8 bit-num))
                        sum (logxor sum byte-acc)
                        byte-acc 0
                        bit-num 0)))
            (setq x (1+ x)))
          (setq y (1+ y)))
        sum)))
nil
    """;
    public static final String FIB = """
            ;;; -*- lexical-binding: t -*-
            (defalias 'fib
              #'(lambda (x)
                  (if (<= x 2)
                      1
                    (+ (fib (1- x)) (fib (- x 2))))))
            nil
            """;

    private Context context;

    @Setup
    public void setup() throws IOException {
        context = Context.newBuilder("elisp").build();
        context.eval(Source.newBuilder("elisp", FIB, "fib").build());
        context.eval(Source.newBuilder("elisp", MANDELBROT, "mandelbrot").build());
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
    public long mandelbrot() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(mandelbrot 750)", "<750>").build());
        return v.asLong();
    }

    @Test
    public void testMandelbrot() throws IOException {
        ELispInterpreterTest benchmark = new ELispInterpreterTest();
        benchmark.setup();
        assertEquals(192, benchmark.mandelbrot());
        benchmark.tearDown();
    }

    @Test
    public void testFib() throws IOException {
        ELispInterpreterTest benchmark = new ELispInterpreterTest();
        benchmark.setup();
        assertEquals(9227465, benchmark.fib());
        benchmark.tearDown();
    }
}
