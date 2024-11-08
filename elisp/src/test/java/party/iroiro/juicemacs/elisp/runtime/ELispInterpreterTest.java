package party.iroiro.juicemacs.elisp.runtime;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@Fork(1)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 3, time = 5)
@State(Scope.Benchmark)
public class ELispInterpreterTest {
    public final static String MANDELBROT_NESTED_LETS = """
            ;;; -*- lexical-binding: t -*-
            (defalias
              'mandelbrot-lets
              #'(lambda (size)
                  (let ((sum 0)
                        (byte-acc 0)
                        (bit-num 0)
                        (y 0))
                    (while (< y size)
                      (let ((ci (- (/ (* 2.0 y) size) 1.0))
                            (x 0))
                        (while (< x size)
                          (let ((zr 0.0)
                                (zrzr 0.0)
                                (zi 0.0)
                                (zizi 0.0)
                                (cr (- (/ (* 2.0 x) size) 1.5))
                                (z 0)
                                (escape 1)) ;; this was originally a let block
                            (while (< z 50)
                              (let ((tr (+ (- zrzr zizi) cr))
                                    (ti (+ (* 2.0 zr zi) ci)))
                                (setq zr tr
                                      zi ti)
                                (setq zrzr (* zr zr)
                                      zizi (* zi zi))
                                (if (> (+ zrzr zizi) 4.0)
                                    (setq escape 0
                                          z 50))
                                (setq z (1+ z))))
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
                            (setq x (1+ x))))
                        (setq y (1+ y))))
                    sum)))
            nil
            """;
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
                        (setq zr 0.0   ;; setting values here to 0 instead of 0.0 causes huge gc pressure due to boxing
                              zrzr 0.0
                              zi 0.0
                              zizi 0.0
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
        context.eval(Source.newBuilder("elisp", MANDELBROT_NESTED_LETS, "mandelbrot-lets").build());
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
        assertEquals(192, v.asLong());
        return v.asLong();
    }

    @Benchmark
    public long mandelbrotNestedLets() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(mandelbrot-lets 750)", "<750>").build());
        assertEquals(192, v.asLong());
        return v.asLong();
    }

    @Benchmark
    public long mandelbrotJava() {
        int mandelbrot = Mandelbrot.mandelbrot(750);
        assertEquals(192, mandelbrot);
        return mandelbrot;
    }

    @Test
    public void testMandelbrot() throws IOException {
        ELispInterpreterTest benchmark = new ELispInterpreterTest();
        benchmark.setup();
        assertEquals(192, benchmark.mandelbrot());
        Value v = benchmark.context
                .eval(Source.newBuilder("elisp", "(mandelbrot 1000)", "<1000>").build());
        assertEquals(5, v.asLong());
        benchmark.tearDown();
    }

    @Test
    public void testMandelbrotNestedLets() throws IOException {
        ELispInterpreterTest benchmark = new ELispInterpreterTest();
        benchmark.setup();
        for (int i = 0; i < 10; i++) {
            assertEquals(192, benchmark.mandelbrotNestedLets());
        }
        Value v = benchmark.context
                .eval(Source.newBuilder("elisp", "(mandelbrot-lets 1000)", "<1000>").build());
        assertEquals(5, v.asLong());
        benchmark.tearDown();
    }

    @Test
    public void testMandelbrotJava() {
        assertEquals(192, mandelbrotJava());
    }

    @Test
    public void testFib() throws IOException {
        ELispInterpreterTest benchmark = new ELispInterpreterTest();
        benchmark.setup();
        assertEquals(9227465, benchmark.fib());
        benchmark.tearDown();
    }

    // Copyright © 2004-2013 Brent Fulgham
    //
    // All rights reserved.
    //
    // Redistribution and use in source and binary forms, with or without
    // modification, are permitted provided that the following conditions are met:
    //
    //   * Redistributions of source code must retain the above copyright notice,
    //     this list of conditions and the following disclaimer.
    //
    //   * Redistributions in binary form must reproduce the above copyright notice,
    //     this list of conditions and the following disclaimer in the documentation
    //     and/or other materials provided with the distribution.
    //
    //   * Neither the name of "The Computer Language Benchmarks Game" nor the name
    //     of "The Computer Language Shootout Benchmarks" nor the names of its
    //     contributors may be used to endorse or promote products derived from this
    //     software without specific prior written permission.
    //
    // THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    // AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    // IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    // DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
    // FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    // DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    // SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    // CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
    // OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    // OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    // The Computer Language Benchmarks Game
    // http://benchmarksgame.alioth.debian.org
    //
    //  contributed by Karl von Laudermann
    //  modified by Jeremy Echols
    //  modified by Detlef Reichl
    //  modified by Joseph LaFata
    //  modified by Peter Zotov

    // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=mandelbrot&lang=yarv&id=3

    final static class Mandelbrot {

        public static int mandelbrot(final double size) {
            int sum = 0;

            int byte_acc = 0;
            int bit_num = 0;

            int y = 0;
            while (y < size) {
                double ci = (2.0 * y / size) - 1.0;

                int x = 0;
                while (x < size) {
                    double zr = 0.0;
                    double zrzr = 0.0;
                    double zi = 0.0;
                    double zizi = 0.0;
                    double cr = (2.0 * x / size) - 1.5;

                    int z = 0;
                    int escape = 1;

                    while (z < 50) {
                        double tr = zrzr - zizi + cr;
                        double ti = 2.0 * zr * zi + ci;

                        zr = tr;
                        zi = ti;

                        // preserve recalculation
                        zrzr = zr * zr;
                        zizi = zi * zi;
                        if (zrzr + zizi > 4.0) {
                            escape = 0;
                            break;
                        }
                        z += 1;
                    }

                    byte_acc = (byte_acc << 1) | escape;
                    bit_num += 1;

                    // Code is very similar for these cases, but using separate blocks
                    // ensures we skip the shifting when it's unnecessary, which is most cases.
                    if (bit_num == 8) {
                        sum ^= byte_acc;
                        byte_acc = 0;
                        bit_num = 0;
                    } else if (x == size - 1) {
                        byte_acc <<= (8 - bit_num);
                        sum ^= byte_acc;
                        byte_acc = 0;
                        bit_num = 0;
                    }
                    x += 1;
                }
                y += 1;
            }
            return sum;
        }
    }

    @Test
    public void testAstCache() {
        try (Context c = Context.newBuilder("elisp").build()) {
            String source = "(equal (nconc '(1 2 3) nil '(4 5 6)) '(1 2 3 4 5 6))";
            assertTrue(c.eval("elisp", source).asBoolean());
            // Ast caching can be disastrous...
            assertFalse(c.eval("elisp", source).asBoolean());
        }
    }

    @Test
    public void testEmacsDiff() {
        try (Context c = Context.newBuilder("elisp").build()) {
            c.eval("elisp", "(setq ast '(equal (nconc '(1 2 3) nil '(4 5 6)) '(1 2 3 4 5 6)) _ 0)");
            // Emacs: true
            assertTrue(c.eval("elisp", "(eval ast)").asBoolean());
            // Emacs: false
            assertFalse(c.eval("elisp", "(eval ast)").asBoolean());
        }
    }
}
