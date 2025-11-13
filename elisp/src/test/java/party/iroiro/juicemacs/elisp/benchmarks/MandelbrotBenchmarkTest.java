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
public class MandelbrotBenchmarkTest {
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
    public final static String MANDELBROT_BYTE_COMPILED = """
            (defalias
              'mandelbrot-byte-compiled
              (make-byte-code
               257
               (unibyte-string 192 137 137 137 137 5 87 131 189 0 137 193 95 5 165 194 90 192 137 6 7 87 131 183 0 195 \
               137 137 137 4 193 95 6 11 165 196 90 192 197 1 198 87 131 103 0 5 4 90 3 92 6 7 193 95 6 6 95 6 10 92 1 \
               178 9 137 178 7 6 8 137 95 178 8 6 6 137 95 178 6 6 7 6 6 92 199 86 131 94 0 192 178 3 198 178 4 3 84 \
               178 4 182 2 130 39 0 200 201 6 13 197 34 2 34 178 12 6 10 84 178 11 6 10 202 85 131 144 0 203 6 13 6 13 \
               34 178 13 192 178 12 192 178 11 182 7 130 179 0 6 7 6 14 83 85 131 177 0 201 6 12 202 6 13 90 34 178 12 203 \
               6 13 6 13 34 178 13 192 178 12 192 178 11 182 7 84 130 18 0 182 2 84 130 4 0 3 135)
               [0 2.0 1.0 0.0 1.5 1 50 4.0 logior ash 8 logxor]
               18
               "(fn SIZE)"))
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

    private Context context;

    @Setup
    public void setup() throws IOException {
        context = getTestingContext();
        context.eval(Source.newBuilder("elisp", MANDELBROT, "mandelbrot").build());
        context.eval(Source.newBuilder("elisp", MANDELBROT_NESTED_LETS, "mandelbrot-lets").build());
        context.eval(Source.newBuilder("elisp", MANDELBROT_BYTE_COMPILED, "mandelbrot-byte-compiled").build());
    }

    @TearDown
    public void tearDown() {
        context.close();
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
    public long mandelbrotByteCompiled() throws IOException {
        Value v = context.eval(Source.newBuilder("elisp", "(mandelbrot-byte-compiled 750)", "<750>").build());
        assertEquals(192, v.asLong());
        return v.asLong();
    }

    @Benchmark
    public long mandelbrotJava() {
        int mandelbrot = MandelbrotBenchmarkTest.Mandelbrot.mandelbrot(750);
        assertEquals(192, mandelbrot);
        return mandelbrot;
    }

    @Test
    public void testMandelbrot() throws IOException {
        MandelbrotBenchmarkTest benchmark = new MandelbrotBenchmarkTest();
        benchmark.setup();
        assertEquals(192, benchmark.mandelbrot());
        Value v = benchmark.context
                .eval(Source.newBuilder("elisp", "(mandelbrot 1000)", "<1000>").build());
        assertEquals(5, v.asLong());
        benchmark.tearDown();
    }

    @Test
    public void testMandelbrotNestedLets() throws IOException {
        MandelbrotBenchmarkTest benchmark = new MandelbrotBenchmarkTest();
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
    public void testMandelbrotByteCompiled() throws IOException {
        MandelbrotBenchmarkTest benchmark = new MandelbrotBenchmarkTest();
        benchmark.setup();
        for (int i = 0; i < 10; i++) {
            assertEquals(192, benchmark.mandelbrotByteCompiled());
        }
        Value v = benchmark.context
                .eval(Source.newBuilder("elisp", "(mandelbrot-byte-compiled 1000)", "<1000>").build());
        assertEquals(5, v.asLong());
        benchmark.tearDown();
    }

    @Test
    public void testMandelbrotJava() {
        assertEquals(192, mandelbrotJava());
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
}
