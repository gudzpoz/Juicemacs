package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.debug.*;
import com.oracle.truffle.tck.DebuggerTester;
import org.graalvm.polyglot.*;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.annotations.*;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.StreamSupport;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContextBuilder;

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

    @SuppressWarnings("NotNullFieldNotInitialized")
    private Context context;

    @Setup
    public void setup() throws IOException {
        context = getTestingContext();
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
        try (Context c = getTestingContext()) {
            String source = "(equal (nconc '(1 2 3) nil '(4 5 6)) '(1 2 3 4 5 6))";
            assertTrue(c.eval("elisp", source).asBoolean());
            // Ast caching can be disastrous...
            assertFalse(c.eval("elisp", source).asBoolean());
        }
    }

    @Test
    public void testEmacsDiff() {
        try (Context c = getTestingContext()) {
            c.eval("elisp", "(setq ast '(equal (nconc '(1 2 3) nil '(4 5 6)) '(1 2 3 4 5 6)) _ 0)");
            // Emacs: true
            assertTrue(c.eval("elisp", "(eval ast)").asBoolean());
            // Emacs: false
            assertFalse(c.eval("elisp", "(eval ast)").asBoolean());
        }
    }

    @Test
    public void testFullStackTrace() {
        try (Context c = getTestingContext()) {
            // Lisp functions
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (defalias 'f1 #'(lambda ()
                          (f2)))
                        (defalias 'f2 #'(lambda ()
                          (f3)))
                        (defalias 'f3 #'(lambda ()
                          (f4)))
                        (defalias 'f4 #'(lambda ()
                          (f5)))
                        (defalias 'f5 #'(lambda ()
                          (signal 'error "test")))
                        (f1)
                        """)
                );
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("signal", list.getFirst().getRootName());
                assertEquals("BuiltInEval.java", list.getFirst().getSourceLocation().getSource().getName());
                for (int i = 1; i <= 5; i++) {
                    PolyglotException.StackFrame frame = list.get(i);
                    int fn = 5 - i + 1;
                    assertEquals("f" + fn, frame.getRootName());
                    assertEquals(fn * 2, frame.getSourceLocation().getStartLine());
                }
                assertEquals("Unnamed", list.get(6).getRootName());
                assertEquals(11, list.get(6).getSourceLocation().getStartLine());
            }
            // Built-in functions
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (defalias 'f1 #'(lambda () (f2)))
                        (defalias 'f2 #'(lambda () (f3)))
                        (defalias 'f3 #'(lambda () (autoload 1 1)))
                        (f1)
                        """)
                );
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("autoload", list.getFirst().getRootName());
            }
            // Built-in function thrown
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (set-match-data 0)
                        """)
                );
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("set-match-data", list.getFirst().getRootName());
                SourceSection location = list.getFirst().getSourceLocation();
                assertNotNull(location);
            }
            // Macro expansion #1
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (defalias 'f1 (cons 'macro #'(lambda () (list 'signal ''error ''data))))
                        (f1)
                        """));
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("signal", list.getFirst().getRootName());
                assertEquals("Unnamed", list.get(1).getRootName());
                assertEquals(2, list.get(1).getSourceLocation().getStartLine());
            }
            // Macro expansion #2
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (defalias 'f1 (cons 'macro #'(lambda () 'undefined-var)))
                        (f1)
                        """));
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("Unnamed", list.getFirst().getRootName());
                assertEquals(2, list.getFirst().getSourceLocation().getStartLine());
            }
            // Eval
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (eval
                          '(signal 'error 'data))
                        """)
                );
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("signal", list.getFirst().getRootName());
                assertEquals("<eval>", list.get(1).getRootName());
                assertEquals(2, list.get(1).getSourceLocation().getStartLine());
            }
            // Inlined
            {
                PolyglotException e = assertThrows(PolyglotException.class, () -> c.eval("elisp", """
                        (+
                          1
                          2
                          nil)
                        """)
                );
                Iterable<PolyglotException.StackFrame> trace = e.getPolyglotStackTrace();
                List<PolyglotException.StackFrame> list = StreamSupport.stream(trace.spliterator(), false).toList();
                assertEquals("Unnamed", list.getFirst().getRootName());
                assertEquals(4, list.getFirst().getSourceLocation().getEndLine());
            }
        }
    }

    @Test
    public void testDebuggerBreakpoint() throws IOException {
        Source source = Source.newBuilder("elisp", """
                ;;; -*- lexical-binding: t -*-
                (defvar b)
                (defalias 'test #'(lambda (c)
                  (let ((a 1)
                        (b 2))
                    (+ a b
                       (1+ c)))))
                (test 3)
                """, "test-func").build();
        try (
                DebuggerTester tester = new DebuggerTester(getTestingContextBuilder().option("elisp.truffleDebug", "true"));
                DebuggerSession session = tester.startSession()
        ) {
            // Pre-evaluation is needed to expand the AST
            tester.startEval(source);
            tester.expectDone();

            session.suspendNextExecution();
            session.install(Breakpoint.newBuilder(source.getURI()).lineIs(6).build());
            tester.startEval(Source.create("elisp", "(test 3)"));

            // Top level root node
            tester.expectSuspended(event -> {
                assertEquals("Unnamed", event.getTopStackFrame().getName());
                event.prepareContinue();
            });
            // Multiple frames
            tester.expectSuspended(event -> {
                assertEquals(6, event.getSourceSection().getStartLine());
                List<DebugStackFrame> list = StreamSupport.stream(event.getStackFrames().spliterator(), false).toList();
                assertEquals(2, list.size());
                assertEquals(64, event.getTopStackFrame().eval("(expt 2 (+ a b c))").asLong());
                event.prepareStepOver(1);
            });
            // Scope access
            tester.expectSuspended(event -> {
                assertEquals(7, event.getSourceSection().getStartLine());

                DebugStackFrame frame = event.getTopStackFrame();
                assertEquals("test", frame.getName());
                DebugScope scope = frame.getScope();
                assertNotNull(scope);
                DebugValue a = scope.getDeclaredValue("a");
                DebugValue b = scope.getDeclaredValue("b");
                DebugValue c = scope.getDeclaredValue("c");
                assertNull(scope.getDeclaredValue("d"));
                assertNotNull(a);
                assertNull(b);
                b = session.getTopScope("elisp").getDeclaredValue("b");
                assertNotNull(b);
                assertNotNull(c);
                assertEquals(1, a.asInt());
                assertEquals(2, b.asInt());
                assertEquals(3, c.asInt());
                assertEquals(64, frame.eval("(expt 2 (+ a b c))").asLong());
                event.prepareContinue();
            });
            tester.expectDone();
        }
    }
}
