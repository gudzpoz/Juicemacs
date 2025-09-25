package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.debug.*;
import com.oracle.truffle.tck.DebuggerTester;
import org.graalvm.polyglot.*;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;
import java.util.stream.StreamSupport;

import static org.junit.jupiter.api.Assertions.*;
import static party.iroiro.juicemacs.elisp.TestingUtils.getContextBuilder;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContext;
import static party.iroiro.juicemacs.elisp.forms.BaseFormTest.getTestingContextBuilder;

public class ELispInterpreterTest {
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
                assertEquals("Unnamed", list.get(1).getRootName());
                assertEquals(2, list.get(1).getSourceLocation().getStartLine());
                assertEquals("Unnamed", list.get(2).getRootName());
                assertEquals(1, list.get(2).getSourceLocation().getStartLine());
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
                assertNull(c);
                c = scope.getParent().getDeclaredValue("c");
                assertNotNull(c);
                assertEquals(1, a.asInt());
                assertEquals(2, b.asInt());
                assertEquals(3, c.asInt());
                assertEquals(64, frame.eval("(expt 2 (+ a b c))").asLong());

                event.prepareStepOver(1);
            });
            // Scope access
            tester.expectSuspended(event -> {
                // FIXME: should be 7 instead of 1
                assertEquals(1, event.getSourceSection().getStartLine());
                event.prepareContinue();
            });
            tester.expectDone();
        }
    }

    @Test
    public void testIncList() {
        try (Context context = getContextBuilder(null).build()) {
            context.eval("elisp", """
                    ;;; -*- lexical-binding: t -*-
                    (defvar elb-inclist-no-type-hints-list
                      (mapcar #'random (make-list 50000 100)))
                    
                    (defalias 'elb-inclist #'(lambda (l)
                      (prog1 l
                        (while l
                          (let ((c l))
                    	    (setcar c (1+ (car c)))
                    	    (setq l (cdr c)))))))
                    """);
            for (int i = 0; i < 10000; i++) {
                context.eval("elisp", "(elb-inclist elb-inclist-no-type-hints-list)");
            }
        }
    }
}
