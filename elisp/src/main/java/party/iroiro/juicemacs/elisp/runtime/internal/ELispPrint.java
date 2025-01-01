package party.iroiro.juicemacs.elisp.runtime.internal;

import com.oracle.truffle.api.CompilerDirectives;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.lang.constant.Constable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_5_BYTE_CHAR;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public final class ELispPrint {
    private final PrintFunc func;
    private final HashMap<Object, Integer> visited;
    private final ArrayList<Object> stack;
    private boolean inString = false;

    private ELispPrint(PrintFunc func) {
        this.stack = new ArrayList<>();
        this.visited = new HashMap<>();
        this.func = func;
    }

    public void startString() {
        if (inString) {
            throw new IllegalStateException("already in string");
        }
        inString = true;
        func.print('"');
    }

    public void endString() {
        if (!inString) {
            throw new IllegalStateException("not in string");
        }
        inString = false;
        func.print('"');
    }

    public void printRawByte(byte b) {
        func.print(b >= 0 ? b : ((b & 0x7F) + MAX_5_BYTE_CHAR + 1));
    }

    public ELispPrint print(int c) {
        if (inString && c == '"') {
            func.print('\\');
        }
        func.print(c);
        return this;
    }

    public ELispPrint print(String s) {
        func.print(MuleString.fromString(s));
        return this;
    }

    public void start(Object o) {
        stack.add(o);
    }

    public void end() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("unbalanced end");
        }
        stack.removeLast();
    }

    public ELispPrint sep() {
        func.print(' ');
        return this;
    }

    public ELispPrint printInt(int i) {
        print(MuleString.fromString(Integer.toString(i)));
        return this;
    }

    public void printCons(ELispCons cons) {
        Object car = cons.car();
        Object cdr = cons.cdr();
        start(cons);
        switch (cdr) {
            case ELispCons quoted when car == QUOTE && isNil(quoted.cdr()) ->
                    print('\'').print(quoted.car());
            case ELispCons function when car == FUNCTION && isNil(function.cdr()) ->
                    print('#').print('\'').print(function.car());
            case ELispCons backquote when car == BACKQUOTE && isNil(backquote.cdr()) ->
                    print('`').print(backquote.car());
            case ELispCons comma when car == COMMA && isNil(comma.cdr()) ->
                    print(',').print(comma.car());
            case ELispCons comma when car == COMMA_AT && isNil(comma.cdr()) ->
                    print(',').print('@').print(comma.car());
            default -> {
                print('(').print(car);
                ELispCons.ConsIterator i = cons.consIterator(1);
                while (i.hasNextCons()) {
                    sep();
                    if (i.hasNextCons()) {
                        cons = i.nextCons();
                        print(cons.car());
                    } else {
                        print('.').sep().print(cons.cdr());
                        break;
                    }
                }
                print(')').end();
            }
        }
    }

    public ELispPrint print(MuleString s) {
        func.print(s);
        return this;
    }

    private boolean printExisting(Object o) {
        if (stack.contains(o)) {
            Integer index = visited.computeIfAbsent(o, _ -> visited.size() + 1);
            print('#').printInt(index);
            return true;
        }
        return false;
    }

    @CompilerDirectives.TruffleBoundary
    public ELispPrint print(Object o) {
        if (o instanceof Constable) {
            if (o == Boolean.FALSE) {
                func.print(NIL.name());
            } else if (o == Boolean.TRUE) {
                func.print(T.name());
            } else if (o instanceof Long l) {
                func.print(MuleString.fromString(l.toString()));
            } else if (o instanceof Double d) {
                func.print(MuleString.fromString(d.toString()));
            } else {
                throw ELispSignals.error("unable to print: " + o + "(" + o.getClass() + ")");
            }
            return this;
        }

        // TODO: print-level
        if (printExisting(o)) {
            return this;
        }
        switch (o) {
            case ELispValue v -> v.display(this);
            case MuleString s -> func.print(s);
            default -> throw ELispSignals.error("unable to print: " + o + "(" + o.getClass() + ")");
        }
        return this;
    }

    public void flush() {
        func.flush();
    }

    public static ELispPrint fromBuffer(ELispBuffer buffer) {
        return new ELispPrint(new BufferPrintFunc(buffer));
    }

    public static ELispPrint fromMarker(ELispMarker marker) {
        return new ELispPrint(new MarkerPrintFunc(marker));
    }

    public static ELispPrint fromFunc(Object func) {
        return new ELispPrint(new FuncPrintFunc(func));
    }

    public static ELispPrint fromBuilder(MuleStringBuffer buffer) {
        return new ELispPrint(new StringPrintFunc(buffer));
    }

    public static MuleString toString(Object o) {
        MuleStringBuffer buffer = new MuleStringBuffer();
        fromBuilder(buffer).print(o).flush();
        return buffer.build();
    }

    private interface PrintFunc {
        void print(int c);
        void flush();
        default void print(MuleString s) {
            PrimitiveIterator.OfInt iterator = s.iterator(0);
            while (iterator.hasNext()) {
                print(iterator.nextInt());
            }
        }
    }

    private static abstract class BufferedPrintFunc implements PrintFunc {
        protected final MuleStringBuffer buffer;

        protected BufferedPrintFunc() {
            this(new MuleStringBuffer());
        }

        protected BufferedPrintFunc(MuleStringBuffer buffer) {
            this.buffer = buffer;
        }

        @Override
        public void print(int c) {
            buffer.appendCodePoint(c);
        }

        @Override
        public void print(MuleString s) {
            buffer.append(s);
        }
    }

    private static final class StringPrintFunc extends BufferedPrintFunc {
        StringPrintFunc(MuleStringBuffer buffer) {
            super(buffer);
        }

        @Override
        public void flush() {
        }
    }
    private static final class BufferPrintFunc extends BufferedPrintFunc {
        private final ELispBuffer buffer;

        private BufferPrintFunc(ELispBuffer buffer) {
            this.buffer = buffer;
        }

        @Override
        public void flush() {
            buffer.insert(super.buffer.build());
        }
    }

    private static final class MarkerPrintFunc extends BufferedPrintFunc {
        private final ELispMarker marker;

        private MarkerPrintFunc(ELispMarker marker) {
            this.marker = marker;
        }

        private ELispBuffer getBuffer() {
            ELispBuffer b = marker.getBuffer();
            return b == null ? ELispContext.get(null).currentBuffer() : b;
        }

        @Override
        public void flush() {
            ELispBuffer b = getBuffer();
            // TODO: Restore?
            b.setPoint(marker.getPosition());
            b.insert(super.buffer.build());
        }
    }

    private record FuncPrintFunc(Object func) implements PrintFunc {
        @Override
        public void print(int c) {
            BuiltInEval.FFuncall.funcall(func, new Object[]{(long) c});
        }

        @Override
        public void flush() {
        }
    }
}
