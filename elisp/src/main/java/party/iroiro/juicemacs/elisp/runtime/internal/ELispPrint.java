package party.iroiro.juicemacs.elisp.runtime.internal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.strings.TruffleString;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.parser.ELispLexer;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;

import java.lang.constant.Constable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.PrimitiveIterator;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.MAX_5_BYTE_CHAR;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.NO_BREAK_SPACE;
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
        int c = b >= 0 ? b : ((b & 0x7F) + MAX_5_BYTE_CHAR + 1);
        if (inString) {
            print(c);
        } else {
            func.print(c);
        }
    }

    public boolean isPrintableChar(int c) {
        if (c > Character.MAX_CODE_POINT || Character.isISOControl(c)) {
            return false;
        }
        Character.UnicodeBlock block = Character.UnicodeBlock.of(c);
        return block != null && block != Character.UnicodeBlock.SPECIALS;
    }

    @CompilerDirectives.TruffleBoundary
    public ELispPrint print(int c) {
        if (inString) {
            if (c == '"' || c == '\\') {
                func.print('\\');
                // fallthrough
            } else if (!isPrintableChar(c)) {
                if (c < 128 || c > MAX_5_BYTE_CHAR) {
                    if (c > MAX_5_BYTE_CHAR) {
                        c = c - MAX_5_BYTE_CHAR + 127;
                    }
                    func.print('\\');
                    print(Integer.toString(c, 8));
                } else if (c <= 0xFFFF) {
                    String s = Integer.toString(c, 16);
                    func.print('\\');
                    func.print('u');
                    print("0".repeat(4 - s.length()) + s);
                } else {
                    String s = Integer.toString(c, 16);
                    func.print('\\');
                    func.print('U');
                    print("0".repeat(8 - s.length()) + s);
                }
                return this;
            }
        }
        func.print(c);
        return this;
    }

    public ELispPrint print(String s) {
        func.print(new ELispString(s));
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
        print(new ELispString(Integer.toString(i)).asTruffleStringUncached());
        return this;
    }

    public void printCons(ELispCons cons) {
        Object car = cons.car();
        int top = stack.size();
        start(cons);
        switch (cons.cdr()) {
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
                while (true) {
                    if (cons.cdr() instanceof ELispCons next) {
                        cons = next;
                        if (sep().printExisting(cons)) {
                            break;
                        }
                        start(cons);
                        print(cons.car());
                    } else {
                        if (!isNil(cons.cdr())) {
                            sep().print('.').sep().print(cons.cdr());
                        }
                        break;
                    }
                }
                print(')').end();
            }
        }
        while (stack.size() > top) {
            stack.removeLast();
        }
    }

    private boolean isDigit(int c) {
        return '0' <= c && c <= '9';
    }

    private boolean isNumber(String str) {
        return ELispLexer.INTEGER_PATTERN.matcher(str).matches() || ELispLexer.FLOAT_PATTERN.matcher(str).matches();
    }

    private boolean isSpecialChar(int c) {
        return c == '\"' || c == '\\' || c == '\''
                || c == ';' || c == '#' || c == '(' || c == ')'
                || c == ',' || c == '`'
                || c == '[' || c == ']' || c <= ' '
                || c == NO_BREAK_SPACE;
    }

    public void printSymbol(ELispSymbol symbol) {
        // TODO: symbol interned -> #: prefix
        String name = symbol.name();
        int length = name.length();
        if (length == 0) {
            print('#').print('#');
            return;
        }
        // If the symbol looks similar to a number, prefix it with a backslash
        int c1 = name.charAt(0);
        int numStart = ((c1 == '-' || c1 == '+') && name.length() > 1) ? 1 : 0;
        int c2 = name.charAt(numStart);
        if (((isDigit(c2) || c2 == '.') && isNumber(name)) || c1 == '?') {
            print('\\');
        }

        PrimitiveIterator.OfInt iterator = name.codePoints().iterator();
        while (iterator.hasNext()) {
            int c = iterator.nextInt();
            if (isSpecialChar(c)) {
                print('\\');
            }
            print(c);
        }
    }

    public ELispPrint print(ELispString s) {
        func.print(s);
        return this;
    }

    @CompilerDirectives.TruffleBoundary
    private boolean printExisting(Object o) {
        if (stack.contains(o)) {
            Integer index = visited.computeIfAbsent(o, _ -> visited.size() + 1); // NOPMD
            print('#').printInt(index);
            return true;
        }
        return false;
    }

    @CompilerDirectives.TruffleBoundary
    public ELispPrint print(Object o) {
        if (o instanceof Constable) {
            if (o == Boolean.FALSE) {
                print("nil");
            } else if (o == Boolean.TRUE) {
                print("t");
            } else if (o instanceof Long l) {
                TruffleString s = TruffleString.FromLongNode.getUncached().execute(l, TruffleString.Encoding.UTF_32, false);
                func.print(new ELispString(s, StringSupport.STATE_ASCII));
            } else if (o instanceof Double d) {
                TruffleString s = BuiltInData.FNumberToString.numberToStringFloat(d).asTruffleStringUncached();
                func.print(new ELispString(s, StringSupport.STATE_ASCII));
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
            case TruffleString s -> func.print(new ELispString(s));
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

    public static ELispPrint fromBuilder(MuleStringBuilder buffer) {
        return new ELispPrint(new StringPrintFunc(buffer));
    }

    public static TruffleString toString(Object o) {
        MuleStringBuilder buffer = new MuleStringBuilder();
        fromBuilder(buffer).print(o).flush();
        return buffer.build();
    }

    private interface PrintFunc {
        void print(int c);
        void flush();

        @CompilerDirectives.TruffleBoundary
        default void print(ELispString s) {
            PrimitiveIterator.OfInt iterator = s.iterator(0);
            while (iterator.hasNext()) {
                print(iterator.nextInt());
            }
        }
    }

    private static abstract class BufferedPrintFunc implements PrintFunc {
        protected final MuleStringBuilder buffer;

        protected BufferedPrintFunc() {
            this(new MuleStringBuilder());
        }

        protected BufferedPrintFunc(MuleStringBuilder buffer) {
            this.buffer = buffer;
        }

        @Override
        public void print(int c) {
            buffer.appendCodePoint(c);
        }

        @Override
        public void print(ELispString s) {
            buffer.appendString(s);
        }
    }

    private static final class StringPrintFunc extends BufferedPrintFunc {
        StringPrintFunc(MuleStringBuilder buffer) {
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
            buffer.insert(super.buffer.buildString());
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
            b.setPoint(marker.point());
            b.insert(super.buffer.buildString());
        }
    }

    private record FuncPrintFunc(Object func) implements PrintFunc {
        @Override
        public void print(int c) {
            BuiltInEval.FFuncall.funcall(null, func, (long) c);
        }

        @Override
        public void flush() {
        }
    }
}
