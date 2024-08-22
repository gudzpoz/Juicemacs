package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleString.Encoding;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.NumberVariant;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.*;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

/**
 * A ELisp parser and reader
 *
 * <p>
 * Similar to the {@code read} ELisp function, this class serves to read
 * ELisp as S-expressions, and then, optionally, convert the executable
 * parts into Truffle nodes.
 * </p>
 */
public class ELispParser {

    private static final Object NIL = Boolean.FALSE;

    private final ELispLexer lexer;
    private final ELispContext context;

    public ELispParser(Source source, ELispContext context) {
        this.lexer = new ELispLexer(source);
        this.context = context;
    }

    private Token peekedToken = null;

    private Token peek() throws IOException {
        if (peekedToken == null) {
            peekedToken = lexer.next();
        }
        return peekedToken;
    }

    private Token read() throws IOException {
        Token token = peek();
        peekedToken = null;
        return token;
    }

    private boolean lexicalBinding = false;
    
    private Object nextObject() throws IOException {
        Token token = read();
        return switch (token.data()) {
            case EOF() -> throw new IOException("Unexpected EOF");
            case SetLexicalBindingMode(boolean value) -> {
                lexicalBinding = value;
                yield nextObject();
            }
            case Num(NumberVariant.FixNum(long value)) -> value;
            case Num(NumberVariant.BigNum(BigInteger value)) -> new ELispBigNum(value);
            case Num(NumberVariant.Float(double value)) -> value;
            case Char(int value) -> (long) value;
            case Str(String value) ->
                    TruffleString.fromConstant(value, Encoding.UTF_8).asMutableTruffleStringUncached(Encoding.UTF_8);
            case Symbol(String value, boolean intern, boolean shorthand) -> {
                String symbol = value;
                if (shorthand) {
                    symbol = context.applyShorthands(symbol);
                }
                yield intern ? context.intern(symbol) : new ELispSymbol(symbol);
            }
            case BoolVec(long length, String value) -> {
                byte[] bytes = new byte[(int) Math.ceilDiv(length, 8)];
                int len = Math.min(bytes.length, value.length());
                for (int i = 0; i < len; i++) {
                    bytes[i] = (byte) value.charAt(i);
                }
                yield BitSet.valueOf(bytes);
            }
            case Function(), Quote() -> quote(ELispContext.QUOTE);
            case BackQuote() -> quote(ELispContext.BACKQUOTE);
            case Unquote() -> ELispContext.UNQUOTE;
            case UnquoteSplicing() -> ELispContext.UNQUOTE_SPLICING;
            case ParenOpen() -> {
                if (peek().data() instanceof ParenClose) {
                    read();
                    yield NIL;
                }
                ELispCons object = new ELispCons();
                object.car = nextObject();
                if (peek().data() instanceof Dot) {
                    read();
                    object.cdr = nextObject();
                    yield object;
                }
                ELispCons tail = object;
                while (!(peek().data() instanceof ParenClose)) {
                    tail.cdr = new ELispCons();
                    tail = (ELispCons) tail.cdr;
                    tail.car = nextObject();
                }
                read();
                yield object;
            }
            case SquareOpen() -> readVector();
            case ByteCodeOpen() -> readVector(); // TODO: Convert to byte-code function
            case RecordOpen() -> readList(); // TODO: Convert to record
            // case CircularRef(long _) -> {}
            // case CircularDef(long _) -> {}
            case ParenClose(), SquareClose(), Dot() -> throw new IOException("Expected start of expression");
            default -> throw new UnsupportedOperationException("TODO");
        };
    }

    private ELispCons quote(ELispSymbol quote) throws IOException {
        ELispCons cons = new ELispCons();
        cons.car = quote;
        ELispCons cdr = new ELispCons();
        cons.cdr = cdr;
        cdr.car = nextObject();
        return cons;
    }

    private List<Object> readVector() throws IOException {
        List<Object> vector = new ArrayList<>();
        while (!(peek().data() instanceof SquareClose)) {
            vector.add(nextObject());
        }
        read();
        return vector;
    }

    private List<Object> readList() throws IOException {
        List<Object> vector = new ArrayList<>();
        while (!(peek().data() instanceof ParenClose)) {
            vector.add(nextObject());
        }
        read();
        return vector;
    }

    private ELispExpressionNode nextExpression() throws IOException {
        return context.valueToExpression(nextObject());
    }

    public static ELispExpressionNode parse(Source source, ELispContext context) throws IOException {
        ELispParser parser = new ELispParser(source, context);
        return parser.nextExpression();
    }

}
