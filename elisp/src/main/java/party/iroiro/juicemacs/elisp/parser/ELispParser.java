package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInLRead;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.NumberVariant;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.TokenData.*;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

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

    @Nullable
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

    public boolean getLexicalBinding() {
        return lexicalBinding;
    }

    private final HashMap<Long, Object> cyclicReferences = new HashMap<>();
    private final HashSet<Object> readObjectsCompleted = new HashSet<>();

    private Object nextObject() throws IOException {
        Token token = read();
        return switch (token.data()) {
            case EOF() -> throw new IOException("Unexpected EOF");
            case SkipToEnd() -> false; // TODO: Skip to EOF
            case SetLexicalBindingMode(boolean value) -> {
                lexicalBinding = value;
                yield nextObject();
            }
            case Num(NumberVariant.FixNum(long value)) -> value;
            case Num(NumberVariant.BigNum(BigInteger value)) -> new ELispBigNum(value);
            case Num(NumberVariant.Float(double value)) -> value;
            case Char(int value) -> (long) value;
            case Str(MutableTruffleString value) -> new ELispString(value);
            case Symbol(String value, boolean intern, boolean shorthand) -> {
                String symbol = value;
                if (shorthand) {
                    symbol = context.applyShorthands(symbol);
                }
                if (intern) {
                    if (value.equals("nil")) {
                        yield false;
                    }
                    if (value.equals("t")) {
                        yield true;
                    }
                    yield context.intern(symbol);
                }
                yield new ELispSymbol(symbol);
            }
            case BoolVec(long length, MutableTruffleString value) -> {
                byte[] bytes = new byte[(int) Math.ceilDiv(length, 8)];
                TruffleStringIterator iterator = TruffleString.CreateCodePointIteratorNode.getUncached()
                        .execute(value, TruffleString.Encoding.UTF_16);
                for (int i = 0; i < length; i += 8) {
                    if (!iterator.hasNext()) {
                        throw new IOException("Unmatched bit vector length");
                    }
                    int codepoint = iterator.nextUncached();
                    if (codepoint > 0xFF) {
                        throw new IOException("Expected raw byte string");
                    }
                    bytes[i / 8] = (byte) codepoint;
                }
                if (iterator.hasNext()) {
                    throw new IOException("Unmatched bit vector length");
                }
                yield new ELispBoolVector(BitSet.valueOf(bytes), (int) length);
            }
            case Quote() -> quote(context.QUOTE); // 'a -> (quote a)
            case Function() -> quote(context.FUNCTION); // #'a -> (function a)
            case BackQuote() -> quote(context.BACKQUOTE); // `a -> (` a)
            case Unquote() -> context.COMMA;
            case UnquoteSplicing() -> context.COMMA_AT;
            case Dot() -> context.intern("."); // [.] -> vec[ <symbol "."> ], (a . b) handled by ParenOpen
            case ParenOpen() -> {
                if (peek().data() instanceof ParenClose) {
                    read();
                    yield NIL;
                }
                ELispCons object = new ELispCons(nextObject());
                ELispCons tail = object;
                while (!(peek().data() instanceof ParenClose)) {
                    if (peek().data() instanceof Dot) {
                        // (a b . c)
                        read();
                        object.setCdr(nextObject());
                        if (!(read().data() instanceof ParenClose)) {
                            throw new IOException("Expected ')'");
                        }
                        // TODO: Understand what Emacs does for (#$ . FIXNUM)
                        yield object;
                    }
                    tail.setCdr(new ELispCons(nextObject()));
                    tail = (ELispCons) tail.cdr();
                }
                read();
                yield object;
            }
            case RecordOpen() -> {
                List<Object> list = readList();
                Object type = list.getFirst();
                if (type instanceof ELispSymbol sym && sym == context.HASH_TABLE) {
                    yield ELispHashtable.hashTableFromPlist(context, list);
                }
                yield new ELispRecord(list);
            }
            case StrWithPropsOpen() -> {
                List<Object> list = readList();
                ELispString base = (ELispString) list.getFirst();
                base.syncFromPlist(list);
                yield base;
            }
            case SquareOpen() -> new ELispVector(readVector());
            case ByteCodeOpen() -> ELispByteCode.create(readVector());
            case CharTableOpen() -> ELispCharTable.create(readVector());
            case SubCharTableOpen() -> ELispCharTable.SubTable.create(readVector());
            case CircularRef(long i) -> Objects.requireNonNull(cyclicReferences.get(i));
            case CircularDef(long i) -> {
                ELispCons placeholder = new ELispCons(NIL);
                cyclicReferences.put(i, placeholder);
                Object def = nextObject();
                if (def == placeholder) {
                    // Emacs: "Catch silly games like #1=#1#"
                    throw new IOException("Unexpected self reference");
                }
                if (def instanceof ELispCons cons) {
                    readObjectsCompleted.add(placeholder);
                    placeholder.setCar(cons.car());
                    placeholder.setCdr(cons.cdr());
                    yield placeholder;
                } else {
                    readObjectsCompleted.add(def);
                    BuiltInLRead.FLreadSubstituteObjectInSubtree.lreadSubstituteObjectInSubtree(
                            def,
                            placeholder,
                            readObjectsCompleted
                    );
                    cyclicReferences.put(i, def);
                    yield def;
                }
            }
            case ParenClose(), SquareClose() -> throw new IOException("Expected start of expression");
        };
    }

    private ELispCons quote(ELispSymbol quote) throws IOException {
        ELispCons cons = new ELispCons(quote);
        ELispCons cdr = new ELispCons(nextObject());
        cons.setCdr(cdr);
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

    public Object nextLisp() throws IOException {
        lexicalBinding = false;
        cyclicReferences.clear();
        readObjectsCompleted.clear();
        return nextObject();
    }

    public static ELispExpressionNode parse(Source source, ELispContext context) throws IOException {
        return context.valueToExpression(read(source, context));
    }

    public static Object read(Source source, ELispContext context) throws IOException {
        ELispParser parser = new ELispParser(source, context);
        return parser.nextLisp();
    }

}
