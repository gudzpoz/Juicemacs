package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.forms.BuiltInLRead;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.NumberVariant;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.*;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;

import java.io.EOFException;
import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

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

    private final ELispLexer lexer;
    private boolean lexicalBinding;

    public ELispParser(Source source) {
        this.lexer = new ELispLexer(source);
        try {
            if (peek() instanceof SetLexicalBindingMode(boolean value)) {
                lexicalBinding = value;
                read();
            } else {
                lexicalBinding = false;
            }
        } catch (IOException e) {
            lexicalBinding = false;
        }
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

    public boolean getLexicalBinding() {
        return lexicalBinding;
    }

    private final HashMap<Long, Object> cyclicReferences = new HashMap<>();
    private final HashSet<Object> readObjectsCompleted = new HashSet<>();

    @CompilerDirectives.TruffleBoundary
    private Object nextObject() throws IOException {
        Token token = read();
        return switch (token) {
            case EOF() -> throw new EOFException();
            case SkipToEnd() -> false; // TODO: Skip to EOF
            case SetLexicalBindingMode _ -> throw ELispSignals.invalidReadSyntax("Unexpected lexical binding mode");
            case Num(NumberVariant.FixNum(long value)) -> value;
            case Num(NumberVariant.BigNum(BigInteger value)) -> ELispBigNum.wrap(value);
            case Num(NumberVariant.Float(double value)) -> value;
            case Char(int value) -> (long) value;
            case Str(MutableTruffleString value) -> new ELispString(value);
            case Symbol(String value, boolean intern, boolean shorthand) -> {
                String symbol = value;
                if (shorthand) {
                    symbol = ELispContext.applyShorthands(symbol);
                }
                if (intern) {
                    yield ELispContext.intern(symbol);
                }
                yield new ELispSymbol(symbol);
            }
            case BoolVec(long length, MutableTruffleString value) -> {
                byte[] bytes = new byte[(int) Math.ceilDiv(length, 8)];
                TruffleStringIterator iterator = TruffleString.CreateCodePointIteratorNode.getUncached()
                        .execute(value, TruffleString.Encoding.UTF_16);
                for (int i = 0; i < length; i += 8) {
                    if (!iterator.hasNext()) {
                        throw ELispSignals.invalidReadSyntax("Unmatched bit vector length");
                    }
                    int codepoint = iterator.nextUncached();
                    if (codepoint > 0xFF) {
                        throw ELispSignals.invalidReadSyntax("Expected raw byte string");
                    }
                    bytes[i / 8] = (byte) codepoint;
                }
                if (iterator.hasNext()) {
                    throw ELispSignals.invalidReadSyntax("Unmatched bit vector length");
                }
                yield new ELispBoolVector(BitSet.valueOf(bytes), (int) length);
            }
            case Quote() -> quote(QUOTE); // 'a -> (quote a)
            case Function() -> quote(FUNCTION); // #'a -> (function a)
            case BackQuote() -> quote(BACKQUOTE); // `a -> (` a)
            case Unquote() -> quote(COMMA); // ,a -> (, a)
            case UnquoteSplicing() -> quote(COMMA_AT); // ,@a -> (,@ a)
            case Dot() -> ELispContext.intern("."); // [.] -> vec[ <symbol "."> ], (a . b) handled by ParenOpen
            case ParenOpen() -> {
                int line = lexer.getLine();
                int column = Math.max(lexer.getColumn() - 1, 1); // get to the position before the '('
                if (peek() instanceof ParenClose) {
                    read();
                    yield false;
                }
                ELispCons object = new ELispCons(nextObject());
                ELispCons tail = object;
                while (!(peek() instanceof ParenClose)) {
                    if (peek() instanceof Dot) {
                        // (a b . c)
                        read();
                        object.setCdr(nextObject());
                        if (!(read() instanceof ParenClose)) {
                            throw ELispSignals.invalidReadSyntax("Expected ')'");
                        }
                        // TODO: Understand what Emacs does for (#$ . FIXNUM)
                        object.setSourceLocation(line, column, lexer.getLine(), lexer.getColumn());
                        yield object;
                    }
                    tail.setCdr(new ELispCons(nextObject()));
                    tail = (ELispCons) tail.cdr();
                }
                read();
                object.setSourceLocation(line, column, lexer.getLine(), lexer.getColumn());
                yield object;
            }
            case RecordOpen() -> {
                List<Object> list = readList();
                Object type = list.getFirst();
                if (type instanceof ELispSymbol sym && sym == HASH_TABLE) {
                    yield ELispHashtable.hashTableFromPlist(list);
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
                    throw ELispSignals.invalidReadSyntax("Unexpected self reference");
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
            case ParenClose(), SquareClose() -> throw ELispSignals.invalidReadSyntax("Expected start of expression");
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
        while (!(peek() instanceof SquareClose)) {
            vector.add(nextObject());
        }
        read();
        return vector;
    }

    private List<Object> readList() throws IOException {
        List<Object> vector = new ArrayList<>();
        while (!(peek() instanceof ParenClose)) {
            vector.add(nextObject());
        }
        read();
        return vector;
    }

    public int getCodepointOffset() {
        return lexer.getCodepointOffset();
    }

    public boolean hasNext() throws IOException {
        return !(peek() instanceof EOF);
    }

    @CompilerDirectives.TruffleBoundary
    public Object nextLisp() throws IOException {
        cyclicReferences.clear();
        readObjectsCompleted.clear();
        return nextObject();
    }

    @CompilerDirectives.TruffleBoundary
    public static ELispRootNode parse(ELispLanguage language, Source source) throws IOException {
        ELispParser parser = new ELispParser(source);
        List<Object> expressions = new ArrayList<>();
        while (parser.hasNext()) {
            expressions.add(parser.nextLisp());
        }
        // TODO: We might need a CompilerDirectives.transferToInterpreterAndInvalidate() here.
        ELispExpressionNode expr = valueToExpression(expressions.toArray(), parser.getLexicalBinding());
        source = Source.newBuilder(source)
                .content(Source.CONTENT_NONE)
                .build();
        return new ELispRootNode(language, expr, source.createSection(
                1, 1,
                parser.lexer.getLine(), parser.lexer.getColumn()
        ));
    }

    @CompilerDirectives.TruffleBoundary
    public static Object read(String s) throws IOException {
        return read(Source.newBuilder("elisp", s, "").build());
    }

    public static Object read(Source source) throws IOException {
        ELispParser parser = new ELispParser(source);
        return parser.nextLisp();
    }

}
