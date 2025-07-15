package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.forms.BuiltInLRead;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.LocatedToken;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.*;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;

import java.io.EOFException;
import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asCons;

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
    public interface InternContext {
        ELispSymbol intern(String name);
        ELispSymbol intern(MuleString name);
        MuleString applyShorthands(MuleString symbol);
    }

    private @Nullable Source source = null;
    private final InternContext context;
    private final ELispLexer lexer;
    private boolean lexicalBinding;

    public ELispParser(InternContext context, Source source) {
        this(context, new ELispLexer(source));
    }

    public ELispParser(InternContext context, ELispLexer lexer) {
        this.context = context;
        this.lexer = lexer;
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
    private LocatedToken peekedToken = null;
    @Nullable
    private LocatedToken lastRead = null;

    private Token peek() throws IOException {
        if (peekedToken == null) {
            peekedToken = lexer.next();
        }
        return peekedToken.token();
    }

    private LocatedToken read() throws IOException {
        if (peekedToken == null) {
            lastRead = lexer.next();
        } else {
            lastRead = peekedToken;
            peekedToken = null;
        }
        return lastRead;
    }

    public boolean isLexicallyBound() {
        return lexicalBinding;
    }

    private final HashMap<Long, Object> cyclicReferences = new HashMap<>();
    private final ELispHashtable readObjectsCompleted = new ELispHashtable();

    @CompilerDirectives.TruffleBoundary
    private Object nextObject() throws IOException {
        LocatedToken token = read();
        return switch (token.token()) {
            case EOF() -> throw new EOFException();
            case SkipToEnd() -> false; // TODO: Skip to EOF
            case LoadFileName() -> LOAD_FILE_NAME.getValue();
            case SetLexicalBindingMode _ -> throw ELispSignals.invalidReadSyntax("Unexpected lexical binding mode");
            case FixNum(long value) -> value;
            case BigNum(BigInteger value) -> ELispBigNum.wrap(value);
            case FloatNum(double value) -> value;
            case Char(int value) -> (long) value;
            case Str(MuleString value) -> new ELispString(value);
            case Symbol(MuleString value, boolean intern, boolean shorthand) -> {
                MuleString symbol = value;
                if (shorthand) {
                    symbol = context.applyShorthands(symbol);
                }
                if (intern) {
                    ELispSymbol interned = context.intern(symbol);
                    if (interned == T) {
                        yield true;
                    }
                    if (interned == NIL) {
                        yield false;
                    }
                    yield interned;
                }
                yield new ELispSymbol(symbol);
            }
            case BoolVec(long length, MuleString value) -> {
                byte[] bytes = new byte[(int) Math.ceilDiv(length, 8)];
                PrimitiveIterator.OfInt iterator = value.iterator(0);
                for (int i = 0; i < length; i += 8) {
                    if (!iterator.hasNext()) {
                        throw ELispSignals.invalidReadSyntax("Unmatched bit vector length");
                    }
                    int codepoint = iterator.nextInt();
                    if (codepoint > 0xFF) {
                        throw ELispSignals.invalidReadSyntax("Expected raw byte string");
                    }
                    bytes[i / 8] = (byte) codepoint;
                }
                if (iterator.hasNext()) {
                    throw ELispSignals.invalidReadSyntax("Unmatched bit vector length");
                }
                yield ELispBoolVector.fromBytes(bytes, (int) length);
            }
            case Quote() -> quote(QUOTE, token); // 'a -> (quote a)
            case Function() -> quote(FUNCTION, token); // #'a -> (function a)
            case BackQuote() -> quote(BACKQUOTE, token); // `a -> (` a)
            case Unquote() -> quote(COMMA, token); // ,a -> (, a)
            case UnquoteSplicing() -> quote(COMMA_AT, token); // ,@a -> (,@ a)
            case Dot() -> context.intern("."); // [.] -> vec[ <symbol "."> ], (a . b) handled by ParenOpen
            case ParenOpen() -> {
                if (peek() instanceof ParenClose) {
                    read();
                    yield false;
                }
                ArrayList<Object> elements = new ArrayList<>();
                elements.add(nextObject());
                while (!(peek() instanceof ParenClose)) {
                    if (peek() instanceof Dot) {
                        // (a b . ???
                        read();
                        if (peek() instanceof ParenClose) {
                            // (a b .) -> (a b \.)
                            elements.add(context.intern("."));
                            break;
                        }
                        // (a b . c)
                        Object cdr = nextObject();
                        if (!(read().token() instanceof ParenClose)) {
                            throw ELispSignals.invalidReadSyntax("Expected ')'");
                        }
                        // TODO: Understand what Emacs does for (#$ . FIXNUM)
                        yield irregularList(elements, cdr);
                    }
                    elements.add(nextObject());
                }
                LocatedToken endLocation = read();
                yield arrayConsList(elements.toArray(), token, endLocation);
            }
            case RecordOpen() -> {
                List<Object> list = readList();
                Object type = list.getFirst();
                if (type == HASH_TABLE) {
                    yield ELispHashtable.hashTableFromPlist(list, true);
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
            case ClosureOpen() -> AbstractELispClosure.create(readVector(), source);
            case CharTableOpen() -> ELispCharTable.create(readVector());
            case SubCharTableOpen() -> ELispCharTable.SubTable.create(readVector());
            case CircularRef(long i) -> Objects.requireNonNull(cyclicReferences.get(i));
            case CircularDef(long i) -> {
                ELispCons placeholder = ELispCons.listOf(NIL);
                cyclicReferences.put(i, placeholder);
                Object def = nextObject();
                if (def == placeholder) {
                    // Emacs: "Catch silly games like #1=#1#"
                    throw ELispSignals.invalidReadSyntax("Unexpected self reference");
                }
                if (def instanceof ELispCons cons) {
                    readObjectsCompleted.put(placeholder, true);
                    placeholder.setCar(cons.car());
                    placeholder.setCdr(cons.cdr());
                    yield placeholder;
                } else {
                    BuiltInLRead.FLreadSubstituteObjectInSubtree.lreadSubstituteObjectInSubtree(
                            def,
                            placeholder,
                            readObjectsCompleted
                    );
                    readObjectsCompleted.put(def, true);
                    cyclicReferences.put(i, def);
                    yield def;
                }
            }
            case ParenClose(), SquareClose() -> throw ELispSignals.invalidReadSyntax("Expected start of expression");
        };
    }

    private Object arrayConsList(Object[] elements, LocatedToken startLocation, LocatedToken endLocation) {
        ELispCons list = asCons(ELispCons.listOf(elements));
        list.setSourceLocation(
                startLocation.startLine(), startLocation.startColumn(),
                endLocation.endLine(), endLocation.endColumn()
        );
        return list;
    }

    private Object irregularList(ArrayList<Object> elements, Object cdr) {
        return asCons(ELispCons.listWithCdrOf(elements.toArray(), cdr));
    }

    private ELispCons quote(ELispSymbol quote, LocatedToken start) throws IOException {
        ELispCons cons = ELispCons.listOf(quote, nextObject());
        LocatedToken end = Objects.requireNonNull(lastRead);
        cons.setSourceLocation(start.startLine(), start.startColumn(), end.endLine(), end.endColumn());
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

    private SourceSection getWholeSection(Source source) {
        LocatedToken end = Objects.requireNonNull(lastRead);
        try {
            return source.createSection(1, 1, end.endLine(), end.endColumn());
        } catch (IllegalArgumentException ignored) {
            return source.createSection(0, source.getLength());
        }
    }

    public long getCodepointOffset() {
        return lexer.getCodePointOffset();
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

    private Object[] parse(@Nullable Source source) throws IOException {
        this.source = source;
        List<Object> expressions = new ArrayList<>();
        while (hasNext()) {
            expressions.add(nextLisp());
        }
        return expressions.toArray();
    }

    @CompilerDirectives.TruffleBoundary
    public static ELispRootNode parse(ELispLanguage language, InternContext context, Source source) throws IOException {
        ELispParser parser = new ELispParser(context, source);
        boolean debug = context instanceof ELispContext c && c.options().debug();
        return parse(language, parser, source, debug);
    }

    @CompilerDirectives.TruffleBoundary
    public static ELispRootNode parse(ELispLanguage language, InternContext context, Source source, ELispBuffer buffer)
            throws IOException {
        ELispParser parser = new ELispParser(context, new ELispLexer(CodePointReader.from(buffer)));
        boolean debug = context instanceof ELispContext c && c.options().debug();
        return parse(language, parser, source, debug);
    }

    @CompilerDirectives.TruffleBoundary
    public static ELispRootNode parseDebugEval(
            ELispLanguage language,
            InternContext context,
            Source source,
            ELispLexical.@Nullable Scope debugScope
    ) throws IOException {
        ELispParser parser = new ELispParser(context, source);
        ELispExpressionNode expr = ELispInterpretedNode.createRoot(parser.parse(source), debugScope);
        return new ELispRootNode(language, expr, parser.getWholeSection(source));
    }

    private static ELispRootNode parse(ELispLanguage language, ELispParser parser, Source source, boolean debug)
            throws IOException {
        // TODO: We might need a CompilerDirectives.transferToInterpreterAndInvalidate() here.
        ELispExpressionNode expr = ELispInterpretedNode.createMacroexpand(parser.parse(source), parser.isLexicallyBound());
        if (!debug) {
            source = Source.newBuilder(source).content(Source.CONTENT_NONE).build();
        }
        return new ELispRootNode(language, expr, parser.getWholeSection(source));
    }

    @CompilerDirectives.TruffleBoundary
    public static Object read(InternContext context, String s) throws IOException {
        return read(context, Source.newBuilder("elisp", s, "").build());
    }

    public static Object read(InternContext context, Source source) throws IOException {
        ELispParser parser = new ELispParser(context, source);
        return parser.nextLisp();
    }

}
