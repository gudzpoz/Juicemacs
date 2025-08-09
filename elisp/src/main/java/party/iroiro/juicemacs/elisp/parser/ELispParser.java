package party.iroiro.juicemacs.elisp.parser;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.forms.BuiltInLRead;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ast.ELispRootNodes;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.LocatedToken;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token;
import party.iroiro.juicemacs.elisp.parser.ELispLexer.Token.*;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.TruffleUtils;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

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
        ELispSymbol intern(TruffleString name);
        String applyShorthands(String symbol);
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

    @TruffleBoundary
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
            case Str(TruffleString value, int state) -> new ELispString(value, state);
            case Symbol(String value, boolean intern, boolean shorthand) -> {
                String symbol = value;
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
            case BoolVec(long length, TruffleString value) -> {
                byte[] bytes = new byte[(int) Math.ceilDiv(length, 8)];
                TruffleStringIterator iterator = value.createCodePointIteratorUncached(TruffleString.Encoding.UTF_32);
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
                ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
                builder.add(nextObject());
                int i = 1;
                while (!(peek() instanceof ParenClose)) {
                    if (peek() instanceof Dot) {
                        // (a b . ???
                        read();
                        if (peek() instanceof ParenClose) {
                            // (a b .) -> (a b \.)
                            builder.add(context.intern("."));
                            break;
                        }
                        // (a b . c)
                        Object cdr = nextObject();
                        if (!(read().token() instanceof ParenClose)) {
                            throw ELispSignals.invalidReadSyntax("Expected ')'");
                        }
                        // TODO: Understand what Emacs does for (#$ . FIXNUM)
                        // Irregular lists are most likely not source snippets,
                        // so we don't bother storing location info.
                        yield builder.buildWithCdr(cdr);
                    }
                    builder.add(nextObject());
                    i++;
                }
                LocatedToken endLocation = read();
                yield arrayConsList(builder, i, token, endLocation);
            }
            case RecordOpen() -> {
                ArrayList<Object> list = readList();
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
            case ClosureOpen() -> {
                AbstractELispClosure closure = AbstractELispClosure.create(
                        readVector(),
                        new AbstractELispClosure.ClosureCommons(source)
                );
                if (closure instanceof ELispBytecode function && source != null) {
                    function.setSourceLocation(
                            token.startLine(), token.startColumn(),
                            token.endLine(), token.endColumn()
                    );
                }
                yield closure;
            }
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

    private Object arrayConsList(ELispCons.ListBuilder builder, int count, LocatedToken startLocation, LocatedToken endLocation) {
        ELispCons list = asCons(builder.build());
        list.setSourceLocation(
                startLocation.startLine(), startLocation.startColumn(),
                endLocation.endLine(), endLocation.endColumn()
        );
        ELispCons cons = list;
        for (int i = 1; i < count; i++) {
            ELispCons next = asCons(cons.cdr());
            next.fillDebugInfo(list);
            cons = next;
        }
        return list;
    }

    private ELispCons quote(ELispSymbol quote, LocatedToken start) throws IOException {
        ELispCons cons = ELispCons.listOf(quote, nextObject());
        LocatedToken end = Objects.requireNonNull(lastRead);
        cons.setSourceLocation(start.startLine(), start.startColumn(), end.endLine(), end.endColumn());
        return cons;
    }

    private ArrayList<Object> readVector() throws IOException {
        ArrayList<Object> vector = new ArrayList<>();
        while (!(peek() instanceof SquareClose)) {
            vector.add(nextObject());
        }
        read();
        return vector;
    }

    private ArrayList<Object> readList() throws IOException {
        ArrayList<Object> vector = new ArrayList<>();
        while (!(peek() instanceof ParenClose)) {
            vector.add(nextObject());
        }
        read();
        return vector;
    }

    @Nullable
    private SourceSection getWholeSection(Source source) {
        LocatedToken end = Objects.requireNonNull(lastRead);
        return TruffleUtils.createSection(source, 1, 1, end.endLine(), end.endColumn());
    }

    public long getCodepointOffset() {
        return lexer.getCodePointOffset();
    }

    public boolean hasNext() throws IOException {
        return !(peek() instanceof EOF);
    }

    @TruffleBoundary
    public Object nextLisp() throws IOException {
        cyclicReferences.clear();
        readObjectsCompleted.clear();
        return nextObject();
    }

    private Object[] parse(@Nullable Source source) throws IOException {
        this.source = source;
        ArrayList<Object> expressions = new ArrayList<>();
        while (hasNext()) {
            expressions.add(nextLisp());
        }
        return expressions.toArray();
    }

    @TruffleBoundary
    public static ELispRootNode parse(ELispLanguage language, InternContext context, Source source) throws IOException {
        ELispParser parser = new ELispParser(context, source);
        boolean debug = context instanceof ELispContext c && c.options().debug();
        return parse(language, parser, source, debug);
    }

    @TruffleBoundary
    public static ELispRootNode parse(ELispLanguage language, InternContext context, Source source, ELispBuffer buffer)
            throws IOException {
        ELispParser parser = new ELispParser(context, new ELispLexer(CodePointReader.from(buffer, buffer.pointMin())));
        boolean debug = context instanceof ELispContext c && c.options().debug();
        return parse(language, parser, source, debug);
    }

    @TruffleBoundary
    public static ELispRootNode parseDebugEval(
            ELispLanguage language,
            InternContext context,
            Source source,
            ELispLexical.@Nullable Scope debugScope
    ) throws IOException {
        ELispParser parser = new ELispParser(context, source);
        ELispExpressionNode expr = ELispRootNodes.createRoot(parser.parse(source), debugScope);
        return new ELispRootNode(language, expr, parser.getWholeSection(source));
    }

    private static ELispRootNode parse(ELispLanguage language, ELispParser parser, Source source, boolean debug)
            throws IOException {
        // TODO: We might need a CompilerDirectives.transferToInterpreterAndInvalidate() here.
        ELispExpressionNode expr = ELispRootNodes.createMacroexpand(parser.parse(source), parser.isLexicallyBound());
        if (!debug) {
            source = Source.newBuilder(source).content(Source.CONTENT_NONE).build();
        }
        return new ELispRootNode(language, expr, parser.getWholeSection(source));
    }

    @TruffleBoundary
    public static Object read(InternContext context, String s) throws IOException {
        return read(context, Source.newBuilder("elisp", s, "").build());
    }

    public static Object read(InternContext context, Source source) throws IOException {
        ELispParser parser = new ELispParser(context, source);
        return parser.nextLisp();
    }

}
