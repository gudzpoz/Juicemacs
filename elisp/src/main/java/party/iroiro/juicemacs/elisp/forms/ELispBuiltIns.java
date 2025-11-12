package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.source.Source;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/// The base class for built-in container classes like [BuiltInData]
///
/// NOTE: When subclassing this class, make sure to add the class to:
/// - [party.iroiro.juicemacs.elisp.runtime.ELispGlobalsBase#builtIns]
/// - [party.iroiro.juicemacs.elisp.runtime.pdump.ELispPortableDumper#FORY]
public abstract class ELispBuiltIns {

    static final ConcurrentHashMap<String, Source> BUILT_IN_SOURCES = new ConcurrentHashMap<>();

    protected ELispBuiltIns() {
        this(false);
    }

    /// Configures the built-ins defined inside this class
    ///
    /// Setting `inline` to `true` will ask the interpreter to inline
    /// the built-in functions so that the AST transitions from:
    /// ```
    /// ConsEvalNode
    /// -> ConsFunctionCallNode
    ///   -> (FunctionCall)
    ///     -> TheBuiltInExpressionNode
    /// ```
    /// to:
    /// ```
    /// ConsEvalNode
    /// -> ConsFunctionCallNode
    ///   -> TheBuiltInExpressionNode
    /// ```
    /// This might help the Truffle compiler to optimize the built-in functions
    /// by allowing each node to get its own specialization instead of sharing
    /// a common one defined in the function body. However, it might also
    /// bloat the AST and make Truffle bail out. Personally,
    /// I think files other than [BuiltInData] and [BuiltInFns] simply
    /// do not need to be inlined.
    ///
    /// @param inline Whether to inline the built-in functions
    public ELispBuiltIns(boolean inline) {
        this.inline = inline;
    }

    protected abstract List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories();

    private final boolean inline;

    @TruffleBoundary
    public final InitializationResult initialize(ELispLanguage language) {
        Source javaSource = Source.newBuilder(
                "java", "", this.getClass().getSimpleName() + ".java"
        )
                .content(Source.CONTENT_NONE)
                .internal(false)
                .build();
        BUILT_IN_SOURCES.put(this.getClass().getSimpleName(), javaSource);

        List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> factories = getNodeFactories();
        ArrayList<SemiInitializedBuiltIn> results = new ArrayList<>(factories.size());
        for (NodeFactory<? extends ELispExpressionNode> factory : factories) {
            for (ELispBuiltIn builtIn : factory.getNodeClass().getAnnotationsByType(ELispBuiltIn.class)) {
                boolean varArgs = builtIn.varArgs();
                ArrayList<ReadFunctionArgNode> args = new ArrayList<>(builtIn.maxArgs() + (varArgs ? 1 : 0));
                for (int i = 0; i < builtIn.maxArgs(); i++) {
                    args.add(new ReadFunctionArgNode(i));
                }
                if (varArgs) {
                    args.add(new ReadFunctionArgNode.ReadFunctionRestArgsNode(builtIn.maxArgs()));
                }
                ELispExpressionNode function = factory.createNode((Object) args.toArray(ReadFunctionArgNode[]::new));
                ReadFunctionArgNode.ArgCountVerificationNode wrapper = new ReadFunctionArgNode.ArgCountVerificationNode(
                        function,
                        builtIn.minArgs(),
                        varArgs ? -1 : builtIn.maxArgs()
                );
                ELispString symbol = ELispString.ofJava(builtIn.name());
                symbol.setImmutable();
                FunctionRootNode rootNode = new FunctionRootNode(language, false, wrapper, null); // NOPMD
                Object inlineInfo = switch (function) {
                    case ELispBuiltInBaseNode.InlineFactory _, ELispBuiltInBaseNode.SpecialFactory _ -> function;
                    default -> inline && !builtIn.rawArg() ? factory : null;
                };
                results.add(new SemiInitializedBuiltIn(
                        rootNode,
                        symbol,
                        new ELispSubroutine(
                                new ELispFunctionObject(rootNode.getCallTarget()),
                                builtIn,
                                inlineInfo
                        )
                ));
            }
        }
        return new InitializationResult(results);
    }

    public record InitializationResult(List<SemiInitializedBuiltIn> subroutines) {
    }

    /// A semi-initialized built-in function
    ///
    /// The built-in function is fully initialized yet and
    /// the caller should:
    ///
    /// 1. Intern the symbol
    /// 2. Set the function value of the symbol to the subroutine
    /// 3. Set the [FunctionRootNode#lispFunction] of the node to the interned symbol
    public record SemiInitializedBuiltIn(FunctionRootNode node, ELispString symbol, ELispSubroutine subroutine) {
    }
}
