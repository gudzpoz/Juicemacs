package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.source.Source;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.mule.MuleString;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

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

    public final InitializationResult initialize(ELispLanguage language) {
        Source javaSource = Source.newBuilder(
                "java", "", this.getClass().getSimpleName() + ".java"
        )
                .content(Source.CONTENT_NONE)
                .internal(false)
                .build();
        BUILT_IN_SOURCES.put(this.getClass().getSimpleName(), javaSource);

        List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> factories = getNodeFactories();
        List<SemiInitializedBuiltIn> results = new ArrayList<>(factories.size());
        for (NodeFactory<? extends ELispExpressionNode> factory : factories) {
            for (ELispBuiltIn builtIn : factory.getNodeClass().getAnnotationsByType(ELispBuiltIn.class)) {
                boolean varArgs = builtIn.varArgs();
                List<ReadFunctionArgNode> args = new ArrayList<>(builtIn.maxArgs() + (varArgs ? 1 : 0));
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
                MuleString symbol = MuleString.fromString(builtIn.name());
                FunctionRootNode rootNode = new FunctionRootNode(language, false, wrapper, null); // NOPMD
                ELispSubroutine.InlineInfo inlineInfo = getInlineInfo(factory, builtIn, function);
                results.add(new SemiInitializedBuiltIn(
                        rootNode,
                        symbol,
                        new ELispSubroutine(
                                new ELispFunctionObject(rootNode.getCallTarget()),
                                inlineInfo,
                                builtIn
                        )
                ));
            }
        }
        return new InitializationResult(results);
    }

    private ELispSubroutine.@Nullable InlineInfo getInlineInfo(
            NodeFactory<? extends ELispExpressionNode> factory,
            ELispBuiltIn builtIn,
            ELispExpressionNode function
    ) {
        ELispSubroutine.@Nullable InlineInfo inlineInfo = null;
        @Nullable Object inliner = null;
        if (function instanceof ELispBuiltInBaseNode.InlineFactory inlineFactory) {
            inliner = inlineFactory;
        } else if (inline && !builtIn.rawArg()) {
            inliner = factory;
        }
        if (inliner != null || builtIn.rawArg()) {
            inlineInfo = new ELispSubroutine.InlineInfo(builtIn, inliner);
        }
        return inlineInfo;
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
    public record SemiInitializedBuiltIn(FunctionRootNode node, MuleString symbol, ELispSubroutine subroutine) {
    }
}
