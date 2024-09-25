package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispBindingScope;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispInterpretedClosure;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSubroutine;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.util.ArrayList;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.*;

/**
 * Special nodes for interpreted ELisp
 *
 * <p>
 * Interpreted ELisp is too dynamic to compile to a constant AST. This class (and its subclasses)
 * are used to represent interpreted ELisp expressions, somehow dynamically.
 * </p>
 *
 * <h2>Dynamically Constant AST</h2>
 * <p>
 * Basically, what it does is to try to "predict" the AST of the expression at compile time, and
 * dynamically "replaces" the nodes that we have had a wrong prediction.
 * </p>
 */
public abstract sealed class ELispInterpretedNode extends ELispExpressionNode {

    public static ELispInterpretedNode create(Object expression) {
        return switch (expression) {
            case ELispSymbol symbol -> new ELispSymbolDereferenceNode(symbol);
            case ELispCons cons -> new ELispConsExpressionNode(cons);
            default -> new ELispLiteralExpressionNode(expression);
        };
    }

    public static ELispInterpretedNode[] create(Object[] expressions) {
        ELispInterpretedNode[] nodes = new ELispInterpretedNode[expressions.length];
        for (int i = 0; i < expressions.length; i++) {
            nodes[i] = ELispInterpretedNode.create(expressions[i]);
        }
        return nodes;
    }

    public static ELispInterpretedNode create(Object[] expressions, boolean lexical) {
        return new ELispInterpretedExpressions(expressions, lexical);
    }

    protected abstract Object getInterpreted();

    private static Object getIndirectFunction(Object function) {
        if (function instanceof ELispSymbol symbol) {
            function = symbol.getIndirectFunction();
        } else {
            function = BuiltInEval.FFunction.function(function);
        }
        return function;
    }

    private final static class ELispInterpretedExpressions extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Children
        private ELispInterpretedNode[] nodes;

        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private boolean lexical;

        public ELispInterpretedExpressions(Object[] expressions, boolean lexical) {
            this.nodes = create(expressions);
            this.lexical = lexical;
        }

        @Override
        protected Object getInterpreted() {
            return nodes;
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Object last = false;
            try (var _ = ELispBindingScope.withLexicalBinding(lexical)) {
                for (ELispInterpretedNode node : nodes) {
                    last = node.executeGeneric(frame);
                }
            }
            return last;
        }
    }

    private final static class ELispLiteralExpressionNode extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private Object literal;

        public ELispLiteralExpressionNode(Object literal) {
            this.literal = literal;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return literal;
        }

        @Override
        protected Object getInterpreted() {
            return literal;
        }
    }

    private final static class ELispSymbolDereferenceNode extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private ELispSymbol symbol;

        public ELispSymbolDereferenceNode(ELispSymbol symbol) {
            this.symbol = symbol;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            if (symbol == NIL) {
                return false;
            }
            if (symbol == T) {
                return true;
            }
            Object lexical = ELispBindingScope.getLexical(symbol);
            return lexical == null ? symbol.getValue() : lexical;
        }

        @Override
        protected Object getInterpreted() {
            return symbol;
        }
    }

    private record SpecialAssumption(Object indirectFunction, int special) {
        public final static int FORM_FUNCTION = 0;
        public final static int FORM_SPECIAL = 1;
        public final static int FORM_MACRO = 2;

        public static SpecialAssumption create(Object function) {
            // JDK-8332106: VerifyError when using switch pattern in this(...) or super(...)
            // We have to use a non-constructor method here.
            return new SpecialAssumption(
                    function,
                    switch (function) {
                        case ELispSubroutine(_, boolean specialForm) when specialForm -> FORM_SPECIAL;
                        case ELispCons cons when cons.car() == MACRO -> FORM_MACRO;
                        default -> FORM_FUNCTION;
                    }
            );
        }
    }

    private final static class ELispConsExpressionNode extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private ELispCons cons;

        @CompilerDirectives.CompilationFinal
        private volatile SpecialAssumption cachedSpecial;

        @Child
        @Nullable
        private volatile ELispExpressionNode functionNode = null;

        @Children
        private ELispExpressionNode[] children;

        @Child
        @Nullable
        private volatile ELispExpressionNode generated = null;

        public ELispConsExpressionNode(ELispCons cons) {
            this.cons = cons;
            this.cachedSpecial = SpecialAssumption.create(getIndirectFunction(cons.car()));
            initChildren();
            adoptChildren();
        }

        @Override
        protected Object getInterpreted() {
            return cons;
        }

        @CompilerDirectives.TruffleBoundary
        private void initChildren() {
            List<ELispExpressionNode> childrenList = new ArrayList<>();
            ELispCons.BrentTortoiseHareIterator argIterator = cons.listIterator(1);
            int special = cachedSpecial.special;
            while (argIterator.hasNext()) {
                if (special == SpecialAssumption.FORM_FUNCTION) {
                    childrenList.add(ELispInterpretedNode.create(argIterator.next()));
                } else {
                    childrenList.add(new ELispLiteralExpressionNode(argIterator.next()));
                }
            }
            if (children == null) {
                children = childrenList.toArray(new ELispExpressionNode[0]);
            } else {
                for (int i = 0; i < children.length; i++) {
                    children[i].replace(childrenList.get(i));
                }
            }
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            SpecialAssumption special = cachedSpecial;
            Object function = getIndirectFunction(cons.car());
            if (special == null || special.indirectFunction != function) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                int specialType = special == null ? -1 : special.special;
                cachedSpecial = SpecialAssumption.create(function);
                if (specialType != cachedSpecial.special) {
                    initChildren();
                }
                ELispExpressionNode prevGen = generated;
                if (prevGen != null) {
                    generated = null;
                }
                if (cachedSpecial.special == SpecialAssumption.FORM_MACRO) {
                    function = getIndirectFunction(((ELispCons) function).cdr());
                }
                if (function instanceof ELispExpressionNode fNode) {
                    functionNode = fNode;
                } else {
                    functionNode = null;
                }
                adoptChildren();
            }

            ELispExpressionNode functionNode = this.functionNode;
            if (functionNode != null) {
                function = functionNode.executeGeneric(frame);
            }

            Object[] args = new Object[children.length];
            for (int i = 0; i < children.length; i++) {
                args[i] = children[i].executeGeneric(frame);
            }

            int type = cachedSpecial.special;

            if (type == SpecialAssumption.FORM_MACRO) {
                try (var _ = ELispBindingScope.withLexicalBinding(true)) {
                    Object o = callFunction(function, args);
                    ELispExpressionNode macro = generated;
                    if (macro == null || (
                            macro instanceof ELispInterpretedNode iNode &&
                            !BuiltInFns.FEqual.equal(iNode.getInterpreted(), o)
                    )) {
                        CompilerDirectives.transferToInterpreterAndInvalidate();
                        macro = ELispInterpretedNode.create(o);
                        generated = macro;
                        adoptChildren();
                    }
                    return macro.executeGeneric(frame);
                }
            } else if (type == SpecialAssumption.FORM_SPECIAL) {
                ELispExpressionNode form = generated;
                if (form == null) {
                    form = (ELispExpressionNode) callFunction(function, args);
                    generated = form;
                }
                return form.executeGeneric(frame);
            } else {
                return callFunction(function, args);
            }
        }

        private Object callFunction(Object function, Object[] args) {
            return switch (function) {
                case ELispSubroutine(CallTarget body, _) -> body.call(args);
                case ELispInterpretedClosure closure -> closure.getFunction().callTarget().call(args);
                default -> throw new UnsupportedOperationException();
            };
        }
    }

}
