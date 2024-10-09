package party.iroiro.juicemacs.elisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispLexical;
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
public abstract class ELispInterpretedNode extends ELispExpressionNode {

    public static ELispInterpretedNode create(Object expression) {
        return switch (expression) {
            case ELispSymbol symbol -> new ELispSymbolDereferenceNode(symbol);
            case ELispCons cons -> ELispInterpretedNodeFactory.ELispConsExpressionNodeGen.create(cons);
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
        return new ELispRootExpressions(expressions, lexical);
    }

    private static Object getIndirectFunction(Object function) {
        if (function instanceof ELispSymbol symbol) {
            function = symbol.getIndirectFunction();
        } else if (function instanceof ELispCons cons && cons.car() == LAMBDA) {
            function = BuiltInEval.FFunction.function(function);
        }
        return function;
    }

    private final static class ELispRootExpressions extends ELispInterpretedNode {
        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispExpressionNode node;

        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private boolean lexical;

        public ELispRootExpressions(Object[] expressions, boolean lexical) {
            this.node = BuiltInEval.FProgn.progn(expressions);
            this.lexical = lexical;
            adoptChildren();
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            if (lexical) {
                new ELispLexical(frame, null, null, List.of());
            }
            try (var _ = ELispLexical.withLexicalBinding(lexical)) {
                return node.executeGeneric(frame);
            }
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
    }

    private final static class ELispSymbolDereferenceNode extends ELispInterpretedNode {
        public static final int DYNAMIC = 1;
        @SuppressWarnings("FieldMayBeFinal")
        @CompilerDirectives.CompilationFinal
        private ELispSymbol symbol;

        @CompilerDirectives.CompilationFinal
        private VirtualFrame frame = null;
        @CompilerDirectives.CompilationFinal
        private int index = 0;

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
            if (index == 0) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                ELispLexical lexicalFrame = ELispLexical.getLexicalFrame(frame);
                ELispLexical.LexicalReference lexical = lexicalFrame == null ? null : lexicalFrame.getLexicalReference(frame, symbol);
                if (lexical == null) {
                    index = DYNAMIC;
                } else {
                    index = lexical.index();
                    this.frame = lexical.frame();
                }
            }
            if (index == DYNAMIC) {
                return symbol.getValue();
            }
            return ELispLexical.getVariable(
                    this.frame == null ? frame : this.frame,
                    index
            );
        }
    }

    private abstract static class ConsCallNode extends ELispExpressionNode {
        protected final Object function;

        @Children
        protected ELispExpressionNode[] args;

        protected ConsCallNode(Object function, ELispCons cons, boolean special) {
            this.function = function;
            this.args = initChildren(cons, special);
            adoptChildren();
        }

        @ExplodeLoop
        protected Object[] evalArgs(VirtualFrame frame) {
            int length = this.args.length;
            Object[] args = new Object[length];
            for (int i = 0; i < length; i++) {
                args[i] = this.args[i].executeGeneric(frame);
            }
            return args;
        }

        private static ELispExpressionNode[] initChildren(ELispCons cons, boolean special) {
            List<ELispExpressionNode> childrenList = new ArrayList<>();
            ELispCons.BrentTortoiseHareIterator argIterator = cons.listIterator(1);
            while (argIterator.hasNext()) {
                if (special) {
                    childrenList.add(new ELispLiteralExpressionNode(argIterator.next()));
                } else {
                    childrenList.add(ELispInterpretedNode.create(argIterator.next()));
                }
            }
            return childrenList.toArray(new ELispExpressionNode[0]);
        }

        public Object getFunction() {
            return function;
        }

        protected ELispFunctionObject getFunctionObject(Object function) {
            return switch (function) {
                case ELispSubroutine(ELispFunctionObject body, _) -> body;
                case ELispInterpretedClosure closure -> closure.getFunction();
                default -> throw new UnsupportedOperationException();
            };
        }
    }

    abstract static class ConsFunctionCallNode extends ConsCallNode {
        @Child
        @Nullable
        private ELispExpressionNode inlineLambdaNode = null;

        ConsFunctionCallNode(Object function, ELispCons cons) {
            super(function, cons, false);
            if (function instanceof ELispExpressionNode node) {
                inlineLambdaNode = node;
            }
            adoptChildren();
        }

        @Specialization
        public Object call(VirtualFrame frame, @Cached FunctionDispatchNode dispatchNode) {
            Object function = this.function;
            if (inlineLambdaNode != null) {
                function = inlineLambdaNode.executeGeneric(frame);
            }
            return dispatchNode.executeDispatch(this, getFunctionObject(function), evalArgs(frame));
        }
    }

    abstract static class ConsSpecialCallNode extends ConsCallNode {
        @Nullable
        @Child
        private ELispExpressionNode generated = null;

        ConsSpecialCallNode(Object function, ELispCons cons) {
            super(function, cons, true);
        }

        @Specialization
        public Object call(VirtualFrame frame, @Cached FunctionDispatchNode dispatchNode) {
            ELispExpressionNode form = generated;
            if (form == null) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                form = (ELispExpressionNode) dispatchNode.executeDispatch(this, getFunctionObject(function), evalArgs(frame));
                generated = form;
                adoptChildren();
            }
            return form.executeGeneric(frame);
        }
    }

    abstract static class ConsMacroCallNode extends ConsCallNode {
        @Nullable
        private Object generated = null;

        @Child
        @Nullable
        private ELispExpressionNode inlineLambdaNode = null;

        @Child
        @Nullable
        private ELispExpressionNode generatedNode = null;

        ConsMacroCallNode(Object function, ELispCons cons) {
            super(getIndirectFunction(((ELispCons) function).cdr()), cons, true);
            if (this.function instanceof ELispExpressionNode node) {
                inlineLambdaNode = node;
            }
            adoptChildren();
        }

        @Specialization
        public Object call(VirtualFrame frame, @Cached FunctionDispatchNode dispatchNode) {
            try (ELispLexical.Dynamic _ = ELispLexical.withLexicalBinding(true)) {
                Object function = this.function;
                if (inlineLambdaNode != null) {
                    function = inlineLambdaNode.executeGeneric(frame);
                }
                Object o = dispatchNode.executeDispatch(this, getFunctionObject(function), evalArgs(frame));
                ELispExpressionNode macro = generatedNode;
                Object generated = this.generated;
                if (macro == null || generated == null || !BuiltInFns.FEqual.equal(generated, o)) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    this.generated = o;
                    macro = ELispInterpretedNode.create(o);
                    this.generatedNode = macro;
                    adoptChildren();
                }
                return macro.executeGeneric(frame);
            }
        }
    }

    public abstract static class ELispConsExpressionNode extends ELispInterpretedNode {
        public final static int FORM_FUNCTION = 0;
        public final static int FORM_SPECIAL = 1;
        public final static int FORM_MACRO = 2;

        @CompilerDirectives.CompilationFinal
        private final ELispCons cons;

        @CompilerDirectives.CompilationFinal
        private volatile int type = -1;

        @Child
        @Nullable
        private volatile ConsCallNode callNode;

        public ELispConsExpressionNode(ELispCons cons) {
            this.cons = cons;
            this.callNode = null;
        }

        @Specialization
        public Object call(VirtualFrame frame) {
            Object function = getIndirectFunction(cons.car());
            int newType = switch (function) {
                case ELispSubroutine(_, boolean specialForm) when specialForm -> FORM_SPECIAL;
                case ELispCons c when c.car() == MACRO -> FORM_MACRO;
                default -> FORM_FUNCTION;
            };
            ConsCallNode node = callNode;
            if (node == null || type != newType || node.getFunction() != function) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                node = switch (newType) {
                    case FORM_FUNCTION -> ELispInterpretedNodeFactory.ConsFunctionCallNodeGen.create(function, cons);
                    case FORM_SPECIAL -> ELispInterpretedNodeFactory.ConsSpecialCallNodeGen.create(function, cons);
                    case FORM_MACRO -> ELispInterpretedNodeFactory.ConsMacroCallNodeGen.create(function, cons);
                    default -> throw new UnsupportedOperationException();
                };
                type = newType;
                callNode = node;
                adoptChildren();
            }
            return node.executeGeneric(frame);
        }

        @Override
        public SourceSection getSourceSection() {
            RootNode rootNode = getRootNode();
            if (rootNode == null) {
                return null;
            }
            SourceSection section = rootNode.getSourceSection();
            if (section == null) {
                return null;
            }
            return cons.getSourceSection(section.getSource());
        }
    }

}
