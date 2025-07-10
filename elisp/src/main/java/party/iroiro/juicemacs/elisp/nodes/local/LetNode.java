package party.iroiro.juicemacs.elisp.nodes.local;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.collections.Pair;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ELispInterpretedNode;
import party.iroiro.juicemacs.elisp.nodes.GlobalVariableWriteNodeGen;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.LISTP;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class LetNode extends ELispExpressionNode {
    @SuppressWarnings("FieldMayBeFinal")
    @Children
    ELispExpressionNode[] letClauses = new ELispExpressionNode[0];

    @Nullable
    @CompilerDirectives.CompilationFinal
    ELispLexical lexicalBlock = null;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    ELispSymbol[] dynamicSymbols = new ELispSymbol[0];

    @SuppressWarnings("FieldMayBeFinal")
    @Child
    @Nullable
    ELispExpressionNode bodyNode = null;

    private final Object varlist;
    private final Object[] body;
    private final boolean letx;

    public LetNode(Object varlist, Object[] body, boolean letx) {
        this.varlist = varlist;
        this.body = body;
        this.letx = letx;
    }

    private boolean isDynamic(@Nullable ELispLexical block, ELispSymbol symbol) {
        if (block == null) {
            return true;
        }
        ELispLexical.@Nullable Scope upper = block.upperScope();
        if (upper != null) {
            if (upper.isDynamic(symbol)) {
                return true;
            }
        }
        return getContext().getStorageLazy(symbol).map(ValueStorage::isSpecial).orElse(false);
    }

    private ELispExpressionNode updateClauses() {
        @Nullable ELispExpressionNode bodyNode = this.bodyNode;
        if (bodyNode != null) {
            return bodyNode;
        }

        CompilerDirectives.transferToInterpreterAndInvalidate();
        Pair<ELispExpressionNode[], ELispSymbol[]> pair = parseClauses(varlist);
        ELispSymbol[] symbols = pair.getRight();
        ELispExpressionNode[] clauses = pair.getLeft();

        ArrayList<ELispSymbol> dynamicSymbols = new ArrayList<>();

        @Nullable ELispLexical block = ELispLexical.newBlock(this, symbols);
        int length = symbols.length;
        Iterator<?> sourceSectionProvider = ELispCons.iterate(varlist).iterator();
        for (int i = 0; i < length; i++) {
            ELispSymbol symbol = symbols[i];
            ELispExpressionNode clause = clauses[i];
            boolean isDynamic = isDynamic(block, symbol);
            if (isDynamic) {
                if (letx) {
                    clause = GlobalVariableWriteNodeGen.GlobalVariableDynamicSwapNodeGen.create(symbol, clause);
                }
                dynamicSymbols.add(symbol);
            }
            if (block != null) {
                if (isDynamic) {
                    block.markAsDynamic(symbol);
                } else {
                    int slot = i + ELispLexical.FRAME_SLOT_START;
                    clause = ELispFrameSlotWriteNode.createWrite(0, slot, clause);
                }
                clause = new BuiltInEval.ScopeWrapperNode(clause, block.newScope(letx ? i : 0));
            }
            if (sourceSectionProvider.hasNext() && sourceSectionProvider.next() instanceof ELispCons cons) {
                clause = new ELispInterpretedNode.SourceSectionWrapper(cons, clause);
            }
            clauses[i] = insert(clause);
            notifyInserted(clause);
        }
        this.letClauses = clauses;
        this.lexicalBlock = block;
        this.dynamicSymbols = dynamicSymbols.toArray(new ELispSymbol[0]);

        bodyNode = BuiltInEval.FProgn.progn(body);
        if (block != null) {
            bodyNode = new BuiltInEval.ScopeWrapperNode(bodyNode, block.newScope(length));
        }
        this.bodyNode = insert(bodyNode);
        notifyInserted(bodyNode);
        return bodyNode;
    }

    @ExplodeLoop
    public @Nullable Dynamic executeClauses(VirtualFrame frame, @Nullable ELispLexical block) {
        Object[] dynamicValues = dynamicSymbols.length == 0 ? null : new Object[dynamicSymbols.length];
        int dynamicI = 0;
        for (int i = 0; i < letClauses.length; i++) {
            boolean dynamic = block == null || block.slots()[i] == ELispLexical.DYNAMIC_VARIABLE_SLOT;
            ELispExpressionNode letClause = letClauses[i];
            if (dynamic) {
                assert dynamicValues != null;
                dynamicValues[dynamicI++] = letClause.executeGeneric(frame);
            } else {
                letClause.executeVoid(frame);
            }
        }
        if (!letx && dynamicValues != null) {
            for (int i = 0; i < dynamicValues.length; i++) {
                dynamicValues[i] = dynamicSymbols[i].swapThreadLocalValue(dynamicValues[i]);
            }
        }
        return dynamicValues == null ? null : Dynamic.preparePopDynamic(dynamicSymbols, dynamicValues);
    }

    @Override
    public void executeVoid(VirtualFrame frame) {
        ELispExpressionNode bodyNode = updateClauses();
        ELispLexical block = lexicalBlock;
        if (lexicalBlock != null) {
            frame = block.newFrame(frame);
        }
        try (Dynamic _ = executeClauses(frame, block)) {
            bodyNode.executeVoid(frame);
        }
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ELispExpressionNode bodyNode = updateClauses();
        ELispLexical block = lexicalBlock;
        if (lexicalBlock != null) {
            frame = block.newFrame(frame);
        }
        try (Dynamic _ = executeClauses(frame, block)) {
            return bodyNode.executeGeneric(frame);
        }
    }

    @Override
    public InstrumentableNode materializeInstrumentableNodes(Set<Class<? extends Tag>> materializedTags) {
        updateClauses();
        return this;
    }

    private static Pair<ELispExpressionNode[], ELispSymbol[]> parseClauses(Object varlist) {
        List<ELispSymbol> symbolList = new ArrayList<>();
        List<ELispExpressionNode> values = new ArrayList<>();
        for (Object assignment : ELispCons.iterate(varlist)) {
            Object symbol;
            Object value;
            if (toSym(assignment) instanceof ELispSymbol sym) {
                symbol = sym;
                value = false;
            } else {
                ELispCons cons = asCons(assignment);
                symbol = cons.car();
                if (cons.cdr() instanceof ELispCons cdr) {
                    if (!isNil(cdr.cdr())) {
                        throw ELispSignals.error("`let' bindings can have only one value-form");
                    }
                    value = cdr.car();
                } else {
                    if (!isNil(cons.cdr())) {
                        throw ELispSignals.wrongTypeArgument(LISTP, cons);
                    }
                    value = false;
                }
            }
            symbolList.add(asSym(symbol));
            values.add(ELispInterpretedNode.create(value));
        }
        ELispSymbol[] symbols = symbolList.toArray(ELispSymbol[]::new);
        ELispExpressionNode[] valueNodes = values.toArray(ELispExpressionNode[]::new);
        return Pair.create(valueNodes, symbols);
    }

}
