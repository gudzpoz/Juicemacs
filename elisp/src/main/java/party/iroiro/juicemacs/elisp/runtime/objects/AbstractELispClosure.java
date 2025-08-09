package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInData.isMultibyte;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CLOSURE_STACK_DEPTH;

public sealed abstract class AbstractELispClosure extends AbstractELispVector
        permits ELispInterpretedClosure, ELispBytecode {
    protected final ClosureCommons commons;
    @Nullable
    protected Object name = null;

    AbstractELispClosure(Object[] inner, ClosureCommons commons) {
        super(inner);
        this.commons = commons;
    }

    public Object getName() {
        return name == null ? this : name;
    }

    public void setName(Object name) {
        this.name = name;
        FunctionRootNode f = commons.rootNode;
        if (f != null) {
            f.setLispFunction(name);
        }
    }

    public ClosureCommons getCommons() {
        return commons;
    }

    protected Object getArgs() {
        return get(CLOSURE_ARGLIST);
    }

    protected abstract FunctionRootNode getFunctionRootNode();

    public final ELispFunctionObject getFunction() {
        ELispFunctionObject f = commons.function;
        if (f == null) {
            CompilerDirectives.transferToInterpreter();
            FunctionRootNode root = getFunctionRootNode();
            f = new ELispFunctionObject(root.getCallTarget());
            commons.rootNode = root; // NOPMD
            commons.function = f;
        }
        return f;
    }

    @Override
    public void display(ELispPrint print) {
        vectorPrintHelper(print, "#[", "]", inner);
    }

    @Nullable
    public static Source getRootNodeSource(@Nullable RootNode rootNode) {
        if (rootNode == null) {
            return null;
        }
        SourceSection source = rootNode.getSourceSection();
        return source == null ? null : source.getSource();
    }

    @TruffleBoundary
    public static AbstractELispClosure create(List<?> inner, ClosureCommons rootSource) {
        Object[] array = inner.toArray();
        int size = array.length;
        if (size >= CLOSURE_STACK_DEPTH && size <= CLOSURE_INTERACTIVE + 1) {
            Object argList = array[CLOSURE_ARGLIST];
            Object code = array[CLOSURE_CODE];
            Object constants = array[CLOSURE_CONSTANTS];
            if (argList instanceof Long || BuiltInData.FListp.listp(argList)) {
                // bytecode
                if (code instanceof ELispString s && !isMultibyte(s)
                        && constants instanceof ELispVector
                        && size > CLOSURE_STACK_DEPTH && array[CLOSURE_STACK_DEPTH] instanceof Long) {
                    return new ELispBytecode(array, rootSource);
                }
                // interpreted closure
                if (BuiltInData.FListp.listp(argList) && code instanceof ELispCons && BuiltInData.FListp.listp(constants)) {
                    return new ELispInterpretedClosure(array, rootSource);
                }
            }
        }
        throw ELispSignals.invalidReadSyntax("Invalid byte-code object");
    }

    public static final class ClosureCommons {
        @Nullable
        transient FunctionRootNode rootNode = null;
        @Nullable
        transient ELispFunctionObject function = null;
        @Nullable
        Source source;

        public @Nullable Source getSource() {
            return source;
        }

        public ClosureCommons() {
            this.source = null;
        }
        public ClosureCommons(@Nullable Source source) {
            this.source = source;
        }
        public ClosureCommons(@Nullable RootNode parent) {
            this.source = parent == null ? null : getRootNodeSource(parent);
        }
    }
}
