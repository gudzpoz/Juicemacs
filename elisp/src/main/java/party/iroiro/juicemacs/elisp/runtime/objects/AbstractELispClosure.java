package party.iroiro.juicemacs.elisp.runtime.objects;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInData;
import party.iroiro.juicemacs.elisp.nodes.FunctionRootNode;
import party.iroiro.juicemacs.elisp.runtime.ELispFunctionObject;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispPrint;

import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.BuiltInData.isMultibyte;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.CLOSURE_STACK_DEPTH;

public sealed abstract class AbstractELispClosure extends AbstractELispVector
        permits ELispInterpretedClosure, ELispBytecode {
    @Nullable
    protected final Source rootSource;
    @Nullable
    protected volatile FunctionRootNode functionRootNode = null;
    @Nullable
    protected volatile ELispFunctionObject function = null;
    @Nullable
    protected Object name = null;

    AbstractELispClosure(Object[] inner, @Nullable Source rootSource) {
        super(inner);
        this.rootSource = rootSource;
    }

    public Object getName() {
        return name == null ? this : name;
    }

    public void setName(Object name) {
        this.name = name;
        FunctionRootNode f = functionRootNode;
        if (f != null) {
            f.setLispFunction(name);
        }
    }

    @Nullable
    public Source getRootSource() {
        return rootSource;
    }

    protected Object getArgs() {
        return get(CLOSURE_ARGLIST);
    }

    protected abstract FunctionRootNode getFunctionRootNode();
    public final ELispFunctionObject getFunction() {
        ELispFunctionObject f = function;
        if (f == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            FunctionRootNode root = getFunctionRootNode();
            f = new ELispFunctionObject(root.getCallTarget());
            functionRootNode = root; // NOPMD
            function = f;
        }
        return f;
    }

    @Override
    public void display(ELispPrint print) {
        displayHelper(print, "#[", "]");
    }

    @Nullable
    protected static Source getRootNodeSource(@Nullable RootNode rootNode) {
        if (rootNode == null) {
            return null;
        }
        SourceSection source = rootNode.getSourceSection();
        return source == null ? null : source.getSource();
    }

    public static ELispVectorLike<Object> create(List<?> inner, @Nullable RootNode root) {
        return create(inner, getRootNodeSource(root));
    }
    public static ELispVectorLike<Object> create(List<?> inner, @Nullable Source rootSource) {
        Object[] array = inner.toArray();
        int size = array.length;
        if (size >= CLOSURE_STACK_DEPTH && size <= CLOSURE_INTERACTIVE + 1) {
            Object argList = array[CLOSURE_ARGLIST];
            Object code = array[CLOSURE_CODE];
            Object constants = array[CLOSURE_CONSTANTS];
            if (argList instanceof Long || BuiltInData.FListp.listp(argList)) {
                // bytecode
                if (code instanceof ELispString s && !isMultibyte(s.value())
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
}
