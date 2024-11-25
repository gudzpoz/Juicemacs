package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.FileNotFoundException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/// Internal presentations of `catch/throw` and `condition-case/signal`
public abstract class ELispSignals {
    @ExportLibrary(InteropLibrary.class)
    abstract sealed static class ELispNonLocalExitException extends AbstractTruffleException {
        private final Object tag;
        private final Object data;

        private ELispNonLocalExitException(Object tag, Object data, @Nullable Node location) {
            super(location);
            this.tag = tag;
            this.data = data;
        }

        public Object getTag() {
            return tag;
        }

        public Object getData() {
            return data;
        }

        @Override
        public String getMessage() {
            return new ELispCons(tag, data).toString();
        }

        @ExportMessage
        ExceptionType getExceptionType() {
            return ExceptionType.RUNTIME_ERROR;
        }

        @ExportMessage
        boolean hasSourceLocation() {
            return getLocation() != null;
        }

        @ExportMessage(name = "getSourceLocation")
        SourceSection getSourceSection() throws UnsupportedMessageException {
            if (!hasSourceLocation()) {
                throw UnsupportedMessageException.create();
            }
            return getLocation().getSourceSection();
        }
    }

    public static final class ELispSignalException extends ELispNonLocalExitException {
        public ELispSignalException(Object tag, Object data, @Nullable Node location) {
            super(tag, data, location);
        }
        public ELispSignalException(Object tag, Object data) {
            this(tag, data, null);
        }
    }

    public static final class ELispCatchException extends ELispNonLocalExitException {
        public ELispCatchException(Object tag, Object data, @Nullable Node location) {
            super(tag, data, location);
        }
        public ELispCatchException(Object tag, Object data) {
            this(tag, data, null);
        }
    }

    private static ELispSignalException attachLocation(ELispSignalException exception, Node location) {
        if (exception.hasSourceLocation()) {
            return exception;
        }
        return new ELispSignalException(exception.getTag(), exception.getData(), location);
    }

    @CompilerDirectives.TruffleBoundary
    private static ELispSignalException signal(ELispSymbol error, Object... data) {
        ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
        for (Object datum : data) {
            builder.add(datum instanceof String s ? new ELispString(s) : datum);
        }
        return new ELispSignalException(error, builder.build());
    }

    public static ELispSignalException error(String message) {
        return signal(ELispContext.ERROR, message);
    }

    //#region Symbol operations
    public static ELispSignalException cyclicVariableIndirection(ELispSymbol symbol) {
        return signal(ELispContext.CYCLIC_VARIABLE_INDIRECTION, symbol);
    }
    public static ELispSignalException settingConstant(ELispSymbol symbol) {
        return signal(ELispContext.SETTING_CONSTANT, symbol);
    }
    public static ELispSignalException voidVariable(ELispSymbol symbol) {
        return signal(ELispContext.VOID_VARIABLE, symbol);
    }
    //#endregion Symbol operations

    //#region Function operations
    public static ELispSignalException argsOutOfRange(long index) {
        return signal(ELispContext.ARGS_OUT_OF_RANGE, index, false);
    }
    public static ELispSignalException argsOutOfRange(Object object, long index) {
        return signal(ELispContext.ARGS_OUT_OF_RANGE, object, index);
    }
    public static ELispSignalException argsOutOfRange(Object object, long left, long right) {
        return signal(ELispContext.ARGS_OUT_OF_RANGE, object, left, right);
    }
    public static ELispSignalException wrongNumberOfArguments(Object function, long actual) {
        return signal(ELispContext.WRONG_NUMBER_OF_ARGUMENTS, function, actual);
    }
    public static ELispSignalException wrongTypeArgument(ELispSymbol predicate, Object actual) {
        return signal(ELispContext.WRONG_TYPE_ARGUMENT, predicate, actual);
    }
    public static ELispSignalException invalidFunction(Object function) {
        return signal(ELispContext.INVALID_FUNCTION, function);
    }
    public static ELispSignalException voidFunction(Object function) {
        return signal(ELispContext.VOID_FUNCTION, function);
    }
    //#endregion Function operations

    //#region File operations
    public static ELispSignalException fileMissing(FileNotFoundException e, Object file) {
        return signal(ELispContext.FILE_MISSING, e.getClass().getSimpleName(), e.getMessage(), file);
    }
    public static ELispSignalException endOfFile() {
        return signal(ELispContext.END_OF_FILE);
    }
    public static ELispSignalException endOfFile(Object file) {
        return signal(ELispContext.END_OF_FILE, file);
    }
    //#endregion File operations

    //#region Lisp parsing
    public static ELispSignalException invalidReadSyntax(String message) {
        return signal(ELispContext.INVALID_READ_SYNTAX, message);
    }
    //#endregion Lisp parsing

    //#region Object operations
    public static ELispSignalException circularList(Object list) {
        return signal(ELispContext.CIRCULAR_LIST, list);
    }
    public static ELispSignalException invalidRegexp(String message) {
        return signal(ELispContext.INVALID_REGEXP, message);
    }
    public static ELispSignalException searchFailed() {
        return signal(ELispContext.SEARCH_FAILED);
    }
    //#endregion Object operations

    //#region emacs_abort
    public static ELispSignalException fatal() {
        return signal(ELispContext.intern("fatal"));
    }
    //#endregion emacs_abort

    //#region Remap
    private final static Pattern CLASS_CAST_GUESS =
            Pattern.compile("(?=class )?(\\S+) cannot be cast to (=?class )?(\\S+)");

    private final static Map<String, ELispSymbol> CLASS_CAST_MAP;

    static {
        CLASS_CAST_MAP = Map.of(
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol",
                ELispContext.SYMBOLP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispCons",
                ELispContext.CONSP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispString",
                ELispContext.STRINGP,
                "java.lang.Long",
                ELispContext.INTEGERP,
                "java.lang.Double",
                ELispContext.FLOATP
        );
    }

    @CompilerDirectives.TruffleBoundary
    public static RuntimeException remapException(RuntimeException e, Node location) {
        ELispSignals.ELispSignalException mapped = switch (e) {
            case ClassCastException cast -> {
                Matcher matcher = CLASS_CAST_GUESS.matcher(cast.getMessage());
                if (matcher.find()) {
                    String actual = matcher.group(1);
                    String expected = matcher.group(2);
                    ELispSymbol predicate = CLASS_CAST_MAP.get(expected);
                    if (predicate == null) {
                        predicate = ELispContext.intern(expected);
                    }
                    yield ELispSignals.wrongTypeArgument(predicate, actual);
                }
                yield ELispSignals.wrongTypeArgument(ELispContext.UNSPECIFIED, e.getMessage());
            }
            case UnsupportedSpecializationException _ ->
                    ELispSignals.wrongTypeArgument(ELispContext.UNSPECIFIED, e.getMessage());
            case ELispSignals.ELispSignalException signal -> ELispSignals.attachLocation(signal, location);
            default -> null;
        };
        return mapped == null ? e : ELispSignals.attachLocation(mapped, location);
    }
    //#endregion Remap
}
