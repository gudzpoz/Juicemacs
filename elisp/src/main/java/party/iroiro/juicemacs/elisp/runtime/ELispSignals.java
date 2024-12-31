package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.polyglot.Value;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;

/// Internal presentations of `catch/throw` and `condition-case/signal`
public abstract class ELispSignals {
    private static final ELispSymbol FATAL = new ELispSymbol("fatal");

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
        return signal(ERROR, message);
    }

    //#region Symbol operations
    public static ELispSignalException cyclicVariableIndirection(ELispSymbol symbol) {
        return signal(CYCLIC_VARIABLE_INDIRECTION, symbol);
    }
    public static ELispSignalException settingConstant(ELispSymbol symbol) {
        return signal(SETTING_CONSTANT, symbol);
    }
    public static ELispSignalException voidVariable(ELispSymbol symbol) {
        return signal(VOID_VARIABLE, symbol);
    }
    //#endregion Symbol operations

    //#region Function operations
    public static ELispSignalException argsOutOfRange(long index) {
        return signal(ARGS_OUT_OF_RANGE, index, false);
    }
    public static ELispSignalException argsOutOfRange(Object object, long index) {
        return signal(ARGS_OUT_OF_RANGE, object, index);
    }
    public static ELispSignalException argsOutOfRange(Object object, long left, long right) {
        return signal(ARGS_OUT_OF_RANGE, object, left, right);
    }
    public static ELispSignalException wrongNumberOfArguments(Object function, long actual) {
        return signal(WRONG_NUMBER_OF_ARGUMENTS, function, actual);
    }
    public static ELispSignalException wrongTypeArgument(ELispSymbol predicate, Object actual) {
        return signal(WRONG_TYPE_ARGUMENT, predicate, actual);
    }
    public static ELispSignalException invalidFunction(Object function) {
        return signal(INVALID_FUNCTION, function);
    }
    public static ELispSignalException voidFunction(Object function) {
        return signal(VOID_FUNCTION, function);
    }
    //#endregion Function operations

    //#region File operations
    public static ELispSignalException reportFileError(IOException e, Object file) {
        return signal(FILE_ERROR, e.getMessage(), file);
    }
    public static ELispSignalException fileMissing(FileNotFoundException e, Object file) {
        return signal(FILE_MISSING, e.getClass().getSimpleName(), e.getMessage(), file);
    }
    public static ELispSignalException endOfFile() {
        return signal(END_OF_FILE);
    }
    public static ELispSignalException endOfFile(Object file) {
        return signal(END_OF_FILE, file);
    }
    //#endregion File operations

    //#region Lisp parsing
    public static ELispSignalException invalidReadSyntax(String message) {
        return signal(INVALID_READ_SYNTAX, message);
    }
    //#endregion Lisp parsing

    //#region Object operations
    public static ELispSignalException circularList(Object list) {
        return signal(CIRCULAR_LIST, list);
    }
    public static ELispSignalException invalidRegexp(String message) {
        return signal(INVALID_REGEXP, message);
    }
    public static ELispSignalException searchFailed() {
        return signal(SEARCH_FAILED);
    }
    //#endregion Object operations

    //#region emacs_abort
    public static ELispSignalException fatal() {
        return signal(FATAL);
    }
    public static ELispSignalException fatal(String message) {
        return signal(FATAL, message);
    }
    //#endregion emacs_abort

    //#region Remap
    private final static Pattern CLASS_CAST_GUESS =
            Pattern.compile("(?=class )?(\\S+) cannot be cast to (=?class )?(\\S+)");

    private final static Map<String, ELispSymbol> CLASS_CAST_MAP;

    static {
        CLASS_CAST_MAP = Map.of(
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol",
                SYMBOLP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispCons",
                CONSP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispString",
                STRINGP,
                "java.lang.Long",
                INTEGERP,
                "long",
                INTEGERP,
                "java.lang.Double",
                FLOATP,
                "double",
                FLOATP
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
                    ELispSymbol predicate = CLASS_CAST_MAP.getOrDefault(expected, UNSPECIFIED);
                    yield ELispSignals.wrongTypeArgument(predicate, actual);
                }
                yield ELispSignals.wrongTypeArgument(UNSPECIFIED, e.getMessage());
            }
            case UnsupportedSpecializationException dsl -> {
                Method[] methods = dsl.getNode().getClass().getMethods();
                Object[] supplied = dsl.getSuppliedValues();
                for (Method method : methods) {
                    if (method.isAnnotationPresent(Specialization.class)) {
                        Class<?>[] types = method.getParameterTypes();
                        int typeI = types.length > 0 && Frame.class.isAssignableFrom(types[0])
                                ? 1
                                : 0;
                        for (int i = 0; i < supplied.length && typeI < types.length; i++, typeI++) {
                            Value value = Value.asValue(supplied[i]);
                            Class<?> expected = types[typeI];
                            try {
                                value.as(expected);
                            } catch (Exception ignored) {
                                ELispSymbol predicate = CLASS_CAST_MAP.getOrDefault(expected.getName(), UNSPECIFIED);
                                yield ELispSignals.wrongTypeArgument(predicate, supplied[i]);
                            }
                        }
                    }
                }
                yield ELispSignals.wrongTypeArgument(UNSPECIFIED, e.getMessage());
            }
            case ELispSignals.ELispSignalException signal -> ELispSignals.attachLocation(signal, location);
            default -> null;
        };
        return mapped == null ? e : ELispSignals.attachLocation(mapped, location);
    }
    //#endregion Remap
}
