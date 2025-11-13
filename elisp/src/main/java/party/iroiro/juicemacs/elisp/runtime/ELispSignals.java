package party.iroiro.juicemacs.elisp.runtime;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
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
import org.jspecify.annotations.Nullable;
import org.graalvm.polyglot.Value;
import party.iroiro.juicemacs.elisp.forms.BuiltInEditFns;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Objects;
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
            return ELispCons.cons(tag, data).toString();
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

        @Override
        public ExceptionType getExceptionType() {
            return this.getTag() == FATAL ? ExceptionType.EXIT : super.getExceptionType();
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

    @TruffleBoundary
    private static ELispSignalException signal(ELispSymbol error, Object... data) {
        for (int i = 0; i < data.length; i++) {
            if (data[i] instanceof String s) {
                data[i] = new ELispString(s);
            }
        }
        return new ELispSignalException(error, ELispCons.listOf(data));
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
    @TruffleBoundary
    public static ELispSignalException argsOutOfRange(long index) {
        return signal(ARGS_OUT_OF_RANGE, index, false);
    }
    @TruffleBoundary
    public static ELispSignalException argsOutOfRange(Object object, long index) {
        return signal(ARGS_OUT_OF_RANGE, object, index);
    }
    @TruffleBoundary
    public static ELispSignalException argsOutOfRange(Object object, long left, long right) {
        return signal(ARGS_OUT_OF_RANGE, object, left, right);
    }
    public static ELispSignalException cyclicFunctionIndirection(ELispSymbol symbol) {
        return signal(CYCLIC_FUNCTION_INDIRECTION, symbol);
    }
    @TruffleBoundary
    public static ELispSignalException wrongNumberOfArguments(Object function, long actual) {
        return signal(WRONG_NUMBER_OF_ARGUMENTS, function, actual);
    }
    @TruffleBoundary
    public static ELispSignalException wrongTypeArgument(ELispSymbol predicate, Object actual) {
        return signal(WRONG_TYPE_ARGUMENT, predicate, actual);
    }
    @TruffleBoundary
    public static ELispSignalException wrongLengthArgument(long expected, long actual) {
        return signal(WRONG_LENGTH_ARGUMENT, expected, actual);
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
        return signal(FILE_ERROR, TruffleUtils.eMessage(e), file);
    }
    public static ELispSignalException fileMissing(FileNotFoundException e, Object file) {
        return signal(FILE_MISSING, TruffleUtils.eMessage(e), file);
    }
    public static ELispSignalException fileAlreadyExists(TruffleFile file) {
        return signal(FILE_ALREADY_EXISTS, file.toString());
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

    //#region Math
    public static ELispSignalException arithError() {
        return signal(ARITH_ERROR);
    }
    public static ELispSignalException overflowError() {
        return signal(OVERFLOW_ERROR);
    }
    //#endregion Math

    //#region emacs_abort
    public static ELispSignalException kill(Object data) {
        return signal(FATAL, KILL_EMACS, data);
    }
    public static ELispSignalException fatal(String message) {
        return signal(FATAL, FATAL, message);
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
                "party.iroiro.juicemacs.elisp.runtime.string.ELispString",
                STRINGP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispBigNum",
                INTEGERP,
                "party.iroiro.juicemacs.elisp.runtime.objects.ELispMarker",
                MARKERP,
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

    @TruffleBoundary
    public static RuntimeException remapException(RuntimeException e, Node location) {
        ELispSignals.ELispSignalException mapped = switch (e) {
            case ClassCastException cast -> {
                Matcher matcher = CLASS_CAST_GUESS.matcher(cast.getMessage());
                if (matcher.find()) {
                    String actual = matcher.group(1);
                    String expected = matcher.group(2);
                    ELispSymbol predicate = CLASS_CAST_MAP.get(expected);
                    if (predicate != null) {
                        yield ELispSignals.wrongTypeArgument(predicate, actual);
                    }
                }
                BuiltInEditFns.FMessage.message(ELispContext.get(null), "unrecognized class cast: " + e.getMessage());
                yield ELispSignals.wrongTypeArgument(UNSPECIFIED, Objects.requireNonNull(e.getMessage()));
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
                            } catch (Throwable ignored) {
                                ELispSymbol predicate = CLASS_CAST_MAP.getOrDefault(expected.getName(), UNSPECIFIED);
                                yield ELispSignals.wrongTypeArgument(predicate, supplied[i]);
                            }
                        }
                    }
                }
                yield ELispSignals.wrongTypeArgument(UNSPECIFIED, Objects.requireNonNull(e.getMessage()));
            }
            case ELispSignals.ELispSignalException signal -> ELispSignals.attachLocation(signal, location);
            case ArithmeticException _ -> ELispSignals.arithError();
            case UnsupportedOperationException _ -> {
                // - If Juicemacs ever gets mature enough, we should make this a lisp error.
                // - But currently, for development purposes, knowing exactly where this exception
                //   is thrown is very useful, so, by default, UnsupportedOperationException is
                //   non-catchable from lisp code, to provide a clear stack trace.
                if (ELispContext.get(location).options().catchUnsupported()) {
                    yield ELispSignals.error(Objects.requireNonNullElse(e.getMessage(), "unsupported"));
                }
                yield null;
            }
            default -> null;
        };
        return mapped == null ? e : ELispSignals.attachLocation(mapped, location);
    }
    //#endregion Remap
}
