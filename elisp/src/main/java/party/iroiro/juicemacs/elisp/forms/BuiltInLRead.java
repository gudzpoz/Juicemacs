package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.*;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.io.EOFException;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/**
 * Built-in functions from {@code src/lread.c}
 */
public class BuiltInLRead extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInLReadFactory.getFactories();
    }

    public static Object readInternalStart(
            Object stream, Object start, Object end, boolean locateSymbols
    ) {
        // TODO: Handle stream instanceof ELispBuffer, ELispCons
        ELispString s = asStr(stream);
        Source source = Source.newBuilder(ELispLanguage.ID, s.toString(), null).build();
        try {
            return ELispParser.read(ELispContext.get(null), source);
        } catch (IOException e) {
            String message = e.getMessage();
            throw ELispSignals.error(message == null ? stream.toString() : message);
        }
    }

    private static boolean completeFilenameP(ELispString filename) {
        // TODO: This is different from Emacs.
        return Path.of(filename.toString()).isAbsolute();
    }

    private static final long ACCESS_R_OK = 4;
    private static final long ACCESS_W_OK = 2;
    private static final long ACCESS_X_OK = 1;

    private static Predicate<ELispString> getLocateOpenPredicate(TruffleLanguage.Env env, Object predicate) {
        if (predicate instanceof Long l) {
            return (f) -> {
                TruffleFile file = env.getPublicTruffleFile(f.toString());
                if (!file.exists() || file.isDirectory()) {
                    return false;
                }
                if ((l & ACCESS_X_OK) != 0 && !file.isExecutable()) {
                    return false;
                }
                if ((l & ACCESS_W_OK) != 0 && !file.isWritable()) {
                    return false;
                }
                return (l & ACCESS_R_OK) == 0 || file.isReadable();
            };
        }
        if (isNil(predicate) || isT(predicate)) {
            return f -> !BuiltInFileIO.FFileDirectoryP.fileDirectoryP(f)
                    && BuiltInFileIO.FFileReadableP.fileReadableP(f);
        }
        return f -> {
            Object ret = BuiltInEval.FFuncall.funcall(null, predicate, f);
            return !isNil(ret) && (ret == DIR_OK || !BuiltInFileIO.FFileDirectoryP.fileDirectoryP(f));
        };
    }

    @Nullable
    public static ELispString locateOpenP(Object paths, ELispString name, Object suffixes,
                                          Object predicate, boolean newer, boolean noNative) {
        TruffleLanguage.Env env = ELispContext.get(null).truffleEnv();
        Predicate<ELispString> filePredicate = getLocateOpenPredicate(env, predicate);

        Instant saveMtime = Instant.MIN;
        boolean absolute = completeFilenameP(name);
        ELispString original = name;
        @Nullable ELispString result = null;
        ELispCons pathList = paths instanceof ELispCons cons ? cons : new ELispCons(new ELispString("."));
        ELispCons suffixList = new ELispCons(new ELispString(""), suffixes);
        ELispContext context = ELispContext.get(null);
        for (Object path : pathList) {
            ELispString directory = asStr(path);
            name = BuiltInFileIO.FExpandFileName.expandFileName(original, directory);
            if (!completeFilenameP(name)) {
                name = BuiltInFileIO.FExpandFileName.expandFileName(name, context.currentBuffer().getDirectory());
                if (!completeFilenameP(name)) {
                    continue;
                }
            }
            // Emacs: Copy FILENAME's data to FN but remove starting /: if any.
            // TODO: Why?
            if (name.value().startsWith("/:")) {
                name = BuiltInFns.FSubstring.substring(name, 2L, false);
            }
            for (Object suffix : suffixList) {
                ELispString suffixString = asStr(suffix);
                ELispString test = new ELispString(
                        new MuleStringBuffer()
                                .append(name.value())
                                .append(suffixString.value())
                                .build());
                Object handler = BuiltInFileIO.FFindFileNameHandler.findFileNameHandler(test, FILE_EXISTS_P);
                boolean exists;
                if (isNil(handler) && (isNil(predicate) || isT(predicate))) {
                    TruffleFile file = env.getPublicTruffleFile(test.toString());
                    exists = !file.isDirectory() && file.isReadable();
                } else {
                    exists = filePredicate.test(test);
                }
                if (exists) {
                    if (!newer) {
                        return test;
                    }
                    Instant lastModified;
                    try {
                        FileTime lastModifiedTime = env.getPublicTruffleFile(test.toString()).getLastModifiedTime();
                        lastModified = lastModifiedTime.toInstant();
                    } catch (IOException e) {
                        throw ELispSignals.fileMissing(new FileNotFoundException(test.toString()), test);
                    }
                    if (lastModified.isAfter(saveMtime)) {
                        saveMtime = lastModified;
                        result = test;
                    }
                }
            }
            if (absolute) {
                break;
            }
        }
        return result;
    }

    @CompilerDirectives.TruffleBoundary
    public static boolean loadFile(ELispLanguage language, @Nullable Node caller, Object file, boolean errorIfNotFound) {
        Object loader = LOAD_SOURCE_FILE_FUNCTION.getValue();
        if (isNil(loader)) {
            TruffleFile path = findLoadFilePureJava(file, errorIfNotFound);
            if (path == null) {
                return false;
            }
            ELispRootNode root = loadFilePureJava(language, path);
            try (Dynamic _ = Dynamic.pushDynamic(LOAD_FILE_NAME, new ELispString(path.getName()))) {
                root.getCallTarget().call(caller);
            }
            return true;
        } else {
            ELispString path = locateOpenP(
                    LOAD_PATH.getValue(),
                    asStr(file),
                    LOAD_SUFFIXES.getValue(),
                    false, true, true
            );
            if (path == null) {
                if (errorIfNotFound) {
                    throw ELispSignals.fileMissing(new FileNotFoundException(file.toString()), file);
                }
                return false;
            }
            return !isNil(BuiltInEval.FFuncall.funcall(caller, loader,
                    path,
                    file,
                    !errorIfNotFound,
                    false
            ));
        }
    }

    @Nullable
    private static TruffleFile findLoadFilePureJava(Object file, boolean errorIfNotFound) {
        CompilerDirectives.transferToInterpreter();
        ELispContext context = ELispContext.get(null);
        TruffleLanguage.Env env = context.truffleEnv();

        Object loadPath = LOAD_PATH.getValue();
        if (isNil(loadPath)) {
            if (errorIfNotFound) {
                throw ELispSignals.fileMissing(new FileNotFoundException(file.toString()), file);
            }
            return null;
        }
        String stem = file.toString();
        for (Object path : asCons(loadPath)) {
            TruffleFile directory = env.getPublicTruffleFile(asStr(path).toString()).getAbsoluteFile();
            TruffleFile target;
            if (stem.endsWith(".el") || stem.endsWith(".elc")) {
                target = directory.resolve(stem);
            } else {
                target = directory.resolve(stem + ".elc");
                if (!target.isRegularFile()) {
                    target = directory.resolve(stem + ".el");
                }
            }
            if (target.isRegularFile()) {
                return target;
            }
        }
        if (errorIfNotFound) {
            throw ELispSignals.fileMissing(new FileNotFoundException(file.toString()), file);
        }
        return null;
    }

    private static ELispRootNode loadFilePureJava(ELispLanguage language, TruffleFile file) {
        ELispContext context = ELispContext.get(null);
        TruffleLanguage.Env env = context.truffleEnv();
        try {
            context.out().println("load: " + env.getCurrentWorkingDirectory().getAbsoluteFile().relativize(file));
            return ELispParser.parse(
                    language,
                    context,
                    Source.newBuilder("elisp", file).build()
            );
        } catch (FileNotFoundException e) {
            throw ELispSignals.fileMissing(e, file);
        } catch (EOFException e) {
            throw ELispSignals.endOfFile(file);
        } catch (IOException e) {
            throw ELispSignals.error(e.getMessage());
        }
    }

    /**
     * See {@code substitute_object_recurse} in {@code src/lread.c}
     */
    public record SubstituteObjectRecurse(
            Object object,
            Object placeholder,
            ELispHashtable recursive
    ) {
        @CompilerDirectives.TruffleBoundary
        public Object substitute(Object tree) {
            if (tree == placeholder) {
                return object;
            }
            return switch (tree) {
                case Long _, Double _, ELispBigNum _, ELispSymbol _ -> tree;
                case ELispString s when !s.hasIntervals() -> tree;
                case ELispBoolVector _ -> tree;
                default -> {
                    if (recursive.containsKey(tree)) {
                        yield tree;
                    }
                    yield switch (tree) {
                        case ELispCons cons -> {
                            ELispCons.ConsIterator iterator = cons.consIterator(0);
                            ELispCons last = cons;
                            while (iterator.hasNextCons()) {
                                last = iterator.nextCons();
                                last.setCar(substitute(last.car()));
                                if (last.cdr() == placeholder) {
                                    last.setCdr(object);
                                    yield tree;
                                }
                            }
                            last.setCdr(substitute(last.cdr()));
                            yield cons;
                        }
                        case ELispString s -> {
                            s.forProperties((o, _, _) -> substitute(o));
                            yield s;
                        }
                        case ELispVectorLike<?> vec -> {
                            // TODO: CHAR_TABLE_P, SUB_CHAR_TABLE_P, CLOSUREP, HASH_TABLE_P, RECORD P
                            for (int i = 0; i < vec.size(); i++) {
                                vec.setUntyped(i, substitute(vec.get(i)));
                            }
                            yield this;
                        }
                        default -> tree;
                    };
                }
            };
        }
    }

    /**
     * <pre>
     * Read a character event from the command input (keyboard or macro).
     * It is returned as a number.
     * If the event has modifiers, they are resolved and reflected in the
     * returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
     * If some of the modifiers cannot be reflected in the character code, the
     * returned value will include those modifiers, and will not be a valid
     * character code: it will fail the `characterp' test.  Use `event-basic-type'
     * to recover the character code with the modifiers removed.
     *
     * If the user generates an event which is not a character (i.e. a mouse
     * click or function key event), `read-char' signals an error.  As an
     * exception, switch-frame events are put off until non-character events
     * can be read.
     * If you want to read non-character events, or ignore them, call
     * `read-event' or `read-char-exclusive' instead.
     *
     * If the optional argument PROMPT is non-nil, display that as a prompt.
     * If PROMPT is nil or the string \"\", the key sequence/events that led
     * to the current command is used as the prompt.
     *
     * If the optional argument INHERIT-INPUT-METHOD is non-nil and some
     * input method is turned on in the current buffer, that input method
     * is used for reading a character.
     *
     * If the optional argument SECONDS is non-nil, it should be a number
     * specifying the maximum number of seconds to wait for input.  If no
     * input arrives in that time, return nil.  SECONDS may be a
     * floating-point value.
     *
     * If `inhibit-interaction' is non-nil, this function will signal an
     * `inhibited-interaction' error.
     * </pre>
     */
    @ELispBuiltIn(name = "read-char", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readChar(Object prompt, Object inheritInputMethod, Object seconds) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read an event object from the input stream.
     *
     * If you want to read non-character events, consider calling `read-key'
     * instead.  `read-key' will decode events via `input-decode-map' that
     * `read-event' will not.  On a terminal this includes function keys such
     * as &lt;F7&gt; and &lt;RIGHT&gt;, or mouse events generated by `xterm-mouse-mode'.
     *
     * If the optional argument PROMPT is non-nil, display that as a prompt.
     * If PROMPT is nil or the string \"\", the key sequence/events that led
     * to the current command is used as the prompt.
     *
     * If the optional argument INHERIT-INPUT-METHOD is non-nil and some
     * input method is turned on in the current buffer, that input method
     * is used for reading a character.
     *
     * If the optional argument SECONDS is non-nil, it should be a number
     * specifying the maximum number of seconds to wait for input.  If no
     * input arrives in that time, return nil.  SECONDS may be a
     * floating-point value.
     *
     * If `inhibit-interaction' is non-nil, this function will signal an
     * `inhibited-interaction' error.
     * </pre>
     */
    @ELispBuiltIn(name = "read-event", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadEvent extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readEvent(Object prompt, Object inheritInputMethod, Object seconds) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read a character event from the command input (keyboard or macro).
     * It is returned as a number.  Non-character events are ignored.
     * If the event has modifiers, they are resolved and reflected in the
     * returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
     * If some of the modifiers cannot be reflected in the character code, the
     * returned value will include those modifiers, and will not be a valid
     * character code: it will fail the `characterp' test.  Use `event-basic-type'
     * to recover the character code with the modifiers removed.
     *
     * If the optional argument PROMPT is non-nil, display that as a prompt.
     * If PROMPT is nil or the string \"\", the key sequence/events that led
     * to the current command is used as the prompt.
     *
     * If the optional argument INHERIT-INPUT-METHOD is non-nil and some
     * input method is turned on in the current buffer, that input method
     * is used for reading a character.
     *
     * If the optional argument SECONDS is non-nil, it should be a number
     * specifying the maximum number of seconds to wait for input.  If no
     * input arrives in that time, return nil.  SECONDS may be a
     * floating-point value.
     *
     * If `inhibit-interaction' is non-nil, this function will signal an
     * `inhibited-interaction' error.
     * </pre>
     */
    @ELispBuiltIn(name = "read-char-exclusive", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadCharExclusive extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readCharExclusive(Object prompt, Object inheritInputMethod, Object seconds) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the suffixes that `load' should try if a suffix is \
     * required.
     * This uses the variables `load-suffixes' and `load-file-rep-suffixes'.
     * </pre>
     */
    @ELispBuiltIn(name = "get-load-suffixes", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FGetLoadSuffixes extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispCons getLoadSuffixes() {
            // TODO: Let load use these.
            return ELispCons.listOf(
                    new ELispString(".elc"),
                    new ELispString(".el")
            );
        }
    }

    /**
     * <pre>
     * Execute a file of Lisp code named FILE.
     * First try FILE with `.elc' appended, then try with `.el', then try
     * with a system-dependent suffix of dynamic modules (see `load-suffixes'),
     * then try FILE unmodified (the exact suffixes in the exact order are
     * determined by `load-suffixes').  Environment variable references in
     * FILE are replaced with their values by calling `substitute-in-file-name'.
     * This function searches the directories in `load-path'.
     *
     * If optional second arg NOERROR is non-nil,
     * report no error if FILE doesn't exist.
     * Print messages at start and end of loading unless
     * optional third arg NOMESSAGE is non-nil (but `force-load-messages'
     * overrides that).
     * If optional fourth arg NOSUFFIX is non-nil, don't try adding
     * suffixes to the specified name FILE.
     * If optional fifth arg MUST-SUFFIX is non-nil, insist on
     * the suffix `.elc' or `.el' or the module suffix; don't accept just
     * FILE unless it ends in one of those suffixes or includes a directory name.
     *
     * If NOSUFFIX is nil, then if a file could not be found, try looking for
     * a different representation of the file by adding non-empty suffixes to
     * its name, before trying another file.  Emacs uses this feature to find
     * compressed versions of files when Auto Compression mode is enabled.
     * If NOSUFFIX is non-nil, disable this feature.
     *
     * The suffixes that this function tries out, when NOSUFFIX is nil, are
     * given by the return value of `get-load-suffixes' and the values listed
     * in `load-file-rep-suffixes'.  If MUST-SUFFIX is non-nil, only the
     * return value of `get-load-suffixes' is used, i.e. the file name is
     * required to have a non-empty suffix.
     *
     * When searching suffixes, this function normally stops at the first
     * one that exists.  If the option `load-prefer-newer' is non-nil,
     * however, it tries all suffixes, and uses whichever file is the newest.
     *
     * Loading a file records its definitions, and its `provide' and
     * `require' calls, in an element of `load-history' whose
     * car is the file name loaded.  See `load-history'.
     *
     * While the file is in the process of being loaded, the variable
     * `load-in-progress' is non-nil and the variable `load-file-name'
     * is bound to the file's name.
     *
     * Return t if the file exists and loads successfully.
     * </pre>
     */
    @ELispBuiltIn(name = "load", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FLoad extends ELispBuiltInBaseNode {
        @Specialization
        public boolean load(ELispString file, Object noerror, Object nomessage, Object nosuffix, Object mustSuffix) {
            // TODO: Potential lock candidate
            return loadFile(getLanguage(), this, file, isNil(noerror));
        }
    }

    /**
     * <pre>
     * Search for FILENAME through PATH.
     * Returns the file's name in absolute form, or nil if not found.
     * If SUFFIXES is non-nil, it should be a list of suffixes to append to
     * file name when searching.
     * If non-nil, PREDICATE is used instead of `file-readable-p'.
     * PREDICATE can also be an integer to pass to the faccessat(2) function,
     * in which case file-name-handlers are ignored.
     * This function will normally skip directories, so if you want it to find
     * directories, make sure the PREDICATE function returns `dir-ok' for them.
     * </pre>
     */
    @ELispBuiltIn(name = "locate-file-internal", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FLocateFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object locateFileInternal(ELispString filename, Object path, Object suffixes, Object predicate) {
            if (!(path instanceof ELispCons list)) {
                return false;
            }
            ELispString absolutePath = locateOpenP(list, filename, suffixes, predicate, false, false);
            return absolutePath == null ? false : absolutePath;
        }
    }

    /**
     * <pre>
     * Execute the accessible portion of current buffer as Lisp code.
     * You can use \\[narrow-to-region] to limit the part of buffer to be evaluated.
     * When called from a Lisp program (i.e., not interactively), this
     * function accepts up to five optional arguments:
     * BUFFER is the buffer to evaluate (nil means use current buffer),
     *  or a name of a buffer (a string).
     * PRINTFLAG controls printing of output by any output functions in the
     *  evaluated code, such as `print', `princ', and `prin1':
     *   a value of nil means discard it; anything else is the stream to print to.
     *   See Info node `(elisp)Output Streams' for details on streams.
     * FILENAME specifies the file name to use for `load-history'.
     * UNIBYTE, if non-nil, specifies `load-convert-to-unibyte' for this
     *  invocation.
     * DO-ALLOW-PRINT, if non-nil, specifies that output functions in the
     *  evaluated code should work normally even if PRINTFLAG is nil, in
     *  which case the output is displayed in the echo area.
     *
     * This function ignores the current value of the `lexical-binding'
     * variable.  Instead it will heed any
     *   -*- lexical-binding: t -*-
     * settings in the buffer, and if there is no such setting, the buffer
     * will be evaluated without lexical binding.
     *
     * This function preserves the position of point.
     * </pre>
     */
    @ELispBuiltIn(name = "eval-buffer", minArgs = 0, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FEvalBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public boolean evalBuffer(Object buffer, Object printflag, Object filename, Object unibyte, Object doAllowPrint) {
            ELispContext context = getContext();
            ELispBuffer current = isNil(buffer) ? context.currentBuffer() : asBuffer(buffer);
            Object path = or(current.getFileTruename(), current.getFilename(), filename);
            ELispString name = asStr(or(filename, path, current.getName()));
            @Nullable Source source = null;
            if (!isNil(path)) {
                TruffleFile file = context.truffleEnv().getPublicTruffleFile(path.toString());
                if (file.exists()) {
                    Source.SourceBuilder builder = Source.newBuilder("elisp", file);
                    if (context.options().debug()) {
                        try {
                            source = builder.build();
                        } catch (IOException ignored) {
                        }
                    }
                    if (source == null) {
                        source = builder.content(Source.CONTENT_NONE).build();
                    }
                }
            }
            if (source == null) {
                source = Source.newBuilder("elisp", "", name.toString()).build();
            }
            try {
                ELispRootNode root = ELispParser.parse(getLanguage(), getContext(), source, current);
                root.getCallTarget().call(this);
            } catch (IOException e) {
                throw ELispSignals.error(e.getMessage());
            }
            return true;
        }
    }

    /**
     * <pre>
     * Execute the region as Lisp code.
     * When called from programs, expects two arguments,
     * giving starting and ending indices in the current buffer
     * of the text to be executed.
     * Programs can pass third argument PRINTFLAG which controls output:
     *  a value of nil means discard it; anything else is stream for printing it.
     *  See Info node `(elisp)Output Streams' for details on streams.
     * Also the fourth argument READ-FUNCTION, if non-nil, is used
     * instead of `read' to read each expression.  It gets one argument
     * which is the input stream for reading characters.
     *
     * This function does not move point.
     * </pre>
     */
    @ELispBuiltIn(name = "eval-region", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FEvalRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void evalRegion(Object start, Object end, Object printflag, Object readFunction) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read one Lisp expression as text from STREAM, return as Lisp object.
     * If STREAM is nil, use the value of `standard-input' (which see).
     * STREAM or the value of `standard-input' may be:
     *  a buffer (read from point and advance it)
     *  a marker (read from where it points and advance it)
     *  a function (call it with no arguments for each character,
     *      call it with a char as argument to push a char back)
     *  a string (takes text from string, starting at the beginning)
     *  t (read text line using minibuffer and use it, or read from
     *     standard input in batch mode).
     * </pre>
     */
    @ELispBuiltIn(name = "read", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRead extends ELispBuiltInBaseNode {
        @Specialization
        public static Object read(Object stream) {
            if (isNil(stream)) {
                // TODO: Vstandard_input
                throw new UnsupportedOperationException();
            }
            if (isT(stream)) {
                stream = READ_CHAR;
            }
            if (stream == READ_CHAR) {
                throw new UnsupportedOperationException();
            }
            return readInternalStart(stream, false, false, false);
        }
    }

    /**
     * <pre>
     * Read one Lisp expression as text from STREAM, return as Lisp object.
     * Convert each occurrence of a symbol into a "symbol with pos" object.
     *
     * If STREAM is nil, use the value of `standard-input' (which see).
     * STREAM or the value of `standard-input' may be:
     *  a buffer (read from point and advance it)
     *  a marker (read from where it points and advance it)
     *  a function (call it with no arguments for each character,
     *      call it with a char as argument to push a char back)
     *  a string (takes text from string, starting at the beginning)
     *  t (read text line using minibuffer and use it, or read from
     *     standard input in batch mode).
     * </pre>
     */
    @ELispBuiltIn(name = "read-positioning-symbols", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReadPositioningSymbols extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readPositioningSymbols(Object stream) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read one Lisp expression which is represented as text by STRING.
     * Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
     * FINAL-STRING-INDEX is an integer giving the position of the next
     * remaining character in STRING.  START and END optionally delimit
     * a substring of STRING from which to read;  they default to 0 and
     * \(length STRING) respectively.  Negative values are counted from
     * the end of STRING.
     * </pre>
     */
    @ELispBuiltIn(name = "read-from-string", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FReadFromString extends ELispBuiltInBaseNode {
        @Specialization
        public Object readFromString(ELispString string, Object start, Object end) {
            long from = notNilOr(start, 0L);
            long to = notNilOr(end, string.length());
            MuleString sub = string.value().subSequence((int) from, (int) to);
            try {
                Source elisp = Source.newBuilder("elisp", sub.toString(), "read-from-string").build();
                ELispParser parser = new ELispParser(getContext(), elisp);
                Object o = parser.nextLisp();
                return new ELispCons(o, from + parser.getCodepointOffset());
            } catch (IOException e) {
                throw ELispSignals.endOfFile();
            }
        }
    }

    /**
     * <pre>
     * In OBJECT, replace every occurrence of PLACEHOLDER with OBJECT.
     * COMPLETED is a hash table of objects that might be circular, or is t
     * if any object might be circular.
     * </pre>
     */
    @ELispBuiltIn(name = "lread--substitute-object-in-subtree", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FLreadSubstituteObjectInSubtree extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean lreadSubstituteObjectInSubtree(Object object, Object placeholder, ELispHashtable completed) {
            new SubstituteObjectRecurse(
                    object,
                    placeholder,
                    completed
            ).substitute(object);
            return false;
        }
    }

    /**
     * <pre>
     * Return the canonical symbol whose name is STRING.
     * If there is none, one is created by this function and returned.
     * A second optional argument specifies the obarray to use;
     * it defaults to the value of `obarray'.
     * </pre>
     */
    @ELispBuiltIn(name = "intern", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FIntern extends ELispBuiltInBaseNode {
        @Specialization
        public Object intern(ELispString string, Object obarray) {
            ELispObarray array = isNil(obarray) ? getContext().obarray() : asObarray(obarray);
            return array.intern(string.value());
        }
    }

    /**
     * <pre>
     * Return the canonical symbol named NAME, or nil if none exists.
     * NAME may be a string or a symbol.  If it is a symbol, that exact
     * symbol is searched for.
     * A second optional argument specifies the obarray to use;
     * it defaults to the value of `obarray'.
     * </pre>
     */
    @ELispBuiltIn(name = "intern-soft", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternSoft extends ELispBuiltInBaseNode {
        @Specialization
        public Object internSoft(ELispSymbol name, Object obarray) {
            ELispObarray array = isNil(obarray) ? getContext().obarray() : asObarray(obarray);
            ELispSymbol found = array.internSoft(name.name());
            return name == found ? name : false;
        }
        @Specialization
        public Object internSoft(ELispString name, Object obarray) {
            ELispObarray array = isNil(obarray) ? getContext().obarray() : asObarray(obarray);
            ELispSymbol symbol = array.internSoft(name.value());
            return Objects.requireNonNullElse(symbol, false);
        }
    }

    /**
     * <pre>
     * Delete the symbol named NAME, if any, from OBARRAY.
     * The value is t if a symbol was found and deleted, nil otherwise.
     * NAME may be a string or a symbol.  If it is a symbol, that symbol
     * is deleted, if it belongs to OBARRAY--no other symbol is deleted.
     * OBARRAY, if nil, defaults to the value of the variable `obarray'.
     * usage: (unintern NAME OBARRAY)
     * </pre>
     */
    @ELispBuiltIn(name = "unintern", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FUnintern extends ELispBuiltInBaseNode {
        @Specialization
        public boolean unintern(Object name, Object obarray) {
            ELispObarray array = isNil(obarray) ? getContext().obarray() : asObarray(obarray);
            MuleString s;
            if (name instanceof ELispString string) {
                s = string.value();
            } else {
                s = asSym(name).name();
            }
            return array.unintern(s) != null;
        }
    }

    /**
     * <pre>
     * Return a new obarray of size SIZE.
     * The obarray will grow to accommodate any number of symbols; the size, if
     * given, is only a hint for the expected number.
     * </pre>
     */
    @ELispBuiltIn(name = "obarray-make", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObarrayMake extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispObarray obarrayMake(Object size) {
            return new ELispObarray(new HashMap<>((int) notNilOr(size, 0)));
        }
    }

    /**
     * <pre>
     * Return t iff OBJECT is an obarray.
     * </pre>
     */
    @ELispBuiltIn(name = "obarrayp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObarrayp extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean obarrayp(Object object) {
            return object instanceof ELispVector vector && !vector.isEmpty();
        }
    }

    /**
     * <pre>
     * Remove all symbols from OBARRAY.
     * </pre>
     */
    @ELispBuiltIn(name = "obarray-clear", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FObarrayClear extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static boolean obarrayClear(ELispObarray obarray) {
            obarray.symbols().clear();
            return false;
        }
    }

    /**
     * <pre>
     * Call FUNCTION on every symbol in OBARRAY.
     * OBARRAY defaults to the value of `obarray'.
     * </pre>
     */
    @ELispBuiltIn(name = "mapatoms", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FMapatoms extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public boolean mapatoms(Object function, Object obarray) {
            ELispObarray array = isNil(obarray) ? getContext().obarray() : asObarray(obarray);
            for (ELispSymbol symbol : array.symbols().values()) {
                BuiltInEval.FFuncall.funcall(null, function, symbol);
            }
            return false;
        }
    }

    /**
     * <pre>
     * Symbols in each bucket of OBARRAY.  Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "internal--obarray-buckets", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FInternalObarrayBuckets extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalObarrayBuckets(Object obarray) {
            throw new UnsupportedOperationException();
        }
    }
}
