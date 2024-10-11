package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.strings.TruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;

import java.io.EOFException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.asCons;
import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.asStr;
import static party.iroiro.juicemacs.elisp.runtime.ELispContext.READ_CHAR;

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
            return ELispParser.read(source);
        } catch (IOException e) {
            throw ELispSignals.error(e.getMessage());
        }
    }

    public static boolean loadFile(ELispLanguage language, Object file) {
        Object loadPath = ELispContext.LOAD_PATH.getValue();
        if (ELispSymbol.isNil(loadPath)) {
            return false;
        }
        String stem = file.toString();
        for (Object path : asCons(loadPath)) {
            Path directory = Path.of(asStr(path).toString());
            Path target = directory.resolve(stem + ".elc");
            if (!target.toFile().isFile()) {
                target = directory.resolve(stem + ".el");
            }
            if (target.toFile().isFile()) {
                try {
                    System.out.println("load: " + target);
                    ELispRootNode expr = ELispParser.parse(
                            language,
                            Source.newBuilder(
                                    "elisp",
                                    new FileReader(target.toFile()),
                                    target.toFile().getName()
                            ).build()
                    );
                    expr.getCallTarget().call();
                    return true;
                } catch (FileNotFoundException e) {
                    throw ELispSignals.fileMissing(e, target);
                } catch (EOFException e) {
                    throw ELispSignals.endOfFile(target);
                } catch (IOException e) {
                    throw ELispSignals.error(e.getMessage());
                }
            }
        }
        return false;
    }

    /**
     * See {@code substitute_object_recurse} in {@code src/lread.c}
     */
    public record SubstituteObjectRecurse(
            Object object,
            Object placeholder,
            @Nullable ELispHashtable recursive,
            @Nullable HashSet<Object> seen
    ) {
        @CompilerDirectives.TruffleBoundary
        public Object substitute(Object tree) {
            if (tree == placeholder) {
                return object;
            }
            return switch (tree) {
                case Long _, Double _, ELispBigNum _, ELispSymbol _ -> tree;
                case ELispString s when s.intervals() == 0 -> tree;
                case ELispBoolVector _ -> tree;
                default -> {
                    if (recursive == null) {
                        if (seen != null && seen.contains(tree)) {
                            yield tree;
                        }
                    } else if (recursive.containsKey(tree)) {
                        yield tree;
                    }
                    yield switch (tree) {
                        case ELispCons cons -> {
                            cons.setCar(substitute(cons.car()));
                            cons.setCdr(substitute(cons.cdr()));
                            yield cons;
                        }
                        case ELispString s -> {
                            s.forProperties(this::substitute);
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
        public static Void getLoadSuffixes() {
            throw new UnsupportedOperationException();
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
        public boolean load(Object file, Object noerror, Object nomessage, Object nosuffix, Object mustSuffix) {
            return loadFile(ELispLanguage.get(this), file);
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
        public static Void locateFileInternal(Object filename, Object path, Object suffixes, Object predicate) {
            throw new UnsupportedOperationException();
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
        public static Void evalBuffer(Object buffer, Object printflag, Object filename, Object unibyte, Object doAllowPrint) {
            throw new UnsupportedOperationException();
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
            if (ELispSymbol.isNil(stream)) {
                // TODO: Vstandard_input
                throw new UnsupportedOperationException();
            }
            if (ELispSymbol.isT(stream)) {
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
        public static Object readFromString(ELispString string, Object start, Object end) {
            long from = ELispSymbol.notNilOr(start, 0L);
            long to = ELispSymbol.notNilOr(end, string.codepointCount());
            TruffleString sub = string.toTruffleString().substringUncached((int) from, (int) (to - from), ELispString.ENCODING, false);
            try {
                Source elisp = Source.newBuilder("elisp", sub.toString(), "read-from-string").build();
                ELispParser parser = new ELispParser(elisp);
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
        public static boolean lreadSubstituteObjectInSubtree(Object object, Object placeholder, Object completed) {
            new SubstituteObjectRecurse(
                    object,
                    placeholder,
                    completed instanceof ELispHashtable t ? t : null,
                    completed instanceof ELispHashtable ? null : new HashSet<>()
            ).substitute(object);
            return false; // return Qnil;
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
        public static Object intern(ELispString string, Object obarray) {
            if (!ELispSymbol.isNil(obarray)) {
                throw new UnsupportedOperationException();
            }
            return ELispContext.intern(string.toString());
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
        public static Void internSoft(Object name, Object obarray) {
            throw new UnsupportedOperationException();
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
        public static Void unintern(Object name, Object obarray) {
            throw new UnsupportedOperationException();
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
        public static Void obarrayMake(Object size) {
            throw new UnsupportedOperationException();
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
        public static Void obarrayp(Object object) {
            throw new UnsupportedOperationException();
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
        @Specialization
        public static Void obarrayClear(Object obarray) {
            throw new UnsupportedOperationException();
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
        @Specialization
        public static Void mapatoms(Object function, Object obarray) {
            throw new UnsupportedOperationException();
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
