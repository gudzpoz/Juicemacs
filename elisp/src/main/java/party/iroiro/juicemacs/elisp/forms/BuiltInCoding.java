package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel;
import org.jspecify.annotations.Nullable;
import org.graalvm.collections.Pair;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns.FCopySequence;
import party.iroiro.juicemacs.elisp.forms.coding.*;
import party.iroiro.juicemacs.elisp.nodes.ELispRootNode;
import party.iroiro.juicemacs.elisp.runtime.*;
import party.iroiro.juicemacs.elisp.runtime.array.ConsIterator;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;

import java.io.IOException;
import java.util.*;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInCoding extends ELispBuiltIns {
    public final ELispCodings codings = new ELispCodings();

    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCodingFactory.getFactories();
    }

    private static ELispCodings getThis(@Nullable Node node) {
        return ELispContext.get(node).globals().builtInCoding.codings;
    }

    public static Object checkCodingSystem(Object codingSystem) {
        if (isNil(codingSystem)) {
            return false;
        }
        Object defineForm = BuiltInFns.FGet.get(codingSystem, CODING_SYSTEM_DEFINE_FORM);
        if (!isNil(defineForm)) {
            BuiltInFns.FPut.put(asSym(codingSystem), CODING_SYSTEM_DEFINE_FORM, false);
            ELispRootNode root = BuiltInEval.FEval.getEvalRoot(null, defineForm, true);
            try {
                root.getCallTarget().call();
            } catch (ELispSignals.ELispSignalException e) {
                // TODO: safe_eval
                throw new UnsupportedOperationException("unable to safe_eval coding system define form", e);
            }
        }
        if (FCodingSystemP.codingSystemP(codingSystem)) {
            return codingSystem;
        }
        throw BuiltInEval.FSignal.signal(CODING_SYSTEM_ERROR, codingSystem);
    }

    /**
     * <pre>
     * Return t if OBJECT is nil or a coding-system.
     * See the documentation of `define-coding-system' for information
     * about coding-system objects.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCodingSystemP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean codingSystemP(Object object) {
            if (isNil(object)) {
                return true;
            }
            if (toSym(object) instanceof ELispSymbol sym) {
                return getThis(null).getCodingSystem(sym) != null || !isNil(BuiltInFns.FGet.get(sym, CODING_SYSTEM_DEFINE_FORM));
            }
            return false;
        }
    }

    /**
     * <pre>
     * Read a coding system from the minibuffer, prompting with string PROMPT.
     * </pre>
     */
    @ELispBuiltIn(name = "read-non-nil-coding-system", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FReadNonNilCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readNonNilCodingSystem(Object prompt) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Read a coding system from the minibuffer, prompting with string PROMPT.
     * If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
     * Ignores case when completing coding systems (all Emacs coding systems
     * are lower-case).
     * </pre>
     */
    @ELispBuiltIn(name = "read-coding-system", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FReadCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Void readCodingSystem(Object prompt, Object defaultCodingSystem) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Check validity of CODING-SYSTEM.
     * If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
     * It is valid if it is nil or a symbol defined as a coding system by the
     * function `define-coding-system'.
     * </pre>
     */
    @ELispBuiltIn(name = "check-coding-system", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCheckCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Object checkCodingSystem(Object codingSystem) {
            return BuiltInCoding.checkCodingSystem(codingSystem);
        }
    }

    /**
     * <pre>
     * Detect coding system of the text in the region between START and END.
     * Return a list of possible coding systems ordered by priority.
     * The coding systems to try and their priorities follows what
     * the function `coding-system-priority-list' (which see) returns.
     *
     * If only ASCII characters are found (except for such ISO-2022 control
     * characters as ESC), it returns a list of single element `undecided'
     * or its subsidiary coding system according to a detected end-of-line
     * format.
     *
     * If optional argument HIGHEST is non-nil, return the coding system of
     * highest priority.
     * </pre>
     */
    @ELispBuiltIn(name = "detect-coding-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDetectCodingRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void detectCodingRegion(Object start, Object end, Object highest) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Detect coding system of the text in STRING.
     * Return a list of possible coding systems ordered by priority.
     * The coding systems to try and their priorities follows what
     * the function `coding-system-priority-list' (which see) returns.
     *
     * If only ASCII characters are found (except for such ISO-2022 control
     * characters as ESC), it returns a list of single element `undecided'
     * or its subsidiary coding system according to a detected end-of-line
     * format.
     *
     * If optional argument HIGHEST is non-nil, return the coding system of
     * highest priority.
     * </pre>
     */
    @ELispBuiltIn(name = "detect-coding-string", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDetectCodingString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void detectCodingString(Object string, Object highest) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "find-coding-systems-region-internal", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFindCodingSystemsRegionInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void findCodingSystemsRegionInternal(Object start, Object end, Object exclude) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return position of first un-encodable character in a region.
     * START and END specify the region and CODING-SYSTEM specifies the
     * encoding to check.  Return nil if CODING-SYSTEM does encode the region.
     *
     * If optional 4th argument COUNT is non-nil, it specifies at most how
     * many un-encodable characters to search.  In this case, the value is a
     * list of positions.
     *
     * If optional 5th argument STRING is non-nil, it is a string to search
     * for un-encodable characters.  In that case, START and END are indexes
     * to the string and treated as in `substring'.
     * </pre>
     */
    @ELispBuiltIn(name = "unencodable-char-position", minArgs = 3, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FUnencodableCharPosition extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unencodableCharPosition(Object start, Object end, Object codingSystem, Object count, Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Check if text between START and END is encodable by CODING-SYSTEM-LIST.
     *
     * START and END are buffer positions specifying the region.
     * CODING-SYSTEM-LIST is a list of coding systems to check.
     *
     * If all coding systems in CODING-SYSTEM-LIST can encode the region, the
     * function returns nil.
     *
     * If some of the coding systems cannot encode the whole region, value is
     * an alist, each element of which has the form (CODING-SYSTEM POS1 POS2 ...),
     * which means that CODING-SYSTEM cannot encode the text at buffer positions
     * POS1, POS2, ...
     *
     * START may be a string.  In that case, check if the string is
     * encodable, and the value contains character indices into the string
     * instead of buffer positions.  END is ignored in this case.
     *
     * If the current buffer (or START if it is a string) is unibyte, the value
     * is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "check-coding-systems-region", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FCheckCodingSystemsRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void checkCodingSystemsRegion(Object start, Object end, Object codingSystemList) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Decode the current region using the specified coding system.
     * Interactively, prompt for the coding system to decode the region, and
     * replace the region with the decoded text.
     *
     * \"Decoding\" means transforming bytes into readable text (characters).
     * If, for instance, you have a region that contains data that represents
     * the two bytes #xc2 #xa9, after calling this function with the utf-8
     * coding system, the region will contain the single
     * character ?\\N{COPYRIGHT SIGN}.
     *
     * When called from a program, takes four arguments:
     *         START, END, CODING-SYSTEM, and DESTINATION.
     * START and END are buffer positions.
     *
     * Optional 4th arguments DESTINATION specifies where the decoded text goes.
     * If nil, the region between START and END is replaced by the decoded text.
     * If buffer, the decoded text is inserted in that buffer after point (point
     * does not move).  If that buffer is unibyte, it receives the individual
     * bytes of the internal representation of the decoded text.
     * In those cases, the length of the decoded text is returned.
     * If DESTINATION is t, the decoded text is returned.
     *
     * This function sets `last-coding-system-used' to the precise coding system
     * used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
     * not fully specified.)
     * </pre>
     */
    @ELispBuiltIn(name = "decode-coding-region", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FDecodeCodingRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void decodeCodingRegion(Object start, Object end, Object codingSystem, Object destination) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Encode the current region using the specified coding system.
     * Interactively, prompt for the coding system to encode the region, and
     * replace the region with the bytes that are the result of the encoding.
     *
     * What's meant by \"encoding\" is transforming textual data (characters)
     * into bytes.  If, for instance, you have a region that contains the
     * single character ?\\N{COPYRIGHT SIGN}, after calling this function with
     * the utf-8 coding system, the data in the region will represent the two
     * bytes #xc2 #xa9.
     *
     * When called from a program, takes four arguments:
     *         START, END, CODING-SYSTEM and DESTINATION.
     * START and END are buffer positions.
     *
     * Optional 4th argument DESTINATION specifies where the encoded text goes.
     * If nil, the region between START and END is replaced by the encoded text.
     * If buffer, the encoded text is inserted in that buffer after point (point
     * does not move).
     * In those cases, the length of the encoded text is returned.
     * If DESTINATION is t, the encoded text is returned.
     *
     * This function sets `last-coding-system-used' to the precise coding system
     * used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
     * not fully specified.)
     * </pre>
     */
    @ELispBuiltIn(name = "encode-coding-region", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FEncodeCodingRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void encodeCodingRegion(Object start, Object end, Object codingSystem, Object destination) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-encode-string-utf-8", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FInternalEncodeStringUtf8 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalEncodeStringUtf8(Object string, Object buffer, Object nocopy, Object handle8Bit, Object handleOverUni, Object encodeMethod, Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "internal-decode-string-utf-8", minArgs = 7, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FInternalDecodeStringUtf8 extends ELispBuiltInBaseNode {
        @Specialization
        public static Void internalDecodeStringUtf8(Object string, Object buffer, Object nocopy, Object handle8Bit, Object handleOverUni, Object decodeMethod, Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Decode STRING which is encoded in CODING-SYSTEM, and return the result.
     *
     * Optional third arg NOCOPY non-nil means it is OK to return STRING itself
     * if the decoding operation is trivial.
     *
     * Optional fourth arg BUFFER non-nil means that the decoded text is
     * inserted in that buffer after point (point does not move).  In this
     * case, the return value is the length of the decoded text.  If that
     * buffer is unibyte, it receives the individual bytes of the internal
     * representation of the decoded text.
     *
     * This function sets `last-coding-system-used' to the precise coding system
     * used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
     * not fully specified.)  The function does not change the match data.
     * </pre>
     */
    @ELispBuiltIn(name = "decode-coding-string", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FDecodeCodingString extends ELispBuiltInBaseNode {
        @Specialization
        public Object decodeCodingString(ELispString string, ELispSymbol codingSystem, Object nocopy, Object buffer) {
            FCheckCodingSystem.checkCodingSystem(codingSystem);
            ELispCodings codings = getThis(this);
            ELispCodingSystem coding = codings.resolveCodingSystem(codingSystem);
            if (coding.getSpec().isAsciiCompat() && string.isAscii()) {
                return isNil(nocopy) ? FCopySequence.copySequenceString(string) : string;
            }

            // TODO: handle decoding multibyte strings, like "中\377"

            ValueStorage.Forwarded container = new ValueStorage.Forwarded();
            SeekableInMemoryByteChannel channel = null;
            try {
                channel = new SeekableInMemoryByteChannel(string.bytes());
                return codings.decode(
                        coding, channel, 0, string.bytes().length, container
                ).build();
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, ELispGlobals.STRING);
            } finally {
                if (channel != null) {
                    channel.close();
                }
            }
        }
    }

    /**
     * <pre>
     * Encode STRING to CODING-SYSTEM, and return the result.
     *
     * Optional third arg NOCOPY non-nil means it is OK to return STRING
     * itself if the encoding operation is trivial.
     *
     * Optional fourth arg BUFFER non-nil means that the encoded text is
     * inserted in that buffer after point (point does not move).  In this
     * case, the return value is the length of the encoded text.
     *
     * This function sets `last-coding-system-used' to the precise coding system
     * used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
     * not fully specified.)  The function does not change the match data.
     * </pre>
     */
    @ELispBuiltIn(name = "encode-coding-string", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FEncodeCodingString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object encodeCodingString(Object string, Object codingSystem, Object nocopy, Object buffer) {
            // TODO
            return string;
        }
    }

    /**
     * <pre>
     * Decode a Japanese character which has CODE in shift_jis encoding.
     * Return the corresponding character.
     * </pre>
     */
    @ELispBuiltIn(name = "decode-sjis-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDecodeSjisChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void decodeSjisChar(Object code) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Encode a Japanese character CH to shift_jis encoding.
     * Return the corresponding code in SJIS.
     * </pre>
     */
    @ELispBuiltIn(name = "encode-sjis-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FEncodeSjisChar extends ELispBuiltInBaseNode {
        @Specialization
        public static Void encodeSjisChar(Object ch) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Decode a Big5 character which has CODE in BIG5 coding system.
     * Return the corresponding character.
     * </pre>
     */
    @ELispBuiltIn(name = "decode-big5-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDecodeBig5Char extends ELispBuiltInBaseNode {
        @Specialization
        public static Void decodeBig5Char(Object code) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Encode the Big5 character CH to BIG5 coding system.
     * Return the corresponding character code in Big5.
     * </pre>
     */
    @ELispBuiltIn(name = "encode-big5-char", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FEncodeBig5Char extends ELispBuiltInBaseNode {
        @Specialization
        public static Void encodeBig5Char(Object ch) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "set-terminal-coding-system-internal", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetTerminalCodingSystemInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setTerminalCodingSystemInternal(Object codingSystem, Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "set-safe-terminal-coding-system-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetSafeTerminalCodingSystemInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean setSafeTerminalCodingSystemInternal(Object codingSystem) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return coding system specified for terminal output on the given terminal.
     * TERMINAL may be a terminal object, a frame, or nil for the selected
     * frame's terminal device.
     * </pre>
     */
    @ELispBuiltIn(name = "terminal-coding-system", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTerminalCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Void terminalCodingSystem(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Internal use only.
     * </pre>
     */
    @ELispBuiltIn(name = "set-keyboard-coding-system-internal", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetKeyboardCodingSystemInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setKeyboardCodingSystemInternal(Object codingSystem, Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return coding system specified for decoding keyboard input.
     * </pre>
     */
    @ELispBuiltIn(name = "keyboard-coding-system", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKeyboardCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Void keyboardCodingSystem(Object terminal) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Choose a coding system for an operation based on the target name.
     * The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
     * DECODING-SYSTEM is the coding system to use for decoding
     * \(in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
     * for encoding (in case OPERATION does encoding).
     *
     * The first argument OPERATION specifies an I/O primitive:
     *   For file I/O, `insert-file-contents' or `write-region'.
     *   For process I/O, `call-process', `call-process-region', or `start-process'.
     *   For network I/O, `open-network-stream'.
     *
     * The remaining arguments should be the same arguments that were passed
     * to the primitive.  Depending on which primitive, one of those arguments
     * is selected as the TARGET.  For example, if OPERATION does file I/O,
     * whichever argument specifies the file name is TARGET.
     *
     * TARGET has a meaning which depends on OPERATION:
     *   For file I/O, TARGET is a file name (except for the special case below).
     *   For process I/O, TARGET is a process name.
     *   For network I/O, TARGET is a service name or a port number.
     *
     * This function looks up what is specified for TARGET in
     * `file-coding-system-alist', `process-coding-system-alist',
     * or `network-coding-system-alist' depending on OPERATION.
     * They may specify a coding system, a cons of coding systems,
     * or a function symbol to call.
     * In the last case, we call the function with one argument,
     * which is a list of all the arguments given to this function.
     * If the function can't decide a coding system, it can return
     * `undecided' so that the normal code-detection is performed.
     *
     * If OPERATION is `insert-file-contents', the argument corresponding to
     * TARGET may be a cons (FILENAME . BUFFER).  In that case, FILENAME is a
     * file name to look up, and BUFFER is a buffer that contains the file's
     * contents (not yet decoded).  If `file-coding-system-alist' specifies a
     * function to call for FILENAME, that function should examine the
     * contents of BUFFER instead of reading the file.
     *
     * usage: (find-operation-coding-system OPERATION ARGUMENTS...)
     * </pre>
     */
    @ELispBuiltIn(name = "find-operation-coding-system", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFindOperationCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Void findOperationCodingSystem(Object operation, Object[] arguments) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Assign higher priority to the coding systems given as arguments.
     * If multiple coding systems belong to the same category,
     * all but the first one are ignored.
     *
     * usage: (set-coding-system-priority &amp;rest coding-systems)
     * </pre>
     */
    @ELispBuiltIn(name = "set-coding-system-priority", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FSetCodingSystemPriority extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean setCodingSystemPriority(Object[] codingSystems) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return a list of coding systems ordered by their priorities.
     * The list contains a subset of coding systems; i.e. coding systems
     * assigned to each coding category (see `coding-category-list').
     *
     * HIGHESTP non-nil means just return the highest priority one.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-priority-list", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCodingSystemPriorityList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void codingSystemPriorityList(Object highestp) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * For internal use only.
     * usage: (define-coding-system-internal ...)
     * </pre>
     */
    @ELispBuiltIn(name = "define-coding-system-internal", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FDefineCodingSystemInternal extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public static boolean defineCodingSystemInternal(Object[] args) {
            if (args.length < CODING_ARG_MAX) {
                throw ELispSignals.wrongNumberOfArguments(DEFINE_CODING_SYSTEM_INTERNAL, args.length);
            }
            ELispCodings coding = getThis(null);

            ELispVector attrs = new ELispVector(CODING_ATTR_LAST_INDEX, false);

            ELispSymbol name = asSym(args[CODING_ARG_NAME]);
            attrs.set(CODING_ATTR_BASE_NAME, name);

            long mnemonic = checkMnemonic(args[CODING_ARG_MNEMONIC]);
            attrs.set(CODING_ATTR_MNEMONIC, mnemonic);

            ELispSymbol codingType = asSym(args[CODING_ARG_CODING_TYPE]);
            attrs.set(CODING_ATTR_TYPE, codingType);

            Pair<Object, ELispString> charsetListPair = checkCharSetList(args[CODING_ARG_CHARSET_LIST], codingType);
            Object charsetList = charsetListPair.getLeft();
            ELispString safeCharsets = charsetListPair.getRight();
            attrs.set(CODING_ATTR_CHARSET_LIST, charsetList);
            attrs.set(CODING_ATTR_SAFE_CHARSETS, safeCharsets);

            attrs.set(CODING_ATTR_ASCII_COMPAT, args[CODING_ARG_ASCII_COMPATIBLE_P]);

            Object decodeTable = args[CODING_ARG_DECODE_TRANSLATION_TABLE];
            if (!(decodeTable instanceof ELispCharTable) && !(decodeTable instanceof ELispCons)) {
                asSym(decodeTable);
            }
            attrs.set(CODING_ATTR_DECODE_TBL, decodeTable);

            Object encodeTable = args[CODING_ARG_ENCODE_TRANSLATION_TABLE];
            if (!(encodeTable instanceof ELispCharTable) && !(encodeTable instanceof ELispCons)) {
                asSym(encodeTable);
            }
            attrs.set(CODING_ATTR_ENCODE_TBL, encodeTable);

            attrs.set(CODING_ATTR_POST_READ, asSym(args[CODING_ARG_POST_READ_CONVERSION]));
            attrs.set(CODING_ATTR_PRE_WRITE, asSym(args[CODING_ARG_PRE_WRITE_CONVERSION]));
            Object defaultChar = args[CODING_ARG_DEFAULT_CHAR];
            attrs.set(CODING_ATTR_DEFAULT_CHAR, isNil(defaultChar) ? (long) ' ' : asChar(defaultChar));
            attrs.set(CODING_ATTR_FOR_UNIBYTE, !isNil(args[CODING_ARG_FOR_UNIBYTE]));
            attrs.set(CODING_ATTR_PLIST, asList(args[CODING_ARG_PLIST]));

            int category = coding.initExtraAttrs(codingType, attrs, args, charsetList);
            attrs.set(CODING_ATTR_CATEGORY, (long) category);
            attrs.set(
                    CODING_ATTR_PLIST,
                    new ELispCons.ListBuilder()
                            .add(CASCII_COMPATIBLE_P)
                            .add(attrs.get(CODING_ATTR_ASCII_COMPAT))
                            .add(CCATEGORY)
                            .add(attrs.get(CODING_ATTR_CATEGORY))
                            .buildWithCdr(attrs.get(CODING_ATTR_PLIST))
            );

            Object eolType = args[CODING_ARG_EOL_TYPE];
            if (!isNil(eolType) && eolType != UNIX && eolType != DOS && eolType != MAC) {
                throw ELispSignals.error("Invalid eol-type");
            }

            ELispCons aliases = ELispCons.listOf(name);
            ELispSymbol[] eolTypes;
            if (isNil(eolType)) {
                ELispSymbol[] subsidiaries = makeSubsidiaries(name);
                eolTypes = subsidiaries;
                for (int i = 0; i < subsidiaries.length; i++) {
                    ELispSymbol subsidiary = subsidiaries[i];
                    ELispCons currentAliases = ELispCons.listOf(subsidiary);
                    ELispSymbol currentEolType = switch (i) {
                        case 0 -> UNIX;
                        case 1 -> DOS;
                        default -> MAC;
                    };
                    coding.addCodingSystem(
                            codingType,
                            subsidiary,
                            new ELispCodingSystem.Spec(subsidiary, attrs, currentAliases, currentEolType),
                            -1
                    );
                }
            } else {
                eolTypes = new ELispSymbol[]{asSym(eolType)};
            }
            ELispCodingSystem.Spec system = new ELispCodingSystem.Spec(name, attrs, aliases, eolTypes);
            coding.addCodingSystem(codingType, name, system, category);
            return false;
        }

        private static ELispSymbol[] makeSubsidiaries(ELispSymbol baseString) {
            ELispString base = baseString.name();
            ELispSymbol[] symbols = new ELispSymbol[3];
            String[] suffixes = {"-unix", "-dos", "-mac"};
            for (int i = 0; i < suffixes.length; i++) {
                symbols[i] = ELispContext.get(null).intern(StringSupport.appendAscii(base, suffixes[i]));
            }
            return symbols;
        }

        @TruffleBoundary
        private static Pair<Object, ELispString> checkCharSetList(Object arg, ELispSymbol codingType) {
            int maxCharsetId = 0;
            ELispCons list;
            if (isNil(arg)) {
                list = null;
            } else if (toSym(arg) instanceof ELispSymbol symbol) {
                if (symbol == ISO_2022) {
                    if (codingType != ISO_2022) {
                        throw ELispSignals.error("Invalid charset-list");
                    }
                    list = asCons(BuiltInCharSet.getThis(null).iso2022CharsetList.getValue());
                } else if (symbol == EMACS_MULE) {
                    if (codingType != EMACS_MULE) {
                        throw ELispSignals.error("Invalid charset-list");
                    }
                    list = asCons(BuiltInCharSet.getThis(null).emacsMuleCharsetList.getValue());
                } else {
                    throw ELispSignals.error("Invalid charset-list");
                }
                maxCharsetId = list.stream().mapToInt(ELispTypeSystem::asInt).max().orElse(0);
            } else {
                list = BuiltInFns.FCopySequence.copySequenceList(asCons(arg));
                ConsIterator iterator = list.listIterator(0);
                while (iterator.hasNextCons()){
                    ELispCons current = iterator.nextCons();
                    Object charset = current.car();
                    ELispCharset cs = BuiltInCharSet.getCharset(charset);
                    if ((codingType == ISO_2022 && cs.isoFinal < 0)
                            || (codingType == EMACS_MULE && cs.emacsMuleId < 0)
                    ) {
                        throw ELispSignals.error("Can't handle charset");
                    }
                    current.setCar((long) cs.id);
                    maxCharsetId = Math.max(maxCharsetId, cs.id);
                }
            }
            byte[] safeCharsetsBytes = new byte[maxCharsetId + 1];
            Arrays.fill(safeCharsetsBytes, (byte) 0xFF);
            if (list != null) {
                for (Object id : list) {
                    safeCharsetsBytes[asInt(id)] = 0;
                }
            }
            ELispString safeCharsets = ELispString.ofBytes(safeCharsetsBytes);
            return Pair.create(list == null ? false : list, safeCharsets);
        }

        private static long checkMnemonic(Object arg) {
            if (arg instanceof ELispString s) {
                return s.codePointAt(0);
            }
            return asChar(arg);
        }
    }

    /**
     * <pre>
     * Change value of CODING-SYSTEM's property PROP to VAL.
     *
     * The following properties, if set by this function, override the values
     * of the corresponding attributes set by `define-coding-system':
     *
     *   `:mnemonic', `:default-char', `:ascii-compatible-p'
     *   `:decode-translation-table', `:encode-translation-table',
     *   `:post-read-conversion', `:pre-write-conversion'
     *
     * See `define-coding-system' for the description of these properties.
     * See `coding-system-get' and `coding-system-plist' for accessing the
     * property list of a coding-system.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FCodingSystemPut extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean codingSystemPut(Object codingSystem, Object prop, Object val) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Define ALIAS as an alias for CODING-SYSTEM.
     * </pre>
     */
    @ELispBuiltIn(name = "define-coding-system-alias", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDefineCodingSystemAlias extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean defineCodingSystemAlias(Object alias, Object codingSystem) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the base of CODING-SYSTEM.
     * Any alias or subsidiary coding system is not a base coding system.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-base", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCodingSystemBase extends ELispBuiltInBaseNode {
        @Specialization
        public ELispSymbol codingSystemBase(ELispSymbol codingSystem) {
            if (isNil(codingSystem)) {
                return NO_CONVERSION;
            }
            checkCodingSystem(codingSystem);
            ELispCodingSystem system = getThis(this).getCodingSystem(codingSystem);
            if (system == null) {
                throw ELispSignals.wrongTypeArgument(CODING_SYSTEM_P, codingSystem);
            }
            return asSym(system.getSpec().getBaseName());
        }
    }

    /**
     * <pre>
     * Return the property list of CODING-SYSTEM.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-plist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCodingSystemPlist extends ELispBuiltInBaseNode {
        @Specialization
        public Object codingSystemPlist(ELispSymbol codingSystem) {
            if (codingSystem == NIL) {
                codingSystem = UNDECIDED;
            }
            ELispCodingSystem system = getThis(this).getCodingSystem(codingSystem);
            if (system == null) {
                throw ELispSignals.wrongTypeArgument(CODING_SYSTEM_P, codingSystem);
            }
            return system.getSpec().getPlist();
        }
    }

    /**
     * <pre>
     * Return the list of aliases of CODING-SYSTEM.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-aliases", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCodingSystemAliases extends ELispBuiltInBaseNode {
        @Specialization
        public static Void codingSystemAliases(Object codingSystem) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return eol-type of CODING-SYSTEM.
     * An eol-type is an integer 0, 1, 2, or a vector of coding systems.
     *
     * Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
     * and CR respectively.
     *
     * A vector value indicates that a format of end-of-line should be
     * detected automatically.  Nth element of the vector is the subsidiary
     * coding system whose eol-type is N.
     * </pre>
     */
    @ELispBuiltIn(name = "coding-system-eol-type", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FCodingSystemEolType extends ELispBuiltInBaseNode {
        @Specialization
        public Object codingSystemEolType(ELispSymbol codingSystem) {
            if (isNil(codingSystem)) {
                codingSystem = NO_CONVERSION;
            }
            if (!FCodingSystemP.codingSystemP(codingSystem)) {
                return false;
            }
            ELispCodingSystem system = getThis(this).getCodingSystem(codingSystem);
            if (system == null) {
                throw ELispSignals.wrongTypeArgument(CODING_SYSTEM_P, codingSystem);
            }
            ELispSymbol[] eolTypes = system.getSpec().eolTypes();
            if (eolTypes.length > 1) {
                return new ELispVector(((Object[]) eolTypes).clone()); // NOPMD
            }
            return eolTypes[0] == UNIX ? 0L : (eolTypes[0] == DOS ? 1L : 2L);
        }
    }
}
