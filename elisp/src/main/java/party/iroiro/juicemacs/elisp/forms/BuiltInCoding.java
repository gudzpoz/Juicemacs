package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInCoding extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInCodingFactory.getFactories();
    }

    public final static int CODING_CATEGORY_ISO_7 = 0;
    public final static int CODING_CATEGORY_ISO_7_TIGHT = 1;
    public final static int CODING_CATEGORY_ISO_8_1 = 2;
    public final static int CODING_CATEGORY_ISO_8_2 = 3;
    public final static int CODING_CATEGORY_ISO_7_ELSE = 4;
    public final static int CODING_CATEGORY_ISO_8_ELSE = 5;
    public final static int CODING_CATEGORY_UTF_8_AUTO = 6;
    public final static int CODING_CATEGORY_UTF_8_NOSIG = 7;
    public final static int CODING_CATEGORY_UTF_8_SIG = 8;
    public final static int CODING_CATEGORY_UTF_16_AUTO = 9;
    public final static int CODING_CATEGORY_UTF_16_BE = 10;
    public final static int CODING_CATEGORY_UTF_16_LE = 11;
    public final static int CODING_CATEGORY_UTF_16_BE_NOSIG = 12;
    public final static int CODING_CATEGORY_UTF_16_LE_NOSIG = 13;
    public final static int CODING_CATEGORY_CHARSET = 14;
    public final static int CODING_CATEGORY_SJIS = 15;
    public final static int CODING_CATEGORY_BIG5 = 16;
    public final static int CODING_CATEGORY_CCL = 17;
    public final static int CODING_CATEGORY_EMACS_MULE = 18;
    public final static int CODING_CATEGORY_RAW_TEXT = 19;
    public final static int CODING_CATEGORY_UNDECIDED = 20;
    public final static int CODING_CATEGORY_MAX = 21;

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
        public static Void codingSystemP(Object object) {
            throw new UnsupportedOperationException();
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
        public static Void checkCodingSystem(Object codingSystem) {
            throw new UnsupportedOperationException();
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
        public static Void decodeCodingString(Object string, Object codingSystem, Object nocopy, Object buffer) {
            throw new UnsupportedOperationException();
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
        public static Void encodeCodingString(Object string, Object codingSystem, Object nocopy, Object buffer) {
            throw new UnsupportedOperationException();
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
        @Specialization
        public static boolean defineCodingSystemInternal(Object[] args) {
            // TODO
            return false;
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
        public static Void codingSystemBase(Object codingSystem) {
            throw new UnsupportedOperationException();
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
        public static Void codingSystemPlist(Object codingSystem) {
            throw new UnsupportedOperationException();
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
        public static Void codingSystemEolType(Object codingSystem) {
            throw new UnsupportedOperationException();
        }
    }
}
