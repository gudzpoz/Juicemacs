package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

/**
 * Built-in functions from {@code src/process.c}
 */
public class BuiltInProcess extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInProcessFactory.getFactories();
    }

    @ELispBuiltIn(name = "processp", minArgs = 1, maxArgs = 1, doc = "Return t if OBJECT is a process.")
    @GenerateNodeFactory
    public abstract static class FProcessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-process", minArgs = 1, maxArgs = 1, doc = "Return the process named NAME, or nil if there is none.")
    @GenerateNodeFactory
    public abstract static class FGetProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getProcess(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-process", minArgs = 0, maxArgs = 1, doc = "Delete PROCESS: kill it and forget about it immediately.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\n\nInteractively, it will kill the current buffer's process.")
    @GenerateNodeFactory
    public abstract static class FDeleteProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteProcess(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-status", minArgs = 1, maxArgs = 1, doc = "Return the status of PROCESS.\nThe returned value is one of the following symbols:\nrun  -- for a process that is running.\nstop -- for a process stopped but continuable.\nexit -- for a process that has exited.\nsignal -- for a process that has got a fatal signal.\nopen -- for a network stream connection that is open.\nlisten -- for a network stream server that is listening.\nclosed -- for a network stream connection that is closed.\nconnect -- when waiting for a non-blocking connection to complete.\nfailed -- when a non-blocking connection has failed.\nnil -- if arg is a process name and no such process exists.\nPROCESS may be a process, a buffer, the name of a process, or\nnil, indicating the current buffer's process.")
    @GenerateNodeFactory
    public abstract static class FProcessStatus extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processStatus(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-exit-status", minArgs = 1, maxArgs = 1, doc = "Return the exit status of PROCESS or the signal number that killed it.\nIf PROCESS has not yet exited or died, return 0.")
    @GenerateNodeFactory
    public abstract static class FProcessExitStatus extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processExitStatus(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-id", minArgs = 1, maxArgs = 1, doc = "Return the process id of PROCESS.\nThis is the pid of the external process which PROCESS uses or talks to,\nan integer.\nFor a network, serial, and pipe connections, this value is nil.")
    @GenerateNodeFactory
    public abstract static class FProcessId extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processId(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-name", minArgs = 1, maxArgs = 1, doc = "Return the name of PROCESS, as a string.\nThis is the name of the program invoked in PROCESS,\npossibly modified to make it unique among process names.")
    @GenerateNodeFactory
    public abstract static class FProcessName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-command", minArgs = 1, maxArgs = 1, doc = "Return the command that was executed to start PROCESS.\nThis is a list of strings, the first string being the program executed\nand the rest of the strings being the arguments given to it.\nFor a network or serial or pipe connection, this is nil (process is running)\nor t (process is stopped).")
    @GenerateNodeFactory
    public abstract static class FProcessCommand extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processCommand(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-tty-name", minArgs = 1, maxArgs = 2, doc = "Return the name of the terminal PROCESS uses, or nil if none.\nThis is the terminal that the process itself reads and writes on,\nnot the name of the pty that Emacs uses to talk with that terminal.\n\nIf STREAM is nil, return the terminal name if any of PROCESS's\nstandard streams use a terminal for communication.  If STREAM is one\nof `stdin', `stdout', or `stderr', return the name of the terminal\nPROCESS uses for that stream specifically, or nil if that stream\ncommunicates via a pipe.")
    @GenerateNodeFactory
    public abstract static class FProcessTtyName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processTtyName(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-buffer", minArgs = 2, maxArgs = 2, doc = "Set buffer associated with PROCESS to BUFFER (a buffer, or nil).\nReturn BUFFER.")
    @GenerateNodeFactory
    public abstract static class FSetProcessBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessBuffer(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-buffer", minArgs = 1, maxArgs = 1, doc = "Return the buffer PROCESS is associated with.\nThe default process filter inserts output from PROCESS into this buffer.")
    @GenerateNodeFactory
    public abstract static class FProcessBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-mark", minArgs = 1, maxArgs = 1, doc = "Return the marker for the end of the last output from PROCESS.")
    @GenerateNodeFactory
    public abstract static class FProcessMark extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processMark(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-filter", minArgs = 2, maxArgs = 2, doc = "Give PROCESS the filter function FILTER; nil means default.\nA value of t means stop accepting output from the process.\n\nWhen a process has a non-default filter, its buffer is not used for output.\nInstead, each time it does output, the entire string of output is\npassed to the filter.\n\nThe filter gets two arguments: the process and the string of output.\nThe string argument is normally a multibyte string, except:\n- if the process's input coding system is no-conversion or raw-text,\n  it is a unibyte string (the non-converted input).")
    @GenerateNodeFactory
    public abstract static class FSetProcessFilter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessFilter(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-filter", minArgs = 1, maxArgs = 1, doc = "Return the filter function of PROCESS.\nSee `set-process-filter' for more info on filter functions.")
    @GenerateNodeFactory
    public abstract static class FProcessFilter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processFilter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-sentinel", minArgs = 2, maxArgs = 2, doc = "Give PROCESS the sentinel SENTINEL; nil for default.\nThe sentinel is called as a function when the process changes state.\nIt gets two arguments: the process, and a string describing the change.")
    @GenerateNodeFactory
    public abstract static class FSetProcessSentinel extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessSentinel(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-sentinel", minArgs = 1, maxArgs = 1, doc = "Return the sentinel of PROCESS.\nSee `set-process-sentinel' for more info on sentinels.")
    @GenerateNodeFactory
    public abstract static class FProcessSentinel extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSentinel(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-thread", minArgs = 2, maxArgs = 2, doc = "Set the locking thread of PROCESS to be THREAD.\nIf THREAD is nil, the process is unlocked.")
    @GenerateNodeFactory
    public abstract static class FSetProcessThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessThread(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-thread", minArgs = 1, maxArgs = 1, doc = "Return the locking thread of PROCESS.\nIf PROCESS is unlocked, this function returns nil.")
    @GenerateNodeFactory
    public abstract static class FProcessThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processThread(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-window-size", minArgs = 3, maxArgs = 3, doc = "Tell PROCESS that it has logical window size WIDTH by HEIGHT.\nValue is t if PROCESS was successfully told about the window size,\nnil otherwise.")
    @GenerateNodeFactory
    public abstract static class FSetProcessWindowSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessWindowSize(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-inherit-coding-system-flag", minArgs = 2, maxArgs = 2, doc = "Determine whether buffer of PROCESS will inherit coding-system.\nIf the second argument FLAG is non-nil, then the variable\n`buffer-file-coding-system' of the buffer associated with PROCESS\nwill be bound to the value of the coding system used to decode\nthe process output.\n\nThis is useful when the coding system specified for the process buffer\nleaves either the character code conversion or the end-of-line conversion\nunspecified, or if the coding system used to decode the process output\nis more appropriate for saving the process buffer.\n\nBinding the variable `inherit-process-coding-system' to non-nil before\nstarting the process is an alternative way of setting the inherit flag\nfor the process which will run.\n\nThis function returns FLAG.")
    @GenerateNodeFactory
    public abstract static class FSetProcessInheritCodingSystemFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessInheritCodingSystemFlag(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-query-on-exit-flag", minArgs = 2, maxArgs = 2, doc = "Specify if query is needed for PROCESS when Emacs is exited.\nIf the second argument FLAG is non-nil, Emacs will query the user before\nexiting or killing a buffer if PROCESS is running.  This function\nreturns FLAG.")
    @GenerateNodeFactory
    public abstract static class FSetProcessQueryOnExitFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessQueryOnExitFlag(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-query-on-exit-flag", minArgs = 1, maxArgs = 1, doc = "Return the current value of query-on-exit flag for PROCESS.")
    @GenerateNodeFactory
    public abstract static class FProcessQueryOnExitFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processQueryOnExitFlag(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-contact", minArgs = 1, maxArgs = 3, doc = "Return the contact info of PROCESS; t for a real child.\nFor a network or serial or pipe connection, the value depends on the\noptional KEY arg.  If KEY is nil, value is a cons cell of the form\n\\(HOST SERVICE) for a network connection or (PORT SPEED) for a serial\nconnection; it is t for a pipe connection.  If KEY is t, the complete\ncontact information for the connection is returned, else the specific\nvalue for the keyword KEY is returned.  See `make-network-process',\n`make-serial-process', or `make-pipe-process' for the list of keywords.\n\nIf PROCESS is a non-blocking network process that hasn't been fully\nset up yet, this function will block until socket setup has completed.\nIf the optional NO-BLOCK parameter is specified, return nil instead of\nwaiting for the process to be fully set up.")
    @GenerateNodeFactory
    public abstract static class FProcessContact extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processContact(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-plist", minArgs = 1, maxArgs = 1, doc = "Return the plist of PROCESS.")
    @GenerateNodeFactory
    public abstract static class FProcessPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processPlist(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-plist", minArgs = 2, maxArgs = 2, doc = "Replace the plist of PROCESS with PLIST.  Return PLIST.")
    @GenerateNodeFactory
    public abstract static class FSetProcessPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessPlist(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-connection", minArgs = 1, maxArgs = 1, doc = "Return the connection type of PROCESS.\nThe value is nil for a pipe, t or `pty' for a pty, or `stream' for\na socket connection.")
    @GenerateNodeFactory
    public abstract static class FProcessConnection extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processConnection(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-type", minArgs = 1, maxArgs = 1, doc = "Return the connection type of PROCESS.\nThe value is either the symbol `real', `network', `serial', or `pipe'.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.")
    @GenerateNodeFactory
    public abstract static class FProcessType extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processType(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "format-network-address", minArgs = 1, maxArgs = 2, doc = "Convert network ADDRESS from internal format to a string.\nA 4 or 5 element vector represents an IPv4 address (with port number).\nAn 8 or 9 element vector represents an IPv6 address (with port number).\nIf optional second argument OMIT-PORT is non-nil, don't include a port\nnumber in the string, even when present in ADDRESS.\nReturn nil if format of ADDRESS is invalid.")
    @GenerateNodeFactory
    public abstract static class FFormatNetworkAddress extends ELispBuiltInBaseNode {
        @Specialization
        public static Object formatNetworkAddress(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-list", minArgs = 0, maxArgs = 0, doc = "Return a list of all processes that are Emacs sub-processes.")
    @GenerateNodeFactory
    public abstract static class FProcessList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processList() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-process", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Start a program in a subprocess.  Return the process object for it.\n\nThis is similar to `start-process', but arguments are specified as\nkeyword/argument pairs.  The following arguments are defined:\n\n:name NAME -- NAME is name for process.  It is modified if necessary\nto make it unique.\n\n:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate\nwith the process.  Process output goes at end of that buffer, unless\nyou specify a filter function to handle the output.  BUFFER may be\nalso nil, meaning that this process is not associated with any buffer.\n\n:command COMMAND -- COMMAND is a list starting with the program file\nname, followed by strings to give to the program as arguments.  If the\nprogram file name is not an absolute file name, `make-process' will\nlook for the program file name in `exec-path' (which is a list of\ndirectories).\n\n:coding CODING -- If CODING is a symbol, it specifies the coding\nsystem used for both reading and writing for this process.  If CODING\nis a cons (DECODING . ENCODING), DECODING is used for reading, and\nENCODING is used for writing.\n\n:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and\nthe process is running.  If BOOL is not given, query before exiting.\n\n:stop BOOL -- BOOL must be nil.  The `:stop' key is ignored otherwise\nand is retained for compatibility with other process types such as\npipe processes.  Asynchronous subprocesses never start in the\n`stopped' state.  Use `stop-process' and `continue-process' to send\nsignals to stop and continue a process.\n\n:connection-type TYPE -- TYPE is control type of device used to\ncommunicate with subprocesses.  Values are `pipe' to use a pipe, `pty'\nto use a pty, or nil to use the default specified through\n`process-connection-type'.  If TYPE is a cons (INPUT . OUTPUT), then\nINPUT will be used for standard input and OUTPUT for standard output\n(and standard error if `:stderr' is nil).\n\n:filter FILTER -- Install FILTER as the process filter.\n\n:sentinel SENTINEL -- Install SENTINEL as the process sentinel.\n\n:stderr STDERR -- STDERR is either a buffer or a pipe process attached\nto the standard error of subprocess.  When specifying this, the\nsubprocess's standard error will always communicate via a pipe, no\nmatter the value of `:connection-type'.  If STDERR is nil, standard error\nis mixed with standard output and sent to BUFFER or FILTER.  (Note\nthat specifying :stderr will create a new, separate (but associated)\nprocess, with its own filter and sentinel.  See\nInfo node `(elisp) Asynchronous Processes' for more details.)\n\n:file-handler FILE-HANDLER -- If FILE-HANDLER is non-nil, then look\nfor a file name handler for the current buffer's `default-directory'\nand invoke that file name handler to make the process.  If there is no\nsuch handler, proceed as if FILE-HANDLER were nil.\n\nusage: (make-process &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMakeProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-pipe-process", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Create and return a bidirectional pipe process.\n\nIn Emacs, pipes are represented by process objects, so input and\noutput work as for subprocesses, and `delete-process' closes a pipe.\nHowever, a pipe process has no process id, it cannot be signaled,\nand the status codes are different from normal processes.\n\nArguments are specified as keyword/argument pairs.  The following\narguments are defined:\n\n:name NAME -- NAME is the name of the process.  It is modified if necessary to make it unique.\n\n:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate\nwith the process.  Process output goes at the end of that buffer,\nunless you specify a filter function to handle the output.  If BUFFER\nis not given, the value of NAME is used.\n\n:coding CODING -- If CODING is a symbol, it specifies the coding\nsystem used for both reading and writing for this process.  If CODING\nis a cons (DECODING . ENCODING), DECODING is used for reading, and\nENCODING is used for writing.\n\n:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and\nthe process is running.  If BOOL is not given, query before exiting.\n\n:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.\nIn the stopped state, a pipe process does not accept incoming data,\nbut you can send outgoing data.  The stopped state is cleared by\n`continue-process' and set by `stop-process'.\n\n:filter FILTER -- Install FILTER as the process filter.\n\n:sentinel SENTINEL -- Install SENTINEL as the process sentinel.\n\nusage:  (make-pipe-process &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMakePipeProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makePipeProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-datagram-address", minArgs = 1, maxArgs = 1, doc = "Get the current datagram address associated with PROCESS.\nIf PROCESS is a non-blocking network process that hasn't been fully\nset up yet, this function will block until socket setup has completed.")
    @GenerateNodeFactory
    public abstract static class FProcessDatagramAddress extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processDatagramAddress(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-datagram-address", minArgs = 2, maxArgs = 2, doc = "Set the datagram address for PROCESS to ADDRESS.\nReturn nil upon error setting address, ADDRESS otherwise.\n\nIf PROCESS is a non-blocking network process that hasn't been fully\nset up yet, this function will block until socket setup has completed.")
    @GenerateNodeFactory
    public abstract static class FSetProcessDatagramAddress extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessDatagramAddress(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-network-process-option", minArgs = 3, maxArgs = 4, doc = "For network process PROCESS set option OPTION to value VALUE.\nSee `make-network-process' for a list of options and values.\nIf optional fourth arg NO-ERROR is non-nil, don't signal an error if\nOPTION is not a supported option, return nil instead; otherwise return t.\n\nIf PROCESS is a non-blocking network process that hasn't been fully\nset up yet, this function will block until socket setup has completed.")
    @GenerateNodeFactory
    public abstract static class FSetNetworkProcessOption extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setNetworkProcessOption(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "serial-process-configure", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Configure speed, bytesize, etc. of a serial process.\n\nArguments are specified as keyword/argument pairs.  Attributes that\nare not given are re-initialized from the process's current\nconfiguration (available via the function `process-contact') or set to\nreasonable default values.  The following arguments are defined:\n\n:process PROCESS\n:name NAME\n:buffer BUFFER\n:port PORT\n-- Any of these arguments can be given to identify the process that is\nto be configured.  If none of these arguments is given, the current\nbuffer's process is used.\n\n:speed SPEED -- SPEED is the speed of the serial port in bits per\nsecond, also called baud rate.  Any value can be given for SPEED, but\nmost serial ports work only at a few defined values between 1200 and\n115200, with 9600 being the most common value.  If SPEED is nil, the\nserial port is not configured any further, i.e., all other arguments\nare ignored.  This may be useful for special serial ports such as\nBluetooth-to-serial converters which can only be configured through AT\ncommands.  A value of nil for SPEED can be used only when passed\nthrough `make-serial-process' or `serial-term'.\n\n:bytesize BYTESIZE -- BYTESIZE is the number of bits per byte, which\ncan be 7 or 8.  If BYTESIZE is not given or nil, a value of 8 is used.\n\n:parity PARITY -- PARITY can be nil (don't use parity), the symbol\n`odd' (use odd parity), or the symbol `even' (use even parity).  If\nPARITY is not given, no parity is used.\n\n:stopbits STOPBITS -- STOPBITS is the number of stopbits used to\nterminate a byte transmission.  STOPBITS can be 1 or 2.  If STOPBITS\nis not given or nil, 1 stopbit is used.\n\n:flowcontrol FLOWCONTROL -- FLOWCONTROL determines the type of\nflowcontrol to be used, which is either nil (don't use flowcontrol),\nthe symbol `hw' (use RTS/CTS hardware flowcontrol), or the symbol `sw'\n\\(use XON/XOFF software flowcontrol).  If FLOWCONTROL is not given, no\nflowcontrol is used.\n\n`serial-process-configure' is called by `make-serial-process' for the\ninitial configuration of the serial port.\n\nExamples:\n\n\\(serial-process-configure :process \"/dev/ttyS0\" :speed 1200)\n\n\\(serial-process-configure\n    :buffer \"COM1\" :stopbits 1 :parity \\\\='odd :flowcontrol \\\\='hw)\n\n\\(serial-process-configure :port \"\\\\\\\\.\\\\COM13\" :bytesize 7)\n\nusage: (serial-process-configure &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FSerialProcessConfigure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object serialProcessConfigure(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-serial-process", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Create and return a serial port process.\n\nIn Emacs, serial port connections are represented by process objects,\nso input and output work as for subprocesses, and `delete-process'\ncloses a serial port connection.  However, a serial process has no\nprocess id, it cannot be signaled, and the status codes are different\nfrom normal processes.\n\n`make-serial-process' creates a process and a buffer, on which you\nprobably want to use `process-send-string'.  Try \\\\[serial-term] for\nan interactive terminal.  See below for examples.\n\nArguments are specified as keyword/argument pairs.  The following\narguments are defined:\n\n:port PORT -- (mandatory) PORT is the path or name of the serial port.\nFor example, this could be \"/dev/ttyS0\" on Unix.  On Windows, this\ncould be \"COM1\", or \"\\\\\\\\.\\\\COM10\" for ports higher than COM9 (double\nthe backslashes in strings).\n\n:speed SPEED -- (mandatory) is handled by `serial-process-configure',\nwhich this function calls.\n\n:name NAME -- NAME is the name of the process.  If NAME is not given,\nthe value of PORT is used.\n\n:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate\nwith the process.  Process output goes at the end of that buffer,\nunless you specify a filter function to handle the output.  If BUFFER\nis not given, the value of NAME is used.\n\n:coding CODING -- If CODING is a symbol, it specifies the coding\nsystem used for both reading and writing for this process.  If CODING\nis a cons (DECODING . ENCODING), DECODING is used for reading, and\nENCODING is used for writing.\n\n:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and\nthe process is running.  If BOOL is not given, query before exiting.\n\n:stop BOOL -- Start process in the `stopped' state if BOOL is non-nil.\nIn the stopped state, a serial process does not accept incoming data,\nbut you can send outgoing data.  The stopped state is cleared by\n`continue-process' and set by `stop-process'.\n\n:filter FILTER -- Install FILTER as the process filter.\n\n:sentinel SENTINEL -- Install SENTINEL as the process sentinel.\n\n:plist PLIST -- Install PLIST as the initial plist of the process.\n\n:bytesize\n:parity\n:stopbits\n:flowcontrol\n-- This function calls `serial-process-configure' to handle these\narguments.\n\nThe original argument list, possibly modified by later configuration,\nis available via the function `process-contact'.\n\nExamples:\n\n\\(make-serial-process :port \"/dev/ttyS0\" :speed 9600)\n\n\\(make-serial-process :port \"COM1\" :speed 115200 :stopbits 2)\n\n\\(make-serial-process :port \"\\\\\\\\.\\\\COM13\" :speed 1200 :bytesize 7 :parity \\\\='odd)\n\n\\(make-serial-process :port \"/dev/tty.BlueConsole-SPP-1\" :speed nil)\n\nusage:  (make-serial-process &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMakeSerialProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeSerialProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-network-process", minArgs = 0, maxArgs = 0, varArgs = true, doc = "Create and return a network server or client process.\n\nIn Emacs, network connections are represented by process objects, so\ninput and output work as for subprocesses and `delete-process' closes\na network connection.  However, a network process has no process id,\nit cannot be signaled, and the status codes are different from normal\nprocesses.\n\nArguments are specified as keyword/argument pairs.  The following\narguments are defined:\n\n:name NAME -- NAME is name for process.  It is modified if necessary\nto make it unique.\n\n:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate\nwith the process.  Process output goes at end of that buffer, unless\nyou specify a filter function to handle the output.  BUFFER may be\nalso nil, meaning that this process is not associated with any buffer.\n\n:host HOST -- HOST is name of the host to connect to, or its IP\naddress.  The symbol `local' specifies the local host.  If specified\nfor a server process, it must be a valid name or address for the local\nhost, and only clients connecting to that address will be accepted.\nIf all interfaces should be bound, an address of \\\"0.0.0.0\\\" (for\nIPv4) or \\\"::\\\" (for IPv6) can be used.  (On some operating systems,\nusing \\\"::\\\" listens on both IPv4 and IPv6.)  `local' will use IPv4 by\ndefault, use a FAMILY of `ipv6' to override this.\n\n:service SERVICE -- SERVICE is name of the service desired, or an\ninteger specifying a port number to connect to.  If SERVICE is t,\na random port number is selected for the server.  A port number can\nbe specified as an integer string, e.g., \"80\", as well as an integer.\n\n:type TYPE -- TYPE is the type of connection.  The default (nil) is a\nstream type connection, `datagram' creates a datagram type connection,\n`seqpacket' creates a reliable datagram connection.\n\n:family FAMILY -- FAMILY is the address (and protocol) family for the\nservice specified by HOST and SERVICE.  The default (nil) is to use\nwhatever address family (IPv4 or IPv6) that is defined for the host\nand port number specified by HOST and SERVICE.  Other address families\nsupported are:\n  local -- for a local (i.e. UNIX) address specified by SERVICE.\n  ipv4  -- use IPv4 address family only.\n  ipv6  -- use IPv6 address family only.\n\n:local ADDRESS -- ADDRESS is the local address used for the connection.\nThis parameter is ignored when opening a client process. When specified\nfor a server process, the FAMILY, HOST and SERVICE args are ignored.\n\n:remote ADDRESS -- ADDRESS is the remote partner's address for the\nconnection.  This parameter is ignored when opening a stream server\nprocess.  For a datagram server process, it specifies the initial\nsetting of the remote datagram address.  When specified for a client\nprocess, the FAMILY, HOST, and SERVICE args are ignored.\n\nThe format of ADDRESS depends on the address family:\n- An IPv4 address is represented as a vector of integers [A B C D P]\ncorresponding to numeric IP address A.B.C.D and port number P.\n- An IPv6 address has the same format as an IPv4 address but with 9\nelements rather than 5.\n- A local address is represented as a string with the address in the\nlocal address space.\n- An \"unsupported family\" address is represented by a cons (F . AV)\nwhere F is the family number and AV is a vector containing the socket\naddress data with one element per address data byte.  Do not rely on\nthis format in portable code, as it may depend on implementation\ndefined constants, data sizes, and data structure alignment.\n\n:coding CODING -- If CODING is a symbol, it specifies the coding\nsystem used for both reading and writing for this process.  If CODING\nis a cons (DECODING . ENCODING), DECODING is used for reading, and\nENCODING is used for writing.\n\n:nowait BOOL -- If NOWAIT is non-nil for a stream type client\nprocess, return without waiting for the connection to complete;\ninstead, the sentinel function will be called with second arg matching\n\"open\" (if successful) or \"failed\" when the connect completes.\nDefault is to use a blocking connect (i.e. wait) for stream type\nconnections.\n\n:noquery BOOL -- Query the user unless BOOL is non-nil, and process is\nrunning when Emacs is exited.\n\n:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.\nIn the stopped state, a server process does not accept new\nconnections, and a client process does not handle incoming traffic.\nThe stopped state is cleared by `continue-process' and set by\n`stop-process'.\n\n:filter FILTER -- Install FILTER as the process filter.\n\n:filter-multibyte BOOL -- If BOOL is non-nil, strings given to the\nprocess filter are multibyte, otherwise they are unibyte.\nIf this keyword is not specified, the strings are multibyte.\n\n:sentinel SENTINEL -- Install SENTINEL as the process sentinel.\n\n:log LOG -- Install LOG as the server process log function.  This\nfunction is called when the server accepts a network connection from a\nclient.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER\nis the server process, CLIENT is the new process for the connection,\nand MESSAGE is a string.\n\n:plist PLIST -- Install PLIST as the new process's initial plist.\n\n:tls-parameters LIST -- is a list that should be supplied if you're\nopening a TLS connection.  The first element is the TLS type (either\n`gnutls-x509pki' or `gnutls-anon'), and the remaining elements should\nbe a keyword list accepted by gnutls-boot (as returned by\n`gnutls-boot-parameters').\n\n:server QLEN -- if QLEN is non-nil, create a server process for the\nspecified FAMILY, SERVICE, and connection type (stream or datagram).\nIf QLEN is an integer, it is used as the max. length of the server's\npending connection queue (also known as the backlog); the default\nqueue length is 5.  Default is to create a client process.\n\nThe following network options can be specified for this connection:\n\n:broadcast BOOL    -- Allow send and receive of datagram broadcasts.\n:dontroute BOOL    -- Only send to directly connected hosts.\n:keepalive BOOL    -- Send keep-alive messages on network stream.\n:linger BOOL or TIMEOUT -- Send queued messages before closing.\n:oobinline BOOL    -- Place out-of-band data in receive data stream.\n:priority INT      -- Set protocol defined priority for sent packets.\n:reuseaddr BOOL    -- Allow reusing a recently used local address\n                      (this is allowed by default for a server process).\n:bindtodevice NAME -- bind to interface NAME.  Using this may require\n                      special privileges on some systems.\n:use-external-socket BOOL -- Use any pre-allocated sockets that have\n                             been passed to Emacs.  If Emacs wasn't\n                             passed a socket, this option is silently\n                             ignored.\n\n\nConsult the relevant system programmer's manual pages for more\ninformation on using these options.\n\n\nA server process will listen for and accept connections from clients.\nWhen a client connection is accepted, a new network process is created\nfor the connection with the following parameters:\n\n- The client's process name is constructed by concatenating the server\nprocess's NAME and a client identification string.\n- If the FILTER argument is non-nil, the client process will not get a\nseparate process buffer; otherwise, the client's process buffer is a newly\ncreated buffer named after the server process's BUFFER name or process\nNAME concatenated with the client identification string.\n- The connection type and the process filter and sentinel parameters are\ninherited from the server process's TYPE, FILTER and SENTINEL.\n- The client process's contact info is set according to the client's\naddressing information (typically an IP address and a port number).\n- The client process's plist is initialized from the server's plist.\n\nNotice that the FILTER and SENTINEL args are never used directly by\nthe server process.  Also, the BUFFER argument is not used directly by\nthe server process, but via the optional :log function, accepted (and\nfailed) connections may be logged in the server process's buffer.\n\nThe original argument list, modified with the actual connection\ninformation, is available via the `process-contact' function.\n\nusage: (make-network-process &rest ARGS)")
    @GenerateNodeFactory
    public abstract static class FMakeNetworkProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeNetworkProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "network-interface-list", minArgs = 0, maxArgs = 2, doc = "Return an alist of all network interfaces and their network address.\nEach element is cons of the form (IFNAME . IP) where IFNAME is a\nstring containing the interface name, and IP is the network address in\ninternal format; see the description of ADDRESS in\n`make-network-process'.  The interface name is not guaranteed to be\nunique.\n\nOptional parameter FULL non-nil means return all IP address info for\neach interface.  Each element is then a list of the form\n    (IFNAME IP BCAST MASK)\nwhere IFNAME is the interface name, IP the IP address,\nBCAST the broadcast address, and MASK the network mask.\n\nOptional parameter FAMILY controls the type of addresses to return.\nThe default of nil means both IPv4 and IPv6, symbol `ipv4' means IPv4\nonly, symbol `ipv6' means IPv6 only.\n\nSee also `network-interface-info', which is limited to IPv4 only.\n\nIf the information is not available, return nil.")
    @GenerateNodeFactory
    public abstract static class FNetworkInterfaceList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object networkInterfaceList(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "network-interface-info", minArgs = 1, maxArgs = 1, doc = "Return information about network interface named IFNAME.\nThe return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),\nwhere ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,\nNETMASK is the layer 3 network mask, HWADDR is the layer 2 address, and\nFLAGS is the current flags of the interface.\n\nData that is unavailable is returned as nil.")
    @GenerateNodeFactory
    public abstract static class FNetworkInterfaceInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object networkInterfaceInfo(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "network-lookup-address-info", minArgs = 1, maxArgs = 3, doc = "Look up Internet Protocol (IP) address info of NAME.\nOptional argument FAMILY controls whether to look up IPv4 or IPv6\naddresses.  The default of nil means both, symbol `ipv4' means IPv4\nonly, symbol `ipv6' means IPv6 only.\nOptional argument HINTS allows specifying the hints passed to the\nunderlying library call.  The only supported value is `numeric', which\nmeans treat NAME as a numeric IP address.  This also suppresses DNS\ntraffic.\nReturn a list of addresses, or nil if none were found.  Each address\nis a vector of integers, as per the description of ADDRESS in\n`make-network-process'.  In case of error log the error message\nreturned from the lookup.")
    @GenerateNodeFactory
    public abstract static class FNetworkLookupAddressInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object networkLookupAddressInfo(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "accept-process-output", minArgs = 0, maxArgs = 4, doc = "Allow any pending output from subprocesses to be read by Emacs.\nIt is given to their filter functions.\nOptional argument PROCESS means to return only after output is\nreceived from PROCESS or PROCESS closes the connection.\n\nOptional second argument SECONDS and third argument MILLISEC\nspecify a timeout; return after that much time even if there is\nno subprocess output.  If SECONDS is a floating point number,\nit specifies a fractional number of seconds to wait.\nThe MILLISEC argument is obsolete and should be avoided.\n\nIf optional fourth argument JUST-THIS-ONE is non-nil, accept output\nfrom PROCESS only, suspending reading output from other processes.\nIf JUST-THIS-ONE is an integer, don't run any timers either.\nReturn non-nil if we received any output from PROCESS (or, if PROCESS\nis nil, from any process) before the timeout expired or the\ncorresponding connection was closed.")
    @GenerateNodeFactory
    public abstract static class FAcceptProcessOutput extends ELispBuiltInBaseNode {
        @Specialization
        public static Object acceptProcessOutput(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-process-filter", minArgs = 2, maxArgs = 2, doc = "Function used as default process filter.\nThis inserts the process's output into its buffer, if there is one.\nOtherwise it discards the output.")
    @GenerateNodeFactory
    public abstract static class FInternalDefaultProcessFilter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultProcessFilter(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-send-region", minArgs = 3, maxArgs = 3, doc = "Send current contents of region as input to PROCESS.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\nCalled from program, takes three arguments, PROCESS, START and END.\nIf the region is larger than the input buffer of the process (the\nlength of which depends on the process connection type and the\noperating system), it is sent in several bunches.  This may happen\neven for shorter regions.  Output from processes can arrive in between\nbunches.\n\nIf PROCESS is a non-blocking network process that hasn't been fully\nset up yet, this function will block until socket setup has completed.")
    @GenerateNodeFactory
    public abstract static class FProcessSendRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSendRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-send-string", minArgs = 2, maxArgs = 2, doc = "Send PROCESS the contents of STRING as input.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\nIf STRING is larger than the input buffer of the process (the length\nof which depends on the process connection type and the operating\nsystem), it is sent in several bunches.  This may happen even for\nshorter strings.  Output from processes can arrive in between bunches.\n\nIf PROCESS is a non-blocking network process that hasn't been fully\nset up yet, this function will block until socket setup has completed.")
    @GenerateNodeFactory
    public abstract static class FProcessSendString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSendString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-running-child-p", minArgs = 0, maxArgs = 1, doc = "Return non-nil if PROCESS has given control of its terminal to a child.\nIf the operating system does not make it possible to find out, return t.\nIf it's possible to find out, return the numeric ID of the foreground\nprocess group if PROCESS did give control of its terminal to a\nchild process, and return nil if it didn't.\n\nPROCESS must be a real subprocess, not a connection.")
    @GenerateNodeFactory
    public abstract static class FProcessRunningChildP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processRunningChildP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-interrupt-process", minArgs = 0, maxArgs = 2, doc = "Default function to interrupt process PROCESS.\nIt shall be the last element in list `interrupt-process-functions'.\nSee function `interrupt-process' for more details on usage.")
    @GenerateNodeFactory
    public abstract static class FInternalDefaultInterruptProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultInterruptProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "interrupt-process", minArgs = 0, maxArgs = 2, doc = "Interrupt process PROCESS.\nPROCESS may be a process, a buffer, or the name of a process or buffer.\nNo arg or nil means current buffer's process.\nSecond arg CURRENT-GROUP non-nil means send signal to\nthe current process-group of the process's controlling terminal\nrather than to the process's own process group.\nIf the process is a shell, this means interrupt current subjob\nrather than the shell.\n\nIf CURRENT-GROUP is `lambda', and if the shell owns the terminal,\ndon't send the signal.\n\nThis function calls the functions of `interrupt-process-functions' in\nthe order of the list, until one of them returns non-nil.")
    @GenerateNodeFactory
    public abstract static class FInterruptProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object interruptProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-process", minArgs = 0, maxArgs = 2, doc = "Kill process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.")
    @GenerateNodeFactory
    public abstract static class FKillProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "quit-process", minArgs = 0, maxArgs = 2, doc = "Send QUIT signal to process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.")
    @GenerateNodeFactory
    public abstract static class FQuitProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object quitProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "stop-process", minArgs = 0, maxArgs = 2, doc = "Stop process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.\nIf PROCESS is a network or serial or pipe connection, inhibit handling\nof incoming traffic.")
    @GenerateNodeFactory
    public abstract static class FStopProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stopProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "continue-process", minArgs = 0, maxArgs = 2, doc = "Continue process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.\nIf PROCESS is a network or serial process, resume handling of incoming\ntraffic.")
    @GenerateNodeFactory
    public abstract static class FContinueProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object continueProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-signal-process", minArgs = 2, maxArgs = 3, doc = "Default function to send PROCESS the signal with code SIGCODE.\nIt shall be the last element in list `signal-process-functions'.\nSee function `signal-process' for more details on usage.")
    @GenerateNodeFactory
    public abstract static class FInternalDefaultSignalProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultSignalProcess(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "signal-process", minArgs = 2, maxArgs = 3, doc = "Send PROCESS the signal with code SIGCODE.\nPROCESS may also be a number specifying the process id of the\nprocess to signal; in this case, the process need not be a child of\nthis Emacs.\nIf PROCESS is a process object which contains the property\n`remote-pid', or PROCESS is a number and REMOTE is a remote file name,\nPROCESS is interpreted as process on the respective remote host, which\nwill be the process to signal.\nIf PROCESS is a string, it is interpreted as process object with the\nrespective process name, or as a number.\nSIGCODE may be an integer, or a symbol whose name is a signal name.")
    @GenerateNodeFactory
    public abstract static class FSignalProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object signalProcess(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-send-eof", minArgs = 0, maxArgs = 1, doc = "Make PROCESS see end-of-file in its input.\nEOF comes after any text already sent to it.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\nIf PROCESS is a network connection, or is a process communicating\nthrough a pipe (as opposed to a pty), then you cannot send any more\ntext to PROCESS after you call this function.\nIf PROCESS is a serial process, wait until all output written to the\nprocess has been transmitted to the serial port.")
    @GenerateNodeFactory
    public abstract static class FProcessSendEof extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSendEof(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-process-sentinel", minArgs = 2, maxArgs = 2, doc = "Function used as default sentinel for processes.\nThis inserts a status message into the process's buffer, if there is one.")
    @GenerateNodeFactory
    public abstract static class FInternalDefaultProcessSentinel extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultProcessSentinel(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-coding-system", minArgs = 1, maxArgs = 3, doc = "Set coding systems of PROCESS to DECODING and ENCODING.\nDECODING will be used to decode subprocess output and ENCODING to\nencode subprocess input.")
    @GenerateNodeFactory
    public abstract static class FSetProcessCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessCodingSystem(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-coding-system", minArgs = 1, maxArgs = 1, doc = "Return a cons of coding systems for decoding and encoding of PROCESS.")
    @GenerateNodeFactory
    public abstract static class FProcessCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processCodingSystem(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-buffer-process", minArgs = 1, maxArgs = 1, doc = "Return the (or a) live process associated with BUFFER.\nBUFFER may be a buffer or the name of one.\nReturn nil if all processes associated with BUFFER have been\ndeleted or killed.")
    @GenerateNodeFactory
    public abstract static class FGetBufferProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBufferProcess(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-inherit-coding-system-flag", minArgs = 1, maxArgs = 1, doc = "Return the value of inherit-coding-system flag for PROCESS.\nIf this flag is t, `buffer-file-coding-system' of the buffer\nassociated with PROCESS will inherit the coding system used to decode\nthe process output.")
    @GenerateNodeFactory
    public abstract static class FProcessInheritCodingSystemFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processInheritCodingSystemFlag(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "waiting-for-user-input-p", minArgs = 0, maxArgs = 0, doc = "Return non-nil if Emacs is waiting for input from the user.\nThis is intended for use by asynchronous process output filters and sentinels.")
    @GenerateNodeFactory
    public abstract static class FWaitingForUserInputP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object waitingForUserInputP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "list-system-processes", minArgs = 0, maxArgs = 0, doc = "Return a list of numerical process IDs of all running processes.\nIf this functionality is unsupported, return nil.\nIf `default-directory' is remote, return process IDs of the respective remote host.\n\nSee `process-attributes' for getting attributes of a process given its ID.")
    @GenerateNodeFactory
    public abstract static class FListSystemProcesses extends ELispBuiltInBaseNode {
        @Specialization
        public static Object listSystemProcesses() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-attributes", minArgs = 1, maxArgs = 1, doc = "Return attributes of the process given by its PID, a number.\nIf `default-directory' is remote, PID is regarded as process\nidentifier on the respective remote host.\n\nValue is an alist where each element is a cons cell of the form\n\n    (KEY . VALUE)\n\nIf this functionality is unsupported, the value is nil.\n\nSee `list-system-processes' for getting a list of all process IDs.\n\nThe KEYs of the attributes that this function may return are listed\nbelow, together with the type of the associated VALUE (in parentheses).\nNot all platforms support all of these attributes; unsupported\nattributes will not appear in the returned alist.\nUnless explicitly indicated otherwise, numbers can have either\ninteger or floating point values.\n\n euid    -- Effective user User ID of the process (number)\n user    -- User name corresponding to euid (string)\n egid    -- Effective user Group ID of the process (number)\n group   -- Group name corresponding to egid (string)\n comm    -- Command name (executable name only) (string)\n state   -- Process state code, such as \"S\", \"R\", or \"T\" (string)\n ppid    -- Parent process ID (number)\n pgrp    -- Process group ID (number)\n sess    -- Session ID, i.e. process ID of session leader (number)\n ttname  -- Controlling tty name (string)\n tpgid   -- ID of foreground process group on the process's tty (number)\n minflt  -- number of minor page faults (number)\n majflt  -- number of major page faults (number)\n cminflt -- cumulative number of minor page faults (number)\n cmajflt -- cumulative number of major page faults (number)\n utime   -- user time used by the process, in `current-time' format\n stime   -- system time used by the process (current-time)\n time    -- sum of utime and stime (current-time)\n cutime  -- user time used by the process and its children (current-time)\n cstime  -- system time used by the process and its children (current-time)\n ctime   -- sum of cutime and cstime (current-time)\n pri     -- priority of the process (number)\n nice    -- nice value of the process (number)\n thcount -- process thread count (number)\n start   -- time the process started (current-time)\n vsize   -- virtual memory size of the process in KB's (number)\n rss     -- resident set size of the process in KB's (number)\n etime   -- elapsed time the process is running (current-time)\n pcpu    -- percents of CPU time used by the process (floating-point number)\n pmem    -- percents of total physical memory used by process's resident set\n              (floating-point number)\n args    -- command line which invoked the process (string).")
    @GenerateNodeFactory
    public abstract static class FProcessAttributes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processAttributes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "num-processors", minArgs = 0, maxArgs = 1, doc = "Return the number of processors, a positive integer.\nEach usable thread execution unit counts as a processor.\nBy default, count the number of available processors,\noverridable via the OMP_NUM_THREADS environment variable.\nIf optional argument QUERY is `current', ignore OMP_NUM_THREADS.\nIf QUERY is `all', also count processors not available.")
    @GenerateNodeFactory
    public abstract static class FNumProcessors extends ELispBuiltInBaseNode {
        @Specialization
        public static Object numProcessors(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "signal-names", minArgs = 0, maxArgs = 0, doc = "Return a list of known signal names on this system.")
    @GenerateNodeFactory
    public abstract static class FSignalNames extends ELispBuiltInBaseNode {
        @Specialization
        public static Object signalNames() {
            throw new UnsupportedOperationException();
        }
    }
}
