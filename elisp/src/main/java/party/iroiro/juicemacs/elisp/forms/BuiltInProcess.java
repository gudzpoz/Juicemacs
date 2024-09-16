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

    @ELispBuiltIn(name = "processp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-process", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getProcess(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-process", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteProcess(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-status", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessStatus extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processStatus(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-exit-status", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessExitStatus extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processExitStatus(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-id", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessId extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processId(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-command", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessCommand extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processCommand(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-tty-name", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FProcessTtyName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processTtyName(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-buffer", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessBuffer(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-mark", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessMark extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processMark(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-filter", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessFilter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessFilter(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-filter", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessFilter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processFilter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-sentinel", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessSentinel extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessSentinel(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-sentinel", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessSentinel extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSentinel(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-thread", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessThread(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-thread", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessThread extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processThread(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-window-size", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetProcessWindowSize extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessWindowSize(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-inherit-coding-system-flag", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessInheritCodingSystemFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessInheritCodingSystemFlag(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-query-on-exit-flag", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessQueryOnExitFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessQueryOnExitFlag(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-query-on-exit-flag", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessQueryOnExitFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processQueryOnExitFlag(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-contact", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FProcessContact extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processContact(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-plist", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processPlist(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-plist", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessPlist extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessPlist(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-connection", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessConnection extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processConnection(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-type", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessType extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processType(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "format-network-address", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFormatNetworkAddress extends ELispBuiltInBaseNode {
        @Specialization
        public static Object formatNetworkAddress(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-list", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FProcessList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processList() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-process", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-pipe-process", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakePipeProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makePipeProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-datagram-address", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessDatagramAddress extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processDatagramAddress(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-datagram-address", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetProcessDatagramAddress extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessDatagramAddress(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-network-process-option", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FSetNetworkProcessOption extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setNetworkProcessOption(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "serial-process-configure", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FSerialProcessConfigure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object serialProcessConfigure(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-serial-process", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeSerialProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeSerialProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-network-process", minArgs = 0, maxArgs = 0, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FMakeNetworkProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeNetworkProcess(Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "network-interface-list", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FNetworkInterfaceList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object networkInterfaceList(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "network-interface-info", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNetworkInterfaceInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object networkInterfaceInfo(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "network-lookup-address-info", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FNetworkLookupAddressInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object networkLookupAddressInfo(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "accept-process-output", minArgs = 0, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FAcceptProcessOutput extends ELispBuiltInBaseNode {
        @Specialization
        public static Object acceptProcessOutput(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-process-filter", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalDefaultProcessFilter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultProcessFilter(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-send-region", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FProcessSendRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSendRegion(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-send-string", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FProcessSendString extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSendString(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-running-child-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessRunningChildP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processRunningChildP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-interrupt-process", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalDefaultInterruptProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultInterruptProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "interrupt-process", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInterruptProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object interruptProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-process", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FKillProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "quit-process", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FQuitProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object quitProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "stop-process", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FStopProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object stopProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "continue-process", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FContinueProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object continueProcess(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-signal-process", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FInternalDefaultSignalProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultSignalProcess(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "signal-process", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSignalProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object signalProcess(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-send-eof", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessSendEof extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processSendEof(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal-default-process-sentinel", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalDefaultProcessSentinel extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalDefaultProcessSentinel(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-process-coding-system", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetProcessCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setProcessCodingSystem(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-coding-system", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessCodingSystem extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processCodingSystem(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-buffer-process", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetBufferProcess extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBufferProcess(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-inherit-coding-system-flag", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessInheritCodingSystemFlag extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processInheritCodingSystemFlag(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "waiting-for-user-input-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FWaitingForUserInputP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object waitingForUserInputP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "list-system-processes", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FListSystemProcesses extends ELispBuiltInBaseNode {
        @Specialization
        public static Object listSystemProcesses() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "process-attributes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FProcessAttributes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object processAttributes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "num-processors", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNumProcessors extends ELispBuiltInBaseNode {
        @Specialization
        public static Object numProcessors(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "signal-names", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSignalNames extends ELispBuiltInBaseNode {
        @Specialization
        public static Object signalNames() {
            throw new UnsupportedOperationException();
        }
    }
}
