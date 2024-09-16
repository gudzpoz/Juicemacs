package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispContext.NIL;

public class BuiltInBuffer extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInBufferFactory.getFactories();
    }

    @ELispBuiltIn(name = "buffer-live-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLiveP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-list", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferList extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferList(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-file-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetFileBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getFileBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-truename-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FGetTruenameBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getTruenameBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "find-buffer", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFindBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findBuffer(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "get-buffer-create", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGetBufferCreate extends ELispBuiltInBaseNode {
        @Specialization
        public static Object getBufferCreate(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-indirect-buffer", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FMakeIndirectBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeIndirectBuffer(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "generate-new-buffer-name", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FGenerateNewBufferName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object generateNewBufferName(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-last-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLastName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLastName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-file-name", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferFileName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-base-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferBaseBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferBaseBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-local-value", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FBufferLocalValue extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLocalValue(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-local-variables", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferLocalVariables extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferLocalVariables(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-modified-p", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferModifiedP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "force-mode-line-update", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FForceModeLineUpdate extends ELispBuiltInBaseNode {
        @Specialization
        public static Object forceModeLineUpdate(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-modified-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferModifiedP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "restore-buffer-modified-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FRestoreBufferModifiedP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object restoreBufferModifiedP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-modified-tick", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferModifiedTick(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "internal--set-buffer-modified-tick", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FInternalSetBufferModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Object internalSetBufferModifiedTick(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-chars-modified-tick", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferCharsModifiedTick extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferCharsModifiedTick(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rename-buffer", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FRenameBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object renameBuffer(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "other-buffer", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FOtherBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object otherBuffer(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-enable-undo", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferEnableUndo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferEnableUndo(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-buffer", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKillBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "bury-buffer-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBuryBufferInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object buryBufferInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-major-mode", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBufferMajorMode extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferMajorMode(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "current-buffer", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object currentBuffer() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBuffer(ELispString a) {
            // TODO: Real buffers
            return NIL;
        }
    }

    @ELispBuiltIn(name = "barf-if-buffer-read-only", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBarfIfBufferReadOnly extends ELispBuiltInBaseNode {
        @Specialization
        public static Object barfIfBufferReadOnly(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "erase-buffer", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FEraseBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object eraseBuffer() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "buffer-swap-text", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FBufferSwapText extends ELispBuiltInBaseNode {
        @Specialization
        public static Object bufferSwapText(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-multibyte", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetBufferMultibyte extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferMultibyte(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "kill-all-local-variables", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FKillAllLocalVariables extends ELispBuiltInBaseNode {
        @Specialization
        public static Object killAllLocalVariables(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlayp", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayp extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayp(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-overlay", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FMakeOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeOverlay(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "move-overlay", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FMoveOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object moveOverlay(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-overlay", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteOverlay extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteOverlay(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-all-overlays", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteAllOverlays extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteAllOverlays(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-start", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayStart(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-end", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayEnd(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayBuffer(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-properties", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayProperties extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayProperties(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlays-at", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOverlaysAt extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlaysAt(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlays-in", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOverlaysIn extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlaysIn(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "next-overlay-change", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FNextOverlayChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nextOverlayChange(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "previous-overlay-change", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FPreviousOverlayChange extends ELispBuiltInBaseNode {
        @Specialization
        public static Object previousOverlayChange(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-lists", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FOverlayLists extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayLists() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-recenter", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayRecenter extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayRecenter(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-get", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FOverlayGet extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayGet(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-put", minArgs = 3, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FOverlayPut extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayPut(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "overlay-tree", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FOverlayTree extends ELispBuiltInBaseNode {
        @Specialization
        public static Object overlayTree(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
